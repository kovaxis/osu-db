//! Parsing for the `osu!.db` file, containing cached information about the beatmap listing.

use crate::prelude::*;
use std::hash::Hash;

/// In these `osu!.db` versions several breaking changes were introduced.
/// While parsing, these changes are automatically handled depending on the `osu!.db` version.
pub const CHANGE_20140609: u32 = 20140609;
pub const CHANGE_20191106: u32 = 20191106;

/// A structure representing the `osu!.db` binary database.
/// This database contains pre-processed data and settings for all available osu! beatmaps.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Listing {
    /// The `osu!.db` version number.
    /// This is a decimal number in the form `YYYYMMDD` (eg. `20150203`).
    pub version: u32,

    /// The amount of folders within the "Songs" directory.
    /// Probably for quick checking of changes within the directory.
    pub folder_count: u32,

    /// Whether the account is locked/banned, and when will be it be unbanned.
    pub unban_date: Option<DateTime<Utc>>,

    /// Self-explanatory.
    pub player_name: Option<String>,

    /// All stored beatmaps and the information stored about them.
    /// The main bulk of information.
    pub beatmaps: Vec<Beatmap>,

    /// User permissions (0 = None, 1 = Normal, 2 = Moderator, 4 = Supporter,
    /// 8 = Friend, 16 = peppy, 32 = World Cup staff)
    pub user_permissions: u32,
}
impl Listing {
    pub fn from_bytes(bytes: &[u8]) -> Result<Listing, Error> {
        Ok(listing(bytes).map(|(_rem, listing)| listing)?)
    }

    /// Parse a listing from the `osu!.db` database file.
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Listing, Error> {
        Self::from_bytes(&fs::read(path)?)
    }

    /// Write the listing to an arbitrary writer.
    pub fn to_writer<W: Write>(&self, mut out: W) -> io::Result<()> {
        self.wr(&mut out)
    }

    /// Similar to `to_writer` but writes the listing to a file (ie. `osu!.db`).
    pub fn save<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        self.to_writer(BufWriter::new(File::create(path)?))
    }
}

#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Beatmap {
    /// The name of the artist without special characters.
    pub artist_ascii: Option<String>,
    /// The unrestrained artist name.
    pub artist_unicode: Option<String>,
    /// The song title without special characters.
    pub title_ascii: Option<String>,
    /// The unrestrained song title.
    pub title_unicode: Option<String>,
    /// The name of the beatmap mapper.
    pub creator: Option<String>,
    /// The name of this specific difficulty.
    pub difficulty_name: Option<String>,
    /// The filename of the song file.
    pub audio: Option<String>,
    /// The MD5 hash of the beatmap.
    pub hash: Option<String>,
    /// The filename of the `.osu` file corresponding to this specific difficulty.
    pub file_name: Option<String>,
    pub status: RankedStatus,
    pub hitcircle_count: u16,
    pub slider_count: u16,
    pub spinner_count: u16,
    pub last_modified: DateTime<Utc>,
    pub approach_rate: f32,
    pub circle_size: f32,
    pub hp_drain: f32,
    pub overall_difficulty: f32,
    pub slider_velocity: f64,
    pub std_ratings: StarRatings,
    pub taiko_ratings: StarRatings,
    pub ctb_ratings: StarRatings,
    pub mania_ratings: StarRatings,
    /// Drain time in seconds.
    pub drain_time: u32,
    /// Total beatmap time in milliseconds.
    pub total_time: u32,
    /// When should the song start playing when previewed, in milliseconds since the start of the
    /// song.
    pub preview_time: u32,
    pub timing_points: Vec<TimingPoint>,
    pub beatmap_id: i32,
    pub beatmapset_id: i32,
    pub thread_id: u32,
    pub std_grade: Grade,
    pub taiko_grade: Grade,
    pub ctb_grade: Grade,
    pub mania_grade: Grade,
    pub local_beatmap_offset: u16,
    pub stack_leniency: f32,
    pub mode: Mode,
    /// Where did the song come from, if anywhere.
    pub song_source: Option<String>,
    /// Song tags, separated by whitespace.
    pub tags: Option<String>,
    pub online_offset: u16,
    pub title_font: Option<String>,
    /// Whether the beatmap has been played, and if it has, when was it last played.
    pub last_played: Option<DateTime<Utc>>,
    /// Whether the beatmap was in `osz2` format.
    pub is_osz2: bool,
    /// The folder name of the beatmapset within the "Songs" folder.
    pub folder_name: Option<String>,
    /// When was the beatmap last checked against the online osu! repository.
    pub last_online_check: DateTime<Utc>,
    pub ignore_sounds: bool,
    pub ignore_skin: bool,
    pub disable_storyboard: bool,
    pub disable_video: bool,
    pub visual_override: bool,
    /// Quoting the wiki: "Unknown. Only present if version is less than 20140609".
    pub mysterious_short: Option<u16>,
    /// Who knows.
    ///
    /// Perhaps an early attempt at "last modified", but scrapped once peppy noticed it only had
    /// 32 bits.
    pub mysterious_last_modified: u32,
    pub mania_scroll_speed: u8,
}

#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RankedStatus {
    Unknown,
    Unsubmitted,
    /// Any of the three.
    PendingWipGraveyard,
    Ranked,
    Approved,
    Qualified,
    Loved,
}
impl RankedStatus {
    pub fn from_raw(byte: u8) -> Option<RankedStatus> {
        use self::RankedStatus::*;
        Some(match byte {
            0 => Unknown,
            1 => Unsubmitted,
            2 => PendingWipGraveyard,
            4 => Ranked,
            5 => Approved,
            6 => Qualified,
            7 => Loved,
            _ => return None,
        })
    }

    pub fn raw(self) -> u8 {
        use self::RankedStatus::*;
        match self {
            Unknown => 0,
            Unsubmitted => 1,
            PendingWipGraveyard => 2,
            Ranked => 4,
            Approved => 5,
            Qualified => 6,
            Loved => 7,
        }
    }
}

/// A list of the precalculated amount of difficulty stars a given mod combination yields for a
/// beatmap.
///
/// You might want to convert this list into a map using
/// `ratings.into_iter().collect::<HashMap<_>>()` or variations, allowing for quick indexing with
/// different mod combinations.
///
/// Note that old "osu!.db" files (before the 2014/06/09 version) do not have these ratings.
pub type StarRatings = Vec<(ModSet, f64)>;

#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TimingPoint {
    /// The bpm of the timing point.
    pub bpm: f64,
    /// The amount of milliseconds from the start of the song this timing point is located on.
    pub offset: f64,
    /// Whether the timing point inherits or not.
    ///
    /// Basically, inherited timing points are absolute, and define a new bpm independent of any previous bpms.
    /// On the other hand, timing points that do not inherit have a negative bpm representing a percentage of the
    /// bpm of the previous timing point.
    /// See the osu wiki on the `.osu` format for more details.
    pub inherits: bool,
}

/// A grade obtained by passing a beatmap.
/// Also called a rank.
///
/// Note that currently grades are just exposed as a raw byte.
/// I am not sure of how do this bytes map to grades as of now.
/// TODO: Figure out grades.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Grade {
    /// SS+, silver SS rank
    /// Ie. only perfect scores with hidden mod enabled.
    SSPlus,
    /// S+, silver S rank
    /// Ie. highest performance with hidden mod enabled.
    SPlus,
    /// SS rank
    /// Ie. only perfect scores.
    SS,
    S,
    A,
    B,
    C,
    D,
    /// No rank achieved yet.
    Unplayed,
}
impl Grade {
    pub fn raw(self) -> u8 {
        use self::Grade::*;
        match self {
            SSPlus => 0,
            SPlus => 1,
            SS => 2,
            S => 3,
            A => 4,
            B => 5,
            C => 6,
            D => 7,
            Unplayed => 9,
        }
    }
    pub fn from_raw(raw: u8) -> Option<Grade> {
        use self::Grade::*;
        Some(match raw {
            0 => SSPlus,
            1 => SPlus,
            2 => SS,
            3 => S,
            4 => A,
            5 => B,
            6 => C,
            7 => D,
            9 => Unplayed,
            _ => return None,
        })
    }
}

named!(listing<&[u8], Listing>, do_parse!(
    version: int >>
    folder_count: int >>
    account_unlocked: boolean >>
    unlock_date: datetime >>
    player_name: opt_string >>
    beatmaps: length_count!(int, call!(beatmap, version)) >>
    user_permissions: int >>
    (Listing{
        version,folder_count,unban_date: build_option(account_unlocked, unlock_date),player_name,beatmaps,user_permissions,
    })
));
writer!(Listing [this, out] {
    this.version.wr(out)?;
    this.folder_count.wr(out)?;
    write_option(out,this.unban_date,0_u64)?;
    this.player_name.wr(out)?;
    PrefixedList(&this.beatmaps).wr_args(out,this.version)?;
    this.user_permissions.wr(out)?;
});

named_args!(beatmap (version: u32) <&[u8], Beatmap>, do_parse!(
    beatmap_size: cond!(version<CHANGE_20191106, int) >>
    artist_ascii: opt_string >>
    artist_unicode: opt_string >>
    title_ascii: opt_string >>
    title_unicode: opt_string >>
    creator: opt_string >>
    difficulty_name: opt_string >>
    audio: opt_string >>
    hash: opt_string >>
    file_name: opt_string >>
    status: ranked_status >>
    hitcircle_count: short >>
    slider_count: short >>
    spinner_count: short >>
    last_modified: datetime >>
    approach_rate: call!(difficulty_value, version) >>
    circle_size: call!(difficulty_value, version) >>
    hp_drain: call!(difficulty_value, version) >>
    overall_difficulty: call!(difficulty_value, version) >>
    slider_velocity: double >>
    std_ratings: call!(star_ratings, version) >>
    taiko_ratings: call!(star_ratings, version) >>
    ctb_ratings: call!(star_ratings, version) >>
    mania_ratings: call!(star_ratings, version) >>
    drain_time: int >>
    total_time: int >>
    preview_time: int >>
    timing_points: length_count!(int, timing_point) >>
    beatmap_id: int >>
    beatmapset_id: int >>
    thread_id: int >>
    std_grade: grade >>
    taiko_grade: grade >>
    ctb_grade: grade >>
    mania_grade: grade >>
    local_beatmap_offset: short >>
    stack_leniency: single >>
    mode: map_opt!(byte, Mode::from_raw) >>
    song_source: opt_string >>
    tags: opt_string >>
    online_offset: short >>
    title_font: opt_string >>
    unplayed: boolean >>
    last_played: datetime >>
    is_osz2: boolean >>
    folder_name: opt_string >>
    last_online_check: datetime >>
    ignore_sounds: boolean >>
    ignore_skin: boolean >>
    disable_storyboard: boolean >>
    disable_video: boolean >>
    visual_override: boolean >>
    mysterious_short: cond!(version<CHANGE_20140609, short) >>
    mysterious_last_modified: int >>
    mania_scroll_speed: byte >>
    (Beatmap{
        artist_ascii,artist_unicode,title_ascii,title_unicode,creator,
        difficulty_name,audio,hash,file_name,status,hitcircle_count,slider_count,
        spinner_count,last_modified,approach_rate,circle_size,hp_drain,
        overall_difficulty,slider_velocity,std_ratings,taiko_ratings,ctb_ratings,
        mania_ratings,drain_time,total_time,preview_time,timing_points,
        beatmap_id: beatmap_id as i32,beatmapset_id: beatmapset_id as i32,thread_id,std_grade,
        taiko_grade,ctb_grade,mania_grade,local_beatmap_offset,stack_leniency,mode,song_source,
        tags,online_offset,title_font,is_osz2,folder_name,last_online_check,ignore_sounds,
        ignore_skin,disable_storyboard,disable_video,visual_override,mysterious_short,
        mysterious_last_modified,mania_scroll_speed,
        last_played: build_option(unplayed,last_played),
    })
));

writer!(Beatmap [this,out,version: u32] {
    //Write into a writer without prefixing the length
    fn write_dry<W: Write>(this: &Beatmap, out: &mut W, version: u32) -> io::Result<()> {
        macro_rules! wr_difficulty_value {
            ($f32:expr) => {{
                if version>=CHANGE_20140609 {
                    $f32.wr(out)?;
                }else{
                    ($f32 as u8).wr(out)?;
                }
            }};
        }
        this.artist_ascii.wr(out)?;
        this.artist_unicode.wr(out)?;
        this.title_ascii.wr(out)?;
        this.title_unicode.wr(out)?;
        this.creator.wr(out)?;
        this.difficulty_name.wr(out)?;
        this.audio.wr(out)?;
        this.hash.wr(out)?;
        this.file_name.wr(out)?;
        this.status.wr(out)?;
        this.hitcircle_count.wr(out)?;
        this.slider_count.wr(out)?;
        this.spinner_count.wr(out)?;
        this.last_modified.wr(out)?;
        wr_difficulty_value!(this.approach_rate);
        wr_difficulty_value!(this.circle_size);
        wr_difficulty_value!(this.hp_drain);
        wr_difficulty_value!(this.overall_difficulty);
        this.slider_velocity.wr(out)?;
        this.std_ratings.wr_args(out,version)?;
        this.taiko_ratings.wr_args(out,version)?;
        this.ctb_ratings.wr_args(out,version)?;
        this.mania_ratings.wr_args(out,version)?;
        this.drain_time.wr(out)?;
        this.total_time.wr(out)?;
        this.preview_time.wr(out)?;
        PrefixedList(&this.timing_points).wr(out)?;
        (this.beatmap_id as u32).wr(out)?;
        (this.beatmapset_id as u32).wr(out)?;
        this.thread_id.wr(out)?;
        this.std_grade.wr(out)?;
        this.taiko_grade.wr(out)?;
        this.ctb_grade.wr(out)?;
        this.mania_grade.wr(out)?;
        this.local_beatmap_offset.wr(out)?;
        this.stack_leniency.wr(out)?;
        this.mode.raw().wr(out)?;
        this.song_source.wr(out)?;
        this.tags.wr(out)?;
        this.online_offset.wr(out)?;
        this.title_font.wr(out)?;
        write_option(out,this.last_played,0_u64)?;
        this.is_osz2.wr(out)?;
        this.folder_name.wr(out)?;
        this.last_online_check.wr(out)?;
        this.ignore_sounds.wr(out)?;
        this.ignore_skin.wr(out)?;
        this.disable_storyboard.wr(out)?;
        this.disable_video.wr(out)?;
        this.visual_override.wr(out)?;
        if version<CHANGE_20140609 {
            this.mysterious_short.unwrap_or(0).wr(out)?;
        }
        this.mysterious_last_modified.wr(out)?;
        this.mania_scroll_speed.wr(out)?;
        Ok(())
    }
    if version < CHANGE_20191106 {
        //Write beatmap into a temporary buffer, as beatmap length needs to be
        //known and prefixed
        let mut raw_buf = Vec::new();
        write_dry(this, &mut raw_buf, version)?;
        //Write the raw buffer prefixed by its length
        (raw_buf.len() as u32).wr(out)?;
        out.write_all(&raw_buf)?;
    }else{
        //Write beatmap as-is
        write_dry(this, out, version)?;
    }
});

named!(timing_point<&[u8], TimingPoint>, do_parse!(
    bpm: double >>
    offset: double >>
    inherits: boolean >>
    (TimingPoint{
        bpm,offset,inherits
    })
));
writer!(TimingPoint [this,out] {
    this.bpm.wr(out)?;
    this.offset.wr(out)?;
    this.inherits.wr(out)?;
});

named_args!(star_ratings(version: u32) <&[u8], Vec<(ModSet,f64)>>, switch!(value!(version>=CHANGE_20140609),
    true => length_count!(int, do_parse!(
        tag!(&[0x08]) >>
        mods: map!(int, ModSet::from_bits) >>
        tag!(&[0x0d]) >>
        stars: double >>
        ((mods,stars))
    )) |
    false => value!(Vec::new())
));
writer!(Vec<(ModSet,f64)> [this,out,version: u32] {
    if version>=CHANGE_20140609 {
        PrefixedList(&this).wr(out)?;
    }
});
writer!((ModSet,f64) [this,out] {
    0x08_u8.wr(out)?;
    this.0.bits().wr(out)?;
    0x0d_u8.wr(out)?;
    this.1.wr(out)?;
});

/// Before the breaking change in 2014 several difficulty values were stored as bytes.
/// After it they were stored as single floats.
/// Accomodate this differences.
fn difficulty_value(bytes: &[u8], version: u32) -> IResult<&[u8], f32> {
    if version >= CHANGE_20140609 {
        single(bytes)
    } else {
        byte(bytes).map(|(rem, b)| (rem, b as f32))
    }
}

named!(ranked_status<&[u8], RankedStatus>, map_opt!(byte, RankedStatus::from_raw));
writer!(RankedStatus [this,out] this.raw().wr(out)?);

named!(grade<&[u8], Grade>, map_opt!(byte, Grade::from_raw));
writer!(Grade [this,out] this.raw().wr(out)?);

fn build_option<T>(is_none: bool, content: T) -> Option<T> {
    if is_none {
        None
    } else {
        Some(content)
    }
}
fn write_option<W: Write, T: SimpleWritable, D: SimpleWritable>(
    out: &mut W,
    opt: Option<T>,
    def: D,
) -> io::Result<()> {
    match opt {
        Some(t) => {
            false.wr(out)?;
            t.wr(out)?;
        }
        None => {
            true.wr(out)?;
            def.wr(out)?;
        }
    }
    Ok(())
}
