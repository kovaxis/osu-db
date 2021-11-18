//! Parsing for replay and score files, which are very similar.

use crate::prelude::*;

/// The LZMA compression level (a number between 0 and 9) used to write replay data when it is
/// not otherwise specified.
const DEFAULT_COMPRESSION_LEVEL: u32 = 5;

/// An osu! replay.
/// The replay might come from a large `ScoreList` score database, or from an individual standalone
/// `.osr` file.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Replay {
    /// The gamemode the replay was scored in.
    pub mode: Mode,
    /// The `.db` version of the replay file.
    /// If the replay is inside a `scores.db` file, the version should be redundant with it (?).
    pub version: u32,
    /// The MD5 hash of the beatmap played.
    pub beatmap_hash: Option<String>,
    /// The name of the player who scored the replay.
    pub player_name: Option<String>,
    /// The replay-specific MD5 hash.
    pub replay_hash: Option<String>,
    /// The same in all modes: amount of 300 scores.
    pub count_300: u16,
    /// Amount of 100 scores in std and ctb, but 150 in taiko and 200 in mania.
    pub count_100: u16,
    /// Amount of 50 scores in std and mania, but small fruit in ctb.
    pub count_50: u16,
    /// Amount of gekis in std, but MAX scores (rainbow 300s) in mania.
    pub count_geki: u16,
    /// Amount of katsus in std, but 100 scores in mania.
    pub count_katsu: u16,
    /// Amount of misses in all modes.
    pub count_miss: u16,
    /// The numerical score achieved.
    pub score: u32,
    pub max_combo: u16,
    pub perfect_combo: bool,
    /// The mod combination with which the replay was done.
    pub mods: ModSet,
    /// A string representing a graph of how much life bar did the player have along the beatmap.
    ///
    /// It is a comma-separated list of human-readable entries in the form `<offset>|<life>`, where
    /// `<offset>` is the amount of milliseconds since the start of the song and `<life>` is a
    /// number between 0 and 1 representing the amount of life left.
    pub life_graph: Option<String>,
    /// When was the replay scored.
    pub timestamp: DateTime<Utc>,
    /// Decompressed replay data.
    ///
    /// Only available on standalone `.osr` replays, and if the `compression` feature is enabled
    /// (enabled by default).
    ///
    /// When writing `.osr` files (and `.osr` files only), if the `compression` feature is enabled
    /// and this field is `Some`, these actions will be compressed and written. Otherwise,
    /// `raw_replay_data` will be written instead.
    pub replay_data: Option<Vec<Action>>,
    /// Raw replay data, available on `.osr` files even if the `compression` feature is not enabled.
    ///
    /// When writing, this field is used as a fallback if `replay_data` is `None` or the
    /// `compression` feature is disabled.
    pub raw_replay_data: Option<Vec<u8>>,
    /// Online score id.
    /// Only has a useful value on replays embedded in a `ScoreList`.
    pub online_score_id: u64,
}
impl Replay {
    /// Parse a replay from its raw bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<Replay, Error> {
        replay(bytes, true).map(|(_rem, replay)| replay)
    }

    /// Read a replay from a standalone `.osr` osu! replay file.
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Replay, Error> {
        Self::from_bytes(&fs::read(path)?)
    }

    /// Write the replay to an arbitrary writer, with the given compression level.
    ///
    /// If the compression level is `None` the arbitrary default
    /// `replay::DEFAULT_COMPRESSION_LEVEL` will be used.
    /// If the `compression` feature is disabled this argument has no effect.
    pub fn to_writer<W: Write>(
        &self,
        mut out: W,
        compression_level: Option<u32>,
    ) -> io::Result<()> {
        self.wr_args(
            &mut out,
            Some(compression_level.unwrap_or(DEFAULT_COMPRESSION_LEVEL)),
        )
    }

    /// Similar to `to_writer` but writes the replay to an `osr` file.
    pub fn save<P: AsRef<Path>>(&self, path: P, compression_level: Option<u32>) -> io::Result<()> {
        self.to_writer(BufWriter::new(File::create(path)?), compression_level)
    }
}

pub(crate) fn replay(bytes: &[u8], standalone: bool) -> Result<(&[u8], Replay), Error> {
    let (rem, mode) = map_opt(byte, Mode::from_raw)(bytes)?;
    let (rem, version) = int(rem)?;
    let (rem, beatmap_hash) = opt_string(rem)?;
    let (rem, player_name) = opt_string(rem)?;
    let (rem, replay_hash) = opt_string(rem)?;
    let (rem, count_300) = short(rem)?;
    let (rem, count_100) = short(rem)?;
    let (rem, count_50) = short(rem)?;
    let (rem, count_geki) = short(rem)?;
    let (rem, count_katsu) = short(rem)?;
    let (rem, count_miss) = short(rem)?;
    let (rem, score) = int(rem)?;
    let (rem, max_combo) = short(rem)?;
    let (rem, perfect_combo) = boolean(rem)?;
    let (rem, mods) = map(int, ModSet::from_bits)(rem)?;
    let (rem, life_graph) = opt_string(rem)?;
    let (rem, timestamp) = datetime(rem)?;

    let (rem, raw_replay_data) = if standalone {
        map(length_data(int), Some)(rem)?
    } else {
        let (rem, _tag) = tag(&[0xff, 0xff, 0xff, 0xff])(rem)?;

        (rem, None)
    };

    let replay_data = parse_replay_data(raw_replay_data)?;
    let (rem, online_score_id) = long(rem)?;

    let replay = Replay {
        mode,
        version,
        beatmap_hash,
        player_name,
        replay_hash,
        count_300,
        count_100,
        count_50,
        count_geki,
        count_katsu,
        count_miss,
        score,
        max_combo,
        perfect_combo,
        mods,
        life_graph,
        timestamp,
        replay_data,
        raw_replay_data: raw_replay_data.map(ToOwned::to_owned),
        online_score_id,
    };

    Ok((rem, replay))
}
writer!(Replay [this,out,compress_data: Option<u32>] {
    this.mode.raw().wr(out)?;
    this.version.wr(out)?;
    this.beatmap_hash.wr(out)?;
    this.player_name.wr(out)?;
    this.replay_hash.wr(out)?;
    this.count_300.wr(out)?;
    this.count_100.wr(out)?;
    this.count_50.wr(out)?;
    this.count_geki.wr(out)?;
    this.count_katsu.wr(out)?;
    this.count_miss.wr(out)?;
    this.score.wr(out)?;
    this.max_combo.wr(out)?;
    this.perfect_combo.wr(out)?;
    this.mods.bits().wr(out)?;
    this.life_graph.wr(out)?;
    this.timestamp.wr(out)?;
    if let Some(compression_level) = compress_data {
        write_replay_data(
            this.replay_data.as_deref(),
            this.raw_replay_data.as_deref(),
            out,
            compression_level
        )?;
    }else{
        0xffffffff_u32.wr(out)?;
    }
    this.online_score_id.wr(out)?;
});

/// Represents a single action within a replay.
/// The meaning of an action depends on the gamemode of the replay, but all actions
/// contain:
///
/// - An integral amount of milliseconds elapsed since the last action, `delta`.
/// - 3 pieces of floating-point payload: `x`, `y` and `z`.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Action {
    /// The amount of milliseconds since the last action.
    pub delta: i64,
    /// First bit of payload in the action.
    ///
    /// In standard:
    /// Represents the `x` coordinate of the mouse, from `0` to `512`.
    ///
    /// In mania:
    /// Represents the bitwise combination of buttons pressed.
    pub x: f32,
    /// Second bit of payload in the action.
    ///
    /// In standard:
    /// Represents the `y` coordinate of the mouse, from `0` to `384`.
    pub y: f32,
    /// Third bit of payload in the action.
    ///
    /// In standard:
    /// Represents the bitwise combination of buttons pressed.
    pub z: f32,
}
impl Action {
    /// Get the pressed osu!standard buttons.
    pub fn std_buttons(&self) -> StandardButtonSet {
        StandardButtonSet::from_bits(self.z as u32)
    }

    /// Get the pressed osu!mania buttons.
    pub fn mania_buttons(&self) -> ManiaButtonSet {
        ManiaButtonSet::from_bits(self.x as u32)
    }
}

#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum StandardButton {
    MousePrimary,
    MouseSecondary,
    KeyPrimary,
    KeySecondary,
}
impl StandardButton {
    pub fn raw(&self) -> u32 {
        *self as u32
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct StandardButtonSet(pub u32);
impl StandardButtonSet {
    pub fn bits(self) -> u32 {
        self.0
    }
    pub fn from_bits(bits: u32) -> StandardButtonSet {
        StandardButtonSet(bits)
    }

    /// Create a new button combination with no buttons pressed.
    pub fn none() -> StandardButtonSet {
        StandardButtonSet::from_bits(0)
    }

    /// Check whether the combination lists the button as pressed.
    pub fn is_down(&self, button: StandardButton) -> bool {
        self.bits().bit(button.raw() as u32)
    }

    /// Set the pressed status of the given button.
    pub fn set_down(&self, button: StandardButton, is_down: bool) -> StandardButtonSet {
        let mut bits = self.bits();
        bits.set_bit(button.raw() as u32, is_down);
        StandardButtonSet::from_bits(bits)
    }
    /// Set the pressed status of a button to `true`.
    pub fn press(&self, button: StandardButton) -> StandardButtonSet {
        self.set_down(button, true)
    }
    /// Set the pressed status of a button to `false`.
    pub fn release(&self, button: StandardButton) -> StandardButtonSet {
        self.set_down(button, false)
    }
}

/// Any combination of mania buttons being pressed.
///
/// Button indices start from `0`, and go left-to-right.
/// Button indices outside the replay key count should never be down.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ManiaButtonSet(pub u32);
impl ManiaButtonSet {
    pub fn bits(&self) -> u32 {
        self.0
    }
    pub fn from_bits(bits: u32) -> ManiaButtonSet {
        ManiaButtonSet(bits)
    }

    /// Create a new key combination with no keys pressed.
    pub fn none() -> ManiaButtonSet {
        ManiaButtonSet::from_bits(0)
    }

    /// Check whether a certain key is pressed.
    pub fn is_down(&self, button: u32) -> bool {
        self.bits().bit(button)
    }

    /// Set the pressed status of a key.
    pub fn set_down(&self, button: u32, is_down: bool) -> ManiaButtonSet {
        let mut bits = self.bits();
        bits.set_bit(button, is_down);
        ManiaButtonSet::from_bits(bits)
    }
    /// Set the pressed status of a key to `true`.
    pub fn press(&self, button: u32) -> ManiaButtonSet {
        self.set_down(button, true)
    }
    /// Set the pressed status of a key to `false`.
    pub fn release(&self, button: u32) -> ManiaButtonSet {
        self.set_down(button, false)
    }
}

fn parse_replay_data(raw: Option<&[u8]>) -> Result<Option<Vec<Action>>, Error> {
    #[cfg(feature = "compression")]
    {
        if let Some(raw) = raw {
            use xz2::{stream::Stream, write::XzDecoder};

            let mut decoder =
                XzDecoder::new_stream(Vec::new(), Stream::new_lzma_decoder(u64::MAX)?);
            decoder.write_all(raw)?;
            let data = decoder.finish()?;
            let actions = actions(&data)?.1;
            return Ok(Some(actions));
        }
    }
    Ok(None)
}

fn write_replay_data<W: Write>(
    actions: Option<&[Action]>,
    raw: Option<&[u8]>,
    out: &mut W,
    compression_level: u32,
) -> io::Result<()> {
    let mut raw = raw.as_deref();
    let compress_buf;
    //Compress if it's enabled and available
    #[cfg(feature = "compression")]
    {
        if let Some(actions) = actions {
            use xz2::{
                stream::{LzmaOptions, Stream},
                write::XzEncoder,
            };
            let mut encoder = XzEncoder::new_stream(
                Vec::new(),
                Stream::new_lzma_encoder(&LzmaOptions::new_preset(compression_level)?)?,
            );
            for action in actions.iter() {
                action.wr(&mut encoder)?;
            }
            compress_buf = encoder.finish()?;
            raw = Some(&compress_buf[..]);
        }
    }
    let raw = raw.unwrap_or_default();
    //Prefix the data with its length
    (raw.len() as u32).wr(out)?;
    out.write_all(raw)?;
    Ok(())
}

// Parse the plaintext list of actions.
fn actions(bytes: &[u8]) -> IResult<&[u8], Vec<Action>> {
    many0(action)(bytes)
}

fn action(bytes: &[u8]) -> IResult<&[u8], Action> {
    let (rem, delta) = number(bytes)?;
    let (rem, _tag) = tag(b"|")(rem)?;
    let (rem, x) = number(rem)?;
    let (rem, _tag) = tag(b"|")(rem)?;
    let (rem, y) = number(rem)?;
    let (rem, _tag) = tag(b"|")(rem)?;
    let (rem, z) = number(rem)?;
    let (rem, _tag) = tag(b",")(rem)?;

    let action = Action {
        delta: delta as i64,
        x: x as f32,
        y: y as f32,
        z: z as f32,
    };

    Ok((rem, action))
}

writer!(Action [this,out] {
    write!(out, "{}|{}|{}|{},", this.delta,this.x,this.y,this.z)?;
});

// Parse a textually encoded decimal number.
fn number(bytes: &[u8]) -> IResult<&[u8], f64> {
    let (rem, sign) = opt(tag(b"-"))(bytes)?;
    let (rem, whole) = take_while1(|b: u8| b.is_ascii_digit())(rem)?;
    let (rem, decimal) = opt(number_bytes)(rem)?;

    let mut num = 0.0;

    for byte in whole {
        num *= 10.0;
        num += (*byte - b'0') as f64;
    }

    if let Some(decimal) = decimal {
        let mut value = 1.0;

        for byte in decimal {
            value /= 10.0;
            num += (*byte - b'0') as f64 * value;
        }
    }

    if sign.is_some() {
        num *= -1.0
    }

    Ok((rem, num))
}

fn number_bytes(bytes: &[u8]) -> IResult<&[u8], &[u8]> {
    let (rem, _tag) = tag(b".")(bytes)?;

    take_while(|b: u8| b.is_ascii_digit())(rem)
}
