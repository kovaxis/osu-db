//! A simple command-line tool for decoding and encoding osu binary files to plaintext.
//!
//! **NOTE**: Currently encoding from plaintext is unsupported by osu-db.

use chrono::{DateTime, Utc};
use failure::{bail, ensure, err_msg, format_err, AsFail, Fallible, ResultExt};
use fxhash::{FxHashMap as HashMap, FxHashSet as HashSet};
use osu_db::{
    collection::Collection,
    listing::{Beatmap, RankedStatus},
    replay::Action,
    score::BeatmapScores,
    CollectionList, Listing, Replay, ScoreList,
};
use std::{
    cmp::{self, Reverse},
    env,
    ffi::OsStr,
    fmt::{self, Write},
    fs,
    hash::{Hash, Hasher},
    mem,
    path::{Path, PathBuf},
};

const HELP_MSG: &'static str = r#"
Usage: osuconv [OPTIONS]

Available options:
    -i  --in            An input file with an optional format. At least one input must be supplied.
    -i  --out           An output file with an optional format. At most one output can be supplied.
    -h  --help          Print this message.
    -u  --union         Deduplicate items that appear in more than one file. This is the default.
    -n  --intersection  Remove any items that do not appear in all input files.
    -d  --difference    Remove any items in the first file that appear in the other files.

By default the output filename is the same as the input with the extension changed to the target format.

`osuconv` will try to guess input and output formats, but for finer control you can explicitly specify formats
after file paths, separated by a '?' character.
Available formats:
    `listing`, `collection`, `score`, `replay`: Type of osu! file.
    `bin`, `ron`, `json`: Encoding format.
Note that encodings and filetypes are not exclusive (eg. `osu!.db?listing?bin` is a valid path).
"#;

struct PrintErr<F: AsFail>(pub F);
impl<F: AsFail> fmt::Display for PrintErr<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let fail = self.0.as_fail();
        write!(f, "{}", fail)?;
        for cause in fail.iter_causes() {
            writeln!(f)?;
            write!(f, "  caused by: {}", cause)?;
        }
        Ok(())
    }
}

/// Rank a beatmap by correctness, such that if there is a collision one can be chosen over the
/// other.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
struct BmCorrectness<'a> {
    folder_name: PreferPresent<'a>,
    hash: PreferPresent<'a>,
    file_name: PreferPresent<'a>,
    beatmap_id: PreferPositive<i32>,
    beatmapset_id: PreferPositive<i32>,
    difficulty_name: PreferPresent<'a>,
    ranked_status: PreferRanked,
    audio: PreferPresent<'a>,
    last_online_check: PreferLarger<DateTime<Utc>>,
    last_played: PreferLarger<Option<DateTime<Utc>>>,
}
impl<'a> BmCorrectness<'a> {
    fn new(bm: &'a Beatmap) -> Self {
        Self {
            folder_name: PreferPresent(&bm.folder_name),
            hash: PreferPresent(&bm.hash),
            file_name: PreferPresent(&bm.file_name),
            beatmap_id: PreferPositive(bm.beatmap_id),
            beatmapset_id: PreferPositive(bm.beatmapset_id),
            difficulty_name: PreferPresent(&bm.difficulty_name),
            ranked_status: PreferRanked(bm.status),
            audio: PreferPresent(&bm.audio),
            last_online_check: PreferLarger(bm.last_online_check),
            last_played: PreferLarger(bm.last_played),
        }
    }
}

macro_rules! impl_ord_by_key {
    {
        impl[$($bounds:tt)*] * for $ty:ty {
            fn key($val:ident) $key:expr
        }
    } => {
        impl<$($bounds)*> Ord for $ty {
            fn cmp(&self, rhs: &Self) -> cmp::Ordering {
                let l = {
                    let $val = self;
                    $key
                };
                let r = {
                    let $val = rhs;
                    $key
                };
                l.cmp(&r)
            }
        }
        impl<$($bounds)*> PartialOrd for $ty {
            fn partial_cmp(&self, rhs: &Self) -> Option<cmp::Ordering> {
                Some(self.cmp(rhs))
            }
        }
        impl<$($bounds)*> PartialEq for $ty {
            fn eq(&self, rhs: &Self) -> bool {
                self.cmp(rhs) == cmp::Ordering::Equal
            }
        }
        impl<$($bounds)*> Eq for $ty {}
    };
}

#[derive(Debug, Clone)]
struct PreferRanked(RankedStatus);
impl_ord_by_key! {
    impl[] * for PreferRanked {
        fn key(ranked) {
            use osu_db::listing::RankedStatus::*;
            match ranked.0 {
                Unknown => 0,
                Unsubmitted => 1,
                PendingWipGraveyard => 1,
                Approved => 2,
                Qualified => 2,
                Loved => 3,
                Ranked => 4,
            }
        }
    }
}
impl From<RankedStatus> for PreferRanked {
    fn from(inner: RankedStatus) -> Self {
        Self(inner)
    }
}

#[derive(Debug, Clone)]
struct PreferPositive<T>(T);
impl_ord_by_key! {
    impl[T: PartialOrd+Default] * for PreferPositive<T> {
        fn key(p) {
            (p.0 >= T::default()) as u8
        }
    }
}
impl<T> From<T> for PreferPositive<T> {
    fn from(inner: T) -> Self {
        Self(inner)
    }
}

#[derive(Clone)]
struct PreferPresent<'a>(&'a Option<String>);
impl fmt::Debug for PreferPresent<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "PreferPresent({})",
            if self.0.is_some() {
                "Present"
            } else {
                "NotPresent"
            }
        )
    }
}
impl_ord_by_key! {
    impl[] * for PreferPresent<'_> {
        fn key(p) {
            p.0.is_some() as u8
        }
    }
}
impl<'a> From<&'a Option<String>> for PreferPresent<'a> {
    fn from(inner: &'a Option<String>) -> Self {
        Self(inner)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
struct PreferSmaller<T>(Reverse<T>);
impl<T: fmt::Debug> fmt::Debug for PreferSmaller<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "PreferSmaller(")?;
        fmt::Debug::fmt(&(self.0).0, f)?;
        write!(f, ")")?;
        Ok(())
    }
}
impl<T> From<T> for PreferSmaller<T> {
    fn from(inner: T) -> Self {
        Self(Reverse(inner))
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
struct PreferLarger<T>(T);
impl<T: fmt::Debug> fmt::Debug for PreferLarger<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "PreferLarger(")?;
        fmt::Debug::fmt(&self.0, f)?;
        write!(f, ")")?;
        Ok(())
    }
}
impl<T> From<T> for PreferLarger<T> {
    fn from(inner: T) -> Self {
        Self(inner)
    }
}

#[derive(Debug, Clone)]
enum InMemory {
    Listing(Listing),
    Collections(CollectionList),
    Scores(ScoreList),
    Replay(Replay),
}
impl InMemory {
    fn file_type(&self) -> FileType {
        match self {
            InMemory::Listing(_) => FileType::Listing,
            InMemory::Collections(_) => FileType::Collections,
            InMemory::Scores(_) => FileType::Scores,
            InMemory::Replay(_) => FileType::Replay,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum FileType {
    Listing,
    Collections,
    Scores,
    Replay,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum FileFormat {
    Bin,
    Ron,
    Json,
}

fn read(ty: FileType, fmt: FileFormat, bytes: &[u8]) -> Fallible<InMemory> {
    macro_rules! file_types {
        ($($name:ident [$ty:ty]),*) => {{
            match ty {
                $(
                    FileType::$name => InMemory::$name(match fmt {
                        FileFormat::Bin => {
                            <$ty>::from_bytes(bytes)?
                        }
                        FileFormat::Ron => {
                            ron::de::from_bytes(bytes)?
                        }
                        FileFormat::Json => {
                            serde_json::from_reader(bytes)?
                        }
                    }),
                )*
            }
        }}
    }
    Ok(file_types!(
        Listing[Listing],
        Collections[CollectionList],
        Scores[ScoreList],
        Replay[Replay]
    ))
}

fn merge(merge_op: MergeOp, inputs: impl IntoIterator<Item = InMemory>) -> Fallible<InMemory> {
    let inputs = inputs.into_iter().collect::<Vec<_>>();
    let mut ty = None;
    ensure!(
        inputs
            .iter()
            .all(|in_mem| { in_mem.file_type() == *ty.get_or_insert(in_mem.file_type()) }),
        "cannot merge different types of files"
    );
    let ty = ty.ok_or(err_msg("zero inputs"))?;
    if inputs.len() > 1 {
        eprintln!("merging {} {:?} files", inputs.len(), ty);
    }
    macro_rules! prepare {
        ($name:ident => |$val:ident| $get_list:expr) => {{
            let mut inputs = inputs
                .into_iter()
                .map(|in_mem| match in_mem {
                    InMemory::$name(d) => d,
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>();
            let tmp = {
                let $val = &mut inputs[0];
                mem::replace(&mut $get_list, Default::default())
            };
            let out = inputs[0].clone();
            {
                let $val = &mut inputs[0];
                let refmut = &mut $get_list;
                *refmut = tmp;
            }
            (inputs, out)
        }};
    }
    let merge_meta_add = |idx| match merge_op {
        MergeOp::Or => 1,
        MergeOp::And => 1,
        MergeOp::Diff => {
            if idx == 0 {
                0
            } else {
                1
            }
        }
    };
    fn merge_filter<T>(
        (merge_op, listing_count): (MergeOp, usize),
        (meta, item): (i32, T),
    ) -> Option<T> {
        let include = match merge_op {
            MergeOp::Or => true,
            MergeOp::And => meta as usize == listing_count,
            MergeOp::Diff => meta == 0,
        };
        if include {
            Some(item)
        } else {
            None
        }
    }
    let merge_ctx = (merge_op, inputs.len());
    let out = match ty {
        FileType::Listing => {
            let (listings, mut out) = prepare!(Listing => |listing| listing.beatmaps);
            out.folder_count = 0;
            let mut by_hash = HashMap::default();
            let mut no_hash = Vec::new();
            let mut folders = HashSet::default();
            for (i, listing) in listings.into_iter().enumerate() {
                eprintln!("processing listing {}", i);
                eprintln!("  version: {}", listing.version);
                eprintln!("  beatmaps: {}", listing.beatmaps.len());
                eprintln!("  folder_count: {}", listing.folder_count);
                eprintln!(
                    "  player_name: '{}'",
                    listing.player_name.unwrap_or_default()
                );
                for bm in listing.beatmaps {
                    if let Some(fname) = &bm.folder_name {
                        folders.insert(fname.clone());
                    } else {
                        eprintln!(
                            "  WARNING: beatmap {:?} has no folder name",
                            (&bm.title_ascii, &bm.difficulty_name, &bm.file_name)
                        );
                    }
                    if let Some(hash) = &bm.hash {
                        if let Some((meta, old_bm)) = by_hash.get_mut(&hash[..]) {
                            let new = BmCorrectness::new(&bm);
                            let old = BmCorrectness::new(old_bm);
                            let replace = new > old;
                            println!(
                                "found beatmaps with the same hash: new {} old -> {}\n{:?}\n{:?}\n",
                                match new.cmp(&old) {
                                    cmp::Ordering::Less => "<",
                                    cmp::Ordering::Equal => "=",
                                    cmp::Ordering::Greater => ">",
                                },
                                if replace {
                                    "replacing"
                                } else {
                                    "not replacing"
                                },
                                new,
                                old
                            );
                            if replace {
                                *old_bm = bm;
                            }
                            *meta += merge_meta_add(i);
                        } else {
                            let meta = merge_meta_add(i);
                            by_hash.insert(hash.clone(), (meta, bm));
                        }
                    } else {
                        eprintln!(
                            "  WARNING: beatmap {:?} has no hash",
                            (&bm.title_ascii, &bm.difficulty_name, &bm.folder_name)
                        );
                        no_hash.push(bm);
                    }
                }
            }
            out.beatmaps = by_hash
                .drain()
                .filter_map(|(_hash, bm)| merge_filter(merge_ctx, bm))
                .chain(no_hash.into_iter())
                .collect();
            out.folder_count = folders.len() as u32;
            eprintln!(
                "output listing has {} beatmaps and {} folders",
                out.beatmaps.len(),
                out.folder_count
            );
            InMemory::Listing(out)
        }
        FileType::Collections => {
            let (lists, mut out) = prepare!(Collections => |list| list.collections);
            let mut by_name: HashMap<String, (i32, HashMap<String, i32>)> = HashMap::default();
            //Merge everything into hashmaps
            for (i, list) in lists.into_iter().enumerate() {
                eprintln!("processing collection listing {}", i);
                eprintln!("  version: {}", list.version);
                eprintln!("  collections: {}", list.collections.len());
                for (j, collection) in list.collections.into_iter().enumerate() {
                    if let Some(collection_name) = collection.name {
                        eprintln!("  processing collection {} '{}'", j, collection_name);
                        eprintln!("    beatmaps: {}", collection.beatmap_hashes.len());
                        let (collection_meta, out_collection) =
                            by_name.entry(collection_name).or_default();
                        *collection_meta += merge_meta_add(i);
                        for (k, bm_hash) in collection.beatmap_hashes.into_iter().enumerate() {
                            if let Some(bm_hash) = bm_hash {
                                *out_collection.entry(bm_hash).or_default() += merge_meta_add(i);
                            } else {
                                println!(
                                    "    WARNING: beatmap {} in collection has no hash?? skipping",
                                    k
                                );
                            }
                        }
                    } else {
                        println!("  WARNING: collection {} has no name, skipping", j);
                    }
                }
            }
            //Rebuild collection list
            for (name, (collection_meta, collection)) in by_name {
                let out_collection = Collection {
                    name: Some(name),
                    beatmap_hashes: collection
                        .into_iter()
                        .filter_map(|(hash, meta)| merge_filter(merge_ctx, (meta, Some(hash))))
                        .collect(),
                };
                if !out_collection.beatmap_hashes.is_empty()
                    || merge_filter(merge_ctx, (collection_meta, ())).is_some()
                {
                    out.collections.push(out_collection);
                }
            }
            //Print info
            eprintln!(
                "output collection list has {} collections",
                out.collections.len()
            );
            for collection in out.collections.iter() {
                eprintln!(
                    "  '{}': {} beatmaps",
                    collection.name.as_deref().unwrap_or_default(),
                    collection.beatmap_hashes.len()
                );
            }
            InMemory::Collections(out)
        }
        FileType::Scores => {
            let (lists, mut out) = prepare!(Scores => |list| list.beatmaps);
            //Merge replays into a hashmap of hashsets
            let mut bms_by_hash: HashMap<String, (i32, HashMap<ReplayWrapper, i32>)> =
                HashMap::default();
            //Hashes all NaNs as equal and all zeroes as equal
            fn hash_f32<H: Hasher>(f: f32, h: &mut H) {
                if f.is_nan() {
                    h.write_u32(0xffffffff);
                } else if f == 0. {
                    h.write_u32(0);
                } else {
                    h.write_u32(f.to_bits());
                }
            }
            //Compares all NaNs as equal
            fn eq_f32(lhs: f32, rhs: f32) -> bool {
                if lhs.is_nan() && rhs.is_nan() {
                    true
                } else {
                    lhs == rhs
                }
            }
            struct ActionsEq<'a>(&'a Option<Vec<Action>>);
            impl Hash for ActionsEq<'_> {
                fn hash<H: Hasher>(&self, h: &mut H) {
                    h.write_u8(self.0.is_some() as u8);
                    if let Some(actions) = self.0 {
                        h.write_usize(actions.len());
                        for action in actions {
                            let Action { delta, x, y, z } = action;
                            h.write_i64(*delta);
                            hash_f32(*x, h);
                            hash_f32(*y, h);
                            hash_f32(*z, h);
                        }
                    }
                }
            }
            impl PartialEq for ActionsEq<'_> {
                fn eq(&self, rhs: &Self) -> bool {
                    match (self.0.as_deref(), rhs.0.as_deref()) {
                        (Some(lhs), Some(rhs)) => {
                            if lhs.len() != rhs.len() {
                                return false;
                            }
                            lhs.iter().zip(rhs.iter()).all(|(lhs, rhs)| {
                                lhs.delta == rhs.delta
                                    && eq_f32(lhs.x, rhs.x)
                                    && eq_f32(lhs.y, rhs.y)
                                    && eq_f32(lhs.z, rhs.z)
                            })
                        }
                        (None, None) => true,
                        _ => false,
                    }
                }
            }
            impl Eq for ActionsEq<'_> {}
            #[derive(Debug, Clone)]
            struct ReplayWrapper(Replay);
            fn get_important<'a>(replay: &'a ReplayWrapper) -> impl Eq + Hash + 'a {
                let Replay {
                    version: _,
                    mode,
                    beatmap_hash,
                    player_name,
                    replay_hash: _,
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
                    raw_replay_data: _,
                    online_score_id,
                } = &replay.0;
                fn s(s: &Option<String>, normalize: bool) -> Option<&str> {
                    if normalize && s.is_none() {
                        Some("")
                    } else {
                        s.as_deref()
                    }
                }
                (
                    mode,
                    [
                        s(beatmap_hash, false),
                        s(player_name, false),
                        s(life_graph, true),
                    ],
                    [
                        count_300,
                        count_100,
                        count_50,
                        count_geki,
                        count_katsu,
                        count_miss,
                        max_combo,
                    ],
                    mods,
                    score,
                    perfect_combo,
                    timestamp,
                    ActionsEq(replay_data),
                    online_score_id,
                )
            };
            impl Hash for ReplayWrapper {
                fn hash<H: Hasher>(&self, h: &mut H) {
                    get_important(self).hash(h);
                }
            }
            impl Eq for ReplayWrapper {}
            impl PartialEq for ReplayWrapper {
                fn eq(&self, rhs: &Self) -> bool {
                    get_important(self) == get_important(&rhs)
                }
            }
            for (i, list) in lists.into_iter().enumerate() {
                eprintln!("processing scorelist {}", i);
                eprintln!("  version: {}", list.version);
                eprintln!("  beatmaps: {}", list.beatmaps.len());
                eprintln!(
                    "  total scores: {}",
                    list.beatmaps
                        .iter()
                        .map(|bm| bm.scores.len())
                        .sum::<usize>()
                );
                for (j, bm) in list.beatmaps.into_iter().enumerate() {
                    if let Some(bm_hash) = bm.hash {
                        let (meta, bm_scores) = bms_by_hash.entry(bm_hash).or_default();
                        *meta += merge_meta_add(i);
                        for (_k, replay) in bm.scores.into_iter().enumerate() {
                            println!("    {:?}", replay);
                            let entry = bm_scores.entry(ReplayWrapper(replay));
                            if let std::collections::hash_map::Entry::Occupied(_) = &entry {
                                println!("      found duplicate");
                            }
                            *entry.or_default() += merge_meta_add(i);
                        }
                    } else {
                        eprintln!("    WARNING: beatmap {} has no hash, skipping", j);
                    }
                }
            }
            //Rebuild scorelist
            for (hash, (replays_meta, replays)) in bms_by_hash {
                let bm = {
                    let mut bm = BeatmapScores {
                        hash: Some(hash),
                        scores: replays
                            .into_iter()
                            .filter_map(|(wrapper, meta)| {
                                merge_filter(merge_ctx, (meta, wrapper.0))
                            })
                            .collect(),
                    };
                    for replay in bm.scores.iter_mut() {
                        //Remove replay data and leave raw replay data only, to prevent any
                        //unintended changes by recompression
                        replay.replay_data = None;
                    }
                    bm.scores
                        .sort_unstable_by_key(|replay| Reverse(replay.score));
                    bm
                };
                if !bm.scores.is_empty() || merge_filter(merge_ctx, (replays_meta, ())).is_some() {
                    out.beatmaps.push(bm);
                }
            }
            //Print info
            eprintln!("output scorelist:");
            eprintln!("  beatmaps: {}", out.beatmaps.len());
            eprintln!(
                "  total scores: {}",
                out.beatmaps.iter().map(|bm| bm.scores.len()).sum::<usize>()
            );
            InMemory::Scores(out)
        }
        FileType::Replay => {
            if inputs.len() != 1 {
                bail!("replays cannot be merged")
            } else {
                inputs.into_iter().next().unwrap()
            }
        }
    };
    Ok(out)
}

fn write(fmt: FileFormat, data: InMemory) -> Fallible<Vec<u8>> {
    macro_rules! file_types {
        ($($name:ident [$($args:tt)*]),*) => {{
            match data {
                $(
                    InMemory::$name(d) => match fmt {
                        FileFormat::Bin => {
                            let mut out = Vec::new();
                            d.to_writer(&mut out $($args)*)?;
                            out
                        }
                        FileFormat::Ron => {
                            ron::ser::to_string_pretty(&d, Default::default())?.into_bytes()
                        }
                        FileFormat::Json => {
                            serde_json::to_vec_pretty(&d)?
                        }
                    },
                )*
            }
        }}
    }
    Ok(file_types!(Listing[], Collections[], Scores[], Replay[, None]))
}

fn get_extension(ty: FileType, format: FileFormat) -> &'static str {
    use self::{FileFormat::*, FileType::*};
    match (ty, format) {
        (Replay, Bin) => "osr",
        (_, Bin) => "db",
        (_, Ron) => "txt",
        (_, Json) => "json",
    }
}

fn unpack_path(path: Option<&Path>) -> (&str, &str) {
    fn flatten_osstr(opt: Option<&OsStr>) -> &str {
        opt.unwrap_or_default().to_str().unwrap_or_default()
    }
    path.map(|path| {
        (
            flatten_osstr(path.file_stem()),
            flatten_osstr(path.extension()),
        )
    })
    .unwrap_or(("", ""))
}

fn guess_read_input(
    in_path: &Path,
    in_bytes: &[u8],
    in_ty: Option<FileType>,
    in_fmt: Option<FileFormat>,
) -> Fallible<(InMemory, FileFormat)> {
    let (in_name, in_ext) = unpack_path(Some(in_path));
    //Guess input
    let mut check_order = vec![];
    for file_format in [FileFormat::Bin, FileFormat::Ron, FileFormat::Json].iter() {
        for file_type in [
            FileType::Listing,
            FileType::Scores,
            FileType::Collections,
            FileType::Replay,
        ]
        .iter()
        {
            check_order.push((*file_format, *file_type));
        }
    }
    if let Some(in_ty) = in_ty {
        check_order.retain(|(_f, t)| *t == in_ty);
    }
    if let Some(in_fmt) = in_fmt {
        check_order.retain(|(f, _t)| *f == in_fmt);
    }
    macro_rules! priorize {
        (Format($fmt:ident) => $prio:expr) => {{
            check_order.sort_by_key(|(f, _ty)| {
                let $fmt = *f;
                (!($prio)) as u8
            });
        }};
        (Type($ty:ident) => $prio:expr) => {{
            check_order.sort_by_key(|(_fmt, t)| {
                let $ty = *t;
                (!($prio)) as u8
            });
        }};
    }
    //Priorize filetype
    match (in_name, in_ext) {
        ("osu!", "db") => priorize!(Type(ty) => ty == FileType::Listing),
        ("collection", "db") => priorize!(Type(ty) => ty == FileType::Collections),
        ("scores", "db") => priorize!(Type(ty) => ty == FileType::Scores),
        (_, "osr") => priorize!(Type(ty) => ty == FileType::Replay),
        _ => {}
    }
    //Prioritize fileformat
    match in_ext {
        "db" => priorize!(Format(fmt) => fmt == FileFormat::Bin),
        "osr" => priorize!(Format(fmt) => fmt == FileFormat::Bin),
        "txt" => priorize!(Format(fmt) => fmt == FileFormat::Ron || fmt == FileFormat::Json),
        "ron" => priorize!(Format(fmt) => fmt == FileFormat::Ron),
        "json" => priorize!(Format(fmt) => fmt == FileFormat::Json),
        _ => {}
    }
    //Actually try to parse the different options
    let mut errors = Vec::new();
    let mut in_info = None;
    for &(fmt, ty) in check_order.iter() {
        match read(ty, fmt, in_bytes) {
            Ok(in_mem) => {
                in_info = Some((in_mem, fmt));
                break;
            }
            Err(err) => {
                errors.push((fmt, ty, err));
            }
        }
    }
    let (in_data, in_fmt) = in_info.ok_or_else(|| {
        let mut msg = format!("could not recognize input file:");
        for (fmt, ty, err) in errors.iter() {
            write!(msg, "\n  not a ({:?}/{:?}) file: {}", ty, fmt, err).unwrap();
        }
        err_msg(msg)
    })?;
    Ok((in_data, in_fmt))
}

fn guess_output(
    in_path: &Path,
    in_ty: FileType,
    in_fmt: FileFormat,
    out_path: Option<&Path>,
    out_ty: Option<FileType>,
    out_fmt: Option<FileFormat>,
) -> (FileType, FileFormat, PathBuf) {
    let (out_name, out_ext) = unpack_path(out_path);
    //Guess type
    let out_ty = out_ty
        .or_else(|| match (out_name, out_ext) {
            (_, "osr") => Some(FileType::Replay),
            (_, _) => None,
        })
        .unwrap_or(in_ty);
    //Guess format
    let out_fmt = out_fmt
        .or_else(|| {
            Some(match out_ext {
                "db" | "osr" => FileFormat::Bin,
                "txt" | "ron" => FileFormat::Ron,
                "json" => FileFormat::Json,
                _ => return None,
            })
        })
        .unwrap_or_else(|| match in_fmt {
            FileFormat::Bin => FileFormat::Ron,
            FileFormat::Ron | FileFormat::Json => FileFormat::Bin,
        });
    //Guess path (if not available already)
    let out_path = out_path.map(|path| path.to_path_buf()).unwrap_or_else(|| {
        let mut out_path = in_path.to_path_buf();
        let ext = get_extension(out_ty, out_fmt);
        out_path.set_extension(ext);
        let mut file_name_base = out_path.file_stem().unwrap_or_default().to_os_string();
        file_name_base.push("_out");
        let mut idx = 0;
        while out_path.exists() {
            let mut file_name = file_name_base.clone();
            if idx > 0 {
                file_name.push(format!("{}", idx));
            }
            idx += 1;
            out_path.set_file_name(&file_name);
            out_path.set_extension(ext);
        }
        out_path
    });
    (out_ty, out_fmt, out_path)
}

#[derive(Debug, Clone)]
struct InputOpt {
    path: PathBuf,
    ty: Option<FileType>,
    fmt: Option<FileFormat>,
}

#[derive(Debug, Copy, Clone)]
enum MergeOp {
    Or,
    And,
    Diff,
}

#[derive(Debug, Clone)]
struct Options {
    inputs: Vec<InputOpt>,
    output_path: Option<PathBuf>,
    output_ty: Option<FileType>,
    output_fmt: Option<FileFormat>,
    merge_op: MergeOp,
}
impl Options {
    fn parse() -> Fallible<Option<Options>> {
        let mut inputs = Vec::new();
        let mut output = (None, None, None);
        let mut merge_op = MergeOp::Or;

        let mut args = env::args().skip(1);
        fn parse_path(
            mut path: String,
        ) -> Fallible<(PathBuf, Option<FileType>, Option<FileFormat>)> {
            let mut ty = None;
            let mut fmt = None;
            while let Some(sep) = path.rfind('?') {
                match &path[sep + 1..] {
                    "bin" => fmt = Some(FileFormat::Bin),
                    "ron" | "txt" => fmt = Some(FileFormat::Ron),
                    "json" => fmt = Some(FileFormat::Json),
                    "listing" | "osu!" => ty = Some(FileType::Listing),
                    "collection" | "collections" => ty = Some(FileType::Collections),
                    "score" | "scores" => ty = Some(FileType::Scores),
                    "osr" | "replay" => ty = Some(FileType::Replay),
                    unknown => bail!("unknown file format/type '{}'", unknown),
                }
                path.truncate(sep);
            }
            Ok((path.into(), ty, fmt))
        }
        let mut any_arg = false;
        while let Some(opt) = args.next() {
            any_arg = true;
            //Check option
            match &opt[..] {
                "-h" | "--help" => eprint!("{}", HELP_MSG),
                "-i" | "--input" => {
                    let (path, ty, fmt) =
                        parse_path(args.next().ok_or(err_msg("expected input path"))?)?;
                    inputs.push(InputOpt { path, ty, fmt });
                }
                "-o" | "--output" => {
                    if output.0.is_some() {
                        bail!("too many outputs");
                    }
                    let (path, ty, fmt) =
                        parse_path(args.next().ok_or(err_msg("expected output path"))?)?;
                    output = (Some(path), ty, fmt);
                }
                "-u" | "--union" => {
                    merge_op = MergeOp::Or;
                }
                "-n" | "--intersection" => {
                    merge_op = MergeOp::And;
                }
                "-d" | "--difference" => {
                    merge_op = MergeOp::Diff;
                }
                _ => bail!(format_err!("unknown option '{}'", opt)),
            }
        }
        //Error on missing inputs
        ensure!(!inputs.is_empty(), "expected at least one input path");
        if !inputs
            .iter()
            .all(|InputOpt { path, .. }| path.extension() == inputs[0].path.extension())
        {
            eprintln!("warning: inputs should have matching extensions?");
        }
        //Print help message if no arguments were given
        if any_arg {
            let (output_path, output_ty, output_fmt) = output;
            Ok(Some(Options {
                inputs,
                merge_op,
                output_path,
                output_ty,
                output_fmt,
            }))
        } else {
            eprint!("{}", HELP_MSG);
            Ok(None)
        }
    }
}

fn run() -> Fallible<()> {
    let opt = match Options::parse()? {
        Some(opt) => opt,
        None => return Ok(()),
    };

    //Read and parse inputs
    let inputs = opt
        .inputs
        .iter()
        .map(|InputOpt { path, ty, fmt }| -> Fallible<_> {
            eprintln!(
                "reading input at \"{}\" with format {:?}/{:?}",
                path.display(),
                ty,
                fmt
            );
            let bytes = fs::read(path)
                .with_context(|_| format_err!("failed to read input at \"{}\"", path.display()))?;
            let (in_mem, fmt) = guess_read_input(path, &bytes, *ty, *fmt)?;
            eprintln!(
                "  determined file format to be {:?}/{:?}",
                in_mem.file_type(),
                fmt
            );
            Ok((in_mem, fmt))
        })
        .collect::<Result<Vec<_>, _>>()?;
    ensure!(!inputs.is_empty(), "got no inputs");

    //Get output type and location
    let (main_in, main_in_fmt) = &inputs[0];
    let (out_ty, out_fmt, out_path) = guess_output(
        &opt.inputs[0].path,
        main_in.file_type(),
        *main_in_fmt,
        opt.output_path.as_deref(),
        opt.output_ty,
        opt.output_fmt,
    );

    //Merge inputs
    let merged = merge(
        opt.merge_op,
        inputs.into_iter().map(|(in_mem, _fmt)| in_mem),
    )?;

    //Serialize output
    eprintln!("serializing as a {:?}/{:?} file", out_ty, out_fmt);
    ensure!(
        merged.file_type() == out_ty,
        "cannot convert input filetype to output filetype ({:?} -> {:?})",
        merged.file_type(),
        out_ty
    );
    let out_bytes = write(out_fmt, merged)?;

    //Write output
    eprintln!("writing output to \"{}\"", out_path.display());
    fs::write(&out_path, &out_bytes[..]).with_context(|_| {
        format_err!("failed to write output file at \"{}\"", out_path.display())
    })?;

    Ok(())
}

pub fn main() {
    match run() {
        Ok(()) => {}
        Err(err) => eprintln!("fatal error: {}", PrintErr(err)),
    }
}
