//! Parsing for the `scores.db` osu file, which contains partial replay data locally.

use crate::{prelude::*,replay::{Replay,replay}};

///A score database, usually coming from a `scores.db` file.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Debug,Clone)]
pub struct ScoreList {
    pub version: u32,
    pub beatmaps: Vec<BeatmapScores>,
}
impl ScoreList {
    ///Read a score database from its raw bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<ScoreList, NomErr<&[u8]>> {
        scores(bytes).map(|(_rem,scores)| scores)
    }
    
    ///Read a score database from a `scores.db` file.
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<ScoreList, Error> {
        let bytes=fs::read(path)?;
        Ok(ScoreList::from_bytes(&bytes)?)
    }
}

///The scores for a single beatmap.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Debug,Clone)]
pub struct BeatmapScores {
    ///The beatmap hash.
    ///Should be redundant with the individual replay hashes.
    pub hash: Option<String>,
    ///All the scored replays for this beatmap.
    pub scores: Vec<Replay>,
}

named!(scores<&[u8], ScoreList>, do_parse!(
    version: int >>
    beatmaps: length_count!(int, do_parse!(
        hash: opt_string >>
        scores: length_count!(int, call!(replay, false)) >>
        (BeatmapScores{ hash, scores })
    )) >>
    (ScoreList{ version, beatmaps })
));
