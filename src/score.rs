//! Parsing for the `scores.db` osu file, which contains partial replay data locally.

use crate::{
    prelude::*,
    replay::{replay, Replay},
};

///A score database, usually coming from a `scores.db` file.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Debug, Clone)]
pub struct ScoreList {
    pub version: u32,
    pub beatmaps: Vec<BeatmapScores>,
}
impl ScoreList {
    ///Read a score database from its raw bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<ScoreList, Error> {
        Ok(scores(bytes).map(|(_rem, scores)| scores)?)
    }

    ///Read a score database from a `scores.db` file.
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<ScoreList, Error> {
        Self::from_bytes(&fs::read(path)?)
    }

    ///Write the score database to an arbitrary writer.
    pub fn to_writer<W: Write>(&self, mut out: W) -> io::Result<()> {
        self.wr(&mut out)
    }
    
    ///Similar to `to_writer` but writes the scores to a file.
    pub fn save<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        self.to_writer(BufWriter::new(File::create(path)?))
    }
}

///The scores for a single beatmap.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Debug, Clone)]
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
writer!(ScoreList [this,out] {
    this.version.wr(out)?;
    PrefixedList(&this.beatmaps).wr(out)?;
});
writer!(BeatmapScores [this,out] {
    this.hash.wr(out)?;
    PrefixedList(&this.scores).wr_args(out,None)?;
});
