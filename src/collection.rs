//! Parsing for the `collection.db` file, containing all user collections.

use crate::prelude::*;

/// A structure representing the `collection.db` file.
/// Contains a list of collections.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct CollectionList {
    pub version: u32,
    pub collections: Vec<Collection>,
}
impl CollectionList {
    /// Read a collection list from its raw bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<CollectionList, Error> {
        Ok(collections(bytes).map(|(_rem, collections)| collections)?)
    }

    /// Read a collection list from a `collection.db` file.
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<CollectionList, Error> {
        Self::from_bytes(&fs::read(path)?)
    }

    /// Writes the collection list to an arbitrary writer.
    pub fn to_writer<W: Write>(&self, mut out: W) -> io::Result<()> {
        self.wr(&mut out)
    }

    /// Similar to `to_writer` but writes the collection database to a file (ie. `collection.db`).
    pub fn to_file<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        self.to_writer(BufWriter::new(File::create(path)?))
    }
}

/// A single collection.
/// Contains a list of beatmap hashes that fall within this collection.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct Collection {
    pub name: Option<String>,
    pub beatmap_hashes: Vec<Option<String>>,
}

named!(collections<&[u8], CollectionList>, do_parse!(
    version: int >>
    collections: length_count!(int, do_parse!(
        name: opt_string >>
        beatmap_hashes: length_count!(int, opt_string) >>
        (Collection { name, beatmap_hashes })
    )) >>
    (CollectionList { version, collections })
));
writer!(CollectionList [this,out] {
    this.version.wr(out)?;
    PrefixedList(&this.collections).wr(out)?;
});
writer!(Collection [this,out] {
    this.name.wr(out)?;
    PrefixedList(&this.beatmap_hashes).wr(out)?;
});
