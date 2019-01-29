//! Parsing for the `collection.db` file, containing all user collections.

use crate::prelude::*;

///A structure representing the `collection.db` file.
///Contains a list of collections.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct CollectionList {
    pub version: u32,
    pub collections: Vec<Collection>,
}
impl CollectionList {
    ///Read a collection list from its raw bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<CollectionList, NomErr<&[u8]>> {
        collections(bytes).map(|(_rem,collections)| collections)
    }
    
    ///Read a collection list from a `collection.db` file.
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<CollectionList, Error> {
        let bytes=fs::read(path)?;
        Ok(CollectionList::from_bytes(&bytes)?)
    }
}

///A single collection.
///Contains a list of beatmap hashes that fall within this collection.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
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
