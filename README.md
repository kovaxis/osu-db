# osu-db

`osu-db` is an osu binary file format encoder/decoder.
To use, simply add this line to your `Cargo.toml`:

```
osu-db = "*"
```

After that you will want to use the different load/save functions on the
`Listing` (cached beatmap database), `ScoreList` (summary of all player scores),
`CollectionList` (in-game beatmap collections) or `Replay` (a single in-depth
standalone replay file).

For example, to change all your osu!mania grades to `SS+`:

```
use osu_db::listing::{Listing, Grade};

// Load the listing to memory
let mut listing=Listing::from_file("osu!.db").unwrap();

// Modify listing in-place
for beatmap in listing.beatmaps.iter_mut() {
    beatmap.mania_grade = Grade::SSPlus;
}

// Save back to disk
listing.save("osu!.db").unwrap();
```

More details in the crate documentation.
