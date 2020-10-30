
TODO:

- Support replays with the "Target Practice" mod enabled.

# 0.2

- Added support for osu! binary files from `20191106` up to at least `20201017`.
- Renamed `mysterious_int` to `user_permissions`.
- Removed the `Hash` and `PartialEq` impls on `Beatmap` that only took the beatmap hash and beatmap
    ID into account.
- Fields of `Replay` no longer change depending on features. Instead the `replay_data` field is
    always available, but is only `Some` when the `compression` feature is enabled. The
    `raw_replay_data` field is always available, instead.
- `PartialEq` is now implemented everywhere.

# 0.1

- Support for reading and writing `osu!.db`, `collection.db`, `scores.db` and `*.osr` files.
- Support for osu! binary files up to at least `20181221`.
