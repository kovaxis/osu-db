
# 0.2

- Added support for osu! binary files from `20191106` up to at least `20201017`.
- Renamed `mysterious_int` to `user_permissions`.
- Removed the `Hash` and `PartialEq` impls on `Beatmap` that only took the beatmap hash and beatmap
    ID into account.

# 0.1

- Support for reading and writing `osu!.db`, `collection.db`, `scores.db` and `*.osr` files.
- Support for osu! binary files up to at least `20181221`.
