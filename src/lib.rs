//! Representation and parsing for osu! binary formats: beatmap listing, collections, replays and
//! scores.
//!
//! # A note on strings
//!
//! The osu `.db` file format allows for strings to be absent.
//! This notably happens with the unicode versions of titles and authors.
//! For this reason all of the parsed strings are expressed as `Option<String>` instead of a
//! simple `String`.
//! You can default to an empty string by using `string.unwrap_or_default()`, which does no
//! allocations and is very cheap.
//!
//! # A note on features and replays
//!
//! By default, replay data will be decompressed and parsed, using the `xz2` dependency.
//! To disable this behaviour and remove the dependency on `xz2`, disable the `compression` feature:
//!
//! ```toml
//! osu-db = { version = "*", default-features = false }
//! ```
//!
//! When `compression` is disabled, the
//! [`Replay::replay_data`](replay/struct.Replay.html#structfield.replay_data) field will always be
//! `None`, and will be ignored when writing.
//! In any case, the
//! [`Replay::raw_replay_data`](replay/struct.Replay.html#structfield.raw_replay_data) field is
//! always available.
//!
//! # A note on future-proofness
//!
//! Osu `.db` formats are used internally by osu!, and are not intended to be shared.
//! There does not seem to be any public contract on breaking changes, and breaking changes
//! already occured twice (on 2014 and 2019), so this library might not work with future versions
//! of osu!.
//!
//! It is currently guaranteed to work on osu! `.db` versions up to at least `20201017`.
//! The current implementation might work for a long time, or break tomorrow.

//Because otherwise compiling the large beatmap nom combinator fails
#![recursion_limit = "128"]

use crate::prelude::*;

pub use crate::{collection::CollectionList, listing::Listing, replay::Replay, score::ScoreList};

//Writer generator macro
trait Writable {
    type Args;
    fn wr_args<W: Write>(&self, out: &mut W, args: Self::Args) -> io::Result<()>;
}
trait SimpleWritable
where
    Self: Writable,
{
    fn wr<W: Write>(&self, out: &mut W) -> io::Result<()>;
}
impl<T> SimpleWritable for T
where
    T: Writable<Args = ()>,
{
    fn wr<W: Write>(&self, out: &mut W) -> io::Result<()> {
        self.wr_args(out, ())
    }
}
macro_rules! writer {
    ($type:ty [$this:ident, $out:ident] $code:expr) => {
        writer!($type [$this, $out, _arg: ()] $code);
    };
    ($type:ty [$this:ident, $out:ident, $args:ident : $args_ty:ty] $code:expr) => {
        impl crate::Writable for $type {
            type Args=$args_ty;
            fn wr_args<W: Write>(&self, $out: &mut W, $args: $args_ty) -> io::Result<()> {
                let $this = self;
                let () = $code;
                Ok(())
            }
        }
    };
}

mod prelude {
    pub(crate) use crate::{
        boolean, byte, datetime, double, int, long, opt_string, short, single, Bit, Error, ModSet,
        Mode, PrefixedList, SimpleWritable, Writable,
    };
    pub(crate) use chrono::{DateTime, Duration, TimeZone, Utc};
    pub(crate) use nom::{
        bytes::complete::{tag, take, take_while, take_while1},
        combinator::{cond, map, map_opt, map_res, opt},
        error::{Error as NomError, ErrorKind as NomErrorKind},
        multi::{length_count, length_data, many0},
        Err as NomErr, IResult, Needed,
    };
    #[cfg(feature = "ser-de")]
    pub use serde_derive::{Deserialize, Serialize};
    pub(crate) use std::{
        fmt,
        fs::{self, File},
        io::{self, BufWriter, Write},
        ops,
        path::Path,
    };
    #[cfg(feature = "compression")]
    pub use xz2::stream::Error as LzmaError;
}

pub mod collection;
pub mod listing;
pub mod replay;
pub mod score;

#[derive(Debug)]
pub enum Error {
    /// Only available with the `compression` feature enabled.
    #[cfg(feature = "compression")]
    Compression(LzmaError),
    Io(io::Error),
    ParseError(NomErrorKind),
    ParseIncomplete(Needed),
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            #[cfg(feature = "compression")]
            Error::Compression(_err) => f.write_str("failed to compress/decompress replay data"),
            Error::Io(_err) => f.write_str("failed to read osu .db file"),
            Error::ParseError(kind) => {
                write!(f, "failed to parse osu file: {}", kind.description())
            }
            Error::ParseIncomplete(Needed::Size(u)) => write!(
                f,
                "failed to parse osu file: parsing requires {} bytes/chars",
                u
            ),
            Error::ParseIncomplete(Needed::Unknown) => {
                f.write_str("failed to parse osu file: parsing requires more data")
            }
        }
    }
}
impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            #[cfg(feature = "compression")]
            Error::Compression(err) => Some(err as &dyn std::error::Error),
            Error::Io(err) => Some(err as &dyn std::error::Error),
            Error::ParseError(_kind) => None,
            Error::ParseIncomplete(_needed) => None,
        }
    }
}
impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}
impl From<NomErr<NomError<&[u8]>>> for Error {
    fn from(err: NomErr<NomError<&[u8]>>) -> Self {
        match err {
            NomErr::Incomplete(needed) => Self::ParseIncomplete(needed),
            NomErr::Error(err) | NomErr::Failure(err) => Self::ParseError(err.code),
        }
    }
}

#[cfg(feature = "compression")]
impl From<LzmaError> for Error {
    fn from(err: LzmaError) -> Self {
        Error::Compression(err)
    }
}

trait Bit {
    fn bit(&self, pos: u32) -> bool;
    fn bit_range(&self, pos: ops::Range<u32>) -> Self;
    fn set_bit(&mut self, pos: u32, val: bool);
    fn set_bit_range(&mut self, pos: ops::Range<u32>, val: Self);
}
macro_rules! impl_bit {
    (@ $ty:ty) => {
        impl Bit for $ty {
            fn bit(&self, pos: u32) -> bool {
                (*self & 1 << pos) != 0
            }
            fn bit_range(&self, pos: ops::Range<u32>) -> Self {
                (*self & ((1<<pos.end)-1)) >> pos.start
            }
            fn set_bit(&mut self, pos: u32, val: bool) {
                *self = (*self & !(1<<pos)) | ((val as Self)<<pos);
            }
            fn set_bit_range(&mut self, pos: ops::Range<u32>, val: Self) {
                let mask = ((1<<(pos.end-pos.start))-1) << pos.start;
                *self = (*self & !mask) | ((val<<pos.start)&mask);
            }
        }
    };
    ($($ty:ty),*) => {
        $(
            impl_bit!(@ $ty);
        )*
    }
}
impl_bit!(u8, u16, u32, u64);

//Common fixed-size osu `.db` primitives.
use nom::number::complete::le_f32 as single;
use nom::number::complete::le_f64 as double;
use nom::number::complete::le_u16 as short;
use nom::number::complete::le_u32 as int;
use nom::number::complete::le_u64 as long;
use nom::number::complete::le_u8 as byte;

fn boolean(bytes: &[u8]) -> IResult<&[u8], bool> {
    map(byte, |byte: u8| byte != 0)(bytes)
}

writer!(u8 [this,out] out.write_all(&this.to_le_bytes())?);
writer!(u16 [this,out] out.write_all(&this.to_le_bytes())?);
writer!(u32 [this,out] out.write_all(&this.to_le_bytes())?);
writer!(u64 [this,out] out.write_all(&this.to_le_bytes())?);
writer!(f32 [this,out] this.to_bits().wr(out)?);
writer!(f64 [this,out] this.to_bits().wr(out)?);
writer!(bool [this,out] (if *this {1_u8} else {0_u8}).wr(out)?);

//Writer for a list of items preceded by its length as an int
struct PrefixedList<'a, T>(&'a [T]);
impl<T> Writable for PrefixedList<'_, T>
where
    T: Writable,
    T::Args: Clone,
{
    type Args = T::Args;
    fn wr_args<W: Write>(&self, out: &mut W, args: T::Args) -> io::Result<()> {
        (self.0.len() as u32).wr(out)?;
        for item in self.0 {
            item.wr_args(out, args.clone())?;
        }
        Ok(())
    }
}

/// Get a datetime from an amount of "windows ticks":
/// The amount of 100-nanosecond units since midnight of the date 0001/01/01.
fn windows_ticks_to_datetime(ticks: u64) -> DateTime<Utc> {
    let epoch = Utc.ymd(1, 1, 1).and_hms(0, 0, 0);
    epoch
        + Duration::microseconds((ticks / 10) as i64)
        + Duration::nanoseconds((ticks % 10 * 100) as i64)
}

fn datetime(bytes: &[u8]) -> IResult<&[u8], DateTime<Utc>> {
    map(long, windows_ticks_to_datetime)(bytes)
}

fn datetime_to_windows_ticks(datetime: &DateTime<Utc>) -> u64 {
    let epoch = Utc.ymd(1, 1, 1).and_hms(0, 0, 0);
    let duration = datetime.signed_duration_since(epoch);
    let ticks_since: i64 = (duration * 10).num_microseconds().unwrap_or(0);
    ticks_since.max(0) as u64
}
writer!(DateTime<Utc> [this,out] datetime_to_windows_ticks(this).wr(out)?);

// The variable-length ULEB128 encoding used mainly for string lengths.
fn uleb(bytes: &[u8]) -> IResult<&[u8], usize> {
    let (rem, prelude) = take_while(|b: u8| b.bit(7))(bytes)?;
    let (rem, finalizer) = byte(rem)?;

    let mut out = 0;
    let mut offset = 0;

    for byte in prelude {
        out |= (byte.bit_range(0..7) as usize) << offset;
        offset += 7;
    }

    out |= (finalizer as usize) << offset;

    Ok((rem, out))
}

writer!(usize [this,out] {
    let mut this=*this;
    loop {
        let mut byte=this as u8;
        this>>=7;
        let continues={this!=0};
        byte.set_bit(7, continues);
        byte.wr(out)?;
        if !continues {break}
    }
});

// An optional string.
fn opt_string(bytes: &[u8]) -> IResult<&[u8], Option<String>> {
    let (rem, first_byte) = byte(bytes)?;

    match first_byte {
        0x00 => Ok((rem, None)),
        0x0b => {
            let (rem, len) = uleb(rem)?;
            let (rem, string) = map_res(take(len), std::str::from_utf8)(rem)?;

            Ok((rem, Some(string.to_owned())))
        }
        _ => Err(NomErr::Error(NomError::new(bytes, NomErrorKind::Switch))),
    }
}

writer!(Option<String> [this,out] {
    match this {
        Some(string) => {
            0x0b_u8.wr(out)?;
            string.len().wr(out)?;
            out.write_all(string.as_bytes())?;
        },
        None => 0x00_u8.wr(out)?,
    }
});

/// An osu! gamemode.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Mode {
    Standard,
    Taiko,
    CatchTheBeat,
    Mania,
}
impl Mode {
    pub fn raw(self) -> u8 {
        self as u8
    }

    pub fn from_raw(raw: u8) -> Option<Mode> {
        use self::Mode::*;
        Some(match raw {
            0 => Standard,
            1 => Taiko,
            2 => CatchTheBeat,
            3 => Mania,
            _ => return None,
        })
    }
}

/// A single osu! mod.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Mod {
    NoFail,
    Easy,
    TouchDevice,
    Hidden,
    HardRock,
    SuddenDeath,
    DoubleTime,
    Relax,
    HalfTime,
    /// Always goes with `DoubleTime`.
    Nightcore,
    Flashlight,
    Autoplay,
    SpunOut,
    /// Also called "Relax2".
    Autopilot,
    Perfect,
    Key4,
    Key5,
    Key6,
    Key7,
    Key8,
    FadeIn,
    Random,
    /// Cinema.
    LastMod,
    /// Only on osu!cuttingedge it seems.
    TargetPractice,
    Key9,
    Coop,
    Key1,
    Key3,
    Key2,
}
impl Mod {
    /// Each of the 29 mods have a corresponding integer between [0,28], inclusive.
    /// This method retrieves its integer.
    pub fn raw(&self) -> u8 {
        *self as u8
    }

    /// Build a mod from its corresponding integer.
    /// Returns `None` if the integer is out-of-range (>28).
    pub fn from_raw(bit_offset: u8) -> Option<Mod> {
        use self::Mod::*;
        Some(match bit_offset {
            0 => NoFail,
            1 => Easy,
            2 => TouchDevice,
            3 => Hidden,
            4 => HardRock,
            5 => SuddenDeath,
            6 => DoubleTime,
            7 => Relax,
            8 => HalfTime,
            9 => Nightcore,
            10 => Flashlight,
            11 => Autoplay,
            12 => SpunOut,
            13 => Autopilot,
            14 => Perfect,
            15 => Key4,
            16 => Key5,
            17 => Key6,
            18 => Key7,
            19 => Key8,
            20 => FadeIn,
            21 => Random,
            22 => LastMod,
            23 => TargetPractice,
            24 => Key9,
            25 => Coop,
            26 => Key1,
            27 => Key3,
            28 => Key2,
            _ => return None,
        })
    }
}

/// A combination of `Mod`s.
///
/// Very cheap to copy around, as it is a just a wrapped 32-bit integer.
#[cfg_attr(feature = "ser-de", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModSet(pub u32);
impl ModSet {
    pub fn bits(&self) -> u32 {
        self.0
    }
    pub fn from_bits(bits: u32) -> ModSet {
        ModSet(bits)
    }

    /// Create a `ModSet` with no mods included.
    pub fn empty() -> ModSet {
        ModSet::from_bits(0)
    }

    /// Check whether the set contains the given mod.
    pub fn contains(&self, m: Mod) -> bool {
        self.bits().bit(m.raw() as u32)
    }

    /// Make a new set of mods with the given mod included or not included.
    pub fn set(&self, m: Mod, include: bool) -> ModSet {
        let mut bits = self.bits();
        bits.set_bit(m.raw() as u32, include);
        ModSet::from_bits(bits)
    }

    /// Make a new set of mods with the given mod included.
    pub fn with(&self, m: Mod) -> ModSet {
        self.set(m, true)
    }

    /// Make a new set of mods with the given mod removed.
    pub fn without(&self, m: Mod) -> ModSet {
        self.set(m, false)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic() {
        assert_eq!(
            byte::<_, NomError<&[u8]>>(" ".as_bytes()),
            Ok((&[][..], 32))
        );
        assert_eq!(
            short::<_, NomError<&[u8]>>(&[10, 2][..]),
            Ok((&[][..], 522))
        );
        assert_eq!(
            int::<_, NomError<&[u8]>>(&[10, 10, 0, 0, 3][..]),
            Ok((&[3][..], 2570))
        );
        assert_eq!(
            long::<_, NomError<&[u8]>>(&[0, 0, 1, 0, 2, 0, 3, 0][..]),
            Ok((&[][..], 844_433_520_132_096))
        );
        assert_eq!(
            single::<_, NomError<&[u8]>>(&[0, 0, 0b00100000, 0b00111110, 4][..]),
            Ok((&[4][..], 0.15625))
        );
        assert_eq!(
            double::<_, NomError<&[u8]>>(&[0b00000010, 0, 0, 0, 0, 0, 0b11110000, 0b00111111][..]),
            Ok((&[][..], 1.0000000000000004))
        );
        assert_eq!(boolean(&[34, 4, 0][..]), Ok((&[4, 0][..], true)));
        assert_eq!(
            int::<_, NomError<&[u8]>>(&[3, 5, 4][..]),
            Err(NomErr::Error(NomError::new(
                &[3, 5, 4][..],
                NomErrorKind::Eof
            )))
        );
        assert_eq!(
            boolean(&[][..]),
            Err(NomErr::Error(NomError::new(&[][..], NomErrorKind::Eof)))
        );
        assert_eq!(
            double::<_, NomError<&[u8]>>(&[14, 25, 15, 24, 3][..]),
            Err(NomErr::Error(NomError::new(
                &[14, 25, 15, 24, 3][..],
                NomErrorKind::Eof
            )))
        );
    }

    #[test]
    fn uleb128() {
        assert_eq!(uleb(&[70]), Ok((&[][..], 70)));
        assert_eq!(
            uleb(&[]),
            Err(NomErr::Error(NomError::new(&[][..], NomErrorKind::Eof)))
        );
        assert_eq!(uleb(&[129, 2]), Ok((&[][..], 257)));
        assert_eq!(uleb(&[124, 2]), Ok((&[2][..], 124)));
    }

    #[test]
    fn strings() {
        let long_str = "w".repeat(129);

        assert_eq!(opt_string(b"\x00sf"), Ok((&b"sf"[..], None)));
        assert_eq!(
            opt_string(b"\x0b\x02ghf"),
            Ok((&b"f"[..], Some("gh".to_string())))
        );
        //Invalid string header
        assert!(opt_string(b"\x01ww").is_err());
        //Invalid utf-8
        assert!(opt_string(b"\x0b\x01\xff").is_err());
        //Missing string length
        assert_eq!(
            opt_string(b"\x0b"),
            Err(NomErr::Error(NomError::new(&[][..], NomErrorKind::Eof)))
        );
        //Long strings
        let mut raw = Vec::from(&b"\x0b\x81\x01"[..]);
        raw.extend_from_slice(long_str.as_bytes());
        raw.extend_from_slice(&b"afaf"[..]);
        assert_eq!(opt_string(&raw), Ok((&b"afaf"[..], Some(long_str))));
    }
}
