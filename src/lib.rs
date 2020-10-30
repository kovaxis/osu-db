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
//! # A note on future-proofness
//!
//! Osu `.db` formats are used internally by osu!, and are not intended to be shared.
//! There does not seem to be any public contract on breaking changes, and breaking changes
//! already occured once on 2014, so this library might not work with future versions of osu!.
//!
//! It is currently guaranteed to work on osu! `.db` versions up to at least 20181221.
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
        boolean, byte, datetime, double, int, long, opt_string, short, single, Error, ModSet, Mode,
        PrefixedList, SimpleWritable, Writable,
    };
    pub(crate) use bit::BitIndex;
    pub(crate) use chrono::{DateTime, Duration, TimeZone, Utc};
    pub(crate) use failure::Fail;
    pub(crate) use nom::{
        call, complete, cond, do_parse, length_bytes, length_count, many0, map, map_opt, map_res,
        named, named_args, opt, switch, tag, take, take_str, take_while, take_while1, value,
        Err as NomErr, ErrorKind as NomErrKind, IResult,
    };
    #[cfg(feature = "ser-de")]
    pub use serde_derive::{Deserialize, Serialize};
    pub(crate) use std::{
        fmt,
        fs::{self, File},
        io::{self, BufWriter, Write},
        path::Path,
    };
    #[cfg(feature = "compression")]
    pub(crate) use xz2::stream::Error as LzmaError;
}

pub mod collection;
pub mod listing;
pub mod replay;
pub mod score;

#[derive(Debug)]
pub enum Error {
    Parse(NomErrKind),
    Io(io::Error),
    #[cfg(feature = "compression")]
    Compression(LzmaError),
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Parse(errkind) => {
                write!(f, "failed to parse osu file: {}", errkind.description())
            }
            Error::Io(_err) => write!(f, "failed to read osu .db file"),
            #[cfg(feature = "compression")]
            Error::Compression(_err) => write!(f, "failed to compress/decompress replay data"),
        }
    }
}
impl Fail for Error {
    fn cause(&self) -> Option<&dyn Fail> {
        match self {
            Error::Parse(_err) => None,
            Error::Io(err) => Some(err as &dyn Fail),
            #[cfg(feature = "compression")]
            Error::Compression(err) => Some(err as &dyn Fail),
        }
    }
}
impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}
impl<I> From<NomErr<I>> for Error {
    fn from(err: NomErr<I>) -> Error {
        Error::Parse(err.into_error_kind())
    }
}
#[cfg(feature = "compression")]
impl From<LzmaError> for Error {
    fn from(err: LzmaError) -> Error {
        Error::Compression(err)
    }
}

//Common fixed-size osu `.db` primitives.
use nom::le_f32 as single;
use nom::le_f64 as double;
use nom::le_u16 as short;
use nom::le_u32 as int;
use nom::le_u64 as long;
use nom::le_u8 as byte;
named!(boolean<&[u8],bool>, map!(take!(1),|b| b[0]!=0));

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
named!(datetime<&[u8], DateTime<Utc>>, map!(long,windows_ticks_to_datetime));

fn datetime_to_windows_ticks(datetime: &DateTime<Utc>) -> u64 {
    let epoch = Utc.ymd(1, 1, 1).and_hms(0, 0, 0);
    let duration = datetime.signed_duration_since(epoch);
    let ticks_since: i64 = (duration * 10).num_microseconds().unwrap_or(0);
    ticks_since.max(0) as u64
}
writer!(DateTime<Utc> [this,out] datetime_to_windows_ticks(this).wr(out)?);

// The variable-length ULEB128 encoding used mainly for string lengths.
named!(uleb<&[u8],usize>, do_parse!(
    prelude: take_while!(|b: u8| b.bit(7)) >>
    finalizer: take!(1) >>
    ({
        let mut out=0;
        let mut offset=0;
        for byte in prelude {
            out|=(byte.bit_range(0..7) as usize)<<offset;
            offset+=7;
        }
        out|=(finalizer[0] as usize)<<offset;
        out
    })
));

writer!(usize [this,out] {
    let mut this=*this;
    loop {
        let mut byte=this as u8;
        this>>=7;
        let continues={this!=0};
        byte.set_bit(7,continues);
        byte.wr(out)?;
        if !continues {break}
    }
});

// An optional string.
named!(opt_string<&[u8],Option<String>>, switch!(take!(1),
    &[0x00] => value!(None) |
    &[0x0b] => do_parse!(
        len: uleb >>
        string: take_str!(len) >>
        (Some(string.to_string()))
    )
));

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
        self.bits().bit(m.raw() as usize)
    }

    /// Make a new set of mods with the given mod included or not included.
    pub fn set(&self, m: Mod, include: bool) -> ModSet {
        let mut bits = self.bits();
        bits.set_bit(m.raw() as usize, include);
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
    use nom::Needed;

    #[test]
    fn basic() {
        assert_eq!(byte(b" "), Ok((&[][..], 32)));
        assert_eq!(short(&[10, 2]), Ok((&[][..], 522)));
        assert_eq!(int(&[10, 10, 0, 0, 3]), Ok((&[3][..], 2570)));
        assert_eq!(
            long(&[0, 0, 1, 0, 2, 0, 3, 0]),
            Ok((&[][..], 844_433_520_132_096))
        );
        assert_eq!(
            single(&[0, 0, 0b00100000, 0b00111110, 4]),
            Ok((&[4][..], 0.15625))
        );
        assert_eq!(
            double(&[0b00000010, 0, 0, 0, 0, 0, 0b11110000, 0b00111111]),
            Ok((&[][..], 1.0000000000000004))
        );
        assert_eq!(boolean(&[34, 4, 0]), Ok((&[4, 0][..], true)));
        assert_eq!(int(&[3, 5, 4]), Err(NomErr::Incomplete(Needed::Size(4))));
        assert_eq!(boolean(&[]), Err(NomErr::Incomplete(Needed::Size(1))));
        assert_eq!(
            double(&[14, 25, 15, 24, 3]),
            Err(NomErr::Incomplete(Needed::Size(8)))
        );
    }

    #[test]
    fn uleb128() {
        assert_eq!(uleb(&[70]), Ok((&[][..], 70)));
        assert_eq!(uleb(&[]), Err(NomErr::Incomplete(Needed::Size(1))));
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
            Err(NomErr::Incomplete(Needed::Size(1)))
        );
        //Long strings
        let mut raw = Vec::from(&b"\x0b\x81\x01"[..]);
        raw.extend_from_slice(long_str.as_bytes());
        raw.extend_from_slice(&b"afaf"[..]);
        assert_eq!(opt_string(&raw), Ok((&b"afaf"[..], Some(long_str))));
    }
}
