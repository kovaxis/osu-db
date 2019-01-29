//! A simple command-line tool for decoding and encoding osu binary files to plaintext.
//!
//! **NOTE**: Currently encoding from plaintext is unsupported by osu-db.

use failure::{bail, err_msg, format_err, AsFail, Fallible, ResultExt};
use osu_db::{CollectionList, Listing, Replay, ScoreList};
use serde::Serialize;
use std::{
    env, fmt, fs,
    path::{Path, PathBuf},
};

const HELP_MSG: &'static str = r#"
Usage: osuconv [options] <input> [<output>]

By default the output filename is the same as the input with the extension changed to the target format.

Available options:
    -h  --help          Print this message.
    -d  --decode        Force decoding to plaintext from a binary file.
    -e  --encode        Force encoding to a binary file from plaintext.
    -b  --bin-type      Set the format of the binary file to decode/encode.
                        By default attempts to guess from filename.
                        Options are: `listing`, `collections`, `scores`, `replay`.
    -t  --text-type     Set the format of the plaintext to write/read. Defaults to `ron`.
                        Options are: `ron`, `json`.
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Operation {
    Decode,
    Encode,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum BinFormat {
    Listing,
    Collections,
    Scores,
    Replay,
}
impl BinFormat {
    fn from_name(name: &str) -> Option<BinFormat> {
        Some(match name {
            "listing" => BinFormat::Listing,
            "collections" => BinFormat::Collections,
            "scores" => BinFormat::Scores,
            "replay" => BinFormat::Replay,
            _ => return None,
        })
    }

    fn convert<P: AsRef<Path>>(self, reencode: TextFormat, input: P) -> Fallible<Vec<u8>> {
        let input = input.as_ref();
        eprintln!("reading binary from '{}'", input.display());
        Ok(match self {
            BinFormat::Listing => reencode.write(&Listing::from_file(input)?)?,
            BinFormat::Collections => reencode.write(&CollectionList::from_file(input)?)?,
            BinFormat::Scores => reencode.write(&ScoreList::from_file(input)?)?,
            BinFormat::Replay => reencode.write(&Replay::from_file(input)?)?,
        })
    }

    fn extension(self) -> &'static str {
        use self::BinFormat::*;
        match self {
            Listing | Collections | Scores => "db",
            Replay => "osr",
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum TextFormat {
    Ron,
    Json,
}
impl TextFormat {
    fn from_name(name: &str) -> Option<TextFormat> {
        Some(match name {
            "ron" => TextFormat::Ron,
            "json" => TextFormat::Json,
            _ => return None,
        })
    }

    fn write<T: Serialize>(self, data: &T) -> Fallible<Vec<u8>> {
        eprintln!("converting to plaintext");
        Ok(match self {
            TextFormat::Ron => serde_json::to_vec_pretty(&data)?,
            TextFormat::Json => ron::ser::to_string_pretty(&data, Default::default())?.into_bytes(),
        })
    }

    fn _extension(self) -> &'static str {
        use self::TextFormat::*;
        match self {
            Ron => "ron",
            Json => "json",
        }
    }
}

#[derive(Debug, Clone)]
struct Options {
    input: PathBuf,
    output: PathBuf,
    op: Operation,
    bin_format: BinFormat,
    text_format: TextFormat,
}
impl Options {
    fn parse() -> Fallible<Option<Options>> {
        let input;
        let output;
        let mut op = None;
        let mut bin_format = None;
        let mut text_format = None;

        let mut args = env::args().skip(1).peekable();
        let any_arg = args.peek().is_some();
        while let Some(arg) = args.peek() {
            if arg.starts_with('-') {
                //Check option
                match &arg[1..] {
                    "h" | "-help" => eprint!("{}", HELP_MSG),
                    "d" | "-decode" => op = Some(Operation::Decode),
                    "e" | "-encode" => op = Some(Operation::Encode),
                    "b" | "-bin-format" => {
                        args.next();
                        let format = args.peek().ok_or(err_msg("expected binary format"))?;
                        bin_format =
                            Some(BinFormat::from_name(format).ok_or_else(|| {
                                format_err!("invalid binary format '{}'", format)
                            })?);
                    }
                    "t" | "-text-format" => {
                        args.next();
                        let format = args.peek().ok_or(err_msg("expected text format"))?;
                        text_format = Some(
                            TextFormat::from_name(format)
                                .ok_or_else(|| format_err!("invalid text format '{}'", format))?,
                        );
                    }
                    _ => bail!(format_err!("unknown option '{}'", arg)),
                }
                args.next();
            } else {
                //Stop parsing options
                break;
            }
        }
        //Error on missing input
        input = match args.next() {
            Some(i) => PathBuf::from(i),
            None => bail!("expected input path"),
        };
        //If operation is missing, guess from input extension
        let op = op.unwrap_or(if input.extension() == Some("db".as_ref()) || input.extension() == Some("osr".as_ref()) {
            Operation::Decode
        } else {
            Operation::Encode
        });
        //Guess binformat from input
        let bin_format = match bin_format {
            Some(bf) => bf,
            None => match (
                input.extension().and_then(|s| s.to_str()).unwrap_or(""),
                input.file_name().and_then(|s| s.to_str()).unwrap_or(""),
            ) {
                ("osr", _) => BinFormat::Replay,
                ("db", "collection.db") => BinFormat::Collections,
                ("db", "scores.db") => BinFormat::Scores,
                ("db", _) => BinFormat::Listing,
                (ext, _name) => bail!("failed to guess binary format for extension '{}'", ext),
            },
        };
        //If output is missing, guess from input extension
        output = match args.next() {
            Some(o) => PathBuf::from(o),
            None => input.with_extension(match op {
                Operation::Decode => "txt",
                Operation::Encode => bin_format.extension(),
            }),
        };
        //Guess textformat from output, defaulting to ron
        let text_format = text_format.unwrap_or_else(|| {
            match output.extension().and_then(|s| s.to_str()).unwrap_or("") {
                "json" => TextFormat::Json,
                _ => TextFormat::Ron,
            }
        });
        if any_arg {
            Ok(Some(Options {
                input,
                output,
                op,
                bin_format,
                text_format,
            }))
        } else {
            eprint!("{}", HELP_MSG);
            Ok(None)
        }
    }
}

fn decode(opt: &Options) -> Fallible<()> {
    //Read, parse and reencode
    let out = opt.bin_format.convert(opt.text_format, &opt.input)?;

    //Write output to file
    eprintln!("writing plaintext to '{}'", opt.output.display());
    fs::write(&opt.output, &out)?;

    Ok(())
}

fn encode(_opt: &Options) -> Fallible<()> {
    bail!("encoding is currently not implemented by osu-db")
}

fn run() -> Fallible<()> {
    let opt = match Options::parse()? {
        Some(opt) => opt,
        None => return Ok(()),
    };

    match opt.op {
        Operation::Decode => decode(&opt)
            .with_context(|_| format_err!("failed to decode '{}'", opt.input.display()))?,
        Operation::Encode => encode(&opt).with_context(|_| {
            format_err!(
                "failed to encode from '{}' to '{}'",
                opt.input.display(),
                opt.output.display()
            )
        })?,
    }

    Ok(())
}

pub fn main() {
    match run() {
        Ok(()) => {}
        Err(err) => eprintln!("fatal error: {}", PrintErr(err)),
    }
}
