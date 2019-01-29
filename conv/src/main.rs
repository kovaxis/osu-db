//! A simple command-line tool for decoding and encoding osu binary files to plaintext.
//!
//! **NOTE**: Currently encoding from plaintext is unsupported by osu-db.

use failure::{bail, err_msg, format_err, AsFail, Fallible, ResultExt};
use osu_db::{CollectionList, Listing, Replay, ScoreList};
use serde::{de::DeserializeOwned, Serialize};
use std::{env, fmt, fs, path::PathBuf};

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

    fn decode(self, out_format: TextFormat, input: &[u8]) -> Fallible<Vec<u8>> {
        let input = input.as_ref();
        eprintln!("parsing binary as a {:?} file", self);
        Ok(match self {
            BinFormat::Listing => out_format.write(&Listing::from_bytes(input)?)?,
            BinFormat::Collections => out_format.write(&CollectionList::from_bytes(input)?)?,
            BinFormat::Scores => out_format.write(&ScoreList::from_bytes(input)?)?,
            BinFormat::Replay => out_format.write(&Replay::from_bytes(input)?)?,
        })
    }

    fn encode(self, in_format: TextFormat, input: &[u8]) -> Fallible<Vec<u8>> {
        let mut out = Vec::new();
        macro_rules! bintypes {
            ($($variant:ident => $type:ty {$($args:tt)*},)*) => {{
                match self {
                    $(BinFormat::$variant => {
                        let in_mem=in_format.read::<$type>(input,self)?;
                        eprintln!("converting to a binary {:?} file", self);
                        in_mem.to_writer(&mut out $($args)*)?;
                    })*
                }
            }};
        }
        bintypes! {
            Listing => Listing {},
            Collections => CollectionList {},
            Scores => ScoreList {},
            Replay => Replay { , None },
        }
        Ok(out)
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
        eprintln!("converting to plaintext as a {:?} file", self);
        Ok(match self {
            TextFormat::Ron => ron::ser::to_string_pretty(&data, Default::default())?.into_bytes(),
            TextFormat::Json => serde_json::to_vec_pretty(&data)?,
        })
    }

    fn read<T: DeserializeOwned>(self, bytes: &[u8], binformat: BinFormat) -> Fallible<T> {
        eprintln!(
            "parsing plaintext as a {:?} file representing a binary {:?} file",
            self, binformat
        );
        Ok(match self {
            TextFormat::Ron => ron::de::from_bytes(bytes)?,
            TextFormat::Json => serde_json::from_reader(bytes)?,
        })
    }

    fn extension(self) -> &'static str {
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
        output = args.next();
        //Guess binformat from input/output
        macro_rules! concretize_bin_format {
            ($bin_io:expr) => {{
                match bin_format {
                    Some(bf) => bf,
                    None => {
                        let bin_io = &$bin_io;
                        match (
                            bin_io.extension().and_then(|s| s.to_str()).unwrap_or(""),
                            bin_io.file_stem().and_then(|s| s.to_str()).unwrap_or(""),
                        ) {
                            ("osr", _) => BinFormat::Replay,
                            ("db", "collection") => BinFormat::Collections,
                            ("db", "scores") => BinFormat::Scores,
                            ("db", _) => BinFormat::Listing,
                            (ext, _name) => {
                                bail!("failed to guess binary format for extension '{}'", ext)
                            }
                        }
                    }
                }
            }};
        };
        //Guess textformat from output, defaulting to ron
        macro_rules! concretize_text_format {
            ($text_io:expr) => {{
                text_format.unwrap_or_else(|| {
                    let text_io = &$text_io;
                    match text_io.extension().and_then(|s| s.to_str()).unwrap_or("") {
                        "json" => TextFormat::Json,
                        _ => TextFormat::Ron,
                    }
                })
            }};
        }
        //If operation is missing, guess from input extension
        let op = op.unwrap_or(
            if input.extension() == Some("db".as_ref()) || input.extension() == Some("osr".as_ref())
            {
                Operation::Decode
            } else {
                Operation::Encode
            },
        );
        //Guess missing parameters
        let (output, bin_format, text_format) = match op {
            Operation::Decode => {
                let bin_format = concretize_bin_format!(input);
                let output = match output {
                    Some(o) => PathBuf::from(o),
                    None => input.with_extension(
                        text_format
                            .map(|format| format.extension())
                            .unwrap_or("txt"),
                    ),
                };
                let text_format = concretize_text_format!(output);
                (output, bin_format, text_format)
            }
            Operation::Encode => {
                let text_format = concretize_text_format!(input);
                let output = match output {
                    Some(o) => PathBuf::from(o),
                    None => {
                        let ext = bin_format.map(|format| format.extension()).unwrap_or(
                            match input.to_string_lossy() {
                                ref name
                                    if name.contains("collection")
                                        || name.contains("scores")
                                        || name.contains("osu!") =>
                                {
                                    "db"
                                }
                                _ => "osr",
                            },
                        );
                        input.with_extension(ext)
                    }
                };
                let bin_format = concretize_bin_format!(output);
                (output, bin_format, text_format)
            }
        };
        //Print help message if no argument was given
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
    //Read
    eprintln!("reading binary from '{}'", opt.input.display());
    let raw = fs::read(&opt.input)?;

    //Parse and rewrite as plaintext
    let out = opt.bin_format.decode(opt.text_format, &raw)?;

    //Write output to file
    eprintln!("writing plaintext to '{}'", opt.output.display());
    fs::write(&opt.output, &out)?;

    Ok(())
}

fn encode(opt: &Options) -> Fallible<()> {
    //Read
    eprintln!("reading plaintext from '{}'", opt.input.display());
    let plaintext = fs::read(&opt.input)?;

    //Parse plaintext and reencode as binary
    let out = opt.bin_format.encode(opt.text_format, &plaintext)?;

    //Write output to file
    eprintln!("writing binary output to '{}'", opt.output.display());
    fs::write(&opt.output, &out)?;

    Ok(())
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
