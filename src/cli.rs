use clap::{
    builder::{styling::AnsiColor, Styles, ValueParser},
    error::ErrorKind,
    Arg, ArgAction, ArgMatches, Command, Error,
};
use std::{
    path::PathBuf,
    sync::atomic::{AtomicBool, Ordering},
};

pub enum CLI {
    Build(Build),
    Read(Read),
    Print(Print),
}

pub struct Build {
    pub file: PathBuf,
    pub includes: Vec<PathBuf>,
    pub output: Option<PathBuf>,
    pub bc: bool,
}

pub struct Read {
    pub file: PathBuf,
}

pub struct Print {
    pub file: PathBuf,
    pub includes: Vec<PathBuf>,
}

static VERBOSITY: AtomicBool = AtomicBool::new(false);

#[inline]
pub fn verbose() -> bool {
    VERBOSITY.load(Ordering::Relaxed)
}

const STYLES: Styles = Styles::styled()
    .header(AnsiColor::Green.on_default().bold())
    .usage(AnsiColor::Green.on_default().bold())
    .placeholder(AnsiColor::Green.on_default())
    .literal(AnsiColor::Cyan.on_default().bold())
    .valid(AnsiColor::Cyan.on_default().bold());

impl CLI {
    pub fn parse() -> Self {
        let command = Self::command();

        let matches = command.get_matches();
        Self::from_arg_matches(&matches).unwrap_or_else(|e| e.format(&mut Self::command()).exit())
    }

    fn command() -> Command {
        Command::new("tycho")
            .styles(STYLES)
            .version(env!("CARGO_PKG_VERSION"))
            .subcommand_required(true)
            .arg_required_else_help(true)
            .arg(
                Arg::new("verbose")
                    .short('v')
                    .long("verbose")
                    .help("Enable verbose output")
                    .value_parser(ValueParser::bool())
                    .action(ArgAction::SetTrue)
                    .global(true),
            )
            .subcommands([Build::command(), Read::command(), Print::command()])
    }

    fn from_arg_matches(matches: &ArgMatches) -> Result<Self, Error> {
        VERBOSITY.store(matches.get_flag("verbose"), Ordering::Relaxed);
        let (name, sub_matches) = matches
            .subcommand()
            .ok_or_else(|| Error::new(ErrorKind::MissingSubcommand))?;

        match name {
            "build" => Ok(Self::Build(Build::from_arg_matches(sub_matches)?)),
            "read" => Ok(Self::Read(Read::from_arg_matches(sub_matches)?)),
            "print" => Ok(Self::Print(Print::from_arg_matches(sub_matches)?)),
            _ => Err(Error::raw(
                ErrorKind::InvalidSubcommand,
                format!("the subcommand `{name}` wasn't recognized"),
            )),
        }
    }
}

impl Build {
    fn command() -> Command {
        Command::new("build")
            .styles(STYLES)
            .about("Build a file")
            .args([
                file_arg(),
                includes_arg(),
                Arg::new("output")
                    .short('o')
                    .value_name("FILE")
                    .help("output file, stdout if not present")
                    .value_parser(ValueParser::path_buf())
                    .action(ArgAction::Set),
                Arg::new("bc")
                    .short('b')
                    .help("compile to bytecode")
                    .value_parser(ValueParser::bool())
                    .action(ArgAction::SetTrue),
            ])
    }

    fn from_arg_matches(matches: &ArgMatches) -> Result<Self, Error> {
        Ok(Build {
            file: pluck(matches, "file")?,
            includes: pluck_many(matches, "includes"),
            output: matches.get_one("output").cloned(),
            bc: matches.get_flag("bc"),
        })
    }
}

impl Read {
    fn command() -> Command {
        Command::new("read")
            .styles(STYLES)
            .about("Read a bytecode file and pretty-print it")
            .arg(file_arg())
    }

    fn from_arg_matches(matches: &ArgMatches) -> Result<Self, Error> {
        Ok(Read {
            file: pluck(matches, "file")?,
        })
    }
}

impl Print {
    fn command() -> Command {
        Command::new("print")
            .styles(STYLES)
            .about("Read a bytecode file and pretty-print it")
            .args([file_arg(), includes_arg()])
    }

    fn from_arg_matches(matches: &ArgMatches) -> Result<Self, Error> {
        Ok(Print {
            file: pluck(matches, "file")?,
            includes: pluck_many(matches, "includes"),
        })
    }
}

fn pluck<T: Clone + Send + Sync + 'static>(matches: &ArgMatches, id: &str) -> Result<T, Error> {
    matches.get_one(id).cloned().ok_or_else(|| {
        Error::raw(
            ErrorKind::MissingRequiredArgument,
            format!("The following required argument was not provided: `{id}`"),
        )
    })
}

fn pluck_many<T: Clone + Send + Sync + 'static>(matches: &ArgMatches, id: &str) -> Vec<T> {
    matches.get_many(id).unwrap_or_default().cloned().collect()
}

fn file_arg() -> Arg {
    Arg::new("file")
        .value_name("FILE")
        .value_parser(ValueParser::path_buf())
        .action(ArgAction::Set)
        .required(true)
}

fn includes_arg() -> Arg {
    Arg::new("includes")
        .value_name("INCLUDES")
        .num_args(1..)
        .value_parser(ValueParser::path_buf())
        .action(ArgAction::Append)
}
