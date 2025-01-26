use std::path::PathBuf;

use structopt::StructOpt;

mod errors;
mod mem_size;
mod pretty;
mod types;
mod util;

pub mod driver;
pub mod lexer;
pub mod parser;
pub mod transpiler;
pub mod type_env;
pub mod typecheck;

#[derive(StructOpt)]
pub enum TychoOpt {
    /// Build a file
    Build(BuildOpt),
}

#[derive(StructOpt)]
pub struct BuildOpt {
    #[structopt(parse(from_os_str))]
    file: PathBuf,

    #[structopt(parse(from_os_str))]
    includes: Vec<PathBuf>,

    /// Output file, stdout if not present
    #[structopt(short, parse(from_os_str))]
    output: Option<PathBuf>,

    /// Enable verbose output
    #[structopt(short, long)]
    verbose: bool,
}
