use std::path::PathBuf;

use structopt::StructOpt;

mod errors;
mod mem_size;
mod pretty;
mod types;
mod util;

pub mod bench;
pub mod compiler;
pub mod driver;
pub mod lexer;
pub mod parser;
pub mod type_env;
pub mod typecheck;

#[derive(StructOpt)]
pub enum TychoOpt {
    /// Benchmark the compiler itself
    Bench(BenchOpt),
    /// Build a file
    Build(BuildOpt),
}

#[derive(StructOpt)]
pub struct BenchOpt {
    #[structopt(parse(from_os_str))]
    file: PathBuf,

    #[structopt(parse(from_os_str))]
    includes: Vec<PathBuf>,

    #[structopt(long, default_value = "3")]
    total_time: f64,
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
