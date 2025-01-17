use std::path::PathBuf;

use mimalloc::MiMalloc;
use structopt::StructOpt;

mod bench;
mod compiler;
mod driver;
mod errors;
mod lexer;
mod parser;
mod pretty;
mod type_env;
mod typecheck;
mod types;
mod util;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

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

fn main() {
    let args = TychoOpt::from_args();

    match args {
        TychoOpt::Bench(bench_opt) => {
            bench::bench_all(bench_opt);
        }
        TychoOpt::Build(build_opt) => {
            driver::main(build_opt);
        }
    }
}
