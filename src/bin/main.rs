use mimalloc::MiMalloc;
use structopt::StructOpt;

use tycho::{bench, driver, TychoOpt};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

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
