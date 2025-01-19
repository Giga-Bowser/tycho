use mimalloc::MiMalloc;
use structopt::StructOpt;

use tycho::{driver, TychoOpt};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() {
    let args = TychoOpt::from_args();

    match args {
        TychoOpt::Build(build_opt) => {
            driver::main(build_opt);
        }
    }
}
