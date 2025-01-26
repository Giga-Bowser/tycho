use mimalloc::MiMalloc;
use structopt::StructOpt;

use tycho::{driver, luajit, TychoOpt};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() {
    let args = TychoOpt::from_args();

    match args {
        TychoOpt::Build(build_opt) => {
            driver::main(build_opt);
        }
        TychoOpt::Dump(dump_opt) => {
            luajit::dump_main(dump_opt);
        },
        TychoOpt::Read(read_opt) => {
            luajit::read_main(read_opt);
        },
    }
}
