use mimalloc::MiMalloc;

use tycho::{cli::CLI, driver, luajit};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() {
    let args = CLI::parse();

    match args {
        CLI::Build(build_opt) => driver::main(&build_opt),
        CLI::Read(read_opt) => luajit::read_main(&read_opt),
        CLI::Print(print_opt) => driver::print_main(&print_opt),
    }
}
