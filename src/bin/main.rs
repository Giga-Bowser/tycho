use mimalloc::MiMalloc;

use tycho::{cli::CLI, driver};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() {
    let args = CLI::parse();

    let res = match &args {
        CLI::Build(build_opt) => driver::build_main(build_opt),
        CLI::Read(read_opt) => driver::read_main(read_opt),
        CLI::Print(print_opt) => driver::print_main(print_opt),
    };

    if let Err(e) = res {
        eprintln!("{e}");
        std::process::exit(1);
    }
}
