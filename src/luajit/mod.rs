mod bytecode;
mod utils;

use bytecode::dump_bc;

use crate::{luajit::bytecode::*, DumpOpt, ReadOpt};

pub fn dump_main(args: DumpOpt) {
    let mut output: Box<dyn std::io::Write> = match &args.output {
        Some(file) => Box::new(std::fs::File::create(file).unwrap_or_else(|e| panic!("{e}"))),
        None => Box::new(std::io::stdout()),
    };

    let header = Header::default();

    let bcins = vec![
        BCInstr::new_ad(bytecode::BCOp::ISNEN, 0, 0),
        BCInstr::new_jmp(bytecode::BCOp::JMP, 1, 2),
        BCInstr::new_ad(bytecode::BCOp::KSHORT, 1, 1),
        BCInstr::new_ad(bytecode::BCOp::RET1, 1, 2),
        BCInstr::new_ad(bytecode::BCOp::UGET, 1, 0),
        BCInstr::new_abc(bytecode::BCOp::SUBVN, 3, 0, 1),
        BCInstr::new_abc(bytecode::BCOp::CALL, 1, 2, 2),
        BCInstr::new_abc(bytecode::BCOp::MULVV, 1, 0, 1),
        BCInstr::new_ad(bytecode::BCOp::RET1, 1, 2),
    ];

    let proto = Proto {
        flags: ProtoFlags::default(),
        num_params: 1,
        frame_size: 4,
        bcins,
        upvalue_data: vec![0x8000],
        gc_constants: vec![],
        number_constants: vec![0.0, 1.0],
    };

    let result = dump_bc(&header, &[proto]);

    output.write_all(&result).unwrap();
}

pub fn read_main(args: ReadOpt) {
    let dump = std::fs::read(args.file).unwrap();

    let (header, protos) = read_dump(&dump);

    println!("header: {header:#?}");
    println!("protos: {protos:#?}");
}
