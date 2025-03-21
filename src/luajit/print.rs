use std::fmt::{self, Write as _};

use yansi::Paint;

use crate::{
    luajit::{
        bytecode::{
            BCInstr, BCOp, ChunkName, GCConstant, Header, HeaderFlags, Proto, ProtoFlags,
            TemplateTable, UVData, UVFlags,
        },
        TValue,
    },
    utils::indenter::indented,
};

impl fmt::Display for Header {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<")?;
        if let Some(chunk_name) = &self.chunk_name {
            chunk_name.fmt(f)?;
        } else {
            f.write_str("Chunk")?;
        }
        f.write_str(">\n")?;

        writeln!(f, "Flags: {}", self.flags)
    }
}

impl fmt::Display for HeaderFlags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.iter_names()
                .map(|(name, _)| name)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl fmt::Display for ChunkName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ChunkName::File(path) => write!(f, "@{}", path.display()),
            ChunkName::Custom(name) => write!(f, "={name}"),
        }
    }
}

fn multiline_list(
    f: &mut impl fmt::Write,
    values: impl Iterator<Item = impl fmt::Display>,
) -> fmt::Result {
    let values: Vec<_> = values.collect();

    if values.is_empty() {
        return f.write_str("[]\n");
    }

    f.write_str("[\n")?;

    for value in &values {
        writeln!(indented(f), "{value},")?;
    }

    f.write_str("]\n")
}

impl fmt::Display for Proto {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Proto: {\n")?;

        {
            let f = &mut indented(f);
            writeln!(f, "Flags: {}", self.flags)?;
            writeln!(f, "Number of params: {}", self.num_params)?;
            writeln!(f, "Frame size: {}", self.frame_size)?;

            write!(f, "Upvalue data: ")?;
            multiline_list(f, self.upvalue_data.iter())?;

            write!(f, "GC constants: ")?;
            multiline_list(f, self.gc_constants.iter())?;

            writeln!(f, "Number constants: {:#?}", self.number_constants)?;

            f.write_str("\nCode: {\n")?;
            {
                let f = &mut indented(f);

                for instr in &self.instructions {
                    writeln!(f, "{instr}")?;
                }
            }
            f.write_str("}\n")?;
        }

        f.write_str("}\n")
    }
}

impl fmt::Display for ProtoFlags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.iter_names()
                .map(|(name, _)| name)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl fmt::Display for UVData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.flags.contains(UVFlags::LOCAL) {
            f.write_str("local ")?;
        }

        if self.flags.contains(UVFlags::IMMUTABLE) {
            f.write_str("const ")?;
        } else {
            f.write_str("mut ")?;
        }

        write!(f, "r{}", self.reg)
    }
}

impl fmt::Display for GCConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GCConstant::Child => f.write_str("Child"),
            GCConstant::Table(template_table) => write!(f, "{template_table}"),
            GCConstant::Str(s) => write!(f, "{s:?}"),
        }
    }
}

impl fmt::Debug for GCConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for TemplateTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("table {\n")?;
        {
            let f = &mut indented(f);

            f.write_str("array: ")?;
            multiline_list(f, self.array.iter())?;

            f.write_str("hash: ")?;
            if self.hash.is_empty() {
                f.write_str("{}\n")?;
            } else {
                f.write_str("{\n")?;
                {
                    let f = &mut indented(f);

                    let mut entries = Vec::from_iter(&self.hash);

                    entries.sort_by(|a, b| {
                        type ByteSlice = [u8; std::mem::size_of::<TValue>()];
                        let a = unsafe { &*(std::ptr::from_ref(a.0).cast::<ByteSlice>()) };
                        let b = unsafe { &*(std::ptr::from_ref(b.0).cast::<ByteSlice>()) };
                        a.cmp(b)
                    });

                    for (k, v) in &entries {
                        writeln!(f, "[{k}]: {v},")?;
                    }
                }

                f.write_str("}\n")?;
            }
        }

        f.write_str("}")
    }
}

impl fmt::Display for TValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TValue::Nil => f.write_str("nil"),
            TValue::False => f.write_str("false"),
            TValue::True => f.write_str("true"),
            TValue::String(s) => write!(f, "{s:?}"),
            TValue::Number(n) => write!(f, "{n}"),
        }
    }
}

impl fmt::Display for BCInstr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = self.op();
        let op_str = format!("{op:?}");
        write!(f, "{:<6}  ", op_str.cyan().bold())?;
        let modes = BCInsModes::from_op(op);
        match modes {
            BCInsModes::ABC {
                a_mode,
                b_mode,
                c_mode,
            } => {
                let a_str = format_operand8(self.a(), a_mode);
                let b_str = format_operand8(self.a(), b_mode);
                let c_str = format_operand8(self.a(), c_mode);
                write!(
                    f,
                    "{:>3} {:>3} {:>3}",
                    a_str.red(),
                    b_str.green(),
                    c_str.blue()
                )
            }
            BCInsModes::AD { a_mode, d_mode } => {
                let a_str = format_operand8(self.a(), a_mode);
                let d_str = format_operand16(self.d(), d_mode);

                write!(f, "{:>3} {:>7}", a_str.red(), d_str.magenta())
            }
        }
    }
}

fn format_operand8(val: u8, mode: BCOpMode) -> String {
    match mode {
        BCOpMode::Dst | BCOpMode::Base | BCOpMode::Var | BCOpMode::RBase => {
            format!("r{val}")
        }
        BCOpMode::Upval => format!("^{val}"),
        BCOpMode::None => String::new(),
        _ => format!("{val}"),
    }
}

fn format_operand16(val: u16, mode: BCOpMode) -> String {
    match mode {
        BCOpMode::Dst | BCOpMode::Base | BCOpMode::Var | BCOpMode::RBase => {
            format!("r{val}")
        }
        BCOpMode::Upval => format!("^{val}"),
        BCOpMode::Lits => format!("{}", val as i16),
        BCOpMode::Pri => match val {
            0 => "nil".to_owned(),
            1 => "false".to_owned(),
            2 => "true".to_owned(),
            n => format!("{n}!"),
        },
        BCOpMode::Str | BCOpMode::Table | BCOpMode::CData | BCOpMode::Func | BCOpMode::Num => {
            format!("#{}", !val as i16)
        }
        BCOpMode::Jump => format!("{}", u16::wrapping_sub(val, BCInstr::BIAS_J) as i16),
        _ => format!("{val}"),
    }
}

#[allow(clippy::upper_case_acronyms)]
pub(crate) enum BCInsModes {
    ABC {
        a_mode: BCOpMode,
        b_mode: BCOpMode,
        c_mode: BCOpMode,
    },
    AD {
        a_mode: BCOpMode,
        d_mode: BCOpMode,
    },
}

impl BCInsModes {
    pub(crate) fn from_op(op: BCOp) -> Self {
        let modes = BC_MODES[op as usize];
        let a_mode = BCOpMode::from_u16(modes & 0b111);
        let b_mode = BCOpMode::from_u16((modes >> 3) & 0b1111);
        let c_or_d_mode = BCOpMode::from_u16((modes >> 7) & 0b1111);

        let has_b = b_mode != BCOpMode::None;
        if has_b {
            BCInsModes::ABC {
                a_mode,
                b_mode,
                c_mode: c_or_d_mode,
            }
        } else {
            BCInsModes::AD {
                a_mode,
                d_mode: c_or_d_mode,
            }
        }
    }
}

#[derive(PartialEq, Clone, Copy)]
pub(crate) enum BCOpMode {
    // Mode A must be <= 7
    None,
    Dst,
    Base,
    Var,
    RBase,
    Upval,

    Lit,
    Lits,
    Pri,
    Num,
    Str,
    Table,
    Func,
    Jump,
    CData,
}

impl BCOpMode {
    const fn from_u16(value: u16) -> Self {
        match value {
            0 => BCOpMode::None,
            1 => BCOpMode::Dst,
            2 => BCOpMode::Base,
            3 => BCOpMode::Var,
            4 => BCOpMode::RBase,
            5 => BCOpMode::Upval,
            6 => BCOpMode::Lit,
            7 => BCOpMode::Lits,
            8 => BCOpMode::Pri,
            9 => BCOpMode::Num,
            10 => BCOpMode::Str,
            11 => BCOpMode::Table,
            12 => BCOpMode::Func,
            13 => BCOpMode::Jump,
            14 => BCOpMode::CData,
            _ => panic!(),
        }
    }
}

const BC_MODES: [u16; 97] = [
    12675, 12675, 14723, 14723, 8579, 8579, 9475, 9475, 9347, 9347, 9219, 9219, 45441, 45441,
    45440, 45440, 45827, 45827, 45441, 45441, 33153, 10625, 21657, 23705, 25753, 27801, 29849,
    21657, 23705, 25753, 27801, 29849, 20889, 22937, 24985, 27033, 29081, 31129, 16929, 46337,
    46849, 45953, 46209, 46081, 45314, 45697, 45445, 46341, 46213, 46085, 46724, 5633, 4865, 5505,
    1281, 3331, 409, 1305, 793, 409, 2459, 3355, 2843, 3202, 2459, 19250, 19250, 19202, 19202,
    19250, 19250, 45874, 46722, 45826, 45828, 45828, 45828, 46722, 46722, 46722, 46722, 45826,
    46722, 46722, 45826, 46724, 46724, 45828, 46724, 45060, 45060, 45828, 45060, 45060, 45828,
    45060, 45060,
];
