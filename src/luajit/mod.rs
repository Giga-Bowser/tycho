mod funcstate;
mod print;
mod utils;

pub mod bytecode;
pub mod compiler;

use bitflags::bitflags;

use self::{
    bytecode::{BCPos, BCReg},
    funcstate::VarIdx,
};
#[derive(Debug)]
struct ExprDesc<'s> {
    kind: ExprKind<'s>,
    true_jumplist: BCPos,
    false_jumplist: BCPos,
}

impl<'s> ExprDesc<'s> {
    pub(crate) fn new(kind: ExprKind<'s>) -> Self {
        ExprDesc {
            kind,
            true_jumplist: !0,
            false_jumplist: !0,
        }
    }

    pub(crate) fn has_jump(&self) -> bool {
        self.true_jumplist != self.false_jumplist
    }

    pub(crate) fn is_const(&self) -> bool {
        matches!(
            self.kind,
            ExprKind::KNil
                | ExprKind::KFalse
                | ExprKind::KTrue
                | ExprKind::KString(_)
                | ExprKind::KNumber(_)
        )
    }

    pub(crate) fn is_number(&self) -> bool {
        matches!(self.kind, ExprKind::KNumber(_))
    }
}

#[derive(Debug)]
enum ExprKind<'s> {
    // Constant expressions must be first and in this order:
    KNil,
    KFalse,
    KTrue,
    KString(&'s str), // sval = string value
    KNumber(f64),     // nval = number value
    // TODO: support FFI
    // KCData(u64),      // nval = cdata value, not treated as a constant expression

    // Non-constant expressions follow:

    // info = local register, aux = vstack index
    Local {
        local_reg: BCReg,
        vstack_idx: VarIdx,
    },
    // info = upvalue index, aux = vstack index
    Upvalue {
        upvalue_idx: u16,
        vstack_idx: VarIdx,
    },
    // sval = string value
    Global(&'s str),
    // info = table register, aux = index reg/byte/string const
    Indexed {
        table_reg: BCReg,
        index: BCReg,
    },
    // info = instruction PC
    Jmp {
        instr_idx: BCPos,
    },
    // info = instruction PC
    Relocable {
        instr_idx: BCPos,
    },
    // info = result register
    NonReloc {
        result_reg: BCReg,
    },
    // info = instruction PC, aux = base
    Call {
        instr_idx: BCPos,
        base: u32,
    },
    Void,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TValue {
    // Int(i64),
    // Uint(usize),
    Nil,
    False,
    True,
    String(String),
    Number(f64),
}

impl std::hash::Hash for TValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            TValue::String(s) => s.hash(state),
            TValue::Number(f) => f.to_bits().hash(state),
            _ => (),
        }
    }
}

impl std::cmp::Eq for TValue {}

#[derive(Debug, Default)]
pub struct VarInfo {
    // maybe this should be &'s str?
    pub name: Option<Box<str>>,
    pub startpc: BCPos,
    pub endpc: BCPos,
    pub slot: u8,
    pub flags: VarFlags,
}

bitflags! {
    #[derive(Debug, Default)]
    pub struct VarFlags: u8 {
        const VAR_RW = 0b001;
        const GOTO = 0b010;
        const LABEL = 0b100;
    }
}

impl VarInfo {
    #[inline]
    pub const fn is_goto(&self) -> bool {
        self.flags.contains(VarFlags::GOTO)
    }

    #[inline]
    pub const fn is_label(&self) -> bool {
        self.flags.contains(VarFlags::LABEL)
    }
}
