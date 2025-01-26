//! LuaJIT bytecode dump format

use std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
};

use super::utils::uleb128;

const BC_MAGIC: [u8; 3] = [0x1B, b'L', b'J'];
const BC_VERSION: u8 = 2;

pub fn dump_bc(header: &Header, protos: &[Proto]) -> Vec<u8> {
    let mut result = Vec::new();
    result.extend(BC_MAGIC);
    result.push(BC_VERSION);

    header.write(&mut result);

    for proto in protos {
        let mut proto_buf = Vec::new();
        proto.write(&mut proto_buf);
        uleb128::write_usize(&mut result, proto_buf.len());
        result.append(&mut proto_buf);
    }

    // null terminate
    result.push(0);

    result
}

pub fn read_dump(vec: &[u8]) -> (Header, Vec<Proto>) {
    let mut vec = VecDeque::from_iter(vec.iter().cloned());
    let magic = vec.drain(..3);
    assert!(
        magic.into_iter().eq(BC_MAGIC.into_iter()),
        "bad magic number!"
    );
    assert!(vec.pop_front() == Some(BC_VERSION), "bad bytecode version!");

    let header = Header::read(&mut vec);

    let mut protos = Vec::new();

    while *vec.front().unwrap() != 0x00 {
        let _proto_len = uleb128::read_usize(&mut vec);
        protos.push(Proto::read(&mut vec))
    }

    vec.pop_front();

    (header, protos)
}

#[derive(Debug)]
pub struct Header {
    big_endian: bool,
    strip: bool,
    ffi: bool,
    fr2: bool,
}

impl Default for Header {
    fn default() -> Self {
        Self {
            big_endian: false,
            strip: true,
            ffi: false,
            fr2: true,
        }
    }
}

impl Header {
    pub fn read(vec: &mut VecDeque<u8>) -> Self {
        let byte = vec.pop_front().unwrap();

        let big_endian = byte & 0b0001 != 0;
        let strip = byte & 0b0010 != 0;
        let ffi = byte & 0b0100 != 0;
        let fr2 = byte & 0b1000 != 0;

        Header {
            big_endian,
            strip,
            ffi,
            fr2,
        }
    }

    pub fn write(&self, vec: &mut Vec<u8>) {
        let mut flags = 0;

        flags |= if self.big_endian { 0b0001 } else { 0 };
        flags |= if self.strip { 0b0010 } else { 0 };
        flags |= if self.ffi { 0b0100 } else { 0 };
        flags |= if self.fr2 { 0b1000 } else { 0 };

        vec.push(flags);
    }
}

#[derive(Debug, Default)]
pub struct Proto {
    pub flags: ProtoFlags,
    pub num_params: u8,
    pub frame_size: u8,
    /// Instructions
    pub bcins: Vec<BCInstr>,
    pub upvalue_data: Vec<u16>,
    pub gc_constants: Vec<GCConstant>,
    pub number_constants: Vec<f64>,
}

impl Proto {
    pub fn read(vec: &mut VecDeque<u8>) -> Self {
        // read header
        let flags = ProtoFlags::read(vec);
        let num_params = vec.pop_front().unwrap();
        let frame_size = vec.pop_front().unwrap();
        let upvalue_data_len = vec.pop_front().unwrap() as usize;
        let gc_constants_len = uleb128::read_usize(vec);
        let number_constants_len = uleb128::read_usize(vec);
        let bcins_len = uleb128::read_usize(vec);

        // read body
        let bytes: Vec<u8> = vec.drain(..bcins_len * size_of::<BCInstr>()).collect();
        let bcins = bytes
            .chunks_exact(size_of::<BCInstr>())
            .map(|it| BCInstr::from_bytes(it.try_into().unwrap()))
            .collect();

        let bytes: Vec<u8> = vec.drain(..upvalue_data_len * size_of::<u16>()).collect();
        let upvalue_data = bytes
            .chunks_exact(size_of::<u16>())
            .map(|it| u16::from_le_bytes(it.try_into().unwrap()))
            .collect();

        let mut gc_constants = Vec::with_capacity(gc_constants_len);
        for _ in 0..gc_constants_len {
            gc_constants.push(GCConstant::read(vec))
        }

        let mut number_constants = Vec::with_capacity(number_constants_len);
        for _ in 0..number_constants_len {
            number_constants.push(read_number_constant(vec));
        }

        Proto {
            flags,
            num_params,
            frame_size,
            bcins,
            upvalue_data,
            gc_constants,
            number_constants,
        }
    }

    pub fn write(&self, vec: &mut Vec<u8>) {
        // write header
        self.flags.write(vec);
        vec.push(self.num_params);
        vec.push(self.frame_size);
        vec.push(self.upvalue_data.len() as u8);
        uleb128::write_usize(vec, self.gc_constants.len());
        uleb128::write_usize(vec, self.number_constants.len());
        uleb128::write_usize(vec, self.bcins.len());

        // write body
        vec.extend(self.bcins.iter().flat_map(|it| it.to_bytes()));
        vec.extend(self.upvalue_data.iter().flat_map(|it| it.to_le_bytes()));
        for gc_constant in &self.gc_constants {
            gc_constant.write(vec);
        }

        for n in &self.number_constants {
            write_number_constant(vec, *n);
        }
    }
}

#[derive(Default)]
pub struct ProtoFlags {
    /// Has child prototypes.
    pub child: bool,
    /// Vararg function.
    pub vararg: bool,
    /// Uses BC_KCDATA for FFI datatypes.
    pub ffi: bool,
    /// JIT disabled for this function.
    pub nojit: bool,
    /// Patched bytecode with ILOOP etc.
    pub iloop: bool,
}

impl std::fmt::Debug for ProtoFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut flags = Vec::new();

        if self.child {
            flags.push("child");
        }

        if self.vararg {
            flags.push("vararg");
        }

        if self.ffi {
            flags.push("ffi");
        }

        if self.nojit {
            flags.push("nojit");
        }

        if self.iloop {
            flags.push("iloop");
        }

        write!(f, "[{}]", flags.join(", "))
    }
}

impl ProtoFlags {
    pub fn read(vec: &mut VecDeque<u8>) -> Self {
        let byte = vec.pop_front().unwrap();

        ProtoFlags {
            child: byte & 0b00001 != 0,
            vararg: byte & 0b00010 != 0,
            ffi: byte & 0b00100 != 0,
            nojit: byte & 0b01000 != 0,
            iloop: byte & 0b10000 != 0,
        }
    }

    pub fn write(&self, vec: &mut Vec<u8>) {
        let mut flags = 0;

        flags |= if self.child { 0b00001 } else { 0 };
        flags |= if self.vararg { 0b00010 } else { 0 };
        flags |= if self.ffi { 0b00100 } else { 0 };
        flags |= if self.nojit { 0b01000 } else { 0 };
        flags |= if self.iloop { 0b10000 } else { 0 };

        vec.push(flags);
    }
}

pub enum GCConstant {
    Child,
    Table(KTable),
    // FFI stuff:
    // I64,
    // U64,
    // Complex,
    Str(String),
}

impl std::fmt::Debug for GCConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Child => write!(f, "Child"),
            Self::Table(table) => table.fmt(f),
            Self::Str(s) => write!(f, "{s:?}"),
        }
    }
}

impl GCConstant {
    fn discriminator(&self) -> usize {
        match self {
            GCConstant::Child => 0,
            GCConstant::Table(_) => 1,
            GCConstant::Str(_) => 5,
        }
    }

    pub fn read(vec: &mut VecDeque<u8>) -> Self {
        let discrim = uleb128::read_usize(vec);
        match discrim {
            0 => GCConstant::Child,
            1 => GCConstant::Table(KTable::read(vec)),
            2..=4 => todo!("haven't implemented FFI stuff yet"),
            n => {
                let len = n - 5;
                let bytes: Vec<u8> = vec.drain(..len).collect();
                GCConstant::Str(unsafe { String::from_utf8_unchecked(bytes) })
            }
        }
    }

    pub fn write(&self, vec: &mut Vec<u8>) {
        let discrim = self.discriminator();
        match self {
            GCConstant::Child => {
                uleb128::write_usize(vec, discrim);
            }
            GCConstant::Table(table) => {
                uleb128::write_usize(vec, discrim);
                table.write(vec)
            }
            GCConstant::Str(s) => {
                uleb128::write_usize(vec, discrim + s.len());
                vec.extend(s.as_bytes());
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct KTable {
    array: Vec<KTableVal>,
    hash: HashMap<KTableVal, KTableVal>,
}

impl KTable {
    pub fn read(vec: &mut VecDeque<u8>) -> Self {
        let mut result = KTable::default();
        let array_len = uleb128::read_usize(vec);
        let hash_len = uleb128::read_usize(vec);

        for _ in 0..array_len {
            result.array.push(KTableVal::read(vec));
        }

        for _ in 0..hash_len {
            let key = KTableVal::read(vec);
            let val = KTableVal::read(vec);
            result.hash.insert(key, val);
        }

        result
    }

    pub fn write(&self, vec: &mut Vec<u8>) {
        uleb128::write_usize(vec, self.array.len());
        uleb128::write_usize(vec, self.hash.len());
        for ktabk in &self.array {
            ktabk.write(vec, true);
        }
        for (key, val) in &self.hash {
            key.write(vec, false);
            val.write(vec, true);
        }
    }
}

// pub struct TValue(pub i64);

// pub enum TValueKind {
//     Nil,
//     False,
//     True,
//     LightUD,
//     Str,
//     Thread,
//     Proto,
//     Func,
//     CData,
//     Tab,
//     UData,
//     Num,
// }

// const TOP17: u32 = 0x1FFFF;
// const TNIL: u32 = TOP17;
// const TFALSE: u32 = TOP17 & !1;
// const TTRUE: u32 = TOP17 & !2;
// const TLIGHTUD: u32 = TOP17 & !3;
// const TSTR: u32 = TOP17 & !4;
// const TUPVAL: u32 = TOP17 & !5;
// const TTHREAD: u32 = TOP17 & !6;
// const TPROTO: u32 = TOP17 & !7;
// const TFUNC: u32 = TOP17 & !8;
// const TTRACE: u32 = TOP17 & !9;
// const TCDATA: u32 = TOP17 & !10;
// const TTAB: u32 = TOP17 & !11;
// const TUDATA: u32 = TOP17 & !12;

// impl TValue {
//     fn itype(&self) -> u32 {
//         (self.0 >> 47) as u32
//     }

//     pub fn is_nil(&self) -> bool {
//         self.0 == -1
//     }

//     pub fn kind(&self) -> TValueKind {
//         match self.itype() {
//             TNIL => TValueKind::Nil,
//             TFALSE => TValueKind::False,
//             TTRUE => TValueKind::True,
//             TLIGHTUD => TValueKind::LightUD,
//             TSTR => TValueKind::Str,
//             TUPVAL => TValueKind::Thread,
//             TTHREAD => TValueKind::Proto,
//             TPROTO => TValueKind::Func,
//             TFUNC => TValueKind::Thread,
//             TTRACE => TValueKind::Proto,
//             TCDATA => TValueKind::CData,
//             TTAB => TValueKind::Tab,
//             TUDATA => TValueKind::UData,
//             _ => TValueKind::Num,
//         }
//     }

//     // pub fn write(&self, vec: &mut Vec<u8>) {
//     //     match self.kind() {
//     //         TValueKind::Str => {

//     //         }
//     //     }
//     // }
// }

#[derive(Debug, PartialEq)]
pub enum KTableVal {
    Nil,
    False,
    True,
    Num(f64),
    Str(String),
}

impl Eq for KTableVal {}

impl Hash for KTableVal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            KTableVal::Num(n) => {
                n.to_bits().hash(state);
            }
            KTableVal::Str(s) => {
                s.hash(state);
            }
            _ => (),
        }
    }
}

impl KTableVal {
    pub fn read(vec: &mut VecDeque<u8>) -> Self {
        let tag = uleb128::read_u32(vec);
        match tag {
            0 => KTableVal::Nil,
            1 => KTableVal::False,
            2 => KTableVal::True,
            3 => {
                let n = uleb128::read_u32(vec);
                KTableVal::Num(n as f64)
            }
            4 => {
                let bits = uleb128::read_usize(vec);
                KTableVal::Num(f64::from_bits(bits as u64))
            }
            n => {
                let len = n as usize - 5;
                let bytes: Vec<u8> = vec.drain(..len).collect();
                KTableVal::Str(unsafe { String::from_utf8_unchecked(bytes) })
            }
        }
    }

    pub fn write(&self, vec: &mut Vec<u8>, narrow: bool) {
        match self {
            KTableVal::Nil => vec.push(0),
            KTableVal::False => vec.push(1),
            KTableVal::True => vec.push(2),
            KTableVal::Num(n) => {
                let int_n = *n as i32;
                if int_n as f64 == *n && narrow {
                    vec.push(3);
                    uleb128::write_u32(vec, int_n as u32);
                } else {
                    vec.push(4);
                    uleb128::write_usize(vec, n.to_bits() as usize)
                }
            }
            KTableVal::Str(s) => {
                uleb128::write_usize(vec, 5 + s.len());
                vec.extend(s.as_bytes());
            }
        }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct BCInstr(pub u32);

impl std::fmt::Debug for BCInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = self.op();
        let op_str = format!("{op:?}");
        let modes = BCInsModes::from_op(op);
        match modes {
            BCInsModes::ABC {
                a_mode,
                b_mode,
                c_mode,
            } => {
                let a_str = format_operand8(self.a(), a_mode);
                let b_str = format_operand8(self.a(), c_mode);
                let c_str = format_operand8(self.a(), b_mode);
                write!(f, "\x1b[36m{op_str:<6}\x1b[0m \x1b[31m{a_str:>3}\x1b[0m \x1b[32m{b_str:>3}\x1b[0m \x1b[34m{c_str:>3}\x1b[0m")
            }
            BCInsModes::AD { a_mode, d_mode } => {
                let a_str = format_operand8(self.a(), a_mode);
                let d_str = format_operand16(self.d(), d_mode);

                write!(
                    f,
                    "\x1b[36m{op_str:<6}\x1b[0m \x1b[31m{a_str:>3}\x1b[0m \x1b[34m{d_str:>7}\x1b[0m"
                )
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
        BCOpMode::Lits => format!("{}", val as i16),
        BCOpMode::Pri => match val {
            0 => "nil".to_owned(),
            1 => "false".to_owned(),
            2 => "true".to_owned(),
            n => format!("{n}!"),
        },
        BCOpMode::Str | BCOpMode::Table | BCOpMode::CData | BCOpMode::Func => {
            format!("#{}", val as i16)
        }
        BCOpMode::Dst | BCOpMode::Base | BCOpMode::Var | BCOpMode::RBase => {
            format!("r{val}")
        }
        BCOpMode::Jump => format!("{}", (val - 0x8000) as i16),
        _ => format!("{val}"),
    }
}

impl BCInstr {
    pub const fn new_abc(op: BCOp, a: u8, b: u8, c: u8) -> BCInstr {
        Self(u32::from_le_bytes([op as u8, a, c, b]))
    }

    pub const fn new_ad(op: BCOp, a: u8, d: u16) -> BCInstr {
        Self(u32::from_le_bytes([
            op as u8,
            a,
            d.to_ne_bytes()[0],
            d.to_ne_bytes()[1],
        ]))
    }

    pub const fn new_jmp(op: BCOp, a: u8, j: u16) -> BCInstr {
        let d = j | 0x8000;
        Self(u32::from_le_bytes([
            op as u8,
            a,
            d.to_ne_bytes()[0],
            d.to_ne_bytes()[1],
        ]))
    }

    pub const fn op(&self) -> BCOp {
        unsafe { std::mem::transmute::<u8, BCOp>((self.0 & 0xFF) as u8) }
    }

    pub const fn a(&self) -> u8 {
        ((self.0 >> 8) & 0xFF) as u8
    }

    pub const fn b(&self) -> u8 {
        (self.0 >> 24) as u8
    }

    pub const fn c(&self) -> u8 {
        ((self.0 >> 16) & 0xFF) as u8
    }

    pub const fn d(&self) -> u16 {
        (self.0 >> 16) as u16
    }

    pub const fn j(&self) -> u16 {
        self.d() - 0x8000
    }

    pub const fn to_bytes(self) -> [u8; 4] {
        self.0.to_le_bytes()
    }

    pub const fn from_bytes(bytes: [u8; 4]) -> Self {
        BCInstr(u32::from_le_bytes(bytes))
    }
}

#[allow(clippy::upper_case_acronyms, unused)]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum BCOp {
    // Comparison ops. ORDER OPR.
    ISLT, //  var,    ___,    var,    lt
    ISGE, //  var,    ___,    var,    lt
    ISLE, //  var,    ___,    var,    le
    ISGT, //  var,    ___,    var,    le

    ISEQV, // var,    ___,    var,    eq
    ISNEV, // var,    ___,    var,    eq
    ISEQS, // var,    ___,    str,    eq
    ISNES, // var,    ___,    str,    eq
    ISEQN, // var,    ___,    num,    eq
    ISNEN, // var,    ___,    num,    eq
    ISEQP, // var,    ___,    pri,    eq
    ISNEP, // var,    ___,    pri,    eq

    // Unary test and copy ops
    ISTC,   //dst,    ___,    var,    ___
    ISFC,   //dst,    ___,    var,    ___
    IST,    //___,    ___,    var,    ___
    ISF,    //___,    ___,    var,    ___
    ISTYPE, //var,    ___,    lit,    ___
    ISNUM,  //var,    ___,    lit,    ___

    // Unary ops.
    MOV, //   dst,    ___,    var,    ___
    NOT, //   dst,    ___,    var,    ___
    UNM, //   dst,    ___,    var,    unm
    LEN, //   dst,    ___,    var,    len

    // Binary ops. ORDER OPR. VV last, POW must be next.
    ADDVN, // dst,    var,    num,    add
    SUBVN, // dst,    var,    num,    sub
    MULVN, // dst,    var,    num,    mul
    DIVVN, // dst,    var,    num,    div
    MODVN, // dst,    var,    num,    mod

    ADDNV, // dst,    var,    num,    add
    SUBNV, // dst,    var,    num,    sub
    MULNV, // dst,    var,    num,    mul
    DIVNV, // dst,    var,    num,    div
    MODNV, // dst,    var,    num,    mod

    ADDVV, // dst,    var,    var,    add
    SUBVV, // dst,    var,    var,    sub
    MULVV, // dst,    var,    var,    mul
    DIVVV, // dst,    var,    var,    div
    MODVV, // dst,    var,    var,    mod

    POW, //   dst,    var,    var,    pow
    CAT, //   dst,    rbase,  rbase,  concat

    // Constant ops.
    KSTR,   //dst,    ___,    str,    ___
    KCDATA, //dst,    ___,    cdata,  ___
    KSHORT, //dst,    ___,    lits,   ___
    KNUM,   //dst,    ___,    num,    ___
    KPRI,   //dst,    ___,    pri,    ___
    KNIL,   //base,   ___,    base,   ___

    // Upvalue and function ops.
    UGET,  // dst,    ___,    uv,     ___
    USETV, // uv,     ___,    var,    ___
    USETS, // uv,     ___,    str,    ___
    USETN, // uv,     ___,    num,    ___
    USETP, // uv,     ___,    pri,    ___
    UCLO,  // rbase,  ___,    jump,   ___
    FNEW,  // dst,    ___,    func,   gc

    // Table ops.
    TNEW,  // dst,    ___,    lit,    gc
    TDUP,  // dst,    ___,    tab,    gc
    GGET,  // dst,    ___,    str,    index
    GSET,  // var,    ___,    str,    newindex
    TGETV, // dst,    var,    var,    index
    TGETS, // dst,    var,    str,    index
    TGETB, // dst,    var,    lit,    index
    TGETR, // dst,    var,    var,    index
    TSETV, // var,    var,    var,    newindex
    TSETS, // var,    var,    str,    newindex
    TSETB, // var,    var,    lit,    newindex
    TSETM, // base,   ___,    num,    newindex
    TSETR, // var,    var,    var,    newindex

    // Calls and vararg handling. T = tail call.
    CALLM,  //base,   lit,    lit,    call
    CALL,   //base,   lit,    lit,    call
    CALLMT, //base,   ___,    lit,    call
    CALLT,  //base,   ___,    lit,    call
    ITERC,  //base,   lit,    lit,    call
    ITERN,  //base,   lit,    lit,    call
    VARG,   //base,   lit,    lit,    ___
    ISNEXT, //base,   ___,    jump,   ___

    // Returns.
    RETM, //  base,   ___,    lit,    ___
    RET,  //  rbase,  ___,    lit,    ___
    RET0, //  rbase,  ___,    lit,    ___
    RET1, //  rbase,  ___,    lit,    ___

    // Loops and branches. I/J = interp/JIT, I/C/L = init/call/loop.
    FORI,  // base,   ___,    jump,   ___
    JFORI, // base,   ___,    jump,   ___

    FORL,  // base,   ___,    jump,   ___
    IFORL, // base,   ___,    jump,   ___
    JFORL, // base,   ___,    lit,    ___

    ITERL,  //base,   ___,    jump,   ___
    IITERL, //base,   ___,    jump,   ___
    JITERL, //base,   ___,    lit,    ___

    LOOP,  // rbase,  ___,    jump,   ___
    ILOOP, // rbase,  ___,    jump,   ___
    JLOOP, // rbase,  ___,    lit,    ___

    JMP, //   rbase,  ___,    jump,   ___

    // Function headers. I/J = interp/JIT, F/V/C = fixarg/vararg/C func.
    FUNCF,  //rbase,  ___,    ___,    ___
    IFUNCF, //rbase,  ___,    ___,    ___
    JFUNCF, //rbase,  ___,    lit,    ___
    FUNCV,  //rbase,  ___,    ___,    ___
    IFUNCV, //rbase,  ___,    ___,    ___
    JFUNCV, //rbase,  ___,    lit,    ___
    FUNCC,  //rbase,  ___,    ___,    ___
    FUNCCW, //rbase,  ___,    ___,    ___
}

#[allow(clippy::upper_case_acronyms)]
pub enum BCInsModes {
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
    pub fn from_op(op: BCOp) -> Self {
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

#[derive(PartialEq)]
pub enum BCOpMode {
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

fn read_number_constant(vec: &mut VecDeque<u8>) -> f64 {
    let is_num = vec.front().unwrap() & 1 == 1;

    let lo = {
        let mut byte = vec.pop_front().unwrap();
        // we shift right by 1 to discard the LSB used to signify float vs int
        let low_bits = (byte & 0x7F) as u32 >> 1;
        let mut shift = 6;
        let mut result = low_bits;

        while byte & 0x80 != 0 {
            byte = vec.pop_front().unwrap();
            let low_bits = (byte & 0x7F) as u32;
            result |= low_bits << shift;
            shift += 7;
        }

        result
    };

    if is_num {
        let hi = uleb128::read_u32(vec) as u64;

        f64::from_bits((hi << 32) | (lo as u64))
    } else {
        lo as f64
    }
}

fn write_number_constant(vec: &mut Vec<u8>, n: f64) {
    let int_n = n as i32;
    if int_n as f64 == n {
        let k = int_n as u32;
        uleb128::write_u32(vec, (k << 1) | (k & 0x80000000));

        if int_n < 0 {
            let last = vec.last_mut().unwrap();
            *last = (*last & 7) | ((k >> 27) & 0x18) as u8;
        }
    } else {
        let hi = (n.to_bits() >> 32) as u32;
        let lo = n.to_bits() as u32;

        uleb128::write_u32(vec, 1 + ((2 * lo) | (lo & 0x80000000)));
        if lo >= 0x80000000 {
            let last = vec.last_mut().unwrap();
            *last = (*last & 7) | ((lo >> 27) & 0x18) as u8;
        }
        uleb128::write_u32(vec, hi);
    }
}
