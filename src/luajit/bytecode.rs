//! LuaJIT bytecode dump format

use std::{
    collections::VecDeque,
    ops::{Index, IndexMut},
    path::{Path, PathBuf},
};

use bitflags::bitflags;
use rustc_hash::FxHashMap;

use crate::{
    luajit::{utils::uleb128, TValue},
    parser::ast,
};

const BC_MAGIC: [u8; 3] = [0x1B, b'L', b'J'];
const BC_VERSION: u8 = 2;

pub fn dump_protos(protos: &[Proto]) -> Vec<u8> {
    let mut result = Vec::new();
    result.extend(BC_MAGIC);
    result.push(BC_VERSION);

    Header::default().write(&mut result);

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

pub(crate) fn dump_bc(header: &Header, protos: &[Proto]) -> Vec<u8> {
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

pub(crate) fn read_dump(vec: &[u8]) -> (Header, Vec<Proto>) {
    let mut vec: VecDeque<u8> = vec.iter().copied().collect();
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
        protos.push(Proto::read(&mut vec, header.flags));
    }

    vec.pop_front();

    (header, protos)
}

#[derive(Debug, Default)]
pub(crate) struct Header {
    pub flags: HeaderFlags,
    pub chunk_name: Option<ChunkName>,
}

impl Header {
    pub(crate) fn read(vec: &mut VecDeque<u8>) -> Self {
        let flags = HeaderFlags::read(vec);

        let chunk_name = if flags.debug() {
            Some(ChunkName::read(vec))
        } else {
            None
        };

        Self { flags, chunk_name }
    }

    pub(crate) fn write(&self, vec: &mut Vec<u8>) {
        self.flags.write(vec);

        if let Some(chunk_name) = &self.chunk_name {
            if self.flags.debug() {
                chunk_name.write(vec);
            }
        }
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub(crate) struct HeaderFlags: u8 {
        const BIG_ENDIAN = 0b0001;
        const STRIP = 0b0010;
        const FFI = 0b0100;
        const FR2 = 0b1000;
    }
}

impl Default for HeaderFlags {
    fn default() -> Self {
        HeaderFlags::STRIP | HeaderFlags::FR2
    }
}

impl HeaderFlags {
    pub(crate) fn read(vec: &mut VecDeque<u8>) -> Self {
        let byte = vec.pop_front().unwrap();

        HeaderFlags::from_bits_truncate(byte)
    }

    pub(crate) fn write(self, vec: &mut Vec<u8>) {
        vec.push(self.bits());
    }

    pub(crate) const fn debug(self) -> bool {
        !self.contains(Self::STRIP)
    }
}

#[derive(Debug)]
pub(crate) enum ChunkName {
    File(Box<Path>),
    Custom(Box<str>),
}

impl ChunkName {
    pub(crate) fn read(vec: &mut VecDeque<u8>) -> Self {
        let name_len = uleb128::read_usize(vec);
        let name_vec: Vec<u8> = vec.drain(..name_len).collect();
        match name_vec.split_at(1) {
            (b"@", filename) => {
                Self::File(PathBuf::from(String::from_utf8_lossy(filename).as_ref()).into())
            }
            (b"=", name) => Self::Custom(String::from_utf8_lossy(name).into()),
            _ => panic!("bad chunk name!"),
        }
    }

    pub(crate) fn write(&self, vec: &mut Vec<u8>) {
        match self {
            ChunkName::File(path) => {
                let path_str = path.to_string_lossy();
                uleb128::write_usize(vec, path_str.len() + 1);
                vec.push(b'@');
                vec.extend(path_str.as_bytes());
            }
            ChunkName::Custom(name) => {
                uleb128::write_usize(vec, name.len() + 1);
                vec.push(b'@');
                vec.extend(name.as_bytes());
            }
        };
    }
}

#[derive(Debug, Default)]
pub struct Proto {
    pub flags: ProtoFlags,
    pub num_params: u8,
    pub frame_size: u8,
    pub instructions: Vec<BCInstr>,
    pub upvalue_data: Vec<UVData>,
    pub gc_constants: Vec<GCConstant>,
    pub number_constants: Vec<f64>,
    pub debug_info: Option<Box<DebugInfo>>,
}

impl Proto {
    pub(crate) fn read(vec: &mut VecDeque<u8>, header_flags: HeaderFlags) -> Self {
        // read header
        let flags = ProtoFlags::from_bits_retain(vec.pop_front().unwrap());
        let num_params = vec.pop_front().unwrap();
        let frame_size = vec.pop_front().unwrap();
        let upvalue_data_len = vec.pop_front().unwrap() as usize;
        let gc_constants_len = uleb128::read_usize(vec);
        let number_constants_len = uleb128::read_usize(vec);
        let bcins_len = uleb128::read_usize(vec);

        let (debug_len, first_line, num_lines) = if header_flags.debug() {
            let debug_len = uleb128::read_usize(vec);

            if debug_len != 0 {
                let first_line = uleb128::read_usize(vec);
                let num_lines = uleb128::read_usize(vec);

                (debug_len, first_line, num_lines)
            } else {
                (0, 0, 0)
            }
        } else {
            (0, 0, 0)
        };

        // read body
        let bytes: Vec<u8> = vec.drain(..bcins_len * size_of::<BCInstr>()).collect();
        let instructions = bytes
            .chunks_exact(size_of::<BCInstr>())
            .map(|it| BCInstr::from_bytes(it.try_into().unwrap()))
            .collect();

        let bytes: Vec<u8> = vec.drain(..upvalue_data_len * size_of::<u16>()).collect();
        let upvalue_data = bytes
            .chunks_exact(2)
            .map(|it| UVData::from_bytes(it.try_into().unwrap()))
            .collect();

        let mut gc_constants = Vec::with_capacity(gc_constants_len);
        for _ in 0..gc_constants_len {
            gc_constants.push(GCConstant::read(vec));
        }

        let mut number_constants = Vec::with_capacity(number_constants_len);
        for _ in 0..number_constants_len {
            number_constants.push(read_number_constant(vec));
        }

        let debug_info = if debug_len != 0 {
            Some(Box::new(DebugInfo {
                first_line,
                num_lines,
                info: vec.drain(..debug_len).collect(),
            }))
        } else {
            None
        };

        Proto {
            flags,
            num_params,
            frame_size,
            instructions,
            upvalue_data,
            gc_constants,
            number_constants,
            debug_info,
        }
    }

    pub(crate) fn write(&self, vec: &mut Vec<u8>) {
        // write header
        vec.push(self.flags.intersection(ProtoFlags::DUMP).bits());
        vec.push(self.num_params);
        vec.push(self.frame_size);
        vec.push(self.upvalue_data.len() as u8);
        uleb128::write_usize(vec, self.gc_constants.len());
        uleb128::write_usize(vec, self.number_constants.len());
        uleb128::write_usize(vec, self.instructions.len());

        // write body
        vec.extend(self.instructions.iter().flat_map(|it| it.to_bytes()));
        vec.extend(self.upvalue_data.iter().flat_map(|it| it.to_bytes()));
        for gc_constant in &self.gc_constants {
            gc_constant.write(vec);
        }

        for n in &self.number_constants {
            write_number_constant(vec, *n);
        }
    }
}

bitflags! {
    #[derive(Debug, Default, Clone, Copy)]
    pub struct ProtoFlags: u8 {
        const CHILD = 0b00000001;
        const VARARG = 0b00000010;
        const FFI = 0b00000100;
        const NOJIT = 0b00001000;
        const ILOOP = 0b00010000;

        const DUMP = 0b00011111;

        // Only used during compiling
        const HAS_RETURN = 0b00100000;
        const FIXUP_RETURN = 0b01000000;
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct BCInstr(pub u32);

impl BCInstr {
    // i make these u32's because you're pretty much always
    // comparing a larger integer type with these
    pub const MAX_A: u32 = 0xFF;
    pub const MAX_B: u32 = 0xFF;
    pub const MAX_C: u32 = 0xFF;
    pub const MAX_D: u32 = 0xFFFF;
    pub const BIAS_J: u16 = 0x8000;

    #[must_use]
    #[inline]
    pub const fn new_abc(op: BCOp, a: u8, b: u8, c: u8) -> Self {
        Self(u32::from_le_bytes([op as u8, a, c, b]))
    }

    #[must_use]
    #[inline]
    pub const fn new_ad(op: BCOp, a: u8, d: u16) -> Self {
        Self(u32::from_le_bytes([
            op as u8,
            a,
            d.to_ne_bytes()[0],
            d.to_ne_bytes()[1],
        ]))
    }

    #[must_use]
    #[inline]
    pub const fn new_jmp(op: BCOp, a: u8, j: i16) -> Self {
        BCInstr::new_ad(op, a, u16::wrapping_add(j as u16, BCInstr::BIAS_J))
    }

    #[inline]
    pub const fn op(&self) -> BCOp {
        unsafe { std::mem::transmute::<u8, BCOp>(self.0 as u8) }
    }

    #[inline]
    pub const fn set_op(&mut self, val: BCOp) {
        self.0 = (self.0 & 0xFFFFFF00) | (val as u32);
    }

    #[inline]
    pub const fn a(&self) -> u8 {
        (self.0 >> 8) as u8
    }

    #[inline]
    pub const fn set_a(&mut self, val: u8) {
        self.0 = (self.0 & 0xFFFF00FF) | ((val as u32) << 8);
    }

    #[inline]
    pub const fn b(&self) -> u8 {
        (self.0 >> 24) as u8
    }

    #[inline]
    pub const fn set_b(&mut self, val: u8) {
        self.0 = (self.0 & 0x00FFFFFF) | ((val as u32) << 24);
    }

    #[inline]
    pub const fn c(&self) -> u8 {
        (self.0 >> 16) as u8
    }

    #[inline]
    pub const fn set_c(&mut self, val: u8) {
        self.0 = (self.0 & 0xFF00FFFF) | ((val as u32) << 16);
    }

    #[inline]
    pub const fn d(&self) -> u16 {
        (self.0 >> 16) as u16
    }

    #[inline]
    pub const fn set_d(&mut self, val: u16) {
        self.0 = (self.0 & 0x0000FFFF) | ((val as u32) << 16);
    }

    #[inline]
    pub const fn j(&self) -> i16 {
        (self.d() as i32 - BCInstr::BIAS_J as i32) as i16
    }

    #[inline]
    pub const fn set_j(&mut self, val: i16) {
        self.set_d((val as i32 + BCInstr::BIAS_J as i32) as u16);
    }

    #[must_use]
    #[inline]
    pub const fn to_bytes(self) -> [u8; 4] {
        self.0.to_le_bytes()
    }

    #[inline]
    pub const fn from_bytes(bytes: [u8; 4]) -> Self {
        BCInstr(u32::from_le_bytes(bytes))
    }
}

pub type BCPos = u32;
// NOTE: i should be using an option, probably
pub(crate) const NO_JMP: BCPos = !0;

pub type BCReg = u32;
pub(crate) const NO_REG: BCReg = 0xFF;

#[derive(Debug, Clone, Default)]
pub struct Bytecode {
    pub vec: Vec<BCInstr>,
}

impl Bytecode {
    #[inline]
    pub(crate) fn len(&self) -> BCPos {
        self.vec.len() as BCPos
    }

    #[inline]
    pub(crate) fn last(&self) -> &BCInstr {
        &self[self.len() - 1]
    }

    #[inline]
    pub(crate) fn last_mut(&mut self) -> &mut BCInstr {
        let idx = self.len() - 1;
        &mut self[idx]
    }

    #[inline]
    pub(crate) fn pop(&mut self) -> BCInstr {
        self.vec.pop().unwrap()
    }
}

impl Index<BCPos> for Bytecode {
    type Output = BCInstr;

    fn index(&self, index: BCPos) -> &Self::Output {
        &self.vec[index as usize]
    }
}

impl IndexMut<BCPos> for Bytecode {
    fn index_mut(&mut self, index: BCPos) -> &mut Self::Output {
        &mut self.vec[index as usize]
    }
}

#[allow(clippy::upper_case_acronyms, unused)]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl BCOp {
    pub const OFFSET_S: u8 = BCOp::ISEQS as u8 - BCOp::ISEQV as u8;
    pub const OFFSET_N: u8 = BCOp::ISEQN as u8 - BCOp::ISEQV as u8;
    pub const OFFSET_P: u8 = BCOp::ISEQP as u8 - BCOp::ISEQV as u8;

    #[inline]
    pub const fn from_u8(val: u8) -> Self {
        debug_assert!(val <= 96);
        unsafe { std::mem::transmute(val) }
    }

    #[inline]
    pub(crate) fn from_ast(op: ast::OpKind) -> Self {
        BCOp::from_u8(op as u8 + BCOp::ADDVV as u8)
    }

    #[inline]
    #[must_use]
    pub(crate) fn invert(self) -> Self {
        BCOp::from_u8((self as u8) ^ 1)
    }

    #[inline]
    #[must_use]
    pub const fn transform(self, from: Self, to: Self) -> Self {
        let offset = u8::wrapping_sub(to as u8, from as u8);
        BCOp::from_u8(u8::wrapping_add(self as u8, offset))
    }

    #[inline]
    pub(crate) fn is_ret(self) -> bool {
        matches!(
            self,
            BCOp::RETM | BCOp::RET | BCOp::RET0 | BCOp::RET1 | BCOp::CALLT | BCOp::CALLMT
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UVData {
    pub flags: UVFlags,
    pub reg: u8,
}

impl UVData {
    pub(crate) fn from_bytes(bytes: [u8; 2]) -> Self {
        Self {
            flags: UVFlags::from_bits_retain(bytes[1]),
            reg: bytes[0],
        }
    }

    pub(crate) fn to_bytes(self) -> [u8; 2] {
        [self.reg, self.flags.bits()]
    }
}

bitflags! {
    #[derive(Debug, Default, Clone, Copy)]
    pub struct UVFlags: u8 {
        const IMMUTABLE = 0b01000000;
        const LOCAL = 0b10000000;
    }
}

#[derive(Clone)]
pub enum GCConstant {
    Child,
    Table(TemplateTable),
    // FFI stuff:
    // I64,
    // U64,
    // Complex,
    Str(String),
}

impl GCConstant {
    fn discriminator(&self) -> usize {
        match self {
            GCConstant::Child => 0,
            GCConstant::Table(_) => 1,
            GCConstant::Str(_) => 5,
        }
    }

    pub(crate) fn read(vec: &mut VecDeque<u8>) -> Self {
        let discrim = uleb128::read_usize(vec);
        match discrim {
            0 => GCConstant::Child,
            1 => GCConstant::Table(TemplateTable::read(vec)),
            2..=4 => todo!("haven't implemented FFI stuff yet"),
            n => {
                let len = n - 5;
                let bytes: Vec<u8> = vec.drain(..len).collect();
                GCConstant::Str(unsafe { String::from_utf8_unchecked(bytes) })
            }
        }
    }

    pub(crate) fn write(&self, vec: &mut Vec<u8>) {
        let discrim = self.discriminator();
        match self {
            GCConstant::Child => {
                uleb128::write_usize(vec, discrim);
            }
            GCConstant::Table(table) => {
                uleb128::write_usize(vec, discrim);
                table.write(vec);
            }
            GCConstant::Str(s) => {
                uleb128::write_usize(vec, discrim + s.len());
                vec.extend(s.as_bytes());
            }
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct TemplateTable {
    pub array: Vec<TValue>,
    pub hash: FxHashMap<TValue, TValue>,
}

impl TemplateTable {
    pub(crate) fn with_size(array_len: usize) -> Self {
        Self {
            array: vec![TValue::Nil; array_len],
            ..Default::default()
        }
    }

    pub(crate) fn read(vec: &mut VecDeque<u8>) -> Self {
        let mut result = TemplateTable::default();
        let array_len = uleb128::read_usize(vec);
        let hash_len = uleb128::read_usize(vec);

        for _ in 0..array_len {
            result.array.push(TValue::read(vec));
        }

        for _ in 0..hash_len {
            let key = TValue::read(vec);
            let val = TValue::read(vec);
            result.hash.insert(key, val);
        }

        result
    }

    pub(crate) fn write(&self, vec: &mut Vec<u8>) {
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

    pub(crate) fn insert(&mut self, k: TValue, v: TValue) -> Option<TValue> {
        match k {
            TValue::Number(n) => {
                let int_n = n as i32 as usize;
                if int_n as f64 == n {
                    if int_n < self.array.len() {
                        let old = std::mem::replace(&mut self.array[int_n], v);
                        if let TValue::Nil = old {
                            None
                        } else {
                            Some(old)
                        }
                    } else {
                        self.hash.insert(k, v)
                    }
                } else {
                    self.hash.insert(k, v)
                }
            }
            TValue::Nil => panic!("template table cannot be indexed by nil"),
            _ => self.hash.insert(k, v),
        }
    }
}

impl TValue {
    pub(crate) fn read(vec: &mut VecDeque<u8>) -> Self {
        let tag = uleb128::read_usize(vec);
        match tag {
            0 => TValue::Nil,
            1 => TValue::False,
            2 => TValue::True,
            3 => {
                let n = uleb128::read_u32(vec);
                TValue::Number(n as f64)
            }
            4 => {
                let lo = uleb128::read_u32(vec);
                let hi = uleb128::read_u32(vec);
                TValue::Number(f64::from_bits(((hi as u64) << 32) | lo as u64))
            }
            n => {
                let len = n - 5;
                let bytes: Vec<u8> = vec.drain(..len).collect();
                TValue::String(unsafe { String::from_utf8_unchecked(bytes) })
            }
        }
    }

    pub(crate) fn write(&self, vec: &mut Vec<u8>, narrow: bool) {
        match self {
            TValue::Nil => vec.push(0),
            TValue::False => vec.push(1),
            TValue::True => vec.push(2),
            TValue::Number(n) => {
                let int_n = *n as i32;
                if int_n as f64 == *n && narrow {
                    vec.push(3);
                    uleb128::write_u32(vec, int_n as u32);
                } else {
                    vec.push(4);
                    let bits = n.to_bits();
                    uleb128::write_u32(vec, bits as u32);
                    uleb128::write_u32(vec, (bits >> 32) as u32);
                }
            }
            TValue::String(s) => {
                uleb128::write_usize(vec, 5 + s.len());
                // i'm not happy about this either
                vec.extend(s.as_bytes());
            }
        }
    }
}

#[derive(Debug)]
pub struct DebugInfo {
    pub first_line: usize,
    pub num_lines: usize,
    pub info: Vec<u8>,
}

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

        uleb128::write_u32(vec, 1 + ((lo << 1) | (lo & 0x80000000)));
        if lo >= 0x80000000 {
            let last = vec.last_mut().unwrap();
            *last = (*last & 7) | ((lo >> 27) & 0x18) as u8;
        }
        uleb128::write_u32(vec, hi);
    }
}
