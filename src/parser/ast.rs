use std::ops::Deref;

use crate::{
    parser::ExprRef,
    utils::{Span, SrcLoc},
};

#[derive(Debug, Clone)]
pub enum Stmt {
    Declare(Declare),
    MultiDecl(MultiDecl),
    MethodDecl(MethodDecl),
    Assign(Assign),
    MultiAssign(MultiAssign),
    ExprStmt(SuffixedExpr),
    Block(SpannedBlock),
    Return(ReturnStmt),
    Break(Span),
    IfStmt(IfStmt),
    WhileStmt(WhileStmt),
    RangeFor(RangeFor),
    KeyValFor(KeyValFor),
    StructDecl(StructDecl),
}

#[derive(Debug, Clone)]
pub struct Declare {
    pub lhs: Box<SuffixedName>,
    pub ty: Option<Box<TypeNode>>,
    pub val: Option<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct MultiDecl {
    pub lhs_arr: Vec<Span>,
    pub rhs_arr: Vec<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct MethodDecl {
    pub struct_name: Span,
    pub method_name: Span,
    pub func: Box<FuncNode>,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub lhs: Box<SuffixedName>,
    pub rhs: ExprRef,
}

#[derive(Debug, Clone)]
pub struct MultiAssign {
    pub lhs_arr: Vec<SuffixedExpr>,
    pub rhs_arr: Vec<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct SuffixedName {
    pub name: Span,
    pub suffixes: Box<[Suffix]>,
}

#[derive(Debug, Clone)]
pub enum Block {
    Some(Box<[Stmt]>),
    None(Span),
}

impl Deref for Block {
    type Target = [Stmt];

    #[inline]
    fn deref(&self) -> &[Stmt] {
        match self {
            Block::Some(stmts) => stmts.as_ref(),
            Block::None(_) => &[],
        }
    }
}

impl<'a> IntoIterator for &'a Block {
    type Item = &'a Stmt;
    type IntoIter = std::slice::Iter<'a, Stmt>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// A block with a span requiring no calculation, use when space is available.
#[derive(Debug, Clone)]
pub struct SpannedBlock {
    pub stmts: Box<[Stmt]>,
    pub span: Span,
}

impl Deref for SpannedBlock {
    type Target = [Stmt];

    #[inline]
    fn deref(&self) -> &[Stmt] {
        &self.stmts
    }
}

impl<'a> IntoIterator for &'a SpannedBlock {
    type Item = &'a Stmt;
    type IntoIter = std::slice::Iter<'a, Stmt>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub vals: Vec<ExprRef>,

    // for diagnostics,
    pub kw_span: Span,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: ExprRef,
    pub body: SpannedBlock,
    pub else_: Option<Box<ElseBranch>>,

    // for diagnostics
    pub kw_span: Span,
}

#[derive(Debug, Clone)]
pub enum ElseBranch {
    Else(SpannedBlock),
    ElseIf(IfStmt),
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: ExprRef,
    pub body: Block,

    // for diagnostics
    pub kw_span: Span,
}

#[derive(Debug, Clone)]
pub struct RangeFor {
    pub var: Span,
    pub range: Box<RangeExpr>,
    pub body: Block,

    // for diagnostics
    pub kw_span: Span,
}

#[derive(Debug, Clone)]
pub struct RangeExpr {
    pub lhs: ExprRef,
    pub rhs: ExprRef,

    // for diagnostics
    pub op_span: Span,
}

#[derive(Debug, Clone)]
pub struct KeyValFor {
    pub key_name: Span,
    pub val_name: Span,
    pub iter: ExprRef,
    pub body: Block,

    // for diagnostics
    pub kw_span: Span,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: Span,
    pub members: Vec<Member>,
    pub constructor: Option<Box<FuncNode>>,

    // for diagnostics
    pub end_loc: SrcLoc,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub name: Span,
    pub ty: TypeNode,
}

#[derive(Debug, Clone)]
pub enum Expr {
    BinOp(BinOp),
    UnOp(UnOp),
    Paren(ParenExpr),
    Simple(SimpleExpr),
    Name(Span),
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub op: OpKind,
    pub lhs: ExprRef,
    pub rhs: ExprRef,

    // for diagnostics
    pub op_start: SrcLoc,
}

impl BinOp {
    pub const fn op_span(&self) -> Span {
        let len = match self.op {
            OpKind::Add
            | OpKind::Sub
            | OpKind::Mul
            | OpKind::Div
            | OpKind::Mod
            | OpKind::Pow
            | OpKind::Les
            | OpKind::Gre => 1,
            OpKind::Cat
            | OpKind::Neq
            | OpKind::Equ
            | OpKind::Grq
            | OpKind::Leq
            | OpKind::And
            | OpKind::Or => 2,
        };

        Span::offset_len(self.op_start, len)
    }
}

#[derive(Debug, Clone)]
pub struct UnOp {
    pub op: UnOpKind,
    pub val: ExprRef,

    // for diagnostics
    pub op_span: Span,
}

#[derive(Debug, Clone)]
pub struct ParenExpr {
    pub val: ExprRef,

    // for diagnostics
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum SimpleExpr {
    Num(Span),
    Str(Span),
    Bool(Span),
    Nil(Span),
    FuncNode(Box<FuncNode>),
    TableNode(TableNode),
    SuffixedExpr(SuffixedExpr),
}

#[derive(Debug, Clone)]
pub struct FuncNode {
    pub ty: FunctionType,
    pub body: SpannedBlock,
}

#[derive(Debug, Clone)]
pub struct TableNode {
    pub fields: Box<[FieldNode]>,

    // for diagnostics
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum FieldNode {
    Field { key: Span, val: ExprRef },
    ExprField { key: ExprRef, val: ExprRef },
    ValField { val: ExprRef },
}

#[derive(Debug, Clone)]
pub struct SuffixedExpr {
    pub val: ExprRef,
    pub suffixes: Box<[Suffix]>,
}

#[derive(Debug, Clone)]
pub enum Suffix {
    Index(Index),
    Access(Access),
    Call(Call),
    Method(Method),
}

#[derive(Debug, Clone)]
pub struct Index {
    pub key: ExprRef,

    // for diagnostics
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Access {
    pub field_name: Span,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub args: Box<[ExprRef]>,

    // for diagnostics
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub method_name: Span,
    pub args: Vec<ExprRef>,
}

#[derive(Debug, Clone)]
pub enum TypeNode {
    Name(Span),
    Nil(Span),
    FunctionType(FunctionType),
    TableType(TableType),
    OptionalType(OptionalType),
    VariadicType(Span),
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub params: Vec<Param>,
    pub return_type: Option<Box<ReturnType>>,

    // for diagnostics
    pub header_span: Span, // from `func` to end of params
}

#[derive(Debug, Clone)]
pub enum ReturnType {
    Single(TypeNode),
    Multiple(MultipleType),
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Span,
    pub ty: TypeNode,
}

#[derive(Debug, Clone)]
pub struct TableType {
    pub key_type: Option<Box<TypeNode>>,
    pub val_type: Box<TypeNode>,

    // for diagnostics
    pub key_span: Span, // covers both brackets
}

#[derive(Debug, Clone)]
pub struct MultipleType {
    pub types: Vec<TypeNode>,

    // for diagnostics
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct OptionalType {
    pub inner: Box<TypeNode>,

    // for diagnostics
    pub question: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Cat,
    Neq,
    Equ,
    Les,
    Grq,
    Leq,
    Gre,
    And,
    Or,
}

impl OpKind {
    #[inline]
    pub const fn to_lua(&self) -> &'static str {
        match self {
            OpKind::Add => "+",
            OpKind::Sub => "-",
            OpKind::Mul => "*",
            OpKind::Div => "/",
            OpKind::Mod => "%",
            OpKind::Pow => "^",
            OpKind::Cat => "..",
            OpKind::Equ => "==",
            OpKind::Neq => "~=",
            OpKind::Gre => ">",
            OpKind::Grq => ">=",
            OpKind::Les => "<",
            OpKind::Leq => "<=",
            OpKind::And => "and",
            OpKind::Or => "or",
        }
    }
}

impl std::fmt::Display for OpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            OpKind::Add => "+",
            OpKind::Sub => "-",
            OpKind::Mul => "*",
            OpKind::Div => "/",
            OpKind::Mod => "%",
            OpKind::Pow => "^",
            OpKind::Cat => "..",
            OpKind::Neq => "!=",
            OpKind::Equ => "==",
            OpKind::Les => "<",
            OpKind::Grq => ">=",
            OpKind::Leq => "<=",
            OpKind::Gre => ">",
            OpKind::And => "&&",
            OpKind::Or => "||",
        };

        f.write_str(s)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnOpKind {
    Neg,
    Len,
    Not,
}

impl From<UnOpKind> for &'static str {
    fn from(val: UnOpKind) -> Self {
        match val {
            UnOpKind::Neg => "-",
            UnOpKind::Len => "#",
            UnOpKind::Not => "not",
        }
    }
}
