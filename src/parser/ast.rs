use std::ops::Deref;

use crate::{
    parser::ExprRef,
    utils::{Span, SrcLoc},
};

#[derive(Debug, Clone)]
pub enum Stmt<'s> {
    Declare(Declare<'s>),
    MultiDecl(MultiDecl<'s>),
    MethodDecl(MethodDecl<'s>),
    Assign(Assign<'s>),
    MultiAssign(MultiAssign<'s>),
    ExprStmt(SuffixedExpr<'s>),
    Block(SpannedBlock<'s>),
    Return(ReturnStmt<'s>),
    Break(Span<'s>),
    IfStmt(IfStmt<'s>),
    WhileStmt(WhileStmt<'s>),
    RangeFor(RangeFor<'s>),
    KeyValFor(KeyValFor<'s>),
    StructDecl(StructDecl<'s>),
}

#[derive(Debug, Clone)]
pub struct Declare<'s> {
    pub lhs: Box<SuffixedName<'s>>,
    pub ty: Option<Box<TypeNode<'s>>>,
    pub val: Option<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct MultiDecl<'s> {
    pub lhs_arr: Vec<Span<'s>>,
    pub rhs_arr: Vec<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct MethodDecl<'s> {
    pub struct_name: Span<'s>,
    pub method_name: Span<'s>,
    pub func: Box<FuncNode<'s>>,
}

#[derive(Debug, Clone)]
pub struct Assign<'s> {
    pub lhs: Box<SuffixedName<'s>>,
    pub rhs: ExprRef,
}

#[derive(Debug, Clone)]
pub struct MultiAssign<'s> {
    pub lhs_arr: Vec<SuffixedExpr<'s>>,
    pub rhs_arr: Vec<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct SuffixedName<'s> {
    pub name: Span<'s>,
    pub suffixes: Box<[Suffix<'s>]>,
}

#[derive(Debug, Clone)]
pub enum Block<'s> {
    Some(Box<[Stmt<'s>]>),
    None(Span<'s>),
}

impl<'s> Deref for Block<'s> {
    type Target = [Stmt<'s>];

    #[inline]
    fn deref(&self) -> &[Stmt<'s>] {
        match self {
            Block::Some(stmts) => stmts.as_ref(),
            Block::None(_) => &[],
        }
    }
}

impl<'a, 's> IntoIterator for &'a Block<'s> {
    type Item = &'a Stmt<'s>;
    type IntoIter = std::slice::Iter<'a, Stmt<'s>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// A block with a span requiring no calculation, use when space is available.
#[derive(Debug, Clone)]
pub struct SpannedBlock<'s> {
    pub stmts: Box<[Stmt<'s>]>,
    pub span: Span<'s>,
}

impl<'s> Deref for SpannedBlock<'s> {
    type Target = [Stmt<'s>];

    #[inline]
    fn deref(&self) -> &[Stmt<'s>] {
        &self.stmts
    }
}

impl<'a, 's> IntoIterator for &'a SpannedBlock<'s> {
    type Item = &'a Stmt<'s>;
    type IntoIter = std::slice::Iter<'a, Stmt<'s>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStmt<'s> {
    pub vals: Vec<ExprRef>,

    // for diagnostics,
    pub kw_span: Span<'s>,
}

#[derive(Debug, Clone)]
pub struct IfStmt<'s> {
    pub condition: ExprRef,
    pub body: Block<'s>,
    pub else_: Option<Box<ElseBranch<'s>>>,

    // for diagnostics
    pub kw_span: Span<'s>,
}

#[derive(Debug, Clone)]
pub enum ElseBranch<'s> {
    Else(Block<'s>),
    ElseIf(IfStmt<'s>),
}

#[derive(Debug, Clone)]
pub struct WhileStmt<'s> {
    pub condition: ExprRef,
    pub body: Block<'s>,

    // for diagnostics
    pub kw_span: Span<'s>,
}

#[derive(Debug, Clone)]
pub struct RangeFor<'s> {
    pub var: Span<'s>,
    pub range: Box<RangeExpr>,
    pub body: Block<'s>,

    // for diagnostics
    pub kw_span: Span<'s>,
}

#[derive(Debug, Clone)]
pub struct RangeExpr {
    pub lhs: ExprRef,
    pub rhs: ExprRef,
}

#[derive(Debug, Clone)]
pub struct KeyValFor<'s> {
    pub key_name: Span<'s>,
    pub val_name: Span<'s>,
    pub iter: ExprRef,
    pub body: Block<'s>,

    // for diagnostics
    pub kw_span: Span<'s>,
}

#[derive(Debug, Clone)]
pub struct StructDecl<'s> {
    pub name: Span<'s>,
    pub members: Vec<Member<'s>>,
    pub constructor: Option<Box<FuncNode<'s>>>,

    // for diagnostics
    pub end_loc: SrcLoc,
}

#[derive(Debug, Clone)]
pub struct Member<'s> {
    pub name: Span<'s>,
    pub ty: TypeNode<'s>,
}

#[derive(Debug, Clone)]
pub enum Expr<'s> {
    BinOp(BinOp),
    UnOp(UnOp<'s>),
    Paren(ParenExpr<'s>),
    Simple(SimpleExpr<'s>),
    Name(Span<'s>),
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub op: OpKind,
    pub lhs: ExprRef,
    pub rhs: ExprRef,

    // for diagnostics
    pub op_start: SrcLoc,
}

#[derive(Debug, Clone)]
pub struct UnOp<'s> {
    pub op: UnOpKind,
    pub val: ExprRef,

    // for diagnostics
    pub op_span: Span<'s>,
}

#[derive(Debug, Clone)]
pub struct ParenExpr<'s> {
    pub val: ExprRef,

    // for diagnostics
    pub span: Span<'s>,
}

#[derive(Debug, Clone)]
pub enum SimpleExpr<'s> {
    Num(Span<'s>),
    Str(Span<'s>),
    Bool(Span<'s>),
    Nil(Span<'s>),
    FuncNode(Box<FuncNode<'s>>),
    TableNode(TableNode<'s>),
    SuffixedExpr(SuffixedExpr<'s>),
}

#[derive(Debug, Clone)]
pub struct FuncNode<'s> {
    pub ty: FunctionType<'s>,
    pub body: SpannedBlock<'s>,
}

#[derive(Debug, Clone)]
pub struct TableNode<'s> {
    pub fields: Box<[FieldNode<'s>]>,

    // for diagnostics
    pub span: Span<'s>,
}

#[derive(Debug, Clone)]
pub enum FieldNode<'s> {
    Field { key: Span<'s>, val: ExprRef },
    ExprField { key: ExprRef, val: ExprRef },
    ValField { val: ExprRef },
}

#[derive(Debug, Clone)]
pub struct SuffixedExpr<'s> {
    pub val: ExprRef,
    pub suffixes: Box<[Suffix<'s>]>,
}

#[derive(Debug, Clone)]
pub enum Suffix<'s> {
    Index(Index<'s>),
    Access(Access<'s>),
    Call(Call),
    Method(Method<'s>),
}

#[derive(Debug, Clone)]
pub struct Index<'s> {
    pub key: ExprRef,

    // for diagnostics
    pub span: Span<'s>,
}

#[derive(Debug, Clone)]
pub struct Access<'s> {
    pub field_name: Span<'s>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub args: Vec<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct Method<'s> {
    pub method_name: Span<'s>,
    pub args: Vec<ExprRef>,
}

#[derive(Debug, Clone)]
pub enum TypeNode<'s> {
    Name(Span<'s>),
    Nil(Span<'s>),
    FunctionType(FunctionType<'s>),
    TableType(TableType<'s>),
    OptionalType(OptionalType<'s>),
    VariadicType(Span<'s>),
}

#[derive(Debug, Clone)]
pub struct FunctionType<'s> {
    pub params: Vec<Param<'s>>,
    pub return_type: Option<Box<ReturnType<'s>>>,

    // for diagnostics
    pub header_span: Span<'s>, // from `func` to end of params
}

#[derive(Debug, Clone)]
pub enum ReturnType<'s> {
    Single(TypeNode<'s>),
    Multiple(MultipleType<'s>),
}

#[derive(Debug, Clone)]
pub struct Param<'s> {
    pub name: Span<'s>,
    pub ty: TypeNode<'s>,
}

#[derive(Debug, Clone)]
pub struct TableType<'s> {
    pub key_type: Option<Box<TypeNode<'s>>>,
    pub val_type: Box<TypeNode<'s>>,

    // for diagnostics
    pub key_span: Span<'s>, // covers both brackets
}

#[derive(Debug, Clone)]
pub struct MultipleType<'s> {
    pub types: Vec<TypeNode<'s>>,

    // for diagnostics
    pub span: Span<'s>,
}

#[derive(Debug, Clone)]
pub struct OptionalType<'s> {
    pub inner: Box<TypeNode<'s>>,

    // for diagnostics
    pub question: Span<'s>,
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
