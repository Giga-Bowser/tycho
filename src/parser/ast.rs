use crate::{
    parser::ExprRef,
    types::{Function, Type, User},
};

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Declare(Declare<'a>),
    MultiDecl(MultiDecl<'a>),
    MethodDecl(MethodDecl<'a>),
    Assign(Assign<'a>),
    MultiAssign(MultiAssign<'a>),
    ExprStat(SuffixedExpr<'a>),
    Block(Vec<Statement<'a>>),
    Return(Vec<ExprRef>),
    Break,
    IfStat(IfStat<'a>),
    WhileStat(WhileStat<'a>),
    RangeFor(RangeFor<'a>),
    KeyValFor(KeyValFor<'a>),
    StructDecl(StructDecl<'a>),
}

#[derive(Debug, Clone)]
pub struct Declare<'a> {
    pub lhs: Box<SuffixedName<'a>>,
    pub type_: Box<Type>,
    pub val: Option<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct MultiDecl<'a> {
    pub lhs_arr: Vec<&'a str>,
    pub rhs_arr: Vec<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct MethodDecl<'a> {
    pub struct_name: &'a str,
    pub method_name: &'a str,
    pub func: Box<FuncNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct Assign<'a> {
    pub lhs: Box<SuffixedName<'a>>,
    pub rhs: ExprRef,
}

#[derive(Debug, Clone)]
pub struct MultiAssign<'a> {
    pub lhs_arr: Vec<SuffixedExpr<'a>>,
    pub rhs_arr: Vec<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct SuffixedName<'a> {
    pub name: &'a str,
    pub suffixes: Vec<Suffix<'a>>,
}

#[derive(Debug, Clone)]
pub struct IfStat<'a> {
    pub condition: ExprRef,
    pub body: Vec<Statement<'a>>,
    pub else_: Option<Box<ElseBranch<'a>>>,
}

#[derive(Debug, Clone)]
pub enum ElseBranch<'a> {
    Else(Vec<Statement<'a>>),
    ElseIf(IfStat<'a>),
}

#[derive(Debug, Clone)]
pub struct WhileStat<'a> {
    pub condition: ExprRef,
    pub body: Vec<Statement<'a>>,
}

#[derive(Debug, Clone)]
pub struct RangeFor<'a> {
    pub var: &'a str,
    pub range: Box<RangeExpr>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Debug, Clone)]
pub struct RangeExpr {
    pub lhs: ExprRef,
    pub rhs: ExprRef,
}

#[derive(Debug, Clone)]
pub struct KeyValFor<'a> {
    /// this should be 'keyname, valname' in one str
    pub names: &'a str,
    pub iter: ExprRef,
    pub body: Vec<Statement<'a>>,
}

#[derive(Debug, Clone)]
pub struct StructDecl<'a> {
    pub name: &'a str,
    pub type_: Box<User>,
    pub constructor: Option<FuncNode<'a>>,
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    BinOp(BinOp),
    UnOp(UnOp),
    Paren(ParenExpr),
    Simple(SimpleExpr<'a>),
    Name(&'a str),
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub op: OpKind,
    pub lhs: ExprRef,
    pub rhs: ExprRef,
}

#[derive(Debug, Clone)]
pub struct UnOp {
    pub op: UnOpKind,
    pub val: ExprRef,
}

#[derive(Debug, Clone)]
pub struct ParenExpr {
    pub val: ExprRef,
}

#[derive(Debug, Clone)]
pub enum SimpleExpr<'a> {
    Num(&'a str),
    Str(&'a str),
    Bool(&'a str),
    Nil(&'a str),
    FuncNode(FuncNode<'a>),
    TableNode(TableNode<'a>),
    SuffixedExpr(SuffixedExpr<'a>),
}

#[derive(Debug, Clone)]
pub struct FuncNode<'a> {
    pub type_: Box<Function>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Debug, Clone)]
pub struct TableNode<'a> {
    pub fields: Vec<FieldNode<'a>>,
}

#[derive(Debug, Clone)]
pub enum FieldNode<'a> {
    Field { key: &'a str, val: ExprRef },
    ExprField { key: ExprRef, val: ExprRef },
    ValField { val: ExprRef },
}

#[derive(Debug, Clone)]
pub struct SuffixedExpr<'a> {
    pub val: ExprRef,
    pub suffixes: Vec<Suffix<'a>>,
}

#[derive(Debug, Clone)]
pub enum Suffix<'a> {
    Index(Index),
    Access(Access<'a>),
    Call(Call),
    Method(Method<'a>),
}

#[derive(Debug, Clone)]
pub struct Index {
    pub key: ExprRef,
}

#[derive(Debug, Clone)]
pub struct Access<'a> {
    pub field_name: &'a str,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub args: Vec<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct Method<'a> {
    pub method_name: &'a str,
    pub args: Vec<ExprRef>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpKind {
    Add,
    Sub,
    Mul,
    Div,
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

#[derive(Debug, Clone, Copy)]
pub enum UnOpKind {
    Neg,
    Len,
}

impl From<&UnOpKind> for &'static str {
    fn from(val: &UnOpKind) -> Self {
        match val {
            UnOpKind::Neg => "-",
            UnOpKind::Len => "#",
        }
    }
}
