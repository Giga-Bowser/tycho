use crate::{
    parser::ExprRef,
    types::{Function, Type, User},
};

#[derive(Debug, Clone)]
pub enum Statement<'s> {
    Declare(Declare<'s>),
    MultiDecl(MultiDecl<'s>),
    MethodDecl(MethodDecl<'s>),
    Assign(Assign<'s>),
    MultiAssign(MultiAssign<'s>),
    ExprStat(SuffixedExpr<'s>),
    Block(Block<'s>),
    Return(Vec<ExprRef>),
    Break,
    IfStat(IfStat<'s>),
    WhileStat(WhileStat<'s>),
    RangeFor(RangeFor<'s>),
    KeyValFor(KeyValFor<'s>),
    StructDecl(StructDecl<'s>),
}

#[derive(Debug, Clone)]
pub struct Declare<'s> {
    pub lhs: Box<SuffixedName<'s>>,
    pub type_: Box<Type<'s>>,
    pub val: Option<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct MultiDecl<'s> {
    pub lhs_arr: Vec<&'s str>,
    pub rhs_arr: Vec<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct MethodDecl<'s> {
    pub struct_name: &'s str,
    pub method_name: &'s str,
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
    pub name: &'s str,
    pub suffixes: Vec<Suffix<'s>>,
}

#[derive(Debug, Clone)]
pub struct Block<'s> {
    pub stmts: Vec<Statement<'s>>,
}

impl<'s> std::ops::Deref for Block<'s> {
    type Target = [Statement<'s>];

    #[inline]
    fn deref(&self) -> &[Statement<'s>] {
        self.stmts.as_slice()
    }
}

impl<'a, 's> IntoIterator for &'a Block<'s> {
    type Item = &'a Statement<'s>;
    type IntoIter = std::slice::Iter<'a, Statement<'s>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Debug, Clone)]
pub struct IfStat<'s> {
    pub condition: ExprRef,
    pub body: Block<'s>,
    pub else_: Option<Box<ElseBranch<'s>>>,
}

#[derive(Debug, Clone)]
pub enum ElseBranch<'s> {
    Else(Block<'s>),
    ElseIf(IfStat<'s>),
}

#[derive(Debug, Clone)]
pub struct WhileStat<'s> {
    pub condition: ExprRef,
    pub body: Block<'s>,
}

#[derive(Debug, Clone)]
pub struct RangeFor<'s> {
    pub var: &'s str,
    pub range: Box<RangeExpr>,
    pub body: Block<'s>,
}

#[derive(Debug, Clone)]
pub struct RangeExpr {
    pub lhs: ExprRef,
    pub rhs: ExprRef,
}

#[derive(Debug, Clone)]
pub struct KeyValFor<'s> {
    /// this should be 'keyname, valname' in one str
    pub names: &'s str,
    pub iter: ExprRef,
    pub body: Block<'s>,
}

#[derive(Debug, Clone)]
pub struct StructDecl<'s> {
    pub name: &'s str,
    pub type_: Box<User<'s>>,
    pub constructor: Option<FuncNode<'s>>,
}

#[derive(Debug, Clone)]
pub enum Expr<'s> {
    BinOp(BinOp),
    UnOp(UnOp),
    Paren(ParenExpr),
    Simple(SimpleExpr<'s>),
    Name(&'s str),
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
pub enum SimpleExpr<'s> {
    Num(&'s str),
    Str(&'s str),
    Bool(&'s str),
    Nil(&'s str),
    FuncNode(FuncNode<'s>),
    TableNode(TableNode<'s>),
    SuffixedExpr(SuffixedExpr<'s>),
}

#[derive(Debug, Clone)]
pub struct FuncNode<'s> {
    pub type_: Box<Function<'s>>,
    pub body: Block<'s>,
}

#[derive(Debug, Clone)]
pub struct TableNode<'s> {
    pub fields: Vec<FieldNode<'s>>,
}

#[derive(Debug, Clone)]
pub enum FieldNode<'s> {
    Field { key: &'s str, val: ExprRef },
    ExprField { key: ExprRef, val: ExprRef },
    ValField { val: ExprRef },
}

#[derive(Debug, Clone)]
pub struct SuffixedExpr<'s> {
    pub val: ExprRef,
    pub suffixes: Vec<Suffix<'s>>,
}

#[derive(Debug, Clone)]
pub enum Suffix<'s> {
    Index(Index),
    Access(Access<'s>),
    Call(Call),
    Method(Method<'s>),
}

#[derive(Debug, Clone)]
pub struct Index {
    pub key: ExprRef,
}

#[derive(Debug, Clone)]
pub struct Access<'s> {
    pub field_name: &'s str,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub args: Vec<ExprRef>,
}

#[derive(Debug, Clone)]
pub struct Method<'s> {
    pub method_name: &'s str,
    pub args: Vec<ExprRef>,
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

impl From<&UnOpKind> for &'static str {
    fn from(val: &UnOpKind) -> Self {
        match val {
            UnOpKind::Neg => "-",
            UnOpKind::Len => "#",
            UnOpKind::Not => "not",
        }
    }
}
