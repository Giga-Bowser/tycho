use crate::{
    lexer::Span,
    parser::ExprRef,
    types::{pool::TypeRef, Function, User},
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
    Return(ReturnStmt<'s>),
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
    pub ty: TypeRef<'s>,
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
    pub suffixes: Vec<Suffix<'s>>,
}

#[derive(Debug, Clone)]
pub struct Block<'s> {
    pub stmts: Box<[Statement<'s>]>,
}

impl<'s> std::ops::Deref for Block<'s> {
    type Target = [Statement<'s>];

    #[inline]
    fn deref(&self) -> &[Statement<'s>] {
        self.stmts.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStmt<'s> {
    pub vals: Vec<ExprRef>,

    // for diagnostics,
    pub kw_span: Span<'s>,
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
    pub var: Span<'s>,
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
    pub names: Span<'s>,
    pub iter: ExprRef,
    pub body: Block<'s>,
}

#[derive(Debug, Clone)]
pub struct StructDecl<'s> {
    pub name: Span<'s>,
    pub ty: Box<User<'s>>,
    pub constructor: Option<FuncNode<'s>>,
}

#[derive(Debug, Clone)]
pub enum Expr<'s> {
    BinOp(BinOp),
    UnOp(UnOp<'s>),
    Paren(ParenExpr),
    Simple(SimpleExpr<'s>),
    Name(Span<'s>),
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub op: OpKind,
    pub lhs: ExprRef,
    pub rhs: ExprRef,
}

#[derive(Debug, Clone)]
pub struct UnOp<'s> {
    pub op: UnOpKind,
    // for diagnostics
    pub op_span: Span<'s>,
    pub val: ExprRef,
}

#[derive(Debug, Clone)]
pub struct ParenExpr {
    pub val: ExprRef,
}

#[derive(Debug, Clone)]
pub enum SimpleExpr<'s> {
    Num(Span<'s>),
    Str(Span<'s>),
    Bool(Span<'s>),
    Nil(Span<'s>),
    FuncNode(FuncNode<'s>),
    TableNode(TableNode<'s>),
    SuffixedExpr(SuffixedExpr<'s>),
}

#[derive(Debug, Clone)]
pub struct FuncNode<'s> {
    pub ty: Box<Function<'s>>,
    pub body: Block<'s>,
}

#[derive(Debug, Clone)]
pub struct TableNode<'s> {
    pub fields: Vec<FieldNode<'s>>,
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
    pub suffixes: Vec<Suffix<'s>>,
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
