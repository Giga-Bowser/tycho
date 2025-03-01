use crate::{lexer::Span, parser::ExprRef, utils::Spanned};

#[derive(Debug, Clone)]
pub enum Stmt<'s> {
    Declare(Declare<'s>),
    MultiDecl(MultiDecl<'s>),
    MethodDecl(MethodDecl<'s>),
    Assign(Assign<'s>),
    MultiAssign(MultiAssign<'s>),
    ExprStmt(SuffixedExpr<'s>),
    Block(Block<'s>),
    Return(ReturnStmt<'s>),
    Break,
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
    pub suffixes: Vec<Suffix<'s>>,
}

#[derive(Debug, Clone)]
pub struct Block<'s> {
    pub stmts: Box<[Stmt<'s>]>,
}

impl<'s> std::ops::Deref for Block<'s> {
    type Target = [Stmt<'s>];

    #[inline]
    fn deref(&self) -> &[Stmt<'s>] {
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
    type Item = &'a Stmt<'s>;
    type IntoIter = std::slice::Iter<'a, Stmt<'s>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Debug, Clone)]
pub struct IfStmt<'s> {
    pub condition: ExprRef,
    pub body: Block<'s>,
    pub else_: Option<Box<ElseBranch<'s>>>,
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
    pub members: Vec<Member<'s>>,
    pub constructor: Option<Box<FuncNode<'s>>>,
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
    pub ty: Box<FunctionType<'s>>,
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
    pub header_span: Span<'s>, // from `func` to end of params
    pub return_type: Option<Box<ReturnType<'s>>>,
}

impl<'s> FunctionType<'s> {
    pub fn span(&self) -> Span<'s> {
        self.return_type
            .as_ref()
            .map_or(self.header_span, |it| it.span())
    }
}

#[derive(Debug, Clone)]
pub enum ReturnType<'s> {
    Single(TypeNode<'s>),
    Multiple(MultipleType<'s>),
}

impl<'s> ReturnType<'s> {
    pub fn span(&self) -> Span<'s> {
        match self {
            ReturnType::Single(ty) => ty.span(),
            ReturnType::Multiple(multiple_type) => multiple_type.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Param<'s> {
    pub name: Span<'s>,
    pub ty: TypeNode<'s>,
}

#[derive(Debug, Clone)]
pub struct TableType<'s> {
    pub key_type: Option<Box<TypeNode<'s>>>,
    pub key_span: Span<'s>, // covers both brackets
    pub val_type: Box<TypeNode<'s>>,
}

impl<'s> Spanned<'s> for TableType<'s> {
    fn span(&self) -> Span<'s> {
        Span::cover(self.key_span, self.val_type.span())
    }
}

#[derive(Debug, Clone)]
pub struct MultipleType<'s> {
    pub types: Vec<TypeNode<'s>>,
    pub span: Span<'s>,
}

#[derive(Debug, Clone)]
pub struct OptionalType<'s> {
    pub inner: Box<TypeNode<'s>>,
    pub question: Span<'s>,
}

impl<'s> Spanned<'s> for OptionalType<'s> {
    fn span(&self) -> Span<'s> {
        Span::cover(self.inner.span(), self.question)
    }
}

impl<'s> Spanned<'s> for TypeNode<'s> {
    fn span(&self) -> Span<'s> {
        match self {
            TypeNode::Name(span) | TypeNode::Nil(span) | TypeNode::VariadicType(span) => *span,
            TypeNode::FunctionType(function_type) => function_type.span(),
            TypeNode::TableType(table_type) => {
                Span::cover(table_type.key_span, table_type.val_type.span())
            }
            TypeNode::OptionalType(optional_type) => {
                Span::cover(optional_type.inner.span(), optional_type.question)
            }
        }
    }
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
