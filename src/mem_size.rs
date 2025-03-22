use rustc_hash::FxHashMap;

pub(crate) trait DeepSize {
    fn deep_size_of(&self) -> usize {
        size_of_val(self) + self.deep_size_of_children()
    }

    fn deep_size_of_children(&self) -> usize;
}

impl<T: DeepSize> DeepSize for [T] {
    fn deep_size_of_children(&self) -> usize {
        self.iter().map(DeepSize::deep_size_of).sum()
    }
}

impl<T: DeepSize> DeepSize for Vec<T> {
    fn deep_size_of_children(&self) -> usize {
        self.capacity() * size_of::<T>()
            + self
                .iter()
                .map(DeepSize::deep_size_of_children)
                .sum::<usize>()
    }
}

impl<T: DeepSize> DeepSize for Box<T> {
    fn deep_size_of_children(&self) -> usize {
        self.as_ref().deep_size_of()
    }
}

impl<T: DeepSize> DeepSize for Option<T> {
    fn deep_size_of_children(&self) -> usize {
        match &self {
            Some(t) => t.deep_size_of_children(),
            None => 0,
        }
    }
}

impl DeepSize for String {
    fn deep_size_of_children(&self) -> usize {
        self.capacity()
    }
}

impl<A: DeepSize> DeepSize for (A,) {
    fn deep_size_of_children(&self) -> usize {
        self.0.deep_size_of_children()
    }
}
impl<A: DeepSize, B: DeepSize> DeepSize for (A, B) {
    fn deep_size_of_children(&self) -> usize {
        self.0.deep_size_of_children() + self.1.deep_size_of_children()
    }
}

impl<K: DeepSize, V: DeepSize> DeepSize for FxHashMap<K, V> {
    fn deep_size_of_children(&self) -> usize {
        self.iter()
            .map(|(k, v)| k.deep_size_of() + v.deep_size_of())
            .sum()
    }
}

impl DeepSize for &str {
    fn deep_size_of_children(&self) -> usize {
        0
    }
}

mod utils {
    use super::DeepSize;
    use crate::utils::{Ident, Span, Symbol};

    impl DeepSize for Span {
        fn deep_size_of_children(&self) -> usize {
            0
        }
    }

    impl DeepSize for Symbol {
        fn deep_size_of_children(&self) -> usize {
            0
        }
    }

    impl DeepSize for Ident {
        fn deep_size_of_children(&self) -> usize {
            0
        }
    }
}

mod parser {
    use super::DeepSize;
    use crate::parser::{ast::*, pool::ExprRef};

    impl DeepSize for ExprRef {
        fn deep_size_of_children(&self) -> usize {
            0
        }
    }

    impl DeepSize for Stmt {
        fn deep_size_of_children(&self) -> usize {
            match self {
                Stmt::Declare(declare) => declare.deep_size_of_children(),
                Stmt::MultiDecl(multi_decl) => multi_decl.deep_size_of_children(),
                Stmt::MethodDecl(method_decl) => method_decl.deep_size_of_children(),
                Stmt::Assign(assign) => assign.deep_size_of_children(),
                Stmt::MultiAssign(multi_assign) => multi_assign.deep_size_of_children(),
                Stmt::ExprStmt(suffixed_expr) => suffixed_expr.deep_size_of_children(),
                Stmt::Block(block) => block.deep_size_of_children(),
                Stmt::Return(ReturnStmt { vals, .. }) => vals.deep_size_of_children(),
                Stmt::IfStmt(if_stmt) => if_stmt.deep_size_of_children(),
                Stmt::WhileStmt(while_stmt) => while_stmt.body.deep_size_of_children(),
                Stmt::RangeFor(range_for) => {
                    range_for.body.deep_size_of_children() + range_for.range.deep_size_of()
                }
                Stmt::KeyValFor(key_val_for) => key_val_for.body.deep_size_of_children(),
                Stmt::StructDecl(struct_decl) => {
                    struct_decl.constructor.deep_size_of_children()
                        + struct_decl.members.deep_size_of_children()
                }
                Stmt::Break(_) => 0,
            }
        }
    }

    impl DeepSize for Declare {
        fn deep_size_of_children(&self) -> usize {
            self.lhs.deep_size_of_children()
        }
    }

    impl DeepSize for SuffixedExpr {
        fn deep_size_of_children(&self) -> usize {
            self.suffixes.deep_size_of_children()
        }
    }

    impl DeepSize for SuffixedName {
        fn deep_size_of_children(&self) -> usize {
            self.suffixes.deep_size_of_children()
        }
    }

    impl DeepSize for Suffix {
        fn deep_size_of_children(&self) -> usize {
            match self {
                Suffix::Call(Call { args, .. }) => args.deep_size_of_children(),
                Suffix::Method(Method { args, .. }) => args.deep_size_of_children(),
                _ => 0,
            }
        }
    }

    impl DeepSize for MultiDecl {
        fn deep_size_of_children(&self) -> usize {
            self.lhs_arr.deep_size_of() + self.rhs_arr.deep_size_of_children()
        }
    }

    impl DeepSize for MethodDecl {
        fn deep_size_of_children(&self) -> usize {
            self.func.deep_size_of_children()
        }
    }

    impl DeepSize for FuncNode {
        fn deep_size_of_children(&self) -> usize {
            self.body.deep_size_of_children() + self.ty.deep_size_of_children()
        }
    }

    impl DeepSize for Member {
        fn deep_size_of_children(&self) -> usize {
            self.ty.deep_size_of_children()
        }
    }

    impl DeepSize for Assign {
        fn deep_size_of_children(&self) -> usize {
            self.lhs.deep_size_of_children()
        }
    }

    impl DeepSize for MultiAssign {
        fn deep_size_of_children(&self) -> usize {
            self.lhs_arr.deep_size_of_children() + self.rhs_arr.deep_size_of_children()
        }
    }

    impl DeepSize for Block {
        fn deep_size_of_children(&self) -> usize {
            match self {
                Block::Some(stmts) => stmts.deep_size_of_children(),
                Block::None(_) => 0,
            }
        }
    }

    impl DeepSize for IfStmt {
        fn deep_size_of_children(&self) -> usize {
            self.body.deep_size_of_children() + self.else_.deep_size_of_children()
        }
    }

    impl DeepSize for ElseBranch {
        fn deep_size_of_children(&self) -> usize {
            match self {
                ElseBranch::Else(vec) => vec.deep_size_of_children(),
                ElseBranch::ElseIf(if_stmt) => if_stmt.deep_size_of_children(),
            }
        }
    }

    impl DeepSize for Expr {
        fn deep_size_of_children(&self) -> usize {
            match self {
                Expr::Simple(simple_expr) => simple_expr.deep_size_of_children(),
                _ => 0,
            }
        }
    }

    impl DeepSize for SimpleExpr {
        fn deep_size_of_children(&self) -> usize {
            match self {
                SimpleExpr::Num(_)
                | SimpleExpr::Str(_)
                | SimpleExpr::Bool(_)
                | SimpleExpr::Nil(_) => 0,
                SimpleExpr::FuncNode(func_node) => func_node.deep_size_of_children(),
                SimpleExpr::TableNode(table_node) => table_node.fields.deep_size_of_children(),
                SimpleExpr::SuffixedExpr(suffixed_expr) => suffixed_expr.deep_size_of_children(),
            }
        }
    }

    impl DeepSize for FieldNode {
        fn deep_size_of_children(&self) -> usize {
            0
        }
    }

    impl DeepSize for RangeExpr {
        fn deep_size_of_children(&self) -> usize {
            0
        }
    }

    impl DeepSize for TypeNode {
        fn deep_size_of_children(&self) -> usize {
            match self {
                TypeNode::Name(_) | TypeNode::Nil(_) | TypeNode::VariadicType(_) => 0,
                TypeNode::FunctionType(function_type) => function_type.deep_size_of_children(),
                TypeNode::TableType(table_type) => {
                    table_type.key_type.deep_size_of_children()
                        + table_type.val_type.deep_size_of_children()
                }
                TypeNode::OptionalType(optional_type) => {
                    optional_type.inner.deep_size_of_children()
                }
                TypeNode::ParenType(paren_type) => paren_type.inner.deep_size_of_children(),
            }
        }
    }

    impl DeepSize for FunctionType {
        fn deep_size_of_children(&self) -> usize {
            self.params.deep_size_of_children()
        }
    }

    impl DeepSize for Param {
        fn deep_size_of_children(&self) -> usize {
            self.ty.deep_size_of_children()
        }
    }
}

mod typecheck {
    use super::DeepSize;
    use crate::typecheck::pool::TypeRef;

    impl DeepSize for TypeRef {
        fn deep_size_of_children(&self) -> usize {
            0
        }
    }

    mod types {
        use super::DeepSize;
        use crate::typecheck::types::{Function, Struct, Type, TypeKind};

        impl DeepSize for Type {
            fn deep_size_of_children(&self) -> usize {
                match &self.kind {
                    TypeKind::Function(function) => function.deep_size_of_children(),
                    TypeKind::Struct(strukt) => strukt.deep_size_of_children(),
                    TypeKind::Multiple(vec) => vec.deep_size_of_children(),
                    TypeKind::Optional(opt) => opt.deep_size_of_children(),
                    _ => 0,
                }
            }
        }

        impl DeepSize for Function {
            fn deep_size_of_children(&self) -> usize {
                self.params.deep_size_of_children() + self.returns.deep_size_of_children()
            }
        }

        impl DeepSize for Struct {
            fn deep_size_of_children(&self) -> usize {
                self.fields.deep_size_of_children()
            }
        }
    }

    mod type_env {
        use super::DeepSize;
        use crate::typecheck::type_env::{Resolved, TypeEnv};

        impl DeepSize for TypeEnv {
            fn deep_size_of_children(&self) -> usize {
                self.scopes().deep_size_of_children()
            }
        }

        impl DeepSize for Resolved {
            fn deep_size_of_children(&self) -> usize {
                0
            }
        }
    }
}
