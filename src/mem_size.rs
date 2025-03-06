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

mod lexer {
    use super::DeepSize;
    use crate::utils::Span;

    impl DeepSize for Span<'_> {
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

    impl DeepSize for Stmt<'_> {
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
                Stmt::Break => 0,
            }
        }
    }

    impl DeepSize for Declare<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.lhs.deep_size_of_children()
        }
    }

    impl DeepSize for SuffixedExpr<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.suffixes.deep_size_of_children()
        }
    }

    impl DeepSize for SuffixedName<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.suffixes.deep_size_of_children()
        }
    }

    impl DeepSize for Suffix<'_> {
        fn deep_size_of_children(&self) -> usize {
            match self {
                Suffix::Call(Call { args }) | Suffix::Method(Method { args, .. }) => {
                    args.deep_size_of_children()
                }
                _ => 0,
            }
        }
    }

    impl DeepSize for MultiDecl<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.lhs_arr.deep_size_of() + self.rhs_arr.deep_size_of_children()
        }
    }

    impl DeepSize for MethodDecl<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.func.deep_size_of_children()
        }
    }

    impl DeepSize for FuncNode<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.body.deep_size_of_children() + self.ty.deep_size_of_children()
        }
    }

    impl DeepSize for Member<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.ty.deep_size_of_children()
        }
    }

    impl DeepSize for Assign<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.lhs.deep_size_of_children()
        }
    }

    impl DeepSize for MultiAssign<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.lhs_arr.deep_size_of_children() + self.rhs_arr.deep_size_of_children()
        }
    }

    impl DeepSize for Block<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.stmts.deep_size_of_children()
        }
    }

    impl DeepSize for IfStmt<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.body.deep_size_of_children() + self.else_.deep_size_of_children()
        }
    }

    impl DeepSize for ElseBranch<'_> {
        fn deep_size_of_children(&self) -> usize {
            match self {
                ElseBranch::Else(vec) => vec.deep_size_of_children(),
                ElseBranch::ElseIf(if_stmt) => if_stmt.deep_size_of_children(),
            }
        }
    }

    impl DeepSize for Expr<'_> {
        fn deep_size_of_children(&self) -> usize {
            match self {
                Expr::Simple(simple_expr) => simple_expr.deep_size_of_children(),
                _ => 0,
            }
        }
    }

    impl DeepSize for SimpleExpr<'_> {
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

    impl DeepSize for FieldNode<'_> {
        fn deep_size_of_children(&self) -> usize {
            0
        }
    }

    impl DeepSize for RangeExpr {
        fn deep_size_of_children(&self) -> usize {
            0
        }
    }

    impl DeepSize for TypeNode<'_> {
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
            }
        }
    }

    impl DeepSize for FunctionType<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.params.deep_size_of_children()
        }
    }

    impl DeepSize for Param<'_> {
        fn deep_size_of_children(&self) -> usize {
            self.ty.deep_size_of_children()
        }
    }
}

mod typecheck {
    use super::DeepSize;
    use crate::typecheck::pool::TypeRef;

    impl DeepSize for TypeRef<'_> {
        fn deep_size_of_children(&self) -> usize {
            0
        }
    }

    mod types {
        use super::DeepSize;
        use crate::typecheck::types::{Function, Struct, Type, TypeKind};

        impl DeepSize for Type<'_> {
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

        impl DeepSize for Function<'_> {
            fn deep_size_of_children(&self) -> usize {
                self.params.deep_size_of_children() + self.returns.deep_size_of_children()
            }
        }

        impl DeepSize for Struct<'_> {
            fn deep_size_of_children(&self) -> usize {
                self.fields.deep_size_of_children()
            }
        }
    }

    mod type_env {
        use super::DeepSize;
        use crate::typecheck::type_env::{Resolved, TypeEnv};

        impl DeepSize for TypeEnv<'_> {
            fn deep_size_of_children(&self) -> usize {
                self.scopes().deep_size_of_children()
            }
        }

        impl DeepSize for Resolved<'_> {
            fn deep_size_of_children(&self) -> usize {
                0
            }
        }
    }
}
