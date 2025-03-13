use std::ops::Deref;

use crate::utils::Span;

pub(crate) trait Spanned {
    fn span(&self) -> Span;
}

pub(crate) trait Covering: Iterator<Item = Span> + Sized {
    fn covering(self) -> Option<Span> {
        self.reduce(Span::cover)
    }
}

impl<T: Iterator<Item = Span>> Covering for T {}

impl Spanned for Span {
    fn span(&self) -> Span {
        *self
    }
}

impl<T: Spanned, D: Deref<Target = T>> Spanned for D {
    fn span(&self) -> Span {
        self.deref().span()
    }
}

mod ast {
    use super::*;
    use crate::{
        parser::{
            ast,
            pool::{ExprPool, ExprRef},
        },
        utils::pooled::Pooled,
    };

    impl Spanned for Pooled<'_, &ast::Stmt, ExprPool> {
        fn span(&self) -> Span {
            match &self.val {
                ast::Stmt::Declare(declare) => self.wrap(declare).span(),
                ast::Stmt::MultiDecl(multi_decl) => self.wrap(multi_decl).span(),
                ast::Stmt::MethodDecl(method_decl) => self.wrap(method_decl).span(),
                ast::Stmt::Assign(assign) => self.wrap(assign).span(),
                ast::Stmt::MultiAssign(multi_assign) => self.wrap(multi_assign).span(),
                ast::Stmt::ExprStmt(suffixed_expr) => self.wrap(suffixed_expr).span(),
                ast::Stmt::Block(block) => self.wrap(block).span(),
                ast::Stmt::Return(return_stmt) => self.wrap(return_stmt).span(),
                ast::Stmt::Break(span) => *span,
                ast::Stmt::IfStmt(if_stmt) => self.wrap(if_stmt).span(),
                ast::Stmt::WhileStmt(while_stmt) => self.wrap(while_stmt).span(),
                ast::Stmt::RangeFor(range_for) => self.wrap(range_for).span(),
                ast::Stmt::KeyValFor(key_val_for) => self.wrap(key_val_for).span(),
                ast::Stmt::StructDecl(struct_decl) => self.wrap(struct_decl).span(),
            }
        }
    }

    impl Spanned for Pooled<'_, &ast::Declare, ExprPool> {
        fn span(&self) -> Span {
            let mut span = self.val.lhs.name;

            if let Some(ty) = &self.val.ty {
                span = span.cover(ty.as_ref().span());
            }

            if let Some(val) = self.val.val {
                span = span.cover(self.wrap(val).span());
            }

            span
        }
    }

    impl Spanned for Pooled<'_, &ast::MultiDecl, ExprPool> {
        fn span(&self) -> Span {
            let mut span = self.val.lhs_arr.iter().copied().covering().unwrap();
            if let Some(rhs_span) = self
                .val
                .rhs_arr
                .iter()
                .map(|it| self.wrap(*it).span())
                .covering()
            {
                span = span.cover(rhs_span);
            }
            span
        }
    }

    impl Spanned for Pooled<'_, &ast::MethodDecl, ExprPool> {
        fn span(&self) -> Span {
            Span::cover(self.val.method_name, self.val.struct_name)
                .cover(self.wrap(self.val.func.as_ref()).span())
        }
    }

    impl Spanned for Pooled<'_, &ast::Assign, ExprPool> {
        fn span(&self) -> Span {
            self.wrap(self.val.lhs.as_ref())
                .span()
                .cover(self.wrap(self.val.rhs).span())
        }
    }

    impl Spanned for Pooled<'_, &ast::MultiAssign, ExprPool> {
        fn span(&self) -> Span {
            self.val
                .lhs_arr
                .iter()
                .map(|it| self.wrap(it).span())
                .chain(self.val.rhs_arr.iter().map(|it| self.wrap(*it).span()))
                .covering()
                .unwrap()
        }
    }

    impl Spanned for Pooled<'_, &ast::Block, ExprPool> {
        fn span(&self) -> Span {
            match self.val {
                ast::Block::Some(stmts) => stmts
                    .iter()
                    .map(|it| self.wrap(it).span())
                    .covering()
                    .unwrap(),
                ast::Block::None(span) => *span,
            }
        }
    }

    impl Spanned for Pooled<'_, &ast::SpannedBlock, ExprPool> {
        fn span(&self) -> Span {
            self.val.span
        }
    }

    impl Spanned for Pooled<'_, &ast::ReturnStmt, ExprPool> {
        fn span(&self) -> Span {
            let mut span = self.val.kw_span;

            if let Some(val_span) = self
                .val
                .vals
                .iter()
                .map(|it| self.wrap(*it).span())
                .covering()
            {
                span = span.cover(val_span);
            }

            span
        }
    }

    impl Spanned for Pooled<'_, &ast::IfStmt, ExprPool> {
        fn span(&self) -> Span {
            let mut span = self.wrap(self.val.condition).span();

            span = span.cover(self.wrap(&self.val.body).span());

            if let Some(else_branch) = &self.val.else_ {
                span = match else_branch.as_ref() {
                    ast::ElseBranch::Else(block) => span.cover(self.wrap(block).span()),
                    ast::ElseBranch::ElseIf(if_stmt) => self.wrap(if_stmt).span(),
                };
            }

            span
        }
    }

    impl Spanned for Pooled<'_, &ast::WhileStmt, ExprPool> {
        fn span(&self) -> Span {
            self.val.kw_span.cover(self.wrap(&self.val.body).span())
        }
    }

    impl Spanned for Pooled<'_, &ast::RangeFor, ExprPool> {
        fn span(&self) -> Span {
            self.val.kw_span.cover(self.wrap(&self.val.body).span())
        }
    }

    impl Spanned for Pooled<'_, &ast::KeyValFor, ExprPool> {
        fn span(&self) -> Span {
            self.val.kw_span.cover(self.wrap(&self.val.body).span())
        }
    }

    impl Spanned for Pooled<'_, &ast::StructDecl, ExprPool> {
        fn span(&self) -> Span {
            self.val.name.cover_loc(self.val.end_loc)
        }
    }

    impl Spanned for Pooled<'_, &ast::Member, ExprPool> {
        fn span(&self) -> Span {
            self.val.name.cover(self.val.ty.span())
        }
    }

    // expr nodes
    impl Spanned for Pooled<'_, &ast::Expr, ExprPool> {
        fn span(&self) -> Span {
            match &self.val {
                ast::Expr::BinOp(bin_op) => {
                    Span::cover(self.wrap(bin_op.lhs).span(), self.wrap(bin_op.rhs).span())
                }
                ast::Expr::UnOp(un_op) => un_op.op_span.cover(self.wrap(un_op.val).span()),
                ast::Expr::Paren(paren_expr) => paren_expr.span,
                ast::Expr::Simple(simple_expr) => self.wrap(simple_expr).span(),
                ast::Expr::Name(span) => *span,
            }
        }
    }

    impl Spanned for Pooled<'_, ExprRef, ExprPool> {
        fn span(&self) -> Span {
            self.wrap(&self.pool[self.val]).span()
        }
    }

    impl Spanned for Pooled<'_, &ast::SimpleExpr, ExprPool> {
        fn span(&self) -> Span {
            match self.val {
                ast::SimpleExpr::Num(span)
                | ast::SimpleExpr::Str(span)
                | ast::SimpleExpr::Bool(span)
                | ast::SimpleExpr::Nil(span) => *span,
                ast::SimpleExpr::FuncNode(func_node) => self.wrap(func_node.as_ref()).span(),
                ast::SimpleExpr::TableNode(table_node) => table_node.span,
                ast::SimpleExpr::SuffixedExpr(suffixed_expr) => self.wrap(suffixed_expr).span(),
            }
        }
    }

    impl Spanned for Pooled<'_, &ast::FuncNode, ExprPool> {
        fn span(&self) -> Span {
            Span::cover(self.val.ty.span(), self.wrap(&self.val.body).span())
        }
    }

    impl Spanned for Pooled<'_, &ast::SuffixedExpr, ExprPool> {
        fn span(&self) -> Span {
            let mut span = self.wrap(self.val.val).span();

            for suffix in &self.val.suffixes {
                match suffix {
                    ast::Suffix::Index(index) => span = span.cover(index.span),
                    ast::Suffix::Access(access) => span = span.cover(access.field_name),
                    ast::Suffix::Call(call) => {
                        if let Some(arg_span) =
                            call.args.iter().map(|it| self.wrap(*it).span()).covering()
                        {
                            span = span.cover(arg_span);
                        }
                    }
                    ast::Suffix::Method(method) => {
                        span = span.cover(method.method_name);
                        if let Some(arg_span) = method
                            .args
                            .iter()
                            .map(|it| self.wrap(*it).span())
                            .covering()
                        {
                            span = span.cover(arg_span);
                        }
                    }
                }
            }

            span
        }
    }

    impl Spanned for Pooled<'_, &ast::RangeExpr, ExprPool> {
        fn span(&self) -> Span {
            Span::cover(
                self.wrap(self.val.lhs).span(),
                self.wrap(self.val.rhs).span(),
            )
        }
    }

    impl Spanned for Pooled<'_, &ast::SuffixedName, ExprPool> {
        fn span(&self) -> Span {
            let mut span = self.val.name;

            for suffix in &self.val.suffixes {
                match suffix {
                    ast::Suffix::Index(index) => span = span.cover(index.span),
                    ast::Suffix::Access(access) => span = span.cover(access.field_name),
                    ast::Suffix::Call(call) => {
                        if let Some(arg_span) =
                            call.args.iter().map(|it| self.wrap(*it).span()).covering()
                        {
                            span = span.cover(arg_span);
                        }
                    }
                    ast::Suffix::Method(method) => {
                        span = span.cover(method.method_name);
                        if let Some(arg_span) = method
                            .args
                            .iter()
                            .map(|it| self.wrap(*it).span())
                            .covering()
                        {
                            span = span.cover(arg_span);
                        }
                    }
                }
            }

            span
        }
    }

    // type nodes
    impl Spanned for ast::TypeNode {
        fn span(&self) -> Span {
            match self {
                ast::TypeNode::Name(span)
                | ast::TypeNode::Nil(span)
                | ast::TypeNode::VariadicType(span) => *span,
                ast::TypeNode::FunctionType(function_type) => function_type.span(),
                ast::TypeNode::TableType(table_type) => table_type.span(),
                ast::TypeNode::OptionalType(optional_type) => optional_type.span(),
            }
        }
    }

    impl Spanned for ast::FunctionType {
        fn span(&self) -> Span {
            let mut span = self.header_span;

            if let Some(return_type) = &self.return_type {
                span = span.cover(return_type.span());
            }

            span
        }
    }

    impl Spanned for ast::ReturnType {
        fn span(&self) -> Span {
            match self {
                ast::ReturnType::Single(ty) => ty.span(),
                ast::ReturnType::Multiple(multiple_type) => multiple_type.span,
            }
        }
    }

    impl Spanned for ast::TableType {
        fn span(&self) -> Span {
            Span::cover(self.key_span, self.val_type.as_ref().span())
        }
    }

    impl Spanned for ast::OptionalType {
        fn span(&self) -> Span {
            Span::cover(self.inner.as_ref().span(), self.question)
        }
    }
}
