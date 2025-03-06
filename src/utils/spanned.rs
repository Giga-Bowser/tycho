use crate::utils::Span;

pub(crate) trait Spanned<'s> {
    fn span(&self) -> Span<'s>;
}

mod ast {
    use super::*;
    use crate::parser::ast;
    impl<'s> Spanned<'s> for ast::OptionalType<'s> {
        fn span(&self) -> Span<'s> {
            Span::cover(self.inner.span(), self.question)
        }
    }

    impl<'s> ast::FunctionType<'s> {
        pub fn span(&self) -> Span<'s> {
            self.return_type
                .as_ref()
                .map_or(self.header_span, |it| it.span())
        }
    }

    impl<'s> ast::ReturnType<'s> {
        pub fn span(&self) -> Span<'s> {
            match self {
                ast::ReturnType::Single(ty) => ty.span(),
                ast::ReturnType::Multiple(multiple_type) => multiple_type.span,
            }
        }
    }

    impl<'s> Spanned<'s> for ast::TableType<'s> {
        fn span(&self) -> Span<'s> {
            Span::cover(self.key_span, self.val_type.span())
        }
    }

    impl<'s> Spanned<'s> for ast::TypeNode<'s> {
        fn span(&self) -> Span<'s> {
            match self {
                ast::TypeNode::Name(span)
                | ast::TypeNode::Nil(span)
                | ast::TypeNode::VariadicType(span) => *span,
                ast::TypeNode::FunctionType(function_type) => function_type.span(),
                ast::TypeNode::TableType(table_type) => {
                    Span::cover(table_type.key_span, table_type.val_type.span())
                }
                ast::TypeNode::OptionalType(optional_type) => {
                    Span::cover(optional_type.inner.span(), optional_type.question)
                }
            }
        }
    }
}
