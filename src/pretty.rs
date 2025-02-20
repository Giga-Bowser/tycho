use std::fmt::Display;

use crate::{
    format_to,
    parser::{
        ast::*,
        pool::{ExprPool, ExprRef},
    },
    types::Type,
};

impl Display for OpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpKind::Add => write!(f, "+"),
            OpKind::Sub => write!(f, "-"),
            OpKind::Mul => write!(f, "*"),
            OpKind::Div => write!(f, "/"),
            OpKind::Mod => write!(f, "%"),
            OpKind::Pow => write!(f, "^"),
            OpKind::Cat => write!(f, ".."),
            OpKind::Equ => write!(f, "=="),
            OpKind::Neq => write!(f, "!="),
            OpKind::Gre => write!(f, ">"),
            OpKind::Grq => write!(f, ">="),
            OpKind::Les => write!(f, "<"),
            OpKind::Leq => write!(f, "<="),
            OpKind::And => write!(f, "&&"),
            OpKind::Or => write!(f, "||"),
        }
    }
}

impl Display for UnOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOpKind::Neg => write!(f, "-"),
            UnOpKind::Len => write!(f, "#"),
            UnOpKind::Not => write!(f, "!"),
        }
    }
}

pub struct Printer<'src, 'pool> {
    pub pool: &'pool ExprPool<'src>,
}

impl Printer<'_, '_> {
    pub fn print(&self, stmt: &Statement<'_>) -> String {
        self.print_statement("", stmt, true)
    }

    fn print_statement(&self, prefix: &str, stmt: &Statement<'_>, is_end: bool) -> String {
        match stmt {
            Statement::Assign(assign) => self.print_assign(prefix, assign, is_end),
            Statement::Break => format!("{prefix}{}break\n", if is_end { "└── " } else { "├── " }),
            Statement::MethodDecl(method_decl) => {
                self.print_method_decl(prefix, method_decl, is_end)
            }
            Statement::Declare(decl) => self.print_decl(prefix, decl, is_end),
            Statement::IfStat(if_stat) => self.print_if_stat(prefix, if_stat, is_end),
            // Node::WhileStat(_) => todo!(),
            // Node::Block(_) => todo!(),
            Statement::Return(returns) => self.print_return(prefix, returns, is_end),
            // Node::RangeFor(_) => todo!(),
            // Node::KeyValFor(_) => todo!(),
            // Node::MultiDecl(_) => todo!(),
            // Node::MultiAssign(_) => todo!(),
            Statement::ExprStat(suffixed_expr) => {
                self.print_suffixed_expr(prefix, suffixed_expr, is_end)
            }
            Statement::StructDecl(struct_decl) => {
                self.print_struct_decl(prefix, struct_decl, is_end)
            }
            other => format!(
                "{prefix}{}{other:?}\n",
                if is_end { "└── " } else { "├── " }
            ),
        }
    }

    fn print_assign(&self, prefix: &str, assign: &Assign<'_>, is_end: bool) -> String {
        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };
        format!(
            "{prefix}{}{}\n{}{}",
            if is_end { "└── " } else { "├── " },
            "=",
            self.print_suffixed_name(&new_prefix, &assign.lhs, false),
            self.print_expr(&new_prefix, assign.rhs, true),
        )
    }

    fn print_suffixed_name(
        &self,
        prefix: &str,
        suffixed_name: &SuffixedName<'_>,
        is_end: bool,
    ) -> String {
        let mut result = format!(
            "{prefix}{}{}\n",
            if is_end { "└── " } else { "├── " },
            suffixed_name.name,
        );

        if suffixed_name.suffixes.is_empty() {
            return result;
        }

        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };

        for suffix in suffixed_name
            .suffixes
            .iter()
            .take(suffixed_name.suffixes.len() - 1)
        {
            result += &self.print_suffix(&new_prefix, suffix, false);
        }

        result += &self.print_suffix(&new_prefix, suffixed_name.suffixes.last().unwrap(), true);
        result
    }

    fn print_suffix(&self, prefix: &str, suffix: &Suffix<'_>, is_end: bool) -> String {
        match suffix {
            Suffix::Index(index) => self.print_index(prefix, index, is_end),
            Suffix::Access(access) => self.print_access(prefix, access, is_end),
            Suffix::Call(call) => self.print_call(prefix, call, is_end),
            Suffix::Method(method) => self.print_method(prefix, method, is_end),
        }
    }

    fn print_index(&self, prefix: &str, index: &Index, is_end: bool) -> String {
        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };
        format!(
            "{prefix}{}{}\n{}",
            if is_end { "└── " } else { "├── " },
            "[]",
            self.print_expr(&new_prefix, index.key, true)
        )
    }

    fn print_access(&self, prefix: &str, access: &Access<'_>, is_end: bool) -> String {
        format!(
            "{prefix}{}.{}\n",
            if is_end { "└── " } else { "├── " },
            access.field_name,
        )
    }

    fn print_call(&self, prefix: &str, call: &Call, is_end: bool) -> String {
        let mut result = format!("{prefix}{}{}\n", if is_end { "└── " } else { "├── " }, "()",);

        if call.args.is_empty() {
            return result;
        }

        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };

        for arg in call.args.iter().take(call.args.len() - 1) {
            result += &self.print_expr(&new_prefix, *arg, false);
        }

        result += &self.print_expr(&new_prefix, *call.args.last().unwrap(), true);

        result
    }

    fn print_method(&self, prefix: &str, method: &Method<'_>, is_end: bool) -> String {
        let mut result = format!(
            "{prefix}{}:{}()\n",
            if is_end { "└── " } else { "├── " },
            method.method_name,
        );

        if method.args.is_empty() {
            return result;
        }

        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };

        for arg in method.args.iter().take(method.args.len() - 1) {
            result += &self.print_expr(&new_prefix, *arg, false);
        }

        result += &self.print_expr(&new_prefix, *method.args.last().unwrap(), true);

        result
    }

    fn print_expr(&self, prefix: &str, expr: ExprRef, is_end: bool) -> String {
        match &self.pool[expr] {
            Expr::BinOp(binop) => self.print_binop(prefix, binop, is_end),
            Expr::UnOp(unop) => self.print_unop(prefix, unop, is_end),
            Expr::Paren(paren_expr) => self.print_expr(prefix, paren_expr.val, is_end),
            Expr::Simple(simple_expr) => self.print_simple_expr(prefix, simple_expr, is_end),
            Expr::Name(name) => {
                format!("{prefix}{}{}\n", if is_end { "└── " } else { "├── " }, name,)
            }
        }
    }

    fn print_binop(&self, prefix: &str, binop: &BinOp, is_end: bool) -> String {
        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };
        format!(
            "{prefix}{}{}\n{}{}",
            if is_end { "└── " } else { "├── " },
            binop.op,
            self.print_expr(&new_prefix, binop.lhs, false),
            self.print_expr(&new_prefix, binop.rhs, true),
        )
    }

    fn print_unop(&self, prefix: &str, unop: &UnOp, is_end: bool) -> String {
        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };
        format!(
            "{prefix}{}{}\n{}",
            if is_end { "└── " } else { "├── " },
            unop.op,
            self.print_expr(&new_prefix, unop.val, true)
        )
    }

    fn print_simple_expr(
        &self,
        prefix: &str,
        simple_expr: &SimpleExpr<'_>,
        is_end: bool,
    ) -> String {
        match simple_expr {
            SimpleExpr::Num(str)
            | SimpleExpr::Str(str)
            | SimpleExpr::Bool(str)
            | SimpleExpr::Nil(str) => {
                format!("{prefix}{}{}\n", if is_end { "└── " } else { "├── " }, str,)
            }
            SimpleExpr::FuncNode(func_node) => self.print_func_node(prefix, func_node, is_end),
            SimpleExpr::TableNode(table_node) => self.print_table_node(prefix, table_node, is_end),
            SimpleExpr::SuffixedExpr(suffixed_expr) => {
                self.print_suffixed_expr(prefix, suffixed_expr, is_end)
            }
        }
    }

    fn print_func_node(&self, prefix: &str, func_node: &FuncNode<'_>, is_end: bool) -> String {
        // TODO: pretty print function's type
        let mut result = format!(
            "{prefix}{}{:?}\n",
            if is_end { "└── " } else { "├── " },
            *func_node.type_,
        );

        if func_node.body.is_empty() {
            return result;
        }

        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };

        for stmt in func_node.body.iter().take(func_node.body.len() - 1) {
            result += &self.print_statement(&new_prefix, stmt, false);
        }

        result += &self.print_statement(&new_prefix, func_node.body.last().unwrap(), true);

        result
    }

    fn print_field_node(&self, prefix: &str, field_node: &FieldNode<'_>, is_end: bool) -> String {
        let mut result = format!("{prefix}{}", if is_end { "└── " } else { "├── " });
        result += &match field_node {
            FieldNode::Field { key, val } => {
                format!("\"{key}\" = {}", self.print_expr(prefix, *val, is_end))
            }
            FieldNode::ExprField { key, val } => format!(
                "[{}] = {}",
                self.print_expr(prefix, *key, is_end),
                self.print_expr(prefix, *val, is_end)
            ),
            FieldNode::ValField { val } => self.print_expr(prefix, *val, is_end),
        };

        result += "\n";
        result
    }

    fn print_table_node(&self, prefix: &str, table_node: &TableNode<'_>, is_end: bool) -> String {
        let mut result = format!(
            "{prefix}{}{}\n",
            if is_end { "└── " } else { "├── " },
            "table",
        );

        if table_node.fields.is_empty() {
            return result;
        }

        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };

        for field_node in table_node.fields.iter().take(table_node.fields.len() - 1) {
            result += &self.print_field_node(&new_prefix, field_node, false);
        }

        result += &self.print_field_node(&new_prefix, table_node.fields.last().unwrap(), true);

        result
    }

    fn print_struct_decl(
        &self,
        prefix: &str,
        struct_decl: &StructDecl<'_>,
        is_end: bool,
    ) -> String {
        let mut result = format!(
            "{prefix}{}{}\n",
            if is_end { "└── " } else { "├── " },
            "struct",
        );

        if struct_decl.type_.fields.is_empty() {
            return result;
        }

        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };

        for (name, type_) in struct_decl
            .type_
            .fields
            .iter()
            .take(struct_decl.type_.fields.len() - 1)
        {
            result += &format!("{new_prefix}├── {name}: {type_:?}");
        }

        result += &format!(
            "{new_prefix}└── {}: {:?}",
            struct_decl.type_.fields.last().unwrap().0,
            struct_decl.type_.fields.last().unwrap().1
        );

        result
    }

    pub fn print_suffixed_expr(
        &self,
        prefix: &str,
        suffixed_expr: &SuffixedExpr<'_>,
        is_end: bool,
    ) -> String {
        if suffixed_expr.suffixes.is_empty() {
            return self.print_expr(prefix, suffixed_expr.val, is_end);
        }
        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };
        let mut result = format!("{prefix}{}", self.print_expr("", suffixed_expr.val, is_end),);

        for suffix in suffixed_expr
            .suffixes
            .iter()
            .take(suffixed_expr.suffixes.len() - 1)
        {
            result += &self.print_suffix(&new_prefix, suffix, false);
        }

        result += &self.print_suffix(&new_prefix, suffixed_expr.suffixes.last().unwrap(), true);

        result
    }

    fn print_method_decl(
        &self,
        prefix: &str,
        method_decl: &MethodDecl<'_>,
        is_end: bool,
    ) -> String {
        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };
        format!(
            "{prefix}{}{}:{}\n{}",
            if is_end { "└── " } else { "├── " },
            method_decl.struct_name,
            method_decl.method_name,
            self.print_func_node(&new_prefix, &method_decl.func, true)
        )
    }

    fn print_decl(&self, prefix: &str, decl: &Declare<'_>, is_end: bool) -> String {
        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };

        let mut result = format!(
            "{prefix}{}{}\n{}{}",
            if is_end { "└── " } else { "├── " },
            ":=",
            self.print_suffixed_name(&new_prefix, &decl.lhs, false),
            self.print_type(&new_prefix, &decl.type_.clone(), decl.val.is_none()),
        );

        if let Some(val) = decl.val {
            result += &self.print_expr(&new_prefix, val, true);
        }

        result
    }

    fn print_type(&self, prefix: &str, type_: &Type, is_end: bool) -> String {
        format!(
            "{prefix}{}{:?}\n",
            if is_end { "└── " } else { "├── " },
            type_,
        )
    }

    fn print_if_stat(&self, prefix: &str, if_stat: &IfStat<'_>, is_end: bool) -> String {
        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };

        let mut result = format!(
            "{prefix}{}{}\n{}",
            if is_end { "└── " } else { "├── " },
            "if",
            self.print_expr(&new_prefix, if_stat.condition, false),
        );

        if !if_stat.body.is_empty() {
            for stmt in if_stat.body.iter().take(if_stat.body.len() - 1) {
                result += &self.print_statement(&new_prefix, stmt, false);
            }

            result += &self.print_statement(
                &new_prefix,
                if_stat.body.last().unwrap(),
                if_stat.else_.is_none(),
            );
        }

        if let Some(else_) = &if_stat.else_ {
            format_to!(result, "{new_prefix}else\n");
            match else_.as_ref() {
                ElseBranch::Else(body) => {
                    if !body.is_empty() {
                        for stmt in body.iter().take(body.len() - 1) {
                            result += &self.print_statement(&new_prefix, stmt, false);
                        }

                        result += &self.print_statement(&new_prefix, body.last().unwrap(), true);
                    }
                }
                ElseBranch::ElseIf(else_if_stat) => {
                    result += &self.print_if_stat(&new_prefix, else_if_stat, is_end);
                }
            }
        }

        result
    }

    fn print_return(&self, prefix: &str, returns: &[ExprRef], is_end: bool) -> String {
        let mut result = format!("{prefix}{}return\n", if is_end { "└── " } else { "├── " },);

        if returns.is_empty() {
            return result;
        }

        let new_prefix = prefix.to_owned() + if is_end { "    " } else { "│   " };

        for val in returns.iter().take(returns.len() - 1) {
            result += &self.print_expr(&new_prefix, *val, false);
        }

        result += &self.print_expr(&new_prefix, *returns.last().unwrap(), true);

        result
    }
}
