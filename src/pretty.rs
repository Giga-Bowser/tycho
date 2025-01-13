use std::fmt::Display;

use crate::{parser::*, types::Type};

impl Display for OpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpKind::Add => write!(f, "+"),
            OpKind::Sub => write!(f, "-"),
            OpKind::Mul => write!(f, "*"),
            OpKind::Div => write!(f, "/"),
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
        }
    }
}

pub struct Printer<'src> {
    pub pool: ExprPool<'src>,
}

impl Printer<'_> {
    pub fn print(&self, stat: &Node) -> String {
        self.print_statement("".to_owned(), stat, true)
    }

    pub fn print_statement(&self, prefix: String, stat: &Node, is_end: bool) -> String {
        match stat {
            Node::Assign(assign) => self.print_assign(prefix, assign, is_end),
            Node::Break => todo!(),
            Node::MethodDecl(method_decl) => self.print_method_decl(prefix, method_decl, is_end),
            Node::Declare(decl) => self.print_decl(prefix, decl, is_end),
            Node::IfNode(if_node) => self.print_if_node(prefix, if_node, is_end),
            // Node::WhileNode(_) => todo!(),
            // Node::Block(_) => todo!(),
            Node::Return(returns) => self.print_return(prefix, returns, is_end),
            // Node::RangeFor(_) => todo!(),
            // Node::KeyValFor(_) => todo!(),
            // Node::MultiDecl(_) => todo!(),
            // Node::MultiAssign(_) => todo!(),
            Node::SuffixedExpr(suffixed_expr) => {
                self.print_suffixed_expr(prefix, suffixed_expr, is_end)
            }
            other => format!(
                "{}{}{:?}\n",
                prefix,
                if is_end { "└── " } else { "├── " },
                other
            ),
        }
    }

    fn print_assign(&self, prefix: String, assign: &Assign, is_end: bool) -> String {
        let new_prefix = prefix.clone() + if is_end { "    " } else { "│   " };
        format!(
            "{}{}{}\n{}{}",
            prefix,
            if is_end { "└── " } else { "├── " },
            "=",
            self.print_suffixed_name(new_prefix.clone(), &assign.lhs, false),
            self.print_expr(new_prefix, assign.rhs, true),
        )
    }

    fn print_suffixed_name(
        &self,
        prefix: String,
        suffixed_name: &SuffixedName,
        is_end: bool,
    ) -> String {
        let mut result = format!(
            "{}{}{}\n",
            prefix,
            if is_end { "└── " } else { "├── " },
            suffixed_name.name,
        );

        if suffixed_name.suffixes.is_empty() {
            return result;
        }

        let new_prefix = prefix + if is_end { "    " } else { "│   " };

        for suffix in suffixed_name
            .suffixes
            .iter()
            .take(suffixed_name.suffixes.len() - 1)
        {
            result += &self.print_suffix(new_prefix.clone(), suffix, false);
        }

        result += &self.print_suffix(new_prefix, suffixed_name.suffixes.last().unwrap(), true);
        result
    }

    fn print_suffix(&self, prefix: String, suffix: &Suffix, is_end: bool) -> String {
        match suffix {
            Suffix::Index(index) => self.print_index(prefix, index, is_end),
            Suffix::Access(access) => self.print_access(prefix, access, is_end),
            Suffix::Call(call) => self.print_call(prefix, call, is_end),
            Suffix::Method(method) => self.print_method(prefix, method, is_end),
        }
    }

    fn print_index(&self, prefix: String, index: &Index, is_end: bool) -> String {
        let new_prefix = prefix.clone() + if is_end { "    " } else { "│   " };
        format!(
            "{}{}{}\n{}",
            prefix,
            if is_end { "└── " } else { "├── " },
            "[]",
            self.print_expr(new_prefix, index.key, true)
        )
    }

    fn print_access(&self, prefix: String, access: &Access, is_end: bool) -> String {
        format!(
            "{}{}.{}\n",
            prefix,
            if is_end { "└── " } else { "├── " },
            access.field_name,
        )
    }

    fn print_call(&self, prefix: String, call: &Call, is_end: bool) -> String {
        let mut result = format!(
            "{}{}{}\n",
            prefix,
            if is_end { "└── " } else { "├── " },
            "()",
        );

        if call.args.is_empty() {
            return result;
        }

        let new_prefix = prefix + if is_end { "    " } else { "│   " };

        for arg in call.args.iter().take(call.args.len() - 1) {
            result += &self.print_expr(new_prefix.clone(), *arg, false);
        }

        result += &self.print_expr(new_prefix, *call.args.last().unwrap(), true);

        result
    }

    fn print_method(&self, prefix: String, method: &Method, is_end: bool) -> String {
        let mut result = format!(
            "{}{}:{}()\n",
            prefix,
            if is_end { "└── " } else { "├── " },
            method.method_name,
        );

        if method.args.is_empty() {
            return result;
        }

        let new_prefix = prefix + if is_end { "    " } else { "│   " };

        for arg in method.args.iter().take(method.args.len() - 1) {
            result += &self.print_expr(new_prefix.clone(), *arg, false);
        }

        result += &self.print_expr(new_prefix, *method.args.last().unwrap(), true);

        result
    }

    fn print_expr(&self, prefix: String, expr: ExprRef, is_end: bool) -> String {
        match &self.pool[expr] {
            Node::BinOp(binop) => self.print_binop(prefix, binop, is_end),
            Node::UnOp(unop) => self.print_unop(prefix, unop, is_end),
            Node::SimpleExpr(simple_expr) => self.print_simple_expr(prefix, simple_expr, is_end),
            Node::Name(name) => format!(
                "{}{}{}\n",
                prefix,
                if is_end { "└── " } else { "├── " },
                name,
            ),
            _ => unreachable!(),
        }
    }

    fn print_binop(&self, prefix: String, binop: &BinOp, is_end: bool) -> String {
        let new_prefix = prefix.clone() + if is_end { "    " } else { "│   " };
        format!(
            "{}{}{}\n{}{}",
            prefix,
            if is_end { "└── " } else { "├── " },
            binop.op,
            self.print_expr(new_prefix.clone(), binop.lhs, false),
            self.print_expr(new_prefix, binop.rhs, true),
        )
    }

    fn print_unop(&self, prefix: String, unop: &UnOp, is_end: bool) -> String {
        let new_prefix = prefix.clone() + if is_end { "    " } else { "│   " };
        format!(
            "{}{}{}\n{}",
            prefix,
            if is_end { "└── " } else { "├── " },
            unop.op,
            self.print_expr(new_prefix, unop.val, true)
        )
    }

    fn print_simple_expr(&self, prefix: String, simple_expr: &SimpleExpr, is_end: bool) -> String {
        match simple_expr {
            SimpleExpr::Num(str)
            | SimpleExpr::Str(str)
            | SimpleExpr::Bool(str)
            | SimpleExpr::Nil(str) => {
                format!(
                    "{}{}{}\n",
                    prefix,
                    if is_end { "└── " } else { "├── " },
                    str,
                )
            }
            SimpleExpr::FuncNode(func_node) => self.print_func_node(prefix, func_node, is_end),
            SimpleExpr::TableNode(table_node) => self.print_table_node(prefix, table_node, is_end),
            SimpleExpr::StructNode(struct_node) => {
                self.print_struct_node(prefix, struct_node, is_end)
            }
            SimpleExpr::SuffixedExpr(suffixed_expr) => {
                self.print_suffixed_expr(prefix, suffixed_expr, is_end)
            }
        }
    }

    fn print_func_node(&self, prefix: String, func_node: &FuncNode, is_end: bool) -> String {
        // TODO: pretty print function's type
        let mut result = format!(
            "{}{}{:?}\n",
            prefix,
            if is_end { "└── " } else { "├── " },
            *func_node.type_,
        );

        if func_node.body.is_empty() {
            return result;
        }

        let new_prefix = prefix + if is_end { "    " } else { "│   " };

        for stat in func_node.body.iter().take(func_node.body.len() - 1) {
            result += &self.print_statement(new_prefix.clone(), stat, false);
        }

        result += &self.print_statement(new_prefix, func_node.body.last().unwrap(), true);

        result
    }

    fn print_field_node(&self, prefix: String, field_node: &FieldNode, is_end: bool) -> String {
        let mut result = format!("{}{}", prefix, if is_end { "└── " } else { "├── " });
        result += &match field_node {
            FieldNode::Field { key, val } => {
                format!("\"{}\" = {}", key, self.print_expr(prefix, *val, is_end))
            }
            FieldNode::ExprField { key, val } => format!(
                "[{}] = {}",
                self.print_expr(prefix.clone(), *key, is_end),
                self.print_expr(prefix, *val, is_end)
            ),
            FieldNode::ValField { val } => self.print_expr(prefix, *val, is_end),
        };

        result += "\n";
        result
    }

    fn print_table_node(&self, prefix: String, table_node: &TableNode, is_end: bool) -> String {
        let mut result = format!(
            "{}{}{}\n",
            prefix,
            if is_end { "└── " } else { "├── " },
            "table",
        );

        if table_node.fields.is_empty() {
            return result;
        }

        let new_prefix = prefix + if is_end { "    " } else { "│   " };

        for field_node in table_node.fields.iter().take(table_node.fields.len() - 1) {
            result += &self.print_field_node(new_prefix.clone(), field_node, false)
        }

        result += &self.print_field_node(new_prefix, table_node.fields.last().unwrap(), true);

        result
    }

    fn print_struct_node(&self, prefix: String, struct_node: &StructNode, is_end: bool) -> String {
        let mut result = format!(
            "{}{}{}\n",
            prefix,
            if is_end { "└── " } else { "├── " },
            "struct",
        );

        if struct_node.type_.fields.is_empty() {
            return result;
        }

        let new_prefix = prefix + if is_end { "    " } else { "│   " };

        for (name, type_) in struct_node
            .type_
            .fields
            .iter()
            .take(struct_node.type_.fields.len() - 1)
        {
            result += &format!("{}├── {}: {:?}", new_prefix, name, type_);
        }

        result += &format!(
            "{}└── {}: {:?}",
            new_prefix,
            struct_node.type_.fields.last().unwrap().0,
            struct_node.type_.fields.last().unwrap().1
        );

        result
    }

    fn print_suffixed_expr(
        &self,
        prefix: String,
        suffixed_expr: &SuffixedExpr,
        is_end: bool,
    ) -> String {
        if suffixed_expr.suffixes.is_empty() {
            return self.print_expr(prefix, suffixed_expr.val, is_end);
        }
        let new_prefix = prefix.clone() + if is_end { "    " } else { "│   " };
        let mut result = format!(
            "{}{}",
            prefix,
            self.print_expr("".to_owned(), suffixed_expr.val, is_end),
        );

        for suffix in suffixed_expr
            .suffixes
            .iter()
            .take(suffixed_expr.suffixes.len() - 1)
        {
            result += &self.print_suffix(new_prefix.clone(), suffix, false);
        }

        result += &self.print_suffix(new_prefix, suffixed_expr.suffixes.last().unwrap(), true);

        result
    }

    fn print_method_decl(&self, prefix: String, method_decl: &MethodDecl, is_end: bool) -> String {
        let new_prefix = prefix.clone() + if is_end { "    " } else { "│   " };
        format!(
            "{}{}{}:{}\n{}",
            prefix,
            if is_end { "└── " } else { "├── " },
            method_decl.struct_name,
            method_decl.method_name,
            self.print_func_node(new_prefix, &method_decl.func, true)
        )
    }

    fn print_decl(&self, prefix: String, decl: &Declare, is_end: bool) -> String {
        let new_prefix = prefix.clone() + if is_end { "    " } else { "│   " };

        let mut result = format!(
            "{}{}{}\n{}{}",
            prefix,
            if is_end { "└── " } else { "├── " },
            ":=",
            self.print_suffixed_name(new_prefix.clone(), &decl.lhs, false),
            self.print_type(new_prefix.clone(), &decl.type_.clone(), decl.val.is_none()),
        );

        if let Some(val) = decl.val {
            result += &self.print_expr(new_prefix, val, true);
        }

        result
    }

    fn print_type(&self, prefix: String, type_: &Type, is_end: bool) -> String {
        format!(
            "{}{}{:?}\n",
            prefix,
            if is_end { "└── " } else { "├── " },
            type_,
        )
    }

    fn print_if_node(&self, prefix: String, if_node: &IfNode, is_end: bool) -> String {
        let new_prefix = prefix.clone() + if is_end { "    " } else { "│   " };

        let mut result = format!(
            "{}{}{}\n{}",
            prefix,
            if is_end { "└── " } else { "├── " },
            "if",
            self.print_expr(new_prefix.clone(), if_node.condition, false),
        );

        if !if_node.body.is_empty() {
            for stat in if_node.body.iter().take(if_node.body.len() - 1) {
                result += &self.print_statement(new_prefix.clone(), stat, false);
            }

            result += &self.print_statement(
                new_prefix.clone(),
                if_node.body.last().unwrap(),
                if_node.else_.is_none(),
            );
        }

        if let Some(else_) = &if_node.else_ {
            result += &format!("{}else\n", new_prefix);
            match else_.as_ref() {
                Else::Else(body) => {
                    if !body.is_empty() {
                        for stat in body.iter().take(body.len() - 1) {
                            result += &self.print_statement(new_prefix.clone(), stat, false);
                        }

                        result += &self.print_statement(new_prefix, body.last().unwrap(), true);
                    }
                }
                Else::ElseIf(else_if_node) => {
                    result += &self.print_if_node(new_prefix, else_if_node, is_end)
                }
            }
        }

        result
    }

    fn print_return(&self, prefix: String, returns: &[ExprRef], is_end: bool) -> String {
        let mut result = format!("{}{}return\n", prefix, if is_end { "└── " } else { "├── " },);

        if returns.is_empty() {
            return result;
        }

        let new_prefix = prefix + if is_end { "    " } else { "│   " };

        for val in returns.iter().take(returns.len() - 1) {
            result += &self.print_expr(new_prefix.clone(), *val, false);
        }

        result += &self.print_expr(new_prefix, *returns.last().unwrap(), true);

        result
    }
}
