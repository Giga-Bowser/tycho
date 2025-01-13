use std::cell::Cell;

use crate::{format_to, parser::*};

#[derive(Debug, Default)]
pub struct Compiler<'src> {
    pub pool: ExprPool<'src>,
    indent: Cell<usize>,
}

impl<'src> Compiler<'src> {
    pub fn new(pool: ExprPool<'src>) -> Self {
        Self {
            pool,
            indent: 0.into(),
        }
    }

    pub fn compile_statement(&self, statement: &Node) -> String {
        match statement {
            Node::Declare(decl) => self.compile_decl(decl),
            _ => "".to_owned(),
        }
    }

    fn compile_decl(&self, decl: &Declare) -> String {
        let Some(val) = decl.val else {
            // no val is assigned, this means this statement is only useful for type checking
            // we can just return an empty string
            return "".to_owned();
        };

        if decl.lhs.suffixes.is_empty() {
            if let Node::SimpleExpr(SimpleExpr::FuncNode(func)) = &self.pool[val] {
                let name = decl.lhs.name;
                return format!(
                    "local {name}{}{name} = {}",
                    self.newline(),
                    self.compile_func(func)
                );
            } else {
                return format!("local {} = {}", decl.lhs.name, self.compile_expr(val));
            }
        }

        let mut lhs = decl.lhs.name.to_owned();

        for suffix in &decl.lhs.suffixes {
            lhs += &self.compile_suffix(suffix);
        }

        format_to!(lhs, " = {}", self.compile_expr(val));

        lhs
    }

    fn compile_expr(&self, expr: ExprRef) -> String {
        match &self.pool[expr] {
            Node::BinOp(binop) => format!(
                "({} {} {})",
                self.compile_expr(binop.lhs),
                binop.op,
                self.compile_expr(binop.rhs),
            ),
            Node::UnOp(UnOp { op, val }) => op.to_string() + &self.compile_expr(*val),
            Node::SimpleExpr(simple_expr) => self.compile_simple_expr(simple_expr),
            Node::Name(str) => (*str).to_owned(),

            other => unreachable!("{other:?} found in expr"),
        }
    }

    fn compile_simple_expr(&self, simple_expr: &SimpleExpr) -> String {
        match simple_expr {
            SimpleExpr::Num(str)
            | SimpleExpr::Str(str)
            | SimpleExpr::Bool(str)
            | SimpleExpr::Nil(str) => (*str).to_owned(),
            SimpleExpr::FuncNode(func_node) => self.compile_func(func_node),
            SimpleExpr::TableNode(table_node) => self.compile_table(table_node),
            SimpleExpr::StructNode(struct_node) => self.compile_struct(struct_node),
            SimpleExpr::SuffixedExpr(suffixed_expr) => self.compile_suffixed_expr(suffixed_expr),
        }
    }

    fn compile_func(&self, func_node: &FuncNode) -> String {
        let mut result = "function(".to_owned();
        result += &func_node
            .type_
            .params
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<&str>>()
            .join(", ");
        result += ")";
        result += &self.newline();
        result += &self.compile_block(&func_node.body);
        result
    }

    fn compile_block(&self, block: &[Node]) -> String {
        let mut result = String::new();
        self.indent();
        for statement in block {
            result += &self.compile_statement(statement);
            result += &self.newline();
        }
        self.dedent();
        result
    }

    fn compile_struct(&self, struct_node: &StructNode) -> String {
        let newline = self.newline();
        let name = struct_node.name.unwrap_or("_");
        let mut result = format!("{{}}{newline}{name}.__index = {name}");

        if let Some(constructor) = &struct_node.constructor {
            let mut param_list = String::new();
            for (param_name, _) in &constructor.type_.params {
                param_list.push_str(", ");
                param_list += param_name;
            }

            let body = self.compile_block(&constructor.body);

            result += &newline;
            format_to!(result, ".new = function(_self{param_list}){newline}\t");
            format_to!(result, "local self = {{}}{newline}{body}\t");
            format_to!(result, "setmetatable(self, _self){newline}\t");
            format_to!(result, "_self.__index = _self{newline}\t");
            format_to!(result, "return self{newline}end");
        }

        result
    }

    fn compile_table(&self, table_node: &TableNode) -> String {
        let mut result = String::new();

        result += "{";
        result += &self.newline();
        for field in &table_node.fields {
            result.push('\t');

            let field_str = match field {
                FieldNode::Field { key, val } => format!("{key} = {}", &self.compile_expr(*val)),
                FieldNode::ExprField { key, val } => format!(
                    "[{}] = {}",
                    &self.compile_expr(*key),
                    &self.compile_expr(*val)
                ),
                FieldNode::ValField { val } => self.compile_expr(*val),
            };

            result += &field_str;
            result += &self.newline();
        }
        result.push('}');

        result
    }

    fn compile_suffixed_expr(&self, suffixed_expr: &SuffixedExpr) -> String {
        let mut result = self.compile_expr(suffixed_expr.val);

        for suffix in &suffixed_expr.suffixes {
            result += &self.compile_suffix(suffix);
        }

        result
    }

    fn compile_suffix(&self, suffix: &Suffix) -> String {
        match suffix {
            Suffix::Method(Method {
                method_name: name,
                args,
            }) => {
                format!(
                    ":{name}({})",
                    args.iter()
                        .map(|&arg| self.compile_expr(arg))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Suffix::Call(Call { args }) => {
                format!(
                    "({})",
                    args.iter()
                        .map(|&arg| self.compile_expr(arg))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Suffix::Access(Access { field_name }) => {
                format!(".{field_name}")
            }
            Suffix::Index(Index { key }) => {
                format!("[{}]", self.compile_expr(*key))
            }
        }
    }

    fn newline(&self) -> String {
        format!("\n{}", "\t".repeat(self.indent.get()))
    }

    fn indent(&self) {
        let new = self.indent.get() + 1;
        self.indent.set(new);
    }

    fn dedent(&self) {
        let new = self.indent.get() - 1;
        self.indent.set(new);
    }
}
