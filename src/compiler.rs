use std::{cell::Cell, slice, str};

use crate::{format_to, parser::*};

#[derive(Debug)]
pub struct Compiler<'src, 'pool> {
    pub pool: &'pool ExprPool<'src>,
    indent: Cell<usize>,
}

impl<'src, 'pool> Compiler<'src, 'pool> {
    pub fn new(pool: &'pool ExprPool<'src>) -> Self {
        Self {
            pool,
            indent: 0.into(),
        }
    }

    pub fn compile_statement(&self, statement: &Statement) -> String {
        match statement {
            Statement::Declare(decl) => self.compile_decl(decl),
            Statement::MultiDecl(multi_decl) => self.compile_multi_decl(multi_decl),
            Statement::MethodDecl(method_decl) => self.compile_method_decl(method_decl),
            Statement::Assign(Assign { lhs, rhs }) => format!(
                "{} = {}",
                self.compile_suffixed_name(lhs),
                self.compile_expr(*rhs)
            ),
            Statement::MultiAssign(multi_assign) => self.compile_multi_assign(multi_assign),
            Statement::ExprStat(suffixed_expr) => self.compile_suffixed_expr(suffixed_expr),
            Statement::Block(statements) => {
                let mut result = "do".to_owned();
                self.indent();
                for stat in statements {
                    result += &self.newline();
                    result += &self.compile_statement(stat);
                }
                self.dedent();
                result += &self.newline();
                result += "end";
                result
            }
            Statement::Return(return_exprs) => {
                if return_exprs.is_empty() {
                    "return".to_owned()
                } else {
                    format!(
                        "return {}",
                        return_exprs
                            .iter()
                            .map(|expr| self.compile_expr(*expr))
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }
            }
            Statement::Break => "break".to_owned(),
            Statement::IfStat(if_stat) => self.compile_if_stat(if_stat),
            Statement::WhileStat(while_stat) => self.compile_while_stat(while_stat),
            Statement::RangeFor(range_for) => self.compile_range_for(range_for),
            Statement::KeyValFor(keyval_for) => self.compile_keyval_for(keyval_for),
        }
    }

    fn compile_decl(&self, decl: &Declare) -> String {
        let Some(val) = decl.val else {
            // no val is assigned, this means this statement is only useful for type checking
            // we can just return an empty string
            return "".to_owned();
        };

        if decl.lhs.suffixes.is_empty() {
            if let Expr::Simple(SimpleExpr::FuncNode(func)) = &self.pool[val] {
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

    fn compile_multi_decl(&self, multi_decl: &MultiDecl) -> String {
        let lhs_result = multi_decl.lhs_arr.join(", ");
        let rhs_result = multi_decl
            .rhs_arr
            .iter()
            .map(|expr| self.compile_expr(*expr))
            .collect::<Vec<String>>()
            .join(", ");

        format!("local {lhs_result} = {rhs_result}")
    }

    fn compile_method_decl(&self, method_decl: &MethodDecl) -> String {
        // let Some(val) = method_decl.val else {
        //     // no val is assigned, this means this statement is only useful for type checking
        //     // we can just return an empty string
        //     return "".to_owned();
        // };

        let mut result = format!(
            "{}.{} = function(self",
            method_decl.struct_name, method_decl.method_name
        );

        for (param_name, _) in &method_decl.func.type_.params {
            result += ", ";
            result += param_name;
        }

        let block = self.compile_block(&method_decl.func.body);
        format_to!(result, "){block}end");

        result
    }

    fn compile_multi_assign(&self, multi_assign: &MultiAssign) -> String {
        let lhs_result = multi_assign
            .lhs_arr
            .iter()
            .map(|suffixed_expr| self.compile_suffixed_expr(suffixed_expr))
            .collect::<Vec<String>>()
            .join(", ");
        let rhs_result = multi_assign
            .rhs_arr
            .iter()
            .map(|expr| self.compile_expr(*expr))
            .collect::<Vec<String>>()
            .join(", ");

        format!("{lhs_result} = {rhs_result}")
    }

    fn compile_if_stat(&self, if_stat: &IfStat) -> String {
        let condition = self.compile_expr(if_stat.condition);
        let if_body = self.compile_block(&if_stat.body);
        if let Some(else_node) = &if_stat.else_ {
            match else_node.as_ref() {
                Else::Else(else_body) => {
                    let else_body = self.compile_block(else_body);
                    format!("if {condition} then{if_body}else{else_body}end")
                }
                Else::ElseIf(else_if_stat) => {
                    let else_if_stat = self.compile_if_stat(else_if_stat);
                    format!("if {condition} then{if_body}else{else_if_stat}")
                }
            }
        } else {
            format!("if {condition} then{if_body}end",)
        }
    }

    fn compile_while_stat(&self, while_stat: &WhileStat) -> String {
        let condition = self.compile_expr(while_stat.condition);
        let while_body = self.compile_block(&while_stat.body);
        format!("while {condition} do{while_body}end",)
    }

    fn compile_range_for(&self, range_for: &RangeFor) -> String {
        format!(
            "for {} = {}, {} do{}end",
            range_for.var,
            self.compile_expr(range_for.range.lhs),
            self.compile_expr(range_for.range.rhs),
            self.compile_block(&range_for.body)
        )
    }

    fn compile_keyval_for(&self, keyval_for: &KeyValFor) -> String {
        format!(
            "for {} in pairs({}) do{}end",
            keyval_for.names,
            self.compile_expr(keyval_for.iter),
            self.compile_block(&keyval_for.body)
        )
    }

    fn compile_expr(&self, expr: ExprRef) -> String {
        if let Some(jitted) = self.jit_expr(expr) {
            return jitted.to_owned();
        }

        match &self.pool[expr] {
            Expr::BinOp(binop) => format!(
                "({} {} {})",
                self.compile_expr(binop.lhs),
                binop.op.to_lua(),
                self.compile_expr(binop.rhs),
            ),
            Expr::UnOp(UnOp { op, val }) => op.to_string() + &self.compile_expr(*val),
            Expr::Paren(ParenExpr { val }) => format!("({})", self.compile_expr(*val)),
            Expr::Simple(simple_expr) => self.compile_simple_expr(simple_expr),
            Expr::Name(str) => (*str).to_owned(),
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
        result += &self.compile_block(&func_node.body);
        result += "end";
        result
    }

    fn compile_block(&self, block: &[Statement]) -> String {
        let mut result = String::new();
        self.indent();
        for statement in block {
            result += &self.newline();
            result += &self.compile_statement(statement);
        }
        self.dedent();
        result += &self.newline();
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
            format_to!(
                result,
                "{name}.new = function(_self{param_list}){newline}\t"
            );
            format_to!(result, "local self = {{}}{body}\t");
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

    fn compile_suffixed_name(&self, suffixed_expr: &SuffixedName) -> String {
        let mut result = suffixed_expr.name.to_owned();

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

    fn jit_expr(&self, expr: ExprRef) -> Option<&'src str> {
        match &self.pool[expr] {
            Expr::BinOp(bin_op) => match bin_op.op {
                OpKind::Neq | OpKind::And | OpKind::Or => None,
                _ => {
                    let lhs = self.jit_expr(bin_op.lhs)?;
                    let lhs_range = lhs.as_bytes().as_ptr_range();

                    let rhs = self.jit_expr(bin_op.rhs)?;
                    let rhs_range = rhs.as_bytes().as_ptr_range();

                    let start = std::cmp::min(lhs_range.start, rhs_range.start);
                    let end = std::cmp::max(lhs_range.end, rhs_range.end);

                    Some(unsafe {
                        str::from_utf8_unchecked(slice::from_raw_parts(
                            start,
                            end.offset_from(start) as usize,
                        ))
                    })
                }
            },
            Expr::UnOp(un_op) => {
                let val = self.jit_expr(un_op.val)?;
                let ptr = unsafe { val.as_ptr().sub(1) };
                Some(unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr, val.len() + 1)) })
            }
            Expr::Paren(paren_expr) => {
                let val = self.jit_expr(paren_expr.val)?;
                let ptr = unsafe { val.as_ptr().sub(1) };
                Some(unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr, val.len() + 2)) })
            }
            Expr::Simple(
                SimpleExpr::Num(s) | SimpleExpr::Str(s) | SimpleExpr::Bool(s) | SimpleExpr::Nil(s),
            )
            | Expr::Name(s) => Some(s),
            Expr::Simple(SimpleExpr::SuffixedExpr(suffixed_expr)) => {
                if suffixed_expr.suffixes.is_empty() {
                    self.jit_expr(suffixed_expr.val)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
