use std::{cell::Cell, slice, str};

use crate::{
    format_to,
    parser::{
        ast::*,
        pool::{ExprPool, ExprRef},
    },
};

#[derive(Debug)]
pub struct Transpiler<'s, 'pool> {
    pub pool: &'pool ExprPool<'s>,
    pub result: String,
    pub source: &'s str,
    indent: Cell<usize>,
}

impl<'s, 'pool> Transpiler<'s, 'pool> {
    pub fn new(pool: &'pool ExprPool<'s>, source: &'s str) -> Self {
        Self {
            pool,
            result: "require(\"lualib.tycho\")\n".to_owned(),
            source,
            indent: 0.into(),
        }
    }

    pub fn transpile_statement(&mut self, statement: &Statement<'s>) {
        match statement {
            Statement::Declare(decl) => self.transpile_decl(decl),
            Statement::MultiDecl(multi_decl) => self.transpile_multi_decl(multi_decl),
            Statement::MethodDecl(method_decl) => self.transpile_method_decl(method_decl),
            Statement::Assign(assign) => self.transpile_assign(assign),
            Statement::MultiAssign(multi_assign) => self.transpile_multi_assign(multi_assign),
            Statement::ExprStat(suffixed_expr) => self.transpile_suffixed_expr(suffixed_expr),
            Statement::Block(block) => {
                self.result += "do";
                self.indent();
                for stmt in block {
                    self.result += &self.newline();
                    self.transpile_statement(stmt);
                }
                self.dedent();
                self.result += &self.newline();
                self.result += "end";
            }
            Statement::Return(return_exprs) => {
                self.result += "return";
                if !return_exprs.is_empty() {
                    self.result.push(' ');

                    self.expr_list(return_exprs, ", ");
                }
            }
            Statement::Break => self.result += "break",
            Statement::IfStat(if_stat) => self.transpile_if_stat(if_stat),
            Statement::WhileStat(while_stat) => self.transpile_while_stat(while_stat),
            Statement::RangeFor(range_for) => self.transpile_range_for(range_for),
            Statement::KeyValFor(keyval_for) => self.transpile_keyval_for(keyval_for),
            Statement::StructDecl(struct_decl) => self.transpile_struct_decl(struct_decl),
        }
    }

    fn transpile_decl(&mut self, decl: &Declare<'s>) {
        let Some(val) = decl.val else {
            // no val is assigned, this means this statement is only useful for type checking
            format_to!(self.result, "local {}", decl.lhs.name.to_str(self.source));
            return;
        };

        if decl.lhs.suffixes.is_empty() {
            if let Expr::Simple(SimpleExpr::FuncNode(func_node)) = &self.pool[val] {
                self.transpile_local_func(func_node, decl.lhs.name.to_str(self.source));
            } else {
                format_to!(
                    self.result,
                    "local {} = ",
                    decl.lhs.name.to_str(self.source)
                );
                self.transpile_expr(val);
            }
            return;
        }

        self.result += decl.lhs.name.to_str(self.source);

        for suffix in &decl.lhs.suffixes {
            self.transpile_suffix(suffix);
        }

        self.result += " = ";
        self.transpile_expr(val);
    }

    fn transpile_multi_decl(&mut self, multi_decl: &MultiDecl<'s>) {
        let lhs_result = multi_decl
            .lhs_arr
            .iter()
            .map(|it| it.to_str(self.source))
            .collect::<Vec<&'s str>>()
            .join(", ");

        format_to!(self.result, "local {lhs_result} = ");
        self.expr_list(&multi_decl.rhs_arr, ", ");
    }

    fn transpile_method_decl(&mut self, method_decl: &MethodDecl<'s>) {
        // let Some(val) = method_decl.val else {
        //     // no val is assigned, this means this statement is only useful for type checking
        //     // we can just return an empty string
        //     self.result += &"".to_owned();
        //     return
        // };

        format_to!(
            self.result,
            "{}.{} = function(self",
            method_decl.struct_name.to_str(self.source),
            method_decl.method_name.to_str(self.source)
        );

        for (param_name, _) in &method_decl.func.type_.params {
            self.result += ", ";
            self.result += param_name;
        }

        self.result.push(')');
        self.transpile_block(&method_decl.func.body);
        self.result.push_str("end");
    }

    fn transpile_assign(&mut self, assign: &Assign<'s>) {
        self.transpile_suffixed_name(assign.lhs.as_ref());
        self.result += " = ";
        self.transpile_expr(assign.rhs);
    }

    fn transpile_multi_assign(&mut self, multi_assign: &MultiAssign<'s>) {
        self.suffixed_expr_list(&multi_assign.lhs_arr, ", ");
        self.result += " = ";
        self.expr_list(&multi_assign.rhs_arr, ", ");
    }

    fn transpile_if_stat(&mut self, if_stat: &IfStat<'s>) {
        self.result += "if ";
        self.transpile_expr(if_stat.condition);
        self.result += " then";
        self.transpile_block(&if_stat.body);

        if let Some(else_node) = &if_stat.else_ {
            self.result += "else";
            match else_node.as_ref() {
                ElseBranch::Else(else_body) => {
                    self.transpile_block(else_body);
                    self.result += "end";
                }
                ElseBranch::ElseIf(else_if_stat) => {
                    self.transpile_if_stat(else_if_stat);
                }
            }
        } else {
            self.result += "end";
        }
    }

    fn transpile_while_stat(&mut self, while_stat: &WhileStat<'s>) {
        self.result += "while ";
        self.transpile_expr(while_stat.condition);
        self.result += " do";
        self.transpile_block(&while_stat.body);
        self.result += "end";
    }

    fn transpile_range_for(&mut self, range_for: &RangeFor<'s>) {
        self.result += "for ";
        self.result += range_for.var.to_str(self.source);
        self.result += " = ";
        self.transpile_expr(range_for.range.lhs);
        self.result += ", ";
        self.transpile_expr(range_for.range.rhs);
        self.result += " do";
        self.transpile_block(&range_for.body);
        self.result += "end";
    }

    fn transpile_keyval_for(&mut self, keyval_for: &KeyValFor<'s>) {
        self.result += "for ";
        self.result += keyval_for.names.to_str(self.source);
        self.result += " in pairs(";
        self.transpile_expr(keyval_for.iter);
        self.result += ") do";
        self.transpile_block(&keyval_for.body);
        self.result += "end";
    }

    fn transpile_expr(&mut self, expr: ExprRef) {
        if let Some(jitted) = self.jit_expr(expr) {
            self.result += jitted;
            return;
        }

        match &self.pool[expr] {
            Expr::BinOp(binop) => {
                self.result.push('(');
                self.transpile_expr(binop.lhs);
                self.result.push(' ');
                self.result += binop.op.to_lua();
                self.result.push(' ');
                self.transpile_expr(binop.rhs);
                self.result.push(')');
            }
            Expr::UnOp(UnOp { op, val }) => {
                self.result += op.into();
                self.transpile_expr(*val);
            }
            Expr::Paren(ParenExpr { val }) => {
                self.result.push('(');
                self.transpile_expr(*val);
                self.result.push(')');
            }
            Expr::Simple(simple_expr) => self.transpile_simple_expr(simple_expr),
            Expr::Name(str) => self.result += str.to_str(self.source),
        }
    }

    fn transpile_simple_expr(&mut self, simple_expr: &SimpleExpr<'s>) {
        match simple_expr {
            SimpleExpr::Num(str)
            | SimpleExpr::Str(str)
            | SimpleExpr::Bool(str)
            | SimpleExpr::Nil(str) => self.result += str.to_str(self.source),
            SimpleExpr::FuncNode(func_node) => self.transpile_func(func_node),
            SimpleExpr::TableNode(table_node) => self.transpile_table(table_node),
            SimpleExpr::SuffixedExpr(suffixed_expr) => self.transpile_suffixed_expr(suffixed_expr),
        }
    }

    fn transpile_func(&mut self, func_node: &FuncNode<'s>) {
        self.result += "function(";
        let params = &func_node.type_.params;

        if !params.is_empty() {
            self.result += params[0].0;

            for param in &params[1..] {
                self.result += ", ";
                self.result += param.0;
            }
        }

        self.result.push(')');
        self.transpile_block(&func_node.body);
        self.result += "end";
    }

    fn transpile_local_func(&mut self, func_node: &FuncNode<'s>, name: &str) {
        format_to!(self.result, "local function {name}(");
        let params = &func_node.type_.params;

        if !params.is_empty() {
            self.result += params[0].0;

            for param in &params[1..] {
                self.result += ", ";
                self.result += param.0;
            }
        }

        self.result.push(')');
        self.transpile_block(&func_node.body);
        self.result += "end";
    }

    fn transpile_block(&mut self, block: &[Statement<'s>]) {
        self.indent();
        for statement in block {
            self.result += &self.newline();
            self.transpile_statement(statement);
        }
        self.dedent();
        self.result += &self.newline();
    }

    fn transpile_struct_decl(&mut self, struct_decl: &StructDecl<'s>) {
        let newline = self.newline();
        let name = struct_decl.name.to_str(self.source);
        format_to!(
            self.result,
            "local {name} = {{}}{newline}{name}.__index = {name}"
        );

        if let Some(constructor) = &struct_decl.constructor {
            self.result += &newline;
            format_to!(self.result, "{name}.new = function(_self");
            for (param_name, _) in &constructor.type_.params {
                self.result.push_str(", ");
                self.result += param_name;
            }
            format_to!(self.result, "){newline}\t");
            self.result += "local self = {}";
            self.transpile_block(&constructor.body);
            self.result.push('\t');
            format_to!(self.result, "setmetatable(self, _self){newline}\t");
            format_to!(self.result, "_self.__index = _self{newline}\t");
            format_to!(self.result, "return self{newline}end");
        }
    }

    fn transpile_table(&mut self, table_node: &TableNode<'s>) {
        self.result += "{";
        self.result += &self.newline();
        for field in &table_node.fields {
            self.result.push('\t');

            match field {
                FieldNode::Field { key, val } => {
                    self.result += key.to_str(self.source);
                    self.result += " = ";
                    self.transpile_expr(*val);
                }
                FieldNode::ExprField { key, val } => {
                    self.result.push('[');
                    self.transpile_expr(*key);
                    self.result += "] = ";
                    self.transpile_expr(*val);
                }
                FieldNode::ValField { val } => self.transpile_expr(*val),
            };

            self.result.push(',');

            self.result += &self.newline();
        }
        self.result.push('}');
    }

    fn transpile_suffixed_expr(&mut self, suffixed_expr: &SuffixedExpr<'s>) {
        self.transpile_expr(suffixed_expr.val);

        for suffix in &suffixed_expr.suffixes {
            self.transpile_suffix(suffix);
        }
    }

    fn transpile_suffixed_name(&mut self, suffixed_expr: &SuffixedName<'s>) {
        self.result += suffixed_expr.name.to_str(self.source);

        for suffix in &suffixed_expr.suffixes {
            self.transpile_suffix(suffix);
        }
    }

    fn transpile_suffix(&mut self, suffix: &Suffix<'s>) {
        match suffix {
            Suffix::Method(Method {
                method_name: name,
                args,
            }) => {
                format_to!(self.result, ":{}(", name.to_str(self.source));
                self.expr_list(args, ", ");
                self.result.push(')');
            }
            Suffix::Call(Call { args }) => {
                format_to!(self.result, "(");
                self.expr_list(args, ", ");
                self.result.push(')');
            }
            Suffix::Access(Access { field_name }) => {
                format_to!(self.result, ".{}", field_name.to_str(self.source));
            }
            Suffix::Index(Index { key }) => {
                self.result.push('[');
                self.transpile_expr(*key);
                self.result.push(']');
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

    fn jit_expr(&self, expr: ExprRef) -> Option<&'s str> {
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
            | Expr::Name(s) => Some(s.to_str(self.source)),
            Expr::Simple(SimpleExpr::SuffixedExpr(suffixed_expr)) => {
                if suffixed_expr.suffixes.is_empty() {
                    self.jit_expr(suffixed_expr.val)
                } else {
                    None
                }
            }
            Expr::Simple(_) => None,
        }
    }

    fn expr_list(&mut self, exprs: &[ExprRef], sep: &'static str) {
        if exprs.is_empty() {
            return;
        }

        self.transpile_expr(exprs[0]);

        for expr in &exprs[1..] {
            self.result += sep;
            self.transpile_expr(*expr);
        }
    }

    fn suffixed_expr_list(&mut self, exprs: &[SuffixedExpr<'s>], sep: &'static str) {
        if exprs.is_empty() {
            return;
        }

        self.transpile_suffixed_expr(&exprs[0]);

        for expr in &exprs[1..] {
            self.result += sep;
            self.transpile_suffixed_expr(expr);
        }
    }
}
