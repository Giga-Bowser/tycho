use std::{cell::Cell, slice, str};

use crate::{
    format_to,
    parser::{
        ast::*,
        pool::{ExprPool, ExprRef},
    },
};

#[derive(Debug)]
pub struct Compiler<'src, 'pool> {
    pub pool: &'pool ExprPool<'src>,
    pub result: String,
    indent: Cell<usize>,
}

impl<'src, 'pool> Compiler<'src, 'pool> {
    pub fn new(pool: &'pool ExprPool<'src>) -> Self {
        Self {
            pool,
            result: "require(\"lualib.tycho\")\n".to_owned(),
            indent: 0.into(),
        }
    }

    pub fn compile_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Declare(decl) => self.compile_decl(decl),
            Statement::MultiDecl(multi_decl) => self.compile_multi_decl(multi_decl),
            Statement::MethodDecl(method_decl) => self.compile_method_decl(method_decl),
            Statement::Assign(assign) => self.compile_assign(assign),
            Statement::MultiAssign(multi_assign) => self.compile_multi_assign(multi_assign),
            Statement::ExprStat(suffixed_expr) => self.compile_suffixed_expr(suffixed_expr),
            Statement::Block(statements) => {
                self.result += "do";
                self.indent();
                for stat in statements {
                    self.result += &self.newline();
                    self.compile_statement(stat);
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
            Statement::IfStat(if_stat) => self.compile_if_stat(if_stat),
            Statement::WhileStat(while_stat) => self.compile_while_stat(while_stat),
            Statement::RangeFor(range_for) => self.compile_range_for(range_for),
            Statement::KeyValFor(keyval_for) => self.compile_keyval_for(keyval_for),
        }
    }

    fn compile_decl(&mut self, decl: &Declare) {
        let Some(val) = decl.val else {
            // no val is assigned, this means this statement is only useful for type checking
            return;
        };

        if decl.lhs.suffixes.is_empty() {
            if let Expr::Simple(SimpleExpr::FuncNode(func)) = &self.pool[val] {
                let name = decl.lhs.name;
                format_to!(self.result, "local {name}{}{name} = ", self.newline());
                self.compile_func(func);
            } else {
                format_to!(self.result, "local {} = ", decl.lhs.name);
                self.compile_expr(val);
            }
            return;
        }

        self.result += decl.lhs.name;

        for suffix in &decl.lhs.suffixes {
            self.compile_suffix(suffix);
        }

        self.result += " = ";
        self.compile_expr(val);
    }

    fn compile_multi_decl(&mut self, multi_decl: &MultiDecl) {
        let lhs_result = multi_decl.lhs_arr.join(", ");

        format_to!(self.result, "local {lhs_result} = ");
        self.expr_list(&multi_decl.rhs_arr, ", ");
    }

    fn compile_method_decl(&mut self, method_decl: &MethodDecl) {
        // let Some(val) = method_decl.val else {
        //     // no val is assigned, this means this statement is only useful for type checking
        //     // we can just return an empty string
        //     self.result += &"".to_owned();
        //     return
        // };

        format_to!(
            self.result,
            "{}.{} = function(self",
            method_decl.struct_name,
            method_decl.method_name
        );

        for (param_name, _) in &method_decl.func.type_.params {
            self.result += ", ";
            self.result += param_name;
        }

        self.result.push(')');
        self.compile_block(&method_decl.func.body);
        self.result.push_str("end");
    }

    fn compile_assign(&mut self, assign: &Assign) {
        self.compile_suffixed_name(assign.lhs.as_ref());
        self.result += " = ";
        self.compile_expr(assign.rhs);
    }

    fn compile_multi_assign(&mut self, multi_assign: &MultiAssign) {
        self.suffixed_expr_list(&multi_assign.lhs_arr, ", ");
        self.result += " = ";
        self.expr_list(&multi_assign.rhs_arr, ", ");
    }

    fn compile_if_stat(&mut self, if_stat: &IfStat) {
        self.result += "if ";
        self.compile_expr(if_stat.condition);
        self.result += " then";
        self.compile_block(&if_stat.body);

        if let Some(else_node) = &if_stat.else_ {
            self.result += "else";
            match else_node.as_ref() {
                ElseBranch::Else(else_body) => {
                    self.compile_block(else_body);
                    self.result += "end";
                }
                ElseBranch::ElseIf(else_if_stat) => {
                    self.compile_if_stat(else_if_stat);
                }
            }
        } else {
            self.result += "end";
        }
    }

    fn compile_while_stat(&mut self, while_stat: &WhileStat) {
        self.result += "while ";
        self.compile_expr(while_stat.condition);
        self.result += " do";
        self.compile_block(&while_stat.body);
        self.result += "end";
    }

    fn compile_range_for(&mut self, range_for: &RangeFor) {
        self.result += "for ";
        self.result += range_for.var;
        self.result += " = ";
        self.compile_expr(range_for.range.lhs);
        self.result += ", ";
        self.compile_expr(range_for.range.rhs);
        self.result += " do";
        self.compile_block(&range_for.body);
        self.result += "end";
    }

    fn compile_keyval_for(&mut self, keyval_for: &KeyValFor) {
        self.result += "for ";
        self.result += keyval_for.names;
        self.result += " in pairs(";
        self.compile_expr(keyval_for.iter);
        self.result += ") do";
        self.compile_block(&keyval_for.body);
        self.result += "end";
    }

    fn compile_expr(&mut self, expr: ExprRef) {
        if let Some(jitted) = self.jit_expr(expr) {
            self.result += jitted;
            return;
        }

        match &self.pool[expr] {
            Expr::BinOp(binop) => {
                self.result.push('(');
                self.compile_expr(binop.lhs);
                self.result.push(' ');
                self.result += binop.op.to_lua();
                self.result.push(' ');
                self.compile_expr(binop.rhs);
                self.result.push(')');
            }
            Expr::UnOp(UnOp { op, val }) => {
                self.result += op.into();
                self.compile_expr(*val);
            }
            Expr::Paren(ParenExpr { val }) => {
                self.result.push('(');
                self.compile_expr(*val);
                self.result.push(')');
            }
            Expr::Simple(simple_expr) => self.compile_simple_expr(simple_expr),
            Expr::Name(str) => self.result += str,
        }
    }

    fn compile_simple_expr(&mut self, simple_expr: &SimpleExpr) {
        match simple_expr {
            SimpleExpr::Num(str)
            | SimpleExpr::Str(str)
            | SimpleExpr::Bool(str)
            | SimpleExpr::Nil(str) => self.result += str,
            SimpleExpr::FuncNode(func_node) => self.compile_func(func_node),
            SimpleExpr::TableNode(table_node) => self.compile_table(table_node),
            SimpleExpr::StructNode(struct_node) => self.compile_struct(struct_node),
            SimpleExpr::SuffixedExpr(suffixed_expr) => self.compile_suffixed_expr(suffixed_expr),
        }
    }

    fn compile_func(&mut self, func_node: &FuncNode) {
        self.result += "function(";
        let params = &func_node.type_.params;

        if !params.is_empty() {
            self.result += &params[0].0;

            for param in &params[1..] {
                self.result += ", ";
                self.result += &param.0;
            }
        }

        self.result.push(')');
        self.compile_block(&func_node.body);
        self.result += "end";
    }

    fn compile_block(&mut self, block: &[Statement]) {
        self.indent();
        for statement in block {
            self.result += &self.newline();
            self.compile_statement(statement);
        }
        self.dedent();
        self.result += &self.newline();
    }

    fn compile_struct(&mut self, struct_node: &StructNode) {
        let newline = self.newline();
        let name = struct_node.name.unwrap_or("_");
        format_to!(self.result, "{{}}{newline}{name}.__index = {name}");

        if let Some(constructor) = &struct_node.constructor {
            self.result += &newline;
            format_to!(self.result, "{name}.new = function(_self");
            for (param_name, _) in &constructor.type_.params {
                self.result.push_str(", ");
                self.result += param_name;
            }
            format_to!(self.result, "){newline}\t");
            self.result += "local self = {}";
            self.compile_block(&constructor.body);
            self.result.push('\t');
            format_to!(self.result, "setmetatable(self, _self){newline}\t");
            format_to!(self.result, "_self.__index = _self{newline}\t");
            format_to!(self.result, "return self{newline}end");
        }
    }

    fn compile_table(&mut self, table_node: &TableNode) {
        self.result += "{";
        self.result += &self.newline();
        for field in &table_node.fields {
            self.result.push('\t');

            match field {
                FieldNode::Field { key, val } => {
                    self.result += key;
                    self.result += " = ";
                    self.compile_expr(*val);
                }
                FieldNode::ExprField { key, val } => {
                    self.result.push('[');
                    self.compile_expr(*key);
                    self.result += "] = ";
                    self.compile_expr(*val);
                }
                FieldNode::ValField { val } => self.compile_expr(*val),
            };

            self.result += &self.newline();
        }
        self.result.push('}');
    }

    fn compile_suffixed_expr(&mut self, suffixed_expr: &SuffixedExpr) {
        self.compile_expr(suffixed_expr.val);

        for suffix in &suffixed_expr.suffixes {
            self.compile_suffix(suffix);
        }
    }

    fn compile_suffixed_name(&mut self, suffixed_expr: &SuffixedName) {
        self.result += suffixed_expr.name;

        for suffix in &suffixed_expr.suffixes {
            self.compile_suffix(suffix);
        }
    }

    fn compile_suffix(&mut self, suffix: &Suffix) {
        match suffix {
            Suffix::Method(Method {
                method_name: name,
                args,
            }) => {
                format_to!(self.result, ":{name}(");
                self.expr_list(args, ", ");
                self.result.push(')');
            }
            Suffix::Call(Call { args }) => {
                format_to!(self.result, "(");
                self.expr_list(args, ", ");
                self.result.push(')');
            }
            Suffix::Access(Access { field_name }) => {
                format_to!(self.result, ".{field_name}")
            }
            Suffix::Index(Index { key }) => {
                self.result.push('[');
                self.compile_expr(*key);
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

    fn expr_list(&mut self, exprs: &[usize], sep: &'static str) {
        if exprs.is_empty() {
            return;
        }

        self.compile_expr(exprs[0]);

        for expr in &exprs[1..] {
            self.result += sep;
            self.compile_expr(*expr);
        }
    }

    fn suffixed_expr_list(&mut self, exprs: &[SuffixedExpr], sep: &'static str) {
        if exprs.is_empty() {
            return;
        }

        self.compile_suffixed_expr(&exprs[0]);

        for expr in &exprs[1..] {
            self.result += sep;
            self.compile_suffixed_expr(expr);
        }
    }
}
