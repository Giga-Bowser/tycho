use std::{cell::Cell, slice, str};

use crate::{
    format_to,
    parser::{
        ast,
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

    pub fn transpile_stmt(&mut self, stmt: &ast::Stmt<'s>) {
        match stmt {
            ast::Stmt::Declare(decl) => self.transpile_decl(decl),
            ast::Stmt::MultiDecl(multi_decl) => self.transpile_multi_decl(multi_decl),
            ast::Stmt::MethodDecl(method_decl) => self.transpile_method_decl(method_decl),
            ast::Stmt::Assign(assign) => self.transpile_assign(assign),
            ast::Stmt::MultiAssign(multi_assign) => self.transpile_multi_assign(multi_assign),
            ast::Stmt::ExprStmt(suffixed_expr) => self.transpile_suffixed_expr(suffixed_expr),
            ast::Stmt::Block(block) => {
                self.result += "do";
                self.indent();
                for stmt in block {
                    self.result += &self.newline();
                    self.transpile_stmt(stmt);
                }
                self.dedent();
                self.result += &self.newline();
                self.result += "end";
            }
            ast::Stmt::Return(ast::ReturnStmt { vals, .. }) => {
                self.result += "return";
                if !vals.is_empty() {
                    self.result.push(' ');

                    self.expr_list(vals, ", ");
                }
            }
            ast::Stmt::Break => self.result += "break",
            ast::Stmt::IfStmt(if_stmt) => self.transpile_if_stmt(if_stmt),
            ast::Stmt::WhileStmt(while_stmt) => self.transpile_while_stmt(while_stmt),
            ast::Stmt::RangeFor(range_for) => self.transpile_range_for(range_for),
            ast::Stmt::KeyValFor(keyval_for) => self.transpile_keyval_for(keyval_for),
            ast::Stmt::StructDecl(struct_decl) => self.transpile_struct_decl(struct_decl),
        }
    }

    fn transpile_decl(&mut self, decl: &ast::Declare<'s>) {
        let Some(val) = decl.val else {
            // no val is assigned, this means this statement is only useful for type checking
            format_to!(self.result, "local {}", decl.lhs.name.to_str(self.source));
            return;
        };

        if decl.lhs.suffixes.is_empty() {
            if let ast::Expr::Simple(ast::SimpleExpr::FuncNode(func_node)) = &self.pool[val] {
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

    fn transpile_multi_decl(&mut self, multi_decl: &ast::MultiDecl<'s>) {
        let lhs_result = multi_decl
            .lhs_arr
            .iter()
            .map(|it| it.to_str(self.source))
            .collect::<Vec<&'s str>>()
            .join(", ");

        format_to!(self.result, "local {lhs_result} = ");
        self.expr_list(&multi_decl.rhs_arr, ", ");
    }

    fn transpile_method_decl(&mut self, method_decl: &ast::MethodDecl<'s>) {
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

        for param in &method_decl.func.ty.params {
            self.result += ", ";
            self.result += param.name.to_str(self.source);
        }

        self.result.push(')');
        self.transpile_block(&method_decl.func.body);
        self.result.push_str("end");
    }

    fn transpile_assign(&mut self, assign: &ast::Assign<'s>) {
        self.transpile_suffixed_name(assign.lhs.as_ref());
        self.result += " = ";
        self.transpile_expr(assign.rhs);
    }

    fn transpile_multi_assign(&mut self, multi_assign: &ast::MultiAssign<'s>) {
        self.suffixed_expr_list(&multi_assign.lhs_arr, ", ");
        self.result += " = ";
        self.expr_list(&multi_assign.rhs_arr, ", ");
    }

    fn transpile_if_stmt(&mut self, if_stmt: &ast::IfStmt<'s>) {
        self.result += "if ";
        self.transpile_expr(if_stmt.condition);
        self.result += " then";
        self.transpile_block(&if_stmt.body);

        if let Some(else_node) = &if_stmt.else_ {
            self.result += "else";
            match else_node.as_ref() {
                ast::ElseBranch::Else(else_body) => {
                    self.transpile_block(else_body);
                    self.result += "end";
                }
                ast::ElseBranch::ElseIf(else_if_stmt) => {
                    self.transpile_if_stmt(else_if_stmt);
                }
            }
        } else {
            self.result += "end";
        }
    }

    fn transpile_while_stmt(&mut self, while_stmt: &ast::WhileStmt<'s>) {
        self.result += "while ";
        self.transpile_expr(while_stmt.condition);
        self.result += " do";
        self.transpile_block(&while_stmt.body);
        self.result += "end";
    }

    fn transpile_range_for(&mut self, range_for: &ast::RangeFor<'s>) {
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

    fn transpile_keyval_for(&mut self, keyval_for: &ast::KeyValFor<'s>) {
        format_to!(
            self.result,
            "for {}, {} in pairs(",
            keyval_for.key_name.to_str(self.source),
            keyval_for.val_name.to_str(self.source)
        );
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
            ast::Expr::BinOp(binop) => {
                self.result.push('(');
                self.transpile_expr(binop.lhs);
                self.result.push(' ');
                self.result += binop.op.to_lua();
                self.result.push(' ');
                self.transpile_expr(binop.rhs);
                self.result.push(')');
            }
            ast::Expr::UnOp(unop) => {
                self.result += unop.op.into();
                self.transpile_expr(unop.val);
            }
            ast::Expr::Paren(paren_expr) => {
                self.result.push('(');
                self.transpile_expr(paren_expr.val);
                self.result.push(')');
            }
            ast::Expr::Simple(simple_expr) => self.transpile_simple_expr(simple_expr),
            ast::Expr::Name(str) => self.result += str.to_str(self.source),
        }
    }

    fn transpile_simple_expr(&mut self, simple_expr: &ast::SimpleExpr<'s>) {
        match simple_expr {
            ast::SimpleExpr::Num(str)
            | ast::SimpleExpr::Str(str)
            | ast::SimpleExpr::Bool(str)
            | ast::SimpleExpr::Nil(str) => self.result += str.to_str(self.source),
            ast::SimpleExpr::FuncNode(func_node) => self.transpile_func(func_node),
            ast::SimpleExpr::TableNode(table_node) => self.transpile_table(table_node),
            ast::SimpleExpr::SuffixedExpr(suffixed_expr) => {
                self.transpile_suffixed_expr(suffixed_expr);
            }
        }
    }

    fn transpile_func(&mut self, func_node: &ast::FuncNode<'s>) {
        self.result += "function(";
        let params = &func_node.ty.params;

        if !params.is_empty() {
            self.result += params[0].name.to_str(self.source);

            for param in &params[1..] {
                self.result += ", ";
                self.result += param.name.to_str(self.source);
            }
        }

        self.result.push(')');
        self.transpile_block(&func_node.body);
        self.result += "end";
    }

    fn transpile_local_func(&mut self, func_node: &ast::FuncNode<'s>, name: &str) {
        format_to!(self.result, "local function {name}(");
        let params = &func_node.ty.params;

        if !params.is_empty() {
            self.result += params[0].name.to_str(self.source);

            for param in &params[1..] {
                self.result += ", ";
                self.result += param.name.to_str(self.source);
            }
        }

        self.result.push(')');
        self.transpile_block(&func_node.body);
        self.result += "end";
    }

    fn transpile_block(&mut self, block: &[ast::Stmt<'s>]) {
        self.indent();
        for stmt in block {
            self.result += &self.newline();
            self.transpile_stmt(stmt);
        }
        self.dedent();
        self.result += &self.newline();
    }

    fn transpile_struct_decl(&mut self, struct_decl: &ast::StructDecl<'s>) {
        let newline = self.newline();
        let name = struct_decl.name.to_str(self.source);
        format_to!(
            self.result,
            "local {name} = {{}}{newline}{name}.__index = {name}"
        );

        if let Some(constructor) = &struct_decl.constructor {
            self.result += &newline;
            format_to!(self.result, "{name}.new = function(_self");
            for param in &constructor.ty.params {
                self.result.push_str(", ");
                self.result += param.name.to_str(self.source);
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

    fn transpile_table(&mut self, table_node: &ast::TableNode<'s>) {
        self.result += "{";
        self.result += &self.newline();
        for field in &table_node.fields {
            self.result.push('\t');

            match field {
                ast::FieldNode::Field { key, val } => {
                    self.result += key.to_str(self.source);
                    self.result += " = ";
                    self.transpile_expr(*val);
                }
                ast::FieldNode::ExprField { key, val } => {
                    self.result.push('[');
                    self.transpile_expr(*key);
                    self.result += "] = ";
                    self.transpile_expr(*val);
                }
                ast::FieldNode::ValField { val } => self.transpile_expr(*val),
            };

            self.result.push(',');

            self.result += &self.newline();
        }
        self.result.push('}');
    }

    fn transpile_suffixed_expr(&mut self, suffixed_expr: &ast::SuffixedExpr<'s>) {
        self.transpile_expr(suffixed_expr.val);

        for suffix in &suffixed_expr.suffixes {
            self.transpile_suffix(suffix);
        }
    }

    fn transpile_suffixed_name(&mut self, suffixed_expr: &ast::SuffixedName<'s>) {
        self.result += suffixed_expr.name.to_str(self.source);

        for suffix in &suffixed_expr.suffixes {
            self.transpile_suffix(suffix);
        }
    }

    fn transpile_suffix(&mut self, suffix: &ast::Suffix<'s>) {
        match suffix {
            ast::Suffix::Method(ast::Method {
                method_name: name,
                args,
            }) => {
                format_to!(self.result, ":{}(", name.to_str(self.source));
                self.expr_list(args, ", ");
                self.result.push(')');
            }
            ast::Suffix::Call(ast::Call { args }) => {
                format_to!(self.result, "(");
                self.expr_list(args, ", ");
                self.result.push(')');
            }
            ast::Suffix::Access(ast::Access { field_name }) => {
                format_to!(self.result, ".{}", field_name.to_str(self.source));
            }
            ast::Suffix::Index(ast::Index { key, .. }) => {
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
            ast::Expr::BinOp(bin_op) => match bin_op.op {
                ast::OpKind::Neq | ast::OpKind::And | ast::OpKind::Or => None,
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
            ast::Expr::UnOp(un_op) => {
                let val = self.jit_expr(un_op.val)?;
                let ptr = unsafe { val.as_ptr().sub(1) };
                Some(unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr, val.len() + 1)) })
            }
            ast::Expr::Paren(paren_expr) => {
                let val = self.jit_expr(paren_expr.val)?;
                let ptr = unsafe { val.as_ptr().sub(1) };
                Some(unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr, val.len() + 2)) })
            }
            ast::Expr::Simple(
                ast::SimpleExpr::Num(s)
                | ast::SimpleExpr::Str(s)
                | ast::SimpleExpr::Bool(s)
                | ast::SimpleExpr::Nil(s),
            )
            | ast::Expr::Name(s) => Some(s.to_str(self.source)),
            ast::Expr::Simple(ast::SimpleExpr::SuffixedExpr(suffixed_expr)) => {
                if suffixed_expr.suffixes.is_empty() {
                    self.jit_expr(suffixed_expr.val)
                } else {
                    None
                }
            }
            ast::Expr::Simple(_) => None,
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

    fn suffixed_expr_list(&mut self, exprs: &[ast::SuffixedExpr<'s>], sep: &'static str) {
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
