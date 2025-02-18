use crate::{
    luajit::{
        bytecode::{
            BCInstr, BCOp, BCPos, BCReg, GCConstant, Proto, ProtoFlags, TemplateTable, UVData,
            UVFlags, NO_JMP,
        },
        funcstate::{FuncScope, FuncState, ScopeFlags, VarIdx},
        utils::numlit,
        ExprDesc, ExprKind, TValue, VarFlags, VarInfo,
    },
    parser::{
        ast::*,
        pool::{ExprPool, ExprRef},
    },
    types,
};

const LJ_FR2: bool = true;

#[derive(Debug)]
pub struct LJCompiler<'src, 'pool> {
    pub pool: &'pool ExprPool<'src>,
    pub func_state: FuncState,
    prev_states: Vec<FuncState>,
    var_info: Vec<VarInfo>,
    pub protos: Vec<Proto>,
}

impl<'src, 'pool> LJCompiler<'src, 'pool> {
    pub fn new(pool: &'pool ExprPool<'src>) -> Self {
        let mut result = Self {
            pool,
            func_state: FuncState::top(),
            prev_states: Vec::new(),
            var_info: Vec::new(),
            protos: Vec::new(),
        };

        let func = result.func_state.const_str("require") as u16;
        let arg = result.func_state.const_str("lualib.tycho") as u16;
        result.bcemit(BCInstr::new_ad(BCOp::GGET, 0, func));
        result.bcemit(BCInstr::new_ad(BCOp::KSTR, if LJ_FR2 { 2 } else { 1 }, arg));
        result.bcemit(BCInstr::new_abc(BCOp::CALL, 0, 1, 2));

        result
    }
}

impl LJCompiler<'_, '_> {
    fn fs_init(&mut self) {
        self.prev_states.push(std::mem::take(&mut self.func_state));
    }

    pub fn fs_finish(&mut self) {
        let mut fs = std::mem::replace(
            &mut self.func_state,
            self.prev_states.pop().unwrap_or_else(FuncState::top),
        );
        if fs.bc.len() <= fs.last_target || !fs.bc.last().op().is_ret() {
            if fs.scope.flags.contains(ScopeFlags::UPVAL) {
                fs.bcemit(BCInstr::new_jmp(BCOp::UCLO, 0, 0));
            }
            fs.bcemit(BCInstr::new_ad(BCOp::RET0, 0, 1));
        }

        fs.scope.flags |= ScopeFlags::NOCLOSE;

        let FuncState {
            kt,
            num_kn,
            num_kgc,
            bc,
            num_params,
            flags,
            uv_map,
            frame_size,
            template_tables,
            ..
        } = fs;

        let bcins = bc.vec;

        let upvalue_data = uv_map
            .into_iter()
            .map(|var_idx| {
                let var = &self.var_info[var_idx as usize];
                let reg = var.slot;
                let flags = if var.flags.contains(VarFlags::VAR_RW) {
                    UVFlags::LOCAL
                } else {
                    UVFlags::LOCAL | UVFlags::IMMUTABLE
                };
                UVData { flags, reg }
            })
            .collect();

        let mut gc_constants = vec![GCConstant::Child; num_kgc];
        let mut number_constants = vec![0.0; num_kn];

        for (key, val) in kt {
            let kidx = val as usize;

            match key {
                // TValue::Int(i) => number_constants[kidx] = i as f64,
                // TValue::Uint(u) => number_constants[kidx] = u as f64,
                TValue::Number(f) => number_constants[kidx] = f,
                TValue::String(s) => {
                    gc_constants[num_kgc - kidx - 1] = GCConstant::Str(s.clone());
                }
                _ => unreachable!(),
            }
        }

        for (template, kidx) in template_tables {
            gc_constants[num_kgc - kidx - 1] = GCConstant::Table(template);
        }

        self.protos.push(Proto {
            flags: flags.intersection(ProtoFlags::DUMP),
            num_params,
            frame_size,
            instructions: bcins,
            upvalue_data,
            gc_constants,
            number_constants,
        });
    }

    pub fn fscope_begin(&mut self, flags: ScopeFlags) {
        let new_scope = FuncScope {
            vstart: self.var_info.len() as u32,
            nactvar: self.func_state.nactvar as u8,
            flags,
        };

        let old_scope = std::mem::replace(&mut self.func_state.scope, new_scope);
        self.func_state.prev_scopes.push(old_scope);

        debug_assert_eq!(
            self.func_state.free_reg, self.func_state.nactvar,
            "bad regalloc"
        );
    }

    pub fn fscope_end(&mut self) {
        let prev_scope = self.func_state.prev_scopes.pop().unwrap();
        let dead_scope = std::mem::replace(&mut self.func_state.scope, prev_scope);
        self.var_remove(dead_scope.nactvar as u32);
        self.func_state.free_reg = self.func_state.nactvar;
        debug_assert_eq!(
            dead_scope.nactvar as u32, self.func_state.nactvar,
            "bad regalloc"
        );
        if dead_scope.flags.contains(ScopeFlags::UPVAL)
            && !dead_scope.flags.contains(ScopeFlags::NOCLOSE)
        {
            self.bcemit(BCInstr::new_jmp(BCOp::UCLO, dead_scope.nactvar, 0));
        }

        if dead_scope.flags.contains(ScopeFlags::BREAK) {
            if dead_scope.flags.contains(ScopeFlags::LOOP) {
                let idx = self.func_state.gola_new(
                    &mut self.var_info,
                    VarFlags::LABEL,
                    self.func_state.bc.len(),
                );
                self.func_state
                    .gola_resolve(&mut self.var_info, &dead_scope, idx);
            } else {
                self.func_state.gola_fixup(&mut self.var_info, &dead_scope);
            }
        }
    }
}

impl<'src> LJCompiler<'src, '_> {
    pub fn compile_statement(&mut self, statement: &Statement<'src>) {
        match statement {
            Statement::Declare(decl) => self.compile_decl(decl),
            Statement::MultiDecl(multi_decl) => self.compile_multi_decl(multi_decl),
            Statement::MethodDecl(method_decl) => self.compile_method_decl(method_decl),
            Statement::Assign(assign) => self.compile_assign(assign),
            Statement::MultiAssign(multi_assign) => self.compile_multi_assign(multi_assign),
            Statement::ExprStat(suffixed_expr) => {
                let v = self.compile_suffixed_expr(suffixed_expr);
                match v.kind {
                    ExprKind::Call {
                        instr_idx: instr_pc,
                        base: _,
                    } => {
                        self.func_state.bc[instr_pc].set_b(1);
                    }
                    _ => panic!("non-call suffix in ExprStat: {suffixed_expr:?}"),
                }
            } // self.compile_suffixed_expr(suffixed_expr),
            Statement::Block(statements) => self.compile_block(statements),
            Statement::Return(return_exprs) => self.compile_return(return_exprs),
            Statement::Break => {
                self.func_state.scope.flags |= ScopeFlags::BREAK;
                let pc = self.func_state.bcemit_jmp();
                self.func_state
                    .gola_new(&mut self.var_info, VarFlags::GOTO, pc);
            }
            Statement::IfStat(if_stat) => self.compile_if_stat(if_stat),
            Statement::WhileStat(while_stat) => self.compile_while_stat(while_stat),
            Statement::RangeFor(range_for) => self.compile_range_for(range_for),
            Statement::KeyValFor(keyval_for) => self.compile_keyval_for(keyval_for),
            Statement::StructDecl(struct_decl) => self.compile_struct_decl(struct_decl),
        }
    }

    fn compile_decl(&mut self, decl: &Declare<'src>) {
        self.var_new(0, decl.lhs.name);

        let Some(val) = decl.val else {
            self.assign_adjust(1, 0, ExprDesc::new(ExprKind::Void));
            self.var_add(1);
            return;
        };

        if decl.lhs.suffixes.is_empty() {
            if let Expr::Simple(SimpleExpr::FuncNode(func)) = &self.pool[val] {
                let local_reg = self.func_state.free_reg;
                self.func_state.bcreg_reserve(1);
                self.var_add(1);
                let mut b = self.compile_func::<false>(func);
                // self.bcemit_store(val, b) without setting var as RW
                self.func_state.expr_free(&b);
                self.func_state.expr_toreg(&mut b, local_reg);
                self.var_get(self.func_state.nactvar - 1).startpc = self.func_state.bc.len();
            } else {
                let expr = self.compile_expr(val);
                self.assign_adjust(1, 1, expr);
                self.var_add(1);
            }

            return;
        }

        for _suffix in &decl.lhs.suffixes {
            // self.compile_suffix(suffix);
        }

        // self.compile_expr(val);
    }

    fn compile_multi_decl(&mut self, multi_decl: &MultiDecl<'src>) {
        for (idx, var_name) in multi_decl.lhs_arr.iter().enumerate() {
            self.var_new(idx as u32, var_name);
        }
        let nvars = multi_decl.lhs_arr.len() as u32;
        let nexprs = multi_decl.rhs_arr.len() as u32;

        let rhs_expr = if nexprs != 0 {
            self.expr_list(&multi_decl.rhs_arr)
        } else {
            ExprDesc::new(ExprKind::Void)
        };

        self.assign_adjust(nvars, nexprs, rhs_expr);
        self.var_add(nvars);
    }

    fn compile_method_decl(&mut self, method_decl: &MethodDecl<'src>) {
        let mut lhs = self.var_lookup(method_decl.struct_name);
        self.func_state.expr_toanyreg(&mut lhs);
        let mut key = ExprDesc::new(ExprKind::KString(method_decl.method_name));
        self.compile_index(&mut lhs, &mut key);
        let func = self.compile_func::<true>(method_decl.func.as_ref());

        self.bcemit_store(&lhs, func);
    }

    fn compile_assign(&mut self, assign: &Assign<'src>) {
        let lhs = self.compile_suffixed_name(assign.lhs.as_ref());
        let mut rhs = self.compile_expr(assign.rhs);
        if let ExprKind::Call { instr_idx, base } = rhs.kind {
            if let BCOp::VARG = self.func_state.bc[instr_idx].op() {
                self.func_state.free_reg -= 1;
                rhs.kind = ExprKind::Relocable { instr_idx };
            } else {
                rhs.kind = ExprKind::NonReloc { result_reg: base }
            }
        }
        self.bcemit_store(&lhs, rhs);
    }

    fn compile_multi_assign(&mut self, multi_assign: &MultiAssign<'src>) {
        let mut lhs_list: Vec<_> = multi_assign
            .lhs_arr
            .iter()
            .map(|it| self.compile_suffixed_expr(it))
            .collect();

        let mut last_expr = self.expr_list(&multi_assign.rhs_arr);

        let nvars = multi_assign.lhs_arr.len();
        let nexprs = multi_assign.rhs_arr.len();
        if nvars == nexprs {
            if let ExprKind::Call { instr_idx, base } = last_expr.kind {
                last_expr.kind = if self.func_state.bc[instr_idx].op() == BCOp::VARG {
                    self.func_state.free_reg -= 1;
                    ExprKind::Relocable { instr_idx }
                } else {
                    ExprKind::NonReloc { result_reg: base }
                }
            }
            self.bcemit_store(&lhs_list.pop().unwrap(), last_expr);
        } else {
            self.assign_adjust(nvars as u32, nexprs as u32, last_expr);
        }

        for lhs in lhs_list.into_iter().rev().skip(1) {
            let e = ExprDesc::new(ExprKind::NonReloc {
                result_reg: self.func_state.free_reg - 1,
            });

            self.bcemit_store(&lhs, e);
        }

        // self.suffixed_expr_list(&multi_assign.lhs_arr, ", ");

        // self.expr_list(&multi_assign.rhs_arr, ", ");
    }

    fn assign_adjust(
        &mut self,
        nvars: BCReg,
        nexprs: BCReg,
        mut expr: ExprDesc<'src>,
    ) -> ExprDesc<'src> {
        let mut extra = nvars as i32 - nexprs as i32;
        match expr.kind {
            ExprKind::Call { instr_idx, base: _ } => {
                // compensate for the VCALL
                extra += 1;
                extra = extra.max(0);
                self.func_state.bc[instr_idx].set_b(extra as u8 + 1);
                if extra > 1 {
                    self.func_state.bcreg_reserve(extra as BCReg - 1);
                }
            }
            ExprKind::Void => {
                if extra > 0 {
                    let reg = self.func_state.free_reg;
                    self.func_state.bcreg_reserve(extra as BCReg);
                    self.func_state.bcemit_nil(reg, extra as BCReg);
                }
            }
            _ => {
                self.func_state.expr_tonextreg(&mut expr);
                if extra > 0 {
                    let reg = self.func_state.free_reg;
                    self.func_state.bcreg_reserve(extra as BCReg);
                    self.func_state.bcemit_nil(reg, extra as BCReg);
                }
            }
        }

        if nexprs > nvars {
            self.func_state.free_reg -= nexprs - nvars; // Drop leftover regs
        }

        expr
    }

    fn compile_if_stat(&mut self, mut if_stat: &IfStat<'src>) {
        let mut flist = self.compile_if_body(if_stat);
        let mut escape_list = NO_JMP;

        let mut flattened = Vec::new();
        while let Some(ElseBranch::ElseIf(else_if_stat)) = if_stat.else_.as_ref().map(AsRef::as_ref)
        {
            flattened.push(else_if_stat);
            if_stat = else_if_stat;
        }

        for else_if_stat in flattened {
            let idx = self.func_state.bcemit_jmp();
            escape_list = self.func_state.jmp_append(escape_list, idx);
            self.func_state.jmp_tohere(flist);
            flist = self.compile_if_body(else_if_stat);
        }

        let else_branch = if_stat.else_.as_ref().map(AsRef::as_ref);
        if let Some(ElseBranch::Else(else_body)) = else_branch {
            let idx = self.func_state.bcemit_jmp();
            escape_list = self.func_state.jmp_append(escape_list, idx);
            self.func_state.jmp_tohere(flist);
            self.compile_block(else_body);
        } else {
            escape_list = self.func_state.jmp_append(escape_list, flist);
        }

        self.func_state.jmp_tohere(escape_list);
    }

    fn compile_if_body(&mut self, if_stat: &IfStat<'src>) -> BCPos {
        let condexit = {
            // expr_cond
            self.expr_cond(if_stat.condition)
        };

        self.compile_block(&if_stat.body);

        condexit
    }

    fn compile_while_stat(&mut self, while_stat: &WhileStat<'src>) {
        self.func_state.last_target = self.func_state.bc.len();
        let start = self.func_state.bc.len();
        let cond_exit = self.expr_cond(while_stat.condition);
        self.fscope_begin(ScopeFlags::LOOP);
        let loop_instr = self.bcemit(BCInstr::new_ad(
            BCOp::LOOP,
            self.func_state.nactvar as u8,
            0,
        ));
        self.compile_block(&while_stat.body);
        let jmp = self.func_state.bcemit_jmp();
        self.func_state.jmp_patch(jmp, start);
        self.fscope_end();
        self.func_state.jmp_tohere(cond_exit);
        self.func_state
            .jmp_patchins(loop_instr, self.func_state.bc.len());
    }

    fn expr_cond(&mut self, condition: ExprRef) -> u32 {
        let mut v = self.compile_expr(condition);
        if let ExprKind::KNil = v.kind {
            v.kind = ExprKind::KFalse;
        };

        self.func_state.bcemit_branch_t(&mut v);
        v.false_jumplist
    }

    fn compile_range_for(&mut self, range_for: &RangeFor<'src>) {
        const FORL_IDX: BCReg = 0;
        const FORL_STOP: BCReg = 1;
        const FORL_STEP: BCReg = 2;
        const FORL_EXT: BCReg = 3;
        self.fscope_begin(ScopeFlags::LOOP);

        let base = self.func_state.free_reg as u8;
        self.var_new(FORL_IDX, "(for index)");
        self.var_new(FORL_STOP, "(for limit)");
        self.var_new(FORL_STEP, "(for step)");

        self.var_new(FORL_EXT, range_for.var);
        let mut lhs = self.compile_expr(range_for.range.lhs);
        self.func_state.expr_tonextreg(&mut lhs);
        let mut rhs = self.compile_expr(range_for.range.rhs);
        self.func_state.expr_tonextreg(&mut rhs);
        self.bcemit(BCInstr::new_ad(
            BCOp::KSHORT,
            self.func_state.free_reg as u8,
            1,
        ));
        self.func_state.bcreg_reserve(1);
        self.var_add(3); // Hidden control variables
        let loop_idx = self.bcemit(BCInstr::new_jmp(BCOp::FORI, base, NO_JMP as i16));
        self.fscope_begin(ScopeFlags::empty());
        self.var_add(1);
        self.func_state.bcreg_reserve(1);
        self.compile_block(&range_for.body);
        self.fscope_end();
        let loop_end = self.bcemit(BCInstr::new_jmp(BCOp::FORL, base, NO_JMP as i16));
        self.func_state.jmp_patchins(loop_end, loop_idx + 1);
        self.func_state
            .jmp_patchins(loop_idx, self.func_state.bc.len());

        self.fscope_end();
    }

    fn compile_keyval_for(&mut self, keyval_for: &KeyValFor<'src>) {
        const FOR_GEN: &str = "(gen)";
        const FOR_STATE: &str = "(state)";
        const FOR_CTL: &str = "(ctl)";
        const NVARS: BCReg = 5;
        self.fscope_begin(ScopeFlags::LOOP);

        let base = self.func_state.free_reg + 3;

        self.var_new(0, FOR_GEN);
        self.var_new(1, FOR_STATE);
        self.var_new(2, FOR_CTL);

        // now because i'm evil, we have to retokenize the `key, val` string_view
        // but who cares.
        let names = keyval_for.names.as_bytes();

        let mut i = 0;
        while names[i] != b',' {
            i += 1;
        }

        let key_name = unsafe { std::str::from_utf8_unchecked(&names[0..i]) };

        // now we skip the comma
        i += 1;

        while names[i].is_ascii_whitespace() {
            i += 1;
        }

        let val_name = unsafe { std::str::from_utf8_unchecked(&names[i..]) };

        self.var_new(3, key_name);
        self.var_new(4, val_name);

        let call_expr = {
            let mut expr = ExprDesc::new(ExprKind::Global("pairs"));

            self.func_state.expr_tonextreg(&mut expr);
            if LJ_FR2 {
                self.func_state.bcreg_reserve(1);
            }

            expr = self.compile_args(&expr, &[keyval_for.iter]);

            expr
        };

        self.assign_adjust(3, 1, call_expr);

        // The iterator needs another 3 [4] slots (func [pc] | state ctl).
        self.func_state.bcreg_bump(3 + LJ_FR2 as u32);

        self.var_add(3);
        let loop_start = self.bcemit(BCInstr::new_jmp(BCOp::ISNEXT, base as u8, NO_JMP as i16));
        self.fscope_begin(ScopeFlags::empty());
        self.var_add(NVARS - 3);
        self.func_state.bcreg_reserve(NVARS - 3);
        self.compile_block(&keyval_for.body);
        self.fscope_end();

        self.func_state
            .jmp_patchins(loop_start, self.func_state.bc.len());
        self.bcemit(BCInstr::new_abc(
            BCOp::ITERN,
            base as u8,
            NVARS as u8 - 3 + 1,
            2 + 1,
        ));

        let loop_end = self.bcemit(BCInstr::new_jmp(BCOp::ITERL, base as u8, NO_JMP as i16));
        self.func_state.jmp_patchins(loop_end, loop_start + 1);
        self.fscope_end();
    }

    fn compile_expr(&mut self, expr: ExprRef) -> ExprDesc<'src> {
        match &self.pool[expr] {
            Expr::BinOp(binop) => self.compile_binop(binop),
            Expr::UnOp(unop) => self.compile_unop(unop),
            Expr::Paren(ParenExpr { val }) => self.compile_expr(*val),
            Expr::Simple(simple_expr) => self.compile_simple_expr(simple_expr),
            Expr::Name(name) => self.var_lookup(name),
        }
    }

    fn compile_binop(&mut self, binop: &BinOp) -> ExprDesc<'src> {
        let mut lhs = self.compile_expr(binop.lhs);
        // fixup left side
        match binop.op {
            OpKind::And => self.func_state.bcemit_branch_t(&mut lhs),
            OpKind::Or => self.func_state.bcemit_branch_f(&mut lhs),
            OpKind::Cat => {
                self.func_state.expr_tonextreg(&mut lhs);
            }
            OpKind::Equ | OpKind::Neq if !lhs.is_const() || lhs.has_jump() => {
                self.func_state.expr_toanyreg(&mut lhs);
            }
            _ if !lhs.is_number() || lhs.has_jump() => {
                self.func_state.expr_toanyreg(&mut lhs);
            }
            _ => (),
        }

        let mut rhs = self.compile_expr(binop.rhs);

        let expr = match binop.op {
            OpKind::Add | OpKind::Sub | OpKind::Mul | OpKind::Div | OpKind::Mod | OpKind::Pow => {
                self.compile_binop_arith(binop, lhs, rhs)
            }
            OpKind::Neq | OpKind::Equ | OpKind::Les | OpKind::Grq | OpKind::Leq | OpKind::Gre => {
                self.compile_binop_cmp(binop, lhs, rhs)
            }
            OpKind::Cat => {
                let ExprKind::NonReloc {
                    result_reg: lhs_reg,
                } = lhs.kind
                else {
                    unreachable!()
                };

                self.func_state.expr_toval(&mut rhs);
                let instr_idx = match rhs.kind {
                    ExprKind::Relocable { instr_idx }
                        if self.func_state.bc[instr_idx].op() == BCOp::CAT =>
                    {
                        debug_assert_eq!(
                            lhs_reg,
                            self.func_state.bc[instr_idx].b() as u32 - 1,
                            "bad CAT stack layout"
                        );
                        self.func_state.expr_free(&lhs);
                        self.func_state.bc[instr_idx].set_b(lhs_reg as u8);
                        instr_idx
                    }
                    _ => {
                        let rhs_reg = self.func_state.expr_tonextreg(&mut rhs);
                        self.func_state.expr_free(&rhs);
                        self.func_state.expr_free(&lhs);
                        self.bcemit(BCInstr::new_abc(BCOp::CAT, 0, lhs_reg as u8, rhs_reg as u8))
                    }
                };
                ExprDesc::new(ExprKind::Relocable { instr_idx })
            }
            OpKind::And => {
                debug_assert_eq!(lhs.true_jumplist, NO_JMP, "jump list not closed");
                self.func_state.expr_discharge(&mut rhs);
                rhs.false_jumplist = self
                    .func_state
                    .jmp_append(rhs.false_jumplist, lhs.false_jumplist);
                rhs
            }
            OpKind::Or => {
                debug_assert_eq!(lhs.false_jumplist, NO_JMP, "jump list not closed");
                self.func_state.expr_discharge(&mut rhs);
                rhs.true_jumplist = self
                    .func_state
                    .jmp_append(rhs.true_jumplist, lhs.true_jumplist);
                rhs
            }
        };

        expr
    }

    fn compile_binop_arith(
        &mut self,
        binop: &BinOp,
        mut lhs: ExprDesc<'src>,
        mut rhs: ExprDesc<'src>,
    ) -> ExprDesc<'src> {
        let mut bc_op = BCOp::from_ast(binop.op);

        let (rb, rc) = if let OpKind::Pow = binop.op {
            let rc = self.func_state.expr_toanyreg(&mut rhs);
            let rb = self.func_state.expr_toanyreg(&mut lhs);
            (rb, rc)
        } else {
            self.func_state.expr_toval(&mut rhs);
            let mut rhs_const = false;
            let mut rc = if let ExprKind::KNumber(n) = rhs.kind {
                let val = self.func_state.const_num(n);
                if val <= BCInstr::MAX_C {
                    bc_op = bc_op.transform(BCOp::ADDVV, BCOp::ADDVN);
                    rhs_const = true;
                    val
                } else {
                    self.func_state.expr_toanyreg(&mut rhs)
                }
            } else {
                self.func_state.expr_toanyreg(&mut rhs)
            };

            let rb = if let ExprKind::KNumber(n) = lhs.kind {
                if rhs_const {
                    self.func_state.expr_toanyreg(&mut lhs)
                } else {
                    let val = self.func_state.const_num(n);
                    if val <= BCInstr::MAX_C {
                        let temp = rc;
                        rc = val;
                        bc_op = bc_op.transform(BCOp::ADDVV, BCOp::ADDNV);
                        temp
                    } else {
                        self.func_state.expr_toanyreg(&mut lhs)
                    }
                }
            } else {
                self.func_state.expr_toanyreg(&mut lhs)
            };
            (rb, rc)
        };

        if matches!(lhs.kind, ExprKind::NonReloc { result_reg } if result_reg >= self.func_state.nactvar)
        {
            self.func_state.free_reg -= 1;
        }

        if matches!(rhs.kind, ExprKind::NonReloc { result_reg } if result_reg >= self.func_state.nactvar)
        {
            self.func_state.free_reg -= 1;
        }

        let instr_idx = self.bcemit(BCInstr::new_abc(bc_op, 0, rb as u8, rc as u8));
        ExprDesc::new(ExprKind::Relocable { instr_idx })
    }

    fn compile_binop_cmp(
        &mut self,
        binop: &BinOp,
        mut e1: ExprDesc<'src>,
        mut e2: ExprDesc<'src>,
    ) -> ExprDesc<'src> {
        let mut op = match binop.op {
            OpKind::Equ => BCOp::ISEQV,
            OpKind::Neq => BCOp::ISNEV,
            OpKind::Gre => BCOp::ISGT,
            OpKind::Grq => BCOp::ISGE,
            OpKind::Les => BCOp::ISLT,
            OpKind::Leq => BCOp::ISLE,
            other => unreachable!("bad binop {other}"),
        };

        self.func_state.expr_toval(&mut e1);

        let instr = if let BCOp::ISEQV | BCOp::ISNEV = op {
            if e1.is_const() {
                // need constant in 2nd arg
                (e1, e2) = (e2, e1);
            }

            let reg = self.func_state.expr_toanyreg(&mut e1);
            self.func_state.expr_toval(&mut e2);

            let ra = reg as u8;
            match e2.kind {
                ExprKind::KNil => BCInstr::new_ad(op.transform(BCOp::ISEQV, BCOp::ISEQP), ra, 0),
                ExprKind::KFalse => BCInstr::new_ad(op.transform(BCOp::ISEQV, BCOp::ISEQP), ra, 1),
                ExprKind::KTrue => BCInstr::new_ad(op.transform(BCOp::ISEQV, BCOp::ISEQP), ra, 2),
                ExprKind::KString(s) => BCInstr::new_ad(
                    op.transform(BCOp::ISEQV, BCOp::ISEQS),
                    ra,
                    self.func_state.const_str(s) as u16,
                ),
                ExprKind::KNumber(n) => BCInstr::new_ad(
                    op.transform(BCOp::ISEQV, BCOp::ISEQN),
                    ra,
                    self.func_state.const_num(n) as u16,
                ),
                _ => {
                    let rd = self.func_state.expr_toanyreg(&mut e2);
                    BCInstr::new_ad(op, ra, rd as u16)
                }
            }
        } else {
            let op_num = op as u8 - BCOp::ISLT as u8;
            let ra;
            let rd;
            if op_num & 1 != 0 {
                // we HATE greater than. no clue why.
                (e1, e2) = (e2, e1);
                op = BCOp::from_u8((op_num ^ 3) + BCOp::ISLT as u8);

                self.func_state.expr_toval(&mut e1);
                ra = self.func_state.expr_toanyreg(&mut e1);
                rd = self.func_state.expr_toanyreg(&mut e2);
            } else {
                rd = self.func_state.expr_toanyreg(&mut e2);
                ra = self.func_state.expr_toanyreg(&mut e1);
            }
            BCInstr::new_ad(op, ra as u8, rd as u16)
        };

        if let ExprKind::NonReloc { result_reg } = e1.kind {
            if result_reg >= self.func_state.nactvar {
                self.func_state.free_reg -= 1;
            }
        }

        if let ExprKind::NonReloc { result_reg } = e2.kind {
            if result_reg >= self.func_state.nactvar {
                self.func_state.free_reg -= 1;
            }
        }

        self.bcemit(instr);

        e1.kind = ExprKind::Jmp {
            instr_idx: self.func_state.bcemit_jmp(),
        };

        e1
    }

    fn compile_unop(&mut self, unop: &UnOp) -> ExprDesc<'src> {
        let mut val = self.compile_expr(unop.val);

        if matches!(unop.op, UnOpKind::Neg) && !val.has_jump() {
            if let ExprKind::KNumber(n) = &mut val.kind {
                if *n != 0.0 {
                    *n = -*n;
                    return val;
                }
            }
        }

        let reg = self.func_state.expr_toanyreg(&mut val);
        self.func_state.expr_free(&val);
        let instr_idx = match unop.op {
            UnOpKind::Neg => self.bcemit(BCInstr::new_ad(BCOp::UNM, 0, reg as u16)),
            UnOpKind::Len => self.bcemit(BCInstr::new_ad(BCOp::LEN, 0, reg as u16)),
        };
        val.kind = ExprKind::Relocable { instr_idx };
        val
    }

    fn compile_simple_expr(&mut self, simple_expr: &SimpleExpr<'src>) -> ExprDesc<'src> {
        match simple_expr {
            SimpleExpr::Num(str) => ExprDesc::new(ExprKind::KNumber(numlit::parse(str))),
            SimpleExpr::Str(str) => ExprDesc::new(ExprKind::KString(unsafe {
                str.get_unchecked(1..str.len() - 1)
            })),
            SimpleExpr::Bool(str) => match str.chars().next().unwrap() {
                't' => ExprDesc::new(ExprKind::KTrue),
                _ => ExprDesc::new(ExprKind::KFalse),
            },
            SimpleExpr::Nil(_) => ExprDesc::new(ExprKind::KNil),
            SimpleExpr::FuncNode(func_node) => self.compile_func::<false>(func_node), // self.compile_func(func_node)
            SimpleExpr::TableNode(table_node) => self.compile_table(table_node), // self.compile_table(table_node)
            SimpleExpr::SuffixedExpr(suffixed_expr) => self.compile_suffixed_expr(suffixed_expr),
        }
    }

    pub fn compile_chunk(&mut self, statements: &[Statement<'src>]) {
        for statement in statements {
            self.compile_statement(statement);
            debug_assert!(
                self.func_state.frame_size as u32 >= self.func_state.free_reg,
                "bad regalloc"
            );
            debug_assert!(
                self.func_state.free_reg >= self.func_state.nactvar,
                "bad regalloc"
            );
            self.func_state.free_reg = self.func_state.nactvar;
        }
    }

    fn compile_func<const METHOD: bool>(&mut self, func_node: &FuncNode<'src>) -> ExprDesc<'src> {
        self.fs_init();
        self.fscope_begin(ScopeFlags::empty());
        self.func_state.num_params = self.compile_params::<METHOD>(&func_node.type_.params);

        self.compile_chunk(&func_node.body);
        self.fs_finish();
        let pt_idx = self.func_state.num_kgc;
        self.func_state.num_kgc += 1;
        let instr_pc = self.bcemit(BCInstr::new_ad(BCOp::FNEW, 0, pt_idx as u16));
        let e = ExprDesc::new(ExprKind::Relocable {
            instr_idx: instr_pc,
        });

        if !self.func_state.flags.contains(ProtoFlags::CHILD) {
            if self.func_state.flags.contains(ProtoFlags::HAS_RETURN) {
                self.func_state.flags |= ProtoFlags::FIXUP_RETURN;
            }
            self.func_state.flags |= ProtoFlags::CHILD;
        }

        e
    }

    fn compile_params<const METHOD: bool>(&mut self, params: &[(String, types::Type)]) -> u8 {
        let mut num_params = if METHOD {
            self.var_new(0, "self");
            1
        } else {
            0
        };

        for (name, _) in params {
            self.var_new(num_params, name);
            num_params += 1;
        }

        self.var_add(num_params);

        debug_assert_eq!(self.func_state.nactvar, num_params, "bad regalloc");
        self.func_state.bcreg_reserve(num_params);

        num_params as u8
    }

    fn compile_block(&mut self, statements: &[Statement<'src>]) {
        self.fscope_begin(ScopeFlags::empty());
        self.compile_chunk(statements);
        self.fscope_end();
    }

    fn compile_return(&mut self, return_exprs: &[ExprRef]) {
        self.func_state.flags |= ProtoFlags::HAS_RETURN;
        let instr = match return_exprs.len() {
            0 => BCInstr::new_ad(BCOp::RET0, 0, 1),
            1 => {
                let mut last_expr = self.expr_list(return_exprs);
                if let ExprKind::Call { instr_idx, base } = last_expr.kind {
                    // tail call things
                    // NOTE: is ip what's getting popped?
                    if let BCOp::VARG = self.func_state.bc[instr_idx].op() {
                        self.func_state.bc[instr_idx].set_b(0);
                        BCInstr::new_ad(
                            BCOp::RETM,
                            self.func_state.nactvar as u8,
                            (base - self.func_state.nactvar) as u16,
                        )
                    } else {
                        let ip = self.func_state.bc.vec.pop().unwrap();
                        BCInstr::new_ad(
                            BCOp::transform(ip.op(), BCOp::CALL, BCOp::CALLT),
                            ip.a(),
                            ip.c() as u16,
                        )
                    }
                } else {
                    let reg = self.func_state.expr_toanyreg(&mut last_expr);
                    BCInstr::new_ad(BCOp::RET1, reg as u8, 2)
                }
            }
            len => {
                let mut last_expr = self.expr_list(return_exprs);
                if let ExprKind::Call { instr_idx, base } = last_expr.kind {
                    self.func_state.bc[instr_idx].set_b(0);
                    BCInstr::new_ad(
                        BCOp::RETM,
                        self.func_state.nactvar as u8,
                        (base - self.func_state.nactvar) as u16,
                    )
                } else {
                    self.func_state.expr_toanyreg(&mut last_expr);
                    BCInstr::new_ad(BCOp::RET, self.func_state.nactvar as u8, len as u16 + 1)
                }
            }
        };

        if self.func_state.flags.contains(ProtoFlags::CHILD) {
            self.bcemit(BCInstr::new_jmp(BCOp::UCLO, 0, 0));
        }

        self.bcemit(instr);
    }

    fn compile_struct_decl(&mut self, struct_decl: &StructDecl<'src>) {
        self.var_new(0, struct_decl.name);
        let local_reg = self.func_state.free_reg;
        self.func_state.bcreg_reserve(1);
        self.var_add(1);
        let mut table_expr = self.compile_table(&TableNode { fields: Vec::new() });
        // self.bcemit_store(val, b) without setting var as RW
        self.func_state.expr_free(&table_expr);
        self.func_state.expr_toreg(&mut table_expr, local_reg);
        self.var_get(self.func_state.nactvar - 1).startpc = self.func_state.bc.len();

        let mut key = ExprDesc::new(ExprKind::KString("__index"));
        self.compile_index(&mut table_expr, &mut key);
        let mut rhs = self.var_lookup(struct_decl.name);
        self.func_state.expr_toanyreg(&mut rhs);
        self.bcemit_store(&table_expr, rhs);

        if let Some(constructor) = &struct_decl.constructor {
            self.compile_constructor(constructor, struct_decl.name);
        }
    }

    fn compile_constructor(&mut self, constructor: &FuncNode<'src>, struct_name: &'src str) {
        let mut lhs = self.var_lookup(struct_name);
        self.func_state.expr_toanyreg(&mut lhs);
        let mut key = ExprDesc::new(ExprKind::KString("new"));
        self.compile_index(&mut lhs, &mut key);
        self.fscope_begin(ScopeFlags::empty());

        let func = {
            self.fs_init();
            self.fscope_begin(ScopeFlags::empty());
            self.func_state.num_params = {
                self.var_new(0, "_self");
                let mut num_params = 1;
                for (name, _) in &constructor.type_.params {
                    self.var_new(num_params, name);
                    num_params += 1;
                }

                self.var_add(num_params);

                debug_assert_eq!(self.func_state.nactvar, num_params, "bad regalloc");
                self.func_state.bcreg_reserve(num_params);

                num_params as u8
            };

            {
                // create a self variable, the one the user modifies
                self.var_new(0, "self");
                let table_expr = self.compile_table(&TableNode { fields: Vec::new() });
                self.assign_adjust(1, 1, table_expr);
                self.var_add(1);
            };

            self.compile_chunk(&constructor.body);

            {
                // setmetatable(self, _self)

                let mut func_expr = ExprDesc::new(ExprKind::Global("setmetatable"));
                let func_base = self.func_state.expr_tonextreg(&mut func_expr);
                if LJ_FR2 {
                    self.func_state.bcreg_reserve(1);
                }

                // args
                let mut pub_self = self.var_lookup("self");
                self.func_state.expr_tonextreg(&mut pub_self);
                let mut privself = self.var_lookup("_self");
                self.func_state.expr_tonextreg(&mut privself);

                self.bcemit(BCInstr::new_abc(
                    BCOp::CALL,
                    func_base as u8,
                    1,
                    (self.func_state.free_reg - func_base - LJ_FR2 as u32) as u8,
                ));

                self.func_state.free_reg = self.func_state.nactvar;
            }

            {
                // _self.__index = _self
                let mut priv_self = self.var_lookup("_self");
                self.func_state.expr_toanyreg(&mut priv_self);

                let mut key = ExprDesc::new(ExprKind::KString("__index"));
                self.compile_index(&mut priv_self, &mut key);
                let mut rhs = self.var_lookup("_self");
                self.func_state.expr_toanyreg(&mut rhs);
                self.bcemit_store(&priv_self, rhs);
            }

            {
                // return self
                self.func_state.flags |= ProtoFlags::HAS_RETURN;
                let mut pub_self = self.var_lookup("self");
                let reg = self.func_state.expr_toanyreg(&mut pub_self);
                if self.func_state.flags.contains(ProtoFlags::CHILD) {
                    self.bcemit(BCInstr::new_jmp(BCOp::UCLO, 0, 0));
                }

                self.bcemit(BCInstr::new_ad(BCOp::RET1, reg as u8, 2));
            }

            self.fs_finish();
            let pt_idx = self.func_state.num_kgc;
            self.func_state.num_kgc += 1;
            let instr_pc = self.bcemit(BCInstr::new_ad(BCOp::FNEW, 0, pt_idx as u16));
            let e = ExprDesc::new(ExprKind::Relocable {
                instr_idx: instr_pc,
            });

            if !self.func_state.flags.contains(ProtoFlags::CHILD) {
                if self.func_state.flags.contains(ProtoFlags::HAS_RETURN) {
                    self.func_state.flags |= ProtoFlags::FIXUP_RETURN;
                }
                self.func_state.flags |= ProtoFlags::CHILD;
            }

            e
        };

        self.fscope_end();

        self.bcemit_store(&lhs, func);
    }

    fn compile_table(&mut self, table_node: &TableNode<'src>) -> ExprDesc<'src> {
        let mut t_idx: Option<usize> = None;
        let mut free_reg = self.func_state.free_reg;
        let pc = self.bcemit(BCInstr::new_ad(BCOp::TNEW, free_reg as u8, 0));
        let mut table_expr = ExprDesc::new(ExprKind::NonReloc {
            result_reg: free_reg,
        });
        self.func_state.bcreg_reserve(1);
        free_reg += 1;

        let mut vcall = false;
        let mut need_arr = false;
        let mut fix_t = false;
        let mut array_len = 1;
        let mut hash_len = 0;
        for field in &table_node.fields {
            vcall = false;

            let (mut key, val) = match field {
                FieldNode::ExprField { key, val } => {
                    let mut key = self.compile_expr(*key);
                    self.func_state.expr_toval(&mut key);
                    if !key.is_const() {
                        self.compile_index(&mut table_expr, &mut key);
                    }

                    if let ExprKind::KNumber(0.0) = key.kind {
                        need_arr = true;
                    } else {
                        hash_len += 1;
                    }

                    (key, *val)
                }
                FieldNode::Field { key, val } => {
                    hash_len += 1;
                    (ExprDesc::new(ExprKind::KString(key)), *val)
                }
                FieldNode::ValField { val } => {
                    let key = ExprDesc::new(ExprKind::KNumber(array_len as f64));
                    array_len += 1;
                    need_arr = true;
                    vcall = true;
                    (key, *val)
                }
            };

            let mut val = self.compile_expr(val);
            let is_const_no_jump = val.is_const() && !val.has_jump();
            if key.is_const()
                && !matches!(&key.kind, ExprKind::KNil)
                && (matches!(&key.kind, ExprKind::KString(_)) || is_const_no_jump)
            {
                let t_idx = t_idx.get_or_insert_with(|| {
                    let kidx = self.func_state.num_kgc;
                    self.func_state.num_kgc += 1;
                    let len = self.func_state.template_tables.len();
                    let table = if need_arr {
                        TemplateTable::with_size(array_len)
                    } else {
                        TemplateTable::default()
                    };
                    self.func_state.template_tables.push((table, kidx));
                    self.func_state.bc[pc] =
                        BCInstr::new_ad(BCOp::TDUP, free_reg as u8 - 1, kidx as u16);
                    len
                });

                vcall = false;
                let k = match key.kind {
                    ExprKind::KFalse => TValue::False,
                    ExprKind::KTrue => TValue::True,
                    ExprKind::KString(s) => TValue::String(s.to_owned()),
                    ExprKind::KNumber(n) => TValue::Number(n),
                    _ => unreachable!(),
                };

                if is_const_no_jump {
                    let v = match val.kind {
                        ExprKind::KNil => TValue::Nil,
                        ExprKind::KFalse => TValue::False,
                        ExprKind::KTrue => TValue::True,
                        ExprKind::KString(s) => TValue::String(s.to_owned()),
                        ExprKind::KNumber(n) => TValue::Number(n),
                        _ => unreachable!(),
                    };

                    self.func_state.template_tables[*t_idx].0.insert(k, v);
                } else {
                    fix_t = true;
                    if !matches!(
                        val.kind,
                        ExprKind::Call {
                            instr_idx: _,
                            base: _
                        }
                    ) {
                        self.func_state.expr_toanyreg(&mut val);
                        vcall = false;
                    }

                    if key.is_const() {
                        self.compile_index(&mut table_expr, &mut key);
                    }

                    self.bcemit_store(&table_expr, val);
                }
            } else {
                if !matches!(
                    val.kind,
                    ExprKind::Call {
                        instr_idx: _,
                        base: _
                    }
                ) {
                    self.func_state.expr_toanyreg(&mut val);
                    vcall = false;
                }

                if key.is_const() {
                    self.compile_index(&mut table_expr, &mut key);
                }

                self.bcemit_store(&table_expr, val);
            }

            self.func_state.free_reg = free_reg;
        }

        if vcall {
            let val = (0x43300000 << 32) | (array_len - 1) as u64;
            let en = self.func_state.const_num(f64::from_bits(val));
            let instr = self.func_state.bc.last_mut();
            debug_assert_eq!(instr.a(), free_reg as u8, "bad CALL code generation");
            debug_assert_eq!(
                instr.op(),
                if array_len > 256 {
                    BCOp::TSETV
                } else {
                    BCOp::TSETB
                },
                "bad CALL code generation"
            );

            let instr = if array_len > 256 {
                self.func_state.bc.pop();
                self.func_state.bc.last_mut()
            } else {
                instr
            };

            *instr = BCInstr::new_ad(BCOp::TSETM, free_reg as u8, en as u16);
            let prev = self.func_state.bc.len() - 2;
            self.func_state.bc[prev].set_b(0);
        }

        // make expr relocable if possible
        table_expr.kind = if pc == self.func_state.bc.len() - 1 {
            self.func_state.free_reg -= 1;
            ExprKind::Relocable { instr_idx: pc }
        } else {
            let result_reg = match table_expr.kind {
                ExprKind::Indexed { table_reg, .. } => table_reg,
                ExprKind::NonReloc { result_reg } => result_reg,
                _ => unreachable!(),
            };

            ExprKind::NonReloc { result_reg }
        };

        if let Some(t) = t_idx.map(|idx| &mut self.func_state.template_tables[idx].0) {
            if need_arr && t.array.len() < array_len {
                t.array.resize(array_len, TValue::Nil);
            }

            if fix_t {
                eprintln!("fix_t")
            }
        } else {
            let instr = &mut self.func_state.bc[pc];
            if need_arr {
                array_len = array_len.clamp(3, 0x7FF);
            } else {
                array_len = 0;
            };

            let hsize = if hash_len > 1 {
                1 + (u32::trailing_zeros(hash_len - 1) ^ 31) as u16
            } else {
                hash_len as u16
            };

            instr.set_d((hsize << 11) | array_len as u16);
        }
        table_expr
    }

    fn compile_suffixed_expr(&mut self, suffixed_expr: &SuffixedExpr<'src>) -> ExprDesc<'src> {
        let mut expr = self.compile_expr(suffixed_expr.val);

        for suffix in &suffixed_expr.suffixes {
            expr = self.compile_suffix(suffix, expr);
        }

        expr
    }

    fn compile_suffixed_name(&mut self, suffixed_name: &SuffixedName<'src>) -> ExprDesc<'src> {
        let mut expr = self.var_lookup(suffixed_name.name);

        for suffix in &suffixed_name.suffixes {
            expr = self.compile_suffix(suffix, expr);
        }

        expr
    }

    fn compile_suffix(
        &mut self,
        suffix: &Suffix<'src>,
        mut base: ExprDesc<'src>,
    ) -> ExprDesc<'src> {
        match suffix {
            Suffix::Method(Method { method_name, args }) => {
                base = self.bcemit_method(base, method_name);
                base = self.compile_args(&base, args);
            }
            Suffix::Call(Call { args }) => {
                self.func_state.expr_tonextreg(&mut base);
                if LJ_FR2 {
                    self.func_state.bcreg_reserve(1);
                }
                base = self.compile_args(&base, args);
            }
            Suffix::Access(Access { field_name }) => {
                self.func_state.expr_toanyreg(&mut base);
                let mut key = ExprDesc::new(ExprKind::KString(field_name));
                self.compile_index(&mut base, &mut key);
            }
            Suffix::Index(Index { key }) => {
                self.func_state.expr_toanyreg(&mut base);
                let mut key = self.compile_expr(*key);
                self.func_state.expr_toval(&mut key);
                self.compile_index(&mut base, &mut key);
            }
        }

        base
    }

    fn compile_index(&mut self, base: &mut ExprDesc<'src>, key: &mut ExprDesc<'src>) {
        let ExprKind::NonReloc { result_reg } = base.kind else {
            unreachable!("compile_index called on bad expr: {base:?}")
        };

        let index = match key.kind {
            ExprKind::KNumber(n) => {
                let k = n as u8;
                if k as f64 == n {
                    BCInstr::MAX_C + 1 + k as u32
                } else {
                    self.func_state.expr_toanyreg(key)
                }
            }
            ExprKind::KString(s) => {
                let idx = self.func_state.const_str(s);
                if idx <= BCInstr::MAX_C {
                    !idx
                } else {
                    self.func_state.expr_toanyreg(key)
                }
            }
            _ => self.func_state.expr_toanyreg(key),
        };

        base.kind = ExprKind::Indexed {
            table_reg: result_reg,
            index,
        };
    }

    fn expr_list(&mut self, exprs: &[ExprRef]) -> ExprDesc<'src> {
        let mut last = self.compile_expr(exprs[0]);

        for expr in &exprs[1..] {
            self.func_state.expr_tonextreg(&mut last);
            last = self.compile_expr(*expr);
        }

        last
    }

    fn compile_args(&mut self, func: &ExprDesc<'src>, args: &[ExprRef]) -> ExprDesc<'src> {
        let mut arg_expr = if args.is_empty() {
            ExprDesc::new(ExprKind::Void)
        } else {
            let temp = self.expr_list(args);
            if let ExprKind::Call { instr_idx, base: _ } = temp.kind {
                self.func_state.bc[instr_idx].set_b(0);
            }
            temp
        };

        let ExprKind::NonReloc {
            result_reg: func_base,
        } = func.kind
        else {
            panic!("bad expr type {:?}", func.kind)
        };

        let instr = match arg_expr.kind {
            ExprKind::Call { instr_idx: _, base } => BCInstr::new_abc(
                BCOp::CALLM,
                func_base as u8,
                2,
                (base - func_base - 1 - LJ_FR2 as u32) as u8,
            ),
            ExprKind::Void => BCInstr::new_abc(
                BCOp::CALL,
                func_base as u8,
                2,
                (self.func_state.free_reg - func_base - LJ_FR2 as u32) as u8,
            ),
            _ => {
                self.func_state.expr_tonextreg(&mut arg_expr);
                BCInstr::new_abc(
                    BCOp::CALL,
                    func_base as u8,
                    2,
                    (self.func_state.free_reg - func_base - LJ_FR2 as u32) as u8,
                )
            }
        };

        let result = ExprDesc::new(ExprKind::Call {
            instr_idx: self.bcemit(instr),
            base: func_base,
        });

        // leave one result by default
        self.func_state.free_reg = func_base + 1;
        result
    }
}

impl<'src> LJCompiler<'src, '_> {
    #[inline]
    pub fn bcemit(&mut self, instr: BCInstr) -> BCPos {
        self.func_state.bcemit(instr)
    }

    pub fn bcemit_store(
        &mut self,
        var: &ExprDesc<'src>,
        mut expr: ExprDesc<'src>,
    ) -> ExprDesc<'src> {
        let instr = match var.kind {
            ExprKind::Local {
                local_reg,
                vstack_idx,
            } => {
                self.var_info[vstack_idx as usize].flags |= VarFlags::VAR_RW;
                self.func_state.expr_free(&expr);
                self.func_state.expr_toreg(&mut expr, local_reg);
                return expr;
            }
            ExprKind::Upvalue {
                upvalue_idx,
                vstack_idx,
            } => {
                self.var_info[vstack_idx as usize].flags |= VarFlags::VAR_RW;
                self.func_state.expr_toval(&mut expr);

                let ra = upvalue_idx as u8;
                let (op, rd) = match expr.kind {
                    ExprKind::KNil => (BCOp::USETP, 0),
                    ExprKind::KFalse => (BCOp::USETP, 1),
                    ExprKind::KTrue => (BCOp::USETP, 2),
                    _ => {
                        let reg = self.func_state.expr_toanyreg(&mut expr);
                        (BCOp::USETV, reg as u16)
                    }
                };

                BCInstr::new_ad(op, ra, rd)
            }
            ExprKind::Global(name) => {
                let ra = self.func_state.expr_toanyreg(&mut expr);
                BCInstr::new_ad(BCOp::GSET, ra as u8, self.func_state.const_str(name) as u16)
            }
            ExprKind::Indexed { table_reg, index } => {
                let ra = self.func_state.expr_toanyreg(&mut expr) as u8;
                let rb = table_reg as u8;
                let rc = index;

                if (rc as i32) < 0 {
                    BCInstr::new_abc(BCOp::TSETS, ra, rb, (!rc) as u8)
                } else if rc > BCInstr::MAX_C {
                    BCInstr::new_abc(BCOp::TSETB, ra, rb, (rc - (BCInstr::MAX_C + 1)) as u8)
                } else {
                    if cfg!(debug_assertions) {
                        if let ExprKind::NonReloc { result_reg: _ } = expr.kind {
                            if ra as u32 >= self.func_state.nactvar && rc >= ra as u32 {
                                self.func_state.bcreg_free(rc);
                            }
                        }
                    }

                    BCInstr::new_abc(BCOp::TSETV, ra, rb, rc as u8)
                }
            }
            _ => {
                unreachable!("bad expr type {:?}", var.kind)
            }
        };

        self.bcemit(instr);
        self.func_state.expr_free(&expr);
        expr
    }

    pub fn bcemit_method(
        &mut self,
        mut base: ExprDesc<'src>,
        method_name: &'src str,
    ) -> ExprDesc<'src> {
        let obj = self.func_state.expr_toanyreg(&mut base);
        self.func_state.expr_free(&base);
        let func = self.func_state.free_reg as u8;
        self.bcemit(BCInstr::new_ad(
            BCOp::MOV,
            func + 1 + LJ_FR2 as u8,
            obj as u16,
        ));
        let key_idx = self.func_state.const_str(method_name);
        // can it fit in a C operand?
        if key_idx <= BCInstr::MAX_C {
            self.func_state.bcreg_reserve(2 + LJ_FR2 as u32);
            self.bcemit(BCInstr::new_abc(
                BCOp::TGETS,
                func,
                obj as u8,
                key_idx as u8,
            ));
        } else {
            self.func_state.bcreg_reserve(3 + LJ_FR2 as u32);
            let key_reg = func + 2 + LJ_FR2 as u8;
            self.bcemit(BCInstr::new_ad(BCOp::KSTR, key_reg, key_idx as u16));
            self.bcemit(BCInstr::new_abc(BCOp::TGETV, func, obj as u8, key_reg));
        }

        base.kind = ExprKind::NonReloc {
            result_reg: func as u32,
        };
        base
    }
}

// Variable info handling
impl<'src> LJCompiler<'src, '_> {
    fn var_get(&mut self, reg: BCReg) -> &mut VarInfo {
        let idx = self.func_state.var_map[reg as usize];
        &mut self.var_info[idx as usize]
    }

    // NOTE: not sure about this.
    fn var_new(&mut self, n: BCReg, name: &str) {
        let vtop = self.var_info.len();

        let var_idx = self.func_state.nactvar + n;

        self.var_info.push(VarInfo {
            name: Some(name.into()),
            startpc: 0,
            endpc: 0,
            slot: 0,
            flags: VarFlags::empty(),
        });

        self.func_state.var_map[var_idx as usize] = vtop as u16;
    }

    fn var_add(&mut self, num_vars: BCReg) {
        let pc = self.func_state.bc.len();
        let mut nactvar = self.func_state.nactvar;
        for _ in 0..num_vars {
            let v = self.var_get(nactvar);
            v.startpc = pc;
            v.slot = nactvar as u8;
            nactvar += 1;
            v.flags = VarFlags::empty();
        }
        self.func_state.nactvar = nactvar;
    }

    fn var_remove(&mut self, to_level: BCReg) {
        while self.func_state.nactvar > to_level {
            self.func_state.nactvar -= 1;
            self.var_get(self.func_state.nactvar).endpc = self.func_state.bc.len();
        }
    }

    fn var_lookup_uv(&mut self, var_idx: VarIdx) -> u16 {
        if let Some(uv_idx) = self.func_state.uv_map.iter().position(|it| *it == var_idx) {
            return uv_idx as u16;
        }
        // otherwise create
        self.func_state.uv_map.push(var_idx);
        self.func_state.uv_map.len() as u16 - 1
    }

    fn var_lookup(&mut self, name: &'src str) -> ExprDesc<'src> {
        let mut first = true;
        let all_states = self
            .prev_states
            .iter_mut()
            .chain(std::iter::once(&mut self.func_state));

        for func_state in all_states.rev() {
            let reg = (0..func_state.nactvar).rfind(|reg| {
                let idx = func_state.var_map[*reg as usize];
                let var = &self.var_info[idx as usize];
                match var.name.as_ref() {
                    Some(it) => name == it.as_ref(),
                    None => false,
                }
            });

            if let Some(reg) = reg {
                let var_idx = func_state.var_map[reg as usize];
                if first {
                    return ExprDesc::new(ExprKind::Local {
                        local_reg: reg,
                        vstack_idx: var_idx,
                    });
                }

                func_state.uvmark(reg);
                return ExprDesc::new(ExprKind::Upvalue {
                    upvalue_idx: self.var_lookup_uv(var_idx),
                    vstack_idx: var_idx,
                });
            }

            first = false;
        }

        ExprDesc::new(ExprKind::Global(name))
    }
}
