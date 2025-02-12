use std::collections::hash_map::Entry;

use bitflags::bitflags;
use rustc_hash::FxHashMap;

use crate::luajit::{
    bytecode::{BCInstr, BCOp, BCPos, BCReg, Bytecode, ProtoFlags, TemplateTable, NO_JMP, NO_REG},
    ExprDesc, ExprKind, TValue, VarFlags, VarInfo,
};

pub type VarIdx = u16;

#[derive(Debug)]
pub struct FuncState {
    /// Hash table for constants
    pub kt: FxHashMap<TValue, BCReg>,
    pub template_tables: Vec<(TemplateTable, usize)>,
    pub num_kn: usize,
    pub num_kgc: usize,
    pub last_target: BCPos,
    pub jpc: BCPos,
    /// First free register
    pub free_reg: BCReg,
    pub frame_size: u8,
    /// Number of active local variables
    pub nactvar: BCReg,
    pub bc: Bytecode,
    pub num_params: u8,
    pub flags: ProtoFlags,
    // NOTE: maybe box this?
    pub var_map: [VarIdx; 200],
    /// Map from upvalue to `VarIdx`
    pub uv_map: Vec<VarIdx>,
    pub scope: FuncScope,
    pub prev_scopes: Vec<FuncScope>,
}

#[derive(Debug, Default)]
pub struct FuncScope {
    pub vstart: u32,
    pub nactvar: u8,
    pub flags: ScopeFlags,
}

bitflags! {
    #[derive(Debug, Default)]
    pub struct ScopeFlags: u8 {
        const LOOP = 0b00001;
        const BREAK = 0b00010;
        const GOLA = 0b00100;
        const UPVAL = 0b01000;
        const NOCLOSE = 0b10000;
    }
}

impl Default for FuncState {
    fn default() -> Self {
        Self {
            kt: FxHashMap::default(),
            template_tables: Vec::new(),
            num_kn: 0,
            num_kgc: 0,
            last_target: 0,
            jpc: NO_JMP,
            free_reg: 0,
            frame_size: 1,
            nactvar: 0,
            bc: Bytecode::default(),
            num_params: 0,
            flags: ProtoFlags::empty(),
            var_map: [0; 200],
            uv_map: Vec::default(),
            scope: FuncScope::default(),
            prev_scopes: Vec::default(),
        }
    }
}

impl FuncState {
    pub fn top() -> Self {
        Self {
            flags: ProtoFlags::VARARG,
            ..Default::default()
        }
    }

    pub fn uvmark(&mut self, level: BCReg) {
        let mut scopes = self
            .prev_scopes
            .iter_mut()
            .chain(std::iter::once(&mut self.scope));

        if let Some(bl) = scopes.rfind(|it| it.nactvar as u32 <= level) {
            bl.flags |= ScopeFlags::UPVAL;
        }
    }
}

impl FuncState {
    pub fn const_num(&mut self, n: f64) -> BCReg {
        match self.kt.entry(TValue::Number(n)) {
            Entry::Occupied(occupied_entry) => *occupied_entry.get(),
            Entry::Vacant(vacant_entry) => {
                let nkn = self.num_kn;
                self.num_kn += 1;
                *vacant_entry.insert(nkn as BCReg)
            }
        }
    }

    pub fn const_str(&mut self, s: &str) -> BCReg {
        match self.kt.entry(TValue::String(s.to_owned())) {
            Entry::Occupied(occupied_entry) => *occupied_entry.get(),
            Entry::Vacant(vacant_entry) => {
                let nkgc = self.num_kgc;
                self.num_kgc += 1;
                *vacant_entry.insert(nkgc as BCReg)
            }
        }
    }
}

impl FuncState {
    pub fn bcemit(&mut self, instr: BCInstr) -> BCPos {
        let pc = self.bc.len();
        self.jmp_patchval(self.jpc, pc, NO_REG, pc);
        self.jpc = NO_JMP;
        self.bc.vec.push(instr);
        pc
    }

    pub fn bcemit_jmp(&mut self) -> BCPos {
        let jpc = self.jpc;
        let mut j = self.bc.len() - 1;
        let ip = &mut self.bc[j];
        self.jpc = NO_JMP;
        match ip.op() {
            BCOp::UCLO if j as i32 >= self.last_target as i32 => {
                ip.set_j(NO_JMP as i16);
                self.last_target = j + 1;
            }
            _ => {
                j = self.bcemit(BCInstr::new_jmp(
                    BCOp::JMP,
                    self.free_reg as u8,
                    NO_JMP as i16,
                ));
            }
        }

        self.jmp_append(j, jpc)
    }

    fn bcemit_branch(&mut self, expr: &mut ExprDesc<'_>, cond: bool) -> BCPos {
        if let ExprKind::Relocable { instr_idx } = expr.kind {
            let instr = &mut self.bc[instr_idx];
            if let BCOp::NOT = instr.op() {
                *instr = BCInstr::new_ad(if cond { BCOp::ISF } else { BCOp::IST }, 0, instr.d());
                return self.bcemit_jmp();
            }
        }

        let reg = if let ExprKind::NonReloc { result_reg } = expr.kind {
            result_reg
        } else {
            self.bcreg_reserve(1);
            let reg = self.free_reg - 1;
            self.expr_toreg_nobranch(expr, reg);
            reg
        };

        self.bcemit(BCInstr::new_ad(
            if cond { BCOp::ISTC } else { BCOp::ISFC },
            NO_REG as u8,
            reg as u16,
        ));
        let instr_idx = self.bcemit_jmp();
        self.expr_free(expr);
        instr_idx
    }

    pub fn bcemit_branch_t(&mut self, expr: &mut ExprDesc<'_>) {
        self.expr_discharge(expr);
        let instr_idx = match expr.kind {
            ExprKind::KString(_) | ExprKind::KNumber(_) | ExprKind::KTrue => NO_JMP,
            ExprKind::Jmp { instr_idx } => {
                self.invert_cond(instr_idx);
                instr_idx
            }
            ExprKind::KFalse | ExprKind::KNil => {
                self.expr_toreg_nobranch(expr, NO_REG);
                self.bcemit_jmp()
            }
            _ => self.bcemit_branch(expr, false),
        };

        expr.false_jumplist = self.jmp_append(expr.false_jumplist, instr_idx);
        self.jmp_tohere(expr.true_jumplist);
        expr.true_jumplist = NO_JMP;
    }

    pub fn bcemit_branch_f(&mut self, expr: &mut ExprDesc<'_>) {
        self.expr_discharge(expr);
        let instr_idx = match expr.kind {
            ExprKind::KNil | ExprKind::KFalse => NO_JMP,
            ExprKind::Jmp { instr_idx } => instr_idx,
            ExprKind::KString(_) | ExprKind::KNumber(_) | ExprKind::KTrue => {
                self.expr_toreg_nobranch(expr, NO_REG);
                self.bcemit_jmp()
            }
            _ => self.bcemit_branch(expr, true),
        };

        expr.true_jumplist = self.jmp_append(expr.true_jumplist, instr_idx);
        self.jmp_tohere(expr.false_jumplist);
        expr.false_jumplist = NO_JMP;
    }

    fn jmp_next(&self, pc: BCPos) -> Option<BCPos> {
        let delta = self.bc[pc].j();
        if delta as u32 == NO_JMP {
            None
        } else {
            Some(((pc as isize + 1) + delta as isize) as BCPos)
        }
    }

    fn jmp_novalue(&self, mut list: BCPos) -> bool {
        while list != NO_JMP {
            let idx = list.saturating_sub(1);
            let p = self.bc[idx];

            if !matches!(p.op(), BCOp::ISTC | BCOp::ISFC) && p.a() != 0xFF {
                return true;
            }

            let Some(next) = self.jmp_next(list) else {
                break;
            };

            list = next;
        }

        false
    }

    fn jmp_patchtestreg(&mut self, pc: BCPos, reg: BCReg) -> bool {
        let idx = pc.saturating_sub(1);
        let op = self.bc[idx].op();
        if let BCOp::ISTC | BCOp::ISFC = op {
            if reg != NO_REG && reg != self.bc[idx].d() as u32 {
                self.bc[idx].set_a(reg as u8);
            } else {
                self.bc[idx].set_op(unsafe { std::mem::transmute::<u8, BCOp>(op as u8 + 2) });
                self.bc[idx].set_a(0);
            }
        } else if self.bc[idx].a() as BCReg == NO_REG {
            if reg == NO_REG {
                // i'm super skeptical but luajit writes it like this too
                self.bc[idx] = BCInstr::new_jmp(BCOp::JMP, self.bc[pc].a(), 0);
            } else {
                self.bc[idx].set_a(reg as u8);
                if reg >= self.bc[idx + 1].a() as BCReg {
                    self.bc[idx + 1].set_a(reg as u8 + 1);
                }
            }
        } else {
            return false;
        }
        true
    }

    pub fn jmp_patchins(&mut self, pc: BCPos, dest: BCPos) {
        let jmp = &mut self.bc[pc];
        let offset = dest + BCInstr::BIAS_J as u32 - (pc + 1);
        debug_assert!(dest != NO_JMP, "uninitialized jump target");
        assert!(offset <= BCInstr::MAX_D, "luajit gives up here and so do i");

        jmp.set_d(offset as u16);
    }

    pub fn jmp_append(&mut self, l1: BCPos, l2: BCPos) -> BCPos {
        if l2 == NO_JMP {
            l1
        } else if l1 == NO_JMP {
            l2
        } else {
            let mut list = l1;
            while let Some(next) = self.jmp_next(list) {
                list = next;
            }

            self.jmp_patchins(list, l2);

            l1
        }
    }

    fn jmp_patchval(&mut self, list: BCPos, vtarget: BCPos, reg: BCReg, dtarget: BCPos) {
        if list == NO_JMP {
            return;
        }
        let mut next = Some(list);
        while let Some(list) = next {
            next = self.jmp_next(list);

            if self.jmp_patchtestreg(list, reg) {
                self.jmp_patchins(list, vtarget);
            } else {
                self.jmp_patchins(list, dtarget);
            }
        }
    }

    pub fn jmp_tohere(&mut self, list: BCPos) {
        self.last_target = self.bc.len();
        self.jpc = self.jmp_append(self.jpc, list);
    }

    pub fn jmp_patch(&mut self, list: BCPos, target: BCPos) {
        if target == self.bc.len() {
            self.jmp_tohere(list);
        } else {
            debug_assert!(target < self.bc.len(), "bad jump target");
            self.jmp_patchval(list, target, NO_REG, target);
        }
    }

    pub fn bcreg_bump(&mut self, num_regs: BCReg) {
        let sz = self.free_reg + num_regs;
        if sz > self.frame_size as BCReg {
            self.frame_size = sz as u8;
        }
    }

    pub fn bcreg_reserve(&mut self, num_regs: BCReg) {
        self.bcreg_bump(num_regs);
        self.free_reg += num_regs;
    }

    pub fn bcreg_free(&mut self, reg: BCReg) {
        if reg >= self.nactvar {
            self.free_reg -= 1;
            debug_assert!(reg == self.free_reg, "bad regfree");
        }
    }

    /// Free register for expression
    pub fn expr_free(&mut self, expr: &ExprDesc<'_>) {
        if let ExprKind::NonReloc { result_reg } = expr.kind {
            self.bcreg_free(result_reg);
        }
    }

    pub fn expr_discharge(&mut self, expr: &mut ExprDesc<'_>) {
        let instr = match expr.kind {
            ExprKind::Upvalue {
                upvalue_idx,
                vstack_idx: _,
            } => BCInstr::new_ad(BCOp::UGET, 0, upvalue_idx),
            ExprKind::Global(name) => BCInstr::new_ad(BCOp::GGET, 0, self.const_str(name) as u16),
            ExprKind::Indexed { table_reg, index } => {
                let rc = index;
                let instr = if (rc as i32) < 0 {
                    BCInstr::new_abc(BCOp::TGETS, 0, table_reg as u8, !rc as u8)
                } else if rc > BCInstr::MAX_C {
                    BCInstr::new_abc(
                        BCOp::TGETB,
                        0,
                        table_reg as u8,
                        (rc - (BCInstr::MAX_C + 1)) as u8,
                    )
                } else {
                    self.bcreg_free(rc);
                    BCInstr::new_abc(BCOp::TGETV, 0, table_reg as u8, rc as u8)
                };

                self.bcreg_free(table_reg);
                instr
            }
            ExprKind::Call { instr_idx: _, base } => {
                expr.kind = ExprKind::NonReloc { result_reg: base };
                return;
            }
            ExprKind::Local {
                local_reg,
                vstack_idx: _,
            } => {
                expr.kind = ExprKind::NonReloc {
                    result_reg: local_reg,
                };
                return;
            }
            _ => return,
        };

        let instr_pc = self.bcemit(instr);
        expr.kind = ExprKind::Relocable {
            instr_idx: instr_pc,
        };
    }

    pub fn bcemit_nil(&mut self, mut from: BCReg, mut n: BCReg) {
        if self.bc.len() > self.last_target {
            let instr = self.bc.last_mut();
            let pfrom = instr.a() as BCReg;
            match instr.op() {
                BCOp::KPRI if instr.d() == 0 => {
                    if from == pfrom {
                        if n == 1 {
                            return;
                        }

                        *instr = BCInstr::new_ad(BCOp::KNIL, from as u8, (from + n - 1) as u16);

                        return;
                    } else if from == pfrom + 1 {
                        from = pfrom;
                        n += 1;

                        *instr = BCInstr::new_ad(BCOp::KNIL, from as u8, (from + n - 1) as u16);

                        return;
                    }
                }
                BCOp::KNIL => {
                    let pto = instr.d() as BCReg;
                    if pfrom <= from && from <= pto + 1 {
                        if from + n - 1 > pto {
                            instr.set_d((from + n - 1) as u16);
                        }
                        return;
                    }
                }
                _ => (),
            }
        }

        if n == 1 {
            self.bcemit(BCInstr::new_ad(BCOp::KPRI, from as u8, 0));
        } else {
            self.bcemit(BCInstr::new_ad(
                BCOp::KNIL,
                from as u8,
                (from + n - 1) as u16,
            ));
        }
    }

    /// Discharge an expression to a specific register. Ignore branches.
    /// Always returns `NonReloc`
    fn expr_toreg_nobranch(&mut self, expr: &mut ExprDesc<'_>, reg: BCReg) {
        self.expr_discharge(expr);

        let instr = match expr.kind {
            ExprKind::KString(s) => Some(BCInstr::new_ad(
                BCOp::KSTR,
                reg as u8,
                self.const_str(s) as u16,
            )),
            ExprKind::KNumber(n) => {
                let k = n as i16;
                if k as f64 == n {
                    Some(BCInstr::new_ad(BCOp::KSHORT, reg as u8, k as u16))
                } else {
                    Some(BCInstr::new_ad(
                        BCOp::KNUM,
                        reg as u8,
                        self.const_num(n) as u16,
                    ))
                }
            }
            ExprKind::KCData(_) => todo!("ffi"),
            ExprKind::Relocable { instr_idx } => {
                self.bc[instr_idx].set_a(reg as u8);
                None
            }
            ExprKind::NonReloc { result_reg } => {
                // We only move the value if we have to.
                if result_reg == reg {
                    None
                } else {
                    Some(BCInstr::new_ad(BCOp::MOV, reg as u8, result_reg as u16))
                }
            }
            ExprKind::KNil => Some(BCInstr::new_ad(BCOp::KPRI, reg as u8, 0)),
            ExprKind::KFalse => Some(BCInstr::new_ad(BCOp::KPRI, reg as u8, 1)),
            ExprKind::KTrue => Some(BCInstr::new_ad(BCOp::KPRI, reg as u8, 2)),
            ExprKind::Jmp { instr_idx: _ } | ExprKind::Void => return,
            _ => panic!("bad expr type {expr:?}"),
        };

        if let Some(instr) = instr {
            self.bcemit(instr);
        }

        expr.kind = ExprKind::NonReloc { result_reg: reg };
    }

    /// Always returns `NonReloc`
    pub fn expr_toreg(&mut self, expr: &mut ExprDesc<'_>, reg: BCReg) {
        self.expr_toreg_nobranch(expr, reg);

        if let ExprKind::Jmp {
            instr_idx: instr_pc,
        } = expr.kind
        {
            expr.true_jumplist = self.jmp_append(expr.true_jumplist, instr_pc);
        }

        if expr.has_jump() {
            // i love this naming scheme too much to let it go
            let mut jfalse = NO_JMP;
            let mut jtrue = NO_JMP;

            if self.jmp_novalue(expr.true_jumplist) || self.jmp_novalue(expr.false_jumplist) {
                let jval = match expr.kind {
                    ExprKind::Jmp { instr_idx: _ } => NO_JMP,
                    _ => self.bcemit_jmp(),
                };

                jfalse = self.bcemit(BCInstr::new_ad(BCOp::KPRI, reg as u8, 1));
                self.bcemit(BCInstr::new_jmp(BCOp::JMP, self.free_reg as u8, 1));
                jtrue = self.bcemit(BCInstr::new_ad(BCOp::KPRI, reg as u8, 2));
                self.jmp_tohere(jval);
            }

            let jend = self.bc.len();
            self.last_target = jend;
            self.jmp_patchval(expr.false_jumplist, jend, reg, jfalse);
            self.jmp_patchval(expr.true_jumplist, jend, reg, jtrue);
        }

        expr.false_jumplist = NO_JMP;
        expr.true_jumplist = NO_JMP;
        expr.kind = ExprKind::NonReloc { result_reg: reg };
    }

    /// Always returns `NonReloc`
    pub fn expr_tonextreg(&mut self, expr: &mut ExprDesc<'_>) -> BCReg {
        self.expr_discharge(expr);
        self.expr_free(expr);
        self.bcreg_reserve(1);
        let reg = self.free_reg - 1;
        self.expr_toreg(expr, reg);
        reg
    }

    /// Always returns `NonReloc`
    pub fn expr_toanyreg(&mut self, expr: &mut ExprDesc<'_>) -> BCReg {
        self.expr_discharge(expr);
        if let ExprKind::NonReloc { result_reg } = expr.kind {
            if !expr.has_jump() {
                return result_reg;
            } else if result_reg >= self.nactvar {
                self.expr_toreg(expr, result_reg);
                return result_reg;
            }
        }

        self.expr_tonextreg(expr)
    }

    pub fn expr_toval(&mut self, expr: &mut ExprDesc<'_>) {
        if expr.has_jump() {
            self.expr_toanyreg(expr);
        } else {
            self.expr_discharge(expr);
        }
    }

    pub fn invert_cond(&mut self, instr_pc: BCPos) {
        let instr = &mut self.bc[instr_pc - 1];
        instr.set_op(instr.op().invert());
    }
}

const NAME_BREAK: &str = "(break)";

impl FuncState {
    pub fn gola_new(&mut self, var_info: &mut Vec<VarInfo>, flags: VarFlags, pc: BCPos) -> usize {
        let vtop = var_info.len();

        var_info.push(VarInfo {
            name: Some(NAME_BREAK.into()),
            startpc: pc,
            slot: self.nactvar as u8,
            flags,
            ..Default::default()
        });

        vtop
    }

    pub fn gola_patch(&mut self, vg: &mut VarInfo, vl: &VarInfo) {
        let pc = vg.startpc;
        vg.name = None;
        self.bc[pc].set_a(vl.slot);
        self.jmp_patch(pc, vl.startpc);
    }

    pub fn gola_close(&mut self, vg: &VarInfo) {
        let idx = vg.startpc;
        debug_assert!(vg.is_goto(), "expected goto");
        debug_assert!(
            matches!(self.bc[idx].op(), BCOp::JMP | BCOp::UCLO),
            "bad bytecode op {:?}",
            self.bc[idx].op()
        );
        self.bc[idx].set_a(vg.slot);
        if let BCOp::JMP = self.bc[idx].op() {
            let next = self.jmp_next(idx);
            if let Some(next) = next {
                self.jmp_patch(next, idx);
            }
            self.bc[idx].set_op(BCOp::UCLO);
            self.bc[idx].set_j(NO_JMP as i16);
        }
    }

    pub fn gola_resolve(&mut self, var_info: &mut Vec<VarInfo>, scope: &FuncScope, idx: usize) {
        let vl = var_info.drain(idx..).next().unwrap();
        let start = scope.vstart as usize;
        for vg in &mut var_info[start..] {
            if vg.name == vl.name && vg.is_goto() {
                assert!(vg.slot >= vl.slot, "bad goto");

                self.gola_patch(vg, &vl);
            }
        }
    }

    pub fn gola_fixup(&mut self, var_info: &mut [VarInfo], dead_scope: &FuncScope) {
        let start = dead_scope.vstart as usize;

        for idx in start..var_info.len() {
            if let Some(name) = var_info[idx].name.as_ref() {
                if var_info[idx].is_label() {
                    var_info[idx].name = None;
                    let ([.., vl], rest) = var_info.split_at_mut(idx + 1) else {
                        panic!("empty var_info")
                    };

                    for vg in rest {
                        if vg.name == vl.name && vg.is_goto() {
                            if dead_scope.flags.contains(ScopeFlags::UPVAL) && vg.slot > vl.slot {
                                self.gola_close(vg);
                            }

                            self.gola_patch(vg, vl);
                        }
                    }
                } else if var_info[idx].is_goto() {
                    self.scope.flags |= if name.as_ref() == NAME_BREAK {
                        ScopeFlags::BREAK
                    } else {
                        ScopeFlags::GOLA
                    };

                    var_info[idx].slot = dead_scope.nactvar;
                    if dead_scope.flags.contains(ScopeFlags::UPVAL) {
                        self.gola_close(&var_info[idx]);
                    }
                }
            }
        }
    }
}
