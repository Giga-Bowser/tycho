use crate::typecheck::pool::{TypePool, TypeRef};

pub struct FuncCtxStack {
    vec: Vec<FuncCtx>,
}

impl FuncCtxStack {
    pub fn new() -> Self {
        Self {
            vec: vec![FuncCtx::top()],
        }
    }

    /// push a new FuncCtx to the stack with an expected return type
    pub fn push(&mut self, ret_ty: TypeRef) {
        self.vec.push(FuncCtx {
            ret_ty,
            has_return: false,
        });
    }

    pub fn pop(&mut self) {
        self.vec.pop();
    }

    fn top(&self) -> &FuncCtx {
        unsafe { self.vec.last().unwrap_unchecked() }
    }

    fn top_mut(&mut self) -> &mut FuncCtx {
        unsafe { self.vec.last_mut().unwrap_unchecked() }
    }

    pub fn ret_ty(&self) -> TypeRef {
        assert!(self.vec.len() > 1, "FuncCtxStack cannot be empty");
        self.vec.last().map_or(TypePool::any(), |it| it.ret_ty)
    }

    pub fn has_return(&self) -> bool {
        self.top().has_return
    }

    pub fn set_return(&mut self, setting: bool) {
        self.top_mut().has_return = setting;
    }
}

impl Default for FuncCtxStack {
    fn default() -> Self {
        Self::new()
    }
}

pub(super) struct FuncCtx {
    pub ret_ty: TypeRef,
    pub has_return: bool,
}

impl FuncCtx {
    pub(super) fn top() -> Self {
        FuncCtx {
            ret_ty: TypePool::any(),
            has_return: false,
        }
    }
}
