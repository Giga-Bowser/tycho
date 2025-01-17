use crate::types::Type;

#[derive(Clone, Debug, Default)]
pub struct TypeEnv<'a> {
    pub parent_scopes: Option<&'a TypeEnv<'a>>,
    pub current_scope: Vec<(String, Type)>,
}

impl<'a> TypeEnv<'a> {
    pub fn new_with_parent(parent_scopes: &'a TypeEnv<'a>) -> Self {
        Self {
            parent_scopes: Some(parent_scopes),
            current_scope: Vec::new(),
        }
    }

    pub fn get(&'a self, key: &str) -> Option<&'a Type> {
        self.current_scope
            .iter()
            .rev()
            .find(|pair| pair.0 == key)
            .map(|pair| &pair.1)
            .or_else(|| {
                self.parent_scopes
                    .and_then(|parent_scopes| parent_scopes.get(key))
            })
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Type> {
        self.current_scope
            .iter_mut()
            .rev()
            .find(|pair| pair.0 == key)
            .map(|pair| &mut pair.1)
    }

    pub fn len(&self) -> usize {
        self.parent_len() + self.current_scope.len()
    }

    pub fn parent_len(&self) -> usize {
        self.parent_scopes.map(TypeEnv::len).unwrap_or(0)
    }

    pub fn push(&mut self, key: String, val: Type) {
        self.current_scope.push((key, val));
    }

    pub fn pop(&mut self) -> Option<(String, Type)> {
        self.current_scope.pop()
    }

    pub fn truncate(&mut self, len: usize) {
        let parent_len = self.parent_len();
        assert!(
            len >= parent_len,
            "Attempted to truncate name-to-type mappings outside of the current scope"
        );
        self.current_scope.truncate(len - parent_len);
    }
}
