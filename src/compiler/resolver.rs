use crate::arena::{Arena, Ref};
use crate::ast::Function;
use crate::refs_eql;

pub struct Resolver {
    /// Stack at a given time.
    locals: Vec<Local>,

    /// Every scope at a given time.
    scopes: Vec<Scope>,
}

/// A variable on the stack.
#[derive(Clone, Debug)]
pub struct Local {
    pub name: Ref<String>,

    /// Set to false at the beginning of the `var` statement, and to true at the end.
    /// Ensures that a variable being declared can't be accessed in its
    /// initialization code.
    pub initialized: bool,
}

pub enum StackRef {
    Local(usize),
    ClosedOver(usize),
}

struct Scope {
    kind: ScopeKind,

    /// `Scope` with >= 1 variable: index of the first variable.
    /// `Scope` with 0 variables: index of the first variable, if there was one.
    start: usize,

    /// Number of variables belonging to the `Scope`.
    length: usize,
}

impl Scope {
    /// Returns whether the given `Scope` owns the variable with the stack index `idx`.
    fn owns(&self, idx: usize) -> bool {
        (self.start <= idx) && (self.start + self.length > idx)
    }
}

#[derive(PartialEq, Eq)]
enum ScopeKind {
    Block,
    Function,
}

impl Resolver {
    /// Registers a new local in the current scope.
    /// If there are no valid scopes currently, does nothing.
    pub fn declare_local(&mut self, name: Ref<String>) {
        debug_assert!(self.scopes.len() > 0);
        self.locals.push(Local {
            name,
            initialized: false,
        });
        self.scopes
            .last_mut()
            .map(|scope| scope.length += 1);
    }

    /// The last declared local's `initialized` value is set to `true`.
    pub fn init_last_local(&mut self) {
        self.locals
            .last_mut()
            .map_or_else(
                || {
                    unreachable!("Couldn't init_last_local: no local available");
                },
                |local| local.initialized = true
        );
    }

    pub fn resolve_local(&self, str_arena: &Arena<String>, name: Ref<String>) -> Option<StackRef> {
        for (stack_pos, local) in self.locals.iter().enumerate().rev() {
            if refs_eql!(str_arena, local.name, name) {
                let mut same_fn = true;
                for scope in self.scopes.iter().rev() {
                    if scope.owns(stack_pos) {
                        if same_fn {
                            return Some(StackRef::Local(stack_pos - scope.start))
                        } else {
                            todo!("closures not implemented");
                        }
                    }
                    if let ScopeKind::Function = scope.kind {
                        same_fn = false;
                    }
                }
                unreachable!("Predicate violated: local variables must belong to a scope.");
            }
        }
        None
    }

    pub fn current_scope_depth(&self) -> usize {
        self.scopes.len()
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(Scope {
            start: self.locals.len(),
            length: 0,
            kind: ScopeKind::Block,
        });
    }

    pub fn end_scope(&mut self) -> usize {
        let length = self.scopes.pop().map_or(0, |scope| scope.length);
        self.locals.truncate(self.locals.len() - length);
        length
    }

    pub fn begin_fn_scope(&mut self, f: &Function) {
        self.scopes.push(Scope {
            start: self.locals.len(),
            length: 0,
            kind: ScopeKind::Function,
        });
        for arg in &f.args {
            self.declare_local(*arg);
            self.init_last_local();
        }
    }

    pub fn end_fn_scope(&mut self) {
        let length = self.scopes.pop().map_or(0, |scope| scope.length);
        self.locals.truncate(self.locals.len() - length);
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Resolver { locals: Vec::new(), scopes: Vec::new() }
    }
}
