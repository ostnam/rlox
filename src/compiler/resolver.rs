use crate::arena::{Arena, Ref};
use crate::ast::Function;
use crate::refs_eql;

pub struct Resolver {
    locals: Vec<Local>,
    scopes: Vec<Scope>,
}

#[derive(Clone, Debug)]
pub struct Local {
    pub name: Ref<String>,
    pub initialized: bool,
}

pub enum StackRef {
    Local(usize),
    ClosedOver(usize),
}

struct Scope {
    kind: ScopeKind,
    start: usize,
    length: usize,
}

impl Scope {
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
        for (stack_pos, local) in self.locals.iter().rev().enumerate() {
            if refs_eql!(str_arena, local.name, name) {
                let mut closed_over = false;
                for scope in self.scopes.iter().rev() {
                    if scope.owns(stack_pos) {
                        if closed_over {
                            return Some(StackRef::ClosedOver(stack_pos));
                        } else {
                            return Some(StackRef::Local(stack_pos));
                        }
                    }
                    if !closed_over && scope.kind == ScopeKind::Function {
                        closed_over = true;
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
        self.scopes.pop().map_or(0, |scope| scope.length)
    }

    pub fn begin_fn_scope(&mut self, f: &Function) {
        todo!()
    }

    pub fn end_fn_scope(&mut self) {
        todo!()
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Resolver { locals: Vec::new(), scopes: Vec::new() }
    }
}
