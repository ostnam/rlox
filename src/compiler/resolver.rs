use std::fmt::Display;

use crate::arena::{Arena, Ref};
use crate::chunk::{RelativeStackIdx, Upvalue};
use crate::refs_eql;

pub struct Resolver {
    /// Stack at a given time.
    locals: Vec<Local>,

    /// Every scope at a given time.
    scopes: Vec<Scope>,
    next_scope_id: usize,
}

#[derive(Debug)]
pub enum Error {
    LocalAlreadyDeclared,
    ImbalancedScope,
    WrongScope,
    WrongScopeKind,
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LocalAlreadyDeclared => write!(f, "variable name already used in the same scope"),
            Self::ImbalancedScope => write!(f, "INTERNAL BUG:  ended scope but no scope currently active"),
            Self::WrongScope => write!(f, "INTERNAL BUG: scopes closed in the wrong order"),
            Self::WrongScopeKind => write!(f, "INTERNAL BUG: tried to close the wrong scope kind"),
        }
    }
}

/// A variable on the stack.
#[derive(Clone, Debug)]
struct Local {
    name: Ref<String>,

    /// Set to false at the beginning of the `var` statement, and to true at the end.
    /// Ensures that a variable being declared can't be accessed in its
    /// initialization code.
    initialized: bool,
}

pub enum StackRef {
    Local(RelativeStackIdx),
    Upval(usize),
}

struct Scope {
    kind: ScopeKind,

    /// `Scope` with >= 1 variable: index of the first variable.
    /// `Scope` with 0 variables: index of the first variable, if there was one.
    start: usize,

    /// Number of variables belonging to the `Scope`.
    length: usize,

    id: ScopeId,
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[must_use]
pub struct ScopeId(usize);

impl Scope {
    /// Returns whether the given `Scope` owns the variable with the stack index `idx`.
    fn owns(&self, idx: usize) -> bool {
        (self.start <= idx) && (self.start + self.length > idx)
    }
}

#[derive(PartialEq, Eq)]
enum ScopeKind {
    Block,
    Function(Vec<Upvalue>),
}

impl Resolver {
    /// Registers a new local in the current scope.
    /// If there are no valid scopes currently, does nothing.
    pub fn declare_local(
        &mut self,
        arena: &Arena<String>,
        name: Ref<String>,
    ) -> Result<(), Error> {
        debug_assert!(self.scopes.len() > 0);
        for local in self.current_scope() {
            if refs_eql!(arena, name, local.name) {
                return Err(Error::LocalAlreadyDeclared);
            }
        }
        self.locals.push(Local {
            name,
            initialized: false,
        });
        self.scopes
            .last_mut()
            .map(|scope| scope.length += 1);
        Ok(())
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

    /// For a given name, if a variable with that name is available
    /// on the stack, returns `Some(StackRef)`. `StackRef` can be used
    /// as parameter to a {Get,Set}{Local,Upval} instruction.
    /// If needed, register the upvalue in closures.
    pub fn resolve(
        &mut self,
        str_arena: &Arena<String>,
        name: Ref<String>,
    ) -> Option<StackRef> {
        match self.get_var_scope(str_arena, name) {
            Some((scope_idx, pos)) if self.same_fn_scope(scope_idx) =>
                Some(StackRef::Local(pos)),

            Some((scope_idx, r@RelativeStackIdx(_))) => {
                // the variable was declared in an outer function scope.
                // It needs to be declared as an upvalue in this scope,
                // and all the intermediate ones.
                let mut next_upval = Upvalue::Local(r);
                let mut last_upval_idx = 0;

                'scopes:
                for scope in self.scopes[scope_idx + 1..].iter_mut() {
                    // don't register upvals unless it's a function scope.
                    if let Scope { kind: ScopeKind::Function(upvals), .. } = scope {
                        // don't register the same upvalue twice
                        for (idx, upval) in upvals.iter().enumerate() {
                            if *upval == next_upval {
                                next_upval = Upvalue::Parent(idx);
                                last_upval_idx = idx;
                                continue 'scopes;
                            }
                        }
                        upvals.push(next_upval);
                        last_upval_idx = upvals.len() - 1;
                        next_upval = Upvalue::Parent(last_upval_idx);
                    }
                }
                Some(StackRef::Upval(last_upval_idx))
            }

            None => None,
        }
    }

    /// Returns the scope, and the variable position in that scope.
    fn get_var_scope(
        &self,
        str_arena: &Arena<String>,
        name: Ref<String>,
    ) -> Option<(usize, RelativeStackIdx)> {
        for (stack_pos, var) in self.locals.iter().enumerate() {
            if !refs_eql!(str_arena, var.name, name) {
                continue;
            }
            for (scope_idx, scope) in self.scopes.iter().enumerate().rev() {
                if scope.owns(stack_pos) {
                    return Some((scope_idx, RelativeStackIdx(stack_pos - scope.start)));
                }
            }
        }
        None
    }

    pub fn current_scope_depth(&self) -> usize {
        self.scopes.len()
    }

    pub fn begin_scope(&mut self) -> ScopeId {
        let id = self.gen_scope_id();
        self.scopes.push(Scope {
            start: self.locals.len(),
            length: 0,
            kind: ScopeKind::Block,
            id,
        });
        id
    }

    pub fn end_scope(&mut self, expected: ScopeId) -> Result<usize, Error> {
        match self.scopes.pop() {
            Some(Scope { kind: ScopeKind::Function(_), .. }) => Err(Error::WrongScopeKind),
            Some(Scope { id, .. }) if id != expected => Err(Error::WrongScope),
            None => Err(Error::ImbalancedScope),
            Some(Scope { length, .. }) => {
                self.locals.truncate(self.locals.len() - length);
                Ok(length)
            }
        }
    }

    pub fn begin_fn_scope(
        &mut self,
        strings: &Arena<String>,
        args: &[Ref<String>],
    ) -> ScopeId {
        let id = self.gen_scope_id();
        self.scopes.push(Scope {
            start: self.locals.len(),
            length: 0,
            kind: ScopeKind::Function(Vec::new()),
            id,
        });
        for arg in args {
            self.declare_local(strings, *arg);
            self.init_last_local();
        }
        id
    }

    pub fn end_fn_scope(&mut self, expected: ScopeId) -> Result<Vec<Upvalue>, Error> {
        match self.scopes.pop() {
            Some(Scope { kind: ScopeKind::Block, .. }) => Err(Error::WrongScopeKind),
            Some(Scope { id, .. }) if id != expected => Err(Error::WrongScope),
            None => Err(Error::ImbalancedScope),
            Some(Scope { kind: ScopeKind::Function(upvals), length, .. }) => {
                self.locals.truncate(self.locals.len() - length);
                Ok(upvals)
            }
        }
    }

    fn gen_scope_id(&mut self) -> ScopeId {
        self.next_scope_id += 1;
        ScopeId(self.next_scope_id - 1)
    }

    fn current_scope(&self) -> std::slice::Iter<'_, Local> {
        self.locals[self.scopes.last().unwrap().start..].iter()
    }

    fn same_fn_scope(&self, scope_idx: usize) -> bool {
        for (idx, scope) in self.scopes.iter().enumerate().rev() {
            if scope_idx == idx {
                return true;
            }
            if let ScopeKind::Function(_) = scope.kind {
                return false;
            }
        }
        return true;
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Resolver {
            locals: Vec::new(),
            scopes: Vec::new(),
            next_scope_id: 0, 
        }
    }
}
