/*
 *    Copyright 2025 Adrian Paskert
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

use std::cell::{Ref, RefMut};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::AddAssign;
use log::debug;
use crate::core::EdlVarId;
use crate::hir::HirPhase;
use crate::mir::mir_expr::{MirExprId, MirFlowGraph, MirValue};
use crate::mir::{MirError, MirPhase};
use crate::mir::mir_funcs::{MirFuncId, MirFuncRegistry};


pub enum WriteDest<Addr> {
    Tmp(&'static str),
    NewTmp(&'static str),
    Var(EdlVarId),
    NewVar(EdlVarId),
    Addr(Addr)
}

/// A backend is used in dynamic code generation.
pub trait Backend: Sized {
    type Error: Error;
    type Module;
    type Addr;
    type FuncGen<'a>;
    
    fn eval_const_expr(
        &mut self,
        element: MirValue,
        graph: &mut MirFlowGraph,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<MirExprId, MirError<Self>>;
    fn eval_const_bytes(
        &mut self,
        element: MirValue,
        graph: &MirFlowGraph,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<Vec<u8>, MirError<Self>>;

    fn func_reg(&self) -> Ref<'_, MirFuncRegistry<Self>>;
    fn func_reg_mut(&mut self) -> RefMut<'_, MirFuncRegistry<Self>>;

    /// Returns true if the backend is currently in the process of generating a symbol for the
    /// specified function.
    /// In other words, if this function returns `true`, then the backend is currently in the
    /// process of generating the executable code for the specified function.
    fn is_generating_symbol(&self, func_id: &MirFuncId) -> bool;
}


pub trait InstructionCount<B: Backend> {
    /// Returns an approximation for the number of instructions emitted by an item during codegen.
    /// This approximation should be conservative in the sense that it should never be **lower**
    /// than the actual number of generated instructions.
    /// However, since it is often hard to predict the exact number of instructions due to
    /// optimizations, some padding should be applied when this method is used to allocate storage
    /// for the instructions in memory.
    fn count_instructions(&self, phase: &MirPhase, func_reg: &MirFuncRegistry<B>) -> Result<usize, MirError<B>>;
}


/// A codegen module can be used to generate code using a specified backend type.
/// The code is usually emitted to the backend itself.
/// From there, the backend can be used to either build an executable module, or interpret
/// code directly.
pub trait CodeGen<B>: InstructionCount<B>
where B: Backend {
    fn code_gen(
        &self,
        backend: &mut B::FuncGen<'_>,
        type_reg: &mut MirPhase,
    ) -> Result<(), MirError<B>>;
}



#[derive(Debug)]
pub enum StackError {
    /// The stack does not contain any stack levels, yet some operation way performed on the base
    /// stack layer.
    EmptyStack,
}

impl Display for StackError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StackError::EmptyStack => write!(f, "The stack does not contain any stack levels, \
            yet some operation was attempted on the base level"),
        }
    }
}

#[derive(Copy, Clone)]
pub struct AlignedOffset {
    pub alignment: usize,
    pub offset: usize,
}

impl AlignedOffset {
    pub fn from_value(addr: usize, alignment: usize) -> Self {
        AlignedOffset {
            offset: addr % alignment,
            alignment,
        }
    }

    pub fn adjust(&self, val: usize) -> usize {
        let aligned = if val > 0 {
            ((val - 1) / self.alignment) * self.alignment
        } else {
            val
        };

        if aligned + self.offset >= val {
            aligned + self.offset
        } else {
            aligned + self.alignment + self.offset
        }
    }
}

impl Error for StackError {}

pub struct StackState(BackendStack);


/// Alignment in the host system (64-bit address space is assumed)
pub const MACHINE_ALIGNMENT: usize = 8;

/// The backend stack can be used to build a stack frame representation of all items that reside
/// in the stack.
/// The resulting memory layout can then be used by the compiler backend to build the stack frame
/// layouts used during function execution, or resource initialization.
pub struct BackendStack {
    stack: Vec<StackLevel>,
    base_offset: StackLoc,
    max_size: usize,
}

impl BackendStack {
    pub fn new(offset: usize) -> BackendStack {
        BackendStack {
            stack: Vec::new(),
            base_offset: StackLoc(StackLevel::align(offset, MACHINE_ALIGNMENT)),
            max_size: 0,
        }
    }

    /// Creates a backup state for the backend stack.
    /// The state can later be restored.
    pub fn create_state(&self) -> StackState {
        StackState(BackendStack {
            stack: self.stack.clone(),
            base_offset: self.base_offset,
            max_size: self.max_size,
        })
    }

    /// Restores the state of the backend stack.
    pub fn restore_state(&mut self, state: StackState) {
        *self = state.0;
    }

    /// pushes a new stack level into the backend stack
    pub fn push_level(&mut self) {
        let base_index = if let Some(last) = self.stack.last() {
            last.index
        } else {
            self.base_offset
        };
        self.stack.push(StackLevel::new(base_index));
    }

    /// pops the newest stack level from the backend stack.
    pub fn pop_level(&mut self) {
        let last = self.stack.pop();
        if let Some(last) = last {
            let tmp: usize = last.index.into();
            self.max_size = usize::max(self.max_size, tmp - self.base_offset.0);
            debug!("max stack size adjusted to {} B", self.max_size);
        }
    }

    /// Aligns the current stack level to the specified alignment value.
    /// The parameter `alignment` must be a power of two.
    pub fn align(&mut self, alignment: usize) {
        assert!(alignment.is_power_of_two());
        if let Some(last) = self.stack.last_mut() {
            last.index.0 = StackLevel::align(last.index.0, alignment);
        }
    }

    /// Returns the minimal required size to hose the backend stack during runtime.
    /// This method takes the stack by value, since it should only be executed after the layout
    /// of the stack is fully built.
    pub fn size(&self) -> usize {
        self.max_size
    }

    /// Returns the current size of the stack.
    /// This is different from `size()` in that `size()` returns the maximal size of the stack
    /// since the last time `reset_size()` was called.
    pub fn current_size(&self) -> usize {
        if let Some(last) = self.stack.last() {
            let tmp: usize = last.index.into();
            tmp - self.base_offset.0
        } else {
            0
        }
    }

    pub fn push(
        &mut self,
        loc: LocalMemLoc,
        size: usize,
        alignment: usize
    ) -> Result<StackLoc, StackError> {
        let Some(last) = self.stack.last_mut() else {
            return Err(StackError::EmptyStack);
        };
        Ok(last.push(loc, size, alignment))
    }

    /// Tries to get an item from the stack by searching for its local memory location.
    /// During the search, the stack is search back to front, meaning that the most recent items
    /// in the stack are necessarily visited first.
    pub fn get(&self, loc: &LocalMemLoc) -> Option<StackLoc> {
        for item in self.stack.iter().rev() {
            if let Some(loc) = item.get(loc) {
                return Some(loc);
            }
        }
        None
    }

    pub fn reset_size(&mut self) {
        self.max_size = 0;
        if let Some(last) = self.stack.last() {
            let tmp: usize = last.index.into();
            self.max_size = tmp - self.base_offset.0;
        }
        debug!("max stack size adjusted to {} B", self.max_size);
    }

    /// If the backend stack has a bottom level, than this method returns an iterator for all
    /// variable locations on that bottom level.
    /// Since the bottom level on the executor is usually the global variable stack, this
    /// effectively iterates over all global variables that are currently known to the backend
    /// stack.
    pub fn bottom_level_vars(&self) -> Option<Iter<'_>> {
        self.stack.first().map(|first| first.var_iter())
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LocalMemLoc {
    Var(EdlVarId),
    Temp(&'static str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackLoc(usize);

#[derive(Clone)]
/// Represents a single stack level in a backend stack.
struct StackLevel {
    locals: HashMap<LocalMemLoc, StackLoc>,
    index: StackLoc,
}

impl StackLevel {
    fn new(index: StackLoc) -> StackLevel {
        StackLevel {
            locals: HashMap::default(),
            index,
        }
    }

    /// Pushes an entry into the stack level.
    /// Each stack entry consists of a local memory location, which can refer to either a variable
    /// id or the name of a temporary value, and a byte size.
    /// If a variable with the same name already exists within the back level, the previous entry
    /// will be overwritten.
    /// This method returns the stack location at which the new variable is saved.
    fn push(&mut self, loc: LocalMemLoc, mut size: usize, alignment: usize) -> StackLoc {
        // align the value properly
        self.index.0 = Self::align(self.index.0, alignment);
        size = Self::align(size, alignment);

        let offset = self.index;
        self.index += size;
        *self.locals.entry(loc).or_insert(offset) = offset;
        offset
    }

    fn align(val: usize, alignment: usize) -> usize {
        if val > 0 {
            ((val - 1) / alignment + 1) * alignment
        } else {
            0
        }
    }

    /// Returns the stack location at which the value associated with the specified local memory
    /// location is located.
    fn get(&self, loc: &LocalMemLoc) -> Option<StackLoc> {
        self.locals.get(loc).copied()
    }

    /// Returns an iterator for all variables in the stack level
    fn var_iter(&self) -> Iter<'_> {
        Iter {
            iter: self.locals.iter(),
        }
    }
}

pub struct Iter<'a> {
    iter: std::collections::hash_map::Iter<'a, LocalMemLoc, StackLoc>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = EdlVarId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (next, _) = self.iter.next()?;
            if let LocalMemLoc::Var(var_id) = next {
                break Some(*var_id);
            }
        }
    }
}

impl AddAssign<usize> for StackLoc {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl From<StackLoc> for usize {
    fn from(value: StackLoc) -> Self {
        value.0
    }
}