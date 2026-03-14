//! this module implements referencing and dereferencing logic for MirValues

use crate::mir::mir_expr::mir_graph::{BorrowGraph, ConstFrame};
use crate::mir::mir_expr::{MirFlowGraph, MirGraphElement, MirValue, StackFrameLayout};
use crate::mir::mir_type::{MemberOffset, MirAggregateTypeLayout, MirTypeId, MirTypeRegistry};
use crate::prelude::ExecutorVM;


#[derive(Debug, Clone, PartialEq)]
/// Creates a reference from a value type.
/// The base value that is referenced can either be the actual owner, or another reference.
///
/// # References of References
///
/// If the source value for this reference has a reference type, then we don't create a new
/// reference from an owned value, but effectively add an offset onto the internal pointer value in
/// the original reference.
/// The implications for the lifetime however are identical in both cases.
///
/// # Note On Mutable References
///
/// If `mutable = true` then the generated reference is mutable.
/// This means two things; for one,
pub struct MirRef {
    pub mutable: bool,
    pub value: MirValue,
    pub(super) offset: RefOffset,
    pub ty: MirTypeId,
    src_ty: MirTypeId,
}

impl MirGraphElement for MirRef {
    fn collect_vars(&self) -> Vec<MirValue> {
        let mut out = vec![self.value];
        match &self.offset {
            RefOffset::Const(_) => (),
            RefOffset::ArrayIndex { index, .. } => {
                out.push(*index);
            }
            RefOffset::SliceIndex { index, slice_size, .. } => {
                out.push(*index);
                out.push(*slice_size);
            }
            RefOffset::ArrayRange { start, end, .. } => {
                out.push(*start);
                out.push(*end);
            }
            RefOffset::SliceRange { start, end, slice_size, .. } => {
                out.push(*start);
                out.push(*end);
                out.push(*slice_size);
            }
            RefOffset::Entire => (),
        }
        out
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        &self.value == val || match &self.offset {
            RefOffset::Const(_) => false,
            RefOffset::ArrayIndex { index, .. } => {
                index == val
            }
            RefOffset::SliceIndex { index, slice_size, .. } => {
                index == val || slice_size == val
            }
            RefOffset::ArrayRange { start, end, .. } => {
                start == val || end == val
            }
            RefOffset::SliceRange { start, end, slice_size, .. } => {
                start == val || end == val || slice_size == val
            }
            RefOffset::Entire => false,
        }
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        if &self.value == var {
            self.value = *repl;
        }
        match &mut self.offset {
            RefOffset::Const(_) => (),
            RefOffset::ArrayIndex { index, .. } => {
                if index == var {
                    *index = *repl;
                }
            }
            RefOffset::SliceIndex { index, slice_size, .. } => {
                if index == var {
                    *index = *repl;
                }
                if slice_size == var {
                    *slice_size = *repl;
                }
            }
            RefOffset::ArrayRange { start, end, .. } => {
                if start == var {
                    *start = *repl;
                }
                if end == var {
                    *end = *repl;
                }
            }
            RefOffset::SliceRange { start, end, slice_size, .. } => {
                if start == var {
                    *start = *repl;
                }
                if end == var {
                    *end = *repl;
                }
                if slice_size == var {
                    *slice_size = *repl;
                }
            }
            RefOffset::Entire => (),
        }
    }
}

impl MirRef {
    pub fn shared(
        value: MirValue,
        ty: MirTypeId,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_ref_type(&ty).unwrap();
        assert_eq!(src_ty, base_type);

        Self {
            value,
            ty,
            offset: RefOffset::Entire,
            src_ty,
            mutable: false,
        }
    }

    pub fn mutable(
        value: MirValue,
        ty: MirTypeId,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        assert!(reg.is_ref_mutable(&ty));
        let base_type = reg.get_ref_type(&ty).unwrap();
        assert_eq!(src_ty, base_type);

        Self {
            value,
            ty,
            offset: RefOffset::Entire,
            src_ty,
            mutable: true,
        }
    }

    pub fn shared_field(
        value: MirValue,
        field: &str,
        ty: MirTypeId,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_ref_type(&src_ty).unwrap_or(src_ty);
        let layout = reg.get_layout(base_type).unwrap();
        let offset = layout.member_offset(field, reg).unwrap();

        MirRef {
            mutable: false,
            value,
            offset: RefOffset::Const(offset),
            ty,
            src_ty,
        }
    }

    pub fn shared_enum_field(
        value: MirValue,
        variant: &str,
        field: &str,
        ty: MirTypeId,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_ref_type(&src_ty).unwrap_or(src_ty);
        let layout = reg.get_layout(base_type).unwrap();
        let offset = layout.enum_variant_offset(variant, field, reg).unwrap();

        MirRef {
            mutable: false,
            value,
            offset: RefOffset::Const(offset),
            ty,
            src_ty,
        }
    }

    pub fn shared_array_index(
        value: MirValue,
        index: MirValue,
        ty: MirTypeId,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_ref_type(&src_ty).unwrap_or(src_ty);
        let (element_ty, array_size) = reg.get_array_type(&base_type).unwrap();
        assert_eq!(element_ty, reg.get_ref_type(&ty).unwrap());

        MirRef {
            mutable: false,
            value,
            offset: RefOffset::ArrayIndex { index, array_size, element_ty },
            ty,
            src_ty,
        }
    }

    pub fn shared_slice_index(
        value: MirValue,
        index: MirValue,
        ty: MirTypeId,
        slice_length: MirValue,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_ref_type(&src_ty)
            .unwrap_or(src_ty);
        let element_ty = reg.get_slice_type(&base_type).unwrap();
        assert_eq!(element_ty, reg.get_ref_type(&ty).unwrap());

        MirRef {
            mutable: false,
            value,
            offset: RefOffset::SliceIndex { index, slice_size: slice_length, element_ty },
            ty,
            src_ty,
        }
    }

    pub fn shared_array_slice(
        value: MirValue,
        start: MirValue,
        end: MirValue,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
        ty: MirTypeId,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_ref_type(&src_ty).unwrap_or(src_ty);
        let (array_element, array_size) = reg.get_array_type(&base_type).unwrap();
        let target_element = reg.get_slice_type(&ty).unwrap();
        assert_eq!(array_element, target_element);

        MirRef {
            mutable: false,
            value,
            offset: RefOffset::ArrayRange { start, end, array_size, element_ty: array_element },
            ty,
            src_ty
        }
    }

    pub fn mut_field(
        value: MirValue,
        field: &str,
        ty: MirTypeId,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        assert!(reg.is_ref_mutable(&src_ty));
        let base_type = reg.get_ref_type(&src_ty).unwrap_or(src_ty);
        let layout = reg.get_layout(base_type).unwrap();
        let offset = layout.member_offset(field, reg).unwrap();

        MirRef {
            mutable: true,
            value,
            offset: RefOffset::Const(offset),
            ty,
            src_ty,
        }
    }

    pub fn mut_enum_field(
        value: MirValue,
        variant: &str,
        field: &str,
        ty: MirTypeId,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        assert!(reg.is_ref_mutable(&src_ty));
        let base_type = reg.get_ref_type(&src_ty).unwrap_or(src_ty);
        let layout = reg.get_layout(base_type).unwrap();
        let offset = layout.enum_variant_offset(variant, field, reg).unwrap();

        MirRef {
            mutable: true,
            value,
            offset: RefOffset::Const(offset),
            ty,
            src_ty,
        }
    }

    pub fn mut_array_index(
        value: MirValue,
        index: MirValue,
        ty: MirTypeId,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        assert!(reg.is_ref_mutable(&src_ty));
        let base_type = reg.get_ref_type(&src_ty)
            .unwrap_or(src_ty);
        let (element_ty, array_size) = reg.get_array_type(&base_type).unwrap();
        assert_eq!(element_ty, reg.get_ref_type(&ty).unwrap());

        MirRef {
            mutable: true,
            value,
            offset: RefOffset::ArrayIndex { index, array_size, element_ty },
            ty,
            src_ty,
        }
    }

    pub fn mut_slice_index(
        value: MirValue,
        index: MirValue,
        ty: MirTypeId,
        slice_length: MirValue,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        assert!(reg.is_ref_mutable(&src_ty));
        let base_type = reg.get_ref_type(&src_ty)
            .unwrap_or(src_ty);
        let element_ty = reg.get_slice_type(&base_type).unwrap();
        assert_eq!(element_ty, reg.get_ref_type(&ty).unwrap());

        MirRef {
            mutable: true,
            value,
            offset: RefOffset::SliceIndex { index, slice_size: slice_length, element_ty },
            ty,
            src_ty,
        }
    }

    pub fn mut_array_slice(
        value: MirValue,
        start: MirValue,
        end: MirValue,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
        ty: MirTypeId,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        assert!(reg.is_ref_mutable(&src_ty));
        let base_type = reg.get_ref_type(&src_ty)
            .unwrap_or(src_ty);
        let (array_element, array_size) = reg.get_array_type(&base_type).unwrap();
        let target_element = reg.get_slice_type(&ty).unwrap();
        assert_eq!(array_element, target_element);

        MirRef {
            mutable: true,
            value,
            offset: RefOffset::ArrayRange { start, end, array_size, element_ty: array_element },
            ty,
            src_ty
        }
    }

    /// The reference operator creates a subset of another reference, if the source is already a
    /// reference type.
    pub fn is_reference_from_owner(&self, reg: &MirTypeRegistry) -> bool {
        reg.is_ref(&self.src_ty)
    }

    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: &MirValue,
        reg: &MirTypeRegistry,
    ) {
        let (range, ty) = stack_frame.get_offset(&self.value, vm).unwrap();
        let base_ty = reg
            .get_ref_type(&self.ty)
            .unwrap();

        let ptr = if reg.is_ref(&ty) {
            // the base value is already a reference.
            // for all offset calculations, we read `value` as a pointer and then add the offset to
            // that
            // if this operator is a reference to the entire pointer, then we ignore this value and
            // create a pointer of a pointer instead (see match clause below)
            vm.read(self.value, stack_frame, reg).unwrap()
        } else {
            // the base value is not a reference, which means that we have to create a new reference
            // to the memory region in which the base value resides.
            vm.get_data(range.clone(), ty).as_ptr()
        };

        match &self.offset {
            RefOffset::Entire => {
                // in either case, we create a reference to the base value and do not just repeat
                // the reference
                let data = vm.get_data(range.clone(), base_ty);
                unsafe { vm.write_ptr(*target, data.as_ptr(), stack_frame, reg) };
            }
            RefOffset::Const(const_offset) => {
                let data = unsafe { ptr.add(const_offset.offset) };
                unsafe { vm.write_ptr(*target, data, stack_frame, reg) };
            }
            RefOffset::ArrayIndex { index, array_size, element_ty } => {
                let element_size = reg.byte_size(*element_ty).unwrap();
                let index: usize = vm.read(*index, stack_frame, reg).unwrap();
                let offset = element_size * index;
                assert!(index < *array_size);
                let data = unsafe { ptr.add(offset) };
                unsafe { vm.write_ptr(*target, data, stack_frame, reg) };
            }
            RefOffset::SliceIndex { index, slice_size, element_ty } => {
                let element_size = reg.byte_size(*element_ty).unwrap();
                let index: usize = vm.read(*index, stack_frame, reg).unwrap();
                let offset = element_size * index;
                let slice_size: usize = vm.read(*slice_size, stack_frame, reg).unwrap();
                assert!(index < slice_size);
                let data = unsafe { ptr.add(offset) };
                unsafe { vm.write_ptr(*target, data, stack_frame, reg) };
            }
            RefOffset::ArrayRange { start, end, array_size, element_ty } => {
                let element_size = reg.byte_size(*element_ty).unwrap();
                let start: usize = vm.read(*start, stack_frame, reg).unwrap();
                let end: usize = vm.read(*end, stack_frame, reg).unwrap();
                assert!(end <= *array_size);
                let data = unsafe { ptr.add(start * element_size) };
                vm.write_fat_ptr(*target, data, end - start, stack_frame, reg);
            }
            RefOffset::SliceRange { start, end, slice_size, element_ty } => {
                let element_size = reg.byte_size(*element_ty).unwrap();
                let start: usize = vm.read(*start, stack_frame, reg).unwrap();
                let end: usize = vm.read(*end, stack_frame, reg).unwrap();
                let slice_size: usize = vm.read(*slice_size, stack_frame, reg).unwrap();
                assert!(end <= slice_size);
                let data = unsafe {  ptr.add(start * element_size) };
                vm.write_fat_ptr(*target, data, end - start, stack_frame, reg);
            }
        }
    }

    pub(super) fn is_avail(
        &self,
        frame: &ConstFrame,
        graph: &BorrowGraph,
    ) -> bool {
        match &self.offset {
            RefOffset::Entire => frame.is_avail(&self.value, graph),
            RefOffset::Const(_) => frame.is_avail(&self.value, graph),
            RefOffset::ArrayIndex { index, .. } => {
                frame.is_avail(&self.value, graph) && frame.is_avail(index, graph)
            }
            RefOffset::SliceIndex { index, slice_size, .. } => {
                frame.is_avail(&self.value, graph) && frame.is_avail(index, graph) && frame.is_avail(slice_size, graph)
            }
            RefOffset::ArrayRange { start, end, .. } => {
                frame.is_avail(&self.value, graph) && frame.is_avail(start, graph) && frame.is_avail(end, graph)
            }
            RefOffset::SliceRange { start, end, slice_size, .. } => {
                frame.is_avail(&self.value, graph)
                    && frame.is_avail(start, graph)
                    && frame.is_avail(end, graph)
                    && frame.is_avail(slice_size, graph)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum RefOffset {
    /// The entire object is referenced.
    Entire,
    /// A member of the object is referenced.
    Const(MemberOffset),
    /// A specific index from an array is referenced.
    ArrayIndex {
        index: MirValue,
        array_size: usize,
        element_ty: MirTypeId,
    },
    /// A specified index from a slice is referenced.
    SliceIndex {
        index: MirValue,
        slice_size: MirValue,
        element_ty: MirTypeId,
    },
    /// A specific range from an array is referenced.
    ArrayRange {
        start: MirValue,
        end: MirValue,
        array_size: usize,
        element_ty: MirTypeId,
    },
    /// A specific range from a slice is referenced.
    SliceRange {
        start: MirValue,
        end: MirValue,
        slice_size: MirValue,
        element_ty: MirTypeId,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoundsCheck {
    pub start: MirValue,
    pub end: MirValue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirDeref {
    pub ty: MirTypeId,
    pub value: MirValue,
}

impl MirGraphElement for MirDeref {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![self.value]
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        &self.value == val
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        if &self.value == var {
            self.value = *repl;
        }
    }
}

impl MirDeref {
    pub fn new(value: MirValue, graph: &MirFlowGraph, reg: &MirTypeRegistry) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let ty = reg.get_ref_type(&src_ty)
            .expect("only reference types can be dereferenced");
        MirDeref {
            ty,
            value,
        }
    }

    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: &MirValue,
        reg: &MirTypeRegistry,
    ) {
        let (_, value_ty) = stack_frame.get_offset(&self.value, vm).unwrap();
        let ptr: *const u8 = vm.read(self.value, stack_frame, reg).unwrap();

        let (target_range, target_ty) = stack_frame.get_offset(target, vm).unwrap();
        assert_eq!(reg.get_ref_type(&value_ty).unwrap(), target_ty);

        let [mut target_buf] = vm.get_data_mut([target_range.clone()], &[target_ty]);
        unsafe {
            target_buf.read_ptr(ptr);
        }
    }

    pub(super) fn is_avail(
        &self,
        frame: &ConstFrame,
        graph: &BorrowGraph,
    ) -> bool {
        frame.is_deref_avail(&self.value, graph)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirDowncastRef {
    pub ty: MirTypeId,
    pub value: MirValue,
}

impl MirGraphElement for MirDowncastRef {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![self.value]
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        &self.value == val
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        if &self.value == var {
            self.value = *repl;
        }
    }
}

impl MirDowncastRef {
    pub fn new(value: MirValue, target_ty: MirTypeId, graph: &MirFlowGraph, reg: &MirTypeRegistry) -> Self {
        let src_ty = *graph.get_var_type(&value);
        assert!(reg.is_ref_mutable(&src_ty));
        let src_base_ty = reg.get_ref_type(&src_ty)
            .expect("only mutable references can be downcast to immutable references");
        let target_base_ty = reg.get_ref_type(&target_ty)
            .expect("target of a downcast operator has to be a shared reference");
        assert_eq!(src_base_ty, target_base_ty);
        MirDowncastRef {
            ty: target_ty,
            value,
        }
    }

    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: &MirValue,
        reg: &MirTypeRegistry,
    ) {
        let (_, target_ty) = stack_frame.get_offset(target, vm).unwrap();
        let (_, value_ty) = stack_frame.get_offset(&self.value, vm).unwrap();
        assert!(reg.is_ref_mutable(&value_ty));
        assert_eq!(reg.get_ref_type(&target_ty).unwrap(), reg.get_ref_type(&value_ty).unwrap());
        vm.memcpy_slice(&[*target], &[self.value], stack_frame);
    }

    pub(super) fn is_avail(
        &self,
        frame: &ConstFrame,
        graph: &BorrowGraph,
    ) -> bool {
        frame.is_avail(&self.value, graph)
    }
}
