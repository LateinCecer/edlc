//! this module implements referencing and dereferencing logic for MirValues

use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::mir::mir_expr::{MirFlowGraph, MirGraphElement, MirValue};
use crate::mir::mir_type::{MemberOffset, MirAggregateTypeLayout, MirTypeId, MirTypeRegistry};

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
    pub src: ModuleSrc,
    pub pos: SrcPos,
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
            RefOffset::SliceIndex { index, slice_size } => {
                out.push(*index);
                out.push(*slice_size);
            }
            RefOffset::ArrayRange { start, end, .. } => {
                out.push(*start);
                out.push(*end);
            }
            RefOffset::SliceRange { start, end, slice_size } => {
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
            RefOffset::SliceIndex { index, slice_size } => {
                index == val || slice_size == val
            }
            RefOffset::ArrayRange { start, end, .. } => {
                start == val || end == val
            }
            RefOffset::SliceRange { start, end, slice_size } => {
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
            RefOffset::SliceIndex { index, slice_size } => {
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
            RefOffset::SliceRange { start, end, slice_size } => {
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
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        assert!(!reg.is_ref(&src_ty) && !reg.is_mut_ref(&src_ty));
        let base_type = reg.get_ref_type(&ty).unwrap();
        assert_eq!(src_ty, base_type);

        Self {
            value,
            ty,
            src,
            pos,
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
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        assert!(!reg.is_ref(&src_ty) && !reg.is_mut_ref(&src_ty));
        let base_type = reg.get_mut_ref_type(&ty).unwrap();
        assert_eq!(src_ty, base_type);

        Self {
            value,
            ty,
            src,
            pos,
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
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_ref_type(&src_ty)
            .or_else(|| reg.get_mut_ref_type(&src_ty))
            .unwrap_or(src_ty);
        let layout = reg.get_layout(base_type).unwrap();
        let offset = layout.member_offset(field, reg).unwrap();

        MirRef {
            mutable: false,
            value,
            src,
            pos,
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
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_ref_type(&src_ty)
            .or_else(|| reg.get_mut_ref_type(&src_ty))
            .unwrap_or(src_ty);
        let layout = reg.get_layout(base_type).unwrap();
        let offset = layout.enum_variant_offset(variant, field, reg).unwrap();

        MirRef {
            mutable: false,
            value,
            src,
            pos,
            offset: RefOffset::Const(offset),
            ty,
            src_ty,
        }
    }

    pub fn shared_array_index(
        value: MirValue,
        index: MirValue,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_ref_type(&src_ty)
            .or_else(|| reg.get_mut_ref_type(&src_ty))
            .unwrap_or(src_ty);
        let (array_element, array_size) = reg.get_array_type(&base_type).unwrap();
        MirRef {
            mutable: false,
            value,
            src,
            pos,
            offset: RefOffset::ArrayIndex { index, array_size },
            ty: array_element,
            src_ty,
        }
    }

    pub fn shared_slice_index(
        value: MirValue,
        index: MirValue,
        slice_length: MirValue,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_ref_type(&src_ty)
            .or_else(|| reg.get_mut_ref_type(&src_ty))
            .unwrap_or(src_ty);
        let slice_element = reg.get_slice_type(&base_type).unwrap();
        MirRef {
            mutable: false,
            value,
            src,
            pos,
            offset: RefOffset::SliceIndex { index, slice_size: slice_length },
            ty: slice_element,
            src_ty,
        }
    }

    pub fn shared_array_slice(
        value: MirValue,
        start: MirValue,
        end: MirValue,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
        ty: MirTypeId,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_ref_type(&src_ty)
            .or_else(|| reg.get_mut_ref_type(&src_ty))
            .unwrap_or(src_ty);
        let (array_element, array_size) = reg.get_array_type(&base_type).unwrap();
        let target_element = reg.get_slice_type(&ty).unwrap();
        assert_eq!(array_element, target_element);

        MirRef {
            mutable: false,
            value,
            src,
            pos,
            offset: RefOffset::ArrayRange { start, end, array_size },
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
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_mut_ref_type(&src_ty)
            .unwrap_or(src_ty);
        let layout = reg.get_layout(base_type).unwrap();
        let offset = layout.member_offset(field, reg).unwrap();

        MirRef {
            mutable: true,
            value,
            src,
            pos,
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
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_mut_ref_type(&src_ty)
            .unwrap_or(src_ty);
        let layout = reg.get_layout(base_type).unwrap();
        let offset = layout.enum_variant_offset(variant, field, reg).unwrap();

        MirRef {
            mutable: true,
            value,
            src,
            pos,
            offset: RefOffset::Const(offset),
            ty,
            src_ty,
        }
    }

    pub fn mut_array_index(
        value: MirValue,
        index: MirValue,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_mut_ref_type(&src_ty)
            .unwrap_or(src_ty);
        let (array_element, array_size) = reg.get_array_type(&base_type).unwrap();
        MirRef {
            mutable: true,
            value,
            src,
            pos,
            offset: RefOffset::ArrayIndex { index, array_size },
            ty: array_element,
            src_ty,
        }
    }

    pub fn mut_slice_index(
        value: MirValue,
        index: MirValue,
        slice_length: MirValue,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_mut_ref_type(&src_ty)
            .unwrap_or(src_ty);
        let slice_element = reg.get_slice_type(&base_type).unwrap();
        MirRef {
            mutable: true,
            value,
            src,
            pos,
            offset: RefOffset::SliceIndex { index, slice_size: slice_length },
            ty: slice_element,
            src_ty,
        }
    }

    pub fn mut_array_slice(
        value: MirValue,
        start: MirValue,
        end: MirValue,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
        ty: MirTypeId,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let base_type = reg.get_mut_ref_type(&src_ty)
            .unwrap_or(src_ty);
        let (array_element, array_size) = reg.get_array_type(&base_type).unwrap();
        let target_element = reg.get_slice_type(&ty).unwrap();
        assert_eq!(array_element, target_element);

        MirRef {
            mutable: true,
            value,
            src,
            pos,
            offset: RefOffset::ArrayRange { start, end, array_size },
            ty,
            src_ty
        }
    }

    /// The reference operator creates a subset of another reference, if the source is already a
    /// reference type.
    pub fn is_reference_from_owner(&self, reg: &MirTypeRegistry) -> bool {
        reg.is_ref(&self.src_ty) || reg.is_mut_ref(&self.src_ty)
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
    },
    /// A specified index from a slice is referenced.
    SliceIndex {
        index: MirValue,
        slice_size: MirValue,
    },
    /// A specific range from an array is referenced.
    ArrayRange {
        start: MirValue,
        end: MirValue,
        array_size: usize,
    },
    /// A specific range from a slice is referenced.
    SliceRange {
        start: MirValue,
        end: MirValue,
        slice_size: MirValue,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoundsCheck {
    pub start: MirValue,
    pub end: MirValue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirDeref {
    pub pos: SrcPos,
    pub src: ModuleSrc,
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
    pub fn new(value: MirValue, graph: &MirFlowGraph, reg: &MirTypeRegistry, pos: SrcPos, src: ModuleSrc) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let ty = reg.get_ref_type(&src_ty)
            .or_else(|| reg.get_mut_ref_type(&src_ty))
            .expect("only reference types can be dereferenced");
        MirDeref {
            pos,
            src,
            ty,
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirDowncastRef {
    pub pos: SrcPos,
    pub src: ModuleSrc,
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
    pub fn new(value: MirValue, target_ty: MirTypeId, graph: &MirFlowGraph, reg: &MirTypeRegistry, pos: SrcPos, src: ModuleSrc) -> Self {
        let src_ty = *graph.get_var_type(&value);
        let src_base_ty = reg.get_mut_ref_type(&src_ty)
            .expect("only mutable references can be downcast to immutable references");
        let target_base_ty = reg.get_ref_type(&target_ty)
            .expect("target of a downcast operator has to be a shared reference");
        assert_eq!(src_base_ty, target_base_ty);
        MirDowncastRef {
            pos,
            src,
            ty: target_ty,
            value,
        }
    }
}
