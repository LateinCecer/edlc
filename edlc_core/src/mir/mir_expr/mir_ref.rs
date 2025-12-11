//! this module implements referencing and dereferencing logic for MirValues

use std::ops::Range;
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
    mutable: bool,
    value: MirValue,
    src: ModuleSrc,
    pos: SrcPos,
    offset: RefOffset,
    ty: MirTypeId,
    src_ty: MirTypeId,
}

impl MirRef {
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

    pub fn shared_index(
        value: MirValue,
        index: MirValue,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        todo!()
    }

    pub fn shared_slice(
        value: MirValue,
        start: MirValue,
        end: MirValue,
        reg: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        todo!()
    }

    pub fn mutable_field(
        value: MirValue,
        field: &str,
        ty: MirTypeId,
        graph: &MirFlowGraph,
        reg: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Self {
        let src_ty = *graph.get_var_type(&value);
        if reg.is_ref(&src_ty) {
            panic!("cannot create mutable reference from un-mutable parent");
        }
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

    /// The reference operator creates a subset of another reference, if the source is already a
    /// reference type.
    pub fn is_reference_from_owner(&self, reg: &MirTypeRegistry) -> bool {
        reg.is_ref(&self.src_ty) || reg.is_mut_ref(&self.src_ty)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum RefOffset {
    Const(MemberOffset),
    ArrayIndex {
        index: MirValue,
        array_size: usize,
    },
    SliceIndex {
        index: MirValue,
        slice_size: usize,
    },
    ArrayRange {
        start: MirValue,
        end: MirValue,
        array_bounds: Range<usize>,
    },
    SliceRange {
        start: MirValue,
        end: MirValue,
        slice_bounds: BoundsCheck,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoundsCheck {
    pub max: MirValue,
    pub idx: MirValue,
}
