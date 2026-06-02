use crate::core::edl_constraint::{EdlConstraint, EdlTypeConstraintSet};
use crate::core::edl_type::EdlMaybeType;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_type::HirType;
use crate::hir::{HirError, HirErrorType, HirPhase, IntoEdl};
use crate::lexer::SrcPos;
use crate::resolver::ScopeId;

pub struct HirWhereConstraint {
    pub pos: SrcPos,
    pub constrainee: HirType,
    pub options: Vec<HirType>,
}

pub struct HirWhere {
    pub src: ModuleSrc,
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub constraints: Vec<HirWhereConstraint>,
}

impl IntoEdl for HirWhereConstraint {
    type EdlRepr = EdlConstraint;

    fn edl_repr(&mut self, phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError> {
        let map_err = |t: EdlMaybeType| match t {
            EdlMaybeType::Fixed(t) => Ok(t),
            EdlMaybeType::Unknown => Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::TypeNotResolvable),
            }),
        };

        let constrainee = self.constrainee
            .edl_repr(phase)
            .and_then(map_err)?;
        let options = self.options
            .iter_mut()
            .map(|opt| opt
                .edl_repr(phase)
                .and_then(map_err))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(EdlConstraint {
            constrainee,
            can_be: options,
        })
    }
}

impl IntoEdl for HirWhere {
    type EdlRepr = EdlTypeConstraintSet;

    fn edl_repr(&mut self, phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError> {
        self.constraints
            .iter_mut()
            .map(|constraint| constraint.edl_repr(phase))
            .collect::<Result<Vec<_>, _>>()
            .map(EdlTypeConstraintSet::new)
    }
}
