use crate::core::edl_type::EdlTypeInstance;

#[derive(Clone, Debug, PartialEq)]
pub struct EdlConstraint {
    pub constrainee: EdlTypeInstance,
    pub can_be: Vec<EdlTypeInstance>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EdlTypeConstraintSet {
    constraints: Vec<EdlConstraint>,
}

impl EdlTypeConstraintSet {
    pub fn new(constraints: Vec<EdlConstraint>) -> Self {
        Self { constraints }
    }
}
