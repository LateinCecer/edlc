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
use std::error::Error;
use std::mem;
use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_type::{EdlMaybeType};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_block::HirBlock;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker};
use crate::hir::translation::{HirTranslationError, IntoMir};
use crate::hir::{HirContext, HirError, HirPhase, ResolveFn, ResolveNames, ResolveTypes};
use crate::issue;
use crate::issue::SrcError;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::mir_condition::MirCondition;
use crate::mir::mir_expr::mir_if::MirIf;
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::{MirError, MirPhase};
use crate::prelude::{report_infer_error, HirErrorType};
use crate::resolver::ScopeId;


#[derive(Clone, Debug, PartialEq)]
pub enum HirCondition {
    Plane(Box<HirExpression>, Option<NodeId>),
    Match {
        // TODO
    },
}

#[derive(Clone, Debug, PartialEq)]
struct CompileInfo {
    node: NodeId,
    own_uid: TypeUid,
    finalized_type: EdlMaybeType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirIf {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub if_else_blocks: Vec<(HirCondition, HirBlock)>,
    /// this block is only populated when the if-else chain contains a single, last `else`
    /// expression at the end.
    pub else_block: Option<HirBlock>,

    info: Option<CompileInfo>,
}

impl HirIf {
    pub fn new(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        if_else_blocks: Vec<(HirCondition, HirBlock)>,
        else_block: Option<HirBlock>,
    ) -> Self {
        HirIf {
            pos,
            scope,
            src,
            if_else_blocks,
            else_block,
            info: None,
        }
    }
}

impl From<HirIf> for HirExpression {
    fn from(value: HirIf) -> Self {
        HirExpression::If(value)
    }
}

impl ResolveFn for HirCondition {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        match self {
            Self::Plane(val, _) => val.resolve_fn(phase),
            Self::Match {} => unimplemented!("match cases are currently unimplemented")
        }
    }
}

impl ResolveFn for HirIf {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        let mut res = Ok(());

        for (cond, block) in self.if_else_blocks.iter_mut() {
            if let Err(e) = cond.resolve_fn(phase) {
                res = Err(e);
            }
            if let Err(e) = block.resolve_fn(phase) {
                res = Err(e);
            }
        }
        if let Some(block) = self.else_block.as_mut() {
            if let Err(e) = block.resolve_fn(phase) {
                res = Err(e);
            }
        }
        res
    }
}

impl ResolveNames for HirCondition {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        match self {
            Self::Plane(val, _) => val.resolve_names(phase),
            Self::Match {} => unimplemented!("match cases are currently unimplemented")
        }
    }
}

impl ResolveNames for HirIf {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        for (cond, block) in self.if_else_blocks.iter_mut() {
            cond.resolve_names(phase)?;
            block.resolve_names(phase)?;
        }
        if let Some(block) = self.else_block.as_mut() {
            block.resolve_names(phase)?;
        }
        Ok(())
    }
}

impl ResolveTypes for HirCondition {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        match self {
            Self::Plane(val, node) => {
                let node = if let Some(node) = node {
                    *node
                } else {
                    let n = infer_state.node_gen.gen_info(&val.pos(), val.src());
                    *node = Some(n);
                    n
                };

                let val_ty = val.get_type_uid(&mut phase.infer_from(infer_state));
                let empty = phase.types.bool();
                phase.infer_from(infer_state)
                    .at(node)
                    .eq(&val_ty, &empty)
                    .map_err(|err| HirError::new_infer(val.pos(), err))?;
                val.resolve_types(phase, infer_state)
            },
            Self::Match {} => unimplemented!("match cases are currently unimplemented")
        }
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        match self {
            Self::Plane(val, _) => {
                val.get_type_uid(inferer)
            },
            Self::Match {} => unimplemented!("match cases are currently unimplemented"),
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        match self {
            HirCondition::Plane(val, _) => {
                val.finalize_types(inferer)
            }
            HirCondition::Match { .. } => unimplemented!("match cases are currently unimplemented"),
        }
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}

impl ResolveTypes for HirIf {
    fn resolve_types(
        &mut self,
        phase: &mut HirPhase,
        infer_state: &mut InferState
    ) -> Result<(), HirError> {
        let infer = &mut phase.infer_from(infer_state);
        let own_uid = self.get_type_uid(infer);
        let node = self.info.as_ref().unwrap().node;

        // place constraints
        if let Some(fin) = self.else_block.as_mut() {
            for (_, block) in self.if_else_blocks.iter_mut() {
                let block_ty = block.get_type_uid(infer);
                if let Err(err) = infer.at(node).eq(&own_uid, &block_ty) {
                    return Err(report_infer_error(err, infer_state, phase));
                }
            }

            let fin_ty = fin.get_type_uid(infer);
            if let Err(err) = infer.at(node).eq(&own_uid, &fin_ty) {
                return Err(report_infer_error(err, infer_state, phase));
            }
        } else {
            // if statement is not expressive
            let empty = infer.type_reg.empty();
            if let Err(err) = infer.at(node).eq(&own_uid, &empty) {
                return Err(report_infer_error(err, infer_state, phase));
            }
        }

        // resolve types for all conditions
        for (condition, block) in self.if_else_blocks.iter_mut() {
            condition.resolve_types(phase, infer_state)?;
            block.resolve_types(phase, infer_state)?;
        }
        if let Some(fin) = self.else_block.as_mut() {
            fin.resolve_types(phase, infer_state)?;
        }
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.own_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let type_uid = inferer.new_type(node);
            self.info = Some(CompileInfo {
                node,
                own_uid: type_uid,
                finalized_type: EdlMaybeType::Unknown,
            });
            type_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.info.as_mut().unwrap();
        let ty = inferer.find_type(info.own_uid);
        info.finalized_type = ty;

        for (cond, block) in self.if_else_blocks.iter_mut() {
            cond.finalize_types(inferer);
            block.finalize_types(inferer);
        }
        if let Some(fin) = self.else_block.as_mut() {
            fin.finalize_types(inferer);
        }
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}

struct BlockIter<'a> {
    src: &'a HirIf,
    index: usize,
}

struct BlockIterMut<'a> {
    src: &'a mut HirIf,
    index: usize,
}

impl<'a> Iterator for BlockIter<'a> {
    type Item = &'a HirBlock;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.src.if_else_blocks.len() {
            let old_idx = self.index;
            self.index += 1;
            Some(&self.src.if_else_blocks[old_idx].1)
        } else if self.index == self.src.if_else_blocks.len() {
            self.index += 1;
            self.src.else_block.as_ref()
        } else {
            None
        }
    }
}

impl<'a> Iterator for BlockIterMut<'a> {
    type Item = &'a mut HirBlock;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.src.if_else_blocks.len() {
            let old_idx = self.index;
            self.index += 1;
            unsafe {
                Some(mem::transmute::<_, &'a mut HirBlock>(&mut self.src.if_else_blocks[old_idx].1))
            }
        } else if self.index == self.src.if_else_blocks.len() {
            self.index += 1;
            let tmp: &'a mut Option<HirBlock> = unsafe {
                mem::transmute::<_, &'a mut Option<HirBlock>>(&mut self.src.else_block)
            };
            tmp.as_mut()
        } else {
            None
        }
    }
}

impl HirIf {
    pub fn verify(&mut self, phase: &mut HirPhase, context: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        for (cond, block) in self.if_else_blocks.iter_mut() {
            cond.verify(phase, context, infer_state)?;
            block.verify(phase, context, infer_state)?;
            // check condition for termination
            if cond.terminates(phase)? {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("dead code detected")
                    ),
                    &[
                        SrcError::Double {
                            src: self.src.clone(),
                            first: cond.pos().into(),
                            second: block.pos.into(),
                            error_first: issue::format_type_args!(
                                format_args!("early return detected in condition of if-block here")
                            ),
                            error_second: issue::format_type_args!(
                                format_args!("dead code starting here")
                            )
                        }
                    ],
                    None,
                );

                return Err(HirError {
                    ty: Box::new(HirErrorType::DeadCode),
                    pos: cond.pos(),
                });
            }
        }
        if let Some(else_block) = self.else_block.as_mut() {
            else_block.verify(phase, context, infer_state)?;
        }
        self.verify_return_type(phase)?;
        Ok(())
    }

    /// Verify the return types of the individual blocks the if-else chain is made up of.
    fn verify_return_type(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        if !self.is_expressive() {
            // if-else block is not expressive: no need to check for return types
            // print warnings if the type of any block is not `()`
            for (_, block) in self.if_else_blocks.iter() {
                let ty = block.get_type(phase)?;
                if block.terminates(phase)? {
                    continue;
                }

                if ty != EdlMaybeType::Fixed(phase.types.empty()) {
                    phase.report_warn(
                        issue::format_type_args!(
                            format_args!("return type of conditional block not used in \
                            inexpressive if-else chain")
                        ),
                        &[SrcError::Double {
                            src: self.src.clone(),
                            first: self.pos.into(),
                            second: block.find_last_item_position().into(),
                            error_first: issue::format_type_args!(
                                format_args!("if-else chain is not expressive as it is missing a \
                                final, unconditional `else` block")
                            ),
                            error_second: issue::format_type_args!(
                                format_args!("conditional block returns a non-empty value type")
                            )
                        }],
                        Some(issue::format_type_args!(
                            format_args!("The final expression in block can be terminated with a \
                            `;` which turns the overall return value into an empty type.")
                        )),
                    );
                }
            }
            // there is no final branch so there is no need to check it
            return Ok(());
        }
        // fetch total return type for reporting reasons
        let _total_return = self.get_type(phase)?;
        for (_cond, block) in self.if_else_blocks.iter_mut() {
            // check return type of conditional
            // if let Err(err) = cond.force_boolean(phase) {
            //     // report error in conditional type
            //     phase.report_error(
            //         issue::format_type_args!(
            //             format_args!("wrong type in conditional")
            //         ),
            //         &[SrcError::Single {
            //             pos: cond.pos().into(),
            //             src: self.src.clone(),
            //             error: issue::format_type_args!(
            //                 format_args!("conditional cannot be interpreted as a boolean")
            //             )
            //         }],
            //         Some(issue::format_type_args!(
            //             format_args!("Conditionals must have the type `bool`. Please consider \
            //             adapting the type of the conditional to a boolean.")
            //         ))
            //     );
            //     return Err(err);
            // }
            // only continue if the block does not terminate
            if block.terminates(phase)? {
                continue;
            }
            // check return type of block
            // if let Err(err) = block.adapt_type(&mut self.return_ty, phase) {
            //     // report if-chain then-block return type mismatch
            //     let block_return = block.get_type(phase)?;
            //     phase.report_error(
            //         issue::format_type_args!(
            //             format_args!("type mismatch in expressive if-else conditional chain")
            //         ),
            //         &[SrcError::Double {
            //             src: self.src.clone(),
            //             first: self.pos.into(),
            //             second: block.find_last_item_position().into(),
            //             error_first: issue::format_type_args!(
            //                 format_args!("if-else chain is expected to return type "),
            //                 &total_return as &dyn FmtType
            //             ),
            //             error_second: issue::format_type_args!(
            //                 format_args!("but this block returns type "),
            //                 &block_return as &dyn FmtType
            //             )
            //         }],
            //         Some(issue::format_type_args!(
            //             format_args!("In expressive if-else block chains (if-else chains that end \
            //             if a final, unconditional `else` block), **all** blocks must have the \
            //             same type.")
            //         ))
            //     );
            //     return Err(err);
            // }
        }
        let block = self.else_block.as_mut().unwrap();
        if !block.terminates(phase)? {
            // TODO: implement correct verification logic here
            // if let Err(err) = block.adapt_type(&mut self.return_ty, phase) {
            //     // check return type of final `else` block
            //     let block_return = block.get_type(phase)?;
            //     phase.report_error(
            //         issue::format_type_args!(
            //             format_args!("type mismatch in final conditional of expressive if-else chain")
            //         ),
            //         &[SrcError::Double {
            //             src: self.src.clone(),
            //             first: self.pos.into(),
            //             second: block.find_last_item_position().into(),
            //             error_first: issue::format_type_args!(
            //                 format_args!("if-else chain is expected to return type "),
            //                 &total_return as &dyn FmtType
            //             ),
            //             error_second: issue::format_type_args!(
            //                 format_args!("but final block returns type "),
            //                 &block_return as &dyn FmtType
            //             )
            //         }],
            //         Some(issue::format_type_args!(
            //             format_args!("In expressive if-else block chains (if-else chains that end \
            //         if a final, unconditional `else` block), **all** blocks must have the \
            //         same type.")
            //         ))
            //     );
            //     return Err(err);
            // }
        }
        Ok(())
    }

    /// If **all** blocks in the if-else chain return early **and** if the if-else chain is
    /// exhaustive, the entire if expression can be seen as terminating early.
    pub fn terminates(&self, phase: &mut HirPhase) -> Result<bool, HirError> {
        for (_cond, block) in self.if_else_blocks.iter() {
            if !block.terminates(phase)? {
                return Ok(false);
            }
        }
        if let Some(last) = self.else_block.as_ref() {
            if !last.terminates(phase)? {
                return Ok(false);
            }
            Ok(true)
        } else {
            // if-else expression must be exhaustive to terminate unconditionally
            Ok(false)
        }
    }

    fn iter(&self) -> BlockIter<'_> {
        BlockIter {
            src: self,
            index: 0,
        }
    }

    fn iter_mut(&mut self) -> BlockIterMut<'_> {
        BlockIterMut {
            src: self,
            index: 0,
        }
    }

    /// Returns `true`, only if the if-else chain is considered to be expressive.
    /// It is only in this case, that this expression can return a value type other than the
    /// _empty_ default type.
    fn is_expressive(&self) -> bool {
        self.else_block.is_some()
    }
}

impl HirCondition {
    pub fn const_expr(
        &self,
        state: &HirPhase
    ) -> Result<bool, <HirPhase as EdlCompilerState>::Error> {
        match self {
            Self::Plane(val, _) => val.const_expr(state),
            Self::Match {} => unimplemented!("match cases are currently unsupported"),
        }
    }

    pub fn is_comptime(
        &self,
    ) -> bool {
        match self {
            Self::Plane(val, _) => val.is_comptime(),
            Self::Match {} => unimplemented!("match cases are currently unsupported"),
        }
    }

    pub fn verify(&mut self, phase: &mut HirPhase, context: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        match self {
            HirCondition::Plane(value, _) => {
                value.verify(phase, context, infer_state)
            }
            HirCondition::Match { .. } => unimplemented!("match cases are currently unsupported"),
        }
    }

    pub fn terminates(&self, phase: &mut HirPhase) -> Result<bool, HirError> {
        match self {
            HirCondition::Plane(value, _) => value.terminates(phase),
            HirCondition::Match { .. } => unimplemented!("match cases are currently unsupported"),
        }
    }

    fn pos(&self) -> SrcPos {
        match self {
            HirCondition::Plane(val, _) => val.pos(),
            HirCondition::Match { .. } => unimplemented!("match cases are currently unsupported"),
        }
    }
}

impl EdlFnArgument for HirIf {
    type CompilerState = HirPhase;

    /// The return value of an if-expression is only mutable, when all branches are mutable.
    /// For now, this holds true regardless of if these branches can actually be reached.
    fn is_mutable(
        &self,
        state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        for block in self.iter() {
            if !block.is_mutable(state)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// An if expression is `const_expr` exactly when the condition, and **all** internal blocks
    /// are constant.
    fn const_expr(
        &self,
        state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        for block in self.iter() {
            if !block.const_expr(state)? {
                return Ok(false);
            }
        }
        for cond in self.iter() {
            if !cond.const_expr(state)? {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

impl HirTreeWalker for HirCondition {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        match self {
            HirCondition::Plane(val, _) => val.walk(filter, task),
            HirCondition::Match {} => unimplemented!(),
        }
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error,
    {
        match self {
            HirCondition::Plane(val, _) => val.walk_mut(filter, task),
            HirCondition::Match {} => unimplemented!(),
        }
    }
}

impl HirTreeWalker for HirIf {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        let mut out = Vec::new();
        for (cond, block) in self.if_else_blocks.iter() {
            out.append(&mut cond.walk(filter, task)?);
            out.append(&mut block.walk(filter, task)?);
        }
        if let Some(block) = self.else_block.as_ref() {
            out.append(&mut block.walk(filter, task)?);
        }
        Ok(out)
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error,
    {
        let mut out = Vec::new();
        for (cond, block) in self.if_else_blocks.iter_mut() {
            out.append(&mut cond.walk_mut(filter, task)?);
            out.append(&mut block.walk_mut(filter, task)?);
        }
        if let Some(block) = self.else_block.as_mut() {
            out.append(&mut block.walk_mut(filter, task)?);
        }
        Ok(out)
    }
}

impl HirExpr for HirIf {
    fn get_type(&self, _phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(self.info.as_ref().unwrap().finalized_type.clone())
    }

    fn is_comptime(&self) -> bool {
        let conditions = self.if_else_blocks.iter()
            .map(|(cond, _)| cond.is_comptime())
            .reduce(|lhs, rhs| lhs && rhs)
            .unwrap();
        let blocks = self.iter()
            .map(|block| block.is_comptime())
            .reduce(|lhs, rhs| lhs && rhs)
            .unwrap();

        conditions && blocks
    }

    /// For simplicity, constant values for conditional branching expressions are currently
    /// disabled!
    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantExpr),
        })
    }
}

impl IntoMir for HirCondition {
    type MirRepr = MirCondition;

    fn mir_repr<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        mir_funcs: &mut MirFuncRegistry<B>
    ) -> Result<Self::MirRepr, HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        match self {
            Self::Plane(val, _) => {
                let val = val.mir_repr(phase, mir_phase, mir_funcs)?;
                if val.get_type(mir_funcs, mir_phase) != mir_phase.types.bool() {
                    return Err(HirTranslationError::MirError(
                        *val.get_pos(),
                        format!("conditions must be a bool; {:?} != bool",
                                val.get_type(mir_funcs, mir_phase)),
                    ));
                }
                Ok(MirCondition::Plane(Box::new(val)))
            }
            Self::Match {} => Ok(MirCondition::Match {}),
        }
    }
}

impl IntoMir for HirIf {
    type MirRepr = MirIf;

    fn mir_repr<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        mir_funcs: &mut MirFuncRegistry<B>
    ) -> Result<Self::MirRepr, HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        let mut if_else_blocks = vec![];

        let return_ty = &self.info.as_ref().unwrap().finalized_type;
        let EdlMaybeType::Fixed(return_ty) = return_ty else {
            return Err(HirTranslationError::TypeNotFullyResolved {
                ty: return_ty.clone(),
                pos: self.pos,
            });
        };
        if !return_ty.is_fully_resolved() {
            return Err(HirTranslationError::TypeNotFullyResolved {
                ty: EdlMaybeType::Fixed(return_ty.clone()),
                pos: self.pos,
            });
        }

        let ty = mir_phase.types.mir_id(&return_ty, &phase.types)?;

        // fill blocks
        for (cond, block) in self.if_else_blocks.iter() {
            let cond = cond.mir_repr(phase, mir_phase, mir_funcs)?;
            let block = block.mir_repr(phase, mir_phase, mir_funcs)?;

            if cond.mir_type_id(mir_funcs, mir_phase) != mir_phase.types.bool() {
                return Err(HirTranslationError::MirError(
                    *cond.pos(),
                    format!("conditions must be a bool; {:?} != bool",
                            cond.mir_type_id(mir_funcs, mir_phase)),
                ));
            }
            if !block.terminates(&mir_phase.types).map_err(|err: MirError<B>| HirTranslationError::MirError(
                block.pos,
                format!("{err}"),
            ))? && block.ty != ty {
                return Err(HirTranslationError::MirError(
                    block.pos,
                    format!("else-if chain type mismatch: block has type {:?} but the type of the \
                    entire structure is suppose to be {:?}", block.ty, ty),
                ));
            }

            if_else_blocks.push((cond, block));
        }
        // type check and return final else block
        let else_block = if let Some(block) = self.else_block.as_ref() {
            let block = block.mir_repr(phase, mir_phase, mir_funcs)?;
            if !block.terminates(&mir_phase.types).map_err(|err: MirError<B>| HirTranslationError::MirError(
                block.pos,
                format!("{err}"),
            ))? && block.ty != ty {
                return Err(HirTranslationError::MirError(
                    block.pos,
                    format!("else-if chain type mismatch: block has type {:?} but the type of the \
                    entire structure is suppose to be {:?}", block.ty, ty),
                ));
            }
            Some(block)
        } else {
            None
        };

        Ok(MirIf {
            pos: self.pos,
            scope: self.scope,
            src: self.src.clone(),
            id: mir_phase.new_id(),
            ty,
            if_else_blocks,
            else_block,
        })
    }
}




#[cfg(test)]
mod test {
    use crate::inline_code;
    use crate::prelude::{CompilerError, EdlCompiler};

    #[test]
    fn test_simple() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        compiler.push_module("std".to_string())?;

        // push some test module to the compiler
        compiler.prepare_module(&vec!["test"].into())?;
        let module = compiler.parse_module(inline_code!(r#"
fn main() {
    let mut a: usize = 1;
    let b: usize = 32;

    let c = if true {
        a = b;
        a
    } else {
        a
    };
}
        "#), vec!["test".to_string()].into());

        match module {
            Ok(module) => println!("{:?}", module),
            Err(err) => {
                eprintln!("{:#}", err);
                return Err(err);
            }
        }
        Ok(())
    }
}
