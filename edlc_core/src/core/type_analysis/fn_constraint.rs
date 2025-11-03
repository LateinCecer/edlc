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

use crate::core::edl_error::EdlError;
use crate::core::edl_fn::EdlFnSignature;
use crate::core::edl_impl::FnConstraintError;
use crate::core::edl_param_env::{EdlGenericParamValue, EdlParamStack, EdlParameterDef};
use crate::core::edl_type;
use crate::core::edl_type::{EdlEnvId, EdlMaybeType};
use crate::core::type_analysis::constraints::{EnvConstraint, EnvConstraintStack};
use crate::core::type_analysis::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    name: String,
    pub ty: TypeUid,
    comptime: bool,
    mutable: bool,
}

#[derive(Debug, Clone, PartialEq)]
/// Creates a function signature constraint.
/// Please note that this constraint is **single use** and should only be used for one call
/// resolution.
pub struct SigConstraint {
    pub fn_id: EdlTypeId,
    name: String,
    pub env: EnvConstraintStack,
    own_env: EdlEnvId,
    pub comptime: bool,
    pub comptime_only: bool,
    pub ret: TypeUid,
    pub param_types: Vec<Param>,
}

impl SigConstraint {
    pub fn extract_stack(&self, infer: &Infer<'_, '_>) -> EdlParamStack {
        self.env.extract_type(infer)
    }

    pub fn from_signature(
        node: NodeId,
        id: EdlTypeId,
        sig: &EdlFnSignature,
        ctx: &mut Infer<'_, '_>,
        stack: &EnvConstraintStack
    ) -> Result<Self, InferError> {
        let env = EnvConstraint::new(sig.env, &mut ctx.at(node))?;
        let mut stack = stack.clone();
        stack.insert(env);
        // create return type
        let ret_ty = sig.return_type();
        let mut at = ctx.at_env(node, &stack);
        let ret = at.insert_type(&EdlMaybeType::Fixed(ret_ty))?;
        // factor parameters
        let mut param_types = Vec::new();
        for param in sig.params.iter() {
            let ty = at.insert_type(&EdlMaybeType::Fixed(param.ty.clone()))?;
            param_types.push(Param {
                name: param.name.clone(),
                ty,
                mutable: param.mutable,
                comptime: param.comptime,
            });
        }
        Ok(SigConstraint {
            fn_id: id,
            name: sig.name.clone(),
            env: stack,
            own_env: sig.env,
            comptime: sig.comptime,
            comptime_only: sig.comptime_only,
            param_types,
            ret,
        })
    }

    pub fn hint_generics(
        &self,
        hints: &EdlParameterDef,
        infer: &mut Infer<'_, '_>,
        node_id: NodeId
    ) -> Result<(), InferError> {
        if hints.env_id != self.own_env {
            panic!();
        }
        let env = self.env.get_env(self.own_env).unwrap();
        for (index, param) in hints.params.iter().enumerate() {
            match param {
                EdlGenericParamValue::Const(c_value) => {
                    infer.at_env(node_id, &self.env).eq(&env.get_const(index).unwrap(), c_value)?;
                }
                EdlGenericParamValue::Type(ty) => {
                    infer.at_env(node_id, &self.env).eq(&env.get_type(index).unwrap(), ty)?;
                }
                EdlGenericParamValue::ElicitConst(_) => (),
                EdlGenericParamValue::ElicitType => (),
            }
        }
        Ok(())
    }

    /// Adapting function signature constraint to a list of input parameters and return the
    /// return type of the function.
    pub fn adapt(
        &self,
        node: NodeId,
        args: &[TypeUid],
        ret: &TypeUid,
        infer: &mut Infer<'_, '_>
    ) -> Result<(), FnConstraintError> {
        if args.len() != self.param_types.len() {
            return Err(FnConstraintError::EdlError(self.fn_id, EdlError::E040 {
                got: args.len(),
                exp: self.param_types.len(),
            }));
        }
        // future me: check for auto-dereferencing
        infer
            .at_env(node, &self.env)
            .eq(ret, &self.ret)
            .map_err(|err| FnConstraintError::ConstraintMismatch(self.fn_id, err))?;

        // debug print info about return types
        // let ret_exp = infer.find_type(*ret);
        // let ret_got = infer.find_type(self.ret);
        // println!("[DEBUG]: return exp: {ret_exp:?}");
        // println!("[DEBUG]: return got: {ret_got:?}");

        for (param, param_value) in self.param_types.iter().zip(args.iter()) {
            // future me: check for auto-referencing
            // if !infer.at_env(node, &self.env).try_eq(&param.ty, param_value) {}

            let value_is_ref = matches!(infer.find_type(*param_value), EdlMaybeType::Fixed(ty) if ty.ty == edl_type::EDL_REF || ty.ty == edl_type::EDL_MUT_REF);
            let param_is_ref = matches!(infer.find_type(param.ty), EdlMaybeType::Fixed(ty) if ty.ty == edl_type::EDL_REF || ty.ty == edl_type::EDL_MUT_REF);

            if value_is_ref == param_is_ref {
                // dereference nothing
                infer
                    .at_env(node, &self.env)
                    .eq(&param.ty, param_value)
                    .map_err(|err| FnConstraintError::ConstraintMismatch(self.fn_id, err))?;
            } else if value_is_ref {
                // dereference value
                let el_ty = infer.get_generic_type(*param_value, 0).unwrap();
                infer
                    .at_env(node, &self.env)
                    .eq(&param.ty, &el_ty.uid)
                    .map_err(|err| FnConstraintError::ConstraintMismatch(self.fn_id, err))?;
            } else {
                // reference values
                let el_ty = infer.get_generic_type(param.ty, 0).unwrap();
                infer
                    .at_env(node, &self.env)
                    .eq(param_value, &el_ty.uid)
                    .map_err(|err| FnConstraintError::ConstraintMismatch(self.fn_id, err))?;
            }
        }
        Ok(())
    }
}
