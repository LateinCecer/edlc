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
use crate::core::edl_fn::{EdlFnSignature, EdlPreSignature};
use crate::core::edl_param_env::{Adaptable, EdlGenericParam, EdlGenericParamValue, EdlGenericParamVariant};
use crate::core::edl_type::{EdlType, EdlTypeId, EdlTypeInstance, EdlTypeRegistry, FunctionState};
use crate::resolver::QualifierName;

struct AliasParameter {
    generic: EdlGenericParam,
    index: usize,
}

pub struct FixedParameter {
    pub generic: EdlGenericParamValue,
    pub index: usize,
}

pub struct EdlTypeAlias {
    name: QualifierName,
    pub ty: EdlTypeId,
    /// This vector contains the generic parameters of the alias, in the order in which they are
    /// specified on the type itself.
    params: Vec<AliasParameter>,
    fixed: Vec<FixedParameter>,
}

impl EdlTypeAlias {
    /// Creates a new type alias.
    ///
    /// The [alias_env] iterator should contain the indices of all generic environment parameters
    /// of [base] that are passed through to the parameter environment of the alias.
    /// The order in which the iterator provides these parameters must match the order in which
    /// they appear in the aliases' parameter env.
    ///
    /// The [fixed] iterator should provide the alias with all generic parameter values for [base]
    /// that are fixed through the definition of the alias.
    /// Unlike [alias_env] the order in which these values are provided is irrelevant.
    ///
    /// In the end, the total number of provided generic parameters
    /// (`alias_env.into_iter().count() + fixed.into_iter().count()`) must match the number of
    /// generic environment parameters defined by the [base] type **exactly**.
    pub fn new(
        name: QualifierName,
        base: EdlTypeId,
        alias_env: impl IntoIterator<Item=usize>,
        fixed: impl IntoIterator<Item=FixedParameter>,
        regs: &EdlTypeRegistry,
    ) -> Result<Self, EdlError> {
        match regs.get_type(base).ok_or(EdlError::E011(base))? {
            EdlType::Type { param: env_id, .. }
            | EdlType::Function { state: FunctionState::Init { sig: EdlFnSignature { env: env_id, .. } , .. }, .. }
            | EdlType::Function { state: FunctionState::Pre { sig: EdlPreSignature { env: env_id, .. } , .. }, .. } => {
                let env = regs.get_env(*env_id).ok_or(EdlError::E009(*env_id))?;
                let mut total_parameters = 0usize;
                // format parameter environment
                let mut params = vec![];
                for index in alias_env {
                    total_parameters += 1;
                    params.push(AliasParameter {
                        index,
                        generic: env.params.get(index)
                            .ok_or(EdlError::E013(*env_id, index))?
                            .clone(),
                    });
                }
                // format fixed parameters
                let fixed = fixed.into_iter().collect::<Vec<_>>();
                total_parameters += fixed.len();
                assert_eq!(total_parameters, env.params.len());

                Ok(EdlTypeAlias {
                    ty: base,
                    params,
                    fixed,
                    name,
                })
            }
            EdlType::Generic { .. } => {
                assert_eq!(alias_env.into_iter().count(), 0);
                assert_eq!(fixed.into_iter().count(), 0);
                Ok(EdlTypeAlias {
                    ty: base,
                    params: vec![],
                    fixed: vec![],
                    name,
                })
            }
        }
    }

    /// Creates a new type instance of the type that the alias refers to.
    /// Since a type alias may have generic parameters of its own, generic type parameter values
    /// may have to be passed to this function using the [params] iterator.
    /// The number and variant of the generic parameter must then match the expected format
    /// proscribed by the generic parameter environment of the base type.
    /// Additionally, the order in which [params] lists the parameters must match the order in which
    /// the generic parameters are defined in the parameter environment **of the alias**.
    pub fn new_instance<I: IntoIterator<Item = EdlGenericParamValue>>(
        &self,
        params: I,
        types: &EdlTypeRegistry,
    ) -> Result<EdlTypeInstance, EdlError> {
        // create new type instance
        let mut instance = types.new_type_instance(self.ty)
            .ok_or(EdlError::E011(self.ty))?;
        // populate parameter env with parameters provided to the env of the alias
        let mut params_exp = self.params.iter();
        let mut params_got = params.into_iter();
        loop {
            let exp_next = params_exp.next();
            let got_next = params_got.next();
            if exp_next.is_none() && got_next.is_none() {
                break;
            }
            // check if either one is none
            if exp_next.is_none() {
                // more parameters were specified then expected
                panic!();
            } else if got_next.is_none() {
                // less parameters were specified then expected
                panic!();
            }
            let exp_next = exp_next.unwrap();
            let mut got_next = got_next.unwrap();
            exp_next.check_value(&got_next, types)?;
            // insert into parameter env of the instance
            instance.param.params[exp_next.index].adapt(&mut got_next, types)?;
        }
        // populate parameters fixed by the alias definition
        for param in self.fixed.iter() {
            instance.param.params[param.index].adapt(&mut param.generic.clone(), types)?;
        }
        Ok(instance)
    }
}

impl AliasParameter {
    /// Checks if the generic variant of [value] matches the generic variant defined by this
    /// parameter definition.
    fn check_value(&self, value: &EdlGenericParamValue, reg: &EdlTypeRegistry) -> Result<(), EdlError> {
        match self.generic.variant {
            EdlGenericParamVariant::Const(exp) => match value {
                EdlGenericParamValue::Const(got) => {
                    let got = got.get_type(reg)?;
                    if exp == got {
                        Ok(())
                    } else {
                        Err(EdlError::E005 { exp, got })
                    }
                }
                EdlGenericParamValue::ElicitConst(got) => {
                    if exp == *got {
                        Ok(())
                    } else {
                        Err(EdlError::E005 { exp, got: *got })
                    }
                }
                _ => Err(EdlError::E002),
            }
            EdlGenericParamVariant::Type => match value {
                EdlGenericParamValue::Type(_) | EdlGenericParamValue::ElicitType => Ok(()),
                _ => Err(EdlError::E001),
            }
        }
    }
}
