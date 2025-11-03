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

use crate::core::edl_param_env::{EdlGenericParam, EdlGenericParamVariant, EdlParameterEnv};
use crate::prelude::edl_type::EdlTypeId;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::core::edl_error::EdlError;
use crate::core::edl_type::{EdlEnvId, EdlRepresentation, EdlStructVariant, EdlTypeRegistry, EdlTypeState};

#[derive(Debug, Clone, PartialEq)]
/// A tuple is essentially a base type with a variable number of generic parameters.
/// Thus, we have one tuple EDL id for each tuple type with a unique number of members.
struct EdlTuple {
    id: EdlTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EdlDictNameSet(Vec<String>);

impl Display for EdlDictNameSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for name in self.0.iter() {
            if !first {
                write!(f, ",")?;
            } else {
                first = false;
            }
            write!(f, " {}", name)?;
        }
        if !self.0.is_empty() {
            write!(f, " ")?;
        }
        write!(f, "}}")
    }
}

impl EdlDictNameSet {
    pub fn new<I>(content: I) -> Self
    where I: IntoIterator,
          I::Item: AsRef<str> {

        let mut content = content.into_iter()
            .map(|val| val.as_ref().to_string())
            .collect::<Vec<_>>();
        content.sort();
        EdlDictNameSet(content)
    }

    pub fn create_dict_state(
        &self,
        env_id: EdlEnvId,
        reg: &mut EdlTypeRegistry
    ) -> Result<EdlTypeState, EdlError> {
        let mut members = HashMap::new();
        for (i, name) in self.0.iter().enumerate() {
            let ty = reg.find_generic_type(env_id, i)?;
            members.insert(name.clone(), reg.new_type_instance(ty).unwrap());
        }
        Ok(EdlTypeState::Struct {
            can_init: true,
            repr: EdlRepresentation::Edl,
            members: EdlStructVariant::Named(members),
        })
    }
}

pub fn create_tuple_state(
    num_items: usize,
    env_id: EdlEnvId,
    reg: &mut EdlTypeRegistry
) -> Result<EdlTypeState, EdlError> {
    let mut members = Vec::new();
    for i in 0..num_items {
        let ty = reg.find_generic_type(env_id, i)?;
        members.push(reg.new_type_instance(ty).unwrap());
    }
    Ok(EdlTypeState::Struct {
        can_init: true,
        repr: EdlRepresentation::Edl,
        members: EdlStructVariant::List(members),
    })
}

#[derive(Debug, Clone, PartialEq)]
/// A dict is essentially a base type with a variable number of generic parameters, which also
/// have names.
/// Thus, we have one tuple EDL id for each tuple with a unique combination of member names.
struct EdlDict {
    id: EdlTypeId,
}

#[derive(Default)]
/// Type registry components for anonymous types.
/// Anonymous types in EDL can be represented as tuples and dics.
/// These types are equal as long as their components are exactly equal: this applies to tuples
/// which have the exact same types in the exact same order and to dicts which have the exact
/// same member names linked to the exact same types.
///
/// # Ordering in Dicts
///
/// Two dicts are equal if they contain the same member type names and these names have matching
/// types.
/// The order in which the members are presented in the dict is **irrelevant**.
///
/// # Memory Layout
///
/// EDL types, that is types that are fully defined in EDl and are not linked to external Rust
/// types, do not have a stable ABI.
/// This especially important for anonymous types: since there is no good way to specify the memory
/// layout for a type that does not have a name, these types will always have an arbitrary memory
/// layout.
/// When using tuples and dicts in EDL, assume that these types will never have a stable ABI or
/// be compatible with external type definitions!
pub struct AnonymousTypes {
    tuples: HashMap<usize, EdlTuple>,
    dicts: HashMap<EdlDictNameSet, EdlDict>,
}

impl AnonymousTypes {
    pub fn get_tuple_type(&self, num_members: &usize) -> Option<&EdlTypeId> {
        self.tuples.get(num_members).map(|tuple| &tuple.id)
    }

    pub fn get_dict_type(&self, members: &EdlDictNameSet) -> Option<&EdlTypeId> {
        self.dicts.get(members).map(|dict| &dict.id)
    }

    pub fn insert_tuple_type(&mut self, num_members: usize, id: EdlTypeId) {
        self.tuples.insert(num_members, EdlTuple { id });
    }

    pub fn insert_dict_type(&mut self, members: EdlDictNameSet, id: EdlTypeId) {
        self.dicts.insert(members, EdlDict { id });
    }

    pub fn create_tuple_env(&self, num_members: usize) -> EdlParameterEnv {
        let mut env = EdlParameterEnv::default();
        for i in 0..num_members {
            env.params.push(EdlGenericParam {
                name: i.to_string(),
                variant: EdlGenericParamVariant::Type
            });
        }
        env
    }

    pub fn create_dict_env(&self, members: EdlDictNameSet) -> EdlParameterEnv {
        let mut env = EdlParameterEnv::default();
        for i in members.0.iter() {
            env.params.push(EdlGenericParam {
                name: i.to_uppercase(),
                variant: EdlGenericParamVariant::Type
            });
        }
        env
    }
}
