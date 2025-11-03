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
use std::fmt::{Display, Formatter};
use edlc_analysis::graph::LatticeElement;
use crate::core::edl_error::EdlError;
use crate::core::edl_param_env::{EdlGenericParamValue, EdlParameterDef};
use crate::core::edl_type::EdlTypeInstance;
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::NodeId;

#[derive(Debug, Clone)]
pub struct TypeAnalysisError {
    err: EdlError,
    id: NodeId,
}

impl Display for TypeAnalysisError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @{}", self.err, self.id)
    }
}

impl Error for TypeAnalysisError {}


impl LatticeElement for EdlTypeInstance {
    type Conflict = TypeAnalysisError;

    fn lower(self, _other: Self) -> Result<Self, Self::Conflict> {
        todo!()
    }

    fn upper(self, _other: Self) -> Result<Self, Self::Conflict> {
        todo!()
    }

    fn is_lower_bound(&self, _other: &Self) -> bool {
        todo!()
    }

    fn is_upper_bound(&self, _other: &Self) -> bool {
        todo!()
    }
}

impl LatticeElement for EdlParameterDef {
    type Conflict = TypeAnalysisError;

    fn lower(self, _other: Self) -> Result<Self, Self::Conflict> {
        todo!()
    }

    fn upper(self, _other: Self) -> Result<Self, Self::Conflict> {
        todo!()
    }

    fn is_lower_bound(&self, _other: &Self) -> bool {
        todo!()
    }

    fn is_upper_bound(&self, _other: &Self) -> bool {
        todo!()
    }
}

impl LatticeElement for EdlGenericParamValue {
    type Conflict = TypeAnalysisError;

    fn lower(self, _other: Self) -> Result<Self, Self::Conflict> {
        todo!()
    }

    fn upper(self, _other: Self) -> Result<Self, Self::Conflict> {
        todo!()
    }

    fn is_lower_bound(&self, _other: &Self) -> bool {
        todo!()
    }

    fn is_upper_bound(&self, _other: &Self) -> bool {
        todo!()
    }
}

impl LatticeElement for EdlConstValue {
    type Conflict = TypeAnalysisError;

    fn lower(self, _other: Self) -> Result<Self, Self::Conflict> {
        todo!()
    }

    fn upper(self, _other: Self) -> Result<Self, Self::Conflict> {
        todo!()
    }

    fn is_lower_bound(&self, _other: &Self) -> bool {
        todo!()
    }

    fn is_upper_bound(&self, _other: &Self) -> bool {
        todo!()
    }
}
