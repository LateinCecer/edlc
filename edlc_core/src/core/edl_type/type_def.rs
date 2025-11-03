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
use crate::core::edl_type::EdlTypeInstance;
use crate::core::type_analysis::*;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
/// The inner memory representation of types in EDL.
pub enum EdlRepresentation {
    /// This memory representation is used to imitate types directly from the Rust compiler.
    /// Since the Rust ABI is not stable at the time of writing this, the `MirLayout` trait is
    /// used to derive the layout directly from the generated Rust code.
    /// The natural limitation of this work-around is that it is only safe if the `MirLayout`
    /// implementation is correct **and** if the version of the Rust compiler used to compiler the
    /// type in question matches the compiler version used to compile the EDL compiler **exactly**.
    ///
    /// # Layout Generation
    ///
    /// During layout generation in the MIR compiler phase, native Rust type layouts cannot be
    /// generated at all by the compiler.
    /// Instead, each type _instance_ for types that have this representation must be added to the
    /// MIR type registry manually if it is to be used in the code.
    Rust,
    /// All roads lead back to Rome.
    /// Or in the case of computer science, Clang.
    /// Good old plane C type layouts.
    ///
    /// # Layout Generation
    ///
    /// Generating these layouts is trivial, once all members are known.
    C,
    /// Internal EDL type representation.
    /// Like Rust, EDL does not have a stable ABI.
    /// Thus, do **not** use this type layout over FFI boundaries to any other language!
    ///
    /// # Layout Generation
    ///
    /// These types can be generated automatically be the compiler, but since the ABI is unstable,
    /// do not make any assumptions towards the generated layout.
    Edl,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EdlEnumVariant {
    pub base: EdlTypeInstance,
    pub variant: String,
}

#[derive(Clone, Debug, Default)]
pub enum EdlTypeState {
    #[default]
    Uninitialized,
    Struct {
        members: EdlStructVariant,
        /// Indicates if this struct can be created in EDL.
        /// This may be false for some compiler intrinsic types.
        can_init: bool,
        repr: EdlRepresentation,
    },
    Enum {
        variants: HashMap<String, EdlStructVariant>,
        /// Indicates if this enum can be created in EDL.
        /// This may be false for some compiler intrinsic types.
        can_init: bool,
        repr: EdlRepresentation,
    },
    Union {
        members: Vec<EdlTypeInstance>,
        /// Indicates if this union can be created in EDL.
        /// This may be false for some compiler intrinsic types.
        can_init: bool,
        repr: EdlRepresentation,
    },
    /// Opaque types are types, which have some internal structure that is not accessible to the
    /// compiler.
    /// This is usually reserved for types which are inherited from the Rust host with no
    /// additional data being passed to the compiler.
    Opaque,
}

#[derive(Clone, Debug)]
pub enum EdlStructVariant {
    /// A struct or enum variant with unnamed, positional members.
    ///
    /// ```
    /// struct List(i32, usize, f32);
    /// ```
    List(Vec<EdlTypeInstance>),
    /// A struct or enum variant with named members
    ///
    /// ```
    /// struct Named {
    ///     id: u32,
    ///     index: usize,
    ///     val: f32,
    /// }
    /// ```
    Named(HashMap<String, EdlTypeInstance>),
    /// A zero-sized struct or enum variant.
    ///
    /// ```
    /// struct Zero;
    /// ```
    ZeroSized,
}

#[derive(Debug, Clone)]
pub enum EdlTypeInitError {
    ListInfer(usize, InferError),
    NamedInfer(String, InferError),
    WrongNumber {
        exp: usize,
        got: usize,
    },
    NoSuchParameter(String),
    ExpectedList,
    ExpectedNamed,
}

impl Display for EdlTypeInitError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EdlTypeInitError::ListInfer(index, err) => {
                write!(f, "error at parameter with index `{index}`: {err}")
            }
            EdlTypeInitError::NamedInfer(name, err) => {
                write!(f, "error at parameter with name `{name}`: {err}")
            }
            EdlTypeInitError::WrongNumber { exp, got } => {
                write!(f, "wrong number of arguments: expected {exp} but got {got}")
            }
            EdlTypeInitError::NoSuchParameter(name) => {
                write!(f, "no such parameter: `{name}`")
            }
            EdlTypeInitError::ExpectedList => {
                write!(f, "expected parameter list, but got named parameter dict")
            }
            EdlTypeInitError::ExpectedNamed => {
                write!(f, "expected named parameter dict, but got parameter list")
            }
        }
    }
}

impl EdlStructVariant {
    pub fn constraint_list<I: Iterator<Item=TypeUid>>(
        &self,
        values: I,
        infer_at: &mut InferAt<'_, '_, '_>,
    ) -> Result<Vec<bool>, EdlTypeInitError> {
        match self {
            EdlStructVariant::List(list) => {
                let mut deref = Vec::new();
                for (idx, (template, value)) in list.iter()
                    .zip(values)
                    .enumerate() {
                    // check for dereferencing
                    let (base, auto_dereference) = infer_at.auto_deference(value);
                    deref.push(auto_dereference);
                    if let Err(err) = infer_at.eq(&base, template) {
                        return Err(EdlTypeInitError::ListInfer(idx, err));
                    }
                }
                Ok(deref)
            }
            EdlStructVariant::Named(_) => Err(EdlTypeInitError::ExpectedList),
            EdlStructVariant::ZeroSized => {
                let got = values.count();
                if got != 0 {
                    Err(EdlTypeInitError::WrongNumber { exp: 0, got })
                } else {
                    Ok(vec![])
                }
            }
        }
    }

    pub fn constraint_named<I: Iterator<Item=(String, TypeUid)>>(
        &self,
        values: I,
        infer_at: &mut InferAt<'_, '_, '_>,
    ) -> Result<Vec<bool>, EdlTypeInitError> {
        match self {
            EdlStructVariant::List(_) => Err(EdlTypeInitError::ExpectedNamed),
            EdlStructVariant::Named(map) => {
                let mut deref = Vec::new();
                for (name, value) in values {
                    let template = map.get(&name)
                        .ok_or(EdlTypeInitError::NoSuchParameter(name.clone()))?;
                    let (base, auto_dereference) = infer_at.auto_deference(value);
                    deref.push(auto_dereference);
                    infer_at.eq(&base, template)
                        .map_err(|err| EdlTypeInitError::NamedInfer(name, err))?;
                }
                Ok(deref)
            }
            EdlStructVariant::ZeroSized => {
                let got = values.count();
                if got != 0 {
                    Err(EdlTypeInitError::WrongNumber { exp: 0, got })
                } else {
                    Ok(vec![])
                }
            }
        }
    }

    pub fn iter(&self) -> FieldIter {
        match self {
            EdlStructVariant::List(list) => {
                FieldIter::List {
                    range: 0..list.len(),
                    index: 0,
                }
            }
            EdlStructVariant::Named(names) => {
                FieldIter::Named {
                    keys: names.keys().cloned().collect(),
                    index: 0,
                }
            }
            EdlStructVariant::ZeroSized => {
                FieldIter::List {
                    range: 0..0,
                    index: 0,
                }
            }
        }
    }
}

pub enum FieldIter {
    List { range: Range<usize>, index: usize },
    Named { keys: Vec<String>, index: usize },
}

impl Iterator for FieldIter {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::List { range, index } if range.contains(index) => {
                let out = Some(index.to_string());
                *index += 1;
                out
            }
            Self::Named { keys, index } => {
                let out = keys.get(*index).cloned();
                *index += 1;
                out
            }
            _ => None,
        }
    }
}
