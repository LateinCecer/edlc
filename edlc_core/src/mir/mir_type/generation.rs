/*
 *     EDLc, a compiler for the EDL programming language.
 *     Copyright (C) 2026  Adrian Paskert
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

use crate::core::edl_error::EdlError;
use crate::core::edl_param_env::EdlParamStack;
use crate::core::edl_type::{EdlRepresentation, EdlStructVariant, EdlType, EdlTypeInstance, EdlTypeRegistry, EdlTypeState};
use crate::mir::mir_type::layout::{EnumLayoutBuilder, Layout, StructLayoutBuilder, UnionLayoutBuilder};
use crate::mir::mir_type::MirTypeRegistry;

pub fn generate_type(
    ty: EdlTypeInstance,
    edl_reg: &EdlTypeRegistry,
    mir_reg: &mut MirTypeRegistry
) -> Result<Layout, EdlError> {
    match edl_reg.get_type(ty.ty) {
        Some(EdlType::Type { state, name, param }) => {
            match state {
                EdlTypeState::Uninitialized => {
                    eprintln!("  ---  invalid type init  ---  ");
                    eprintln!("  * invalid type name: `{name}`");
                    eprintln!("  * parameter environment: `{param:?}`");
                    eprintln!("  ---                     ---  ");
                    panic!("cannot generate type instance from uninitialized type")
                }
                EdlTypeState::Struct { repr, .. } if *repr == EdlRepresentation::C || *repr == EdlRepresentation::Edl => {
                    CTypeGenerator.generate(&ty, state, edl_reg, mir_reg)
                }
                EdlTypeState::Struct { .. } => {
                    unimplemented!("cannot generate type instances for `Rust` repr types")
                }
                EdlTypeState::Enum { repr, .. } if *repr == EdlRepresentation::C || *repr == EdlRepresentation::Edl => {
                    CTypeGenerator.generate(&ty, state, edl_reg, mir_reg)
                }
                EdlTypeState::Enum { .. } => {
                    unimplemented!("cannot generate type instances for `Rust` repr types")
                }
                EdlTypeState::Union { repr, .. } if *repr == EdlRepresentation::C || *repr == EdlRepresentation::Edl => {
                    CTypeGenerator.generate(&ty, state, edl_reg, mir_reg)
                }
                EdlTypeState::Union { .. } => {
                    unimplemented!("cannot generate type instances for `Rust` repr types")
                }
                EdlTypeState::Opaque => {
                    panic!("cannot generate type instances for opaque types")
                }
            }
        }
        Some(EdlType::Generic { .. }) => {
            panic!("cannot generate layout for generic type")
        }
        Some(EdlType::Function { .. }) => {
            panic!("cannot generate layout for function type")
        }
        None => panic!()
    }
}


trait TypeGenerator {
    fn generate(
        &self,
        base: &EdlTypeInstance,
        state: &EdlTypeState,
        edl_reg: &EdlTypeRegistry,
        mir_reg: &mut MirTypeRegistry
    ) -> Result<Layout, EdlError>;
}

struct CTypeGenerator;
struct EdlTypeGenerator;

impl CTypeGenerator {
    fn struct_variant_layout(
        params: &EdlParamStack,
        members: &EdlStructVariant,
        edl_reg: &EdlTypeRegistry,
        mir_reg: &mut MirTypeRegistry
    ) -> Result<Layout, EdlError> {
        let mut builder = StructLayoutBuilder::default();
        match members {
            EdlStructVariant::List(list) => {
                for (i, m) in list.iter().enumerate() {
                    let ty = m.resolve_generics(params, edl_reg);
                    let ty = mir_reg.mir_id(&ty, edl_reg)?;
                    builder.add(i.to_string(), ty, mir_reg);
                }
            }
            EdlStructVariant::Named(map) => {
                for (name, m) in map.iter() {
                    let ty = m.resolve_generics(params, edl_reg);
                    let ty = mir_reg.mir_id(&ty, edl_reg)?;
                    builder.add(name.clone(), ty, mir_reg);
                }
            }
            EdlStructVariant::ZeroSized => (),
        }
        Ok(builder.make_unchecked())
    }
}

impl TypeGenerator for CTypeGenerator {
    fn generate(
        &self,
        base: &EdlTypeInstance,
        state: &EdlTypeState,
        edl_reg: &EdlTypeRegistry,
        mir_reg: &mut MirTypeRegistry
    ) -> Result<Layout, EdlError> {
        match state {
            EdlTypeState::Uninitialized => {
                todo!()
            }
            EdlTypeState::Struct {
                members,
                repr,
                ..
            } => {
                assert!(*repr == EdlRepresentation::Edl || *repr == EdlRepresentation::C);
                let mut stack = EdlParamStack::default();
                stack.insert_def(base.param.clone());
                Self::struct_variant_layout(&stack, members, edl_reg, mir_reg)
            }
            EdlTypeState::Enum {
                variants,
                repr,
                ..
            } => {
                assert!(*repr == EdlRepresentation::Edl || *repr == EdlRepresentation::C);
                let discriminator_ty = if variants.len() <= u8::MAX as usize {
                    mir_reg.u8()
                } else if variants.len() <= u16::MAX as usize {
                    mir_reg.u16()
                } else if variants.len() <= u32::MAX as usize {
                    mir_reg.u32()
                } else {
                    mir_reg.u64()
                };
                let mut builder = EnumLayoutBuilder::new(discriminator_ty);


                let mut stack = EdlParamStack::default();
                stack.insert_def(base.param.clone());

                for (variant_name, variant) in variants.iter() {
                    let variant_layout = Self::struct_variant_layout(&stack, variant, edl_reg, mir_reg)?;
                    builder.add_variant(variant_name.clone(), variant_layout);
                }
                Ok(builder.make_unchecked(mir_reg))
            }
            EdlTypeState::Union {
                members,
                repr,
                ..
            } => {
                assert!(*repr == EdlRepresentation::Edl || *repr == EdlRepresentation::C);
                let mut builder = UnionLayoutBuilder::default();

                let mut stack = EdlParamStack::default();
                stack.insert_def(base.param.clone());

                for m in members.iter() {
                    let ty = m.resolve_generics(&stack, edl_reg);
                    let ty = mir_reg.mir_id(&ty, edl_reg)?;
                    builder.add_type(ty);
                }
                Ok(builder.make_unchecked(mir_reg))
            }
            EdlTypeState::Opaque => {
                todo!()
            }
        }
    }
}
