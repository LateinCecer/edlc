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

#![feature(offset_of_enum)]


extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Literal};
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput, Fields};


#[proc_macro_derive(MirLayout)]
pub fn derive_mir_layout(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    // let name_str = Literal::string(&name.to_string());

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let layout = layout_from_type(&name, &input.data);

    let expanded = quote! {
        impl #impl_generics ::edlc_core::prelude::mir_type::layout::MirLayout for #name #ty_generics #where_clause {
            fn layout(types: &::edlc_core::prelude::mir_type::MirTypeRegistry) -> ::edlc_core::prelude::mir_type::layout::Layout {
                #layout
            }
        }
    };

    TokenStream::from(expanded)
}

fn layout_from_type(struct_name: &Ident, data: &Data) -> proc_macro2::TokenStream {
    match data {
        Data::Struct(data) => {
            let layout = process_struct_fields(&data.fields, struct_name);
            quote! {
                #[allow(unused_mut)]
                let mut builder = ::edlc_core::prelude::mir_type::layout::OffsetStructLayoutBuilder::default();
                #layout
                ::edlc_core::prelude::mir_type::layout::OffsetStructLayoutBuilder::make::<Self>(builder, types)
            }
        },
        Data::Enum(data) => {
            let variants = data.variants.iter().map(|variant| {
                let variant_name = &variant.ident;
                let variant_name_str = Literal::string(&variant_name.to_string());

                let v = process_enum_fields(&variant.fields, struct_name, variant_name);
                quote_spanned! { variant.span() => ::edlc_core::prelude::mir_type
                    ::layout::EnumLayoutBuilder::add_variant(&mut enum_builder, #variant_name_str.to_string(), {
                        #[allow(unused_mut)]
                        let mut builder = ::edlc_core::prelude::mir_type::layout::OffsetStructLayoutBuilder::default();
                        #v
                        ::edlc_core::prelude::mir_type::layout::OffsetStructLayoutBuilder::make_unchecked(builder, types)
                    });
                }
            });

            quote! {
                #[allow(unused_mut)]
                let mut enum_builder = ::edlc_core::prelude::mir_type::layout::EnumLayoutBuilder::new(types.u8());
                #(#variants)*
                ::edlc_core::prelude::mir_type::layout::EnumLayoutBuilder::make::<Self>(enum_builder, types)
            }
        }
        Data::Union(_data) => {
            todo!()
        }
    }
}

fn process_struct_fields(fields: &Fields, data_ty: &Ident) -> proc_macro2::TokenStream {
    match fields {
        Fields::Named(fields) => {
            let values = fields.named
                .iter()
                .map(|field| {
                    let field_name = field.ident.as_ref().unwrap();
                    let field_name_str = Literal::string(&field_name.to_string());
                    let field_ty = &field.ty;

                    quote_spanned! { field.span() => {
                        let offset = std::mem::offset_of!(#data_ty, #field_name);

                        ::edlc_core::prelude::mir_type
                            ::layout::OffsetStructLayoutBuilder::add_type::<#field_ty>(
                            &mut builder, #field_name_str.to_string(), types, offset);
                    }}
            });

            quote! {
                #(#values)*
            }
        }
        Fields::Unit => quote! {},
        Fields::Unnamed(fields) => {
            let values = fields.unnamed
                .iter()
                .enumerate()
                .map(|(name, field)| {
                    let field_name = name.to_string();
                    let field_name_ident = Literal::usize_unsuffixed(name);
                    let field_name_str = Literal::string(&field_name);
                    let field_ty = &field.ty;

                    quote_spanned! { field.span() => {
                        let offset = std::mem::offset_of!(#data_ty, #field_name_ident);

                        ::edlc_core::prelude::mir_type
                            ::layout::OffsetStructLayoutBuilder::add_type::<#field_ty>(
                            &mut builder, #field_name_str.to_string(), types, offset);
                    }}
            });

            quote! {
                #(#values)*
            }
        }
    }
}

fn process_enum_fields(fields: &Fields, data_ty: &Ident, variant: &Ident) -> proc_macro2::TokenStream {
    match fields {
        Fields::Named(fields) => {
            let values = fields.named
                .iter()
                .map(|field| {
                    let field_name = field.ident.as_ref().unwrap();
                    let field_name_str = Literal::string(&field_name.to_string());
                    let field_ty = &field.ty;

                    quote_spanned! { field.span() => {
                        let offset = std::mem::offset_of!(#data_ty, #variant.#field_name);

                        ::edlc_core::prelude::mir_type
                            ::layout::OffsetStructLayoutBuilder::add_type::<#field_ty>(
                            &mut builder, #field_name_str.to_string(), types, offset);
                    }}
                });

            quote! {
                #(#values)*
            }
        }
        Fields::Unit => quote! {},
        Fields::Unnamed(fields) => {
            let values = fields.unnamed
                .iter()
                .enumerate()
                .map(|(name, field)| {
                    let field_name = name.to_string();
                    let field_name_ident = Literal::usize_unsuffixed(name);
                    let field_name_str = Literal::string(&field_name);
                    let field_ty = &field.ty;

                    quote_spanned! { field.span() => {
                        let offset = std::mem::offset_of!(#data_ty, #variant.#field_name_ident);

                        ::edlc_core::prelude::mir_type
                            ::layout::OffsetStructLayoutBuilder::add_type::<#field_ty>(
                            &mut builder, #field_name_str.to_string(), types, offset);
                    }}
                });

            quote! {
                #(#values)*
            }
        }
    }
}
