/*
 * EDLc, a compiler for the EDL programming language.
 * Copyright (C) 2026  Adrian Paskert
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
//! Mirror representations of `edlc_core`'s documentation items.
//!
//! The `blob` column of the documentation database holds the serde-JSON serialization of the
//! original `edlc_core::prelude::Item`. These types mimic that JSON shape so the blob can be
//! parsed back into a structured representation without depending on the compiler crate itself.
//! They are compiled into both the SSR and the (WASM) hydrate builds.
//!
//! When adding or changing fields in `edlc_core`'s `documentation` module, keep these mirrors in
//! sync with the serialized shape.

use serde::{Deserialize, Serialize};

/// A source position, mirroring `edlc_core::lexer::SrcPos`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SrcPos {
    pub line: usize,
    pub col: usize,
    pub size: usize,
}

/// A qualified name, mirroring `edlc_core::resolver::QualifierName`
/// (serialized as `{"path": [...]}`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct QualifierName {
    pub path: Vec<String>,
}

impl QualifierName {
    /// The full name path joined with `::`.
    pub fn display(&self) -> String {
        self.path.join("::")
    }

    /// The last path segment, if any.
    pub fn last(&self) -> Option<&str> {
        self.path.last().map(|s| s.as_str())
    }
}

/// Where the item's source lives, mirroring `edlc_core::documentation::PortableModuleSrc`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PortableModuleSrc {
    File(String),
    String {
        path: String,
        pos: SrcPos,
        // The full module source can be large and is never needed for rendering, so it is
        // parsed away (without allocating) instead of being kept in memory.
        #[serde(skip)]
        src: String,
    },
}

impl Default for PortableModuleSrc {
    fn default() -> Self {
        Self::File(String::new())
    }
}

/// Documentation of a global variable.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LetDoc {
    pub name: QualifierName,
    #[serde(skip)]
    pub src: PortableModuleSrc,
    pub pos: SrcPos,
    pub doc: String,
    pub ty: TypeDoc,
    pub ms: Modifiers,
}

/// Documentation of a global constant.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstDoc {
    pub name: QualifierName,
    #[serde(skip)]
    pub src: PortableModuleSrc,
    pub pos: SrcPos,
    pub doc: String,
    pub ty: TypeDoc,
    pub ms: Modifiers,
    pub associated_type: Option<TypeDoc>,
}

/// Documentation of a function signature.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FuncDoc {
    pub name: QualifierName,
    #[serde(skip)]
    pub src: PortableModuleSrc,
    pub pos: SrcPos,
    pub doc: String,
    pub env: EnvDoc,
    pub params: FuncParamsDoc,
    pub ret: TypeDoc,
    pub ms: Modifiers,
    pub async_return: bool,
    pub associated_type: Option<TypeDoc>,
}

/// A list of function parameters (a newtype over `Vec`, serialized as a bare array).
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct FuncParamsDoc(pub Vec<FuncParamDoc>);

/// Documentation of a single function parameter.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FuncParamDoc {
    pub name: String,
    pub pos: SrcPos,
    pub ty: TypeDoc,
    pub ms: Modifiers,
}

/// A list of modifiers (a newtype over `Vec`, serialized as a bare array of strings).
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Modifiers(pub Vec<Modifier>);

/// A modifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Modifier {
    Comptime,
    MaybeComptime,
    Mut,
    Async,
    Shared,
}

/// The values provided to instantiate a generic parameter environment.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct EnvInstDoc {
    pub params: Vec<EnvValueDoc>,
}

/// A value of a generic parameter instantiation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum EnvValueDoc {
    Type {
        ty: TypeDoc,
        pos: Option<SrcPos>,
    },
    Const {
        val: DocConstValue,
        pos: Option<SrcPos>,
    },
    ElicitType,
    ElicitConst,
}

/// A generic parameter environment.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct EnvDoc {
    pub params: Vec<EnvParamDoc>,
}

/// A generic parameter declaration.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum EnvParamDoc {
    Type {
        name: String,
        pos: Option<SrcPos>,
    },
    Const {
        name: String,
        pos: Option<SrcPos>,
        ty: TypeDoc,
    },
}

/// A member of a struct or union.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructMemberDoc {
    pub name: String,
    pub pos: SrcPos,
    pub doc: String,
    pub ty: TypeDoc,
    pub modifiers: Modifiers,
}

/// The payload of a struct definition.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StructTypeDoc {
    Named(Vec<StructMemberDoc>),
    Tuple(Vec<TypeDoc>),
    ZeroSized,
}

/// A variant of an enum definition.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumVariantDoc {
    pub name: String,
    pub members: StructTypeDoc,
}

/// The payload of a type definition.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeDefVariant {
    Struct(StructTypeDoc),
    Enum(Vec<EnumVariantDoc>),
    Union(Vec<StructMemberDoc>),
    Alias(TypeDoc),
}

/// Documentation of a type definition.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeDefDoc {
    pub name: QualifierName,
    #[serde(skip)]
    pub src: PortableModuleSrc,
    pub pos: SrcPos,
    pub doc: String,
    pub env: EnvDoc,
    pub params: FuncParamsDoc,
    pub variant: TypeDefVariant,
}

/// A type reference.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeDoc {
    Base(TypeNameDoc, Option<SrcPos>),
    Array(Box<TypeDoc>, DocConstValue, Option<SrcPos>),
    Slice(Box<TypeDoc>, Option<SrcPos>),
    Ref(Box<TypeDoc>, Option<SrcPos>),
    MutRef(Box<TypeDoc>, Option<SrcPos>),
    Empty,
    Tuple(Vec<TypeDoc>, Option<SrcPos>),
    Elicit,
}

/// A qualified type name (a newtype over `Vec`, serialized as a bare array of segments).
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct TypeNameDoc(pub Vec<TypeNameSegmentDoc>);

/// A single segment of a qualified type name.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeNameSegmentDoc {
    pub name: QualifierName,
    pub parameters: EnvInstDoc,
    pub pos: Option<SrcPos>,
}

/// A constant value in a type position.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DocConstValue {
    Const(TypeNameDoc),
    Literal(EdlLiteralValue),
    Elicit,
}

/// A literal value, mirroring `edlc_core::core::edl_value::EdlLiteralValue`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum EdlLiteralValue {
    Usize(usize),
    Isize(isize),
    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    Str(String),
    Char(char),
    Empty(),
}

/// Documentation of a module.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModuleDoc {
    pub name: QualifierName,
    pub doc: String,
}

/// A documented item, mirroring `edlc_core::prelude::Item`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Item {
    GlobalVar(LetDoc),
    GlobalConst(ConstDoc),
    Func(FuncDoc),
    TypeDef(TypeDefDoc),
    Module(ModuleDoc),
}

/// Parses a `blob` string (the serde-JSON serialization of a core `Item`) into the
/// structured mirror representation.
pub fn parse_item(blob: &str) -> Result<Item, serde_json::Error> {
    serde_json::from_str(blob)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_func_blob() {
        let blob = r#"{
            "Func": {
                "name": {"path": ["m", "add"]},
                "src": {"String": {"path": "inline.edl", "pos": {"line": 1, "col": 1, "size": 9}, "src": "fn add(a: u32, b: u32) -> u32"}},
                "pos": {"line": 1, "col": 1, "size": 10},
                "doc": "Adds two numbers.",
                "env": {"params": [{"Type": {"name": "T", "pos": null}}]},
                "params": [
                    {"name": "a", "pos": {"line": 1, "col": 8, "size": 1},
                     "ty": {"Base": [[{"name": {"path": ["T"]}, "parameters": {"params": []}, "pos": null}], null]},
                     "ms": []},
                    {"name": "b", "pos": {"line": 1, "col": 15, "size": 1},
                     "ty": {"Base": [[{"name": {"path": ["u32"]}, "parameters": {"params": []}, "pos": null}], null]},
                     "ms": ["Mut"]}
                ],
                "ret": {"Base": [[{"name": {"path": ["u32"]}, "parameters": {"params": []}, "pos": null}], null]},
                "ms": ["Async"],
                "async_return": true,
                "associated_type": null
            }
        }"#;
        let item = parse_item(blob).unwrap();
        match item {
            Item::Func(f) => {
                assert_eq!(f.name.path, vec!["m".to_string(), "add".to_string()]);
                assert_eq!(f.name.display(), "m::add");
                assert!(f.async_return);
                assert_eq!(f.ms.0, vec![Modifier::Async]);
                assert_eq!(f.params.0.len(), 2);
                assert_eq!(f.params.0[1].ms.0, vec![Modifier::Mut]);
                assert!(!f.doc.is_empty());
            }
            other => panic!("expected a function, got {other:?}"),
        }
    }

    #[test]
    fn parses_all_item_variants() {
        let let_blob = r#"{
            "GlobalVar": {
                "name": {"path": ["v"]},
                "src": {"File": "x.edl"},
                "pos": {"line": 1, "col": 1, "size": 1},
                "doc": "",
                "ty": {"Base": [[{"name": {"path": ["str"]}, "parameters": {"params": []}, "pos": null}], null]},
                "ms": ["Mut"]
            }
        }"#;
        assert!(matches!(parse_item(let_blob).unwrap(), Item::GlobalVar(_)));

        let const_blob = r#"{
            "GlobalConst": {
                "name": {"path": ["C"]},
                "src": {"File": "x.edl"},
                "pos": {"line": 2, "col": 1, "size": 1},
                "doc": "",
                "ty": {"Base": [[{"name": {"path": ["u32"]}, "parameters": {"params": []}, "pos": null}], null]},
                "ms": [],
                "associated_type": null
            }
        }"#;
        assert!(matches!(
            parse_item(const_blob).unwrap(),
            Item::GlobalConst(_)
        ));

        let type_blob = r#"{
            "TypeDef": {
                "name": {"path": ["m", "S"]},
                "src": {"File": "x.edl"},
                "pos": {"line": 3, "col": 1, "size": 1},
                "doc": "",
                "env": {"params": []},
                "params": [],
                "variant": {"Struct": {"Named": [
                    {"name": "x", "pos": {"line": 3, "col": 5, "size": 1}, "doc": "",
                     "ty": {"Base": [[{"name": {"path": ["u8"]}, "parameters": {"params": []}, "pos": null}], null]},
                     "modifiers": []}
                ]}}
            }
        }"#;
        assert!(matches!(parse_item(type_blob).unwrap(), Item::TypeDef(_)));

        let module_blob = r#"{
            "Module": {"name": {"path": ["a", "b"]}, "doc": "A module."}
        }"#;
        assert!(matches!(parse_item(module_blob).unwrap(), Item::Module(_)));
    }

    #[test]
    fn parses_all_type_variants() {
        let blob = r#"{
            "GlobalVar": {
                "name": {"path": ["t"]},
                "src": {"File": "x.edl"},
                "pos": {"line": 1, "col": 1, "size": 1},
                "doc": "",
                "ms": [],
                "ty": {"Tuple": [
                    [
                        {"Array": [{"Base": [[{"name": {"path": ["u32"]}, "parameters": {"params": []}, "pos": null}], null]},
                                    {"Literal": {"U8": 4}}, null]},
                        {"Slice": [{"Base": [[{"name": {"path": ["str"]}, "parameters": {"params": []}, "pos": null}], null]}, null]},
                        {"Ref": [{"Base": [[{"name": {"path": ["T"]}, "parameters": {"params": []}, "pos": null}], null]}, null]},
                        {"MutRef": [{"Base": [[{"name": {"path": ["T"]}, "parameters": {"params": []}, "pos": null}], null]}, null]},
                        {"Empty": null},
                        {"Elicit": null}
                    ],
                    null
                ]}
            }
        }"#;
        match parse_item(blob).unwrap() {
            Item::GlobalVar(d) => match &d.ty {
                TypeDoc::Tuple(ts, _) => {
                    assert_eq!(ts.len(), 6);
                    assert!(matches!(
                        &ts[0],
                        TypeDoc::Array(_, DocConstValue::Literal(EdlLiteralValue::U8(4)), _)
                    ));
                    assert!(matches!(&ts[1], TypeDoc::Slice(_, _)));
                    assert!(matches!(&ts[2], TypeDoc::Ref(_, _)));
                    assert!(matches!(&ts[3], TypeDoc::MutRef(_, _)));
                    assert!(matches!(&ts[4], TypeDoc::Empty));
                    assert!(matches!(&ts[5], TypeDoc::Elicit));
                }
                other => panic!("expected tuple, got {other:?}"),
            },
            other => panic!("expected global var, got {other:?}"),
        }
    }

    #[test]
    fn parses_literal_variants() {
        for (json, expected) in [
            (r#"{"Usize": 1}"#, EdlLiteralValue::Usize(1)),
            (r#"{"Isize": -2}"#, EdlLiteralValue::Isize(-2)),
            (r#"{"Bool": true}"#, EdlLiteralValue::Bool(true)),
            (r#"{"U8": 8}"#, EdlLiteralValue::U8(8)),
            (r#"{"U16": 16}"#, EdlLiteralValue::U16(16)),
            (r#"{"U32": 32}"#, EdlLiteralValue::U32(32)),
            (r#"{"U64": 64}"#, EdlLiteralValue::U64(64)),
            (r#"{"U128": 128}"#, EdlLiteralValue::U128(128)),
            (r#"{"I8": -8}"#, EdlLiteralValue::I8(-8)),
            (r#"{"I16": -16}"#, EdlLiteralValue::I16(-16)),
            (r#"{"I32": -32}"#, EdlLiteralValue::I32(-32)),
            (r#"{"I64": -64}"#, EdlLiteralValue::I64(-64)),
            (r#"{"I128": -128}"#, EdlLiteralValue::I128(-128)),
            (r#"{"Str": "hi"}"#, EdlLiteralValue::Str("hi".to_string())),
            (r#"{"Char": "c"}"#, EdlLiteralValue::Char('c')),
            (r#"{"Empty": []}"#, EdlLiteralValue::Empty()),
        ] {
            let lit: EdlLiteralValue = serde_json::from_str(json).unwrap();
            assert_eq!(lit, expected, "mismatch for {json}");
        }
    }

    #[test]
    fn rejects_malformed_blobs() {
        assert!(parse_item("not json").is_err());
        assert!(parse_item(r#"{"Bogus": {}}"#).is_err());
        assert!(parse_item(r#"{"Func": {}}"#).is_err());
    }
}
