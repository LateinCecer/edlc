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

impl TypeNameDoc {
    /// Returns the segments that make up the type name.
    pub fn segments(&self) -> &[TypeNameSegmentDoc] {
        &self.0
    }
}

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

/// The owner of a documented item, decomposed for display.
///
/// Items implement two kinds of ownership: plain items (functions, variables, constants, and
/// types defined in a module) are owned by the module path that precedes their name, while
/// impl items (functions and constants with an `associated_type`, e.g. the std intrinsics)
/// are owned by their associated type — the type itself is in turn defined in a module.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ItemOwner {
    /// The module path in which the item (or its associated type) is defined.
    pub module: Vec<String>,
    /// The simple name of the associated type, when the item is an impl item (e.g. `SVector`
    /// for `example::types::SVector::norm` or `usize` for `usize::add`).
    pub type_name: Option<String>,
    /// The associated type's name with its generic parameters, e.g. `SVector<f32, N>` or `usize`.
    pub type_display: Option<String>,
    /// A link target for the associated type: its documentation page when the type is defined
    /// in the documented project, otherwise the module page listing the type's items.
    pub type_href: Option<String>,
}

impl ItemOwner {
    /// Whether the item is an impl item (associated with a type).
    pub fn has_type(&self) -> bool {
        self.type_name.is_some()
    }

    /// The plain decomposition of a qualified name: everything but the last segment is the
    /// module, and there is no associated type.
    pub fn from_qual(qual_name: &str) -> Self {
        let segments: Vec<&str> = qual_name.split("::").collect();
        let module = segments
            .iter()
            .take(segments.len().saturating_sub(1))
            .map(|s| s.to_string())
            .collect();
        ItemOwner {
            module,
            type_name: None,
            type_display: None,
            type_href: None,
        }
    }
}

/// Decomposes the owner of a documented item into its module path and associated type.
///
/// For impl items the associated type's full path is the item's owner: `usize::add` is owned
/// by the type `usize` (which defines no module of its own), and
/// `example::types::SVector::norm` is the method `norm` of the type `SVector` defined in the
/// module `example::types`. For all other items, the owner is the qualifier path minus the
/// item's own name.
///
/// `qual_name` is the item's qualified name as stored in the database. When the blob's
/// associated type is absent, not a plain base type reference, or disagrees with `qual_name`,
/// the plain decomposition ([`ItemOwner::from_qual`]) is used.
pub fn item_owner(item: &Item, qual_name: &str) -> ItemOwner {
    let assoc = match item {
        Item::Func(d) => d.associated_type.as_ref(),
        Item::GlobalConst(d) => d.associated_type.as_ref(),
        _ => None,
    };
    let ty = match assoc {
        Some(TypeDoc::Base(ty, _)) if !ty.segments().is_empty() => ty,
        _ => return ItemOwner::from_qual(qual_name),
    };

    // The associated type's full path, e.g. `["example", "types", "SVector"]` or `["usize"]`.
    let mut type_path = Vec::new();
    for segment in ty.segments() {
        type_path.extend(segment.name.path.iter().cloned());
    }
    if type_path.is_empty() {
        return ItemOwner::from_qual(qual_name);
    }

    // The qualified name must be the type path plus the item's own name; otherwise the blob
    // and the database disagree, and the plain decomposition is the safe fallback.
    let qual_segments: Vec<&str> = qual_name.split("::").collect();
    let consistent = qual_segments.len() == type_path.len() + 1
        && qual_segments[..type_path.len()]
            .iter()
            .zip(type_path.iter())
            .all(|(q, t)| q == t)
        && qual_segments.last().copied() == item_name(item).last().map(|s| s.as_str());
    if !consistent {
        return ItemOwner::from_qual(qual_name);
    }

    let type_name = type_path.last().cloned().unwrap();
    let module = type_path[..type_path.len() - 1].to_vec();
    let type_href = if type_path.len() > 1 {
        format!("/item/{}", type_path.join("::"))
    } else {
        format!("/module/{}", type_path.join("::"))
    };
    ItemOwner {
        module,
        type_name: Some(type_name),
        type_display: Some(type_display(ty)),
        type_href: Some(type_href),
    }
}

/// The simple name of the item (the last segment of its qualifier path).
fn item_name(item: &Item) -> &[String] {
    match item {
        Item::GlobalVar(d) => &d.name.path,
        Item::GlobalConst(d) => &d.name.path,
        Item::Func(d) => &d.name.path,
        Item::TypeDef(d) => &d.name.path,
        Item::Module(d) => &d.name.path,
    }
}

/// The associated type's own name with the generic parameters of its last segment,
/// e.g. `SVector<f32, N>` or `usize`. Only the type's own name is used — the module it is
/// defined in is reported separately via [`ItemOwner::module`].
fn type_display(ty: &TypeNameDoc) -> String {
    let mut out = String::new();
    if let Some(last) = ty.segments().last() {
        if let Some(own_name) = last.name.path.last() {
            out.push_str(own_name);
        }
        if !last.parameters.params.is_empty() {
            out.push('<');
            for (i, value) in last.parameters.params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                env_value_display(value, &mut out);
            }
            out.push('>');
        }
    }
    out
}

/// Formats a generic parameter value the same way the core `Display` impls do.
fn env_value_display(value: &EnvValueDoc, out: &mut String) {
    match value {
        EnvValueDoc::Type { ty, .. } => {
            if let TypeDoc::Base(base, _) = ty {
                out.push_str(&base_name_display(base));
            }
        }
        EnvValueDoc::Const { val, .. } => match val {
            DocConstValue::Const(name) => out.push_str(&base_name_display(name)),
            DocConstValue::Literal(lit) => out.push_str(&literal_display(lit)),
            DocConstValue::Elicit => out.push('_'),
        },
        EnvValueDoc::ElicitType | EnvValueDoc::ElicitConst => out.push('_'),
    }
}

/// The plain qualified name of a type name without its parameters, e.g.
/// `example::types::SVector` or `f32`.
fn base_name_display(name: &TypeNameDoc) -> String {
    let mut out = String::new();
    for (i, segment) in name.segments().iter().enumerate() {
        if i > 0 {
            out.push_str("::");
        }
        out.push_str(&segment.name.display());
    }
    out
}

/// Formats a literal value the same way the core `Display` impl does.
fn literal_display(lit: &EdlLiteralValue) -> String {
    match lit {
        EdlLiteralValue::Usize(val) => format!("{val}_usize"),
        EdlLiteralValue::Isize(val) => format!("{val}_isize"),
        EdlLiteralValue::U8(val) => format!("{val}_u8"),
        EdlLiteralValue::U16(val) => format!("{val}_u16"),
        EdlLiteralValue::U32(val) => format!("{val}_u32"),
        EdlLiteralValue::U64(val) => format!("{val}_u64"),
        EdlLiteralValue::U128(val) => format!("{val}_u128"),
        EdlLiteralValue::I8(val) => format!("{val}_i8"),
        EdlLiteralValue::I16(val) => format!("{val}_i16"),
        EdlLiteralValue::I32(val) => format!("{val}_i32"),
        EdlLiteralValue::I64(val) => format!("{val}_i64"),
        EdlLiteralValue::I128(val) => format!("{val}_i128"),
        EdlLiteralValue::Bool(val) => val.to_string(),
        EdlLiteralValue::Str(val) => format!("\"{val}\""),
        EdlLiteralValue::Char(val) => format!("'{}'", val.to_string().replace("\n", "\\n")),
        EdlLiteralValue::Empty() => "()".to_string(),
    }
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

    fn src_pos() -> SrcPos {
        SrcPos {
            line: 1,
            col: 1,
            size: 1,
        }
    }

    /// A base type reference with a single-segment name.
    fn base_ty(name: &str) -> TypeDoc {
        TypeDoc::Base(
            TypeNameDoc(vec![TypeNameSegmentDoc {
                name: QualifierName {
                    path: vec![name.to_string()],
                },
                parameters: EnvInstDoc::default(),
                pos: None,
            }]),
            None,
        )
    }

    /// Round-trips an item through its blob serialization and decomposes its owner.
    fn owner_of(item: &Item, qual_name: &str) -> ItemOwner {
        let parsed = parse_item(&serde_json::to_string(item).unwrap()).unwrap();
        item_owner(&parsed, qual_name)
    }

    /// An impl item of a core (single-segment) type: the type is the owner, there is no module.
    #[test]
    fn item_owner_impl_of_core_type() {
        let item = Item::Func(FuncDoc {
            name: QualifierName {
                path: vec!["add".to_string()],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: src_pos(),
            doc: String::new(),
            env: EnvDoc::default(),
            params: FuncParamsDoc::default(),
            ret: base_ty("usize"),
            ms: Modifiers::default(),
            async_return: false,
            associated_type: Some(base_ty("usize")),
        });
        let owner = owner_of(&item, "usize::add");
        assert!(owner.has_type());
        assert!(owner.module.is_empty());
        assert_eq!(owner.type_name.as_deref(), Some("usize"));
        assert_eq!(owner.type_display.as_deref(), Some("usize"));
        assert_eq!(owner.type_href.as_deref(), Some("/module/usize"));
    }

    /// An impl item of a project type: the type's module is the item's module, and the type's
    /// generic parameters are part of its display.
    #[test]
    fn item_owner_impl_of_project_type() {
        let mut seg = TypeNameSegmentDoc {
            name: QualifierName {
                path: vec![
                    "example".to_string(),
                    "types".to_string(),
                    "SVector".to_string(),
                ],
            },
            parameters: EnvInstDoc::default(),
            pos: None,
        };
        seg.parameters = EnvInstDoc {
            params: vec![
                EnvValueDoc::Type {
                    ty: base_ty("f32"),
                    pos: None,
                },
                EnvValueDoc::Const {
                    val: DocConstValue::Const(TypeNameDoc(vec![TypeNameSegmentDoc {
                        name: QualifierName {
                            path: vec!["N".to_string()],
                        },
                        parameters: EnvInstDoc::default(),
                        pos: None,
                    }])),
                    pos: None,
                },
            ],
        };
        let item = Item::Func(FuncDoc {
            name: QualifierName {
                path: vec![
                    "example".to_string(),
                    "types".to_string(),
                    "SVector".to_string(),
                    "norm".to_string(),
                ],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: src_pos(),
            doc: String::new(),
            env: EnvDoc::default(),
            params: FuncParamsDoc::default(),
            ret: base_ty("f32"),
            ms: Modifiers::default(),
            async_return: false,
            associated_type: Some(TypeDoc::Base(TypeNameDoc(vec![seg]), None)),
        });
        let owner = owner_of(&item, "example::types::SVector::norm");
        assert_eq!(owner.module, vec!["example".to_string(), "types".to_string()]);
        assert_eq!(owner.type_name.as_deref(), Some("SVector"));
        assert_eq!(owner.type_display.as_deref(), Some("SVector<f32, N>"));
        assert_eq!(owner.type_href.as_deref(), Some("/item/example::types::SVector"));
    }

    /// A plain item is owned by the module path that precedes its name.
    #[test]
    fn item_owner_plain_item() {
        let item = Item::TypeDef(TypeDefDoc {
            name: QualifierName {
                path: vec![
                    "example".to_string(),
                    "types".to_string(),
                    "Point".to_string(),
                ],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: src_pos(),
            doc: String::new(),
            env: EnvDoc::default(),
            params: FuncParamsDoc::default(),
            variant: TypeDefVariant::Struct(StructTypeDoc::ZeroSized),
        });
        let owner = owner_of(&item, "example::types::Point");
        assert!(!owner.has_type());
        assert_eq!(owner.module, vec!["example".to_string(), "types".to_string()]);
        assert!(owner.type_display.is_none());
        assert!(owner.type_href.is_none());
    }

    /// A crate-root item has no owner at all.
    #[test]
    fn item_owner_root_item() {
        let item = Item::GlobalVar(LetDoc {
            name: QualifierName {
                path: vec!["pi".to_string()],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: src_pos(),
            doc: String::new(),
            ty: base_ty("f32"),
            ms: Modifiers::default(),
        });
        let owner = owner_of(&item, "pi");
        assert!(!owner.has_type());
        assert!(owner.module.is_empty());
    }

    /// A constant associated with a type decomposes like a function.
    #[test]
    fn item_owner_associated_const() {
        let item = Item::GlobalConst(ConstDoc {
            name: QualifierName {
                path: vec!["MAX".to_string()],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: src_pos(),
            doc: String::new(),
            ty: base_ty("u8"),
            ms: Modifiers::default(),
            associated_type: Some(base_ty("u8")),
        });
        let owner = owner_of(&item, "u8::MAX");
        assert!(owner.module.is_empty());
        assert_eq!(owner.type_name.as_deref(), Some("u8"));
        assert_eq!(owner.type_display.as_deref(), Some("u8"));
        assert_eq!(owner.type_href.as_deref(), Some("/module/u8"));
    }

    /// When the blob's associated type disagrees with the stored qualified name, the plain
    /// decomposition is used.
    #[test]
    fn item_owner_falls_back_on_inconsistent_qual_name() {
        let item = Item::Func(FuncDoc {
            name: QualifierName {
                path: vec!["add".to_string()],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: src_pos(),
            doc: String::new(),
            env: EnvDoc::default(),
            params: FuncParamsDoc::default(),
            ret: base_ty("usize"),
            ms: Modifiers::default(),
            async_return: false,
            associated_type: Some(base_ty("usize")),
        });
        let owner = owner_of(&item, "other::add");
        assert!(!owner.has_type());
        assert_eq!(owner.module, vec!["other".to_string()]);
    }
}
