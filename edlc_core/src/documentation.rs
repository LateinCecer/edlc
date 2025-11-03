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
use crate::core::edl_value::EdlLiteralValue;
use crate::core::edl_var::EdlVarRegistry;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::prelude::edl_type::EdlTypeRegistry;
use crate::resolver::QualifierName;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum PortableModuleSrc {
    File(String),
    String {
        path: String,
        pos: SrcPos,
        src: String,
    },
}

impl From<ModuleSrc> for PortableModuleSrc {
    fn from(value: ModuleSrc) -> Self {
        match value {
            ModuleSrc::File(path) => PortableModuleSrc::File(path.as_ref().path.clone().into_os_string().into_string().unwrap()),
            ModuleSrc::String(src) => PortableModuleSrc::String {
                path: src.relative_path.clone().into_os_string().into_string().unwrap(),
                src: src.src.clone(),
                pos: src.pos,
            }
        }
    }
}

/// Defines the documentation of a global variable.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct LetDoc {
    pub name: QualifierName,
    pub src: PortableModuleSrc,
    pub pos: SrcPos,
    pub doc: String,
    pub ty: TypeDoc,
    pub ms: Modifiers,
}

impl Display for LetDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = *self.name.last().as_ref().unwrap();
        write!(f, "let {}{name}: {}", self.ms, self.ty)
    }
}

/// Defines a constant definition documentation.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct ConstDoc {
    pub name: QualifierName,
    pub src: PortableModuleSrc,
    pub pos: SrcPos,
    pub doc: String,
    pub ty: TypeDoc,
    pub ms: Modifiers,
    pub associated_type: Option<TypeDoc>,
}

impl Display for ConstDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = *self.name.last().as_ref().unwrap();
        write!(f, "const {}{name}: {}", self.ms, self.ty)
    }
}

/// Defines a function signature documentation item.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct FuncDoc {
    pub name: QualifierName,
    pub src: PortableModuleSrc,
    pub pos: SrcPos,
    pub doc: String,
    pub env: EnvDoc,
    pub params: FuncParamsDoc,
    pub ret: TypeDoc,
    pub ms: Modifiers,
    pub associated_type: Option<TypeDoc>,
}

impl Display for FuncDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = *self.name.last().as_ref().unwrap();
        write!(f, "{}fn {name}{}({})", self.ms, self.env, self.params)?;
        if !matches!(self.ret, TypeDoc::Empty) {
            write!(f, " -> {}", self.ret)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct FuncParamsDoc(Vec<FuncParamDoc>);

impl From<Vec<FuncParamDoc>> for FuncParamsDoc {
    fn from(value: Vec<FuncParamDoc>) -> Self {
        FuncParamsDoc(value)
    }
}

impl Display for FuncParamsDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fmt_list(&self.0, f, "", "", ", ")
    }
}

/// Defines a function parameter documentation item.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct FuncParamDoc {
    pub name: String,
    pub pos: SrcPos,
    pub ty: TypeDoc,
    pub ms: Modifiers,
}

impl Display for FuncParamDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}: {}", self.ms, self.name, self.ty)
    }
}

#[derive(Clone, Debug, PartialEq, Default, Serialize)]
pub struct Modifiers(Vec<Modifier>);

impl Modifiers {
    pub fn push(&mut self, m: Modifier) {
        self.0.push(m);
    }
}

impl Display for Modifiers {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for m in self.0.iter() {
            write!(f, "{m} ")?;
        }
        Ok(())
    }
}

/// Defines a modifier.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Modifier {
    Comptime,
    MaybeComptime,
    Mut,
    Async,
}

impl Display for Modifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Modifier::Comptime => write!(f, "comptime"),
            Modifier::MaybeComptime => write!(f, "?comptime"),
            Modifier::Mut => write!(f, "mut"),
            Modifier::Async => write!(f, "async"),
        }
    }
}

/// Defines the values provided to instantiate a generic parameter environment.
#[derive(Clone, Debug, PartialEq, Default, Serialize)]
pub struct EnvInstDoc {
    pub params: Vec<EnvValueDoc>,
}

impl EnvInstDoc {
    fn is_empty(&self) -> bool {
        self.params.is_empty()
    }
}

impl Display for EnvInstDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fmt_list(&self.params, f, "<", ">", ", ")
    }
}

/// Defines the value of a generic parameter.
#[derive(Clone, Debug, PartialEq, Serialize)]
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

impl Display for EnvValueDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type { ty, .. } => write!(f, "{ty}"),
            Self::Const { val, .. } => write!(f, "{val}"),
            Self::ElicitType | Self::ElicitConst => write!(f, "_"),
        }
    }
}

/// Defines the definition of a generic parameter environment.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct EnvDoc {
    pub params: Vec<EnvParamDoc>,
}

impl EnvDoc {
    fn is_empty(&self) -> bool {
        self.params.is_empty()
    }
}

impl Display for EnvDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fmt_list(&self.params, f, "<", ">", ", ")
    }
}

fn fmt_list<T: Display>(values: &[T], f: &mut Formatter<'_>, start: &str, end: &str, delimiter: &str) -> std::fmt::Result {
    if values.is_empty() {
        return Ok(());
    }
    write!(f, "{start}")?;
    let mut not_first = false;
    for param in values.iter() {
        if not_first {
            write!(f, "{delimiter}")?;
        }
        not_first = true;
        write!(f, "{param}")?;
    }
    write!(f, "{end}")
}

/// Defines the definition of a generic parameter type.
#[derive(Clone, Debug, PartialEq, Serialize)]
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

impl Display for EnvParamDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EnvParamDoc::Type { name, .. } => {
                write!(f, "{name}")
            }
            EnvParamDoc::Const { name, ty, .. } => {
                write!(f, "const {name}: {ty}")
            }
        }
    }
}

/// Defines the documentation of a type identifier.
#[derive(Clone, Debug, PartialEq, Serialize)]
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

impl Display for TypeDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Base(name, _) => <TypeNameDoc as Display>::fmt(name, f),
            Self::Array(base, len, _) => write!(f, "[{base}; {len}]"),
            Self::Slice(base, _) => write!(f, "[{base}]"),
            Self::Ref(base, _) => write!(f, "&{base}"),
            Self::MutRef(base, _) => write!(f, "&mut {base}"),
            Self::Empty => write!(f, "()"),
            Self::Tuple(contents, _) => {
                write!(f, "(")?;
                for item in contents.iter() {
                    write!(f, "{item}, ")?;
                }
                write!(f, ")")
            },
            TypeDoc::Elicit => write!(f, "_"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct TypeNameDoc(Vec<TypeNameSegmentDoc>);

impl From<Vec<TypeNameSegmentDoc>> for TypeNameDoc {
    fn from(value: Vec<TypeNameSegmentDoc>) -> Self {
        TypeNameDoc(value)
    }
}

impl From<String> for TypeNameDoc {
    fn from(value: String) -> Self {
        TypeNameDoc(vec![value.into()])
    }
}

impl From<QualifierName> for TypeNameDoc {
    fn from(value: QualifierName) -> Self {
        TypeNameDoc(vec![value.into()])
    }
}

impl Display for TypeNameDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut not_first = false;
        for param in self.0.iter() {
            if not_first {
                write!(f, "::")?;
            }
            not_first = true;
            write!(f, "{}", param)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct TypeNameSegmentDoc {
    pub name: QualifierName,
    pub parameters: EnvInstDoc,
    pub pos: Option<SrcPos>,
}

impl From<String> for TypeNameSegmentDoc {
    fn from(value: String) -> Self {
        TypeNameSegmentDoc {
            name: value.into(),
            parameters: EnvInstDoc::default(),
            pos: None,
        }
    }
}

impl From<QualifierName> for TypeNameSegmentDoc {
    fn from(value: QualifierName) -> Self {
        TypeNameSegmentDoc {
            name: value,
            parameters: EnvInstDoc::default(),
            pos: None,
        }
    }
}

impl Display for TypeNameSegmentDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.parameters.is_empty() {
            write!(f, "::{}", self.parameters)?;
        }
        Ok(())
    }
}

pub struct DocCompilerState<'a> {
    pub types: &'a EdlTypeRegistry,
    pub vars: &'a EdlVarRegistry,
}

/// Implemented by all compiler items which can be transformed into document items from this
/// module.
pub trait DocElement {
    type Doc: Display;

    /// Generates a documentation for the item
    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc;
}

/// Implemented by all compiler items which can be transformed into document items from this
/// module.
pub trait DocElementPhased {
    type Doc: Display;

    /// Generates a documentation for the item
    fn doc(&self, phase: &mut HirPhase) -> Result<Self::Doc, HirPhase>;
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum DocConstValue {
    Const(TypeNameDoc),
    Literal(EdlLiteralValue),
    Elicit,
}

impl Display for DocConstValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DocConstValue::Const(val) => write!(f, "{val}"),
            DocConstValue::Literal(val) => write!(f, "{val}"),
            DocConstValue::Elicit => write!(f, "_"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct ModuleDoc {
    pub name: QualifierName,
    pub doc: String,
}

impl Display for ModuleDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "mod {}", self.name)
    }
}


/// Contains a documented item.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Item {
    GlobalVar(LetDoc),
    GlobalConst(ConstDoc),
    Func(FuncDoc),
    Module(ModuleDoc),
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::GlobalVar(val) => write!(f, "{val}"),
            Item::GlobalConst(val) => write!(f, "{val}"),
            Item::Func(val) => write!(f, "{val}"),
            Item::Module(val) => write!(f, "{val}"),
        }
    }
}

impl From<ModuleDoc> for Item {
    fn from(value: ModuleDoc) -> Self {
        Item::Module(value)
    }
}

impl From<LetDoc> for Item {
    fn from(value: LetDoc) -> Self {
        Item::GlobalVar(value)
    }
}

impl From<ConstDoc> for Item {
    fn from(value: ConstDoc) -> Self {
        Item::GlobalConst(value)
    }
}

impl From<FuncDoc> for Item {
    fn from(value: FuncDoc) -> Self {
        Item::Func(value)
    }
}

/// A [DocGenerator] can be used to generate documentation for any EDL project.
/// It is essentially a facade to pass documented items from the compiler itself to an arbitrary
/// implementation of a documentation generator.
/// The generator must accept HIR-level edl language items, which are then processed and rendered
/// to some sort of viewable documentation.
/// The specifics of this process are up to the implementation of the [DocGenerator].
pub trait DocGenerator {
    type Error;
    fn insert_definition(&mut self, item: &Item) -> Result<(), Self::Error>;
}
