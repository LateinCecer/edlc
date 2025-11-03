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

mod namespace;

use std::collections::{HashMap};
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};
use rand::{Rng, thread_rng};
use log::debug;
use serde::{Deserialize, Serialize};
use crate::core::edl_error::EdlError;
use crate::core::edl_fn::{EdlFnSignature, EdlFunctionBody, EdlPreSignature};
use crate::core::edl_param_env::{EdlParameterEnv};
use crate::core::edl_trait::EdlTraitId;
use crate::core::edl_type::{EdlAliasId, EdlConstId, EdlEnvId, EdlTraitInstance, EdlType, EdlTypeId, EdlTypeInstance, EdlTypeRegistry, EdlTypeState, FunctionState};
use crate::core::edl_value::EdlConstValue;
use crate::core::EdlVarId;
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::resolver::namespace::Namespace;


#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct QualifierName {
    path: Vec<String>,
}

impl QualifierName {
    /// Trims the qualifier name by removing `items` elements from the end of the name path.
    /// A trim must always contain at least one element.
    /// Thus, if `items == self.len()`, `None` is returned, also.
    pub fn trim(&self, items: usize) -> Option<Self> {
        if items >= self.path.len() {
            return None;
        }
        // trim the qualifier name
        let mut name_c = QualifierName::empty();
        self.path[0..(self.path.len() - items)].iter()
            .for_each(|i| name_c.push(i.clone()));
        Some(name_c)
    }

    /// Transforms the qualifier path into a file path by inserting separating strings between
    /// the items that constitute the qualifier name.
    pub fn to_file_path(&self, separator: &str) -> String {
        let mut path = String::new();
        let mut iter = self.path.iter();

        if let Some(first) = iter.next() {
            path.push_str(first);
        }
        for s in iter {
            path.push_str(separator);
            path.push_str(s);
        }
        path
    }
}

impl Display for QualifierName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.path.iter();
        if let Some(item) = iter.next() {
            write!(f, "{item}")?;
            for item in iter {
                write!(f, "::{item}")?;
            }
        }
        Ok(())
    }
}

impl QualifierName {
    pub fn empty() -> Self {
        QualifierName {
            path: Vec::new(),
        }
    }
}

impl From<Vec<String>> for QualifierName {
    fn from(value: Vec<String>) -> Self {
        QualifierName {
            path: value,
        }
    }
}

impl From<Vec<&str>> for QualifierName {
    fn from(value: Vec<&str>) -> Self {
        QualifierName {
            path: value.iter().map(|item| item.to_string()).collect()
        }
    }
}

impl From<&String> for QualifierName {
    fn from(value: &String) -> Self {
        QualifierName {
            path: vec![value.clone()],
        }
    }
}

impl From<String> for QualifierName {
    fn from(value: String) -> Self {
        QualifierName {
            path: vec![value],
        }
    }
}

impl Deref for QualifierName {
    type Target = Vec<String>;

    fn deref(&self) -> &Self::Target {
        &self.path
    }
}

impl DerefMut for QualifierName {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.path
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum ResolveError {
    ItemRedefinition(QualifierName),
    EmptyTypeRegistry(SrcPos),
    EmptyStack,
    InvalidUse(QualifierName),
    UnknownItem(QualifierName),
    EmptyQualifierName,
    UnknownAssociate(EdlTypeInstance),
    UnknownAssociateTrait(EdlTraitInstance),
}

impl Display for ResolveError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::EmptyTypeRegistry(pos) => {
                write!(f, "Unable to register empty type signature defined at {pos}")
            }
            ResolveError::EmptyStack => {
                write!(f, "The resolver stack is empty")
            }
            ResolveError::InvalidUse(name) => {
                write!(f, "Qualifier name '{name}' does not point to a namespace or item in this project")
            }
            ResolveError::EmptyQualifierName => {
                write!(f, "Empty qualifier name")
            }
            ResolveError::ItemRedefinition(name) => {
                write!(f, "Attempted to redefine type with name {name}")
            }
            ResolveError::UnknownItem(name) => {
                write!(f, "Unknown item with name {name}")
            }
            ResolveError::UnknownAssociate(ty) => {
                write!(f, "Cannot associate items to type {ty:?} as it is not registered as an associate")
            }
            ResolveError::UnknownAssociateTrait(ty) => {
                write!(f, "Cannot associate items to trait {ty:?} as it is not registered as an associate")
            }
        }
    }
}

impl std::error::Error for ResolveError {}


#[derive(Clone, Debug, Eq, PartialEq)]
pub enum StructType {
    Struct(),
    Union(),
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct StructDef {
    pos: SrcPos,
    name: String,
    id: EdlTypeId,
}


#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
struct VarDef {
    pos: SrcPos,
    name: String,
    id: EdlTypeId,
}


#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct ScopeId(usize, usize);

impl ScopeId {
    pub fn rand() -> Self {
        let mut rng = thread_rng();
        ScopeId(rng.gen(), rng.gen())
    }
}


/// A local item is an item that exists only within a local context.
/// Prime examples of this are item that only exist within the scope of a function or
/// implementation.
#[derive(Debug, Clone, PartialEq)]
pub enum LocalItem {
    GenericType(EdlTypeInstance),
    GenericConst(EdlEnvId, usize),
    Var(EdlVarId),
}


struct StackLvl {
    _id: ScopeId,
    imports: Imports,
    stack: Vec<ScopeId>,
    is_module: bool,
    _items: Namespace<Item>,
    local_items: HashMap<String, LocalItem>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum SelfType {
    Type(EdlTypeInstance),
    Trait(EdlTraitInstance),
    None,
}

impl SelfType {
    fn is_none(&self) -> bool {
        matches!(self, SelfType::None)
    }
}

#[derive(Default)]
struct Association {
    namespace: Option<QualifierName>,
    associate_types: HashMap<String, EdlTypeInstance>,
    associate_alias: HashMap<String, EdlAliasId>,
    associate_const: HashMap<String, EdlConstId>,
}

impl Association {
    fn resolve<'a>(
        &self,
        path: &QualifierName,
        namespace: &'a Namespace<Item>,
    ) -> Option<&'a Item> {
        if let Some(base_name) = self.namespace.as_ref() {
            let mut base = base_name.clone();
            base.append(&mut path.clone());
            namespace.resolve(&base)
        } else {
            None
        }
    }

    fn resolve_type(
        &self,
        path: &QualifierName,
        namespace: &Namespace<Item>,
        edl_type_registry: &EdlTypeRegistry,
    ) -> Option<EdlTypeInstance> {
        if path.len() == 1 {
            if let Some(ty) = self.associate_types.get(&path[0]) {
                return Some(ty.clone());
            }
        }

        if let Some(base_name) = self.namespace.as_ref() {
            let mut base = base_name.clone();
            base.append(&mut path.clone());
            match namespace.resolve(&base) {
                Some(Item { variant: ItemVariant::Type(id), .. }) => {
                    if let Some(EdlType::Type { .. } | EdlType::Generic { .. }) = edl_type_registry.get_type(*id) {
                        edl_type_registry.new_type_instance(*id)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        } else {
            None
        }
    }

    fn resolve_alias(
        &self,
        path: &QualifierName,
        namespace: &Namespace<Item>,
    ) -> Option<EdlAliasId> {
        if path.len() == 1 {
            if let Some(ty) = self.associate_alias.get(&path[0]) {
                return Some(*ty);
            }
        }

        if let Some(base_name) = self.namespace.as_ref() {
            let mut base = base_name.clone();
            base.append(&mut path.clone());
            match namespace.resolve(&base) {
                Some(Item { variant: ItemVariant::Alias(id), .. }) => Some(*id),
                _ => None,
            }
        } else {
            None
        }
    }

    fn resolve_const(
        &self,
        path: &QualifierName,
        namespace: &Namespace<Item>,
    ) -> Option<EdlConstValue> {
        if path.len() == 1 {
            if let Some(ty) = self.associate_const.get(&path[0]) {
                return Some(EdlConstValue::Const(*ty));
            }
        }
        if let Some(base_name) = self.namespace.as_ref() {
            let mut base = base_name.clone();
            base.append(&mut path.clone());
            match namespace.resolve(&base) {
                Some(Item { variant: ItemVariant::Const(id, ..), .. }) => {
                    Some(EdlConstValue::Const(*id))
                }
                _ => None,
            }
        } else {
            None
        }
    }
}

struct Imports {
    namespaces: HashMap<String, QualifierName>,
    base_namespace: QualifierName,
    associations: HashMap<EdlTypeInstance, Association>,
    trait_associations: HashMap<EdlTraitInstance, Association>,
    self_type: SelfType,
}

impl Imports {
    fn new(base_namespace: QualifierName) -> Self {
        Imports {
            namespaces: HashMap::new(),
            base_namespace,
            associations: HashMap::new(),
            trait_associations: HashMap::new(),
            self_type: SelfType::None,
        }
    }

    /// Resolves a local type based on the current scope
    fn resolve_local<'a>(
        &self,
        path: &QualifierName,
        namespace: &'a Namespace<Item>
    ) -> Option<&'a Item> {
        let mut qualifier_name = self.base_namespace.clone();
        for next in path.iter() {
            qualifier_name.push(next.clone());
        }
        namespace.resolve(&qualifier_name)
    }

    /// Resolves a qualifier path to an item using the specified namespace hierarchy and local
    /// imports / `use` statements.
    fn resolve_type<'a>(
        &self,
        path: &QualifierName,
        namespace: &'a Namespace<Item>
    ) -> Option<&'a Item> {
        if let Some(first) = path.first() {
            // look for first entry in imported namespaces and filter from there
            if let Some(search_path) = self.namespaces.get(first) {
                let mut iter = path.iter();
                iter.next(); // pop first element
                let mut path = search_path.clone();
                for next in iter {
                    path.push(next.clone());
                }
                // try to find item in the namespace tree
                if let Some(item) = namespace.resolve(&path) {
                    return Some(item);
                }
            }

            // try to resolve by prefixing the path with the local base namespace
            let mut qualifier_name = self.base_namespace.clone();
            for next in path.iter() {
                qualifier_name.push(next.clone());
            }
            if let Some(item) = namespace.resolve(&qualifier_name) {
                return Some(item);
            }

            // search from the bottom of the namespace instead
        }
        // see if there is anything in the root of the namespace hierarchy
        namespace.resolve(path)
    }

    // fn resolve_type_mut<'a, 'b: 'a>(
    //     &self,
    //     path: &QualifierName,
    //     namespace: &'a mut Namespace<Item>,
    // ) -> Option<&'b mut Item> {
    //     if let Some(first) = path.first() {
    //         if let Some(search_path) = self.namespaces.get(first) {
    //             let mut iter = path.iter();
    //             iter.next();
    //             let mut path = search_path.clone();
    //             for next in iter {
    //                 path.push(next.clone());
    //             }
    //
    //             if let Some(item) = namespace.resolve_mut(&path) {
    //                 return Some(item);
    //             }
    //         }
    //
    //         let mut qualifier_name = self.base_namespace.clone();
    //         for next in path.iter() {
    //             qualifier_name.push(next.clone());
    //         }
    //         if let Some(item) = namespace.resolve_mut(&qualifier_name) {
    //             return Some(item);
    //         }
    //     }
    //     namespace.resolve_mut(path)
    // }


    /// Inserts a reference to a namespace.
    ///
    /// # Example
    ///
    /// In the code, this would look like this:
    /// ``
    /// use path::to::lib;
    /// ``
    fn insert_namespace(
        &mut self,
        namespace: &Namespace<Item>,
        path: QualifierName
    ) -> Result<(), ResolveError> {
        if let Some(last) = path.last() {
            let last = last.to_owned();
            // check if the path leads to a namespace
            if namespace.is_namespace(&path) || namespace.is_item(&path) {
                self.namespaces.entry(last).or_insert(path);
                Ok(())
            } else {
                Err(ResolveError::InvalidUse(path))
            }
        } else {
            Err(ResolveError::EmptyQualifierName)
        }
    }

    /// Resolves qualifier names which are associated with a type.
    fn resolve_association<'a>(
        &self,
        ty: &EdlTypeInstance,
        path: &QualifierName,
        namespace: &'a Namespace<Item>
    ) -> Option<&'a Item> {
        if let Some(base) = self.associations.get(ty) {
            base.resolve(path, namespace)
        } else {
            None
        }
    }

    fn resolve_trait_association<'a>(
        &self,
        ty: &EdlTraitInstance,
        path: &QualifierName,
        namespace: &'a Namespace<Item>
    ) -> Option<&'a Item> {
        if let Some(base) = self.trait_associations.get(ty) {
            base.resolve(path, namespace)
        } else {
            None
        }
    }

    fn resolve_associated_type(
        &self,
        ty: &EdlTypeInstance,
        path: &QualifierName,
        namespace: &Namespace<Item>,
        edl_reg: &EdlTypeRegistry,
    ) -> Option<EdlTypeInstance> {
        self.associations.get(ty)
            .and_then(|association| association.resolve_type(path, namespace, edl_reg))
    }

    fn resolve_trait_associated_type(
        &self,
        ty: &EdlTraitInstance,
        path: &QualifierName,
        namespace: &Namespace<Item>,
        edl_reg: &EdlTypeRegistry,
    ) -> Option<EdlTypeInstance> {
        self.trait_associations.get(ty)
            .and_then(|association| association.resolve_type(path, namespace, edl_reg))
    }

    fn resolve_associated_alias(
        &self,
        ty: &EdlTypeInstance,
        path: &QualifierName,
        namespace: &Namespace<Item>,
    ) -> Option<EdlAliasId> {
        self.associations.get(ty)
            .and_then(|association| association.resolve_alias(path, namespace))
    }

    fn resolve_trait_associated_alias(
        &self,
        ty: &EdlTraitInstance,
        path: &QualifierName,
        namespace: &Namespace<Item>,
    ) -> Option<EdlAliasId> {
        self.trait_associations.get(ty)
            .and_then(|association| association.resolve_alias(path, namespace))
    }

    fn resolve_associated_const(
        &self,
        ty: &EdlTypeInstance,
        path: &QualifierName,
        namespace: &Namespace<Item>,
    ) -> Option<EdlConstValue> {
        self.associations.get(ty)
            .and_then(|association| association.resolve_const(path, namespace))
    }

    fn resolve_trait_associated_const(
        &self,
        ty: &EdlTraitInstance,
        path: &QualifierName,
        namespace: &Namespace<Item>,
    ) -> Option<EdlConstValue> {
        self.trait_associations.get(ty)
            .and_then(|association| association.resolve_const(path, namespace))
    }

    fn insert_associated_type(
        &mut self,
        ty: &EdlTypeInstance,
        name: String,
        associated: EdlTypeInstance,
    ) -> Result<(), ResolveError> {
        self.associations.entry(ty.clone())
            .or_default()
            .associate_types.insert(name, associated);
        Ok(())
    }

    fn insert_trait_associated_type(
        &mut self,
        ty: &EdlTraitInstance,
        name: String,
        associated: EdlTypeInstance,
    ) -> Result<(), ResolveError> {
        self.trait_associations.entry(ty.clone())
            .or_default()
            .associate_types.insert(name, associated);
        Ok(())
    }

    fn insert_associated_alias(
        &mut self,
        ty: &EdlTypeInstance,
        name: String,
        associated: EdlAliasId,
    ) -> Result<(), ResolveError> {
        self.associations.entry(ty.clone())
            .or_default()
            .associate_alias.insert(name, associated);
        Ok(())
    }

    fn insert_trait_associated_alias(
        &mut self,
        ty: &EdlTraitInstance,
        name: String,
        associated: EdlAliasId,
    ) -> Result<(), ResolveError> {
        self.trait_associations.entry(ty.clone())
            .or_default()
            .associate_alias.insert(name, associated);
        Ok(())
    }

    fn insert_associated_const(
        &mut self,
        ty: &EdlTypeInstance,
        name: String,
        associated: EdlConstId,
    ) -> Result<(), ResolveError> {
        self.associations.entry(ty.clone())
            .or_default()
            .associate_const.insert(name, associated);
        Ok(())
    }

    fn insert_trait_associated_const(
        &mut self,
        ty: &EdlTraitInstance,
        name: String,
        associated: EdlConstId,
    ) -> Result<(), ResolveError> {
        self.trait_associations.entry(ty.clone())
            .or_default()
            .associate_const.insert(name, associated);
        Ok(())
    }

    /// Inserts a new association into the local imports.
    /// This is usually done whenever it is clear that an item as access to associated functions or
    /// types as defined through an implemented trait.
    ///
    /// # Scenario 1
    ///
    /// Let's consider the body of a trait implementation for type `Vec<T>`:
    ///
    /// ```
    /// trait Index<I> {
    ///     type Output;
    ///     fn index(&self, index: I) -> &Self::Output;
    /// }
    ///
    /// impl<T> Index<usize> for Vec<T> {
    ///     type Output = T;
    ///     fn index(&self, index: usize) -> &Self::Output { todo!() }
    /// }
    /// ```
    ///
    /// In this example, `Output` is known to be a valid associated type for `Self`, since
    /// `Self` is an alias for `Vec<T>` which must implement `Index<usize>` as this code is in
    /// the very definition of that implementation.
    ///
    /// # Scenario 2
    ///
    /// We can also force a type to implementation a trait:
    ///
    /// ```
    /// # use core::ops::Index;
    /// let x: <Vec<f32> as Index<usize>>::Output = todo!();
    /// ```
    ///
    /// In this case we know that `Vec<f32>` implements `Index<usize>` (and thus must define the
    /// associated type `Output`) since `<Vec<32> as Index<usize>>` would fail otherwise.
    ///
    /// # Scenario 3
    ///
    /// We can also infer that a trait is implemented for a specific type by using type constraints
    /// in the definition of generic parameter environments:
    ///
    /// ```
    /// # use core::ops::Index;
    /// fn foo<T>() -> Vec::<T>::Output where Vec<T>: Index<usize> { todo!() }
    /// ```
    fn insert_association(&mut self, ty_id: EdlTypeInstance, namespace: QualifierName) {
        self.associations.entry(ty_id)
            .or_default()
            .namespace = Some(namespace);
    }

    fn insert_trait_association(&mut self, ty_id: EdlTraitInstance, namespace: QualifierName) {
        self.trait_associations.entry(ty_id)
            .or_default()
            .namespace = Some(namespace);
    }

    /// Tries to resolve an item related to the `Self` type which can be used in the bodies of
    /// implementations and traits.
    fn resolve_self<'a>(&self, path: &QualifierName, namespace: &'a Namespace<Item>) -> Option<&'a Item> {
        match &self.self_type {
            SelfType::Type(ty) => self.resolve_association(ty, path, namespace),
            SelfType::Trait(trait_id) => self.resolve_trait_association(trait_id, path, namespace),
            SelfType::None => None
        }
    }

    fn resolve_self_type(&self, path: &QualifierName, namespace: &Namespace<Item>, type_reg: &EdlTypeRegistry) -> Option<EdlTypeInstance> {
        match &self.self_type {
            SelfType::Type(ty) => self.resolve_associated_type(ty, path, namespace, type_reg),
            SelfType::Trait(ty) => self.resolve_trait_associated_type(ty, path, namespace, type_reg),
            SelfType::None => None,
        }
    }

    fn resolve_self_alias(&self, path: &QualifierName, namespace: &Namespace<Item>) -> Option<EdlAliasId> {
        match &self.self_type {
            SelfType::Type(ty) => self.resolve_associated_alias(ty, path, namespace),
            SelfType::Trait(ty) => self.resolve_trait_associated_alias(ty, path, namespace),
            SelfType::None => None,
        }
    }

    fn insert_self_type(&mut self, ty: EdlTypeInstance) {
        assert!(
            self.self_type.is_none() || matches!(&self.self_type, SelfType::Type(sty) if *sty == ty),
            "self type already set for current scope"
        );
        self.self_type = SelfType::Type(ty);
    }

    fn insert_self_trait(&mut self, ty: EdlTraitInstance) {
        assert!(
            self.self_type.is_none() || matches!(&self.self_type, SelfType::Trait(sty) if *sty == ty),
            "self type already set for current scope"
        );
        self.self_type = SelfType::Trait(ty);
    }
}



#[derive(Debug)]
pub struct Item {
    pub scope: ScopeId,
    pub variant: ItemVariant
}


#[derive(Debug)]
pub enum ItemVariant {
    Type(EdlTypeId),
    Fn(EdlTypeId),
    Const(EdlConstId),
    Var(EdlVarId),
    Trait(EdlTraitId),
    /// An alias item is a combination of the reference base type and the parameter environment of
    /// the alias type signature.
    Alias(EdlAliasId),
    EnumVariant(EdlTypeId, String),
}


#[derive(Debug, Clone)]
pub enum ItemInit {
    Type {
        params: EdlParameterEnv,
    },
    Function {
        sig: EdlFnSignature,
        scope: ScopeId,
    },
    PreFunction {
        sig: EdlPreSignature,
        scope: ScopeId,
    },
    Const {
        ty: EdlTypeId,
        id: EdlConstId,
    },
    Let {
        id: EdlVarId,
    },
    Trait {
        id: EdlTraitId,
    },
    Alias(EdlEnvId, ModuleSrc, SrcPos),
    EnumVariant {
        id: EdlTypeId,
        name: String,
    }
}

struct Stack {
    stack: Vec<ScopeId>,
    memory: HashMap<ScopeId, StackLvl>,
}

impl Stack {
    fn new() -> Self {
        Stack {
            stack: Vec::new(),
            memory: HashMap::new(),
        }
    }

    fn revert_to_scope(&mut self, scope: &ScopeId) {
        if let Some(lvl) = self.memory.get(scope) {
            self.stack.clear();
            for &el in &lvl.stack {
                self.stack.push(el);
            }
        }
    }

    fn current_scope(&self) -> Result<&ScopeId, ResolveError> {
        self.stack.last().ok_or(ResolveError::EmptyStack)
    }

    fn current_level(&self) -> Result<&StackLvl, ResolveError> {
        self.stack.last()
            .and_then(|idx| self.memory.get(idx))
            .ok_or(ResolveError::EmptyStack)
    }

    fn current_level_mut(&mut self) -> Result<&mut StackLvl, ResolveError> {
        self.stack.last()
            .and_then(|idx| self.memory.get_mut(idx))
            .ok_or(ResolveError::EmptyStack)
    }

    /// Iterates top to bottom through the resolver stack until a module boundary is crossed, or the
    /// bottom of the stack is reached.
    fn rev_iter<F, A, R>(&self, f: F, a: A) -> Option<R>
    where
        F: Fn(&StackLvl) -> Option<R>,
        A: Fn() -> Option<R>, {

        for lvl in self.stack.iter().rev()
            .filter_map(|idx| self.memory.get(idx)) {

            let val = f(lvl);
            if val.is_some() {
                return val;
            }

            // break if a module boundary is crossed, as the local namespace is only available
            // within the scope of a single module.
            if lvl.is_module {
                break;
            }
        }
        a()
    }

    // fn rev_iter_mut<F, A, R>(&mut self, f: F, a: A) -> Option<R>
    // where
    //     F: Fn(&mut StackLvl) -> Option<R>,
    //     A: Fn() -> Option<R>, {
    //
    //     for lvl in self.stack.iter_mut().rev()
    //         .filter_map(|idx| self.memory.get_mut(idx)) {
    //
    //         let val = f(lvl);
    //         if val.is_some() {
    //             return val;
    //         }
    //
    //         if lvl.is_module {
    //             break;
    //         }
    //     }
    //     a()
    // }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn clear(&mut self) {
        self.stack.clear();
    }
}



pub struct TopLevelNameResolver {
    stack: Stack,
    namespace: Namespace<Item>,
    targets: Vec<String>,
}

impl Default for TopLevelNameResolver {
    fn default() -> Self {
        TopLevelNameResolver {
            stack: Stack::new(),
            namespace: Namespace::new("".to_string()),
            targets: Vec::new(),
        }
    }
}

pub enum ItemSrc {
    File(String, SrcPos),
    Intrinsic(String),
}

impl TopLevelNameResolver {
    pub fn add_target(&mut self, target: String) {
        self.targets.push(target);
    }

    pub fn contains_target(&self, target: &String) -> bool {
        self.targets.contains(target)
    }

    pub fn current_scope(&self) -> Result<&ScopeId, ResolveError> {
        self.stack.current_scope()
    }

    pub fn revert_to_scope(&mut self, scope: &ScopeId) {
        self.stack.revert_to_scope(scope);
    }

    /// Take one step back in the local resolver stack.
    pub fn pop(&mut self) {
        self.stack.pop();
    }

    /// Pops all items that are currently in the stack.
    pub fn clear_stack(&mut self) {
        self.stack.clear();
    }

    /// Changes the current scope of the resolver to the specified path.
    /// If the path does not exist in the resolver, an error is returned.
    pub fn scope_to_path(&mut self, path: &QualifierName) -> Result<(), ResolveError> {
        let item = self.find_item(path)
            .ok_or(ResolveError::InvalidUse(path.clone()))?;
        let scope = item.scope;
        self.revert_to_scope(&scope);
        Ok(())
    }

    pub fn push_module(&mut self, name: String) {
        self.push(Some(name), true);
    }

    pub fn push_type(&mut self, name: String) {
        self.push(Some(name), false);
    }

    pub fn push_fn(&mut self, name: String) {
        self.push(Some(name), false);
    }

    pub fn push_block(&mut self) {
        self.push(None, false);
    }

    /// Pushes a new stack level to the resolver.
    /// Each stack level contains its own namepsace.
    /// Thus, a stack level in this context is a namespace in the code.
    /// This can be any named item, except for functions:
    ///
    /// - modules,
    /// - traits
    /// - types (structs, enums, type alias)
    fn push(
        &mut self,
        namespace_name: Option<String>,
        is_module: bool,
    ) {
        // find new id
        let mut id = ScopeId::rand();
        while self.stack.memory.contains_key(&id) {
            id = ScopeId::rand();
        }
        // create new namespace name by appending the name of the namespace to the name of the
        // last namespace
        let mut name = if !self.stack.stack.is_empty() {
            if let Some(top) = self.stack.stack.last()
                .and_then(|id| self.stack.memory.get(id)) {
                top.imports.base_namespace.clone()
            } else {
                QualifierName::empty()
            }
        } else {
            QualifierName::empty()
        };
        // append namespace name to the base name
        if let Some(namespace_name) = namespace_name {
            name.push(namespace_name);
        }

        // create namespace
        if is_module {
            self.namespace.insert_namespace(&name);
        }

        // push new resolve level
        self.stack.stack.push(id);
        self.stack.memory.insert(
            id,
            StackLvl::new(id, name, self.stack.stack.clone(), is_module)
        );
    }

    /// Searches the levels of the resolver stack top to bottom and tries to resolve the item.
    pub fn find_item(&self, name: &QualifierName) -> Option<&Item> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;
        stack.rev_iter(move |lvl| lvl.find_item(name, namespace), || None)
    }

    pub fn find_associated_item(&self, id: &EdlTypeInstance, name: &QualifierName) -> Option<&Item> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;
        stack.rev_iter(move |lvl| lvl.find_associated_item(id, name, namespace), || None)
    }

    pub fn find_associated_trait_item(&self, id: &EdlTraitInstance, name: &QualifierName) -> Option<&Item> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;
        stack.rev_iter(move |lvl| lvl.find_associated_trait_item(id, name, namespace), || None)
    }

    pub fn find_associated_type(&self, id: &EdlTypeInstance, name: &QualifierName, type_reg: &EdlTypeRegistry) -> Option<EdlTypeInstance> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;
        stack.rev_iter(move |lvl| lvl.find_associated_type(id, name, namespace, type_reg), || None)
    }

    pub fn find_associated_alias(&self, id: &EdlTypeInstance, name: &QualifierName) -> Option<EdlAliasId> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;
        stack.rev_iter(move |lvl| lvl.find_associated_alias(id, name, namespace), || None)
    }

    pub fn find_associated_const(&self, id: &EdlTypeInstance, name: &QualifierName) -> Option<EdlConstValue> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;
        stack.rev_iter(move |lvl| lvl.imports.resolve_associated_const(id, name, namespace), || None)
    }

    pub fn find_trait_associated_type(&self, id: &EdlTraitInstance, name: &QualifierName, type_reg: &EdlTypeRegistry) -> Option<EdlTypeInstance> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;
        stack.rev_iter(move |lvl| lvl.find_trait_associated_type(id, name, namespace, type_reg), || None)
    }

    pub fn find_trait_associated_alias(&self, id: &EdlTraitInstance, name: &QualifierName) -> Option<EdlAliasId> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;
        stack.rev_iter(move |lvl| lvl.find_trait_associated_alias(id, name, namespace), || None)
    }

    pub fn find_trait_associated_const(&self, id: &EdlTraitInstance, name: &QualifierName) -> Option<EdlConstValue> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;
        stack.rev_iter(move |lvl| lvl.imports.resolve_trait_associated_const(id, name, namespace), || None)
    }

    pub fn find_self_type(&self) -> SelfType {
        let TopLevelNameResolver {
            stack,
            ..
        } = self;
        if let Some(s) = stack.rev_iter(move |lvl| match lvl.find_self_type() {
            SelfType::None => None,
            s => Some(s),
        }, || None) {
            s
        } else {
            SelfType::None
        }
    }

    pub fn find_local_item(&self, name: &QualifierName) -> Option<LocalItem> {
        let TopLevelNameResolver {
            stack,
            ..
        } = self;

        let last = name.last()?;
        stack.rev_iter(move |lvl| {
            lvl.local_item(last).cloned()
        }, || None)
    }

    /// Returns the scope id of a specified module, if it exists.
    pub fn find_module_scope(&self, name: &QualifierName) -> Option<ScopeId> {
        for (scope, module) in self.stack.memory.iter() {
            if module.is_module && &module.imports.base_namespace == name {
                return Some(*scope);
            }
        }
        None
    }

    /// Iterates through all items of type alias and tries to replace the alias with the item the
    /// alias is in reference to.
    ///
    pub fn try_resolve_alias(&self) {
        todo!()
    }

    pub fn has_namespace(&self, name: &QualifierName) -> bool {
        self.namespace.is_namespace(name)
    }

    pub fn has_item(&self, name: &QualifierName) -> bool {
        self.namespace.is_item(name)
    }

    pub fn find_top_level_type(&self, name: &QualifierName, edl_type_registry: &EdlTypeRegistry) -> Option<EdlTypeId> {
        // look for local types
        if let Some(LocalItem::GenericType(ty)) = self.find_local_item(name) {
            return Some(ty.ty);
        }
        // go through levels from top to bottom and try to resolve the reference to the item
        if let Some(Item { variant: ItemVariant::Type(type_id), .. }) = self.find_item(name) {
            if let Some(EdlType::Type { .. } | EdlType::Generic { .. }) = edl_type_registry.get_type(*type_id) {
                return Some(*type_id);
            }
        }
        None
    }

    pub fn find_top_level_alias(&self, name: &QualifierName) -> Option<EdlAliasId> {
        // go through levels from top to bottom and try to resolve the reference to the item
        if let Some(Item { variant: ItemVariant::Alias(alias), .. }) = self.find_item(name) {
            Some(*alias)
        } else {
            None
        }
    }

    pub fn find_top_level_type_scope(&self, name: &QualifierName) -> Option<ScopeId> {
        if let Some(LocalItem::GenericType(_ty)) = self.find_local_item(name) {
            return Some(*self.current_scope().unwrap());
        };
        if let Some(Item { variant: ItemVariant::Type(_), scope }) = self.find_item(name) {
            return Some(*scope);
        }
        None
    }

    pub fn find_top_level_function(&self, name: &QualifierName, edl_type_registry: &EdlTypeRegistry) -> Option<EdlTypeId> {
        // go through levels from top to bottom and try to resolve the reference to the item
        if let Some(Item { variant: ItemVariant::Fn(type_id), .. }) = self.find_item(name) {
            if let Some(EdlType::Function { .. }) = edl_type_registry.get_type(*type_id) {
                Some(*type_id)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn find_top_level_type_or_function(&self, name: &QualifierName, edl_type_registry: &EdlTypeRegistry) -> Option<EdlTypeId> {
        if let Some(LocalItem::GenericType(ty)) = self.find_local_item(name) {
            return Some(ty.ty);
        }
        // go through levels from top to bottom and try to resolve the reference to the item
        if let Some(Item { variant: ItemVariant::Type(type_id) | ItemVariant::Fn(type_id), .. }) = self.find_item(name) {
            if let Some(EdlType::Function { .. } | EdlType::Type { .. } | EdlType::Generic { .. }) = edl_type_registry.get_type(*type_id) {
                return Some(*type_id);
            }
        }
        None
    }

    pub fn find_top_level_env(&self, name: &QualifierName, edl_type_registry: &EdlTypeRegistry) -> Option<EdlEnvId> {
        if let Some(Item { variant: ItemVariant::Type(type_id) | ItemVariant::Fn(type_id), .. }) = self.find_item(name) {
            if let Some(EdlType::Function {
                            state: FunctionState::Init { sig: EdlFnSignature { env: param, .. }, .. }
                                 | FunctionState::Pre { sig: EdlPreSignature { env: param, .. }, .. },
                            ..
                        } | EdlType::Type { param, .. }) = edl_type_registry.get_type(*type_id) {
                Some(*param)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn find_top_level_generic(&self, name: &QualifierName, edl_type_registry: &EdlTypeRegistry) -> Option<EdlTypeId> {
        if let Some(LocalItem::GenericType(ty)) = self.find_local_item(name) {
            return Some(ty.ty);
        }
        if let Some(Item { variant: ItemVariant::Type(type_id), .. }) = self.find_item(name) {
            if let Some(EdlType::Generic { .. }) = edl_type_registry.get_type(*type_id) {
                return Some(*type_id)
            }
        }
        None
    }

    pub fn find_top_level_const(&self, name: &QualifierName) -> Option<EdlConstValue> {
        if let Some(LocalItem::GenericConst(param, index)) = self.find_local_item(name) {
            return Some(EdlConstValue::GenericConst {param, index})
        }
        if let Some(Item { variant: ItemVariant::Const(id, ..), .. }) = self.find_item(name) {
            return Some(EdlConstValue::Const(*id));
        }
        None
    }

    /// Tries to find a variable in the local context first, and, if it cannot be found in the
    /// local context, the global context is searched.
    pub fn find_top_level_var(&self, name: &QualifierName) -> Option<EdlVarId> {
        if let Some(LocalItem::Var(id)) = self.find_local_item(name) {
            return Some(id);
        }
        if let Some(Item { variant: ItemVariant::Var(id), .. }) = self.find_item(name) {
            return Some(*id)
        }
        None
    }

    pub fn find_top_level_trait(&self, name: &QualifierName) -> Option<EdlTraitId> {
        if let Some(Item { variant: ItemVariant::Trait(id), .. }) = self.find_item(name) {
            Some(*id)
        } else {
            None
        }
    }

    pub fn find_top_level_enum_variant(&self, name: &QualifierName) -> Option<(EdlTypeId, &String)> {
        if let Some(Item { variant: ItemVariant::EnumVariant(ty, name), .. }) = self.find_item(name) {
            Some((*ty, name))
        } else {
            None
        }
    }

    pub fn current_level_name(&self) -> Result<QualifierName, ResolveError> {
        let level = self.stack.current_level()?;
        let full_name = level.imports.base_namespace.clone();
        Ok(full_name)
    }

    /// Pushes top-level items to the namespace hierarchy of the namespace resolver.
    /// This includes:
    ///
    /// - Types,
    /// - top-level Functions,
    /// - top-level Constants
    pub fn push_top_level_item(
        &mut self,
        name: String,
        _src: ItemSrc,
        variant: ItemInit,
        type_registry: &mut EdlTypeRegistry,
    ) -> Result<ScopeId, ResolveError> {
        // check if the item is already available in the scope
        // if self.find_item(&name.clone().into()).is_some() {
        //     return Err(ResolveError::ItemRedefinition(name.into()));
        // }

        // form fully qualified name
        let level = self.stack.current_level()?;
        let mut full_name = level.imports.base_namespace.clone();
        full_name.push(name.clone());

        // try to find item in scope
        if let Some(item) = self.find_item(&full_name) {
            debug!("Item {full_name} is already part of the name resolver. Checking if the item can be updated...");
            let Item { scope, variant: ItemVariant::Fn(ty_id) } = item else {
                return Err(ResolveError::ItemRedefinition(name.into()));
            };

            let ItemInit::Function { sig, .. } = variant else {
                panic!("Expected item of kind `ItemInit::Function` to finalize function definition!");
            };

            type_registry.finalize_function(*ty_id, sig, EdlFunctionBody::Intrinsic()).unwrap();
            debug!("Function with name {full_name} has been finalized successfully!");
            return Ok(*scope)
        }

        // debug
        debug!("Pushing item {} to resolver", full_name);

        let current_scope = *self.current_scope()?;
        // push now stack entry for that item onto the stack
        match &variant {
            ItemInit::Type { .. } => self.push_type(name),
            ItemInit::Alias(_, _, _) => self.push_type(name),
            ItemInit::Function { scope, .. } => self.revert_to_scope(scope),
            ItemInit::Const { .. } => self.push_fn(name), // treat constants as comp-time functions for now
            ItemInit::Let { .. } => self.push_fn(name),
            ItemInit::Trait { .. } => self.push_type(name),
            ItemInit::PreFunction { .. } => self.push_fn(name),
            ItemInit::EnumVariant { .. } => self.push_type(name),
        }
        let scope_id = *self.stack.current_scope()?;
        self.revert_to_scope(&current_scope);

        // insert item into the namespace hierarchy
        let variant = match variant {
            ItemInit::Type { params } => {
                let env_id = type_registry.insert_parameter_env(params);
                let id = type_registry.insert_type(EdlType::Type { param: env_id, name: full_name.clone(), state: EdlTypeState::default() });
                ItemVariant::Type(id)
            },
            ItemInit::EnumVariant { id, name } => {
                ItemVariant::EnumVariant(id, name)
            },
            ItemInit::Alias(env_id, src, pos) => {
                let id = type_registry.insert_alias_type(env_id, full_name.clone(), src, pos);
                ItemVariant::Alias(id)
            },
            ItemInit::Function { sig, .. } => {
                let id = type_registry.insert_type(EdlType::Function { state: FunctionState::Init { sig, body: EdlFunctionBody::Intrinsic() }, name: Some(full_name.clone()) });
                ItemVariant::Fn(id)
            },
            ItemInit::Const { ty: _, id } => {
                ItemVariant::Const(id)
            },
            ItemInit::Let { id } => {
                ItemVariant::Var(id)
            },
            ItemInit::Trait { id } => {
                ItemVariant::Trait(id)
            },
            ItemInit::PreFunction { sig, .. } => {
                let id = type_registry.insert_type(EdlType::Function { state: FunctionState::Pre { sig }, name: Some(full_name.clone()) });
                ItemVariant::Fn(id)
            }
        };
        self.namespace.insert_item(&full_name, Item {
            scope: scope_id,
            variant,
        });
        Ok(scope_id)
    }

    pub fn push_generic_type(
        &mut self,
        name: String,
        env: EdlEnvId,
        index: usize,
        type_registry: &mut EdlTypeRegistry,
    ) -> Result<(), ResolveError> {
        // form fully qualified name
        let level = self.stack.current_level_mut()?;
        let ty = type_registry.generic(env, index);
        level.insert_generic_type(name, ty);
        Ok(())
    }

    pub fn push_generic_const(
        &mut self,
        name: String,
        env: EdlEnvId,
        index: usize,
    ) -> Result<(), ResolveError> {
        // form fully qualified name
        let level = self.stack.current_level_mut()?;
        level.insert_generic_const(name, env, index);
        Ok(())
    }

    pub fn push_local_var(
        &mut self,
        name: String,
        id: EdlVarId,
    ) -> Result<(), ResolveError> {
        let level = self.stack.current_level_mut()?;
        level.insert_var(name, id)
    }

    pub fn push_self_type(
        &mut self,
        ty: EdlTypeInstance,
    ) -> Result<(), ResolveError> {
        let level = self.stack.current_level_mut()?;
        level.imports.insert_self_type(ty);
        Ok(())
    }

    pub fn push_self_trait(
        &mut self,
        ty: EdlTraitInstance,
    ) -> Result<(), ResolveError> {
        let level = self.stack.current_level_mut()?;
        level.imports.insert_self_trait(ty);
        Ok(())
    }

    /// Associates a namespace base name to a type instance.
    /// The base name `name` must be relative to the **absolute base** of the compiled sources.
    pub fn push_association(
        &mut self,
        ty: EdlTypeInstance,
        name: QualifierName,
    ) -> Result<(), ResolveError> {
        let level = self.stack.current_level_mut()?;
        level.imports.insert_association(ty, name);
        Ok(())
    }

    pub fn push_trait_association(
        &mut self,
        ty: EdlTraitInstance,
        name: QualifierName,
    ) -> Result<(), ResolveError> {
        let level = self.stack.current_level_mut()?;
        level.imports.insert_trait_association(ty, name);
        Ok(())
    }

    pub fn push_associated_type(
        &mut self,
        ty: &EdlTypeInstance,
        name: String,
        associated: EdlTypeInstance,
    ) -> Result<(), ResolveError> {
        let level = self.stack.current_level_mut()?;
        level.imports.insert_associated_type(ty, name, associated)
    }

    pub fn push_associated_alias(
        &mut self,
        ty: &EdlTypeInstance,
        name: String,
        associated: EdlAliasId,
    ) -> Result<(), ResolveError> {
        let level = self.stack.current_level_mut()?;
        level.imports.insert_associated_alias(ty, name, associated)
    }

    pub fn push_trait_associated_type(
        &mut self,
        ty: &EdlTraitInstance,
        name: String,
        associated: EdlTypeInstance,
    ) -> Result<(), ResolveError> {
        let level = self.stack.current_level_mut()?;
        level.imports.insert_trait_associated_type(ty, name, associated)
    }

    pub fn push_trait_associated_alias(
        &mut self,
        ty: &EdlTraitInstance,
        name: String,
        associated: EdlAliasId,
    ) -> Result<(), ResolveError> {
        let level = self.stack.current_level_mut()?;
        level.imports.insert_trait_associated_alias(ty, name, associated)
    }

    pub fn push_associated_const(
        &mut self,
        ty: &EdlTypeInstance,
        name: String,
        associated: EdlConstId,
    ) -> Result<(), ResolveError> {
        let level = self.stack.current_level_mut()?;
        level.imports.insert_associated_const(ty, name, associated)
    }

    pub fn push_trait_associated_const(
        &mut self,
        ty: &EdlTraitInstance,
        name: String,
        associated: EdlConstId,
    ) -> Result<(), ResolveError> {
        let level = self.stack.current_level_mut()?;
        level.imports.insert_trait_associated_const(ty, name, associated)
    }

    pub fn push_env(
        &mut self,
        env_id: EdlEnvId,
        type_registry: &mut EdlTypeRegistry,
    ) -> Result<(), EdlError> {
        let env = type_registry.get_env(env_id)
            .ok_or(EdlError::E012(env_id))?.clone();
        env.push_to_resolver(env_id, self, type_registry)
            .map_err(EdlError::E048)
    }

    /// Adds a `use` statement to the current stack
    pub fn push_use(&mut self, name: QualifierName) -> Result<(), ResolveError> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;

        let lvl = stack.current_level_mut()?;
        lvl.imports.insert_namespace(namespace, name)
    }

    pub fn push_raw_type(
        &mut self,
        name: String,
        id: EdlTypeId,
    ) -> Result<ScopeId, ResolveError> {
        if self.find_item(&name.clone().into()).is_some() {
            return Err(ResolveError::ItemRedefinition(name.into()));
        }

        // form fully qualified name
        let level = self.stack.current_level()?;
        let mut full_name = level.imports.base_namespace.clone();
        full_name.push(name.clone());

        debug!("Pushing raw type {} to resolver", full_name);

        self.push_fn(name);
        let scope_id = *self.stack.current_scope()?;
        self.pop();

        let item = ItemVariant::Type(id);
        self.namespace.insert_item(&full_name, Item {
            scope: scope_id,
            variant: item,
        });
        Ok(scope_id)
    }

    /// Returns the name of the current scope.
    ///
    /// # Scope Identification
    ///
    /// Please keep in mind that _not every scope is named_.
    /// Thus, the name of a scope is not a valid unique qualifier to find that same scope in the
    /// stack.
    /// Identification of scopes should *only* be done through the [ScopeId] type.
    pub fn get_current_scope_name(&self) -> Result<QualifierName, ResolveError> {
        let level = self.stack.current_level()?;
        Ok(level.imports.base_namespace.clone())
    }

    /// Returns the name of the specified scope
    ///
    /// # Scope Identification
    ///
    /// Please keep in mind that _not every scope is named_.
    /// Thus, the name of a scope is not a valid unique qualifier to find that same scope in the
    /// stack.
    /// Identification of scopes should *only* be done through the [ScopeId] type.
    pub fn get_scope_name(&self, scope_id: &ScopeId) -> Result<QualifierName, ResolveError> {
        let stack_lvl = self.stack.memory.get(scope_id).unwrap();
        Ok(stack_lvl.imports.base_namespace.clone())
    }
}

impl StackLvl {
    pub fn new(
        id: ScopeId,
        base_module: QualifierName,
        stack: Vec<ScopeId>,
        is_module: bool,
    ) -> Self {
        StackLvl {
            _id: id,
            imports: Imports::new(base_module),
            stack,
            is_module,
            _items: Namespace::new("".to_string()),
            local_items: HashMap::default(),
        }
    }

    fn find_local_item<'a>(&self, name: &QualifierName, namespace: &'a Namespace<Item>) -> Option<&'a Item> {
        self.imports.resolve_local(name, namespace)
    }

    fn find_item<'a>(&self, name: &QualifierName, namespace: &'a Namespace<Item>) -> Option<&'a Item> {
        self.imports.resolve_type(name, namespace)
    }

    fn find_associated_item<'a>(&self, ty: &EdlTypeInstance, name: &QualifierName, namespace: &'a Namespace<Item>) -> Option<&'a Item> {
        self.imports.resolve_association(ty, name, namespace)
    }

    fn find_associated_trait_item<'a>(&self, ty: &EdlTraitInstance, name: &QualifierName, namespace: &'a Namespace<Item>) -> Option<&'a Item> {
        self.imports.resolve_trait_association(ty, name, namespace)
    }

    fn find_associated_type(&self, ty: &EdlTypeInstance, name: &QualifierName, namespace: &Namespace<Item>, type_reg: &EdlTypeRegistry) -> Option<EdlTypeInstance> {
        self.imports.resolve_associated_type(ty, name, namespace, type_reg)
    }

    fn find_associated_alias(&self, ty: &EdlTypeInstance, name: &QualifierName, namespace: &Namespace<Item>) -> Option<EdlAliasId> {
        self.imports.resolve_associated_alias(ty, name, namespace)
    }

    fn find_trait_associated_type(&self, ty: &EdlTraitInstance, name: &QualifierName, namespace: &Namespace<Item>, type_reg: &EdlTypeRegistry) -> Option<EdlTypeInstance> {
        self.imports.resolve_trait_associated_type(ty, name, namespace, type_reg)
    }

    fn find_trait_associated_alias(&self, ty: &EdlTraitInstance, name: &QualifierName, namespace: &Namespace<Item>) -> Option<EdlAliasId> {
        self.imports.resolve_trait_associated_alias(ty, name, namespace)
    }

    fn find_self_type(&self) -> SelfType {
        self.imports.self_type.clone()
    }


    fn local_item(&self, name: &String) -> Option<&LocalItem> {
        self.local_items.get(name)
    }

    /// Inserts a variable into the local stack frame.
    /// If a variable with the same name already exists, the variable is replaced with the new
    /// value.
    /// If another local item exists with the same name (for example, a generic type or a generic
    /// constant) an error is returned.
    fn insert_var(&mut self, name: String, var: EdlVarId) -> Result<(), ResolveError> {
        match self.local_items.get_mut(&name) {
            Some(LocalItem::Var(id)) => {
                *id = var;
                Ok(())
            },
            Some(_) => {
                Err(ResolveError::ItemRedefinition(name.into()))
            },
            None => {
                self.local_items.insert(name, LocalItem::Var(var));
                Ok(())
            }
        }
    }

    fn insert_generic_const(&mut self, name: String, param: EdlEnvId, index: usize) {
        self.local_items.insert(
            name,
            LocalItem::GenericConst(param, index)
        );
    }

    fn insert_generic_type(&mut self, name: String, ty: EdlTypeInstance) {
        self.local_items.insert(
            name,
            LocalItem::GenericType(ty),
        );
    }
}



#[cfg(test)]
mod test {
    use crate::core::edl_fn::{EdlFnRet, EdlFnSignature};
    use crate::core::edl_param_env::EdlParameterEnv;
    use crate::core::edl_type::{EdlMaybeType, EdlTypeRegistry};
    use crate::resolver::{ItemSrc, ItemInit, ResolveError, TopLevelNameResolver};

    #[test]
    fn test() -> Result<(), ResolveError> {
        let mut reg = EdlTypeRegistry::default();
        let mut resolver = TopLevelNameResolver::default();
        resolver.push_module("std".to_string());

        let f32_scope = resolver.push_top_level_item(
            "f32".to_string(),
            ItemSrc::Intrinsic("@ty core::f32".to_string()),
            ItemInit::Type { params: EdlParameterEnv::default() },
            &mut reg
        )?;

        let type_id = resolver.find_top_level_type(
            &vec![
                "f32".to_string(),
            ].into(),
            &reg
        );
        let id = type_id.unwrap();
        resolver.pop();

        resolver.push_module("test".to_string());
        let mut type_id = resolver.find_top_level_type(
            &vec![
                "std".to_string(),
                "f32".to_string(),
            ].into(),
            &reg,
        );
        assert_eq!(type_id, Some(id));

        type_id = resolver.find_top_level_type(
            &vec![
                "f32".to_string(),
            ].into(),
            &reg,
        );
        assert_eq!(type_id, None);

        resolver.push_use(vec![
            "std".to_string()
        ].into())?;

        type_id = resolver.find_top_level_type(
            &vec![
                "std".to_string(),
                "f32".to_string(),
            ].into(),
            &reg,
        );
        assert_eq!(type_id, Some(id));

        type_id = resolver.find_top_level_type(
            &vec![
                "f32".to_string(),
            ].into(),
            &reg,
        );
        assert_eq!(type_id, None);

        resolver.push_use(vec![
            "std".to_string(),
            "f32".to_string(),
        ].into())?;

        type_id = resolver.find_top_level_type(
            &vec![
                "f32".to_string(),
            ].into(),
            &reg,
        );
        assert_eq!(type_id, Some(id));

        let test_scope = *resolver.current_scope()?;
        resolver.pop();

        // get back into the `std::f32` type scope and add a function to the type
        resolver.revert_to_scope(&f32_scope);
        resolver.push_fn("from".to_string());
        
        resolver.push_top_level_item(
            "from".to_string(),
            ItemSrc::Intrinsic("@fn std::f32::from".to_string()),
            ItemInit::Function {
                sig: EdlFnSignature {
                    name: "from".to_string(),
                    env: reg.new_env(),
                    scope: *resolver.current_scope().unwrap(),
                    comptime: false,
                    comptime_only: false,
                    ret: reg.empty(),
                    params: vec![],
                },
                scope: *resolver.current_scope().unwrap()
            },
            &mut reg,
        )?;
        type_id = resolver.find_top_level_function(
            &vec!["from".to_string()].into(),
            &reg,
        );
        assert!(type_id.is_some());
        let from_id = type_id.unwrap();
        resolver.pop();

        // go back into the test scope and try to find the `from` function that was just pushed
        resolver.revert_to_scope(&test_scope);
        type_id = resolver.find_top_level_function(
            &vec![
                "f32".to_string(),
                "from".to_string(),
            ].into(),
            &reg,
        );
        assert_eq!(type_id, Some(from_id));

        type_id = resolver.find_top_level_function(
            &vec![
                "from".to_string(),
            ].into(),
            &reg,
        );
        assert_eq!(type_id, None);

        resolver.push_use(vec!["std".to_string(), "f32".to_string(), "from".to_string()].into())?;
        type_id = resolver.find_top_level_function(
            &vec![
                "from".to_string(),
            ].into(),
            &reg,
        );
        assert_eq!(type_id, Some(from_id));

        Ok(())
    }
}
