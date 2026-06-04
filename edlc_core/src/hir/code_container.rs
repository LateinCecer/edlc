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
use crate::ast::ItemDoc;
use crate::documentation::{DocGenerator, Item, ModuleDoc};
use crate::resolver::QualifierName;



#[derive(Clone, Debug, PartialEq)]
struct ItemInfo {
    item: Item,
    full_name: QualifierName,
}

#[derive(Clone, Debug, PartialEq)]
struct ModuleInfo {
    doc: Option<ItemDoc>,
}

/// A [CodeContainer] can be used to store HIR level items for future reference.
/// This future reference may include incremental compilation stages, documentation passes, etc.
#[derive(Default)]
pub struct CodeContainer {
    items: Vec<Item>,
}


impl CodeContainer {
    /// Uses the [crate::documentation::DocGenerator] API to generate documentation from the code
    /// containers contents.
    /// Please note that everytime this function is called, the entirety of the containers'
    /// content is spilled to the generator.
    pub fn generate_docs<G: DocGenerator>(&self, generator: &mut G) -> Result<(), G::Error> {
        self.items.iter()
            .try_for_each(|info| generator.insert_definition(info))
    }

    /// Inserts a documentation item into the code container.
    pub fn insert_doc(&mut self, val: impl Into<Item>) {
        self.items.push(val.into());
    }

    // Inserts a module.
    // If the module already exists, any provided module documentation is appended to the existing
    // module documentation.
    pub fn insert_module(&mut self, full_name: &QualifierName, docs: &Option<ItemDoc>) {
        let item = self.items.iter_mut()
            .find(|item|
                matches!(item, Item::Module(ModuleDoc { name, .. }) if name == full_name));

        if let Some(item) = item {
            let Item::Module(item) = item else {
                unreachable!();
            };
            // append documentations
            item.doc.push_str(docs.as_ref().map(|doc| doc.doc.as_str())
                .unwrap_or(""));
        } else {
            self.items.push(ModuleDoc {
                name: full_name.clone(),
                doc: docs.as_ref().map(|doc| doc.doc.clone()).unwrap_or_default()
            }.into());
        }
    }
}
