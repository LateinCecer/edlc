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
//! Leptos server functions bridging the client to `DocDb`.
//!
//! These functions are called from the client (via Leptos server function machinery) and
//! executed on the server, where they have access to the `DocDb` provided as context.

use std::sync::{Arc, Mutex};

use edlc_doc_db::{DocDb, DocRow, Kind};
use serde::{Deserialize, Serialize};

/// A documentation item summary, serializable for both SSR and client.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DocSummary {
    pub id: i64,
    pub kind: String,
    pub name: String,
    pub qual_name: String,
    pub module: Option<String>,
    pub signature: String,
    pub doc_text: String,
    pub blob: String,
}

impl From<DocRow> for DocSummary {
    fn from(row: DocRow) -> Self {
        DocSummary {
            id: row.id,
            kind: row.kind.as_str().to_string(),
            name: row.name,
            qual_name: row.qual_name,
            module: row.module,
            signature: row.signature,
            doc_text: row.doc_text,
            blob: row.blob,
        }
    }
}

/// Server context key: the shared database handle.
#[cfg(feature = "ssr")]
pub type DbHandle = Arc<Mutex<DocDb>>;

/// Search documentation items by full-text query.
#[cfg(feature = "ssr")]
pub async fn search_docs(query: String, limit: usize) -> Result<Vec<DocSummary>, String> {
    let db = leptos::prelude::use_context::<DbHandle>()
        .ok_or("database not available in server context")?;
    let rows = tokio::task::spawn_blocking(move || {
        let guard = db.lock().map_err(|e| e.to_string())?;
        guard.search(&query, limit).map_err(|e| e.to_string())
    })
    .await
    .map_err(|e| e.to_string())??;
    Ok(rows.into_iter().map(Into::into).collect())
}

/// Fetch a single documentation item by its name (simple or qualified).
#[cfg(feature = "ssr")]
pub async fn get_doc(name: String) -> Result<Option<DocSummary>, String> {
    let db = leptos::prelude::use_context::<DbHandle>()
        .ok_or("database not available in server context")?;
    let rows = tokio::task::spawn_blocking(move || {
        let guard = db.lock().map_err(|e| e.to_string())?;
        guard.list_items(None).map_err(|e| e.to_string())
    })
    .await
    .map_err(|e| e.to_string())??;
    Ok(rows
        .into_iter()
        .find(|row| row.qual_name == name || row.name == name)
        .map(Into::into))
}

/// List all modules in the documentation database.
#[cfg(feature = "ssr")]
pub async fn list_modules() -> Result<Vec<DocSummary>, String> {
    let db = leptos::prelude::use_context::<DbHandle>()
        .ok_or("database not available in server context")?;
    let rows = tokio::task::spawn_blocking(move || {
        let guard = db.lock().map_err(|e| e.to_string())?;
        guard.modules().map_err(|e| e.to_string())
    })
    .await
    .map_err(|e| e.to_string())??;
    Ok(rows.into_iter().map(Into::into).collect())
}

/// List all items belonging to a specific module.
#[cfg(feature = "ssr")]
pub async fn get_module_items(name: String) -> Result<Vec<DocSummary>, String> {
    let db = leptos::prelude::use_context::<DbHandle>()
        .ok_or("database not available in server context")?;
    let rows = tokio::task::spawn_blocking(move || {
        let guard = db.lock().map_err(|e| e.to_string())?;
        guard.list_items(None).map_err(|e| e.to_string())
    })
    .await
    .map_err(|e| e.to_string())??;
    Ok(rows
        .into_iter()
        .filter(|row| {
            row.module.as_deref() == Some(name.as_str())
                || row.qual_name.starts_with(&format!("{}::", name))
        })
        .map(Into::into)
        .collect())
}
