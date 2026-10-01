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

#[cfg(feature = "ssr")]
use std::sync::{Arc, Mutex};

#[cfg(feature = "ssr")]
use edlc_doc_db::{DocDb, DocRow};
use leptos::prelude::*;
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
    /// Whether a rendered (typeset) HTML form of the doc comment exists for this
    /// item. The HTML itself never crosses the wire; it is served separately at
    /// `/doc-html/<name>` for the item page's `<iframe>`.
    pub has_doc_html: bool,
}

/// Error type returned by the documentation server functions. Serializable to both builds
/// (it crosses the server->client wire and the hydration context).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum DocError {
    /// The documentation database could not be queried.
    Db { message: String },
    /// The requested item does not exist in the database.
    ItemNotFound { name: String },
    /// The requested module does not exist in the database.
    ModuleNotFound { name: String },
    /// The server function could not be reached or its request/response could not be
    /// (de)serialized (client-side transport failures).
    Request { message: String },
    /// An unexpected internal error.
    Internal { message: String },
}

impl std::fmt::Display for DocError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DocError::Db { message } => write!(f, "Documentation database error: {message}"),
            DocError::ItemNotFound { name } => {
                write!(f, "The documentation item '{name}' was not found.")
            }
            DocError::ModuleNotFound { name } => {
                write!(f, "The module '{name}' was not found.")
            }
            DocError::Request { message } => {
                write!(f, "Could not reach the documentation server: {message}")
            }
            DocError::Internal { message } => {
                write!(f, "An unexpected error occurred: {message}")
            }
        }
    }
}

impl FromServerFnError for DocError {
    type Encoder = leptos::server_fn::codec::JsonEncoding;

    fn from_server_fn_error(value: ServerFnErrorErr) -> Self {
        match value {
            ServerFnErrorErr::Request(message)
            | ServerFnErrorErr::UnsupportedRequestMethod(message)
            | ServerFnErrorErr::Serialization(message)
            | ServerFnErrorErr::Deserialization(message)
            | ServerFnErrorErr::Args(message)
            | ServerFnErrorErr::MissingArg(message) => DocError::Request { message },
            ServerFnErrorErr::Registration(message)
            | ServerFnErrorErr::ServerError(message)
            | ServerFnErrorErr::MiddlewareError(message)
            | ServerFnErrorErr::Response(message) => DocError::Internal { message },
        }
    }
}

#[cfg(feature = "ssr")]
impl From<rusqlite::Error> for DocError {
    fn from(e: rusqlite::Error) -> Self {
        DocError::Db {
            message: e.to_string(),
        }
    }
}

#[cfg(feature = "ssr")]
impl<T> From<std::sync::PoisonError<T>> for DocError {
    fn from(e: std::sync::PoisonError<T>) -> Self {
        DocError::Internal {
            message: format!("the documentation database lock was poisoned: {e}"),
        }
    }
}

#[cfg(feature = "ssr")]
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
            has_doc_html: !row.doc_html.is_empty(),
        }
    }
}

/// Server context key: the shared database handle.
#[cfg(feature = "ssr")]
pub type DbHandle = Arc<Mutex<DocDb>>;

/// Search documentation items by full-text query.
///
/// An empty query or a query with no matches is not an error: an empty list is returned.
#[server]
pub async fn search_docs(query: String, limit: usize) -> Result<Vec<DocSummary>, DocError> {
    let db = use_context::<DbHandle>().ok_or_else(|| DocError::Internal {
        message: "database not available in server context".into(),
    })?;
    let rows = tokio::task::spawn_blocking(move || -> Result<Vec<DocRow>, DocError> {
        let guard = db.lock()?;
        Ok(guard.search(&query, limit, None)?)
    })
    .await
    .map_err(|e| DocError::Internal {
        message: e.to_string(),
    })??;
    Ok(rows.into_iter().map(Into::into).collect())
}

/// Fetch a single documentation item by its name (simple or qualified).
///
/// Returns [`DocError::ItemNotFound`] when no item matches (including an empty name).
#[server]
pub async fn get_doc(name: String) -> Result<DocSummary, DocError> {
    let db = use_context::<DbHandle>().ok_or_else(|| DocError::Internal {
        message: "database not available in server context".into(),
    })?;
    let query = name.clone();
    let row = tokio::task::spawn_blocking(move || -> Result<Option<DocRow>, DocError> {
        let guard = db.lock()?;
        Ok(guard.get_item_by_name(&query)?)
    })
    .await
    .map_err(|e| DocError::Internal {
        message: e.to_string(),
    })??;
    row.map(Into::into)
        .ok_or_else(|| DocError::ItemNotFound { name })
}

/// List all modules in the documentation database.
#[server]
pub async fn list_modules() -> Result<Vec<DocSummary>, DocError> {
    let db = use_context::<DbHandle>().ok_or_else(|| DocError::Internal {
        message: "database not available in server context".into(),
    })?;
    let rows = tokio::task::spawn_blocking(move || -> Result<Vec<DocRow>, DocError> {
        let guard = db.lock()?;
        Ok(guard.modules()?)
    })
    .await
    .map_err(|e| DocError::Internal {
        message: e.to_string(),
    })??;
    Ok(rows.into_iter().map(Into::into).collect())
}

/// List all items belonging to a specific module.
///
/// Returns [`DocError::ModuleNotFound`] when the module does not exist. An existing module
/// without items is not an error: an empty list is returned.
#[server]
pub async fn get_module_items(name: String) -> Result<Vec<DocSummary>, DocError> {
    let db = use_context::<DbHandle>().ok_or_else(|| DocError::Internal {
        message: "database not available in server context".into(),
    })?;
    let query = name.clone();
    let rows = tokio::task::spawn_blocking(move || -> Result<Vec<DocRow>, DocError> {
        let guard = db.lock()?;
        let rows = guard.list_module_items(&query)?;
        if rows.is_empty() && !guard.modules()?.iter().any(|m| m.qual_name == query) {
            return Err(DocError::ModuleNotFound { name: query });
        }
        Ok(rows)
    })
    .await
    .map_err(|e| DocError::Internal {
        message: e.to_string(),
    })??;
    Ok(rows.into_iter().map(Into::into).collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_messages() {
        assert_eq!(
            DocError::Db {
                message: "boom".into()
            }
            .to_string(),
            "Documentation database error: boom"
        );
        assert_eq!(
            DocError::ItemNotFound {
                name: "example::foo".into()
            }
            .to_string(),
            "The documentation item 'example::foo' was not found."
        );
        assert_eq!(
            DocError::ModuleNotFound {
                name: "example".into()
            }
            .to_string(),
            "The module 'example' was not found."
        );
        assert_eq!(
            DocError::Request {
                message: "net down".into()
            }
            .to_string(),
            "Could not reach the documentation server: net down"
        );
        assert_eq!(
            DocError::Internal {
                message: "oops".into()
            }
            .to_string(),
            "An unexpected error occurred: oops"
        );
    }

    #[test]
    fn serde_round_trip() {
        let errors = [
            DocError::Db {
                message: "boom".into(),
            },
            DocError::ItemNotFound {
                name: "example::foo".into(),
            },
            DocError::ModuleNotFound {
                name: "example".into(),
            },
            DocError::Request {
                message: "net down".into(),
            },
            DocError::Internal {
                message: "oops".into(),
            },
        ];
        for err in &errors {
            let json = serde_json::to_string(err).unwrap();
            assert_eq!(&serde_json::from_str::<DocError>(&json).unwrap(), err);
        }
    }

    #[test]
    fn serde_wire_shape() {
        assert_eq!(
            serde_json::to_string(&DocError::ItemNotFound { name: "x".into() }).unwrap(),
            r#"{"kind":"item_not_found","name":"x"}"#
        );
    }

    #[test]
    fn from_server_fn_error_maps_transport_failures_to_request() {
        for (input, expected) in [
            (
                ServerFnErrorErr::Request("net down".into()),
                DocError::Request {
                    message: "net down".into(),
                },
            ),
            (
                ServerFnErrorErr::UnsupportedRequestMethod("bad method".into()),
                DocError::Request {
                    message: "bad method".into(),
                },
            ),
            (
                ServerFnErrorErr::Deserialization("bad json".into()),
                DocError::Request {
                    message: "bad json".into(),
                },
            ),
        ] {
            assert_eq!(DocError::from_server_fn_error(input), expected);
        }
    }

    #[test]
    fn from_server_fn_error_maps_internal_failures_to_internal() {
        for (input, message) in [
            (
                ServerFnErrorErr::Registration("poisoned".into()),
                "poisoned",
            ),
            (ServerFnErrorErr::ServerError("boom".into()), "boom"),
            (
                ServerFnErrorErr::MiddlewareError("middleware down".into()),
                "middleware down",
            ),
            (
                ServerFnErrorErr::Response("bad response".into()),
                "bad response",
            ),
        ] {
            assert_eq!(
                DocError::from_server_fn_error(input),
                DocError::Internal {
                    message: message.into()
                }
            );
        }
    }
}
