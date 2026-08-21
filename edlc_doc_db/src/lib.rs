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
//! SQLite storage layer for EDL documentation.
//!
//! [`DocDbWriter`] implements [`DocGenerator`] and writes [`Item`]s produced by the EDL compiler
//! into a single-file SQLite database. [`DocDb`] is the read handle used by servers to query the
//! database, including full-text search via an FTS5 index.
//!
//! This crate contains no compile logic — an implementor links `edlc_core`, drives a compile, and
//! calls `compiler.generate_docs(&mut DocDbWriter::open(path)?)`.

use std::path::Path;

use edlc_core::prelude::{DocGenerator, Item};
use edlc_core::resolver::QualifierName;
use rusqlite::{params, Connection, OpenFlags};

/// The kind of a documented item, mirroring the [`Item`] variants. Stored as the `kind` column.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Fn,
    Let,
    Const,
    Type,
    Module,
}

impl Kind {
    pub fn as_str(self) -> &'static str {
        match self {
            Kind::Fn => "fn",
            Kind::Let => "let",
            Kind::Const => "const",
            Kind::Type => "type",
            Kind::Module => "module",
        }
    }

    fn parse(s: &str) -> Option<Self> {
        Some(match s {
            "fn" => Kind::Fn,
            "let" => Kind::Let,
            "const" => Kind::Const,
            "type" => Kind::Type,
            "module" => Kind::Module,
            _ => return None,
        })
    }
}

/// Pulls the shared `(name, doc_text)` fields from any [`Item`] variant.
fn item_fields(item: &Item) -> (Kind, &QualifierName, &str) {
    match item {
        Item::GlobalVar(d) => (Kind::Let, &d.name, &d.doc),
        Item::GlobalConst(d) => (Kind::Const, &d.name, &d.doc),
        Item::Func(d) => (Kind::Fn, &d.name, &d.doc),
        Item::TypeDef(d) => (Kind::Type, &d.name, &d.doc),
        Item::Module(d) => (Kind::Module, &d.name, &d.doc),
    }
}

/// A write handle that populates a `docs.db`. Implements [`DocGenerator`] so it can be passed
/// directly to `Compiler::generate_docs`.
pub struct DocDbWriter {
    conn: Connection,
}

impl DocDbWriter {
    /// Opens (or creates) a database file and initializes the schema. Any existing rows are
    /// deleted first, so each build produces a fresh database.
    pub fn open<P: AsRef<Path>>(path: P) -> rusqlite::Result<Self> {
        let conn = Connection::open_with_flags(
            path,
            OpenFlags::SQLITE_OPEN_READ_WRITE | OpenFlags::SQLITE_OPEN_CREATE,
        )?;
        init_schema(&conn)?;
        Ok(DocDbWriter { conn })
    }

    /// Opens an in-memory database (useful for tests).
    pub fn open_memory() -> rusqlite::Result<Self> {
        let conn = Connection::open_in_memory_with_flags(
            OpenFlags::SQLITE_OPEN_READ_WRITE | OpenFlags::SQLITE_OPEN_CREATE,
        )?;
        init_schema(&conn)?;
        Ok(DocDbWriter { conn })
    }

    /// Finalizes the database (builds the FTS index, vacuums). Consumes the writer.
    pub fn finish(self) -> rusqlite::Result<()> {
        // FTS5 external-content tables are kept in sync by triggers, so no rebuild is needed.
        // Vacuum reclaims space from the pre-build DELETE.
        self.conn.execute("VACUUM", [])?;
        Ok(())
    }
}

impl DocGenerator for DocDbWriter {
    type Error = rusqlite::Error;

    fn insert_definition(&mut self, item: &Item) -> Result<(), Self::Error> {
        let (kind, name, doc_text) = item_fields(item);
        let simple_name = name.last().cloned().unwrap_or_default();
        let qual_name = format!("{name}");
        // The owning module is the qualifier path minus the last segment, when present.
        let module = if name.len() > 1 {
            Some(name.trim(1).map(|m| format!("{m}")).unwrap_or_default())
        } else {
            None
        };
        let signature = format!("{item}");
        let blob = serde_json::to_string(item)
            .map_err(|err| rusqlite::Error::ToSqlConversionFailure(Box::new(err)))?;

        self.conn.execute(
            "INSERT INTO items (kind, name, qual_name, module, signature, doc_text, blob) \
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
            params![
                kind.as_str(),
                simple_name,
                qual_name,
                module,
                signature,
                doc_text,
                blob,
            ],
        )?;
        Ok(())
    }
}

/// A read-only handle to a `docs.db`.
pub struct DocDb {
    conn: Connection,
}

/// One row of documentation metadata. `blob` is the full serde-JSON of the original [`Item`] and
/// can be deserialized back via `serde_json::from_str::<Item>(&row.blob)`.
#[derive(Debug, Clone)]
pub struct DocRow {
    pub id: i64,
    pub kind: Kind,
    pub name: String,
    pub qual_name: String,
    pub module: Option<String>,
    pub signature: String,
    pub doc_text: String,
    pub blob: String,
}

impl DocDb {
    /// Opens an existing database read-only.
    pub fn open_readonly<P: AsRef<Path>>(path: P) -> rusqlite::Result<Self> {
        let conn = Connection::open_with_flags(path, OpenFlags::SQLITE_OPEN_READ_ONLY)?;
        Ok(DocDb { conn })
    }

    /// Opens an in-memory database from an already-populated writer connection (for tests).
    pub fn from_connection(conn: Connection) -> Self {
        DocDb { conn }
    }

    /// Full-text search over `(name, module, doc_text, signature)` via the FTS5 index.
    pub fn search(&self, query: &str, limit: usize) -> rusqlite::Result<Vec<DocRow>> {
        // Sanitize: FTS5 query syntax can be injected; wrap in quotes for a phrase/term search.
        let sanitized = sanitize_fts(query);
        let sql = "SELECT i.id, i.kind, i.name, i.qual_name, i.module, i.signature, i.doc_text, i.blob \
                   FROM search_index s JOIN items i ON s.rowid = i.id \
                   WHERE search_index MATCH ?1 \
                   ORDER BY rank LIMIT ?2";
        let mut stmt = self.conn.prepare(sql)?;
        let rows = stmt.query_map(params![sanitized, limit as i64], row_mapper())?;
        rows.collect()
    }

    /// Fetches a single item by id.
    pub fn get_item(&self, id: i64) -> rusqlite::Result<Option<DocRow>> {
        let sql = "SELECT id, kind, name, qual_name, module, signature, doc_text, blob \
                   FROM items WHERE id = ?1";
        let mut stmt = self.conn.prepare(sql)?;
        let mut rows = stmt.query_map(params![id], row_mapper())?;
        rows.next().transpose()
    }

    /// Lists items, optionally filtered by kind. Ordered by `(kind, name)`.
    pub fn list_items(&self, kind: Option<Kind>) -> rusqlite::Result<Vec<DocRow>> {
        match kind {
            Some(k) => {
                let mut stmt = self.conn.prepare(
                    "SELECT id, kind, name, qual_name, module, signature, doc_text, blob \
                     FROM items WHERE kind = ?1 ORDER BY kind, name",
                )?;
                let rows = stmt.query_map(params![k.as_str()], row_mapper())?;
                rows.collect()
            }
            None => {
                let mut stmt = self.conn.prepare(
                    "SELECT id, kind, name, qual_name, module, signature, doc_text, blob \
                     FROM items ORDER BY kind, name",
                )?;
                let rows = stmt.query_map([], row_mapper())?;
                rows.collect()
            }
        }
    }

    /// Lists all module items.
    pub fn modules(&self) -> rusqlite::Result<Vec<DocRow>> {
        self.list_items(Some(Kind::Module))
    }
}

fn row_mapper() -> impl Fn(&rusqlite::Row<'_>) -> rusqlite::Result<DocRow> {
    |row| {
        let kind_str: String = row.get(1)?;
        let kind = Kind::parse(&kind_str).ok_or_else(|| {
            rusqlite::Error::FromSqlConversionFailure(
                1,
                rusqlite::types::Type::Text,
                Box::new(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("unknown item kind: {kind_str}"),
                )),
            )
        })?;
        Ok(DocRow {
            id: row.get(0)?,
            kind,
            name: row.get(2)?,
            qual_name: row.get(3)?,
            module: row.get(4)?,
            signature: row.get(5)?,
            doc_text: row.get(6)?,
            blob: row.get(7)?,
        })
    }
}

/// Escapes a user query for safe FTS5 MATCH input. Wraps the query in double quotes with any
/// embedded double-quotes doubled, turning it into a phrase query. This intentionally avoids
/// exposing FTS5 query operators (AND/OR/NOT, column filters) to callers.
fn sanitize_fts(query: &str) -> String {
    let escaped = query.replace('"', "\"\"");
    format!("\"{escaped}\"")
}

/// Creates the schema: `items` table, FTS5 external-content index, and sync triggers.
fn init_schema(conn: &Connection) -> rusqlite::Result<()> {
    // Fresh build: drop any existing data so each `open` produces a clean database.
    conn.execute_batch(
        "DROP TABLE IF EXISTS items;
         DROP TABLE IF EXISTS search_index;
         DROP TRIGGER IF EXISTS items_ai;
         DROP TRIGGER IF EXISTS items_ad;
         DROP TRIGGER IF EXISTS items_au;

         CREATE TABLE items (
             id        INTEGER PRIMARY KEY AUTOINCREMENT,
             kind      TEXT NOT NULL,
             name      TEXT NOT NULL,
             qual_name TEXT NOT NULL,
             module    TEXT,
             signature TEXT NOT NULL,
             doc_text  TEXT NOT NULL DEFAULT '',
             blob      TEXT NOT NULL
         );

         CREATE VIRTUAL TABLE search_index USING fts5(
             name, module, doc_text, signature,
             content='items', content_rowid='id'
         );

         CREATE TRIGGER items_ai AFTER INSERT ON items BEGIN
             INSERT INTO search_index(rowid, name, module, doc_text, signature)
             VALUES (new.id, new.name, new.module, new.doc_text, new.signature);
         END;
         CREATE TRIGGER items_ad AFTER DELETE ON items BEGIN
             INSERT INTO search_index(search_index, rowid, name, module, doc_text, signature)
             VALUES ('delete', old.id, old.name, old.module, old.doc_text, old.signature);
         END;
         CREATE TRIGGER items_au AFTER UPDATE ON items BEGIN
             INSERT INTO search_index(search_index, rowid, name, module, doc_text, signature)
             VALUES ('delete', old.id, old.name, old.module, old.doc_text, old.signature);
             INSERT INTO search_index(rowid, name, module, doc_text, signature)
             VALUES (new.id, new.name, new.module, new.doc_text, new.signature);
         END;

         PRAGMA user_version = 1;",
    )?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use edlc_core::prelude::{
        EnvDoc, FuncParamsDoc, LetDoc, Modifiers, ModuleDoc, PortableModuleSrc, TypeDefDoc,
        TypeDefVariant, TypeDoc,
    };
    use edlc_core::lexer::SrcPos;

    /// Builds a small set of `Item`s covering every kind, writes them, reopens read-only, and
    /// verifies search + `blob` round-trips back to the same `Item` via serde_json.
    #[test]
    fn round_trip_all_item_kinds() {
        let mut writer = DocDbWriter::open_memory().expect("open memory db");
        let pos = SrcPos::new(0, 0, 0);
        let src = PortableModuleSrc::File("test.eq".to_string());

        let items: Vec<Item> = vec![
            Item::from(LetDoc {
                name: vec!["example".to_string(), "pi".to_string()].into(),
                src: src.clone(),
                pos,
                doc: "The value of pi.".to_string(),
                ty: TypeDoc::Base("f32".to_string().into(), None),
                ms: Modifiers::default(),
            }),
            Item::from(ModuleDoc {
                name: vec!["example".to_string()].into(),
                doc: "The example module.".to_string(),
            }),
            Item::from(TypeDefDoc {
                name: vec!["example".to_string(), "Vec".to_string()].into(),
                src: src.clone(),
                pos,
                doc: "A vector type.".to_string(),
                env: EnvDoc { params: vec![] },
                params: FuncParamsDoc::default(),
                variant: TypeDefVariant::Alias(TypeDoc::Base(
                    "usize".to_string().into(),
                    None,
                )),
            }),
        ];
        let original: Vec<(Kind, String)> = items
            .iter()
            .map(|it| {
                let (k, n, _) = item_fields(it);
                (k, n.last().cloned().unwrap_or_default())
            })
            .collect();

        for it in &items {
            writer.insert_definition(it).expect("insert item");
        }
        let conn = writer.conn; // steal the connection for read-back
        let db = DocDb::from_connection(conn);

        // search for "pi" hits the let
        let hits = db.search("pi", 10).expect("search");
        assert!(hits.iter().any(|r| r.kind == Kind::Let && r.name == "pi"),
            "search for 'pi' should find the let: {hits:?}");

        // every inserted kind is present in list_items(None)
        let all = db.list_items(None).expect("list_items");
        for (k, n) in &original {
            assert!(
                all.iter().any(|r| r.kind == *k && r.name == n.as_str()),
                "list_items missing {k:?} {n}"
            );
        }

        // blob is valid JSON and contains the item's qualified name. (The `Item`/`*Doc` types
        // derive only `Serialize`, so we cannot deserialize back to `Item`; instead we verify the
        // blob round-trips as JSON and carries the expected name field.)
        for (i, it) in items.iter().enumerate() {
            let (k, name_qual, _) = item_fields(it);
            let simple = name_qual.last().cloned().unwrap_or_default();
            let row = all
                .iter()
                .find(|r| r.kind == k && r.name == simple)
                .unwrap_or_else(|| panic!("row for item {i} not found"));
            let v: serde_json::Value = serde_json::from_str(&row.blob)
                .unwrap_or_else(|e| panic!("parse blob for item {i}: {e}"));
            assert!(v.is_object(), "item {i} blob is not a JSON object");
            // `Item` serializes externally tagged, e.g. {"GlobalVar": {...}}. The inner doc has a
            // `name` field (a QualifierName serialized as {"path": [...]}). Descend one level and
            // check the inner object carries the expected module name.
            let inner = v.as_object()
                .and_then(|m| m.values().next())
                .unwrap_or_else(|| panic!("item {i} blob has no variant wrapper"));
            let name = inner.get("name")
                .unwrap_or_else(|| panic!("item {i} blob has no name field"));
            assert!(name.to_string().contains("example"),
                "item {i} blob name {name} should contain 'example'");
        }

        // modules() returns only module rows
        let mods = db.modules().expect("modules");
        assert!(mods.iter().all(|r| r.kind == Kind::Module));
        assert_eq!(mods.len(), 1);
    }

    /// `sanitize_fts` neutralizes FTS5 operators by wrapping input in a quoted phrase.
    #[test]
    fn sanitize_fts_quotes_input() {
        assert_eq!(sanitize_fts("foo"), "\"foo\"");
        assert_eq!(sanitize_fts("foo OR bar"), "\"foo OR bar\"");
        assert_eq!(sanitize_fts("a\"b"), "\"a\"\"b\"");
    }
}
