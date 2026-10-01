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
//! database: full-text search via an FTS5 index, lookup by name, and per-module listing.
//!
//! This crate contains no compile logic — an implementor links `edlc_core`, drives a compile, and
//! calls `compiler.generate_docs(&mut DocDbWriter::open(path)?)`.

pub mod fonts;

#[cfg(feature = "render")]
pub mod typst;

use std::path::Path;

use edlc_core::prelude::{DocGenerator, Item, TypeDoc};
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

    /// Parses a kind name (`"fn"`, `"let"`, `"const"`, `"type"`, `"module"`).
    pub fn parse(s: &str) -> Option<Self> {
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

/// The path of the item's associated type, when it is a plain base type reference.
///
/// This is the owner of items registered without a module path (a single-segment name), such
/// as the std intrinsics implemented for the primitive types. Generic parameters are
/// intentionally omitted: `usize::add`, not `usize::<...>::add`.
fn associated_type_path(item: &Item) -> Option<Vec<String>> {
    let ty = match item {
        Item::Func(d) => d.associated_type.as_ref(),
        Item::GlobalConst(d) => d.associated_type.as_ref(),
        _ => None,
    }?;
    match ty {
        TypeDoc::Base(name, _) => {
            let mut path = Vec::new();
            for segment in name.segments() {
                path.extend(segment.name.iter().cloned());
            }
            (!path.is_empty()).then_some(path)
        }
        _ => None,
    }
}

/// A write handle that populates a `docs.db`. Implements [`DocGenerator`] so it can be passed
/// directly to `Compiler::generate_docs`.
pub struct DocDbWriter {
    conn: Connection,
    /// An optional Typst renderer for doc comments. When present, the rendered HTML is
    /// stored in the `doc_html` column; when absent (or when a doc comment fails to render),
    /// `doc_html` stays empty and the raw `doc_text` is the fallback.
    #[cfg(feature = "render")]
    renderer: Option<crate::typst::DocRenderer>,
}

impl DocDbWriter {
    /// Opens (or creates) a database file and initializes the schema. Any existing rows are
    /// deleted first, so each build produces a fresh database. Doc comments are stored as raw
    /// text only (`doc_html` stays empty).
    pub fn open<P: AsRef<Path>>(path: P) -> rusqlite::Result<Self> {
        let conn = Connection::open_with_flags(
            path,
            OpenFlags::SQLITE_OPEN_READ_WRITE | OpenFlags::SQLITE_OPEN_CREATE,
        )?;
        init_schema(&conn)?;
        #[cfg(feature = "render")]
        return Ok(DocDbWriter {
            conn,
            renderer: None,
        });
        #[cfg(not(feature = "render"))]
        Ok(DocDbWriter { conn })
    }

    /// Like [`DocDbWriter::open`], but with a Typst renderer attached so that doc comments
    /// are rendered to HTML (stored in the `doc_html` column) as items are inserted.
    #[cfg(feature = "render")]
    pub fn open_with_renderer<P: AsRef<Path>>(
        path: P,
        renderer: crate::typst::DocRenderer,
    ) -> rusqlite::Result<Self> {
        let conn = Connection::open_with_flags(
            path,
            OpenFlags::SQLITE_OPEN_READ_WRITE | OpenFlags::SQLITE_OPEN_CREATE,
        )?;
        init_schema(&conn)?;
        Ok(DocDbWriter {
            conn,
            renderer: Some(renderer),
        })
    }

    /// Opens an in-memory database (useful for tests).
    pub fn open_memory() -> rusqlite::Result<Self> {
        let conn = Connection::open_in_memory_with_flags(
            OpenFlags::SQLITE_OPEN_READ_WRITE | OpenFlags::SQLITE_OPEN_CREATE,
        )?;
        init_schema(&conn)?;
        #[cfg(feature = "render")]
        return Ok(DocDbWriter {
            conn,
            renderer: None,
        });
        #[cfg(not(feature = "render"))]
        Ok(DocDbWriter { conn })
    }

    /// Like [`DocDbWriter::open_memory`], but with a Typst renderer attached so that doc
    /// comments are rendered to HTML as items are inserted.
    #[cfg(feature = "render")]
    pub fn open_memory_with_renderer(renderer: crate::typst::DocRenderer) -> rusqlite::Result<Self> {
        let conn = Connection::open_in_memory_with_flags(
            OpenFlags::SQLITE_OPEN_READ_WRITE | OpenFlags::SQLITE_OPEN_CREATE,
        )?;
        init_schema(&conn)?;
        Ok(DocDbWriter {
            conn,
            renderer: Some(renderer),
        })
    }

    /// Finalizes the database (builds the FTS index, vacuums). Consumes the writer.
    pub fn finish(self) -> rusqlite::Result<()> {
        // FTS5 external-content tables are kept in sync by triggers, so no rebuild is needed.
        // Vacuum reclaims space from the pre-build DELETE.
        self.conn.execute("VACUUM", [])?;
        Ok(())
    }

    /// Renders a doc comment with the attached renderer, when there is one.
    ///
    /// A doc comment that fails to render is a warning, not an error: an empty
    /// string is returned so the raw `doc_text` remains the fallback.
    fn render_doc_text(&self, doc_text: &str) -> String {
        #[cfg(not(feature = "render"))]
        let _ = doc_text;
        #[cfg(feature = "render")]
        {
            if !doc_text.is_empty() {
                if let Some(renderer) = &self.renderer {
                    return match renderer.render(doc_text) {
                        Ok(html) => html,
                        Err(e) => {
                            eprintln!("warning: {e}; keeping the raw doc text as fallback");
                            String::new()
                        }
                    };
                }
            }
        }
        String::new()
    }
}

impl DocGenerator for DocDbWriter {
    type Error = rusqlite::Error;

    fn insert_definition(&mut self, item: &Item) -> Result<(), Self::Error> {
        let (kind, name, doc_text) = item_fields(item);
        let simple_name = name.last().cloned().unwrap_or_default();
        // Items registered without a module path (a single-segment name) that carry an
        // associated type — e.g. the std intrinsics, where `add` is implemented for `usize` —
        // are owned by that associated type, so its path prefixes the qualified name.
        let mut full_name = name.clone();
        if full_name.len() == 1 {
            if let Some(prefix) = associated_type_path(item) {
                let mut prefixed = QualifierName::from(prefix);
                prefixed.extend(name.iter().cloned());
                full_name = prefixed;
            }
        }
        let qual_name = format!("{full_name}");
        // The owning module is the qualifier path minus the last segment, when present.
        let module = full_name.trim(1).map(|m| format!("{m}"));
        let signature = format!("{item}");
        let doc_html = self.render_doc_text(doc_text);
        let blob = serde_json::to_string(item)
            .map_err(|err| rusqlite::Error::ToSqlConversionFailure(Box::new(err)))?;

        self.conn.execute(
            "INSERT INTO items (kind, name, qual_name, module, signature, doc_text, doc_html, blob) \
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
            params![
                kind.as_str(),
                simple_name,
                qual_name,
                module,
                signature,
                doc_text,
                doc_html,
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
    /// The Typst-rendered HTML of `doc_text` (a standalone HTML document), or empty when the
    /// item has no doc comment, the database was built without a renderer, or the markup
    /// failed to render. Deliberately not part of the FTS index.
    pub doc_html: String,
    pub blob: String,
}

/// The schema version of the `items` table.
const SCHEMA_VERSION: i32 = 2;

impl DocDb {
    /// Opens an existing database read-only.
    ///
    /// Rejects databases built with an older schema (e.g. missing the `doc_html`
    /// column) with a hint to rebuild.
    pub fn open_readonly<P: AsRef<Path>>(path: P) -> rusqlite::Result<Self> {
        let conn = Connection::open_with_flags(path, OpenFlags::SQLITE_OPEN_READ_ONLY)?;
        let version: i32 = conn.query_row("PRAGMA user_version", [], |r| r.get(0))?;
        if version < SCHEMA_VERSION {
            return Err(rusqlite::Error::ToSqlConversionFailure(Box::new(
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "this documentation database has an outdated schema (user_version = \
                         {version}, expected >= {SCHEMA_VERSION}); rebuild it with \
                         `cargo run -p build_doc_db`"
                    ),
                ),
            )));
        }
        Ok(DocDb { conn })
    }

    /// Opens an in-memory database from an already-populated writer connection (for tests).
    pub fn from_connection(conn: Connection) -> Self {
        DocDb { conn }
    }

    /// Full-text search over `(name, module, doc_text, signature)` via the FTS5 index.
    ///
    /// Each whitespace-separated token of `query` must match a token prefix in at least one
    /// indexed column (implicit AND). Results are ranked with bm25, weighting name matches
    /// highest, then signatures, doc text, and module paths. When `kind` is `Some`, only items
    /// of that kind are returned. An empty or whitespace-only query matches nothing and returns
    /// an empty list.
    pub fn search(
        &self,
        query: &str,
        limit: usize,
        kind: Option<Kind>,
    ) -> rusqlite::Result<Vec<DocRow>> {
        let fts_query = match build_fts_query(query) {
            Some(q) => q,
            None => return Ok(Vec::new()),
        };
        let kind_name = kind.map(|k| k.as_str().to_string());
        let sql =
            "SELECT i.id, i.kind, i.name, i.qual_name, i.module, i.signature, i.doc_text, i.doc_html, i.blob \
                    FROM search_index s JOIN items i ON s.rowid = i.id \
                    WHERE search_index MATCH ?1 AND (?3 IS NULL OR i.kind = ?3) \
                    ORDER BY bm25(search_index, 10.0, 1.0, 2.0, 3.0) LIMIT ?2";
        let mut stmt = self.conn.prepare(sql)?;
        let rows = stmt.query_map(
            params![fts_query, limit as i64, kind_name],
            row_mapper(),
        )?;
        rows.collect()
    }

    /// Fetches a single item by id.
    pub fn get_item(&self, id: i64) -> rusqlite::Result<Option<DocRow>> {
        let sql = "SELECT id, kind, name, qual_name, module, signature, doc_text, doc_html, blob \
                   FROM items WHERE id = ?1";
        let mut stmt = self.conn.prepare(sql)?;
        let mut rows = stmt.query_map(params![id], row_mapper())?;
        rows.next().transpose()
    }

    /// Fetches a single item by name. An exact `qual_name` match takes precedence over a simple
    /// `name` match; on a simple-name match the first row in `(kind, name)` order is returned.
    /// Returns `Ok(None)` when no item matches.
    pub fn get_item_by_name(&self, name: &str) -> rusqlite::Result<Option<DocRow>> {
        let sql = "SELECT id, kind, name, qual_name, module, signature, doc_text, doc_html, blob \
                    FROM items WHERE qual_name = ?1 OR name = ?1 \
                    ORDER BY (qual_name != ?1), kind, name LIMIT 1";
        let mut stmt = self.conn.prepare(sql)?;
        let mut rows = stmt.query_map(params![name], row_mapper())?;
        rows.next().transpose()
    }

    /// Fetches all items matching `name` — an exact `qual_name` match plus all simple-`name`
    /// matches — in the same order as [`get_item_by_name`] (exact qual names first, then
    /// `(kind, name)`). Use this to detect and report ambiguous simple names instead of
    /// silently picking the first match.
    pub fn get_item_by_name_candidates(&self, name: &str) -> rusqlite::Result<Vec<DocRow>> {
        let sql = "SELECT id, kind, name, qual_name, module, signature, doc_text, doc_html, blob \
                    FROM items WHERE qual_name = ?1 OR name = ?1 \
                    ORDER BY (qual_name != ?1), kind, name";
        let mut stmt = self.conn.prepare(sql)?;
        let rows = stmt.query_map(params![name], row_mapper())?;
        rows.collect()
    }

    /// Lists all items belonging to `module` — items directly inside it plus items of nested
    /// submodules — ordered by `(kind, name)`.
    pub fn list_module_items(&self, module: &str) -> rusqlite::Result<Vec<DocRow>> {
        let escaped = module
            .replace('\\', "\\\\")
            .replace('%', "\\%")
            .replace('_', "\\_");
        let prefix = format!("{escaped}::%");
        let sql = "SELECT id, kind, name, qual_name, module, signature, doc_text, doc_html, blob \
                   FROM items WHERE module = ?1 OR qual_name LIKE ?2 ESCAPE '\\' \
                   ORDER BY kind, name";
        let mut stmt = self.conn.prepare(sql)?;
        let rows = stmt.query_map(params![module, prefix], row_mapper())?;
        rows.collect()
    }

    /// Lists items, optionally filtered by kind. Ordered by `(kind, name)`.
    pub fn list_items(&self, kind: Option<Kind>) -> rusqlite::Result<Vec<DocRow>> {
        match kind {
            Some(k) => {
                let mut stmt = self.conn.prepare(
                    "SELECT id, kind, name, qual_name, module, signature, doc_text, doc_html, blob \
                     FROM items WHERE kind = ?1 ORDER BY kind, name",
                )?;
                let rows = stmt.query_map(params![k.as_str()], row_mapper())?;
                rows.collect()
            }
            None => {
                let mut stmt = self.conn.prepare(
                    "SELECT id, kind, name, qual_name, module, signature, doc_text, doc_html, blob \
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
            doc_html: row.get(7)?,
            blob: row.get(8)?,
        })
    }
}

/// Builds a safe FTS5 MATCH query from a user query: each whitespace-separated token is
/// wrapped in double quotes (with embedded double-quotes doubled) and suffixed with `*`,
/// turning it into a prefix term. The terms are separated by spaces, which FTS5 interprets
/// as an implicit AND. This matches partial words while typing (e.g. `add ov` matches
/// `add_overflow`) and intentionally avoids exposing FTS5 query operators (OR/NOT, column
/// filters) to callers. Returns `None` when the query has no tokens.
fn build_fts_query(query: &str) -> Option<String> {
    let terms = query
        .split_whitespace()
        .map(|t| format!("\"{}\"*", t.replace('"', "\"\"")))
        .collect::<Vec<_>>();
    (!terms.is_empty()).then(|| terms.join(" "))
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
              doc_html  TEXT NOT NULL DEFAULT '',
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

          PRAGMA user_version = 2;",
    )?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use edlc_core::lexer::SrcPos;
    use edlc_core::prelude::{
        EnvDoc, FuncDoc, FuncParamsDoc, LetDoc, Modifiers, ModuleDoc, PortableModuleSrc,
        TypeDefDoc, TypeDefVariant, TypeDoc, TypeNameDoc, TypeNameSegmentDoc,
    };

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
                variant: TypeDefVariant::Alias(TypeDoc::Base("usize".to_string().into(), None)),
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
        let hits = db.search("pi", 10, None).expect("search");
        assert!(
            hits.iter().any(|r| r.kind == Kind::Let && r.name == "pi"),
            "search for 'pi' should find the let: {hits:?}"
        );

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
            let inner = v
                .as_object()
                .and_then(|m| m.values().next())
                .unwrap_or_else(|| panic!("item {i} blob has no variant wrapper"));
            let name = inner
                .get("name")
                .unwrap_or_else(|| panic!("item {i} blob has no name field"));
            assert!(
                name.to_string().contains("example"),
                "item {i} blob name {name} should contain 'example'"
            );
        }

        // modules() returns only module rows
        let mods = db.modules().expect("modules");
        assert!(mods.iter().all(|r| r.kind == Kind::Module));
        assert_eq!(mods.len(), 1);
    }

    /// `build_fts_query` turns each token into a quoted prefix term (implicit AND).
    #[test]
    fn build_fts_query_prefix_terms() {
        assert_eq!(build_fts_query("foo").as_deref(), Some("\"foo\"*"));
        assert_eq!(
            build_fts_query("foo bar").as_deref(),
            Some("\"foo\"* \"bar\"*")
        );
        assert_eq!(
            build_fts_query("foo   bar").as_deref(),
            Some("\"foo\"* \"bar\"*")
        );
        // FTS5 operators are not special: "OR" is just another (prefix) term.
        assert_eq!(
            build_fts_query("foo OR bar").as_deref(),
            Some("\"foo\"* \"OR\"* \"bar\"*")
        );
        assert_eq!(build_fts_query("a\"b").as_deref(), Some("\"a\"\"b\"*"));
        assert_eq!(build_fts_query(""), None);
        assert_eq!(build_fts_query("   \t "), None);
    }

    /// A partial token matches items whose tokens start with it.
    #[test]
    fn search_matches_token_prefixes() {
        let db = db_with(&[let_item(&["example", "vector"]), let_item(&["example", "pi"])]);
        let hits = db.search("vect", 10, None).expect("search");
        let names: Vec<&str> = hits.iter().map(|r| r.name.as_str()).collect();
        assert_eq!(names, vec!["vector"]);
        assert!(db.search("pi", 10, None).expect("search").len() == 1);
        assert!(db.search("p", 10, None).expect("search").len() == 1);
        // no token starts with "q"
        assert!(db.search("q", 10, None).expect("search").is_empty());
    }

    /// Multi-token queries implicitly AND: all token prefixes must occur.
    #[test]
    fn search_multi_token_is_implicit_and() {
        let mut writer = DocDbWriter::open_memory().expect("open memory db");
        let item = let_item(&["example", "add_overflow"]);
        writer.insert_definition(&item).expect("insert item");
        let db = DocDb::from_connection(writer.conn);

        // Both prefixes occur in the name's tokens ("add", "overflow").
        let hits = db.search("add ov", 10, None).expect("search");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].name, "add_overflow");

        // One prefix occurs, the other does not.
        assert!(db.search("add xyz", 10, None).expect("search").is_empty());
        // Tokens in reverse order also match (AND is order-insensitive).
        assert_eq!(db.search("ov add", 10, None).expect("search").len(), 1);
    }

    /// An empty or whitespace-only query is not an error: an empty list is returned.
    #[test]
    fn search_empty_query_returns_empty() {
        let db = db_with(&[let_item(&["a", "f"])]);
        assert!(db.search("", 10, None).expect("search").is_empty());
        assert!(db.search("   ", 10, None).expect("search").is_empty());
    }

    /// The `kind` filter restricts results to one item kind.
    #[test]
    fn search_kind_filter() {
        let db = db_with(&[
            module_item(&["m"]),
            let_item(&["m", "vec_push"]),
            let_item(&["m", "vec_pop"]),
        ]);
        let all = db.search("vec", 10, None).expect("search");
        assert_eq!(all.len(), 2);
        let lets = db.search("vec", 10, Some(Kind::Let)).expect("search");
        assert_eq!(lets.len(), 2);
        let fns = db.search("vec", 10, Some(Kind::Fn)).expect("search");
        assert!(fns.is_empty());
        // A kind the query text does not match in yields no rows.
        assert_eq!(db.search("m", 10, Some(Kind::Module)).expect("search").len(), 1);
        assert_eq!(db.search("m", 10, Some(Kind::Fn)).expect("search").len(), 0);
    }

    /// `get_item_by_name_candidates` returns every match, exact qual names first.
    #[test]
    fn get_item_by_name_candidates_ambiguous_simple_name() {
        let db = db_with(&[let_item(&["a", "f"]), let_item(&["b", "f"]), let_item(&["f"])]);
        let rows = db
            .get_item_by_name_candidates("f")
            .expect("candidates");
        let quals: Vec<&str> = rows.iter().map(|r| r.qual_name.as_str()).collect();
        // The exact qual name "f" comes first, then the simple-name matches in (kind, name) order.
        assert_eq!(quals, vec!["f", "a::f", "b::f"]);
        let exact = db
            .get_item_by_name_candidates("a::f")
            .expect("candidates");
        assert_eq!(exact.len(), 1);
        assert_eq!(exact[0].qual_name, "a::f");
        assert!(db
            .get_item_by_name_candidates("nope")
            .expect("candidates")
            .is_empty());
    }

    /// Builds a `Let` item from the segments of a qualifier path.
    fn let_item(parts: &[&str]) -> Item {
        Item::from(LetDoc {
            name: parts
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .into(),
            src: PortableModuleSrc::File("test.eq".to_string()),
            pos: SrcPos::new(0, 0, 0),
            doc: String::new(),
            ty: TypeDoc::Base("f32".to_string().into(), None),
            ms: Modifiers::default(),
        })
    }

    /// Builds a `Module` item from the segments of a qualifier path.
    fn module_item(parts: &[&str]) -> Item {
        Item::from(ModuleDoc {
            name: parts
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .into(),
            doc: String::new(),
        })
    }

    /// Builds a `Func` item from the segments of a qualifier path, with an optional
    /// single-segment associated type.
    fn assoc_fn(parts: &[&str], assoc: Option<&str>) -> Item {
        Item::from(FuncDoc {
            name: parts
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .into(),
            src: PortableModuleSrc::File("test.eq".to_string()),
            pos: SrcPos::new(0, 0, 0),
            doc: String::new(),
            env: EnvDoc { params: vec![] },
            params: FuncParamsDoc::default(),
            ret: TypeDoc::Base("usize".to_string().into(), None),
            ms: Modifiers::default(),
            async_return: false,
            associated_type: assoc.map(|a| TypeDoc::Base(a.to_string().into(), None)),
        })
    }

    /// Writes `items` into an in-memory database and returns a read handle over it.
    fn db_with(items: &[Item]) -> DocDb {
        let mut writer = DocDbWriter::open_memory().expect("open memory db");
        for item in items {
            writer.insert_definition(item).expect("insert item");
        }
        DocDb::from_connection(writer.conn)
    }

    /// `get_item_by_name` finds an item by its exact qualified name.
    #[test]
    fn get_item_by_name_exact_qual_name() {
        let db = db_with(&[let_item(&["a", "b", "f"])]);
        let row = db.get_item_by_name("a::b::f").expect("get_item_by_name");
        let row = row.expect("exact qual name should match");
        assert_eq!(row.qual_name, "a::b::f");
        assert_eq!(row.name, "f");
        assert_eq!(row.module.as_deref(), Some("a::b"));
    }

    /// `get_item_by_name` falls back to a simple-name match when no qual name matches.
    #[test]
    fn get_item_by_name_falls_back_to_simple_name() {
        let db = db_with(&[let_item(&["a", "f"])]);
        let row = db.get_item_by_name("f").expect("get_item_by_name");
        let row = row.expect("simple name should match");
        assert_eq!(row.qual_name, "a::f");
        assert_eq!(row.name, "f");
    }

    /// An exact `qual_name` match wins over a colliding simple-`name` match.
    #[test]
    fn get_item_by_name_prefers_exact_qual_name_over_simple() {
        let db = db_with(&[let_item(&["a", "f"]), let_item(&["f"])]);
        let row = db.get_item_by_name("f").expect("get_item_by_name");
        let row = row.expect("name should match");
        assert_eq!(row.qual_name, "f");
    }

    /// `get_item_by_name` returns `Ok(None)` for a name that matches no item.
    #[test]
    fn get_item_by_name_unknown_returns_none() {
        let db = db_with(&[let_item(&["a", "f"])]);
        let row = db.get_item_by_name("nope").expect("get_item_by_name");
        assert!(row.is_none());
    }

    /// `list_module_items` returns items directly in the module plus items of nested submodules.
    #[test]
    fn list_module_items_includes_nested_submodules() {
        let db = db_with(&[
            module_item(&["m"]),
            let_item(&["m", "f"]),
            module_item(&["m", "s"]),
            let_item(&["m", "s", "g"]),
        ]);
        let rows = db.list_module_items("m").expect("list_module_items");
        let quals: Vec<&str> = rows.iter().map(|r| r.qual_name.as_str()).collect();
        assert_eq!(quals, vec!["m::f", "m::s::g", "m::s"]);
        let nested = db.list_module_items("m::s").expect("list_module_items");
        let quals: Vec<&str> = nested.iter().map(|r| r.qual_name.as_str()).collect();
        assert_eq!(quals, vec!["m::s::g"]);
    }

    /// `list_module_items` returns an empty list for a module with no items.
    #[test]
    fn list_module_items_unknown_module_returns_empty() {
        let db = db_with(&[module_item(&["m"]), let_item(&["m", "f"])]);
        let rows = db.list_module_items("nope").expect("list_module_items");
        assert!(rows.is_empty());
    }

    /// `_` in a module name is not treated as a LIKE single-char wildcard.
    #[test]
    fn list_module_items_escapes_underscore_in_like() {
        let db = db_with(&[
            module_item(&["my_mod"]),
            let_item(&["my_mod", "x"]),
            module_item(&["myXmod"]),
            let_item(&["myXmod", "x"]),
        ]);
        let rows = db.list_module_items("my_mod").expect("list_module_items");
        let quals: Vec<&str> = rows.iter().map(|r| r.qual_name.as_str()).collect();
        assert_eq!(quals, vec!["my_mod::x"]);
    }

    /// An item registered with a single-segment name is owned by its associated type: the
    /// associated type's path prefixes the qualified name and becomes the module.
    #[test]
    fn insert_definition_uses_associated_type_for_single_segment_names() {
        let db = db_with(&[assoc_fn(&["add"], Some("usize"))]);
        let row = db.get_item_by_name("usize::add").expect("get_item_by_name");
        let row = row.expect("prefixed qual name should match");
        assert_eq!(row.name, "add");
        assert_eq!(row.qual_name, "usize::add");
        assert_eq!(row.module.as_deref(), Some("usize"));
        // The simple name still resolves.
        assert!(db.get_item_by_name("add").expect("get_item_by_name").is_some());
        // The associated type acts as a module: its items can be listed.
        let rows = db.list_module_items("usize").expect("list_module_items");
        let quals: Vec<&str> = rows.iter().map(|r| r.qual_name.as_str()).collect();
        assert_eq!(quals, vec!["usize::add"]);
    }

    /// A multi-segment name already carries its owner and is not prefixed again, even when
    /// the item also has an associated type.
    #[test]
    fn insert_definition_does_not_double_prefix_associated_type() {
        let db = db_with(&[assoc_fn(&["m", "S", "norm"], Some("S"))]);
        let rows = db.list_items(None).expect("list_items");
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].qual_name, "m::S::norm");
        assert_eq!(rows[0].module.as_deref(), Some("m::S"));
    }

    /// The full path of a qualified associated type is used as the prefix.
    #[test]
    fn insert_definition_prefixes_with_qualified_associated_type() {
        let item = Item::from(FuncDoc {
            name: vec!["add".to_string()].into(),
            src: PortableModuleSrc::File("test.eq".to_string()),
            pos: SrcPos::new(0, 0, 0),
            doc: String::new(),
            env: EnvDoc { params: vec![] },
            params: FuncParamsDoc::default(),
            ret: TypeDoc::Base("usize".to_string().into(), None),
            ms: Modifiers::default(),
            async_return: false,
            associated_type: Some(TypeDoc::Base(
                TypeNameDoc::from(vec![
                    TypeNameSegmentDoc::from(QualifierName::from(vec!["core".to_string()])),
                    TypeNameSegmentDoc::from(QualifierName::from(vec!["u8".to_string()])),
                ]),
                None,
            )),
        });
        let db = db_with(&[item]);
        let row = db.get_item_by_name("core::u8::add").expect("get_item_by_name");
        let row = row.expect("prefixed qual name should match");
        assert_eq!(row.qual_name, "core::u8::add");
        assert_eq!(row.module.as_deref(), Some("core::u8"));
    }

    /// A single-segment name without an associated type stays module-less.
    #[test]
    fn insert_definition_single_segment_without_associated_type_has_no_module() {
        let db = db_with(&[assoc_fn(&["f"], None)]);
        let rows = db.list_items(None).expect("list_items");
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].qual_name, "f");
        assert!(rows[0].module.is_none());
    }

    /// Without a renderer, `doc_html` is empty for every item (raw text is the
    /// content).
    #[test]
    fn doc_html_is_empty_without_a_renderer() {
        let db = db_with(&[let_item(&["example", "pi"])]);
        let rows = db.list_items(None).expect("list_items");
        assert!(rows.iter().all(|r| r.doc_html.is_empty()));
    }

    /// With a renderer, a doc comment is stored both as raw text and as a
    /// standalone HTML document; empty doc comments stay empty in both.
    #[cfg(feature = "render")]
    #[test]
    fn doc_html_is_stored_with_a_renderer() {
        let mut writer =
            DocDbWriter::open_memory_with_renderer(crate::typst::DocRenderer::new())
                .expect("open memory db");
        writer
            .insert_definition(&let_item_with_doc(&["example", "pi"], "The value of *pi*."))
            .expect("insert item");
        writer.insert_definition(&let_item(&["example", "e"])).expect("insert item");
        let db = DocDb::from_connection(writer.conn);

        let row = db.get_item_by_name("example::pi").expect("get").expect("row");
        assert_eq!(row.doc_text, "The value of *pi*.");
        assert!(row.doc_html.starts_with("<!DOCTYPE html>"));
        assert!(row.doc_html.contains("pi"));

        // An item without a doc comment stays empty in both columns.
        let row = db.get_item_by_name("example::e").expect("get").expect("row");
        assert!(row.doc_text.is_empty());
        assert!(row.doc_html.is_empty());
    }

    /// A doc comment that fails to render warns but keeps the build going:
    /// `doc_html` is empty, `doc_text` keeps the raw markup.
    #[cfg(feature = "render")]
    #[test]
    fn broken_doc_comment_falls_back_to_raw_text() {
        let mut writer =
            DocDbWriter::open_memory_with_renderer(crate::typst::DocRenderer::new())
                .expect("open memory db");
        writer
            .insert_definition(&let_item_with_doc(&["example", "bad"], "#let x = "))
            .expect("insert must not fail");
        let db = DocDb::from_connection(writer.conn);
        let row = db.get_item_by_name("example::bad").expect("get").expect("row");
        assert_eq!(row.doc_text, "#let x = ");
        assert!(row.doc_html.is_empty());
    }

    /// A database built with the v1 schema (no `doc_html` column) is rejected
    /// by `open_readonly` with a rebuild hint.
    #[test]
    fn open_readonly_rejects_v1_schema() {
        let dir = std::env::temp_dir().join(format!("edlc_doc_db_v1_test_{}", std::process::id()));
        let path = dir.join("v1.db");
        std::fs::create_dir_all(&dir).expect("create temp dir");
        {
            let conn = Connection::open(&path).expect("open v1 db");
            conn.execute_batch(
                "CREATE TABLE items (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    kind TEXT NOT NULL, name TEXT NOT NULL, qual_name TEXT NOT NULL,
                    module TEXT, signature TEXT NOT NULL,
                    doc_text TEXT NOT NULL DEFAULT '', blob TEXT NOT NULL
                );
                PRAGMA user_version = 1;",
            )
            .expect("create v1 schema");
        }
        let err = match DocDb::open_readonly(&path) {
            Ok(_) => panic!("v1 db must be rejected"),
            Err(err) => err,
        };
        let msg = err.to_string();
        assert!(msg.contains("rebuild"), "error should hint at rebuilding: {msg}");
        let _ = std::fs::remove_file(&path);
        let _ = std::fs::remove_dir(&dir);
    }

    /// Builds a `Let` item with the given doc text.
    #[cfg(feature = "render")]
    fn let_item_with_doc(parts: &[&str], doc: &str) -> Item {
        Item::from(LetDoc {
            name: parts
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .into(),
            src: PortableModuleSrc::File("test.eq".to_string()),
            pos: SrcPos::new(0, 0, 0),
            doc: doc.to_string(),
            ty: TypeDoc::Base("f32".to_string().into(), None),
            ms: Modifiers::default(),
        })
    }
}
