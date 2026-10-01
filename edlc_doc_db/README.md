# edlc_doc_db

SQLite storage layer for EDL documentation.

`edlc_doc_db` is an optional library that writes documentation items produced by the EDL compiler
(`edlc_core`) into a single-file SQLite database, and provides a read handle for querying that
database — including full-text search via an FTS5 index.

This crate is one part of the EDL documentation-server effort (see the workspace-level plan). It
contains **no compile logic**: an implementor links `edlc_core`, drives a compile, and calls
`compiler.generate_docs(&mut DocDbWriter::open(path)?)`. The writer implements `DocGenerator`, so
it slots directly into the compiler's existing documentation pass.

## What it provides

- `DocDbWriter` — implements `edlc_core::prelude::DocGenerator`. Each call to
  `insert_definition(&Item)` upserts a row holding the item's kind, simple and qualified name,
  owning module, `Display` signature, raw doc-comment text, the full serde-JSON of the `Item`, and
  (when built with the `render` feature) the typeset HTML of the doc comment.
- `DocDb` — read-only handle with `search(q, limit)`, `get_item(id)`, `list_items(kind)`, and
  `modules()`. Search is backed by an FTS5 virtual table over `(name, module, doc_text, signature)`.
- `DocRow` / `Kind` — row data and the item-kind enum mirroring `Item`'s variants.

## Schema

A single `items` table plus an external-content `search_index` FTS5 table kept in sync by
`AFTER INSERT/UPDATE/DELETE` triggers. The schema is versioned with `PRAGMA user_version = 2`
(v2 adds the `doc_html` column); `DocDb::open_readonly` rejects databases older than v2. See
`src/lib.rs` for the full DDL.

## Rendered documentation (Typst)

Doc comments are authored as a Typst-flavoured markdown subset. Enabling the optional `render`
feature (`edlc_doc_db = { ..., features = ["render"] }`) pulls in `typst` and lets the writer
typeset each doc comment to HTML at build time:

- `DocDbWriter::open_with_renderer(path, DocRenderer::new()?)` — open a writer that renders doc
  comments. `DocRenderer` (in `src/typst.rs`, behind the feature) runs Typst in a sandboxed world
  (a single in-memory `doc.typ`, no filesystem, `today() == None`).
- Each item's `doc_text` is rendered into the row's `doc_html` column. Rendering is **best-effort**:
  a doc comment that fails to compile emits a warning to stderr and stores an empty `doc_html`, so
  the build never fails. Consumers fall back to the raw `doc_text` when `doc_html` is empty.
- Fonts are vendored (OTF) and embedded via `include_bytes!` in `src/fonts.rs`
  (`pub static FONTS: &[(&str, &[u8])]`). Families are `New Computer Modern` and
  `New Computer Modern Math` (New Computer Modern 10, SIL Open Font License). They are served by
  `edlc_doc_server` for the browser.

**Rebuild note:** `doc_html` is produced at write time. Databases written before a given doc
comment was changed (or before `render` was enabled) must be rebuilt with `cargo run -p
build_doc_db` to refresh their HTML.

## Status

This sub-crate is **LLM-generated** (Mistral Vibe) as part of the documentation-server work.
