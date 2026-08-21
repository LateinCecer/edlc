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
  owning module, `Display` signature, raw doc-comment text, and the full serde-JSON of the `Item`.
- `DocDb` — read-only handle with `search(q, limit)`, `get_item(id)`, `list_items(kind)`, and
  `modules()`. Search is backed by an FTS5 virtual table over `(name, module, doc_text, signature)`.
- `DocRow` / `Kind` — row data and the item-kind enum mirroring `Item`'s variants.

## Schema

A single `items` table plus an external-content `search_index` FTS5 table kept in sync by
`AFTER INSERT/UPDATE/DELETE` triggers. See `src/lib.rs` for the full DDL.

## Status

This sub-crate is **LLM-generated** (Mistral Vibe) as part of the documentation-server work.
