# build_doc_db

Example implementor for `edlc_doc_db`: compiles a bundled EDL project and emits a `docs.db`.

This binary is the reference for how an implementor wires `edlc_core` to `edlc_doc_db`. It loads
EDL source via `edlc_core::prelude::FileSupplier`, drives a compile with the cranelift JIT backend
(`edlc_codegen_cranelift`) so the EDL standard library (trait impls, operators, etc.) is
registered, then calls `compiler.generate_docs(&mut DocDbWriter::open(out_path)?)` to populate a
SQLite documentation database.

The resulting `docs.db` is used to test and showcase `edlc_doc_db` (and, in later phases, the
`edlc_doc_server` documentation server).

## Usage

Run from this crate's directory (the `project/` path is resolved relative to the cwd):

```sh
cargo run -p build_doc_db --
```

This writes `docs.db` in the current directory. Inspect it with `sqlite3`:

```sh
sqlite3 docs.db "SELECT kind, COUNT(*) FROM items GROUP BY kind;"
sqlite3 docs.db "SELECT id, kind, name, qual_name FROM items WHERE qual_name LIKE '%example%';"
```

## Bundled project

`project/` contains a small EDL library exercising every `Item` kind the compiler emits:

- `lib.eq` — module doc, a `fn`, a `let`, a `const`, and `mod` declarations
- `types.eq` — named/tuple/unit structs, an `enum` with named and unit variants, a generic
  `type` with a `where` clause, and an `impl` block
- `child.eq` — a child submodule with its own documented `fn`

Doc generation needs no codegen; the cranelift backend is only used to populate compiler state
(standard-library registration) so the example bodies can use realistic constructs like
operators and loops.

## Status

This sub-crate is **LLM-generated** (Mistral Vibe) as part of the documentation-server work.
