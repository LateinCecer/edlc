# edlc_doc_server

MCP and HTTP server for EDL documentation.

`edlc_doc_server` serves a `docs.db` produced by [`edlc_doc_db`](../edlc_doc_db) to clients over
the Model Context Protocol (MCP, stdio) and over HTTP with a Leptos + axum web frontend. It does
not depend on the EDL compiler (`edlc_core`) — it only reads the pre-built database.

## MCP mode

Run as an MCP server over stdio for use with Claude Desktop, Cursor, or any MCP-compatible
client:

```sh
edlc_doc_server mcp --db docs.db
```

### Tools exposed

| Tool | Parameters | Description |
|---|---|---|
| `search_docs` | `query` (string), `limit` (int, optional, default 20) | Full-text search across item names, modules, signatures, and doc text via FTS5. |
| `get_doc` | `name` (string) | Fetch a single item by its simple or qualified name. |
| `list_modules` | — | List all modules in the database. |
| `get_module` | `name` (string) | List all items belonging to a module. |

Each tool returns JSON containing the item's `id`, `kind`, `name`, `qual_name`, `module`,
`signature`, `doc_text`, and the full serde-JSON `blob` of the original `Item` (which includes
async info, modifiers, params, etc.).

## HTTP mode

Run as an HTTP server with a docs.rs-style web frontend (Leptos SSR + client hydration):

```sh
edlc_doc_server serve --db docs.db
```

Then browse to `http://127.0.0.1:8080/`.

### Pages

| Route | Description |
|---|---|
| `/` | Home page with search bar and module listing |
| `/search?q=...` | Full-text search results (FTS5), live-filtered |
| `/item/:name` | Single item page: kind, signature, doc text, module link |
| `/module/:name` | All items in a module, grouped by kind (Functions, Types, Variables, etc.) |

The layout features a left sidebar with module navigation, a top search bar, and a main content
area — inspired by docs.rs. The theme supports light and dark mode via `prefers-color-scheme`.

### Build

The HTTP frontend is a Leptos hybrid SSR + WASM hydration crate. Use `cargo-leptos` for
development and production builds:

```sh
# Install cargo-leptos (one-time)
cargo install cargo-leptos --locked
# Install the wasm-bindgen-cli matching your wasm-bindgen version
cargo install wasm-bindgen-cli

# Development with hot reload
cargo leptos watch

# Production build
cargo leptos build --release

# Run the server
./target/debug/edlc_doc_server serve --db docs.db
```

The `wasm32-unknown-unknown` target is required (`rustup target add wasm32-unknown-unknown`).

## Configuration

A TOML config file can be provided with `--config` for either mode:

```sh
edlc_doc_server mcp --config server.toml
edlc_doc_server serve --config server.toml
```

Example config:

```toml
db_path = "docs.db"

[mcp]
enabled = true

[http]
enabled = true
port = 8080
```

If `--config` is omitted, `--db` can be used to specify the database path directly (defaults to
`docs.db`). Both `mcp` and `http` are enabled by default.

## Status

This sub-crate is **LLM-generated** (Mistral Vibe) as part of the documentation-server work.
