# edlc_doc_server

MCP and HTTP server for EDL documentation.

`edlc_doc_server` serves a `docs.db` produced by [`edlc_doc_db`](../edlc_doc_db) to clients over
the Model Context Protocol (MCP, stdio) and over HTTP with a Leptos + axum web frontend. It does
not depend on the EDL compiler (`edlc_core`) — it only reads the pre-built database.

## MCP mode

Run as an MCP server over stdio for use with Claude Desktop, Cursor, or any MCP-compatible
client (from the workspace root):

```sh
cargo run -p edlc_doc_server mcp --db examples/build_doc_db/docs.db
```

`cargo run` is fine here — MCP mode never touches the Leptos frontend. (It does rebuild the
server binary with plain cargo, so if you afterwards want to serve the HTTP frontend, re-run
`cargo leptos build` first — see [HTTP mode](#http-mode).)

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

Run as an HTTP server with a docs.rs-style web frontend (Leptos SSR + client hydration) from
the workspace root:

```sh
cargo leptos watch serve --db examples/build_doc_db/docs.db
```

Then browse to `http://127.0.0.1:8080/`. `cargo leptos watch` builds the wasm frontend bundle and
the matching server binary, then runs the server with hot reload. For a one-off build + serve
without watch, see [Running](#running).

> **Note:** Do not start the HTTP server with `cargo run -p edlc_doc_server serve`. `cargo run`
> compiles the server with plain cargo, producing a binary whose server-rendered HTML does not
> match the wasm bundle that `cargo-leptos` builds — the page then fails to hydrate (a "hydration
> error" in the console) and is non-interactive. The server binary must always be produced by
> `cargo-leptos` (`cargo leptos watch` or `cargo leptos build`) so it matches the wasm.

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
development and production builds — it builds **both** the wasm frontend bundle and the server
binary, so the two always match (a server binary built with plain cargo will not hydrate):

```sh
# Install cargo-leptos (one-time)
cargo install cargo-leptos --locked
# Install the wasm-bindgen-cli matching your wasm-bindgen version
cargo install wasm-bindgen-cli

# Development with hot reload (builds both artifacts, then runs the server)
cargo leptos watch serve --db examples/build_doc_db/docs.db

# Production build (builds both; the server binary lands in target/release)
cargo leptos build --release
```

The `wasm32-unknown-unknown` target is required (`rustup target add wasm32-unknown-unknown`).

### Running

Everything runs from the workspace root — the site bundle location is resolved at compile time
(`target/site`), and `build_doc_db` writes its database next to its own manifest:

```sh
# (Re)build the example documentation database (writes examples/build_doc_db/docs.db)
cargo run -p build_doc_db

# Build the frontend (wasm) bundle AND the matching server binary
cargo leptos build

# Serve — run the server binary that `cargo leptos build` just produced
target/debug/edlc_doc_server serve --db examples/build_doc_db/docs.db
```

For a release build, run `cargo leptos build --release` and then
`target/release/edlc_doc_server serve --db examples/build_doc_db/docs.db` instead.

> **Note:** The serve step must run the binary that `cargo-leptos` built. Do not substitute
> `cargo run -p edlc_doc_server serve` here — plain `cargo run` produces a server binary whose
> SSR output does not match the wasm bundle, which breaks client-side hydration.

## Configuration

A TOML config file can be provided with `--config` for either mode:

```sh
cargo run -p edlc_doc_server mcp --config server.toml
target/debug/edlc_doc_server serve --config server.toml   # bin built via `cargo leptos build`
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
