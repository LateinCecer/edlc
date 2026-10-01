# edlc_doc_server

MCP and HTTP server for EDL documentation.

`edlc_doc_server` serves a `docs.db` produced by [`edlc_doc_db`](../edlc_doc_db) to clients over
the Model Context Protocol (MCP, stdio) and over HTTP with a Leptos + axum web frontend. It does
not depend on the EDL compiler (`edlc_core`) — it only reads the pre-built database.

## MCP mode

MCP supports two transports:

- **stdio** (default) — the client spawns the server and talks to it over its stdio streams.
  This is the spec-standard local transport and what desktop MCP clients (Claude Desktop,
  Cursor, ...) use to launch local servers:

  ```sh
  cargo run -p edlc_doc_server mcp --db examples/build_doc_db/docs.db
  ```

- **Streamable HTTP over TCP** — the spec's network transport, for clients that connect over
  the network. The server listens on a TCP socket and serves the MCP endpoint at `/mcp`
  (default `http://127.0.0.1:3000/mcp`):

  ```sh
  cargo run -p edlc_doc_server mcp --db examples/build_doc_db/docs.db --transport http \
      [--mcp-port 3000] [--mcp-bind 127.0.0.1]
  ```

  Clients configure the full endpoint URL, e.g. `http://127.0.0.1:3000/mcp`.

`cargo run` is fine here — MCP mode never touches the Leptos frontend. (It does rebuild the
server binary with plain cargo, so if you afterwards want to serve the HTTP frontend, re-run
`cargo leptos build` first — see [HTTP mode](#http-mode).)

> **Security note:** The MCP HTTP endpoint has no authentication. It binds to `127.0.0.1` by
> default and then enforces loopback-only `Host` headers. If you bind to a non-loopback
> address, `Host` validation is disabled — put an authenticating reverse proxy in front of
> such a deployment.

### Tools exposed

| Tool | Parameters | Description |
|---|---|---|
| `search_docs` | `query` (string), `limit` (int, optional, default 20), `kind` (string, optional), `details` (bool, optional, default false) | Full-text search across item names, modules, signatures, and doc text via FTS5 (prefix matching, names ranked highest). |
| `get_doc` | `name` (string), `details` (bool, optional, default false) | Fetch a single item by its qualified name. If a simple name is ambiguous, an error lists the candidate qualified names. |
| `list_modules` | `details` (bool, optional, default false) | List all modules in the database. |
| `get_module` | `name` (string), `details` (bool, optional, default false) | List all items belonging to a module (including nested submodules). |
| `list_items` | `kind` (string, optional), `details` (bool, optional, default false) | List all items in the database, including crate-root items that `get_module` cannot reach. |

`kind` accepts `fn`, `let`, `const`, `type`, or `module`. Each item is returned as JSON with
`id`, `kind`, `name`, `qual_name`, `module`, `signature`, and `doc_text`; the full serde-JSON
`blob` of the original `Item` (async info, modifiers, params, fields, variants, ...) is only
included when `details: true`, to keep listing and search responses small.

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
| `/item/:name` | Single item page: kind, signature, rendered documentation (or raw doc text), module link |
| `/module/:name` | All items in a module, grouped by kind (Functions, Types, Variables, etc.) |
| `/doc-html/:name` | The typeset HTML of an item's doc comment (same-origin iframe source); 404 if absent |
| `/fonts/:file` | A bundled doc font (OTF); 404 on unknown name |

The layout features a left sidebar with module navigation, a top search bar, and a main content
area — inspired by docs.rs. The site uses a dark midnight-blue theme with a strong green accent.

### Rendered documentation

When the `docs.db` was built with the `render` feature of [`edlc_doc_db`](../edlc_doc_db), each
item's doc comment is typeset from Typst to HTML at build time and stored in the row's `doc_html`
column. On an item page — and in the list of [search results](#pages) — that HTML is shown in a
same-origin `<iframe>` loading `/doc-html/<qual_name>`, themed to match the site (via an injected
`<style>` block, including `@font-face` rules backed by `/fonts/...`). Each iframe is auto-sized to
its content so there is no inner scrollbar (re-measured on window resize). If an item has no
rendered HTML (rendering was skipped or the DB predates `render`), the raw `doc_text` is shown
instead (in a `<pre>` on the item page, in a `<p>` in search results). The MCP endpoints are
unchanged and always return the raw `doc_text`.

> **Caching:** `/doc-html/...` responses are served with `Cache-Control: public, max-age=300`.
> The rendered docs are static for the life of the database, and the search page re-loads many of
> them while you type, so the browser cache keeps that cheap. After rebuilding the database, do a
> hard refresh (or wait for the cache to expire) to see updated rendered documentation.

> **Rebuild note:** `doc_html` is produced at write time. If a database predates `render` (or the
> doc comments changed), rebuild it with `cargo run -p build_doc_db` before serving.

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
>
> **Note:** If `EDL_DOC_SITE` is set (e.g. by a local install, see below) it takes precedence over
> the in-tree `target/site`. When serving a freshly built in-tree bundle while such an env var is
> set, pass `--site target/site` explicitly.

### Locating the site bundle

`serve` needs the Leptos site bundle (a directory containing `pkg/` with the wasm, js, and css
assets). It resolves the site root by trying these in order and using the first one that holds
a `pkg/` subdirectory:

1. `--site <dir>` flag,
2. `EDL_DOC_SITE` environment variable,
3. `site_dir` in the `[http]` config section,
4. two directories above the executable plus `site/` — this covers the in-tree layouts
   (`target/debug/edlc_doc_server` and `target/release/edlc_doc_server` both resolve to
   `target/site`, where `cargo-leptos` writes the bundle for every profile) and the local
   install layout (`~/.edl/bin/edl_docs` resolves to `~/.edl/site`),
5. the compile-time `<workspace>/target/site` (the original behavior, last resort).

If none exists, the server exits listing every candidate it tried.

## Local install (Linux)

`install.sh` installs the doc server into `~/.edl` in the user account:

```sh
cd edlc_doc_server
sh install.sh
```

It performs a release build, then lays out:

- `~/.edl/bin/edl_docs` — the server binary,
- `~/.edl/site/` — the Leptos site bundle (where `serve` finds it via the
  executable-relative rule above),
- `~/.edl/env` — shell snippet that adds `~/.edl/bin` to `$PATH` and exports
  `EDL_DOC_SITE="$HOME/.edl/site"`; the script appends `. "$HOME/.edl/env"` to
  `~/.bashrc` and `~/.profile` (idempotently).

After restarting the shell:

```sh
edl_docs serve --db /path/to/docs.db
```

Re-running `install.sh` reinstalls over the previous installation. The MCP mode (`edl_docs mcp`)
is installed alongside and does not need the site bundle.

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
transport = "stdio"  # or "http"
port = 3000          # used when transport = "http"
bind = "127.0.0.1"   # used when transport = "http"

[http]
enabled = true
port = 8080
site_dir = "/path/to/site"  # optional; see "Locating the site bundle"
```

If `--config` is omitted, `--db` can be used to specify the database path directly (defaults to
`docs.db`). Both `mcp` and `http` are enabled by default.

## Status

This sub-crate is **LLM-generated** (Mistral Vibe) as part of the documentation-server work.
