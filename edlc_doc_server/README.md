# edlc_doc_server

MCP and HTTP server for EDL documentation.

`edlc_doc_server` serves a `docs.db` produced by [`edlc_doc_db`](../edlc_doc_db) to clients over
the Model Context Protocol (MCP) and, eventually, HTTP. It does not depend on the EDL compiler
(`edlc_core`) — it only reads the pre-built database.

## MCP mode (Phase 2 — implemented)

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

### Configuration

A TOML config file can be provided with `--config`:

```sh
edlc_doc_server mcp --config server.toml
```

Example config:

```toml
db_path = "docs.db"

[mcp]
enabled = true

[http]
enabled = false
port = 8080
```

If `--config` is omitted, `--db` can be used to specify the database path directly (defaults to
`docs.db`).

## HTTP mode (Phase 3 — not yet implemented)

The `serve` subcommand will serve a web frontend backed by the database. The config schema
already supports `http.enabled` and `http.port`.

## Status

This sub-crate is **LLM-generated** (Mistral Vibe) as part of the documentation-server work.
