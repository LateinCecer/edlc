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
//! Binary entry point for the EDL documentation server.
//!
//! Supports two modes:
//! - `mcp` — MCP server over stdio (for LLM clients)
//! - `serve` — HTTP server with Leptos + axum (web frontend)
//!
//! ## Usage
//!
//! ```sh
//! # MCP mode
//! edlc_doc_server mcp --db docs.db
//!
//! # HTTP mode
//! edlc_doc_server serve --db docs.db
//!
//! # With TOML config
//! edlc_doc_server serve --config server.toml
//! ```

#[cfg(feature = "ssr")]
mod cli;

#[cfg(feature = "ssr")]
fn main() -> std::process::ExitCode {
    cli::run()
}

#[cfg(not(feature = "ssr"))]
fn main() {
    // Client-side main is handled by `hydrate()` in lib.rs.
}
