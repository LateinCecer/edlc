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
//! EDL documentation server — serves a `docs.db` over MCP (stdio) and, eventually, HTTP.
//!
//! ## Usage
//!
//! Run as an MCP server over stdio (for use with Claude Desktop, Cursor, etc.):
//!
//! ```sh
//! edlc_doc_server mcp --db docs.db
//! ```
//!
//! The server can also read a TOML config file with `--config`:
//!
//! ```sh
//! edlc_doc_server mcp --config server.toml
//! ```
//!
//! Example config:
//!
//! ```toml
//! db_path = "docs.db"
//!
//! [mcp]
//! enabled = true
//!
//! [http]
//! enabled = false
//! port = 8080
//! ```

mod config;
mod mcp;

use std::process::ExitCode;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "edlc_doc_server", about = "Serve EDL documentation over MCP and HTTP")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Run as an MCP server over stdio.
    Mcp {
        /// Path to the SQLite documentation database.
        #[arg(long)]
        db: Option<String>,

        /// Path to a TOML config file. If present, overrides `--db`.
        #[arg(long)]
        config: Option<String>,
    },
    /// Run as an HTTP server (Phase 3 — not yet implemented).
    Serve {
        /// Path to a TOML config file.
        #[arg(long)]
        config: Option<String>,
    },
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    let result = match cli.command {
        Command::Mcp { db, config } => run_mcp(db, config),
        Command::Serve { config: _ } => {
            eprintln!("error: HTTP server is not yet implemented (Phase 3)");
            Err(())
        }
    };
    if result.is_err() {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn run_mcp(db_path: Option<String>, config_path: Option<String>) -> Result<(), ()> {
    let config = load_config(db_path, config_path)?;

    if !config.mcp.enabled {
        eprintln!("error: MCP server is disabled in config");
        return Err(());
    }

    eprintln!("opening database: {}", config.db_path.display());
    let db = edlc_doc_db::DocDb::open_readonly(&config.db_path).map_err(|e| {
        eprintln!("error opening database: {e}");
    })?;

    eprintln!("starting MCP server on stdio...");
    let runtime = tokio::runtime::Runtime::new().map_err(|e| {
        eprintln!("error creating tokio runtime: {e}");
    })?;

    runtime.block_on(mcp::serve_stdio(db)).map_err(|e| {
        eprintln!("MCP server error: {e}");
    })
}

fn load_config(
    db_path: Option<String>,
    config_path: Option<String>,
) -> Result<config::ServerConfig, ()> {
    match config_path {
        Some(path) => {
            let path = std::path::Path::new(&path);
            config::ServerConfig::load(path).map_err(|e| {
                eprintln!("error loading config: {e}");
            })
        }
        None => {
            let db = db_path
                .map(std::path::PathBuf::from)
                .unwrap_or_else(|| std::path::PathBuf::from("docs.db"));
            Ok(config::ServerConfig {
                db_path: db,
                ..Default::default()
            })
        }
    }
}
