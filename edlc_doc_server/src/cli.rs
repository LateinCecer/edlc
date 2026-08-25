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
//! CLI parsing and dispatch for both MCP and HTTP (Leptos+axum) modes.

use std::process::ExitCode;
use std::sync::{Arc, Mutex};

use clap::{Parser, Subcommand};
use edlc_doc_db::DocDb;
use edlc_doc_server::config::ServerConfig;
use edlc_doc_server::server::DbHandle;

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
    /// Run as an HTTP server with Leptos + axum.
    Serve {
        /// Path to the SQLite documentation database.
        #[arg(long)]
        db: Option<String>,

        /// Path to a TOML config file. If present, overrides `--db`.
        #[arg(long)]
        config: Option<String>,
    },
}

pub fn run() -> ExitCode {
    let cli = Cli::parse();
    let result = match cli.command {
        Command::Mcp { db, config } => run_mcp(db, config),
        Command::Serve { db, config } => run_serve(db, config),
    };
    if result.is_err() {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn load_config(
    db_path: Option<String>,
    config_path: Option<String>,
) -> Result<ServerConfig, ()> {
    match config_path {
        Some(path) => {
            let path = std::path::Path::new(&path);
            ServerConfig::load(path).map_err(|e| {
                eprintln!("error loading config: {e}");
            })
        }
        None => {
            let db = db_path
                .map(std::path::PathBuf::from)
                .unwrap_or_else(|| std::path::PathBuf::from("docs.db"));
            Ok(ServerConfig {
                db_path: db,
                ..Default::default()
            })
        }
    }
}

fn open_db(config: &ServerConfig) -> Result<DocDb, ()> {
    eprintln!("opening database: {}", config.db_path.display());
    DocDb::open_readonly(&config.db_path).map_err(|e| {
        eprintln!("error opening database: {e}");
    })
}

fn run_mcp(db_path: Option<String>, config_path: Option<String>) -> Result<(), ()> {
    let config = load_config(db_path, config_path)?;

    if !config.mcp.enabled {
        eprintln!("error: MCP server is disabled in config");
        return Err(());
    }

    let db = open_db(&config)?;

    eprintln!("starting MCP server on stdio...");
    let runtime = tokio::runtime::Runtime::new().map_err(|e| {
        eprintln!("error creating tokio runtime: {e}");
    })?;

    runtime.block_on(edlc_doc_server::mcp::serve_stdio(db)).map_err(|e| {
        eprintln!("MCP server error: {e}");
    })
}

fn run_serve(db_path: Option<String>, config_path: Option<String>) -> Result<(), ()> {
    let config = load_config(db_path, config_path)?;

    if !config.http.enabled {
        eprintln!("error: HTTP server is disabled in config. Set [http] enabled = true in your config, or use --db to override.");
        return Err(());
    }

    let db = open_db(&config)?;
    let db_handle: DbHandle = Arc::new(Mutex::new(db));

    let port = config.http.port;
    let addr = format!("127.0.0.1:{port}");

    eprintln!("starting HTTP server on http://{addr}...");

    let runtime = tokio::runtime::Runtime::new().map_err(|e| {
        eprintln!("error creating tokio runtime: {e}");
    })?;

    runtime.block_on(async {
        use axum::Router;
        use edlc_doc_server::app::*;
        use leptos::prelude::*;
        use leptos_axum::{generate_route_list, LeptosRoutes};

        // cargo-leptos puts the site output at <workspace-root>/target/site/.
        // We resolve it relative to the crate's manifest dir (CARGO_MANIFEST_DIR),
        // which is set at compile time and points to edlc_doc_server/ within the
        // workspace — so the parent is the workspace root.
        let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let site_root = manifest_dir
            .parent()
            .unwrap_or(&manifest_dir)
            .join("target/site");
        let leptos_opts = LeptosOptions::builder()
            .site_addr(addr.parse::<std::net::SocketAddr>().unwrap())
            .output_name("edlc_doc_server".to_string())
            .site_root(site_root.to_string_lossy().to_string())
            .site_pkg_dir("pkg")
            .build();

        let routes = generate_route_list(App);

        let app = Router::new()
            .leptos_routes_with_context(
                &leptos_opts,
                routes,
                {
                    let db_handle = db_handle.clone();
                    move || {
                        leptos::context::provide_context(db_handle.clone());
                    }
                },
                {
                    let leptos_opts = leptos_opts.clone();
                    move || shell(leptos_opts.clone())
                },
            )
            .fallback(leptos_axum::file_and_error_handler(shell))
            .with_state(leptos_opts);

        let listener = tokio::net::TcpListener::bind(&addr).await.unwrap();
        axum::serve(listener, app.into_make_service())
            .await
            .map_err(|e| {
                eprintln!("HTTP server error: {e}");
            })
    }).map_err(|_| ())?;

    Ok(())
}
