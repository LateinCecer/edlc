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
#[command(
    name = "edlc_doc_server",
    about = "Serve EDL documentation over MCP and HTTP"
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Run as an MCP server: over stdio (default) or Streamable HTTP on a TCP socket
    /// (`--transport http`).
    Mcp {
        /// Path to the SQLite documentation database.
        #[arg(long)]
        db: Option<String>,

        /// Path to a TOML config file. If present, overrides `--db`.
        #[arg(long)]
        config: Option<String>,

        /// The MCP transport: `stdio` or `http` (Streamable HTTP on a TCP socket).
        #[arg(long, value_name = "stdio|http")]
        transport: Option<String>,

        /// Port for the `http` transport (default: 3000).
        #[arg(long)]
        mcp_port: Option<u16>,

        /// Address to bind for the `http` transport (default: 127.0.0.1).
        #[arg(long)]
        mcp_bind: Option<String>,
    },
    /// Run as an HTTP server with Leptos + axum.
    Serve {
        /// Path to the SQLite documentation database.
        #[arg(long)]
        db: Option<String>,

        /// Path to a TOML config file. If present, overrides `--db`.
        #[arg(long)]
        config: Option<String>,

        /// Directory containing the built Leptos site (must hold a `pkg/`
        /// subdirectory). Takes precedence over the `EDL_DOC_SITE` environment
        /// variable, the config file, and automatic discovery.
        #[arg(long)]
        site: Option<std::path::PathBuf>,
    },
}

pub fn run() -> ExitCode {
    let cli = Cli::parse();
    let result = match cli.command {
        Command::Mcp {
            db,
            config,
            transport,
            mcp_port,
            mcp_bind,
        } => run_mcp(db, config, transport, mcp_port, mcp_bind),
        Command::Serve { db, config, site } => run_serve(db, config, site),
    };
    if result.is_err() {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn load_config(db_path: Option<String>, config_path: Option<String>) -> Result<ServerConfig, ()> {
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
    if !config.db_path.exists() {
        eprintln!(
            "error: documentation database not found: {}",
            config.db_path.display()
        );
        eprintln!("help: build it with `cargo run -p build_doc_db` (writes examples/build_doc_db/docs.db)");
        return Err(());
    }
    DocDb::open_readonly(&config.db_path).map_err(|e| {
        eprintln!("error opening database: {e}");
    })
}

fn run_mcp(
    db_path: Option<String>,
    config_path: Option<String>,
    transport: Option<String>,
    mcp_port: Option<u16>,
    mcp_bind: Option<String>,
) -> Result<(), ()> {
    let config = load_config(db_path, config_path)?;

    if !config.mcp.enabled {
        eprintln!("error: MCP server is disabled in config");
        return Err(());
    }

    // CLI flags override the config file.
    let transport = transport.unwrap_or_else(|| config.mcp.transport.clone());
    let port = mcp_port.unwrap_or(config.mcp.port);
    let bind = mcp_bind.unwrap_or_else(|| config.mcp.bind.clone());

    let db = open_db(&config)?;

    let runtime = tokio::runtime::Runtime::new().map_err(|e| {
        eprintln!("error creating tokio runtime: {e}");
    })?;

    match transport.as_str() {
        "stdio" => {
            eprintln!("starting MCP server on stdio...");
            runtime
                .block_on(edlc_doc_server::mcp::serve_stdio(db))
                .map_err(|e| {
                    eprintln!("MCP server error: {e}");
                })
        }
        "http" => {
            let addr = format!("{bind}:{port}");
            let listener = runtime
                .block_on(tokio::net::TcpListener::bind(&addr))
                .map_err(|e| {
                    eprintln!("error binding MCP HTTP transport to {addr}: {e}");
                })?;
            eprintln!("starting MCP server on http://{addr}/mcp...");
            runtime
                .block_on(edlc_doc_server::mcp::serve_http(listener, db, &bind))
                .map_err(|e| {
                    eprintln!("MCP server error: {e}");
                })
        }
        other => {
            eprintln!(
                "error: unknown MCP transport '{other}' (expected 'stdio' or 'http')"
            );
            Err(())
        }
    }
}

/// Site-root candidates in priority order:
///
/// 1. the explicit `--site` flag,
/// 2. the `EDL_DOC_SITE` environment variable,
/// 3. `site_dir` from the config file,
/// 4. the executable's grandparent directory joined with `site/` — this covers
///    every layout we support, since the site always sits two levels above the
///    executable:
///    - `target/debug/edlc_doc_server`  -> `target/site` (development),
///    - `target/release/edlc_doc_server` -> `target/site` (in-tree release run;
///      cargo-leptos writes the site to `target/site` for both profiles),
///    - `~/.edl/bin/edl_docs`           -> `~/.edl/site` (local install),
/// 5. the compile-time `CARGO_MANIFEST_DIR/../target/site` (the original
///    behavior, kept as a last resort).
///
/// The caller selects the first candidate that holds a `pkg/` subdirectory, so
/// missing directories never shadow a later, valid candidate.
fn site_candidates(
    explicit: Option<&std::path::Path>,
    env_dir: Option<&str>,
    config_dir: Option<&std::path::Path>,
    exe: Option<&std::path::Path>,
) -> Vec<std::path::PathBuf> {
    let mut candidates = Vec::new();

    if let Some(p) = explicit {
        candidates.push(p.to_path_buf());
    }
    if let Some(p) = env_dir.map(std::path::Path::new).filter(|p| !p.as_os_str().is_empty()) {
        candidates.push(p.to_path_buf());
    }
    if let Some(p) = config_dir {
        candidates.push(p.to_path_buf());
    }
    if let Some(exe) = exe {
        if let Some(exe_dir) = exe.parent() {
            if let Some(root) = exe_dir.parent() {
                candidates.push(root.join("site"));
            }
        }
    }

    // Compile-time fallback: the workspace target directory.
    let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace = manifest_dir.parent().unwrap_or(&manifest_dir);
    candidates.push(workspace.join("target/site"));

    candidates
}

/// The theme for rendered doc pages, injected into each `/doc-html/<name>` response.
const DOC_THEME_CSS: &str = include_str!("doc_html.css");

/// Injects [`DOC_THEME_CSS`] into the standalone HTML document the Typst renderer
/// emitted, right before `</head>`. The document carries no theme or font
/// references of its own, so the browser would otherwise lay it out with a white
/// background and default fonts.
fn inject_doc_theme(html: &str) -> String {
    let tag = format!("<style id=\"edlc-doc-theme\">{DOC_THEME_CSS}</style>");
    match html.find("</head>") {
        Some(idx) => {
            let mut out = String::with_capacity(html.len() + tag.len() + 16);
            out.push_str(&html[..idx]);
            out.push_str(&tag);
            out.push_str(&html[idx..]);
            out
        }
        None => format!("{tag}{html}"),
    }
}

/// Looks up an item by name and returns its rendered (themed) doc HTML, if any.
///
/// Returns `None` (→ 404) when the item does not exist or has no rendered doc
/// (e.g. the database was built without the `render` feature).
fn lookup_doc_html(
    db: &DbHandle,
    name: &str,
) -> Result<Option<String>, axum::http::StatusCode> {
    let internal = axum::http::StatusCode::INTERNAL_SERVER_ERROR;
    let guard = db.lock().map_err(|_| internal)?;
    let row = guard.get_item_by_name(name).map_err(|_| internal)?;
    Ok(row
        .filter(|r| !r.doc_html.is_empty())
        .map(|r| inject_doc_theme(&r.doc_html)))
}

/// Serves a vendored font file (from `edlc_doc_db::fonts::FONTS`) by filename,
/// or 404s on an unknown name. The fonts are immutable, so they are cached
/// aggressively by browsers.
fn serve_font(file: &str) -> Result<axum::response::Response, axum::http::StatusCode> {
    use axum::http::{header, HeaderValue};
    for (name, bytes) in edlc_doc_db::fonts::FONTS {
        if *name == file {
            let mut resp =
                axum::response::Response::new(axum::body::Body::from(bytes.to_vec()));
            resp.headers_mut()
                .insert(header::CONTENT_TYPE, HeaderValue::from_static("font/otf"));
            resp.headers_mut().insert(
                header::CACHE_CONTROL,
                HeaderValue::from_static("public, max-age=31536000, immutable"),
            );
            return Ok(resp);
        }
    }
    Err(axum::http::StatusCode::NOT_FOUND)
}

fn run_serve(
    db_path: Option<String>,
    config_path: Option<String>,
    site_path: Option<std::path::PathBuf>,
) -> Result<(), ()> {
    let config = load_config(db_path, config_path)?;

    if !config.http.enabled {
        eprintln!("error: HTTP server is disabled in config. Set [http] enabled = true in your config, or use --db to override.");
        return Err(());
    }

    let exe = std::env::current_exe().ok();
    let candidates = site_candidates(
        site_path.as_deref(),
        std::env::var("EDL_DOC_SITE").ok().as_deref(),
        config.http.site_dir.as_deref(),
        exe.as_deref(),
    );

    // The first candidate that actually holds a `pkg/` subdirectory wins.
    let site_root = match candidates.iter().find(|c| c.join("pkg").is_dir()) {
        Some(root) => root.clone(),
        None => {
            eprintln!("error: Leptos site assets not found; looked for a `pkg/` directory in:");
            for c in &candidates {
                eprintln!("  {}", c.display());
            }
            eprintln!("help: build the frontend bundle first with `cargo leptos build` (or `cargo leptos watch` for development)");
            eprintln!("help: or point the server at an existing site directory with `--site <dir>` or the EDL_DOC_SITE environment variable");
            return Err(());
        }
    };
    eprintln!("serving site assets from {}", site_root.display());

    // cargo-leptos 0.3.x renames wasm-bindgen's `edlc_doc_server_bg.wasm` to
    // `edlc_doc_server.wasm`, but the name the hydration script requests
    // depends on whether the leptos crate happened to be compiled with
    // LEPTOS_OUTPUT_NAME set (cargo reuses cached builds regardless, so both
    // outcomes occur in practice). Ensure both spellings exist so the client
    // can always load the wasm.
    let pkg_dir = site_root.join("pkg");
    let wasm_plain = pkg_dir.join("edlc_doc_server.wasm");
    let wasm_bg = pkg_dir.join("edlc_doc_server_bg.wasm");
    if wasm_plain.exists() && !wasm_bg.exists() {
        let _ = std::fs::copy(&wasm_plain, &wasm_bg);
    } else if wasm_bg.exists() && !wasm_plain.exists() {
        let _ = std::fs::copy(&wasm_bg, &wasm_plain);
    }

    let db = open_db(&config)?;
    let db_handle: DbHandle = Arc::new(Mutex::new(db));

    let port = config.http.port;
    let addr = format!("127.0.0.1:{port}");

    eprintln!("starting HTTP server on http://{addr}...");

    let runtime = tokio::runtime::Runtime::new().map_err(|e| {
        eprintln!("error creating tokio runtime: {e}");
    })?;

    runtime
        .block_on(async {
            use axum::extract::Path;
            use axum::routing::get;
            use axum::Router;
            use edlc_doc_server::app::*;
            use leptos::prelude::*;
            use leptos_axum::{generate_route_list, LeptosRoutes};

            let leptos_opts = LeptosOptions::builder()
                .site_addr(addr.parse::<std::net::SocketAddr>().unwrap())
                .output_name("edlc_doc_server".to_string())
                .site_root(site_root.to_string_lossy().to_string())
                .site_pkg_dir("pkg")
                .build();

            let routes = generate_route_list(App);

            // The doc HTML and the fonts it references are served directly (not
            // through a Leptos route): the HTML is shown in a same-origin
            // `<iframe>` on item pages and the fonts are pulled in by the
            // `@font-face` rules the theme injects into that page.
            let doc_db = db_handle.clone();

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
                .route(
                    "/doc-html/{name}",
                    get(move |Path(name): Path<String>| {
                        let db = doc_db.clone();
                        async move {
                            let html = tokio::task::spawn_blocking(move || {
                                lookup_doc_html(&db, &name)
                            })
                            .await
                            .map_err(|_| axum::http::StatusCode::INTERNAL_SERVER_ERROR)??;
                            match html {
                                Some(html) => Ok(axum::response::Html(html)),
                                None => Err(axum::http::StatusCode::NOT_FOUND),
                            }
                        }
                    }),
                )
                .route(
                    "/fonts/{file}",
                    get(|Path(file): Path<String>| async move { serve_font(&file) }),
                )
                .fallback(leptos_axum::file_and_error_handler(shell))
                .with_state(leptos_opts);

            let listener = tokio::net::TcpListener::bind(&addr).await.unwrap();
            axum::serve(listener, app.into_make_service())
                .await
                .map_err(|e| {
                    eprintln!("HTTP server error: {e}");
                })
        })
        .map_err(|_| ())?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::site_candidates;

    fn p(s: &str) -> std::path::PathBuf {
        std::path::PathBuf::from(s)
    }

    #[test]
    fn candidates_priority_order() {
        let c = site_candidates(
            Some(p("/flag/site").as_path()),
            Some("/env/site"),
            Some(p("/config/site").as_path()),
            Some(p("/home/u/.edl/bin/edl_docs").as_path()),
        );
        assert_eq!(c[0], p("/flag/site"));
        assert_eq!(c[1], p("/env/site"));
        assert_eq!(c[2], p("/config/site"));
        assert_eq!(c[3], p("/home/u/.edl/site"));
        // The last candidate is always the compile-time workspace target dir.
        assert!(c.last().unwrap().ends_with("target/site"));
        assert_eq!(c.len(), 5);
    }

    #[test]
    fn candidates_skip_empty_sources() {
        let c = site_candidates(
            None,
            Some(""),
            None,
            Some(p("/target/debug/edlc_doc_server").as_path()),
        );
        assert_eq!(c[0], p("/target/site"));
        assert!(c.last().unwrap().ends_with("target/site"));
        assert_eq!(c.len(), 2);
    }

    #[test]
    fn candidates_in_tree_debug_layout() {
        let c =
            site_candidates(None, None, None, Some(p("/repo/target/debug/edlc_doc_server").as_path()));
        assert_eq!(c[0], p("/repo/target/site"));
    }

    #[test]
    fn candidates_in_tree_release_layout() {
        // cargo-leptos writes the site to target/site for both profiles, so the
        // exe-relative candidate is the same for release.
        let c = site_candidates(
            None,
            None,
            None,
            Some(p("/repo/target/release/edlc_doc_server").as_path()),
        );
        assert_eq!(c[0], p("/repo/target/site"));
    }

    #[test]
    fn candidates_installed_layout() {
        let c = site_candidates(None, None, None, Some(p("/home/u/.edl/bin/edl_docs").as_path()));
        assert_eq!(c[0], p("/home/u/.edl/site"));
    }

    #[test]
    fn candidates_bare_exe_name() {
        // An exe path without directory components contributes no candidate;
        // only the compile-time fallback remains.
        let c = site_candidates(None, None, None, Some(p("edlc_doc_server").as_path()));
        assert!(c.last().unwrap().ends_with("target/site"));
        assert_eq!(c.len(), 1);
    }
}
