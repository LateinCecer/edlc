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
//! TOML configuration for the documentation server.

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Top-level server configuration, parsed from a TOML file.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerConfig {
    /// Path to the SQLite documentation database.
    pub db_path: PathBuf,

    /// MCP server configuration. When present, the server exposes MCP tools over stdio.
    #[serde(default)]
    pub mcp: McpConfig,

    /// HTTP server configuration. When enabled, the server serves a web frontend.
    /// (Phase 3 — not yet implemented.)
    #[serde(default)]
    pub http: HttpConfig,
}

/// MCP-specific configuration.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct McpConfig {
    /// Whether the MCP server is enabled. Default: `true`.
    #[serde(default = "default_true")]
    pub enabled: bool,
}

/// HTTP-specific configuration. (Phase 3 — not yet implemented.)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct HttpConfig {
    /// Whether the HTTP server is enabled. Default: `false`.
    #[serde(default)]
    pub enabled: bool,

    /// The port to listen on. Default: `8080`.
    #[serde(default = "default_http_port")]
    pub port: u16,
}

fn default_true() -> bool {
    true
}

fn default_http_port() -> u16 {
    8080
}

impl Default for ServerConfig {
    fn default() -> Self {
        ServerConfig {
            db_path: PathBuf::from("docs.db"),
            mcp: McpConfig { enabled: true },
            http: HttpConfig::default(),
        }
    }
}

impl ServerConfig {
    /// Loads a configuration from a TOML file. If the path does not exist, returns the default
    /// config with `db_path` set to the given path.
    pub fn load(path: &std::path::Path) -> Result<Self, ConfigError> {
        if !path.exists() {
            return Ok(ServerConfig {
                db_path: path.to_path_buf(),
                ..Default::default()
            });
        }
        let content = std::fs::read_to_string(path)?;
        let config: ServerConfig = toml::from_str(&content)?;
        Ok(config)
    }
}

/// Errors that can occur while loading configuration.
#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error("failed to read config file: {0}")]
    Io(#[from] std::io::Error),
    #[error("failed to parse config file: {0}")]
    Parse(#[from] toml::de::Error),
}
