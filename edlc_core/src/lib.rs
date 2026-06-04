/*
 *     EDLc, a compiler for the EDL programming language.
 *     Copyright (C) 2026  Adrian Paskert
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#![allow(unused_parens)]
#![allow(dead_code)]
pub mod lexer;
pub mod parser;
pub mod resolver;
mod ast;
mod core;
mod hir;
mod mir;
mod compiler;
pub mod prelude;
mod file;
mod issue;
mod documentation;
mod report;

#[cfg(test)]
fn setup_logger() -> Result<(), fern::InitError> {
    use fern::colors::*;

    let colors = ColoredLevelConfig::new()
        .info(Color::Green)
        .warn(Color::Yellow)
        .error(Color::Red)
        .debug(Color::BrightCyan);

    fern::Dispatch::new()
        .format(move |out, message, record| {
            use std::time::SystemTime;

            out.finish(format_args!("[{} {} {}] {}",
                humantime::format_rfc3339_seconds(SystemTime::now()),
                colors.color(record.level()),
                record.target(),
                message,
            ))
        })
        .level(log::LevelFilter::Info)
        .chain(std::io::stdout())
        .chain(fern::log_file("debug_log.log")?)
        .apply()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use log::{debug, info, warn};
    use super::*;

    #[test]
    fn it_works() {
        let _ = setup_logger();
        info!("Hello, world!");
        warn!("Warning!");
        debug!("Now, existing.");
    }
}
