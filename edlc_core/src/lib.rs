/*
 *    Copyright 2025 Adrian Paskert
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
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
