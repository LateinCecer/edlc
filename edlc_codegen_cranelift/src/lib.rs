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

// #![feature(core_intrinsics)]

extern crate core;

pub mod compiler;
pub mod codegen;
mod error;
pub mod prelude;
mod executor;

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
        .level(log::LevelFilter::Warn)
        .chain(std::io::stdout())
        .chain(fern::log_file("debug_log.log")?)
        .apply()?;
    Ok(())
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
