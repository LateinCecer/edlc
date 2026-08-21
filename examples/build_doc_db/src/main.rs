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
//! Example implementor: compiles a bundled EDL project and emits a `docs.db`.
//!
//! This binary wires `edlc_core` to `edlc_doc_db`. It is the reference for how an implementor
//! populates the documentation database: load sources via `FileSupplier`, drive a compile with
//! `EdlCompiler`, then call `generate_docs(&mut DocDbWriter)`.

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use edlc_codegen_cranelift::prelude::CraneliftJIT;
use edlc_core::prelude::FileSupplier;
use edlc_doc_db::DocDbWriter;

fn main() -> ExitCode {
    let out_path = PathBuf::from("docs.db");
    match run(&out_path) {
        Ok(()) => {
            println!("wrote {}", out_path.display());
            ExitCode::SUCCESS
        }
        Err(err) => {
            eprintln!("error: {err}");
            ExitCode::FAILURE
        }
    }
}

fn run(out_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    // Using the cranelift JIT backend (rather than a bare `EdlCompiler`) makes the compiler
    // register the EDL standard library — intrinsics, trait impls like `core::Add`, `std::io`,
    // etc. — so the example project can use realistic EDL code (operators, std calls, loops).
    // Doc generation itself needs no codegen; we only use the backend to populate the compiler
    // state, then read it back via `generate_docs`.
    let mut jit = CraneliftJIT::<()>::default();
    jit.init()?;

    let project_dir = Path::new("project");
    let supplier = FileSupplier::new(project_dir)
        .ok_or_else(|| format!("project directory not found: {}", project_dir.display()))?;
    jit.compile_lib("example", &supplier)?;

    let mut writer = DocDbWriter::open(out_path)?;
    jit.compiler.generate_docs(&mut writer);
    writer.finish()?;
    Ok(())
}
