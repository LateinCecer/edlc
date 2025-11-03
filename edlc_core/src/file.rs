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
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::{fs, io};
use std::fmt::{Debug, Formatter};
use crate::lexer::SrcPos;
use crate::parser::Parser;

/// The `SrcSupplier` trait is an abstraction for any source code management data structure to load
/// raw sources as strings.
/// This should be used over file system-dependent methods of loading source code, as this
/// makes it easy to implement alternative source code sources.
/// An example for this would be a package manager that supplies sources over network in cases
/// where there is not a lot of disk space available.
pub trait SrcSupplier {
    /// Loads the contents of a src file as a string.
    fn load_src<S: AsRef<Path>>(&self, path: &[S]) -> Result<ModuleSrc, io::Error>;
}

/// A local file is a file in the **Rust** source code and **not** in the EDL code base.
/// This struct is used to get information where in the Rust code base raw EDL source code is
/// injected in to the compiler, in case that EDL source code causes errors or warnings.
#[derive(PartialEq, PartialOrd)]
pub struct LocalFileSource {
    pub relative_path: PathBuf,
    pub pos: SrcPos,
    pub src: String,
}

#[derive(PartialEq, PartialOrd)]
pub struct BufferedFile {
    pub path: PathBuf,
    pub buffer: String,
}

impl Debug for BufferedFile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "BufferedFile {}, buffer size: {} bytes", self.path.display(), self.buffer.as_bytes().len())
    }
}

impl Debug for LocalFileSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "LocalFileSource {}:{}, ", self.relative_path.display(), self.pos, )
    }
}

/// Contains the source files of a module.
/// A module source code can be expressed in two different ways; either as a path to a local source
/// file which can be read to obtain the raw source code, or as the raw source code itself.
/// The last case should only be chosen if the source code is not present as a file in it's raw
/// form.
/// This is usually the case when raw source code is loaded into EDL from the controlling Rust
/// source code.
///
/// # Struct Size
///
/// On a 64-bit system, this struct should have the same size as an `Arc`, or, in other words, one
/// pointer size.
/// Since the structure is also thread save and its contents to not have to be `mut`, this means
/// that we can attach the `ModuleSrc` to any file src position without fear of bricking the
/// performance or memory consumption of the program.
#[derive(Clone, Debug, PartialEq)]
pub enum ModuleSrc {
    File(Arc<BufferedFile>),
    String(Arc<LocalFileSource>),
}

#[macro_export]
macro_rules! inline_code {
    ($src:expr) => (
        $crate::prelude::ModuleSrc::inline(
            $src.to_string(),
            std::path::PathBuf::from(std::file!()),
            std::line!() as usize,
            std::column!() as usize,
        )
    )
}

impl ModuleSrc {
    pub fn inline(src: String, relative_path: PathBuf, line: usize, col: usize) -> Self {
        ModuleSrc::String(Arc::new(LocalFileSource {
            relative_path,
            src,
            pos: SrcPos::new(line, col, 1),
        }))
    }

    /// Formats the location of the module source in a human readable, but compact way.
    pub fn format_location(&self) -> String {
        match self {
            ModuleSrc::File(file) => {
                file.as_ref().path.clone().into_os_string().into_string().unwrap()
            },
            ModuleSrc::String(local_src) => {
                format!("INJECTED@ {}", local_src.relative_path.clone().into_os_string().into_string().unwrap())
            }
        }
    }

    pub fn format_pos(&self, pos: SrcPos) -> String {
        match self {
            ModuleSrc::File(file) => {
                let path = file.as_ref().path.clone().into_os_string().into_string().unwrap();
                format!("{path}:{}:{}", pos.line + 1, pos.col + 1)
            },
            ModuleSrc::String(local_src) => {
                let path = local_src.relative_path.clone().into_os_string().into_string().unwrap();
                let col = if pos.line == 0 { pos.col + local_src.pos.col } else { pos.col + 1 };
                let line = pos.line + local_src.pos.line;
                format!("{path}:{line}:{col}")
            }
        }
    }

    /// Returns the source code as a string.
    /// Since the source file might just be stored as a file path, there might be some I/O read
    /// accesses facilitated through this method.
    pub fn get_src(&self) -> Result<&String, io::Error> {
        match self {
            ModuleSrc::File(file) => {
                // these I/O operations should not fail, as, if this point is reached, the files
                // must have already been read with this exact path.
                // Assuming that the files have not been modified while the compiler was running,
                // we can assume that there should be no error happening here other than stuff
                // that is out of our control.
                // let mut file = File::open(file.as_ref())?;
                // let mut output = String::new();
                // file.read_to_string(&mut output)?;
                Ok(&file.as_ref().buffer)
            },
            ModuleSrc::String(src) => {
                Ok(&src.src)
            }
        }
    }
}

pub struct FileSupplier<'a> {
    working_dir: &'a Path,
    file_extension: &'static str,
}

impl<'a> FileSupplier<'a> {
    /// Creates a new file supplier with the path to the base directory as the basis.
    pub fn new(path: &'a Path) -> Option<Self> {
        if path.is_dir() {
            Some(FileSupplier {
                working_dir: path,
                file_extension: "eq",
            })
        } else {
            None
        }
    }

    /// Creates a new file supplier with a custom file extension.
    /// The path to the base directory is supplied as the `path` parameter.
    ///
    /// # File Extensions
    ///
    /// To create the final file extension in the fully formulated source file paths,
    /// `std::path::Path::set_extension` is used.
    /// Following the spec of this function, the file extension should be presented to this
    /// method **without** the leading `dot`, as to make the file extension formulation OS
    /// independent.
    pub fn with_file_ext(path: &'a Path, file_ext: &'static str) -> Option<Self> {
        if path.is_dir() {
            Some(FileSupplier {
                working_dir: path,
                file_extension: file_ext,
            })
        } else {
            None
        }
    }

    /// Loads a path from the file supplier and opens the file requested file if it exists.
    ///
    /// # Behaviour
    ///
    /// When the specified path points to a directory and not a file, this method will also
    /// return `None`, just like when the file does not exist in the first place.
    /// This is because it can be expected that source files are always files, not dirs.
    ///
    /// # Example
    ///
    /// The following example could be used to load a file which is located at position
    /// `src/test/path/to/file.eq` relative to the execution directory of the program.
    ///
    /// ```
    /// # use std::io;
    /// # use edlc_core::prelude::FileSupplier;
    /// # use std::path::Path;
    /// # fn main() -> Result<(), io::Error> {
    /// let file_supplier = FileSupplier::new(Path::new("src/test")).unwrap();
    /// let file = file_supplier.load_child(&["path", "to", "file.eq"])?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn load_child<S: AsRef<Path>>(&self, path: &[S]) -> Result<ModuleSrc, io::Error> {
        let mut buf = PathBuf::new();
        buf.push(self.working_dir);

        let mut iter = path.iter();
        iter.next();
        for name in iter {
            buf.push(name);
        }
        buf.set_extension(self.file_extension);

        if buf.is_file() {
            let file_content = fs::read_to_string(&buf)?;
            Ok(ModuleSrc::File(Arc::new(BufferedFile {
                path: buf,
                buffer: file_content,
            })))
        } else {
            Err(io::Error::new(ErrorKind::NotFound, format!(
                "failed to find file `{}` in source tree",
                buf.into_os_string().into_string().unwrap()
            )))
        }
    }
}

impl SrcSupplier for FileSupplier<'_> {
    fn load_src<S: AsRef<Path>>(&self, path: &[S]) -> Result<ModuleSrc, io::Error> {
        self.load_child(path)
    }
}

pub trait ParserSupplier {
    /// Creates a parser using the resources from the parser supplier and the specified source.
    /// The parser supplier must stay mutably borrowed for the during in which the parser is active,
    /// since the parser borrows its resources.
    ///
    /// # Example usage
    ///
    /// The parser supplier is usually used in conjunction with a file supplier to parse hierarchies
    /// of source files:
    ///
    /// ```rust
    /// use std::io::Read;
    /// use edlc_core::parser::Parsable;
    /// use edlc_core::prelude::{AstModule, FileSupplier, ParserSupplier};
    /// fn parse_hierarchy<P: ParserSupplier>(files: &FileSupplier, sub: &mut P) -> Result<(), ()> {
    ///     // ...
    ///     let mut file = files.load_child(&["path", "to", "module"]).unwrap();
    ///     let mut src = file.get_src().unwrap().clone();
    ///     let mut parser = sub.create_parser(&src, file);
    ///     let ast = AstModule::parse(&mut parser, vec!["module"].into(), r#"
    /// Test Module
    /// ===
    /// 
    /// This is a test module that exists for testing purposes only.
    /// "#.to_string()).unwrap();
    ///     // ...
    ///     Ok(())
    /// }
    /// ```
    fn create_parser<'a>(&mut self, src: &'a str, module_src: ModuleSrc) -> Parser<'a, '_>;

    fn create_parser_with_pos<'a>(&mut self, src: &'a str, pos: SrcPos, module_src: ModuleSrc) -> Parser<'a, '_>;
}

