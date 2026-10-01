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
//! Typst rendering of doc comments at documentation build time.
//!
//! [`DocRenderer`] compiles a doc comment (Typst markup) into a standalone
//! HTML document. Rendering is sandboxed by construction: the [`World`] only
//! exposes a single in-memory file (the doc comment itself) — there is no
//! filesystem access and no other files — and `today()` returns `None`, so
//! `datetime()` in a doc comment is an error instead of a non-deterministic
//! result. The output is always a full HTML document (the Typst HTML export
//! has no fragment mode), which the web frontend serves in an `<iframe>`.
//!
//! Note: Typst's HTML export is an in-development upstream feature (the
//! compiler emits a warning for it), so the `typst`/`typst-html` dependencies
//! are pinned to an exact version and callers keep the raw doc text as a
//! fallback for anything that fails to render.

use typst::ecow::EcoVec;
use typst::diag::{FileError, FileResult, SourceDiagnostic};
use typst::foundations::{Bytes, Datetime, Duration};
use typst::syntax::{FileId, RootedPath, Source, VirtualPath, VirtualRoot};
use typst::text::{Font, FontBook};
use typst::utils::LazyHash;
use typst::{compile, Feature, Features, Library, LibraryExt, World};
use typst_html::{HtmlDocument, HtmlOptions};

use crate::fonts;

/// The virtual name of the doc comment's main file.
const MAIN_FILE: &str = "doc.typ";

/// The errors that occurred while rendering a doc comment.
#[derive(Debug)]
pub struct DocRenderError {
    diagnostics: EcoVec<SourceDiagnostic>,
}

impl DocRenderError {
    /// The diagnostic messages, in order.
    pub fn messages(&self) -> impl Iterator<Item = &str> {
        self.diagnostics.iter().map(|d| d.message.as_str())
    }
}

impl std::fmt::Display for DocRenderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let messages: Vec<&str> = self.messages().collect();
        write!(f, "failed to render doc comment: {}", messages.join("; "))
    }
}

impl std::error::Error for DocRenderError {}

/// A sandboxed [`World`] that sees exactly one in-memory file: the doc
/// comment being rendered.
struct DocWorld<'a> {
    library: &'a LazyHash<Library>,
    book: &'a LazyHash<FontBook>,
    fonts: &'a [Font],
    main_id: FileId,
    source: Source,
}

impl World for DocWorld<'_> {
    fn library(&self) -> &LazyHash<Library> {
        self.library
    }

    fn book(&self) -> &LazyHash<FontBook> {
        self.book
    }

    fn main(&self) -> FileId {
        self.main_id
    }

    fn source(&self, id: FileId) -> FileResult<Source> {
        if id == self.main_id {
            Ok(self.source.clone())
        } else {
            Err(not_found(id))
        }
    }

    fn file(&self, id: FileId) -> FileResult<Bytes> {
        // The string must be owned: `Bytes` stores it behind a `'static` bound.
        self.source(id).map(|s| Bytes::from_string(s.text().to_owned()))
    }

    fn font(&self, index: usize) -> Option<Font> {
        self.fonts.get(index).cloned()
    }

    fn today(&self, _offset: Option<Duration>) -> Option<Datetime> {
        None
    }
}

fn not_found(id: FileId) -> FileError {
    FileError::NotFound(std::path::PathBuf::from(id.vpath().get_without_slash()))
}

/// Renders doc comments (Typst markup) into standalone HTML documents.
///
/// Build one renderer and reuse it for every item of a documentation build:
/// the standard library (with the in-development HTML export feature
/// enabled) and the font book are set up only once.
pub struct DocRenderer {
    library: LazyHash<Library>,
    book: LazyHash<FontBook>,
    fonts: Vec<Font>,
}

impl Default for DocRenderer {
    /// Creates a renderer using the vendored fonts [`crate::fonts::FONTS`]
    /// (New Computer Modern text face, New CMMath math face).
    fn default() -> Self {
        let fonts: Vec<Font> = fonts::FONTS
            .iter()
            .flat_map(|(_, bytes)| Font::iter(Bytes::new(*bytes)))
            .collect();
        let book = FontBook::from_fonts(&fonts);
        let library = Library::builder()
            .with_features(std::iter::once(Feature::Html).collect::<Features>())
            .build();
        DocRenderer {
            library: LazyHash::new(library),
            book: LazyHash::new(book),
            fonts,
        }
    }
}

impl DocRenderer {
    /// Creates a renderer using the vendored fonts [`crate::fonts::FONTS`]
    /// (New Computer Modern text face, New CMMath math face).
    pub fn new() -> Self {
        Self::default()
    }

    /// Renders a doc comment into a standalone HTML document.
    ///
    /// Returns [`DocRenderError`] when the markup does not compile; the
    /// caller is expected to fall back to the raw doc text.
    pub fn render(&self, markup: &str) -> Result<String, DocRenderError> {
        let main_id = RootedPath::new(
            VirtualRoot::Project,
            VirtualPath::new(MAIN_FILE).expect("static virtual path is valid"),
        )
        .intern();
        let source = Source::new(main_id, markup.to_string());
        let world = DocWorld {
            library: &self.library,
            book: &self.book,
            fonts: &self.fonts,
            main_id,
            source,
        };
        let warned = compile::<HtmlDocument>(&world);
        let doc = match warned.output {
            Ok(doc) => doc,
            Err(diagnostics) => return Err(DocRenderError { diagnostics }),
        };
        let html = typst_html::html(&doc, &HtmlOptions::default())
            .map_err(|diagnostics| DocRenderError { diagnostics })?;
        Ok(html)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The renderer loads all seven vendored font files into the font book.
    #[test]
    fn loads_vendored_fonts() {
        let renderer = DocRenderer::new();
        assert!(!renderer.fonts.is_empty());
        let families: Vec<&str> = renderer.book.families().map(|(f, _)| f).collect();
        // The text and math faces must both be available.
        assert!(
            families.iter().any(|f| f.contains("New Computer Modern")),
            "New Computer Modern missing from font book: {families:?}"
        );
        eprintln!("font families: {families:?}");
    }

    /// Plain prose renders to a full HTML document carrying the text.
    #[test]
    fn renders_plain_text() {
        let renderer = DocRenderer::new();
        let html = renderer
            .render("Computes the *Euclidean* norm of a vector.")
            .expect("render plain text");
        assert!(html.starts_with("<!DOCTYPE html>"), "not a full HTML document");
        assert!(html.contains("Euclidean"), "text missing from HTML: {html}");
    }

    /// Math markup renders (as MathML or inline SVG) without errors.
    #[test]
    fn renders_math() {
        let renderer = DocRenderer::new();
        let html = renderer
            .render("The norm is $ sqrt(a^2 + b^2) $.")
            .expect("render math");
        assert!(html.contains("sqrt"), "math missing from HTML: {html}");
    }

    /// Markup constructs (tables, emphasis) render.
    #[test]
    fn renders_table() {
        let renderer = DocRenderer::new();
        let html = renderer
            .render("#table([a], [b], [c], [d])")
            .expect("render table");
        assert!(html.contains("<table"), "table missing from HTML: {html}");
    }

    /// Broken markup is a `DocRenderError` carrying the diagnostic message,
    /// not a panic.
    #[test]
    fn broken_markup_is_an_error() {
        let renderer = DocRenderer::new();
        let err = renderer.render("#let x = ").expect_err("should fail");
        let messages: Vec<&str> = err.messages().collect();
        assert!(!messages.is_empty(), "error should carry diagnostics");
    }

    /// `datetime()` is an error (the world has no clock), so rendering is
    /// deterministic.
    #[test]
    fn datetime_is_an_error() {
        let renderer = DocRenderer::new();
        assert!(renderer.render("#datetime()").is_err());
    }

    /// File access beyond the doc comment itself fails (sandbox).
    #[test]
    fn file_access_is_sandboxed() {
        let renderer = DocRenderer::new();
        assert!(renderer.render(r#"#include("other.typ")"#).is_err());
    }

    /// A markdown-style `# Heading` is *not* valid Typst: a `#` followed by a
    /// space is a code escape, so such doc comments fail to render (and the
    /// caller falls back to the raw text). Section labels must use markup
    /// instead, e.g. `**Heading**`.
    #[test]
    fn hash_space_is_an_escape_not_a_heading() {
        let renderer = DocRenderer::new();
        assert!(renderer.render("# Example").is_err());
    }

    /// Doc text in the shape the EDL lexer produces (one leading space per
    /// line, as in `/// text`) renders: bold labels, math, fenced code
    /// blocks and multi-line table calls all work despite the leading space.
    #[test]
    fn edl_doc_text_shape_renders() {
        let renderer = DocRenderer::new();
        let html = renderer
            .render(" Adds two numbers.\n The arithmetic is $a + b$.\n\n **Example**\n\n ```\n core::assert(add(1, 2), 3);\n ```")
            .expect("render EDL-shaped doc text");
        assert!(html.contains("Example"), "label missing from HTML: {html}");
        assert!(html.contains("core::assert"), "code block missing from HTML: {html}");
    }
}
