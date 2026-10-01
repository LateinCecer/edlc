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
//! The vendored font files used for Typst doc-comment rendering.
//!
//! New Computer Modern (Typst's default text face) and New CMMath (its math
//! face), vendored in `assets/fonts/` so that rendered documents are
//! deterministic across machines and the web frontend can serve the same
//! faces to browsers (via `@font-face`) — the HTML export does not embed
//! fonts, so the browser must be given the very fonts the renderer laid out
//! with. See `assets/fonts/NOTICE` for the font licenses.

/// The vendored font files as `(filename, bytes)` pairs.
pub static FONTS: &[(&str, &[u8])] = &[
    ("NewCM10-Regular.otf", include_bytes!("../assets/fonts/NewCM10-Regular.otf")),
    ("NewCM10-Bold.otf", include_bytes!("../assets/fonts/NewCM10-Bold.otf")),
    ("NewCM10-Italic.otf", include_bytes!("../assets/fonts/NewCM10-Italic.otf")),
    ("NewCM10-BoldItalic.otf", include_bytes!("../assets/fonts/NewCM10-BoldItalic.otf")),
    ("NewCMMath-Regular.otf", include_bytes!("../assets/fonts/NewCMMath-Regular.otf")),
    ("NewCMMath-Book.otf", include_bytes!("../assets/fonts/NewCMMath-Book.otf")),
    ("NewCMMath-Bold.otf", include_bytes!("../assets/fonts/NewCMMath-Bold.otf")),
];
