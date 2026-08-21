//! The `example` library module.
//!
//! This is a small EDL library used to exercise the documentation generator. Its doc comment is
//! formatted as Typst source, like all EDL doc comments.

mod types;
mod child;

/// Adds two 32-bit floating-point numbers.
fn add(a: f32, b: f32) -> f32 {
    a + b
}

/// The ratio of a circle's circumference to its diameter.
let pi: f32 = 3.1415;

/// The number of entries in the example dataset.
const DATASET_SIZE: usize = 10;
