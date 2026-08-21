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

/// Dispatches a gradient calculation asynchronously.
///
/// The `shared` modifier on `field` marks it as read-only shared data; `async dst` marks the
/// destination as an asynchronously-written handle.
async fn gradient(shared field: f32, async dst: f32) {
    let _ = field + dst;
}

/// Initializes a field asynchronously.
async fn init_field(async field: f32) {
    let _ = field;
}

/// Wraps a value into an async handle.
///
/// This function has an `async` return type, meaning the result is delivered asynchronously
/// rather than synchronously.
fn wrap_async(x: f32) -> async f32 {
    x
}

/// The ratio of a circle's circumference to its diameter.
let pi: f32 = 3.1415;

/// The number of entries in the example dataset.
const DATASET_SIZE: usize = 10;
