//! This is the very important `foo` function, which implements the `foo` algorithm based on the ISO-foo-1234 standard.
mod boa;


/// Implements the function foo, which should be visible from everywhere that can reach the base module.
///
/// # Parameters
///
/// There is no formal description pattern for the documentation of functions / methods in EDL.
/// However, there are some canonical patterns that can be followed to get some level of coherency.
/// One of these is the `# Parameters` section, which _can_ be used to elaborate on the function parameters.
fn bar(
    /// First parameter
    a: f64,
    /// Second parameter
    b: f64,
) -> f64 {
    f64::max(a, b) + b
}

let pi: f32 = 3.1415;
const TEST: usize = 10;

fn print_pi() {
    test_lib::println(pi);
}
