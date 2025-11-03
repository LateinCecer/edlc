//! This is a library

/// Prints a message
fn println<T>(msg: T) {
    std::io::print(msg);
    std::io::print("\n");
}
