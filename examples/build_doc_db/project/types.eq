//! The `types` submodule, containing aggregate type definitions.

/// A named struct representing a 2D point $(x, y)$.
///
/// The squared length of a point is $x^2 + y^2$; the vector counterpart offers
/// #strong[norm] for the unsquared length.
type Point = struct {
    x: f32,
    y: f32,
};

/// A tuple struct wrapping a single #emph[float] value.
type Wrapper = struct(f32);

/// A zero-sized unit struct.
type Marker = struct;

/// A tagged union over several numeric representations.
///
/// The variants and their underlying storage:
///
/// #table(
///   columns: (auto, auto),
///   table.header([Variant], [Storage]),
///   [`F32`], [single-precision float],
///   [`F64`], [double-precision float],
///   [`I32`], [signed 32-bit integer],
///   [`Unit`], [zero-sized],
/// )
type Numeric = enum {
    F32 { value: f32 },
    F64 { value: f64 },
    I32 { value: i32 },
    Unit
};

/// A data buffer with an asynchronously-managed field.
type DataBuffer = struct {
    /// The raw data buffer.
    data: [u8; 64],
    /// An asynchronously-written handle to the buffer's sync state.
    async sync_state: u32,
};

/// A generic fixed-size vector of $t$ holding $n$ elements.
///
/// The storage is a flat array: $vec = (v_1, v_2, ..., v_n)$.
type SVector<T, const N: usize>
where T: f32 | f64 = struct {
    data: [T; N],
};

impl<const N: usize> SVector<f32, N> {
    /// Computes the Euclidean norm of the vector.
    ///
    /// $ norm = sqrt(sum_(i = 1)^n v_i^2) $
    fn norm(self) -> f32 {
        let mut out = 0.0;
        let mut i = 0usize;
        loop {
            if i >= N { break }
            out += self.data[i] * self.data[i];
            i += 1;
        }
        out
    }
}
