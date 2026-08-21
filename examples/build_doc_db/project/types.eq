//! The `types` submodule, containing aggregate type definitions.

/// A named struct representing a 2D point.
type Point = struct {
    x: f32,
    y: f32,
};

/// A tuple struct wrapping a single value.
type Wrapper = struct(f32);

/// A zero-sized unit struct.
type Marker = struct;

/// A tagged union over several numeric representations.
type Numeric = enum {
    F32 { value: f32 },
    F64 { value: f64 },
    I32 { value: i32 },
    Unit
};

/// A generic fixed-size vector.
type SVector<T, const N: usize>
where T: f32 | f64 = struct {
    data: [T; N],
};

impl<const N: usize> SVector<f32, N> {
    /// Computes the Euclidean norm of the vector.
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
