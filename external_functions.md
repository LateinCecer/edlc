

```rust

let position: Field<f32, 3> = Field::new(1000);
let velocity: Field<f32, 3> = Field::new(1000);
let neighbors: Buffer<u32> = Buffer::new(1000 * 4);
let force: Field<f32, 3> = Field::new(1000);

// This function is compiled to PTX
fn calc_force(
    pos: &SVector<f32, 3>,
    v: &SVector<f32, 3>,
    neighbors: &[u32],
) -> SVector<f32, 3> {
    // ... some actual implementation
    SVector::new(0.0, 0.0, 0.0)
}

fn main() {
    // execute `calc_force` is executed for every element in `force`, results are writing to `force`
    gpu::for_each(force, calc_force, { pos: position, v: velocity }, { neighbors });
}

```



