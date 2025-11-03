mod bar;

type Float = f32;

/// Define test value for PI
let pi: Float = 3.1415;


#[target=gpu]
fn foo(a: i32, b: i32) -> i32 {
    test_fracht::bar(1)
}
