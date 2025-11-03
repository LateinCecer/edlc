mod foo;

use test_fracht::foo::bar::something;
use std::MyData;

type Double = f64;
type Vec<const N: usize> = [foo::Float; N];
type V1 = Vec<1>;

let values: Vec<_> = [0.0, 1.0, 3.0];
let v1: V1 = [42.0];


/// This is how a trait can be defined in EDL.
/// A trait can contain:
///
/// - type definitions
/// - constant definitions
/// - function definitions
///
/// As a trait does not contain the actual logic of the code, but rather the 'facade' for a programming interface,
/// all of these items only need to be specified for their signature.
/// The actual definition of the value behind this signature is then done in the trait implementations.
/// You can think of the items contained in a trait more of like a promise that the implementation makes: Any type
/// that implements the trait promises to populate all of the items defined in the trait definition with valid logic.
trait MyTrait<T> {
    type Output<F>;
    const N: usize;

    fn do_stuff(this: Self, val: [T; Self::N]) -> Self::Output::<T>;
}

impl MyTrait<i32> for MyData<i32> {
    type Output<F> = [F; Self::N];
    const N: usize = 4;

    fn do_stuff(this: Self, val: [i32; Self::N]) -> Self::Output::<i32> {
        let test: [f64; Self::N] = zero_array();
        val
    }
}

fn zero_array<T, const N: usize>() -> [T; N] {
    todo("implement")
}

type A = struct {
    a: i32,
    b: str,
};

impl<T> core::Add<A, T> for A {
    type Output = Self;

    fn add(lhs: Self, rhs: T) -> Self {
        lhs
    }
}


/// This is usually the main entry point for a program.
fn main() {
    let x = foo::foo(1, 2);
    let y = something();
    let z = test_generics::<i32>(32);

    let pi = foo::pi;

    let mut data = MyData::<i32>::new();
    let x = data.do_something();
    let x = data.do_stuff(zero_array());

    let t = test_min(test_min(1.0, 3.0), 2.0);
    let t: f64 = test_min(1.0, data.do_something() as f64);

    let mut a = data.a;
    a = 42.0;
    data.a = a;

    let b = data.b;

    // create test struct A
    let test = A {
        a: 1,
        b: "hello, world!",
    };
    let tmp = test + 42_i32;

    let test2 = { a: 1i32, b: 2usize, c: 3.0f32 };
    let test3 = ( 0.0i32,    0.0132f32          );
    let a = test2.a;
    let b = test3.1;
}

fn test_min(x: Double, y: f64) -> f64 {
    ret x;
}

fn test_generics<T>(val: T) -> T {
    val
}

// fn create_tuple<T>(x: i32, y: T) -> (i32, T) {}
// fn create_dict<T>(x: i32, val: T) -> { id: i32, val: T } {}

let ITEMS: [i32; 3] = [1_i32, 42_i32, 3_i32];

fn bar(x: usize) -> i32 {
    ITEMS[x]
}

fn todo(msg: str) -> ! {
    if true {
        loop {
            if true { break; }
        };
    }
    loop {}
}

impl MyData<usize> {
    fn blub(this: Self) {
        todo("blübedi")
    }
}

impl<T> MyData<T> {
    fn new() -> Self {
        todo("implement creation of struct creation")
    }

    fn filter(this: Self) -> MyData<T> {
        this
    }

    fn do_something(this: Self) -> i32 {
        // test creation of Self with parameters
        let x: Self<i32> = Self::<_>::new();
        // this just loops a bunch of times to calc fib_10.
        let mut prev = 0_i32;
        let mut current = 1_i32;
        let mut n = 1usize;
        loop {
            if true {
                break current;
            }
            n = 1;

            let x = 0;
            // let x = prev + current;
            // (addition is not implemented without a backend)
            prev = current;
            current = x;
        }
    }
}
