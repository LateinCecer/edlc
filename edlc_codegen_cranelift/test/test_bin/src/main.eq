mod foo;

use test_bin::foo::bar;
use test_lib::println;

use test_bin::foo::TEST;
use test_bin::foo::boa::TESTOS;

/// Manual implementation of the `min` function for `i32` signed integer types.
fn min(x: i32, y: i32) -> i32 {
    if x < y {
        // returns x if x < y through early-return
        ret x;
    } else {
        // print test message and return y through the return value if the if-else chain
        println("what?");
        todo()
        // y
        // std::panic("your code is ass");
        // ret todo();
    }
}

fn todo() -> ! {
    std::panic("unimplemented");
}

type MyData<T> = struct {
    name: str,
    id: usize,
    buffer: T,
};

impl core::Add<MyData<f32>, f32> for MyData<f32> {
    type Output = MyData<f32>;

    fn add(self, rhs: f32) -> Self::Output {
        MyData {
            name: self.name,
            id: self.id,
            buffer: self.buffer + rhs
        }
    }
}

// creates some tuple
fn make_tuple(args: { first: i32, second: f32 }) -> (i32, f32) {
    (args.first, args.second)
//    todo()
}

type SVector<T, const N: usize> = struct {
    data: [T; N],
    test: T,
};

impl<const N: usize> SVector<f32, N> {
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

impl<T, const N: usize> SVector<T, N> {
    fn print(self) {
        std::io::print("(");
        let mut i = 0usize;
        loop {
            if i >= N { break }
            if i != 0 {
                std::io::print(", ");
            }
            std::io::print(self.data[i]);
            i += 1;
        }
        std::io::print(")");
    }
}

impl<T> SVector<T, 1> {
    fn new(x: T) -> Self {
        SVector {
            data: [x],
            test: x,
        }
    }
}

impl<T> SVector<T, 2> {
    fn new(x: T, y: T) -> Self {
        SVector {
            data: [x, y],
            test: x,
        }
    }
}

impl<T> SVector<T, 3> {
    fn new(x: T, y: T, z: T) -> Self {
        SVector {
            data: [x, y, z],
            test: x,
        }
    }
}

/// Main entry point to the solver implementation.
fn main() {
    // this function call uses the import from above to find the function definition in the source tree
    let mut x = bar(1.0, 2.0);
    // this function uses the relative module name to find the function definition.
    // since the module `foo` is a child of this module, we do not need to specify the name of the parent module
    // explicitly in the functions' qualifier name
    x = foo::bar(0.1, x);
    // this function call uses the fully qualified name to resolve the function `bar` in the source tree.
    x = test_bin::foo::bar(x, 3.1415);

    // std::panic("test"); // if this is commented in, the remainder of the function is flagged for dead code!
    // todo();

    let mut data_fragment = MyData { name: "i am a data fragment", id: 1, buffer: 42.0f32 };
    println(data_fragment.name);
    println(data_fragment.id);
    println(data_fragment.buffer);

    let tmp = data_fragment + 0.3_f32;
    println(tmp.name);
    println(tmp.buffer);

    // create some tuple from a dict (lol)
    let t: (i32, f32) = make_tuple({ first: 0, second: 0.2 });
    println(t.0);
    println(t.1);

    let data: [usize; _] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    println(TESTOS);
    foo::boa::i_am_testos();
    println(min(-5, -3));

    let mut i = 0usize;
    let mut max: usize = 0;
    loop {
        if i >= TEST {
            break;
        }
        println("Hello, world!");
        foo::print_pi();
        println(foo::pi);

        max = usize::max(max, data[i]);
        i += 1;
    }
    // todo()
}

#[setup]
fn simple_setup() {
    println("setting up testing environment");
}

#[teardown]
fn simple_teardown() {
    println("tearing down testing environment");
}

#[bench(setup=simple_setup, teardown=simple_teardown)]
#[test(setup=simple_setup, teardown=simple_teardown)]
fn test_array() {
    println("testing weird bug with array accesses...");
    let A_0 = 1.0_f64;
    let A_1 = 1.0_f64;
    let A_2 = 1.0_f64;

    let pi = 3.14159265358979_f64;
    let twopi = pi * 2.0;

    let A: [f64; 3] = [1.0, 1.0, 1.0];
    let sigma: [f64; 3] = [0.1, 0.1, 0.1];

    let prefactor: f64 = 1.0 / (f64::sqrt(twopi * twopi * twopi) * sigma[0] * sigma[1] * sigma[2]);
    println(prefactor);
    println("success!");
}

#[bench]
fn benchmark() {
    let mut i: usize = 0;
    let mut out: f64 = 1.0;
    loop {
        if i >= 1000_000 { break; }
        out = f64::sin(out * 1.5);
        i += 1;
    }
    println(out);
}


// -- testing reference related stuff
impl<T, const N: usize> SVector<T, N> {
    fn index_mut(&mut self, index: usize) -> &mut T {
        core::assert(index < N);
        ret self.data[index];
    }

    fn index(&self, index: usize) -> &T {
        core::assert(index < N);
        ret self.data[index];
    }

    fn set_index(&mut self, index: usize, value: T) {
        self.data[index] = value;
    }

    fn print_test(&self) {
        println(self.test);
    }
}

#[test]
fn test_vec_mut_access() {
    let mut vec = SVector::new(2.1_f64, 1.0_f64);
    vec.print_test();
    core::assert(vec.index(0) == 2.1);
    core::assert(vec.index(1) == 1.0);

    vec.data[0] += 0.1;
    core::assert(vec.index(0) == 2.2);

    vec.set_index(0, 4.2);
    core::assert(vec.index(0) == 4.2);

    vec.index_mut(1) = 204.1;
    core::assert(vec.index(1) == 204.1);
}
