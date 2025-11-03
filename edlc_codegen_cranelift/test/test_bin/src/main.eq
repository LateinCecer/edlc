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
        y
        // std::panic("your code is ass");
//        ret todo();
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

    fn add(lhs: Self, rhs: f32) -> Self::Output {
        MyData {
            name: lhs.name,
            id: lhs.id,
            buffer: lhs.buffer + rhs
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
};

impl<const N: usize> SVector<f32, N> {
    fn norm(self: Self) -> f32 {
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
    fn print(self: Self) {
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
        }
    }
}

impl<T> SVector<T, 2> {
    fn new(x: T, y: T) -> Self {
        SVector {
            data: [x, y],
        }
    }
}

impl<T> SVector<T, 3> {
    fn new(x: T, y: T, z: T) -> Self {
        SVector {
            data: [x, y, z],
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
    println(min(3, -3));

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
}
