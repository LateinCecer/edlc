# What is the EDL language?

EDL is a language that is designed to be used in environments were a program is started with a set amount of resources,
after which a (computationally heavy) task is performed to transform these resources and generate output data.
Following this, the program is terminated and the allocated resources are freed all at once.
A classical example of such a workflow is HPC computing.
To deal with computationally heavy algorithms effectively, the heavy lifting should be done by Rust code that is
called from EDL, such that the actual EDL program just controls overall control flow of the program.

## Why not just use Rust?

We believe that Rust is a wonderful language for HPC computing.
It is fast, modern and encourages good practice in academic software, which is desperately needed in an environment in
which a lot of fundamental code relies on barely maintained legacy codes that is usually interacted with through
multiple layers of abstraction and through multiple language barriers.
As such, the first version of [AcoDyn](https://acodyn.com) was written in entirely in Rust.
However, I repeatedly found myself running into three hurdles that were hard to overcome with just Rust:

1. Certain HPC workloads require rapid prototyping. Rusts low compile times, especially with high optimization profiles,
   make working with these kind of tasks incredibly frustrating. A good project structure can mitigate this to some
   degree, but especially with experimental platforms and ever changing demands, this is not an ideal solution.
2. Relying on generics for things like floating point accuracy or spatial dimensions of grids in fvm or fem simulations
   is neat, but the final decision of what generics should be used is often only known after reading the mesh or
   configuration files; in other words, during runtime. This requires ugly and cumbersome code to actually call the
   generic version of the code from a context that does not know these generics at compiletime.
3. Rust is a fairly hard language to learn for much of the scientific community that often only have a background with
   fundamental tasks in languages like Python or, on your lucky day, Julia. Simple modifications to the logic of a
   numeric solver that is implemented in Rust often require a deeper understanding of the language than should be
   necessary for these modifications or can be expected of a researcher who's primary job is not coding in Rust all day.

EDL is an attempt at fixing these issues, while retaining some of the things that make Rust great.
The core fundamentals of EDL are

1. JIT-compiling:
   1. Compiling the EDL script for a numeric solver only requires compiling the solver itself, at the startup of the 
   program. The Rust code that does the actual heavy lifting is not touched and must therefore not be recompiled.
   2. Since the EDL program is compiled when all configuration files and resources such as mesh data or access to
   physical devices are already present, data from these sources is already present *at compile time* and can be
   incorporated into the program. EDL has special semantics to separate between runtime and
   [`comptime`](#Compiletime).
2. All resources are must be loaded at compile time.
   1. Since all information, including configuration options are known at compile time, and since the program is only
   suppose to run a one transformative task on the input data, there is no real need to allocate new resources after
   the EDL program is compiled. We compile the program, load all the resources necessary for running your task on it
   and then terminate the program, freeing all the resources allocated during compile time in the process.
   2. An exception to this is rule are resources that are loaded temporarily under the hood, while calling a Rust
   function from EDL. This could, for example be a `Vec<_>` that is needed to run a certain algorithm. These temporarily
   allocated resources should, however, be freed when the rust function returns control flow to the EDL program.
3. All resources are unloaded at, and only at, the destruction of the program.
   1. Since we only load resources at the start of the program, we can be expected to keep them around until the
   program terminates. Since no new resources should be permanently allocated, the footprint of the program should stay
   relatively constant during its execution.
   2. This drastically simplifies things like memory management. All things that are allocated dynamically during 
   runtime are things that live on a stack frame of a function, or things that are dealt with by the rust underbelly.
4. Computationally heavy routines and complicated logic are dealt with by Rust.
   1. Everything that is complicated and needs heavy profiling and excellent good computational efficiency should be
   offloaded to the main program written in Rust. Rust is an excellent language to deal with the complicated things that
   need to be blazingly fast, such that EDL does not have to. A good JIT compiler can go a long way to make a JIT 
   compiled language fast, especially if it is strong and statically typed like EDL, but this project does not aim to
   produce a language implementation that is as fast, or faster than the output of Rustc on release mode.
   2. EDL needs to be compatible with Rust on a fundamental level. The main way to ensure this is to use the same
   calling style, and more importantly the same type system as Rust. This goes so far that, in the current version of
   the EDLc Cranelift codegen backend, the layout of aggregate types is snatched directly from the output generated by
   Rustc for the Rust-side equivalent of the type using the `mem::offset_of!` macro, currently available in nightly
   Rust.
5. Elegance in simplicity
   1. EDL should be as accessible to newcomers as possible. We do not want to overload non-CS people with things like
   pointer arithmetic or borrow checking, as these things should be dealt with by the main Rust program anyway.

# Functions

For the most part, functions in EDL work the same way as they do in Rust.
A simple function without a return type (a void function in C) can be written like this

```edl
fn foo() {}
```

or alternatively the empty return type `()` can be used explicitly using a trailing return type specifier:

```edl
fn bar() -> () {}
```

Returns in function can be specified, like in Rust, through explicit or implicit returns.
Likewise, items like `if`- and `loop`-statements can be expressive in the same ways as they can be in Rust.

```edl
fn min(x: f32, y: f32) -> f32 {
    if x < y {
        x           // return through expressive `if` statement
    } else {
        return y;   // return through explicit return statement
    } // `if`-expression can return the value in th first branch since it is the last expression in the function body
      // and not followed by a `;`
}
```

Since EDL borrows it's type system from Rust, function signatures can have generic types and constants:

```edl
fn max<T>(x: T, y: T) -> T {
    std::max(x, y)
}
```

Unfortunately, type constraints are currently not possible in EDL, since the logic for that is just not implemented yet.
The way that generic functions usually work in the projects that already use EDL, is by implementing the function body
as a callback to a Rust function. Since each combination of generic parameters types must be matched exactly to a
specific function instance on the Rust side, generic parameter combinations that are not implemented cause a linking
error during code generation.
This work around is not pretty, but since most of the complex stuff happens on the Rust side anyway, it is kind of
OK at least for the work that I do.
Notice that the max function above calls a function `std::max<D>(x: D, y: D) -> D`.
This function could be implemented in Rust. Since EDL has no concept of type constraints yet, the compiler will just
try to generate an instance of `std::max` with the type substitute of `T` that `max` was instantiated with.
For example, if we want to evaluate the expression `max(2i32, 3)`, the compiler knows that it must generate an instance
of `max` that takes the type `i32` for the generic `T` (if that instance has not already been generated).
In order to generate this function body, the compiler must find or generate an instance of `std::mix` with `D` = `i32`.
Since that function is a callback to a Rust function, the body cannot be generated by EDL itself, and it tries to lookup
the address of a matching instance in it's linker.
If that instance exists, everything works fine.
Otherwise, the linker generates an error.

It should be noted that all functions in this section are strictly runtime functions.
This means that these functions can only be executed once the program is fully compiled and has entered runtime.
To facilitate complex actions during the compile time of an EDL program, [compile time](#Compiletime) functions can be
used.

# Compiletime

Since EDL does a lot more things at compile time than a normal, head-of-time compiled program, special semantics are
useful to distinguish between compile time and runtime.
For this, we took some inspiration from the Zig project and added the `comptime` keyword to EDL.
This keyword can be added to functions to tell the caller (and the compiler) that the function should only be called
during the compile time of the program:

```edl
comptime fn allocate_vector<T>(len: usize) -> Vec<T> {
    // ...
}
```

This function allocates a vector with a certain amount of elements during compile time.
Since allocations are a form of resource acquisition, EDL only allows this action during compile time.
Because we can load all kinds of configuration files and other data, such as mesh data, during compile time (
remember, EDL is JIT compiled) we can do stuff like allocate a vector of elements for each cell of a finite volume
grid *during compiletime*.

Since the initial values of all global variables need to be known at compile time, the RHS of any global variable
declaration must be a `comptime` context.
This also implies that global variables are readable at compile time - this applies both to constants, and to normal
variables.

```edl
const NUM_CELLS: usize = load_num_cells("config/mesh.msh");
let pressure_field = allocate_vector::<f64>(NUM_CELLS);
```

In this example, `allocate_vector` can be called to assign the value of `pressure_field` because `pressure_field` is
defined in a global context, thus making the RHS of the initialization a `comptime` context by default.
Since this kind of erodes the distinction between global variables (defined by `let`) and constants (defined by `const`)
in the context of traditional languages (like Rust), EDLs definition of constants is a bit different from Rusts.
For more information about this topic, refer to the [consts chapter](#Consts).
It should also be noted that, since the RHS of one global variable can depend on another global variable, the order
in which global variables are defined, also defines the order in which these variables are loaded - like it would be
the case in a normal function body.

## Maybe Comptime

There also exist functions that can be called in both `comptime` *and* runtime contexts.
These are all functions that have *no side effects* - they do not load resources, they do not output anything to
secondary channels and they are *stateless*, meaning that running the function with the same parameters always
produces the same results.
An example for such a function would be all simple arithmetic functions like `add` (desugared from the `+` operator).
If we take a look at the definition of the `add` function in the `Add` pseudo-trait, we see the following:

```edl
trait Add<Rhs> {
    type Output;
    ?comptime add(lhs: Self, rhs: Rhs) -> Self::Output;
}
```

The important thing is that the function is declared as `?comptime`, which is the syntax for 'maybe comptime'
functions.
These functions can be called during both `comptime` and runtime.
While these functions are the most versatile of functions, they are also the most restricted kinds of functions
as the `?comptime` context only allows calling other `?comptime` functions.

## Local Comptime Context

Sometimes it is useful to call comptime functions in a runtime context.
An example for this can be that you need a resource allocated at compile time, that is however only useful in the
scope of a certain functions body.

```edl
fn do_something() {
    let len = 32;
    let buffer: Vec<f32> = allocate_vector(len);
}
```

If we try to compile this function, the compiler gets very unhappy and returns an error explaining that we tried
to call a `comptime` function (`allocate_vector`) in a runtime context.
Now, since the call `allocate_vector(len)` only depends on `len`, which is known at compile time at the position
at which `allocate_vector` is called, the compiler __could__ evaluate this call at compile time like how RustC
would evaluate something like this at compile time:

```Rust
const fn example_function(x: usize, y: usize) -> usize {
    x + y
}

fn do_something() {
    let value = example_function(1, 2);
}
```

In Rust, this makes total sense, because `const` functions need to be stateless and without side effects in Rust
such that it just does not matter weather the function is called at compile time or at runtime.
This is equivalent to how `?comptime` functions work on EDL.
For `comptime` functions however, it *does* matter if they are called during compile time or during runtime.
First of all, we cannot call `comptime` functions during runtime as this violates the core principles of the
language.
Secondly, since a `comptime` function can allocate resources, it is crucial that they are actually only called
*once* per invocation, and that is during compile time.
It is also important that the return value of a `comptime` function is available for any subsequent calls to `comptime`
functions.

In previous iterations of the language specifications and the compiler, the `comptime` context in a normal function
body was implicit, meaning that the example above would compile just fine, but the compiler would complain if
it was unable to determine the value of `len` at compile time.
This, however, makes it very hard to read for the programmer which function calls (that, again, might have side
effects) are called at compile time, and which ones are not.
To solve this problem, a `comptime { }` block context was introduced.
The insides of these blocks is always evaluated at compile time, and thus in a `comptime`-context, like the RHS of
global variable declarations.
Additionally, in order for the block to be evaluatable, all variables that are captured from outside the block must
have values known at compile time (or be global variables or constants – as these always have values knowable that
compile time).
Further, this change meant that it was now valid for the compiler to enforce calls to `comptime` functions to always
happen from a `comptime` context.
Thus, to make the example from above compile, we can rewrite it like this:

```edl
fn do_something() {
  let len = 32;
  let buffer: Vec<f32> = comptime { allocate_vector(len) };
}
```

In a sense, EDLs `comptime` functions and blocks work similarly to Rusts `unsafe`, at least concerning readability
and workflow.


## Hybrid Functions

In certain cases, you may find yourself needing to pass `comptime` parameters to a function that is otherwise
called at runtime.
Taking the example from above, we might find ourselves in a situation in which we need to pass the `len` parameter to
the function in the call.
If we tried using normal function parameters, this would not work:

```edl
fn do_something(len: usize) {
  let buffer: Vec<f32> = comptime { allocate_vector(len) };
}
```

The `comptime` block tries to capture the parameter variable `len`, which is not knowable at compile time, as the
specific value of the parameter depends on the value passed in each specific function call.
Even if we call the function using a `comptime`-knowable parameter value like this `do_something(32)`, the function
body itself cannot be built because there is only one instance of the function body to handle all calls.
Getting around this is possible using a hybrid function like this:

```edl
fn do_something(comptime len: usize) {
  let buffer: Vec<f32> = comptime { allocate_vector(len) };
}
```

These hybrid functions can be compiled in either of two ways:

1. the entire function is _always_ inlined
2. we generate a function for each unique function call

If we go with #1, everything is clear.
For #2, there are some things to consider; we could just generate a function for each unique combination of parameters.
However, this would kind of mess with the expected behaviour that invoking a comptime function runs it exactly once
during compile time for each unique function call.
Thus, it is only logical that each hybrid function should evaluate each of it's internal `comptime` contexts
exactly once for every unique call – exactly how you would expect it when calling a `comptime` function.
In the example above, it is reasonable to assume for the caller that each invocation of `do_something` creates its own
`buffer`, even when the function is called twice with the same parameter value for `len` but at different places in the
code.


## What Contexts can Call What?

Here is a table showcasing which contexts can call which functions:

|             | runtime | `comptime` | `?comptime` | hybrid |
|:-----------:|:-------:|:----------:|:-----------:|:------:|
|   runtime   |    ✓    |     ❌     |      ✓      |   ✓    |
| `comptime`  |    ❌   |     ✓      |      ✓      |   ❌   |
| `?comptime` |    ❌   |     ❌     |      ✓      |   ❌   |
|    hybrid   |    ✓    |     ❌     |      ✓      |   ✓    |

To reiterate, a `comptime` context exists inside a `comptime` function body, inside a `comptime {}` block and on
the RHS of a global variable declaration / `const` declaration.
A runtime context exists in all functions bodies *not* declared `comptime` or `?comptime`, *including* hybrid functions
as these are also called during runtime.
Lastly, a `?comptime` context exists *only* in function bodies of `?comptime` functions.


# Consts

Constants are different from global variables in EDL, mainly because constants can be used during type resolution.
This means, that a constant can be used as a generic constant in a type declaration at another point in the program.
Let's say that we have a program like this

```edl
const N: usize = 3;
let array = [0i32; N];
```

In this case, the type of the `array` variable will be interpreted as `[i32; N]` by the type inferer.
The constant `N` is thus part of the type declaration of `array` and must therefore be present during type resolution.
This also entails that the type of every constant must be knowable during type resolution.
In turn, we must be able to determine the type of `N` *before we can interpret the RHS of const declarations*.
Therefore, `const` declarations *must always* contain an explicit type declaration.
On the contrary, type declarations for global `let` declarations are optional, as the value of these does not need
to be known during type resolution.
In the example above, the type inferer is able to infer the type of `array`, meaning that the type must not be
specified explicitly, leading to less unnecessary syntax.


# Types

```edl
/// We can define alias types like this:
type Float = f32;

/// We can structs with named members like this:
type Matrix<T, const N: usize, const M: usize> = struct {
    data: [[T; N]; M],  // column-major layout
};

/// Or with tuple-like members like this:
type TupleLikeStruct = struct(i32, usize);

/// A Vector is an alias of a matrix with exactly one column.
/// The type `Vector<f64, 3>` is the exact same type as `Matrix<f64, 3, 1>` and the can be used interchangibly.
type Vector<T, const N: usize> = Matrix<T, N, 1>;

/// Enums work in the same way as they work in Rust.
/// Variants can be defined as tuple-like, or as having named members, just like structs can.
/// Since EDL is able to snatch the type layout from RustC, EDL enums can be passed to rust functions that take rust
/// enums without issue.
/// This, of cause, only works as long as the EDL compiler was compiled with the same exact RustC version as the
/// Rust function being called.
type Option<T> = enum {
    Some(T),
    None,
};

/// EDL also has anonymous types.
/// These are tuples (which work like rust tuples):
type NormalTuple = (i32, str);

/// ... and dicts:
type DictType = {
    name: str,
    age: i32,
};
/// Two tuple types are equivalent to one another, if the tuple consists of the same member types in th same order.
/// Two dic types are equivalent to one another, if they have the exact same member names, as well as matching types 
/// for these members.
/// Dics are cool, since they allow basically building named function parameters in a way that still feels kind of
/// rusty.
/// The following function can be called like this:
///
/// plot(x, y, {
///     linetype: LineStyle::Dashed,
///     label: Some("performance")
/// });
///
/// This allows us more flexible options for when we want to potentially built an interface to scientific python APIs
/// like pyplot in the future.
fn plot(x: Vec<f32>, y: Vec<f32>, args: { linestyle: LineStyle, label: Option<str> }) {
    // ...
}
```



# ToDos

This sections contains things that should be included in the language at some point, but are currently not finished
or not even started.
There are also some things that are not completely clear regarding the implementation of these features and the language
design still needs some consideration before work on the compiler implementation can start.
This, in particular, is a section that any willing compiler building enthusiast is welcome to contribute to.

## Pattern Matching

In order to make tagged enums actually useful, pattern matching is kind of necessary.
Additionally, pattern matching would be a nice addition for other types of data types such as structs, tuples and dicts.
The current plan sees pattern matching to be implemented effectively in the same way as Rusts pattern matching – if only
in order to keep EDL as accessible as possible to Rust programmers.
Feel free to expand this section with actual, concrete language definitions for pattern matching.

## References

This is a tricky one.
In the context of EDL, having references is both a good and a bad thing.
Since EDL stores a lot of data just on stack frames, there is a lot of copying and moving data involved.
This is not that bad when dealing with small data types and by delegating large amounts of data to resources managed by
the main Rust program that are referenced through abstract pointers a lot of the costs associated with dealing with
large data sets can be mitigated.
However, having references would enable more efficient data processing in source code written directly in EDL.

### The Problem

As nice as references are, there are two main problems associated with using references:

1. dealing with references is hard. While explicit pointer arithmetic is certainly not needed when dealing with rust-like
   references, they are still one of the harder concepts in programming. Since EDL is targeted towards people who do not
   have long standing experience with systems programming languages, introducing references is a slippery slope.
2. memory may be invalidated. When we deal with references, memory that these references inherently point to, may become
   invalid while the reference is still alive. Going the Rusty way, we should probably solve this using some kind of
   borrow-checker adjacent concept.

### Core Idea

So here is how I'm currently thinking to tackle these problems.
All of these ideas are certainly not ideal and have their downsides but I think this might not be the worst route:

First and foremost, references should not be a necessity to use EDL.
Using simple, high-level abstractions should not require thinking about references at all, while still being able
to profit at least in part from their positive impact to performance.
So how can we do this?

Taking a little bit of inspiration from the worst programing language known to man (C++), we can use references in a way
that is implicit when *calling* a function.
Let's look at the following code:

```edl
fn foo(x: &i32) {
    std::println(x);
}

fn main() {
    let x = 42;
    foo(x);
}
```

In this example, we call `foo` by passing it a reference to the variable `x`.
Now, in Rust you would have to specify the referencing of `x` explicitly, using the `&` operator in the function call.
This clearly communicates to the programmer that `foo(x)` takes a reference to `x` without knowing the actual signature
of `foo`.
In Rust this makes a lot of sense since the borrowing rules are complex and Rust is a very precise language.
While I generally prefer Rusts approach (I really do), the C++ way does not really require knowledge of how pointers or
references work at all, so it might be more beginner-friendly as the code reads more like plain code.
The story is a bit different when we use mutable references.
As EDL is immutable by default, there really should be some kind of indicator as to when we toss a mutable reference to
a function.

```edl
fn bar(x: &mut i32) {
    x *= 3;
}

fn main() {
    let mut x = 42;
    foo(mut x);
}
```

One way of handling this could be to just add the `mut` keyword in front of the mutable variable that we toss as a mutable
reference to `bar`.
While, again, I prefer the Rust way, I basically want to avoid all kinds of explicit referencing/dereferencing operators
and keep all of it implicit.

Now, this also requires that some other concepts in the language be a lot simpler in order to avoid having to deal with
complex nets of interconnected lexical lifetimes, like the rust borrow checker and lifetime analysis has to deal with.
So here is a simple set of rules that we could enforce to keep lifetimes in EDL simple:

1. variables cannot store references, except for function parameter variables
2. if the return type of a function is a reference, its upper bound is the least-upper bound of parameter lifetimes for
   that function call. If the function does not take any references as parameters, the reference must be of a global
   variable and thus the lifetime is `'static`
3. references are automatically dereferenced when they are the RHS of the `=` operator
4. references are automatically dereferenced when they are the LHS of the `=` operator
5. variables are automatically referenced, when they are passed as a parameter to a function call that expects a reference
   at that place. If the expected reference is mutable, the `mut` keyword must be used as described above.
6. aggregate types like structs cannot have reference-type members
7. references of references (for example `&&i32`) do not exist in EDL
8. mutable references can be automatically downgraded to a shared reference, but not the other way around
9. there can either be multiple shared references to a variable *or* a single mutable reference
10. a reference may never outlive the data it is referencing


### Implementation

Implementing references in this fashion is much easier than implementing a full borrow checker like the one used by RustC,
since lifetimes are less complex (remember, references cannot be stored in local variables, or be part of structs).
However, a propper and safe implementation still requires some kind of borrow-checker.
First of all, this means that we need non-lexical lifetimes in EDL, complete with a lifetime-resolver.
Then, a simple form of a borrow-checker can be implemented.
Some work has already been done towards this goal however a propper implementation may require basically a full rewrite of
the MIR code implementation towards a propper DFG.


## `self` in methods

Currently methods are associate functions that take a variable of the type they are implemented for as the first parameter.

```edl
type MyData = struct {
    value: f64,
    name: str,
};

impl MyData {
    fn print(s: Self) {
        std::print("MyData { value: ");
        std::print(s.value);
        std::print(", name: '");
        std::print(s.name);
        std::print("' }");
    }
}

fn main() {
    let data = MyData { value: 3.1415, name: "hello, world!" };
    data.print();
}
```

This was easy to implement but the Rust way of separating methods more clearly from normal associated functions would be a
lot nicer.
The brunt work for this was already done by implementing the `Self` reserved type.
All that remains is implementing methods as functions that take `self` as the first parameter without a type specifier
(the type `Self` is implied) like how it works in Rust.

```edl
type MyData = struct {
    value: f64,
    name: str,
};

impl MyData {
    fn print(self) {
        std::print("MyData { value: ");
        std::print(self.value);
        std::print(", name: '");
        std::print(self.name);
        std::print("' }");
    }
}

fn main() {
    let data = MyData { value: 3.1415, name: "hello, world!" };
    data.print();
}
```

Functions that are declared without the `self` parameter in the first slot are then just normal functions and no longer
methods.
The main work in with this is adjusting all the EDL code that already relies on the old system.

## Propper `trait`s

Currently, traits exist in EDL and are totally implementable for different datatypes.
However, defining a trait itself is only possible on the Rust side of things – they cannot be defined in EDL itself.
Additionally, defining a trait is not even that comfortable on the Rust side and was implemented almost purely as a
way to get pseudo-traits for operator desugaring to work.
Traits currently also don't really make use of associated types, as support for associated types was only added to
EDL way after traits were added and I did not have enough time to work on EDL since.

Having proper traits will be a great addition to EDL as it opens up a lot of design-techniques for coding that are
currently just not possible in EDL.

## Type Constraints

As eluded to earlier, EDL does not have type constraints for generic types, as of now.
However, as a Rust-adjacent language, type constraints would be very handy.
To facilitate this, we may implement type constraints in the same fashion has they are implemented in Rust.
This is fairly straight forward, however it does require some serious work to the function and trait resolver and the
type-inferer.

