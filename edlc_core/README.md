EDL is a simple DSL (Domain Specific language) to define governing equations for AcoDyn. The language essentially describes the discretization and solution process. There are a few main design considerations that have to be accounted for during the design process of the language:

- No branching logic: EDL itself does not permit branching of any kind. This is for two reasons: simplicity and optimizations. Defining governing equations can be a very messy process. To content with this, the code should be kept as clean and sleek as possible. Branching logic would get in the way of this goal. As for optimizations, having all sections of the code sequential allows the compiler to add implicit asynchronicity for certain functions. The latter point is especially important for GPU kernel execution, as GPU queues should always be filled as far as possible during runtime.
- Strong and Static typing: having strong and static typing prevents bugs and make optimization easier for the JIT compiler.
- Ahead-of-time memory allocations: the code generated through EDL will be run millions of times during a single simulation run. It is therefore paramount, that code execution is as lean as possible. For this reason, EDL does not allow memory allocations or deletions during the runtime of the program. All variables must be initialized ahead of time, similarly to Fortran. This also means that complex memory management schemes are not needed.
- Code organisation in subroutines: EDL code is organized in subroutines. All subroutines share a fixed set of global variables and can be executed from external callers. This also means that all necessary branching logic can be implemented in the external caller code (implemented in Lua scripts in AcoDyn).

Example layout:

```
module stokes_equation;

// submodules are added to a module in a simple data-tree. All global data
// defined in the parent module can be accessed from the child module.
// The parent, however, respects the childs privacy and cannot access its
// data. The same applies to subroutines.
submodule pressure_equation;


let config: Config = Config::from_file("config.exp");
let cc: CommandAndControl = CommandAndControl::new();

// constant generics are defined globally and values are inferred implicitly
// from context. For example, if the `load_domain` function returns with a
// fuild domain with `DIM = 3` and `NSIZE = 4`, then these values are assumed
// globally for these generics.
// This can be done since the setup is loaded ahead-of-time: all data is
// loaded and allocated, then the subroutines are JIT compiled with these
// values.
//
// If generics have conflicting definitions, or if the value of a generic
// cannot be inferred from context, an error will be thrown.
let domain: FluidDomain<NSIZE, DIM> = load_domain(config, cc);

let u: BoundaryField<DIM, DIM> = BoundaryField::zeros(domain, config);
u.load_boundaries(config, "velocity");

let p: BoundaryField<1, DIM> = BoundaryField::zeros(domain, config);
p.load_boundaries(config, "pressure");
// ...
let p_grad: GradientField<1> = GradientField::zeros(domain.len);


// definition of routines. They can be called from external callers
// and have access to all global variables (as seen in the header)
comptime fn discretize_momentum {
	// the `*` operator is syntax sugar for a dereference operation.
	*p_grad = grad(domain, *p);
	// the equivalent would be:
	p_grad.buf = grad(domain, p.buf);

	// ...
	
	// subroutines are called without a `()` since routines, unlike functions,
	// do not take arguments
	momentum_equation::reduce_buffers;
}


// submodules can also be defined within one file.
submodule momentum_equation {

	// While the variables `p_grad` and `u_laplace` are shadowed to
	// the submodule from the parent module `stokes_equation`, all definitions
	// within the submodule are not visible in the parent module
	let reduce_buffer: Buffer<FloatBuffer> = Buffer::new(
		*p_grad, *u_laplace.corr
	);
	
	// subroutines can be called from routines. Their content may be inlined
	// within the callee. They are inaccessible to external callers.
	comptime fn reduce_buffers() {
	    // the compiletime function emits all functioncalls that are marked with the
	    // `comptime` keyword. 
		comptime {
		    reduce(reduce_buffer, dudt.corr);
        }
	}
}
```


# Regarding comptime functions...

Compiletime functions are not treated as normal functions, but instead compiled into a
fully unrolled sequence of statements.
A call to these functions at compiletime generates this sequence using the argument values
from the function call as compiletime constants.

## Unrolled sequence commands

The fully unrolled sequence of commands consists only of calls to compiletime functions.

## Emitable function calls

Emitable function calls must call to functions that generate can action, which can then
be executed.


```
comptime fn prepare_momentum {
    grad(u, rho, u_mat).emit;
}
```


# Todos

 - Better error reporting for errors in function call resolution
 - Associated types
 - `self` parameters to mark methods
 - Trait definitions in language
 - Struct, Enum and Union definitions
 - Field access operator
 - Pattern matching
 - References in functions + Borrow checking

# Associated Types

Associated types are important for many basic language functions, similar to how they work in Rust.
It is important that associated types are properly implemented in traits, as well as for trait
implementations.

Since alias types are already implemented, the groundwork for associated types has already been
lain.
All that remains now is the actual implementation.

# `self` parameters in function signatures

The `self` parameter is essentially just syntax sugar for `self: Self`.
*But*, like in Rust, we can use it to differentiate methods from functions in the bodies of
implementations and traits: 
Functions that have their first parameter as a `self`, `&self` or `&mut self` can be interpreted
as a method, while other functions cannot.
Note that the `self` parameter can _only_ exist as the first parameter in the function signature.

If change will leave some of the existing codebase broken, but it is better to do this change now,
as there will be significantly more code to update if the postpone this change any further.

# Trait definitions in EDL

Currently, traits can only be defined from the Rust side of the EDL compiler.
It is not possible to insert traits from within EDL source code.
This is not good, as it makes the languages way less flexible when writing libraries.

Implementing support for EDL-defined traits starts with writing the parsing logic for traits,
which should not be too difficult.

# Struct, Enum and Union definitions

The parsing logic for these types already exists, as it was implemented together with alias types.
However, while type definitions are parsed, the compiler currently does not actually do anything
with these types.
This is not good.
We should change that.
The most difficult step in this implementation will be instantiating MIR types for these
user-defined types, as we cannot simply steal the layout from the Rust compiler, like we did so far.
However, we already have logic for dealing with aggregated type layouts from the part of the
compiler that steals the layout of Rust types from rustc.
This can most likely be reused for actual new types.

Then, the only thing that is left to do is to decide on a layout for these structures.
The most straight forward path would be to use C-style type layouts, as this ABI is well understood
by most systems programmers and is also a stable interface to C.
However, we could also optimize that layout a bit more (like most modern languages do) by reordering
the parameters in aggregate types by their size.
That way, the structure of the type is not exactly like specified by the programmer; but the total
size of the type is likely to be smaller than what it would otherwise be.
This way, the programmer does not have to worry about the order in which members are defined in 
structs.

# Type init expressions

We need to be able to create new base types in EDL.
This is currently only possible through compiler intrinsic functions, which kind of defeats the
purpose of having type definitions as a language feature.

To fix this, we need to implement init expressions for the following cases:

- Tuple-like structs
- Dict-like structs
- Tuple-like enum variants
- Dict-like enum variants
- Tuples
- Dicts
- Unions

# Field access operators

Once structs are implemented, it is only natural that the field operator should finally also be
fully implemented.
Most of the logic for this already exists, for the front-end and for the backend.
To only thing really that is left to do is pull the offset of a field from the type registry and
replace the field operator with an offset operator, which is already implemented from the
array-index operator.

# Pattern matching

While structs can be made usable through simple field operators, Rust-like enums are a little 
more involved.
The only good way to deal with Rust-like enums is really the Rust way: through pattern matching.
And if we are to implement pattern matching for enums types, taking the extra mile to 
implement pattern matching for all other types is not that much of a hassle really.

# References

While references are not and should never become a core focus of the language (that's what Rust is
for) using references in a more limited setting may offer significant performance benifits for
those uses who are willing to go the extra mile.
This implementation should be completely transparent when interacting with code that does have
references and it should have fairly simple borrowing rules, since implementing a full
borrow checker is way outside of the scope of this project.

As a starting point, references can only exist as function parameters, and not in _any_ other
place of the code. 
This automatically simplifies lifetimes dramatically, since we can assume that each reference
lives exactly as long as the function scope.

When calling a function, the parameters that are taking as a reference are automatically
referenced, such that explicit reference operators, such as `&` or `&mut` are not needed.
If a function returns a reference, the lifetime of that reference is assumed to be the most
restrictive of the parameter lifetimes.
For the borrow checker, we then have to assume that the returned lifetime captures _all_ of the
input lifetimes, which are then bound as long as the returned lifetime is not dropped.

Since lifetimes cannot exist in variables, the borrow checker only has to consider the transitive
properties of lifetimes captured by function return values in nested function calls.

Once this feature is properly implemented, we can also finally implement a proper version of the
`Index` and `IndexMut` pseudo-trait for the `[]` operator.
This will also let us update the `Set` and `AssignOp` operators to more rigorous versions of
themselves.
