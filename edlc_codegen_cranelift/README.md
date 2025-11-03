# About this crate

What is the purpose of this crate?
As the name suggests, it implements a cranelift-based JIT backend for the EDL language.

Why cranelift and not LLVM?
There are several reasons as to why Cranelift is a better fit for this project then LLVM.
The most important ones are listed below:

1. Embedded compiler: Cranelift is meant to be embedded into the application it is used in. With this, it is much
simpler to build and install a Cranelift powered application than it is to use an LLVM powered JITTER, since LLVM
needs to be linked against statically compiled binaries with have to be pre-installed on the system.
2. Sandboxing: EDL needs to be somewhat mobile by definition and mobile code should never be trusted. LLVM is
build for maximal performance trough aggressive optimizations. Cranelift, on the other hand, uses less aggressive
optimization tactics and much better hardened against security vulnerabilities than LLVM, since it is used for
sandboxed WASM JIT code generation and execution.
3. Compiletime and hot-reloading: Quick feedback is vital for a good user experience, especially for users that are
inexperienced with software development. Since EDL is a scripting language that generally aims to be accessible to
non-tech people, the EDL compiler pipeline to execution should be as fast as possible.
Not only is Cranelift much faster than LLVM in code generation, but it also supports hot-swapping of components in
its JIT executor, which is nice for even faster feedback.
4. WASM: Cranelift can be executed in a WASM environment, while LLVM generally cannot. Since EDL is meant to be
written in a browser window, Cranelift is much better suited for this purpose than LLVM
5. Performance: while LLVM generally generates better optimized and slightly faster code than Cranelift does, is the
performance discrepancy not huge. Especially with EDL and its own MIR-level code transformation, the difference between
LLVM generated code and Cranelift generated code should not be large, since most EDL programs result basically on a
sequence of calls to external functions. To add to this, the vast majority of the runtime of the program will be spend
evaluating these external functions, while the time spend in the actual EDL program is typically negligible.



Panic unwinding in EDL work based on a global panic handler that is located at a special location. The format of the panic handler is described in the following sections:


# Legacy Format

In the legacy format (currently implemented) the panic handler is only really valid for single threaded programs, as all threads look for the same panic indicator. The stack consists of a 2 byte header followed by `n` stack messages.

|     indicator     | stack size                                                                                                                      |
|:-----------------:|:--------------------------------------------------------------------------------------------------------------------------------|
|   size: 1 byte    | size: 1 byte                                                                                                                    |
| `0x00` = no panic | number of stack trace elements in the panic stack. Each message consists essentially of a max. 255 character long utf-8 string. |
|  `0xFF` = panic   |                                                                                                                                 |

|           message length           | message bytes                 |
|:----------------------------------:|:------------------------------|
|            size: 1 byte            | size: max. 255 bytes          |
| Size indicator of the raw message. | Raw message, encoded in UTF-8 |

# New Format

Switching to a new format has two reasons; for one, panics should behave correctly for multithreaded programs - if a thread panics that panic should only be unwound for that thread with all other threads ignoring the panic all together. Secondly, the position of the error on the source code should be encoded separately so that the error messages may be formatted nicely for reporting.

## Header

| # active panics | active panic table | trace pool       |
|-----------------|--------------------|------------------|
| 1-byte          | 8 entries          | 8 separate pools |

## Active Panic Table


##  Trace Pool

| # entries      | entry pool  |
|----------------|-------------|
| 4-byte counter | trace entry |

### trace entry

| src name length        | src name bytes   | padding                 | line   | col    | inlined                                      | padding | msg length      | msg bytes        | padding                 |
|------------------------|------------------|-------------------------|--------|--------|----------------------------------------------|---------|-----------------|------------------|-------------------------|
| 2-byte src name length | max. 65535 bytes | 0-2 bytes               | 2-byte | 2-byte | 1-byte                                       | 1-byte  | 2-bytes         | max. 65535 bytes | 0-2 bytes               |
| u16                    | UTF-8 string     | pad to 4-byte alignment | u16    | u16    | is the source inlined or an EDL source file? | unused  | msg byte length | UTF-8 string     | pad to 4-byte alignment |

