/*
 *    Copyright 2025 Adrian Paskert
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */


#[macro_export]
macro_rules! expr_format(
    () => ("");
    ($one:expr) => ($one);
    ($first:expr, $($later:expr),+) => (
        format!("{}, {}", $first, expr_format!($($later),+))
    );
);
pub use expr_format;

#[macro_export]
macro_rules! jit_func {
    // Implement everything for implementations with associated types
    (for ($base:expr) impl $exec:expr, fn($fn_id:expr),
        fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t:,)* $(const $n: $nt),*>($($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_extern::<extern "C" fn($($a: _),*) -> _>(
            $fn_id,
            edlc_core::inline_code!("<>"),
            Some(edlc_core::inline_code!($base)),
            false,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
        )?;
    }};

    (for ($base:expr) impl $exec:expr, fn($fn_id:expr),
        const fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_extern::<extern "C" fn($($a: _),*) -> _>(
            $fn_id,
            edlc_core::inline_code!("<>"),
            Some(edlc_core::inline_code!($base)),
            true,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
        )?;
    }};

    (for ($base:expr) impl $exec:expr, fn($fn_id:expr), $runtime_id:expr,
        fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($runtime:ident: $Runtime:ty, $($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($runtime: &Option<RwLock<$Runtime>>, $($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_runtime_extern::<extern "C" fn(&Option<RwLock<$Runtime>>, $($a: _),*) -> _, _>(
            $fn_id,
            edlc_core::inline_code!("<>"),
            Some(edlc_core::inline_code!($base)),
            false,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
            $runtime_id,
        )?;
    }};

    (for ($base:expr) impl $exec:expr, fn($fn_id:expr), $runtime_id:expr,
        const fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($runtime:ident: $Runtime:ty, $($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($runtime: &Option<RwLock<$Runtime>>, $($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_runtime_extern::<extern "C" fn(&Option<RwLock<$Runtime>>, $($a: _),*) -> _, _>(
            $fn_id,
            edlc_core::inline_code!("<>"),
            Some(edlc_core::inline_code!($base)),
            true,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
            $runtime_id,
        )?;
    }};


    // same thing, but with parameters

    (for ($base:expr) impl $exec:expr, fn<$($param:expr),*;>($fn_id:expr),
        fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_extern::<extern "C" fn($($a: _),*) -> _>(
            $fn_id,
            edlc_core::inline_code!(&format!("<{}>", expr_format!($($param),*))),
            Some(edlc_core::inline_code!($base)),
            false,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
        )?;
    }};

    (for ($base:expr) impl $exec:expr, fn<$($param:expr),*;>($fn_id:expr),
        const fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_extern::<extern "C" fn($($a: _),*) -> _>(
            $fn_id,
            edlc_core::inline_code!(&format!("<{}>", expr_format!($($param),*))),
            Some(edlc_core::inline_code!($base)),
            true,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
        )?;
    }};

    (for ($base:expr) impl $exec:expr, fn<$($param:expr),*;>($fn_id:expr), $runtime_id:expr,
        fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($runtime:ident: $Runtime:ty, $($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($runtime: &Option<RwLock<$Runtime>>, $($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_runtime_extern::<extern "C" fn(&Option<RwLock<$Runtime>>, $($a: _),*) -> _, _>(
            $fn_id,
            edlc_core::inline_code!(&format!("<{}>", expr_format!($($param),*))),
            Some(edlc_core::inline_code!($base)),
            false,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
            $runtime_id,
        )?;
    }};

    (for ($base:expr) impl $exec:expr, fn<$($param:expr),*;>($fn_id:expr), $runtime_id:expr,
        const fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($runtime:ident: $Runtime:ty, $($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($runtime: &Option<RwLock<$Runtime>>, $($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_runtime_extern::<extern "C" fn(&Option<RwLock<$Runtime>>, $($a: _),*) -> _, _>(
            $fn_id,
            edlc_core::inline_code!(&format!("<{}>", expr_format!($($param),*))),
            Some(edlc_core::inline_code!($base)),
            true,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
            $runtime_id,
        )?;
    }};




    // impl for functions without associated type

    ($exec:expr, fn($fn_id:expr),
        fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_extern::<extern "C" fn($($a: _),*) -> _>(
            $fn_id,
            edlc_core::inline_code!("<>"),
            None,
            false,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
        )?;
    }};

    ($exec:expr, fn($fn_id:expr),
        const fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_extern::<extern "C" fn($($a: _),*) -> _>(
            $fn_id,
            edlc_core::inline_code!("<>"),
            None,
            true,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
        )?;
    }};

    ($exec:expr, fn($fn_id:expr), $runtime_id:expr,
        fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($runtime:ident: $Runtime:ty, $($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($runtime: &Option<RwLock<$Runtime>>, $($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_runtime_extern::<extern "C" fn(&Option<RwLock<$Runtime>>, $($a: _),*) -> _, _>(
            $fn_id,
            edlc_core::inline_code!("<>"),
            None,
            false,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
            $runtime_id,
        )?;
    }};

    ($exec:expr, fn($fn_id:expr), $runtime_id:expr,
        const fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($runtime:ident: $Runtime:ty, $($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($runtime: &Option<RwLock<$Runtime>>, $($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_runtime_extern::<extern "C" fn(&Option<RwLock<$Runtime>>, $($a: _),*) -> _, _>(
            $fn_id,
            edlc_core::inline_code!("<>"),
            None,
            true,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
            $runtime_id,
        )?;
    }};


    // same thing, but with parameters

    ($exec:expr, fn<$($param:expr),*;>($fn_id:expr),
        fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_extern::<extern "C" fn($($a: _),*) -> _>(
            $fn_id,
            edlc_core::inline_code!(&format!("<{}>", expr_format!($($param),*))),
            None,
            false,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
        )?;
    }};

    ($exec:expr, fn<$($param:expr),*;>($fn_id:expr),
        const fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_extern::<extern "C" fn($($a: _),*) -> _>(
            $fn_id,
            edlc_core::inline_code!(&format!("<{}>", expr_format!($($param),*))),
            None,
            true,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
        )?;
    }};

    ($exec:expr, fn<$($param:expr),*;>($fn_id:expr), $runtime_id:expr,
        fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($runtime:ident: $Runtime:ty, $($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($runtime: &Option<RwLock<$Runtime>>, $($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_runtime_extern::<extern "C" fn(&Option<RwLock<$Runtime>>, $($a: _),*) -> _, _>(
            $fn_id,
            edlc_core::inline_code!(&format!("<{}>", expr_format!($($param),*))),
            None,
            false,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
            $runtime_id,
        )?;
    }};

    ($exec:expr, fn<$($param:expr),*;>($fn_id:expr), $runtime_id:expr,
        const fn $name:ident<$(($t:ident = $T:ty),)* $(const ($n:ident = $N:expr): $nt:ty),*>($runtime:ident: $Runtime:ty, $($a:ident: $A:ty),*) -> $Ret:ty
        where $($t_:ty: $clause:tt),*; $body:tt
    ) => {{
        #[allow(improper_ctypes_definitions)]
        #[allow(unused_parens)]
        extern "C" fn $name<$($t,)* $(const $n: $nt),*>($runtime: &Option<RwLock<$Runtime>>, $($a: $A),*) -> $Ret
        where $($t_: $clause),* { $body }

        #[allow(improper_ctypes_definitions)]
        $exec.insert_runtime_extern::<extern "C" fn(&Option<RwLock<$Runtime>>, $($a: _),*) -> _, _>(
            $fn_id,
            edlc_core::inline_code!(&format!("<{}>", expr_format!($($param),*))),
            None,
            true,
            #[allow(improper_ctypes_definitions)]
            $name :: <$($T,)* $($N),*>,
            std::stringify!($name),
            $runtime_id,
        )?;
    }};
}

pub use jit_func;


#[cfg(test)]
mod test {
    use std::{any, fs, mem, slice};
    use edlc_core::prelude::mir_str::FatPtr;
    use log::info;
    use crate::executor::CraneliftJIT;
    use crate::prelude::TypedProgram;
    use std::sync::RwLock;
    use edlc_core::inline_code;
    use edlc_core::prelude::edl_type::{EdlRepresentation, EdlTypeId};
    use edlc_core::prelude::{DocGenerator, Item};
    use edlc_core::prelude::ast_type_def::LayoutOptions;
    use edlc_core::prelude::mir_type::layout::{Layout, MirLayout, OffsetStructLayoutBuilder, StructLayoutBuilder};
    use edlc_core::prelude::mir_type::MirTypeRegistry;
    use crate::{jit_intrinsic_panic};

    #[test]
    fn test() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println(val: str)"))?;
        jit_func!(compiler, fn(fn_id),
            fn println<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                println!("{}", msg);
            }
        );

        compiler.compile_module(vec!["test"].into(), inline_code!(r#"
use std::println;

fn test() -> i32 {
    println("Hello, world!");
    0
}
        "#))?;

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), inline_code!("test()"))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);
        Ok(())
    }

    #[test]
    fn test_bytes() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println(a: u8, b: u8, c: u32)"))?;
        jit_func!(compiler, fn(fn_id),
            fn println<>(a: u8, b: u8, c: u32) -> () where; {
                println!("a={a}, b={b}, c={c}",);
            }
        );

        compiler.compile_module(vec!["test"].into(), inline_code!(r#"
use std::println;

fn test() -> i32 {
    println(1, 2, 3);
    0
}
        "#))?;

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), inline_code!("test()"))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);
        Ok(())
    }

    #[test]
    fn test_runtime() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<i32>::default();
        compiler.init()?;
        compiler.backend.insert_runtime(0, "runtime", 42)?;

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println(val: str)"))?;
        jit_func!(compiler, fn(fn_id), 0,
            fn println<>(runtime: i32, line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                println!("`{}` from runtime {}", msg, runtime.as_ref().unwrap().read().unwrap());

                *runtime.as_ref().unwrap()
                    .write()
                    .unwrap() = 69;
            }
        );

        compiler.compile_module(vec!["test"].into(), inline_code!(r#"
use std::println;

fn test() -> i32 {
    println("Hello, world!");
    0
}
        "#))?;

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), inline_code!("test()"))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);

        let rt = *compiler.backend.get_runtime(0.into())?
            .read().unwrap();
        assert_eq!(rt, 69);
        Ok(())
    }

    #[test]
    fn test_params() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn println_str<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                println!("{}", msg);
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn println_i32<>(line: i32) -> () where; {
                println!("{}", line);
            }
        );

        compiler.compile_module(vec!["test"].into(), inline_code!(r#"
use std::println;

fn test() -> i32 {
    println("Hello, world!");
    println(-42_i32);
    0
}
        "#))?;

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), inline_code!("test()"))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);
        Ok(())
    }

    #[test]
    fn test_params_runtime() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<i32>::default();
        compiler.init()?;
        compiler.backend.insert_runtime(0, "runtime", -69)?;

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id), 0,
            fn println_str<>(runtime: i32, line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };

                println!("`{}` in runtime `{}`", msg, runtime.as_ref().unwrap().read().unwrap());
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn println_i32<>(line: i32) -> () where; {
                println!("{}", line);
            }
        );
        jit_func!(compiler, fn<"f64";>(fn_id),
            fn println_f64<>(line: f64) -> () where; {
                println!("{}", line);
            }
        );

        compiler.compile_module(vec!["test"].into(), inline_code!(r#"
use std::println;

let pi = 3.14159265_f64;

fn test() -> i32 {
    let pi_floored = f64::floor(pi);
    println(pi_floored);

    println("Hello, world!");
    println(-42_i32);
    0
}
        "#))?;

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), inline_code!("test()"))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);
        Ok(())
    }

    #[repr(C)]
    struct MyData<T> {
        key: i32,
        buf: [T; 16],
    }

    impl<T: MirLayout + 'static> MirLayout for MyData<T> {
        fn layout(types: &MirTypeRegistry) -> Layout {
            let mut builder = StructLayoutBuilder::default();
            builder.add("key".to_string(), types.i32(), types);
            builder.add("buf".to_string(), types.get_type_from_rust::<[T; 16]>().unwrap(), types);
            builder.make::<Self>()
        }
    }

    #[test]
    fn test_associated() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn println<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                println!("{}", msg);
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn println_i32<>(val: i32) -> () where; {
                println!("{}", val);
            }
        );
        jit_func!(compiler, fn<"bool";>(fn_id),
            fn println_bool<>(val: bool) -> () where; {
                println!("{}", val);
            }
        );


        compiler.compiler.prepare_module(&vec!["std", "test"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("?comptime fn assert(val: bool, msg: str)"))?;
        jit_func!(compiler, fn(fn_id),
            const fn assert<>(val: bool, msg: FatPtr) -> () where; {
                if !val {
                    let msg = unsafe { std::str::from_utf8_unchecked(
                        slice::from_raw_parts(msg.ptr.0 as *const u8, msg.size)) };
                    eprintln!("assertion failed: {msg}");
                }
            }
        );

        compiler.compiler.change_current_module(&vec!["std"].into());
        compiler.compiler.parse_and_insert_type_def(inline_code!("MyData"), inline_code!("<T>"))?;
        compiler.compiler.insert_type_instance::<[f32; 16]>(inline_code!("[f32; 16]"))?;
        compiler.compiler.insert_type_instance::<[f64; 16]>(inline_code!("[f64; 16]"))?;
        compiler.compiler.insert_type_instance::<MyData<f32>>(inline_code!("MyData<f32>"))?;
        compiler.compiler.insert_type_instance::<MyData<f64>>(inline_code!("MyData<f64>"))?;

        let [new, get_buf, get_key] = compiler.compiler.parse_impl(
            inline_code!("<T>"),
            inline_code!("MyData<T>"),
            [
                inline_code!("fn new(key: i32, repeat: T) -> MyData<T>"),
                inline_code!("fn get_buf(self: MyData<T>) -> [T; 16]"),
                inline_code!("fn get_key(self: MyData<T>) -> i32"),
            ],
            None
        )?;

        fn foo<Data: Copy + 'static>(compiler: &mut CraneliftJIT<()>, new: EdlTypeId) -> Result<(), anyhow::Error> {
            jit_func!(for (&format!("MyData<{}>", any::type_name::<Data>())) impl compiler, fn(new),
                fn new_data<(T = Data), const (N = 16): usize>(key: i32, repeat: T) -> MyData<T>
                where T: Copy,
                    T: 'static; {
                    info!("mem::size_of::<[{}; {}]>() = {}", any::type_name::<T>(), N, mem::size_of::<[T; N]>());
                    MyData {
                        key,
                        buf: [repeat; 16],
                    }
                }
            );
            Ok(())
        }

        foo::<f32>(&mut compiler, new)?;

        jit_func!(for ("MyData<f32>") impl compiler, fn(get_buf),
            fn data_get_buf<>(this: MyData<f32>) -> [f32; 16] where; {
                this.buf
            }
        );
        jit_func!(for ("MyData<f32>") impl compiler, fn(get_key),
            fn data_get_key<>(this: MyData<f32>) -> i32 where; {
                this.key
            }
        );

        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::println;
use std::MyData;
use std::test::assert;

fn print_mantra() {
    println("This machine is my Temple");
    println("each one a sacred Shrine.");
    println("I name every piston Blessed");
    println("and every gear Divine.");
}

fn test() -> i32 {
    println("creating `MyData` instance...");
    let data: MyData<f32> = MyData::new(1234, 1.234_f32);
    println("asserting key...");
    println(data.get_key());
    println(data.get_key() == 1234);


    assert(data.get_key() == 1234, "wrong key");
    let buf: [f32; 16] = data.get_buf();
    assert(buf[0usize] == 1.234f32, "wrong data at index 0");
    assert(buf[3usize] == 1.234f32, "wrong data at index 3");
    assert(buf[7usize] == 1.234f32, "wrong data at index 7");
    assert(buf[11usize] == 1.234f32, "wrong data at index 11");
    assert(buf[15usize] == 1.234f32, "wrong data at index 15");

    print_mantra();
    0
}
        "#))?;

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);
        Ok(())
    }

    #[test]
    fn test_str_args() -> Result<(), anyhow::Error> {
        #[derive(Debug)]
        struct MyRuntime {
            #[allow(dead_code)]
            value: f64,
        }

        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::default();
        compiler.init()?;
        compiler.backend.insert_runtime(0, "runtime", MyRuntime { value: std::f64::consts::PI })?;
        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn println<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                println!("{}", msg);
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn println_i32<>(val: i32) -> () where; {
                println!("{}", val);
            }
        );
        jit_func!(compiler, fn<"bool";>(fn_id),
            fn println_bool<>(val: bool) -> () where; {
                println!("{}", val);
            }
        );

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        compiler.compiler.insert_type_instance::<[f64; 3]>(inline_code!("[f64; 3]"))?;
        compiler.compiler.insert_type_instance::<[FatPtr; 3]>(inline_code!("[str; 3]"))?;
        compiler.compiler.insert_type_instance::<[usize; 3]>(inline_code!("[usize; 3]"))?;

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        compiler.compiler.parse_and_insert_type_def(inline_code!("MyData"), inline_code!("<>"))?;
        compiler.compiler.insert_type_instance::<usize>(inline_code!("MyData"))?;

        let [new_data, fs,  fs2, foo] = compiler.compiler.parse_impl(
            inline_code!("<>"),
            inline_code!("MyData"),
            [
                inline_code!("fn new() -> MyData"),
                inline_code!(r#"
                fn test(this: MyData, domain: usize, msg1: str, msg2: str);
                "#),

                inline_code!(r#"
                fn test2(this: MyData, domain: usize, msg1: str, msg2: str, test: i32);
                "#),

                inline_code!(r#"
                ?comptime fn foo(
                    fields: [str; 3],
                    buffer: [usize; 3],
                    dims: [usize; 3],
                    name: str
                ) -> [usize; 3]"#)
            ],
            None,
        )?;

        jit_func!(for ("MyData") impl &mut compiler, fn(foo), 0,
            const fn foo_<>(            // RDI = return buffer
                runtime: MyRuntime,     // -> RSI
                fields: [FatPtr; 3],    // -> RDX
                buffer: [usize; 3],     // -> RCX
                dims: [usize; 3],       // -> R8?
                name: FatPtr            // -> stack spill
            ) -> [usize; 3] where; {
                let runtime = runtime.as_ref().unwrap().read().unwrap();
                println!("Running test function...");
                println!("Runtime: {runtime:?}");
                let fields = fields.map(|s| unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(s.ptr.0 as *const u8, s.size)
                    )
                });
                let name = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(name.ptr.0 as *const u8, name.size)
                    )
                };
                println!("foo ({name}):");
                println!(" > fields = {fields:?}");
                println!(" > buffer = {buffer:?}");
                println!(" > dims = {dims:?}");
                [42; 3]
            }
        );

        jit_func!(for ("MyData") impl &mut compiler, fn(new_data),
            fn new<>() -> usize where; {
                0
            }
        );
        jit_func!(for ("MyData") impl &mut compiler, fn(fs2), 0,
            fn test__<>(
                runtime: MyRuntime, // RDI
                data: usize,        // RSI
                domain: usize,      // RDX
                msg1: FatPtr,       // RCX & R8
                msg2: FatPtr,       // stack spill
                test: i32           // R9
            ) -> () where; {
                info!("test: runtime = {runtime:?}, data = {data}, domain = {domain}");
                let msg1_str = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg1.ptr.0 as *const u8, msg1.size)
                    )
                };
                let msg2_str = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg2.ptr.0 as *const u8, msg2.size)
                    )
                };
                println!("msg1 = {msg1:?} equal msg1 = {msg1_str}");
                println!("msg2 = {msg2:?} equal msg2 = {msg2_str}");
                println!("test = {test}");
            }
        );
        jit_func!(for ("MyData") impl &mut compiler, fn(fs), 0,
            fn test<>(
                runtime: MyRuntime, // RDI
                data: usize,        // RSI
                domain: usize,      // RDX
                msg1: FatPtr,       // RCX & R8
                msg2: FatPtr        // stack spill
            ) -> () where; {
                println!("test: runtime = {runtime:?}, data = {data}, domain = {domain}");
                let msg1_str = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg1.ptr.0 as *const u8, msg1.size)
                    )
                };
                let msg2_str = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg2.ptr.0 as *const u8, msg2.size)
                    )
                };
                println!("msg1 = {msg1:?} equal msg1 = {msg1_str}");
                println!("msg2 = {msg2:?} equal msg2 = {msg2_str}");
            }
        );

        compiler.compile_module(vec!["test"].into(), inline_code!(r#"
use std::println;
use std::MyData;

fn test() -> i32 {
    println("starting tests...");
    let data = MyData::new();
    data.test(0, "hello,", " world!");
    data.test2(42, "hello,", " world!", -31415);

    println("testing foo...");
    let fields = ["a", "b", "c"];
    let buffers = [0usize, 1usize, 2usize];
    let dims = [2usize, 1usize, 2usize];
    println("...");
    let test: [usize; 3] = MyData::foo(fields, buffers, dims, "rino!");
    println("done testing foo!");
    0
}
        "#))?;

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), inline_code!(r#"test()"#))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);
        Ok(())
    }

    #[test]
    fn test_comptime() -> Result<(), anyhow::Error> {
        #[derive(Debug)]
        struct MyRuntime {
            #[allow(dead_code)]
            value: u64,
            res: Vec<String>,
        }

        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::default();
        compiler.init()?;
        compiler.backend.insert_runtime(0, "runtime", MyRuntime { value: 420, res: Vec::new() })?;
        compiler.compiler.prepare_module(&vec!["std"].into())?;

        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn println<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                println!("{}", msg);
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn println_i32<>(val: i32) -> () where; {
                println!("{}", val);
            }
        );
        jit_func!(compiler, fn<"bool";>(fn_id),
            fn println_bool<>(val: bool) -> () where; {
                println!("{}", val);
            }
        );

        compiler.compiler.parse_and_insert_type_def(inline_code!("Resource"), inline_code!("<>"))?;
        compiler.compiler.insert_type_instance::<usize>(inline_code!("Resource"))?;

        let fs = compiler.compiler.parse_impl(
            inline_code!("<>"),
            inline_code!("Resource"),
            [
                inline_code!("comptime fn new(val: str) -> Resource"),
                inline_code!("fn print(self: Resource)")
            ],
            None,
        )?;

        jit_func!(for ("Resource") impl &mut compiler, fn(fs[0]), 0,
            const fn new<>(runtime: MyRuntime, value: FatPtr) -> usize where; {
                let mut runtime = runtime.as_ref().unwrap()
                    .write().unwrap();
                let id = runtime.res.len();
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(value.ptr.0 as *const u8, value.size)
                    )
                };
                runtime.res.push(msg.to_string());
                id
            }
        );
        jit_func!(for ("Resource") impl &mut compiler, fn(fs[1]), 0,
            fn print_res<>(runtime: MyRuntime, this: usize) -> () where; {
                let runtime = runtime.as_ref().unwrap()
                    .read().unwrap();
                if this > runtime.res.len() {
                    jit_intrinsic_panic!(&format!("invalid resource with id `{this}`"));
                }
                println!("{}", runtime.res[this]);
            }
        );

        compiler.compile_module(vec!["test"].into(), inline_code!(r#"
//! This module exists purely for testing purposes.
//! It can be used to test the recently integrated `comptime` semantics of EDL, which are now one
//! of the core features of the language.

use std::println;
use std::Resource;

comptime fn dim() -> usize {
    0
}

const DIM: usize = dim();

let a = create_resource("asdf");

fn foo(a: u32, b: u32) -> str {
    "I'm a runtime-only function"
}

/// This function is marked as `?comptime`, which means that it __can__ be executed at compiletime,
/// __if__ all arguments passed to the function are available during compiletime.
/// This qualifier will usually be attached to functions that work without side effects, like
/// integer addition.
/// Functions of this type can be executed in **both** runtime & comptime contexts.
?comptime fn bar(val: str) -> str {
    val
}

/// This is a common example for a `comptime` function.
/// It can only be executed at compiletime which also means that its entire contents can be executed
/// at compiletime.
/// Thus, calls to `comptime` functions are entirely possbile without nested `comptime` blocks.
///
/// # Parameters
///
/// Function parameters in `comptime` functions are __implicitly__ understood to be available at
/// compiletime; marking them as `comptime` is technically not a mistake, since they always are
/// `comptime`, but since it makes the function signature unnecassarily hard to read, this will
/// invoke a compiler warning.
comptime fn create_resource(msg: str) -> Resource {
    // foo(1, 2);       <-- this does not compile, as `foo` is a runtime function
    Resource::new(bar(msg))
}

/// This is an example for a hybrid function.
/// Since of its parameters are specified as `comptime` (specifically `msg`) which allows the
/// function to pass this parameter into a comptime block.
/// With this, the function can be executed during runtime, but the comptime block within will
/// only be run __once__ for every __unique call__ to the function.
///
/// # How does this work
///
/// The function that is actually generated by this function defintion only has one parameter;
/// the non-comptime `index` parameter.
/// All parameters marked as `comptime` are not included in the physical function signature, but
/// instead statically compiled into the functiond body at compile time.
/// This means that we need to generate a function instance for every unique combination of
/// comptime parameters for the function.
fn partial_resource(index: i32, comptime msg: str, comptime x: i32) -> Resource {
    let res = comptime {
        Resource::new(msg)
    };
    println("created resource in partially comptime context");
    println(index);
    println(x);
    res
}

?comptime fn maybe_resource(x: i32) -> Resource {
    let l1 = bar("maybe maybe");
    let res = comptime {
        Resource::new(l1)
    };
    res
}

fn test() -> i32 {
    println("Hello, world!");
    let res = comptime { Resource::new("this is a test") };
    res.print();

    // let msg = foo(1, 2);   <-- this does not compile, since  `foo(1, 2)` is not comptime
    let mut msg = "A";         // <-- this does compile, since `"A"` is comptime
    let a = comptime { create_resource(msg) };
    let b = comptime {
        // msg = "C";         // <-- this fails, as comptime contexts cannot modify captured variables
        create_resource("B")
    };
    a.print();
    b.print();

    // let test = comptime { foo(0, 1) };   <-- this does not compile, as `foo` is not marked as
    //                                          `comptime`

    let mut i: i32 = 0;
    loop {
        if i >= 10 {
            break;
        }

        // even though the function is called 10 times here, it only generates a single
        // resource, since this call only has 1 unique combination of `comptime` parameters
        let c = partial_resource(i, "msg", 0);
        c.print();

        i += 1;
    }

    // since the second `comptime` parameter here is different than the one above, this call
    // generates a new resource instance.
    let c = partial_resource(i, "msg", 42);
    c.print();

    // this function call has exactly the same comptime parameter combination as the one in the
    // loop. However, since not all types implement the partial_eq trait, we have to rely on
    // the location of the function call in the source code.
    // Since this call is distinct from the one in the loop, eventhough it has the same
    // combination of comptime parameters, it generates a distinct instance of `partial_resource`
    // and with that a new resource instance.
    let d = partial_resource(i, "msg", 0);
    d.print();

    // since the first `comptime` parameter here is different than the one above, this call
    // generates a new resource instance.
    let e = partial_resource(i, "new msg", 0);
    e.print();


    // the following four calls should compile just fine and produce only one resource, since
    // `maybe_resource` is marked `?comptime` and called from a runtime context; meaning it is
    // compiled as if it was a runtime function.
    //
    // Since the parameter `i` is not known at compiletime, these three calls must be generated
    // during the runtime of the program.
    // If we were to input a compiletime-known value for the parameter instead, these function
    // calls would be resolved during the compiletime, as functions marked `?comptime` can and will
    // be evaluated during the compiletime if all of their parameters are known at compiletime.
    let f = maybe_resource(i);
    f.print();
    let f = maybe_resource(i);
    f.print();
    let f = maybe_resource(i);
    f.print();

    // the following two calls should each be evaluated at compiletime, as a compiletime function.
    // since the entire function body is then evaluated twice and the `comptime` block inside of
    // `maybe_resource` is effectively ignored, this should produce 2 additional resources.
    let g = comptime { maybe_resource(0) };
    g.print();
    let g = comptime { maybe_resource(0) };
    g.print();

    println("done!");
    0
}
        "#))?;

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), inline_code!(r#"test()"#))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);
        assert_eq!(11, compiler.backend.get_runtime(0.into())?.read().unwrap().res.len());

        // test documentation function
        struct SimpleDocPrinter {}

        impl DocGenerator for SimpleDocPrinter {
            type Error = ();
            fn insert_definition(&mut self, item: &Item) -> Result<(), ()> {
                println!("{item}");
                Ok(())
            }
        }

        println!("documentation:");
        compiler.compiler.generate_docs(&mut SimpleDocPrinter {});

        if !fs::exists("test")? {
            fs::create_dir("test")?;
        }
        if !fs::exists("test/docs")? {
            fs::create_dir("test/docs")?;
        }

        println!("\ngenerating docs...\n");
        // let mut html_gen = edl_docs::HtmlGenerator::new(
        //     BufWriter::new(File::create("test/docs/index.html")?));
        // compiler.compiler.generate_docs(&mut html_gen);
        // html_gen.finish()?;
        Ok(())
    }

    #[test]
    fn test_comptime_parameter_passing() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;
        compiler.compiler.prepare_module(&vec!["std"].into())?;

        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn println<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                println!("{}", msg);
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn println_i32<>(val: i32) -> () where; {
                println!("{}", val);
            }
        );
        jit_func!(compiler, fn<"bool";>(fn_id),
            fn println_bool<>(val: bool) -> () where; {
                println!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f64";>(fn_id),
            fn println_bool<>(val: f64) -> () where; {
                println!("{}", val);
            }
        );

        compiler.compile_module(vec!["test"].into(), inline_code!(r#"
use std::println;

fn test(f: f64) -> i32 {
    println("starting test");
    let i = 0;
    let mut j = 12_f64;

    iteration(i, j) as i32
}

#[inline]
fn iteration(i: f64, comptime j: f64) -> usize {
    println("i:");
    println(i);
    println("j:");

    let mut iter: usize = 0;
    loop {
        if iter >= 4 {
            break;
        }
        if single_iter(i, j) == 10 {
            println("hello, world!");
            break;
        }
        iter += 1;
    }
    0
}

fn single_iter(i: f64, comptime j: f64) -> usize {
    println(j);
    0
}
        "#))?;

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), inline_code!(r#"test(3.14)"#))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);
        Ok(())
    }

    #[test]
    fn test_comptime_parameter_passing_debug() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;
        compiler.compiler.prepare_module(&vec!["std"].into())?;

        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn println<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                println!("{}", msg);
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn println_i32<>(val: i32) -> () where; {
                println!("{}", val);
            }
        );
        jit_func!(compiler, fn<"bool";>(fn_id),
            fn println_bool<>(val: bool) -> () where; {
                println!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f64";>(fn_id),
            fn println_bool<>(val: f64) -> () where; {
                println!("{}", val);
            }
        );

        compiler.compile_module(vec!["test"].into(), inline_code!(r#"
use std::println;

fn test(f: f64) -> i32 {
    println("starting test");
    let i = 0.0;
    let mut j = 12.0;

    iteration(i, j) as i32
}

#[inline]
fn iteration(i: f64, comptime j: f64) -> usize {
    println("i:");
    println(i);
    println("j:");

    let mut iter: usize = 0;
    loop {
        if iter >= 4 {
            break;
        }
        if single_iter(i, j) == 10 {
            println("hello, world!");
            break;
        }
        iter += 1;
    }
    0
}

fn single_iter(i: f64, comptime j: f64) -> usize {
    println(j);
    0
}
        "#))?;

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), inline_code!(r#"test(3.14)"#))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);
        Ok(())
    }

    #[test]
    fn test_trigonometry() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;
        compiler.compiler.prepare_module(&vec!["std"].into())?;

        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn println<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                println!("{}", msg);
            }
        );
        jit_func!(compiler, fn<"f32";>(fn_id),
            fn println_i32<>(val: f32) -> () where; {
                println!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f64";>(fn_id),
            fn println_bool<>(val: f64) -> () where; {
                println!("{}", val);
            }
        );

        compiler.compile_module(vec!["test"].into(), inline_code!(r#"
use std::println;

fn test() -> i32 {
    println(f64::sin(3.14159265));
    println(f32::cos(3.1415 / 2.0));
    println(f64::asin(1.0));
    println(f32::asin(1.0));
    println(f64::exp(3.0));
    println(f32::ln(3.14f32.exp()));
    0
}
        "#))?;

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), inline_code!(r#"test()"#))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);
        Ok(())
    }

    #[test]
    fn test_nested_function_resolve_bug() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;
        compiler.compiler.prepare_module(&vec!["std"].into())?;

        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn println<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                println!("{}", msg);
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn println_i32<>(val: i32) -> () where; {
                println!("{}", val);
            }
        );
        jit_func!(compiler, fn<"usize";>(fn_id),
            fn println_usize<>(val: usize) -> () where; {
                println!("{}", val);
            }
        );
        jit_func!(compiler, fn<"bool";>(fn_id),
            fn println_bool<>(val: bool) -> () where; {
                println!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f64";>(fn_id),
            fn println_f64<>(val: f64) -> () where; {
                println!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f32";>(fn_id),
            fn println_f32<>(val: f32) -> () where; {
                println!("{}", val);
            }
        );

        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn print<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn print<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                print!("{}", msg);
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn print_i32<>(val: i32) -> () where; {
                print!("{}", val);
            }
        );
        jit_func!(compiler, fn<"usize";>(fn_id),
            fn print_usize<>(val: usize) -> () where; {
                print!("{}", val);
            }
        );
        jit_func!(compiler, fn<"bool";>(fn_id),
            fn print_bool<>(val: bool) -> () where; {
                print!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f64";>(fn_id),
            fn print_f64<>(val: f64) -> () where; {
                print!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f32";>(fn_id),
            fn print_f32<>(val: f32) -> () where; {
                print!("{}", val);
            }
        );

        let mut options = LayoutOptions::default();
        options.can_init = true;
        options.repr = EdlRepresentation::C;

        #[repr(C)]
        struct PhyNum<T> {
            val: T,
            unit: FatPtr,
        }

        impl<T: MirLayout + 'static> MirLayout for PhyNum<T> {
            fn layout(types: &MirTypeRegistry) -> Layout {
                let mut builder = OffsetStructLayoutBuilder::default();
                builder.add_type::<T>("val".to_string(), types, mem::offset_of!(Self, val));
                builder.add_type::<FatPtr>("unit".to_string(), types, mem::offset_of!(Self, unit));
                builder.make::<Self>(types)
            }
        }

        compiler.compiler.define_type(inline_code!(r#"type PhyNum<T> = struct {
            val: T,
            unit: str,
        };"#), options)?;
        compiler.compiler.insert_type_instance::<PhyNum<f64>>(inline_code!("PhyNum<f64>"))?;
        compiler.compiler.insert_type_instance::<PhyNum<f32>>(inline_code!("PhyNum<f32>"))?;

        let mut options = LayoutOptions::default();
        options.can_init = true;
        options.repr = EdlRepresentation::C;

        #[repr(C)]
        struct SVector<T, const N: usize> {
            data: [T; N],
        }

        impl<T: MirLayout + 'static, const N: usize> MirLayout for SVector<T, N>
        where [T; N]: MirLayout + 'static {
            fn layout(types: &MirTypeRegistry) -> Layout {
                let mut builder = OffsetStructLayoutBuilder::default();
                builder.add_type::<[T; N]>("data".to_string(), types, mem::offset_of!(Self, data));
                builder.make::<Self>(types)
            }
        }

        compiler.compiler.define_type(inline_code!(r#"type SVector<T, const N: usize> = struct {
            data: [T; N],
        };"#), options)?;
        compiler.compiler.insert_type_instance::<SVector<f64, 1>>(inline_code!("SVector<f64, 1>"))?;
        compiler.compiler.insert_type_instance::<SVector<f64, 2>>(inline_code!("SVector<f64, 2>"))?;
        compiler.compiler.insert_type_instance::<SVector<f64, 3>>(inline_code!("SVector<f64, 3>"))?;
        compiler.compiler.insert_type_instance::<SVector<f64, 4>>(inline_code!("SVector<f64, 4>"))?;

        compiler.compiler.insert_type_instance::<SVector<f32, 1>>(inline_code!("SVector<f32, 1>"))?;
        compiler.compiler.insert_type_instance::<SVector<f32, 2>>(inline_code!("SVector<f32, 2>"))?;
        compiler.compiler.insert_type_instance::<SVector<f32, 3>>(inline_code!("SVector<f32, 3>"))?;
        compiler.compiler.insert_type_instance::<SVector<f32, 4>>(inline_code!("SVector<f32, 4>"))?;

        let env = compiler.compiler.parse_fn_signature(inline_code!("?comptime fn env<T>(name: str, def: T) -> T"))?;
        jit_func!(compiler, fn<"PhyNum<f64>";>(env),
            const fn env_num_f64<>(_name: FatPtr, def: PhyNum<f64>) -> PhyNum<f64> where; {
                PhyNum {
                    unit: def.unit,
                    val: std::f64::consts::PI + def.val,
                }
            }
        );

        trait Zero {
            fn zero() -> Self;
        }

        impl Zero for f32 {
            fn zero() -> Self {
                0.0
            }
        }

        impl Zero for f64 {
            fn zero() -> Self {
                0.0
            }
        }

        let zero_gradient = compiler.compiler
            .parse_fn_signature(inline_code!(r#"
            ?comptime fn zero_gradient<T, const N: usize, const DIM: usize>() -> [SVector<T, N>; DIM]
            "#))?;

        fn impl_grad<T: Zero + Copy + 'static, const N: usize, const DIM: usize>(
            comp: &mut CraneliftJIT<()>,
            zero_gradient: EdlTypeId,
        ) -> Result<(), anyhow::Error> {
            jit_func!(comp, fn<any::type_name::<T>(), &format!("{N}"), &format!("{DIM}");>(zero_gradient),
                const fn zero_gradient_<(T_=T), const (N_=N): usize, const (DIM_=DIM): usize>()
                -> [SVector<T_, N_>; DIM_] where T_: Zero, T_: Copy; {
                    [0; DIM_].map(|_| SVector { data: [T_::zero(); N_] })
                }
            );
            Ok(())
        }
        impl_grad::<f32, 1, 1>(&mut compiler, zero_gradient)?;
        impl_grad::<f32, 2, 1>(&mut compiler, zero_gradient)?;
        impl_grad::<f32, 3, 1>(&mut compiler, zero_gradient)?;
        impl_grad::<f32, 4, 1>(&mut compiler, zero_gradient)?;

        impl_grad::<f32, 2, 2>(&mut compiler, zero_gradient)?;
        impl_grad::<f32, 3, 3>(&mut compiler, zero_gradient)?;
        impl_grad::<f32, 4, 4>(&mut compiler, zero_gradient)?;

        impl_grad::<f64, 1, 1>(&mut compiler, zero_gradient)?;
        impl_grad::<f64, 2, 1>(&mut compiler, zero_gradient)?;
        impl_grad::<f64, 3, 1>(&mut compiler, zero_gradient)?;
        impl_grad::<f64, 4, 1>(&mut compiler, zero_gradient)?;

        impl_grad::<f64, 2, 2>(&mut compiler, zero_gradient)?;
        impl_grad::<f64, 3, 3>(&mut compiler, zero_gradient)?;
        impl_grad::<f64, 4, 4>(&mut compiler, zero_gradient)?;

        compiler.compile_module(vec!["test"].into(), inline_code!(r#"
use std::println;
use std::print;
use std::PhyNum;
use std::SVector;

/// Time step size
let time_step: PhyNum<f64> = PhyNum { val: 1.0e-3, unit: "ns" };

type MyData = struct {
    a: i32,
    b: i32,
};

type BoundaryField<T, const N: usize, const DIM: usize> = struct {
    element: SVector<T, N>,
};

type BoundaryCondition<T, const N: usize, const DIM: usize> = struct {
    name: str,
    gradient: [SVector<T, N>; DIM],
};

impl<T, const N: usize, const DIM: usize> BoundaryField<T, N, DIM> {
    fn set_bc(self: Self, id: usize, bc: BoundaryCondition<T, N, DIM>) {
        print("setting boundary for BoundaryField<");
        print(N);
        print(", ");
        print(DIM);
        print("> with id ");
        println(id);
        // print(" to condition ");
        // PrintArray { data: self.element.data }.print();
    }
}

impl<T, const N: usize, const DIM: usize> BoundaryCondition<T, N, DIM> {
    ?comptime fn von_neumann(grad: [SVector<T, N>; DIM]) -> Self {
        BoundaryCondition {
            name: "von-neumann",
            gradient: grad,
        }
    }
}


impl MyData {
    fn print(self: Self) {
        println(self.a);
        println(self.b);
    }
}

impl<T> PhyNum<T> {
    fn new(val: T, unit: str) -> Self {
        PhyNum { val, unit }
    }

    fn print(self: Self) {
        print(self.val);
        print(" ");
        println(self.unit);
    }
}

fn non_dim<T>(val: PhyNum<T>) -> T {
    val.val
}

fn add_vec<const N: usize>(mut array: [f32; N], val: f32) -> [f32; N] {
    let mut i: usize = 0;
    loop {
        if i >= N { break; }
        array[i] += val;
        i += 1;
    }
    array
}

fn test_array<T, const N: usize>(array: [T; N], val: T) -> [T; N] {
    println("test_array");
    array
}

type PrintArray<T, const N: usize> = struct {
    data: [T; N],
};

impl<T, const N: usize> PrintArray<T, N> {
    fn print(self: Self) {
        print_array(self.data)
    }
}

fn print_array<T, const N: usize>(val: [T; N]) {
    let mut i: usize = 0;
    print("[");
    loop {
        if i >= N { break }
        if i != 0 {
            print(", ");
        }
        print(val[i]);
        i += 1;
    }
    println("]");
}

fn test(f: f64) -> i32 {
    println("starting test");
    let num = PhyNum::new(0.32_f64, "s");
    num.print();

    println(non_dim(PhyNum::new(42.0_f64, "m")));
    let tmp: PhyNum<f64> = std::env("T", PhyNum::new(3.0_f64, "s"));
    println(non_dim(std::env("test", PhyNum::new(0.0_f64, "m"))));
    tmp.print();

    let val = add_vec([0.0, 0.0, 1.0, 2.0], 0.1415);
    print_array(val);

    let val = test_array([0.0; 3], 1.0_f32);
    PrintArray { data: val }.print();

    MyData {
        a: 42,
        b: 23,
    }.print();


    // test some deeper going type inference
    println("");
    println("-----------------------------------------------------");
    println("");
    let grad: [SVector<f32, 3>; 3] = std::zero_gradient();
    print_array(grad[1].data);

    let field: BoundaryField<f64, 3, 3> = BoundaryField {
        element: SVector { data: [0.0; 3] },
    };

    let grad = std::zero_gradient::<f32, 3, 3>();
    field.set_bc(1, BoundaryCondition::von_neumann(std::zero_gradient()));
    0
}
        "#))?;

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), inline_code!(r#"test(3.14)"#))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);

        // test external script insert
        let script: TypedProgram<(), _> = compiler
            .compile_expr(&vec!["test"].into(), inline_code!(r#"{
            println(" -------------------------------- ");
            println(" -> externally compiled script <- ");
            println("----------------------------------");
            time_step.print();
        }"#))?;
        script.exec(&mut compiler.backend)?;
        Ok(())
    }
}
