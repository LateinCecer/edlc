/*
 * EDLc, a compiler for the EDL programming language.
 * Copyright (C) 2026  Adrian Paskert
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
//! Equivalence tests: for a core `Item`, serialize it to a blob, parse the blob with the
//! `doc_repr` mirrors, tokenize it, and assert that the concatenated token text is exactly the
//! compiler's `Display` output. This guarantees the highlighted view never changes the visible
//! signature text, *except* for the vertical layout: functions with more than three parameters
//! and named struct/union definitions are broken one parameter/member per line (with trailing
//! commas), which those cases pin down with explicit expected text instead.

use edlc_core::prelude as core;
use edlc_core::resolver::QualifierName;

use edlc_doc_server::doc_repr::parse_item;
use edlc_doc_server::signature::{item_span, span_text};

fn src() -> core::PortableModuleSrc {
    core::PortableModuleSrc::File("x.edl".into())
}

fn pos() -> core::SrcPos {
    core::SrcPos::new(1, 1, 1)
}

fn qname(parts: &[&str]) -> QualifierName {
    QualifierName::from(parts.iter().map(|s| s.to_string()).collect::<Vec<_>>())
}

fn base(name: &str) -> core::TypeDoc {
    core::TypeDoc::Base(core::TypeNameDoc::from(name.to_string()), None)
}

/// A base type reference with a properly segmented qualified name.
fn tref(parts: &[&str]) -> core::TypeDoc {
    core::TypeDoc::Base(
        core::TypeNameDoc::from(vec![core::TypeNameSegmentDoc::from(qname(parts))]),
        None,
    )
}

/// A base type reference with generic (turbofish) parameters.
fn tref_inst(parts: &[&str], params: Vec<core::EnvValueDoc>) -> core::TypeDoc {
    let mut seg = core::TypeNameSegmentDoc::from(qname(parts));
    seg.parameters = core::EnvInstDoc { params };
    core::TypeDoc::Base(core::TypeNameDoc::from(vec![seg]), None)
}

fn param(name: &str, ty: core::TypeDoc, ms: Vec<core::Modifier>) -> core::FuncParamDoc {
    core::FuncParamDoc {
        name: name.to_string(),
        pos: pos(),
        ty,
        ms: core::Modifiers::new(ms),
    }
}

fn func(
    env: core::EnvDoc,
    params: core::FuncParamsDoc,
    ret: core::TypeDoc,
    ms: Vec<core::Modifier>,
    async_return: bool,
) -> core::Item {
    core::Item::Func(core::FuncDoc {
        name: qname(&["m", "f"]),
        src: src(),
        pos: pos(),
        doc: String::new(),
        env,
        params,
        ret,
        ms: core::Modifiers::new(ms),
        async_return,
        associated_type: None,
    })
}

fn check(item: &core::Item) {
    let json = serde_json::to_string(item).unwrap();
    let parsed = parse_item(&json).unwrap();
    assert_eq!(
        span_text(&item_span(&parsed)),
        item.to_string(),
        "mismatch for blob {json}"
    );
}

/// For items whose layout is vertically wrapped, where the token text intentionally differs
/// from the compiler's `Display` output.
fn check_text(item: &core::Item, expected: &str) {
    let json = serde_json::to_string(item).unwrap();
    let parsed = parse_item(&json).unwrap();
    assert_eq!(span_text(&item_span(&parsed)), expected, "mismatch for blob {json}");
}

#[test]
fn lets() {
    for ms in [
        Vec::new(),
        vec![core::Modifier::Mut],
        vec![core::Modifier::Comptime],
        vec![core::Modifier::MaybeComptime],
        vec![core::Modifier::Async],
        vec![core::Modifier::Shared],
        vec![core::Modifier::Comptime, core::Modifier::Mut],
    ] {
        check(&core::Item::GlobalVar(core::LetDoc {
            name: qname(&["m", "v"]),
            src: src(),
            pos: pos(),
            doc: "A value.".into(),
            ty: base("u32"),
            ms: core::Modifiers::new(ms),
        }));
    }
}

#[test]
fn consts() {
    for (ms, assoc) in [
        (Vec::new(), None),
        (vec![core::Modifier::Comptime], Some(tref(&["m", "T"]))),
        (vec![core::Modifier::Shared, core::Modifier::Mut], None),
    ] {
        check(&core::Item::GlobalConst(core::ConstDoc {
            name: qname(&["m", "C"]),
            src: src(),
            pos: pos(),
            doc: String::new(),
            ty: base("str"),
            ms: core::Modifiers::new(ms),
            associated_type: assoc,
        }));
    }
}

#[test]
fn fns() {
    // No params, no return.
    check(&func(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::default(),
        core::TypeDoc::Empty,
        Vec::new(),
        false,
    ));

    // Params and return.
    check(&func(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::from(vec![
            param("x", base("u32"), vec![core::Modifier::Mut]),
            param("y", base("str"), Vec::new()),
        ]),
        base("u32"),
        Vec::new(),
        false,
    ));

    // More than three parameters: vertical layout with a trailing comma.
    check_text(
        &func(
            core::EnvDoc { params: Vec::new() },
            core::FuncParamsDoc::from(vec![
                param("a", base("u32"), Vec::new()),
                param("b", base("str"), Vec::new()),
                param("c", base("u32"), Vec::new()),
                param("d", base("str"), Vec::new()),
            ]),
            base("u32"),
            Vec::new(),
            false,
        ),
        "fn f(\n    a: u32,\n    b: str,\n    c: u32,\n    d: str,\n) -> u32",
    );

    // Generic environment with type and const parameters.
    check(&func(
        core::EnvDoc {
            params: vec![
                core::EnvParamDoc::Type {
                    name: "T".into(),
                    pos: None,
                },
                core::EnvParamDoc::Const {
                    name: "N".into(),
                    pos: None,
                    ty: base("u32"),
                },
            ],
        },
        core::FuncParamsDoc::from(vec![param("x", tref(&["T"]), Vec::new())]),
        core::TypeDoc::Tuple(vec![tref(&["T"]), base("u32")], None),
        Vec::new(),
        false,
    ));

    // Async modifier plus async return.
    check(&func(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::default(),
        base("u32"),
        vec![core::Modifier::Async],
        true,
    ));

    // Elicit return.
    check(&func(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::default(),
        core::TypeDoc::Elicit,
        Vec::new(),
        false,
    ));

    // Reference types.
    check(&func(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::from(vec![
            param(
                "a",
                core::TypeDoc::Ref(Box::new(base("str")), None),
                Vec::new(),
            ),
            param(
                "b",
                core::TypeDoc::MutRef(Box::new(base("str")), None),
                Vec::new(),
            ),
        ]),
        core::TypeDoc::Ref(Box::new(tref(&["m", "T"])), None),
        Vec::new(),
        false,
    ));

    // Array lengths: literal, const reference, elicit.
    for len in [
        core::DocConstValue::Literal(core::edl_value::EdlLiteralValue::U8(5)),
        core::DocConstValue::Const(core::TypeNameDoc::from("N".to_string())),
        core::DocConstValue::Elicit,
    ] {
        check(&func(
            core::EnvDoc { params: Vec::new() },
            core::FuncParamsDoc::default(),
            core::TypeDoc::Array(Box::new(base("u32")), len, None),
            Vec::new(),
            false,
        ));
    }

    // Slices, empty, and multi-segment type references.
    check(&func(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::from(vec![param(
            "s",
            core::TypeDoc::Slice(Box::new(base("u8")), None),
            Vec::new(),
        )]),
        core::TypeDoc::Empty,
        Vec::new(),
        false,
    ));
    check(&func(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::default(),
        core::TypeDoc::Empty,
        Vec::new(),
        false,
    ));
    check(&func(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::from(vec![param("p", tref(&["a", "b", "C"]), Vec::new())]),
        core::TypeDoc::Empty,
        Vec::new(),
        false,
    ));

    // Turbofish generic arguments, including elicits.
    check(&func(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::default(),
        tref_inst(
            &["m", "T"],
            vec![
                core::EnvValueDoc::Type {
                    ty: base("u32"),
                    pos: None,
                },
                core::EnvValueDoc::Const {
                    val: core::DocConstValue::Literal(core::edl_value::EdlLiteralValue::U32(7)),
                    pos: None,
                },
            ],
        ),
        Vec::new(),
        false,
    ));
    check(&func(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::default(),
        tref_inst(
            &["m", "T"],
            vec![
                core::EnvValueDoc::ElicitType,
                core::EnvValueDoc::ElicitConst,
            ],
        ),
        Vec::new(),
        false,
    ));
}

#[test]
fn type_defs() {
    fn tdef(
        env: core::EnvDoc,
        params: core::FuncParamsDoc,
        variant: core::TypeDefVariant,
    ) -> core::Item {
        core::Item::TypeDef(core::TypeDefDoc {
            name: qname(&["m", "S"]),
            src: src(),
            pos: pos(),
            doc: String::new(),
            env,
            params,
            variant,
        })
    }

    let member = |name: &str, ty: core::TypeDoc, ms: Vec<core::Modifier>| core::StructMemberDoc {
        name: name.to_string(),
        pos: pos(),
        doc: String::new(),
        ty,
        modifiers: core::Modifiers::new(ms),
    };

    // Alias.
    check(&tdef(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::default(),
        core::TypeDefVariant::Alias(base("u32")),
    ));

    // Zero-sized struct.
    check(&tdef(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::default(),
        core::TypeDefVariant::Struct(core::StructTypeDoc::ZeroSized),
    ));

    // Named struct, including a modified member; broken vertically with a trailing comma.
    check_text(
        &tdef(
            core::EnvDoc { params: Vec::new() },
            core::FuncParamsDoc::default(),
            core::TypeDefVariant::Struct(core::StructTypeDoc::Named(vec![
                member("x", base("u32"), Vec::new()),
                member("y", base("str"), vec![core::Modifier::Shared]),
            ])),
        ),
        "type S = struct {\n    x: u32,\n    shared  y: str,\n}",
    );

    // Tuple struct.
    check(&tdef(
        core::EnvDoc { params: Vec::new() },
        core::FuncParamsDoc::default(),
        core::TypeDefVariant::Struct(core::StructTypeDoc::Tuple(vec![base("u32"), base("str")])),
    ));

    // Enum with all three variant shapes; broken vertically, one variant per line, with the
    // named struct-variant members laid out like named structs one level deeper.
    check_text(
        &tdef(
            core::EnvDoc { params: Vec::new() },
            core::FuncParamsDoc::default(),
            core::TypeDefVariant::Enum(vec![
                core::EnumVariantDoc {
                    name: "A".into(),
                    members: core::StructTypeDoc::ZeroSized,
                },
                core::EnumVariantDoc {
                    name: "B".into(),
                    members: core::StructTypeDoc::Named(vec![member("x", base("u32"), Vec::new())]),
                },
                core::EnumVariantDoc {
                    name: "C".into(),
                    members: core::StructTypeDoc::Tuple(vec![base("u32"), base("str")]),
                },
            ]),
        ),
        "type S = enum {\n    A,\n    B {\n        x: u32,\n    },\n    C(u32, str),\n}",
    );

    // Union, including a modified member; broken vertically with a trailing comma.
    check_text(
        &tdef(
            core::EnvDoc { params: Vec::new() },
            core::FuncParamsDoc::default(),
            core::TypeDefVariant::Union(vec![
                member("a", base("u32"), Vec::new()),
                member("b", base("str"), vec![core::Modifier::Async]),
            ]),
        ),
        "type S = union {\n    a: u32,\n    async  b: str,\n}",
    );

    // Generic environment.
    check(&tdef(
        core::EnvDoc {
            params: vec![
                core::EnvParamDoc::Type {
                    name: "T".into(),
                    pos: None,
                },
                core::EnvParamDoc::Const {
                    name: "N".into(),
                    pos: None,
                    ty: base("u32"),
                },
            ],
        },
        core::FuncParamsDoc::default(),
        core::TypeDefVariant::Alias(core::TypeDoc::Array(
            Box::new(base("u32")),
            core::DocConstValue::Const(core::TypeNameDoc::from("N".to_string())),
            None,
        )),
    ));

    // Function-style parameters (core's `Display` renders them without parentheses).
    check(&tdef(
        core::EnvDoc {
            params: vec![core::EnvParamDoc::Type {
                name: "T".into(),
                pos: None,
            }],
        },
        core::FuncParamsDoc::from(vec![param("n", base("u32"), Vec::new())]),
        core::TypeDefVariant::Alias(base("u32")),
    ));
}

#[test]
fn modules() {
    check(&core::Item::Module(core::ModuleDoc {
        name: qname(&["a", "b", "c"]),
        doc: "A module.".into(),
    }));
}

#[test]
fn literals_in_type_position() {
    for lit in [
        core::edl_value::EdlLiteralValue::Usize(1),
        core::edl_value::EdlLiteralValue::Isize(-2),
        core::edl_value::EdlLiteralValue::Bool(true),
        core::edl_value::EdlLiteralValue::U8(8),
        core::edl_value::EdlLiteralValue::U16(16),
        core::edl_value::EdlLiteralValue::U32(32),
        core::edl_value::EdlLiteralValue::U64(64),
        core::edl_value::EdlLiteralValue::U128(128),
        core::edl_value::EdlLiteralValue::I8(-8),
        core::edl_value::EdlLiteralValue::I16(-16),
        core::edl_value::EdlLiteralValue::I32(-32),
        core::edl_value::EdlLiteralValue::I64(-64),
        core::edl_value::EdlLiteralValue::I128(-128),
        core::edl_value::EdlLiteralValue::Str("hi".into()),
        core::edl_value::EdlLiteralValue::Char('c'),
        core::edl_value::EdlLiteralValue::Empty(),
    ] {
        check(&core::Item::GlobalVar(core::LetDoc {
            name: qname(&["l"]),
            src: src(),
            pos: pos(),
            doc: String::new(),
            ty: core::TypeDoc::Array(
                Box::new(base("u8")),
                core::DocConstValue::Literal(lit),
                None,
            ),
            ms: core::Modifiers::default(),
        }));
    }
}
