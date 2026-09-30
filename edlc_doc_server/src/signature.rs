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
//! Structured signature rendering for the web frontend.
//!
//! A parsed doc item ([`doc_repr::Item`]) is tokenized into a tree of styled spans whose
//! concatenated text matches the stringified signature produced by `edlc_core`'s `Display`
//! impls, *except* for the vertical layout: function signatures with more than three
//! parameters, named struct/union definitions, and enum definitions (one variant per line,
//! with named struct-variant members laid out like named structs) are broken one
//! parameter/member/variant per line, mirroring `cargo fmt` (with trailing commas). The
//! [`SignatureView`] component renders the plain signature during SSR and swaps in the
//! highlighted, linked view once the blob has been parsed on the client after hydration (via
//! a `<Transition/>` fallback).

use std::collections::BTreeSet;

use leptos::prelude::*;

use crate::doc_repr::*;

// --- Token classes (CSS in style/main.css) ---

pub const C_KEYWORD: &str = "tok-keyword";
pub const C_PUNCT: &str = "tok-punct";
pub const C_OPERATOR: &str = "tok-operator";
pub const C_FN_NAME: &str = "tok-fn-name";
pub const C_VAR_NAME: &str = "tok-var-name";
pub const C_CONST_NAME: &str = "tok-const-name";
pub const C_TYPE_NAME: &str = "tok-type-name";
pub const C_PATH_SEGMENT: &str = "tok-path-segment";
pub const C_NUM: &str = "tok-num-literal";
pub const C_STR: &str = "tok-str-literal";
pub const C_CHAR: &str = "tok-char-literal";
pub const C_BOOL: &str = "tok-bool-literal";

/// Core language types and keywords that have no documentation page and must not be linked.
/// Mirrors the `CORE_*` constants in `edlc_core::lexer`.
const UNLINKABLE: &[&str] = &[
    "bool", "str", "char", "u8", "u16", "u32", "u64", "u128", "usize", "i8", "i16", "i32", "i64",
    "i128", "isize", "f32", "f64", "Self",
];

/// Context for deciding which references can be linked: the names of generic (env) parameters
/// in scope, which have no documentation page of their own.
#[derive(Debug, Clone, Default)]
pub struct Ctx {
    type_params: BTreeSet<String>,
    const_params: BTreeSet<String>,
}

impl Ctx {
    pub fn from_env(env: &EnvDoc) -> Self {
        let (mut type_params, mut const_params) = (BTreeSet::new(), BTreeSet::new());
        for p in &env.params {
            match p {
                EnvParamDoc::Type { name, .. } => {
                    type_params.insert(name.clone());
                }
                EnvParamDoc::Const { name, .. } => {
                    const_params.insert(name.clone());
                }
            }
        }
        Ctx {
            type_params,
            const_params,
        }
    }
}

/// A styled (and optionally linked) fragment of a signature.
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub class: String,
    pub link: Option<String>,
    pub text: String,
    pub children: Vec<Span>,
}

impl Span {
    fn leaf(text: impl Into<String>, class: &str) -> Self {
        Span {
            class: class.to_string(),
            link: None,
            text: text.into(),
            children: Vec::new(),
        }
    }

    fn group(class: &str, children: Vec<Span>) -> Self {
        Span {
            class: class.to_string(),
            link: None,
            text: String::new(),
            children,
        }
    }

    fn link(href: String, class: &str, children: Vec<Span>) -> Self {
        Span {
            class: class.to_string(),
            link: Some(href),
            text: String::new(),
            children,
        }
    }

    fn push(&mut self, span: Span) {
        self.children.push(span);
    }
}

/// Concatenates all text of a span tree (the plain signature text).
pub fn span_text(s: &Span) -> String {
    let mut out = s.text.clone();
    for child in &s.children {
        out.push_str(&span_text(child));
    }
    out
}

/// Collects all links (text -> href) in a span tree, depth-first.
pub fn collect_links(s: &Span) -> Vec<(String, Option<String>)> {
    let mut out = vec![(span_text(s), s.link.clone())];
    for child in &s.children {
        out.extend(collect_links(child));
    }
    out
}

// --- Tokenizer ---
//
// These functions mirror the `Display` impls in `edlc_core::documentation` exactly,
// quirk for quirk, so the highlighted text always matches the stringified signature.

/// Tokenizes a whole item into a span tree.
pub fn item_span(item: &Item) -> Span {
    let mut out = Span::group("", Vec::new());
    match item {
        Item::GlobalVar(d) => let_span(d, &mut out),
        Item::GlobalConst(d) => const_span(d, &mut out),
        Item::Func(d) => func_span(d, &mut out),
        Item::TypeDef(d) => type_def_span(d, &mut out),
        Item::Module(d) => module_span(d, &mut out),
    }
    out
}

fn kw(out: &mut Span, text: &str) {
    out.push(Span::leaf(text, C_KEYWORD));
}

fn punct(out: &mut Span, text: &str) {
    out.push(Span::leaf(text, C_PUNCT));
}

fn op(out: &mut Span, text: &str) {
    out.push(Span::leaf(text, C_OPERATOR));
}

fn plain(out: &mut Span, text: &str) {
    out.push(Span::leaf(text, ""));
}

fn ident(out: &mut Span, text: &str, class: &str) {
    out.push(Span::leaf(text, class));
}

/// `let {ms}{name}: {ty}`
fn let_span(d: &LetDoc, out: &mut Span) {
    kw(out, "let");
    plain(out, " ");
    modifiers(&d.ms, out);
    ident(out, d.name.last().unwrap_or_default(), C_VAR_NAME);
    punct(out, ": ");
    let ctx = Ctx::default();
    type_span(&d.ty, &ctx, out);
}

/// `const {ms}{name}: {ty}`
fn const_span(d: &ConstDoc, out: &mut Span) {
    kw(out, "const");
    plain(out, " ");
    modifiers(&d.ms, out);
    ident(out, d.name.last().unwrap_or_default(), C_CONST_NAME);
    punct(out, ": ");
    let ctx = Ctx::default();
    type_span(&d.ty, &ctx, out);
}

/// `{ms}fn {name}{env}({params})[ -> async {ret}]`
///
/// With more than three parameters the argument list is broken vertically, mirroring
/// `cargo fmt`'s layout: one parameter per indented line, a trailing comma, and the
/// closing parenthesis on its own line.
fn func_span(d: &FuncDoc, out: &mut Span) {
    let ctx = Ctx::from_env(&d.env);
    modifiers(&d.ms, out);
    kw(out, "fn");
    plain(out, " ");
    ident(out, d.name.last().unwrap_or_default(), C_FN_NAME);
    env_span(&d.env, &ctx, out);
    let params = &d.params.0;
    if params.len() > 3 {
        punct(out, "(");
        plain(out, "\n");
        for p in params {
            plain(out, "    ");
            param_span(p, &ctx, out);
            punct(out, ",");
            plain(out, "\n");
        }
        punct(out, ")");
    } else {
        punct(out, "(");
        for (i, p) in params.iter().enumerate() {
            param_span(p, &ctx, out);
            if i + 1 < params.len() {
                punct(out, ", ");
            }
        }
        punct(out, ")");
    }
    if !matches!(d.ret, TypeDoc::Empty) {
        punct(out, " -> ");
        if d.async_return {
            kw(out, "async");
            plain(out, " ");
        }
        type_span(&d.ret, &ctx, out);
    }
}

/// `type {name}{env}{params} = {variant}`
fn type_def_span(d: &TypeDefDoc, out: &mut Span) {
    let ctx = Ctx::from_env(&d.env);
    kw(out, "type");
    plain(out, " ");
    ident(out, d.name.last().unwrap_or_default(), C_TYPE_NAME);
    env_span(&d.env, &ctx, out);
    // NOTE: core's `Display` renders function-style parameters without parentheses.
    let params = &d.params.0;
    for (i, p) in params.iter().enumerate() {
        param_span(p, &ctx, out);
        if i + 1 < params.len() {
            punct(out, ", ");
        }
    }
    punct(out, " = ");
    variant_span(&d.variant, &ctx, out);
}

/// `mod {name}`
fn module_span(d: &ModuleDoc, out: &mut Span) {
    kw(out, "mod");
    plain(out, " ");
    let full = d.name.display();
    let mut children = Vec::new();
    for (i, seg) in d.name.path.iter().enumerate() {
        children.push(Span::leaf(seg, C_PATH_SEGMENT));
        if i + 1 < d.name.path.len() {
            children.push(Span::leaf("::", C_PUNCT));
        }
    }
    out.push(Span::link(format!("/module/{full}"), "", children));
}

/// Each modifier is followed by a trailing space (`"{m} "`).
fn modifiers(ms: &Modifiers, out: &mut Span) {
    for m in &ms.0 {
        match m {
            Modifier::Comptime => kw(out, "comptime"),
            Modifier::MaybeComptime => {
                op(out, "?");
                kw(out, "comptime");
            }
            Modifier::Mut => kw(out, "mut"),
            Modifier::Async => kw(out, "async"),
            Modifier::Shared => kw(out, "shared"),
        }
        plain(out, " ");
    }
}

/// `{ms}{name}: {ty}`
fn param_span(p: &FuncParamDoc, ctx: &Ctx, out: &mut Span) {
    modifiers(&p.ms, out);
    ident(out, &p.name, C_VAR_NAME);
    punct(out, ": ");
    type_span(&p.ty, ctx, out);
}

/// `<T, const N: u32>` (empty when there are no parameters).
fn env_span(env: &EnvDoc, ctx: &Ctx, out: &mut Span) {
    if env.params.is_empty() {
        return;
    }
    punct(out, "<");
    for (i, p) in env.params.iter().enumerate() {
        match p {
            EnvParamDoc::Type { name, .. } => ident(out, name, C_TYPE_NAME),
            EnvParamDoc::Const { name, ty, .. } => {
                kw(out, "const");
                plain(out, " ");
                ident(out, name, C_CONST_NAME);
                punct(out, ": ");
                type_span(ty, ctx, out);
            }
        }
        if i + 1 < env.params.len() {
            punct(out, ", ");
        }
    }
    punct(out, ">");
}

/// `<u32, _>` (empty when there are no values).
fn env_inst_span(inst: &EnvInstDoc, ctx: &Ctx, out: &mut Span) {
    if inst.params.is_empty() {
        return;
    }
    punct(out, "<");
    for (i, v) in inst.params.iter().enumerate() {
        match v {
            EnvValueDoc::Type { ty, .. } => type_span(ty, ctx, out),
            EnvValueDoc::Const { val, .. } => const_value_span(val, ctx, out),
            EnvValueDoc::ElicitType | EnvValueDoc::ElicitConst => punct(out, "_"),
        }
        if i + 1 < inst.params.len() {
            punct(out, ", ");
        }
    }
    punct(out, ">");
}

fn const_value_span(v: &DocConstValue, ctx: &Ctx, out: &mut Span) {
    match v {
        DocConstValue::Const(tn) => type_name_span(tn, ctx, true, out),
        DocConstValue::Literal(l) => literal_span(l, out),
        DocConstValue::Elicit => punct(out, "_"),
    }
}

/// `[u32; 5_u8]`, `[str]`, `&T`, `&mut T`, `()`, `(u32, str, )`, `_`.
fn type_span(ty: &TypeDoc, ctx: &Ctx, out: &mut Span) {
    match ty {
        TypeDoc::Base(n, _) => type_name_span(n, ctx, false, out),
        TypeDoc::Array(base, len, _) => {
            punct(out, "[");
            type_span(base, ctx, out);
            punct(out, "; ");
            const_value_span(len, ctx, out);
            punct(out, "]");
        }
        TypeDoc::Slice(base, _) => {
            punct(out, "[");
            type_span(base, ctx, out);
            punct(out, "]");
        }
        TypeDoc::Ref(base, _) => {
            op(out, "&");
            type_span(base, ctx, out);
        }
        TypeDoc::MutRef(base, _) => {
            op(out, "&");
            kw(out, "mut");
            plain(out, " ");
            type_span(base, ctx, out);
        }
        TypeDoc::Empty => punct(out, "()"),
        TypeDoc::Tuple(items, _) => {
            // NOTE: core's `Display` writes a trailing `, ` after *every* element,
            // including the last one.
            punct(out, "(");
            for t in items {
                type_span(t, ctx, out);
                punct(out, ", ");
            }
            punct(out, ")");
        }
        TypeDoc::Elicit => punct(out, "_"),
    }
}

/// A qualified type (or const) name, with its last path segment of each component colored as a
/// type name. When `is_const_ref`, the reference may be a generic const parameter; otherwise it
/// may be a core type. Linkable references point at the item's documentation page.
fn type_name_span(n: &TypeNameDoc, ctx: &Ctx, is_const_ref: bool, out: &mut Span) {
    let segments = &n.0;
    if segments.is_empty() {
        return;
    }
    let mut path: Vec<&str> = Vec::new();
    for seg in segments {
        path.extend(seg.name.path.iter().map(|s| s.as_str()));
    }
    let full = path.join("::");
    let tail = path.last().copied().unwrap_or("");
    let linkable = if is_const_ref {
        !ctx.const_params.contains(tail)
    } else {
        !UNLINKABLE.contains(&tail) && !ctx.type_params.contains(tail)
    };

    let mut children: Vec<Span> = Vec::new();
    for (i, seg) in segments.iter().enumerate() {
        qual_name_span(&seg.name, &mut children);
        if i + 1 < segments.len() {
            children.push(Span::leaf("::", C_PUNCT));
        }
        if !seg.parameters.params.is_empty() {
            children.push(Span::leaf("::", C_PUNCT));
            let mut inst = Span::group("", Vec::new());
            env_inst_span(&seg.parameters, ctx, &mut inst);
            children.push(inst);
        }
    }

    if linkable {
        out.push(Span::link(format!("/item/{full}"), C_TYPE_NAME, children));
    } else {
        out.push(Span::group(C_TYPE_NAME, children));
    }
}

/// A qualified name; the last path segment is colored as a type name, the rest as path segments.
fn qual_name_span(qn: &QualifierName, out: &mut Vec<Span>) {
    for (i, seg) in qn.path.iter().enumerate() {
        let class = if i + 1 == qn.path.len() {
            C_TYPE_NAME
        } else {
            C_PATH_SEGMENT
        };
        out.push(Span::leaf(seg, class));
        if i + 1 < qn.path.len() {
            out.push(Span::leaf("::", C_PUNCT));
        }
    }
}

/// `5_u8`, `"str"`, `'c'`, `true`, `()`.
fn literal_span(v: &EdlLiteralValue, out: &mut Span) {
    match v {
        EdlLiteralValue::Usize(x) => num(out, format!("{x}_usize")),
        EdlLiteralValue::Isize(x) => num(out, format!("{x}_isize")),
        EdlLiteralValue::U8(x) => num(out, format!("{x}_u8")),
        EdlLiteralValue::U16(x) => num(out, format!("{x}_u16")),
        EdlLiteralValue::U32(x) => num(out, format!("{x}_u32")),
        EdlLiteralValue::U64(x) => num(out, format!("{x}_u64")),
        EdlLiteralValue::U128(x) => num(out, format!("{x}_u128")),
        EdlLiteralValue::I8(x) => num(out, format!("{x}_i8")),
        EdlLiteralValue::I16(x) => num(out, format!("{x}_i16")),
        EdlLiteralValue::I32(x) => num(out, format!("{x}_i32")),
        EdlLiteralValue::I64(x) => num(out, format!("{x}_i64")),
        EdlLiteralValue::I128(x) => num(out, format!("{x}_i128")),
        EdlLiteralValue::Bool(b) => {
            out.push(Span::leaf(if *b { "true" } else { "false" }, C_BOOL));
        }
        EdlLiteralValue::Str(s) => out.push(Span::leaf(format!("\"{s}\""), C_STR)),
        EdlLiteralValue::Char(c) => {
            out.push(Span::leaf(
                format!("'{}'", c.to_string().replace("\n", "\\n")),
                C_CHAR,
            ));
        }
        EdlLiteralValue::Empty() => punct(out, "()"),
    }
}

fn num(out: &mut Span, text: String) {
    out.push(Span::leaf(text, C_NUM));
}

/// `{name}: {ty}` (with leading modifiers plus an extra space when present).
fn member_span(m: &StructMemberDoc, ctx: &Ctx, out: &mut Span) {
    if m.modifiers.0.is_empty() {
        ident(out, &m.name, C_VAR_NAME);
    } else {
        modifiers(&m.modifiers, out);
        plain(out, " ");
        ident(out, &m.name, C_VAR_NAME);
    }
    punct(out, ": ");
    type_span(&m.ty, ctx, out);
}

/// The sequence of named struct/union members, either inline (`m: T, n: U`) or vertically
/// (one member per indented line with a trailing comma), without the surrounding braces.
/// In the vertical layout each member line is indented by `indent + 4` spaces.
fn named_members_span(
    ms: &[StructMemberDoc],
    ctx: &Ctx,
    out: &mut Span,
    vertical: bool,
    indent: usize,
) {
    if vertical {
        let line_indent = " ".repeat(indent + 4);
        plain(out, "\n");
        for m in ms {
            plain(out, &line_indent);
            member_span(m, ctx, out);
            punct(out, ",");
            plain(out, "\n");
        }
    } else {
        for (i, m) in ms.iter().enumerate() {
            member_span(m, ctx, out);
            if i + 1 < ms.len() {
                punct(out, ", ");
            }
        }
    }
}

/// `{ m: T, n: U }`, `(T, U)`, or empty.
///
/// With `vertical`, named members are broken one per indented line (mirroring `cargo fmt`'s
/// struct layout) with a trailing comma and the closing brace on its own line at `indent`
/// spaces: `{\n    m: T,\n}`.
fn struct_type_span(s: &StructTypeDoc, ctx: &Ctx, out: &mut Span, vertical: bool, indent: usize) {
    match s {
        StructTypeDoc::Named(ms) => {
            if vertical {
                punct(out, "{");
                named_members_span(ms, ctx, out, true, indent);
                if indent > 0 {
                    plain(out, &" ".repeat(indent));
                }
                punct(out, "}");
            } else {
                punct(out, "{ ");
                named_members_span(ms, ctx, out, false, 0);
                punct(out, " }");
            }
        }
        StructTypeDoc::Tuple(ts) => {
            punct(out, "(");
            for (i, t) in ts.iter().enumerate() {
                type_span(t, ctx, out);
                if i + 1 < ts.len() {
                    punct(out, ", ");
                }
            }
            punct(out, ")");
        }
        StructTypeDoc::ZeroSized => {}
    }
}

fn enum_variant_span(v: &EnumVariantDoc, ctx: &Ctx, out: &mut Span) {
    ident(out, &v.name, C_TYPE_NAME);
    match &v.members {
        StructTypeDoc::ZeroSized => {}
        // Named members get the same vertical treatment as named structs, one level deeper
        // than the variant itself.
        StructTypeDoc::Named(_) => {
            plain(out, " ");
            struct_type_span(&v.members, ctx, out, true, 4);
        }
        StructTypeDoc::Tuple(_) => struct_type_span(&v.members, ctx, out, false, 0),
    }
}

/// `struct { ... }`, `enum { ... }`, `union { ... }`, or an alias type.
///
/// Named struct and union definitions are broken vertically (one member per line), and enum
/// definitions one variant per line; struct variants with named members use the same vertical
/// member layout as named structs.
fn variant_span(v: &TypeDefVariant, ctx: &Ctx, out: &mut Span) {
    match v {
        TypeDefVariant::Struct(s) => match s {
            StructTypeDoc::ZeroSized => kw(out, "struct"),
            other => {
                kw(out, "struct");
                plain(out, " ");
                struct_type_span(other, ctx, out, true, 0);
            }
        },
        TypeDefVariant::Enum(vs) => {
            kw(out, "enum");
            plain(out, " ");
            punct(out, "{");
            for var in vs.iter() {
                plain(out, "\n    ");
                enum_variant_span(var, ctx, out);
                punct(out, ",");
            }
            plain(out, "\n");
            punct(out, "}");
        }
        TypeDefVariant::Union(ms) => {
            kw(out, "union");
            plain(out, " ");
            punct(out, "{");
            named_members_span(ms, ctx, out, true, 0);
            punct(out, "}");
        }
        TypeDefVariant::Alias(ty) => type_span(ty, ctx, out),
    }
}

// --- View rendering ---

/// Renders a span tree into a Leptos view (spans and links).
pub fn span_view(s: &Span) -> AnyView {
    let children: Vec<AnyView> = if s.children.is_empty() {
        vec![s.text.clone().into_view().into_any()]
    } else {
        s.children.iter().map(span_view).collect()
    };
    if let Some(href) = &s.link {
        if s.class.is_empty() {
            view! { <a href={href.clone()}>{children}</a> }.into_any()
        } else {
            view! { <a class={s.class.clone()} href={href.clone()}>{children}</a> }.into_any()
        }
    } else if s.class.is_empty() {
        children.into_view().into_any()
    } else {
        view! { <span class={s.class.clone()}>{children}</span> }.into_any()
    }
}

/// Renders a doc item's signature with syntax highlighting.
///
/// The `blob` is the serde-JSON serialization of the original core `Item`; `plain` is the
/// pre-stringified signature. During SSR only the plain signature is rendered (fast, and the
/// text users see before hydration). After hydration, the blob is parsed locally and the
/// highlighted view is swapped in via `<Transition/>`. If parsing fails, the plain signature
/// is kept.
#[component]
pub fn SignatureView(blob: String, plain: String, #[prop(into)] class: String) -> impl IntoView {
    let parsed = LocalResource::new(move || {
        let blob = blob.clone();
        async move { parse_item(&blob).ok() }
    });

    let (fb_class, fb_plain) = (class.clone(), plain.clone());

    view! {
        <Transition
            fallback=move || view! {
                <pre class={fb_class}>{fb_plain}</pre>
            }
        >
            {move || {
                let cls = format!("{class} sig");
                match parsed.get().flatten() {
                    Some(item) => {
                        let span = item_span(&item);
                        let tokens = span.children.iter().map(span_view).collect::<Vec<_>>();
                        view! { <pre class={cls}>{tokens}</pre> }.into_any()
                    }
                    None => view! { <pre class={cls}>{plain.clone()}</pre> }.into_any(),
                }
            }}
        </Transition>
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn base(name: &str) -> TypeDoc {
        TypeDoc::Base(
            TypeNameDoc(vec![TypeNameSegmentDoc {
                name: QualifierName {
                    path: vec![name.to_string()],
                },
                parameters: EnvInstDoc::default(),
                pos: None,
            }]),
            None,
        )
    }

    fn func(
        env: Vec<EnvParamDoc>,
        params: Vec<FuncParamDoc>,
        ret: TypeDoc,
        ms: Vec<Modifier>,
        async_return: bool,
    ) -> Item {
        Item::Func(FuncDoc {
            name: QualifierName {
                path: vec!["m".into(), "f".into()],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: SrcPos {
                line: 1,
                col: 1,
                size: 1,
            },
            doc: String::new(),
            env: EnvDoc { params: env },
            params: FuncParamsDoc(params),
            ret,
            ms: Modifiers(ms),
            async_return,
            associated_type: None,
        })
    }

    fn param(name: &str, ty: TypeDoc, ms: Vec<Modifier>) -> FuncParamDoc {
        FuncParamDoc {
            name: name.to_string(),
            pos: SrcPos {
                line: 1,
                col: 1,
                size: 1,
            },
            ty,
            ms: Modifiers(ms),
        }
    }

    #[test]
    fn let_tokens() {
        let item = Item::GlobalVar(LetDoc {
            name: QualifierName {
                path: vec!["m".into(), "v".into()],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: SrcPos {
                line: 1,
                col: 1,
                size: 1,
            },
            doc: String::new(),
            ty: TypeDoc::Array(
                Box::new(base("u32")),
                DocConstValue::Literal(EdlLiteralValue::U8(5)),
                None,
            ),
            ms: Modifiers(vec![Modifier::Mut]),
        });
        let span = item_span(&item);
        assert_eq!(span_text(&span), "let mut v: [u32; 5_u8]");
    }

    #[test]
    fn func_tokens_async() {
        let item = func(
            vec![
                EnvParamDoc::Type {
                    name: "T".into(),
                    pos: None,
                },
                EnvParamDoc::Const {
                    name: "N".into(),
                    pos: None,
                    ty: base("u32"),
                },
            ],
            vec![
                param("x", base("T"), vec![Modifier::Mut]),
                param("y", base("str"), vec![]),
            ],
            TypeDoc::Tuple(vec![base("T"), base("u32")], None),
            vec![Modifier::Async],
            true,
        );
        let span = item_span(&item);
        // Mirrors core's Display, including the trailing comma in the tuple type.
        assert_eq!(
            span_text(&span),
            "async fn f<T, const N: u32>(mut x: T, y: str) -> async (T, u32, )"
        );
    }

    #[test]
    fn type_def_tokens() {
        let item = Item::TypeDef(TypeDefDoc {
            name: QualifierName {
                path: vec!["m".into(), "S".into()],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: SrcPos {
                line: 1,
                col: 1,
                size: 1,
            },
            doc: String::new(),
            env: EnvDoc {
                params: vec![EnvParamDoc::Const {
                    name: "N".into(),
                    pos: None,
                    ty: base("u32"),
                }],
            },
            params: FuncParamsDoc(vec![param("n", base("u32"), vec![])]),
            variant: TypeDefVariant::Struct(StructTypeDoc::Named(vec![StructMemberDoc {
                name: "x".into(),
                pos: SrcPos {
                    line: 1,
                    col: 1,
                    size: 1,
                },
                doc: String::new(),
                ty: TypeDoc::Array(
                    Box::new(base("u8")),
                    DocConstValue::Const(TypeNameDoc(vec![TypeNameSegmentDoc {
                        name: QualifierName {
                            path: vec!["N".into()],
                        },
                        parameters: EnvInstDoc::default(),
                        pos: None,
                    }])),
                    None,
                ),
                modifiers: Modifiers(vec![Modifier::Shared]),
            }])),
        });
        let span = item_span(&item);
        // Mirrors core's Display, including the missing parentheses around the function-style
        // parameters and the double space after the member modifier; the named struct is
        // broken vertically with a trailing comma.
        assert_eq!(
            span_text(&span),
            "type S<const N: u32>n: u32 = struct {\n    shared  x: [u8; N],\n}"
        );
    }

    #[test]
    fn func_params_vertical() {
        let item = func(
            vec![],
            vec![
                param("a", base("u32"), vec![]),
                param("b", base("str"), vec![]),
                param("c", base("m::T"), vec![Modifier::Mut]),
                param("d", base("u8"), vec![]),
            ],
            base("u32"),
            vec![],
            false,
        );
        let span = item_span(&item);
        // More than three parameters: vertical layout, one parameter per indented line,
        // trailing comma, closing parenthesis on its own line.
        assert_eq!(
            span_text(&span),
            "fn f(\n    a: u32,\n    b: str,\n    mut c: m::T,\n    d: u8,\n) -> u32"
        );
    }

    #[test]
    fn func_params_inline_at_threshold() {
        let item = func(
            vec![],
            vec![
                param("a", base("u32"), vec![]),
                param("b", base("str"), vec![]),
                param("c", base("u8"), vec![]),
            ],
            base("u32"),
            vec![],
            false,
        );
        let span = item_span(&item);
        // Exactly three parameters stay on one line.
        assert_eq!(span_text(&span), "fn f(a: u32, b: str, c: u8) -> u32");
    }

    #[test]
    fn union_members_vertical() {
        let member = |name: &str, ty: TypeDoc| StructMemberDoc {
            name: name.into(),
            pos: SrcPos {
                line: 1,
                col: 1,
                size: 1,
            },
            doc: String::new(),
            ty,
            modifiers: Modifiers(vec![]),
        };
        let item = Item::TypeDef(TypeDefDoc {
            name: QualifierName {
                path: vec!["m".into(), "U".into()],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: SrcPos {
                line: 1,
                col: 1,
                size: 1,
            },
            doc: String::new(),
            env: EnvDoc::default(),
            params: FuncParamsDoc(vec![]),
            variant: TypeDefVariant::Union(vec![member("a", base("u32")), member("b", base("f32"))]),
        });
        let span = item_span(&item);
        assert_eq!(span_text(&span), "type U = union {\n    a: u32,\n    b: f32,\n}");
    }

    #[test]
    fn tuple_struct_stays_inline() {
        let item = Item::TypeDef(TypeDefDoc {
            name: QualifierName {
                path: vec!["m".into(), "W".into()],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: SrcPos {
                line: 1,
                col: 1,
                size: 1,
            },
            doc: String::new(),
            env: EnvDoc::default(),
            params: FuncParamsDoc(vec![]),
            variant: TypeDefVariant::Struct(StructTypeDoc::Tuple(vec![
                base("f32"),
                base("u32"),
            ])),
        });
        let span = item_span(&item);
        assert_eq!(span_text(&span), "type W = struct (f32, u32)");
    }

    #[test]
    fn enum_variants_broken_vertically() {
        let item = Item::TypeDef(TypeDefDoc {
            name: QualifierName {
                path: vec!["m".into(), "E".into()],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: SrcPos {
                line: 1,
                col: 1,
                size: 1,
            },
            doc: String::new(),
            env: EnvDoc::default(),
            params: FuncParamsDoc(vec![]),
            variant: TypeDefVariant::Enum(vec![
                EnumVariantDoc {
                    name: "Unit".into(),
                    members: StructTypeDoc::ZeroSized,
                },
                EnumVariantDoc {
                    name: "Wrapped".into(),
                    members: StructTypeDoc::Named(vec![StructMemberDoc {
                        name: "value".into(),
                        pos: SrcPos {
                            line: 1,
                            col: 1,
                            size: 1,
                        },
                        doc: String::new(),
                        ty: base("f32"),
                        modifiers: Modifiers(vec![]),
                    }]),
                },
                EnumVariantDoc {
                    name: "Tup".into(),
                    members: StructTypeDoc::Tuple(vec![base("f32"), base("u32")]),
                },
            ]),
        });
        let span = item_span(&item);
        // One variant per line with a trailing comma; the named struct-variant members use
        // the same vertical layout as named structs, one level deeper.
        assert_eq!(
            span_text(&span),
            "type E = enum {\n    Unit,\n    Wrapped {\n        value: f32,\n    },\n    Tup(f32, u32),\n}"
        );
    }

    #[test]
    fn type_links() {
        let item = func(
            vec![EnvParamDoc::Type {
                name: "T".into(),
                pos: None,
            }],
            vec![
                param("a", base("T"), vec![]),
                param("b", base("u32"), vec![]),
                param(
                    "c",
                    TypeDoc::Base(
                        TypeNameDoc(vec![
                            TypeNameSegmentDoc {
                                name: QualifierName {
                                    path: vec!["m".into(), "T".into()],
                                },
                                parameters: EnvInstDoc::default(),
                                pos: None,
                            },
                            TypeNameSegmentDoc {
                                name: QualifierName {
                                    path: vec!["U".into()],
                                },
                                parameters: EnvInstDoc::default(),
                                pos: None,
                            },
                        ]),
                        None,
                    ),
                    vec![],
                ),
            ],
            base("m::T"),
            vec![],
            false,
        );
        let span = item_span(&item);
        let links: Vec<_> = collect_links(&span)
            .into_iter()
            .filter(|(_, l)| l.is_some())
            .collect();
        // `T` is a generic parameter (no link), `u32` is a core type (no link); the
        // qualified references link to their item pages.
        assert_eq!(links.len(), 2);
        assert_eq!(
            links[0],
            ("m::T::U".to_string(), Some("/item/m::T::U".to_string()))
        );
        assert_eq!(
            links[1],
            ("m::T".to_string(), Some("/item/m::T".to_string()))
        );
    }

    #[test]
    fn module_links() {
        let item = Item::Module(ModuleDoc {
            name: QualifierName {
                path: vec!["a".into(), "b".into()],
            },
            doc: String::new(),
        });
        let span = item_span(&item);
        let links: Vec<_> = collect_links(&span)
            .into_iter()
            .filter(|(_, l)| l.is_some())
            .collect();
        assert_eq!(
            links,
            vec![("a::b".to_string(), Some("/module/a::b".to_string()))]
        );
        assert_eq!(span_text(&span), "mod a::b");
    }

    #[test]
    fn elicit_and_refs() {
        let item = Item::GlobalVar(LetDoc {
            name: QualifierName {
                path: vec!["v".into()],
            },
            src: PortableModuleSrc::File("x.edl".into()),
            pos: SrcPos {
                line: 1,
                col: 1,
                size: 1,
            },
            doc: String::new(),
            ty: TypeDoc::MutRef(Box::new(TypeDoc::Elicit), None),
            ms: Modifiers(vec![]),
        });
        assert_eq!(span_text(&item_span(&item)), "let v: &mut _");
    }
}
