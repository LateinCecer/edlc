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
//! Leptos application: shell, router, and page components.
//!
//! Layout inspired by docs.rs: a top search bar, a left sidebar with module navigation, and a
//! main content area showing item signatures and doc text.

use leptos::attr::custom::custom_attribute;
use leptos::hydration::{AutoReload, HydrationScripts};
use leptos::prelude::*;
use leptos_meta::{provide_meta_context, MetaTags, Stylesheet, Title};
use leptos_router::{
    components::{Route, Router, Routes, A},
    hooks::{use_navigate, use_params_map, use_query_map},
    NavigateOptions, ParamSegment, StaticSegment,
};

use wasm_bindgen::JsCast;

use crate::doc_repr;
use crate::server::{get_doc, get_module_items, list_modules, search_docs, DocError, DocSummary};
use crate::signature::SignatureView;

/// How long to wait after the last keystroke before (re)running a search. Rendering the
/// result iframes is not free, so live search is debounced: the fetch only fires once the
/// user pauses for this long (see `SearchPage`).
const SEARCH_DEBOUNCE: std::time::Duration = std::time::Duration::from_millis(300);

/// Extra height (px) added to a doc iframe on top of its content's measured `scrollHeight`.
/// The content ends up a few px taller than the iframe's viewport (body margins, sub-pixel
/// rounding), which otherwise shows a sliver of inner scrollbar; this padding keeps it clear.
const DOC_IFRAME_HEIGHT_PAD: i32 = 10;

/// Sizes the rendered-doc `<iframe>` to the height of its content, so the typeset
/// doc shows without an inner scrollbar. The iframe is same-origin (it loads
/// `/doc-html/<name>` from this server), so its `contentDocument` is reachable.
fn set_doc_iframe_height(iframe: &web_sys::HtmlIFrameElement) {
    let Some(doc) = iframe.content_document() else {
        return;
    };
    // `ready_state` is a string getter in web-sys ("loading" | "interactive" | "complete").
    if doc.ready_state() != "complete" {
        return;
    }
    let Some(body) = doc.body() else {
        return;
    };
    let height = body.scroll_height() + DOC_IFRAME_HEIGHT_PAD;
    iframe.set_height(&format!("{height}px"));
}

/// Sizes every rendered-doc `<iframe>` on the page to its content. Used on window resize,
/// where each iframe's width — and therefore its content's height — changes. There can be
/// several at once (one per search result), so this iterates all of them.
fn sync_current_doc_iframes() {
    let Some(window) = web_sys::window() else {
        return;
    };
    let Some(doc) = window.document() else {
        return;
    };
    let Ok(iframes) = doc.query_selector_all("iframe.doc-html") else {
        return;
    };
    let count = iframes.length();
    for i in 0..count {
        let Some(node) = iframes.item(i) else {
            continue;
        };
        let Ok(iframe) = node.dyn_into::<web_sys::HtmlIFrameElement>() else {
            continue;
        };
        set_doc_iframe_height(&iframe);
    }
}

/// The HTML shell. Called from the server to produce the initial HTML document.
#[cfg(feature = "ssr")]
pub fn shell(options: LeptosOptions) -> impl IntoView {
    view! {
        <!DOCTYPE html>
        <html lang="en">
            <head>
                <meta charset="utf-8"/>
                <meta name="viewport" content="width=device-width, initial-scale=1"/>
                <AutoReload options=options.clone() />
                <HydrationScripts options/>
                <MetaTags/>
            </head>
            <body>
                <App/>
            </body>
        </html>
    }
}

/// The root application component.
#[component]
pub fn App() -> impl IntoView {
    provide_meta_context();

    // Client-only: keep the rendered-doc `<iframe>` sized to its content when the window
    // is resized (its width — and so its content's height — changes with it). The effect
    // reads no reactive values, so it runs exactly once on mount.
    Effect::new(move || {
        sync_current_doc_iframes();
        if let Some(window) = web_sys::window() {
            let handler =
                wasm_bindgen::prelude::Closure::<dyn FnMut(web_sys::Event)>::new(
                    move |_ev: web_sys::Event| {
                        sync_current_doc_iframes();
                    },
                );
            let _ = window
                .add_event_listener_with_callback("resize", handler.as_ref().unchecked_ref());
            handler.forget();
        }
    });

    view! {
        <Stylesheet id="leptos" href="/pkg/edlc_doc_server.css"/>
        <Title text="EDL Documentation"/>
        <Router>
            <div class="app">
                <Sidebar/>
                <div class="content-wrapper">
                    <SearchBar/>
                    <main class="main">
                        <Routes fallback=|| "Page not found.".into_view()>
                            <Route path=StaticSegment("") view=HomePage/>
                            <Route path=StaticSegment("search") view=SearchPage/>
                            <Route path=(StaticSegment("item"), ParamSegment("name")) view=ItemPage/>
                            <Route path=(StaticSegment("module"), ParamSegment("name")) view=ModulePage/>
                            <Route path=StaticSegment("error") view=ErrorPage/>
                        </Routes>
                    </main>
                </div>
            </div>
        </Router>
    }
}

// --- Error display ---

/// Shared error display, rendered inline where a resource failed (SSR) and on the `/error` page.
#[component]
fn ErrorView(message: String) -> impl IntoView {
    view! {
        <div class="error-page">
            <h1>"Error"</h1>
            <p class="error-message">{message}</p>
            <p>"Something went wrong while loading the documentation. Please try again, or go back to the home page."</p>
            <A href="/">"Back to home"</A>
        </div>
    }
}

/// Dedicated error page at `/error?message=...`.
#[component]
fn ErrorPage() -> impl IntoView {
    let query_map = use_query_map();
    let message = query_map
        .get()
        .get("message")
        .unwrap_or_else(|| "An unknown error occurred.".to_string());

    view! {
        <ErrorView message={message}/>
    }
}

/// On the client, navigates to `/error?message=<percent-encoded>` whenever the given reader
/// yields an error message. Effects are disabled in the `ssr` build, so the SSR output (which
/// renders [`ErrorView`] inline) is unaffected.
fn redirect_on_error(read: impl Fn() -> Option<String> + 'static) {
    let navigate = use_navigate();
    Effect::new(move || {
        if let Some(message) = read() {
            navigate(
                &format!("/error?message={}", urlencoding::encode(&message)),
                Default::default(),
            );
        }
    });
}

// --- Sidebar ---

/// Left sidebar showing all modules (like docs.rs crate navigation).
#[component]
fn Sidebar() -> impl IntoView {
    let modules = Resource::new(move || (), |_| list_modules());
    redirect_on_error(move || {
        modules
            .get()
            .and_then(|res| res.err())
            .map(|e| e.to_string())
    });

    view! {
        <aside class="sidebar">
            <div class="sidebar-header">
                <h1>"EDL Docs"</h1>
            </div>
            <Suspense fallback=|| "Loading...".into_view()>
                {move || {
                    modules.get().map(|res| match res {
                        Err(e) => view! { <ErrorView message={e.to_string()}/> }.into_any(),
                        Ok(mods) => view! {
                            <ul class="module-list">
                                {mods.into_iter().map(|m| {
                                    view! {
                                        <li>
                                            <A href=format!("/module/{}", m.qual_name)>
                                                {m.qual_name}
                                            </A>
                                        </li>
                                    }
                                }).collect::<Vec<_>>()}
                            </ul>
                        }
                        .into_any(),
                    })
                }}
            </Suspense>
        </aside>
    }
}

// --- Search bar ---

/// Top search bar. Navigates to `/search?q=...` on every keystroke (replacing the current
/// history entry) so the search page's results update live as the user types.
#[component]
fn SearchBar() -> impl IntoView {
    let query_map = use_query_map();
    // Start from the current `?q` param, so a direct load of `/search?q=...` pre-fills the box.
    let (query, set_query) = signal(query_map.get().get("q").unwrap_or_default());
    let navigate = use_navigate();

    // Keep the input in sync when the URL changes externally (browser back/forward). Input-driven
    // navigation sets both the signal and the URL to the same value, so this is a no-op then.
    Effect::new(move || {
        let url_q = query_map.get().get("q").unwrap_or_default();
        if url_q != query.get_untracked() {
            set_query(url_q);
        }
    });

    view! {
        <div class="search-bar">
            <input
                type="text"
                placeholder="Search documentation..."
                prop:value=move || query.get()
                on:input=move |ev| {
                    let v = event_target_value(&ev);
                    set_query(v.clone());
                    navigate(
                        &format!("/search?q={}", urlencoding::encode(&v)),
                        NavigateOptions {
                            replace: true,
                            scroll: false,
                            ..Default::default()
                        },
                    );
                }
            />
        </div>
    }
}

// --- Pages ---

/// Home page: shows a welcome message and module list.
#[component]
fn HomePage() -> impl IntoView {
    let modules = Resource::new(move || (), |_| list_modules());
    redirect_on_error(move || {
        modules
            .get()
            .and_then(|res| res.err())
            .map(|e| e.to_string())
    });

    view! {
        <div class="home">
            <h1>"EDL Documentation"</h1>
            <p>"Browse the documentation by selecting a module from the sidebar, or use the search bar above."</p>
            <h2>"Modules"</h2>
            <Suspense fallback=|| "Loading...".into_view()>
                {move || {
                    modules.get().map(|res| match res {
                        Err(e) => view! { <ErrorView message={e.to_string()}/> }.into_any(),
                        Ok(mods) if mods.is_empty() => {
                            view! { <p>"No modules found."</p> }.into_any()
                        }
                        Ok(mods) => view! {
                            <ul class="module-grid">
                                {mods.into_iter().map(|m| {
                                    let desc = m.doc_text.clone();
                                    let name = m.qual_name.clone();
                                    let has_desc = !desc.is_empty();
                                    view! {
                                        <li>
                                            <A href=format!("/module/{}", name)>{name.clone()}</A>
                                            <Show when=move || has_desc>
                                                <p class="module-desc">{desc.clone()}</p>
                                            </Show>
                                        </li>
                                    }
                                }).collect::<Vec<_>>()}
                            </ul>
                        }
                        .into_any(),
                    })
                }}
            </Suspense>
        </div>
    }
}

/// Search page: shows FTS5 search results for `?q=...`.
#[component]
fn SearchPage() -> impl IntoView {
    let query_map = use_query_map();
    let query = Signal::derive(move || {
        query_map
            .get()
            .get("q")
            .map(|s| s.clone())
            .unwrap_or_default()
    });
    // The query that actually drives the fetch. It lags the live `query` by
    // `SEARCH_DEBOUNCE`: the expensive `search_docs` call (and the re-render of the
    // result iframes) only fires once the user pauses. Starting from the current query
    // means a direct load of `/search?q=...` still resolves immediately. `committed` is a
    // single `RwSignal` (read by the resource, written by the debounce) so there is no
    // unused-binding warning in the SSR build, where the client-only debounce below is
    // compiled out.
    let committed = create_rw_signal(query.get());
    #[cfg(feature = "hydrate")]
    {
        // Client-only: `debounce` schedules a browser `setTimeout`, which must not be
        // created (let alone fired) during SSR. Each keystroke changes `query`, which
        // re-runs this effect and resets the timer; after `SEARCH_DEBOUNCE` of silence
        // the latest query is committed, re-triggering the resource below.
        let mut commit = debounce(SEARCH_DEBOUNCE, move |q: String| committed.set(q));
        Effect::new(move || {
            let q = query.get();
            commit(q);
        });
    }
    let results = Resource::new(
        move || committed.get(),
        move |q| async move {
            if q.is_empty() {
                Ok(Vec::new())
            } else {
                search_docs(q, 20).await
            }
        },
    );
    redirect_on_error(move || {
        results
            .get()
            .and_then(|res| res.err())
            .map(|e| e.to_string())
    });

    view! {
        <div class="search-results">
            <h1>"Search Results"</h1>
            {move || {
                let q = query.get();
                if q.is_empty() {
                    view! { <p>"Enter a search query in the bar above."</p> }.into_any()
                } else {
                    view! { <p>"Results for: \"" {q.clone()} "\""</p> }.into_any()
                }
            }}
            <Suspense fallback=|| "Searching...".into_view()>
                {move || {
                    results.get().map(|res| match res {
                        Err(e) => view! { <ErrorView message={e.to_string()}/> }.into_any(),
                        Ok(res) if res.is_empty() => {
                            view! { <p>"No results found."</p> }.into_any()
                        }
                        Ok(res) => view! {
                            <ul class="result-list">
                                {res.into_iter().map(|item| {
                                    let doc = item.doc_text.clone();
                                    let has_doc = !doc.is_empty();
                                    let has_doc_html = item.has_doc_html;
                                    let qual = item.qual_name.clone();
                                    view! {
                                        <li class="result-item">
                                            <A href=format!("/item/{}", item.qual_name)>
                                                <span class="result-kind">{item.kind.clone()}</span>
                                                <span class="result-name">{item.name.clone()}</span>
                                            </A>
                                            <SignatureView
                                                blob=item.blob.clone()
                                                plain=item.signature.clone()
                                                class="result-signature"
                                            />
                                            <Show when=move || has_doc>
                                                {if has_doc_html {
                                                    // The typeset doc comment, shown in a same-origin iframe
                                                    // just like the item page; sized to its content on load
                                                    // and re-measured on window resize.
                                                    view! {
                                                        <iframe
                                                            class="doc-html"
                                                            src=format!("/doc-html/{}", qual)
                                                            on:load=move |ev| {
                                                                if let Some(target) = ev.target() {
                                                                    if let Ok(iframe) =
                                                                        target
                                                                            .dyn_into::<web_sys::HtmlIFrameElement>()
                                                                    {
                                                                        set_doc_iframe_height(&iframe);
                                                                    }
                                                                }
                                                            }
                                                            title="Rendered documentation"
                                                        />
                                                    }
                                                    .into_any()
                                                } else {
                                                    // No rendered HTML (DB built without rendering): fall back
                                                    // to the raw doc text, exactly as before. `doc` is cloned
                                                    // (not moved) because the `<Show>` children closure is
                                                    // re-runnable and may only borrow its captured values.
                                                    view! { <p class="result-doc">{doc.clone()}</p> }.into_any()
                                                }}
                                            </Show>
                                        </li>
                                    }
                                }).collect::<Vec<_>>()}
                            </ul>
                        }
                        .into_any(),
                    })
                }}
            </Suspense>
        </div>
    }
}

/// Item page: shows a single documentation item in detail (like a docs.rs item page).
#[component]
fn ItemPage() -> impl IntoView {
    let params = use_params_map();
    let name = Signal::derive(move || {
        params
            .get()
            .get("name")
            .map(|s| s.clone())
            .unwrap_or_default()
    });
    let item = Resource::new(
        move || name.get(),
        move |n| async move {
            if n.is_empty() {
                Err(DocError::ItemNotFound { name: n })
            } else {
                get_doc(n).await
            }
        },
    );
    redirect_on_error(move || item.get().and_then(|res| res.err()).map(|e| e.to_string()));

    view! {
        <div class="item-page">
            <Suspense fallback=|| "Loading...".into_view()>
                {move || {
                    item.get().map(|res| {
                        match res {
                            Err(e) => view! { <ErrorView message={e.to_string()}/> }.into_any(),
                            Ok(doc) => {
                                let qual = doc.qual_name.clone();
                                // `doc_text` is a `Copy` signal so the re-runnable children closure
                                // below can read it without moving a non-`Copy` value out of this
                                // (re-runnable) closure's environment.
                                let doc_text = signal(doc.doc_text.clone()).0;
                                let kind = doc.kind.clone();
                                let name = doc.name.clone();
                                // Decompose the item's owner: the module it (or its associated
                                // type) is defined in, plus the associated type itself for impl
                                // items. Falls back to the plain qualifier decomposition when
                                // the blob cannot be parsed.
                                let owner = doc_repr::parse_item(&doc.blob)
                                    .ok()
                                    .map(|item| doc_repr::item_owner(&item, &qual))
                                    .unwrap_or_else(|| doc_repr::ItemOwner::from_qual(&qual));
                                let has_qual = qual != name;
                                let has_doc_text = !doc_text.get().is_empty();
                                let has_doc_html = doc.has_doc_html;
                                // The typeset doc is served at `/doc-html/<qual_name>` and shown in a
                                // same-origin `<iframe>`; the raw text stays as the fallback. `doc_src`
                                // is a `Copy` signal so the re-runnable children closure can read it
                                // for the iframe's `src` without moving a non-`Copy` value out of its
                                // environment.
                                let doc_src = signal(format!("/doc-html/{}", qual)).0;
                                let has_module = !owner.module.is_empty();
                                let module_link = owner.module.join("::");
                                let has_type = owner.has_type();
                                let type_display = owner.type_display.clone().unwrap_or_default();
                                let type_href = owner.type_href.clone().unwrap_or_default();
                                // The qualified name's segments, with the associated type's
                                // segment rendered in the type color so that module and type
                                // identifiers can be told apart at a glance. The name is shared
                                // through a signal because the re-runnable children closure
                                // below may only capture `Copy` values.
                                let qual_sig = signal(qual).0;
                                view! {
                                    <div class="item-header">
                                        <span class="item-kind">{kind.clone()}</span>
                                        <h1>{name.clone()}</h1>
                                        <Show when=move || has_qual>
                                            {move || {
                                                // Render each `::`-separated segment of the qualified
                                                // name; the associated type's segment (second to last) is
                                                // colored as a type so modules and types are distinguishable.
                                                let qual_segments: Vec<String> = qual_sig
                                                    .get()
                                                    .split("::")
                                                    .map(|s| s.to_string())
                                                    .collect();
                                                let type_index = has_type.then_some(qual_segments.len() - 2);
                                                let views: Vec<AnyView> = qual_segments
                                                    .iter()
                                                    .enumerate()
                                                    .flat_map(|(i, seg)| {
                                                        let cls = if Some(i) == type_index {
                                                            "q-type"
                                                        } else {
                                                            "q-mod"
                                                        };
                                                        let mut views = vec![
                                                            view! {
                                                                <span class=cls.to_string()>{seg.to_string()}</span>
                                                            }
                                                            .into_any()
                                                        ];
                                                        if i + 1 < qual_segments.len() {
                                                            views.push(
                                                                view! { <span class="q-sep">"::"</span> }.into_any()
                                                            );
                                                        }
                                                        views
                                                    })
                                                    .collect();
                                                view! { <span class="item-qual">{views}</span> }.into_any()
                                            }}
                                        </Show>
                                    </div>
                                    <SignatureView
                                        blob=doc.blob.clone()
                                        plain=doc.signature.clone()
                                        class="item-signature"
                                    />
                                    <Show when=move || has_doc_text>
                                        <div class="item-doc">
                                            <h2>"Documentation"</h2>
                                            {move || {
                                                // The typeset doc (when the database was built with
                                                // rendering) is shown in a same-origin `<iframe>`;
                                                // otherwise the raw doc text is shown as before.
                                                // Fresh locals (`src`, `text`) are moved into the
                                                // children; the captured `doc_src`/`doc_text` are only
                                                // borrowed, keeping this re-runnable closure `Fn`.
                                                let src = doc_src.get();
                                                let text = doc_text.get();
                                                if has_doc_html {
                                                    view! {
                                                        <iframe
                                                            class="doc-html"
                                                            src=src
                                                            on:load=move |ev| {
                                                                if let Some(target) = ev.target() {
                                                                    if let Ok(iframe) =
                                                                        target.dyn_into::<web_sys::HtmlIFrameElement>()
                                                                    {
                                                                        set_doc_iframe_height(&iframe);
                                                                    }
                                                                }
                                                            }
                                                            title="Rendered documentation"
                                                        />
                                                    }
                                                    .into_any()
                                                } else {
                                                    view! {
                                                        <pre class="doc-text">{text}</pre>
                                                    }
                                                    .into_any()
                                                }
                                            }}
                                        </div>
                                    </Show>
                                    <div class="item-meta">
                                        <Show when=move || has_module>
                                            <A href=format!("/module/{}", module_link.clone())>
                                                {format!("Module: {}", module_link)}
                                            </A>
                                        </Show>
                                        <Show when=move || has_type>
                                            <div class="item-assoc">
                                                "Defined on: "
                                                <span class="assoc-type">
                                                    <a href={type_href.clone()}>{type_display.clone()}</a>
                                                </span>
                                            </div>
                                        </Show>
                                    </div>
                                }.into_any()
                            }
                        }
                    })
                }}
            </Suspense>
        </div>
    }
}

/// Module page: shows all items in a module, grouped by kind.
#[component]
fn ModulePage() -> impl IntoView {
    let params = use_params_map();
    let name = Signal::derive(move || {
        params
            .get()
            .get("name")
            .map(|s| s.clone())
            .unwrap_or_default()
    });
    let items = Resource::new(
        move || name.get(),
        move |n| async move {
            if n.is_empty() {
                Ok(Vec::new())
            } else {
                get_module_items(n).await
            }
        },
    );
    // All module items, used to tell a real module apart from a type that owns items
    // (e.g. `usize` for the std intrinsics).
    let modules = Resource::new(
        move || (),
        move |_| async move { list_modules().await },
    );
    redirect_on_error(move || items.get().and_then(|res| res.err()).map(|e| e.to_string()));

    view! {
        <div class="module-page">
            <Suspense fallback=|| "Loading...".into_view()>
                {move || {
                    let n = name.get();
                    let is_module = modules.get().is_some_and(|res| {
                        res.as_ref()
                            .ok()
                            .is_some_and(|mods| mods.iter().any(|m| m.qual_name == n))
                    });
                    let heading = if n.is_empty() || is_module {
                        "Module: "
                    } else {
                        "Type: "
                    };
                    items.get().map(|res| {
                        match res {
                            Err(e) => view! { <ErrorView message={e.to_string()}/> }.into_any(),
                            Ok(items) if items.is_empty() && !n.is_empty() => view! {
                                <h1>{heading.to_string()} {n.clone()}</h1>
                                <p>{if is_module { "No items found in this module." } else { "No items found in this type." }}</p>
                            }
                            .into_any(),
                            Ok(items) => {
                                let mut fns = Vec::new();
                                let mut types = Vec::new();
                                let mut lets = Vec::new();
                                let mut consts = Vec::new();
                                let mut modules = Vec::new();
                                for item in items {
                                    match item.kind.as_str() {
                                        "fn" => fns.push(item),
                                        "type" => types.push(item),
                                        "let" => lets.push(item),
                                        "const" => consts.push(item),
                                        "module" => modules.push(item),
                                        _ => {}
                                    }
                                }
                                view! {
                                    <h1>{heading.to_string()} {n.clone()}</h1>
                                    {item_group_view("Modules", modules)}
                                    {item_group_view("Functions", fns)}
                                    {item_group_view("Types", types)}
                                    {item_group_view("Variables", lets)}
                                    {item_group_view("Constants", consts)}
                                }.into_any()
                            }
                        }
                    })
                }}
            </Suspense>
        </div>
    }
}

/// Renders a group of items under a heading, if the list is non-empty.
fn item_group_view(title: &str, items: Vec<DocSummary>) -> impl IntoView {
    let title = title.to_string();
    if items.is_empty() {
        view! { <span style="display:none"></span> }.into_any()
    } else {
        view! {
            <section class="item-group">
                <h2>{title.clone()}</h2>
                <ul>
                    {items.into_iter().map(|item| {
                        let href = if item.kind == "module" {
                            format!("/module/{}", item.qual_name)
                        } else {
                            format!("/item/{}", item.qual_name)
                        };
                        let doc = item.doc_text.clone();
                        let has_doc = !doc.is_empty();
                        let has_doc_html = item.has_doc_html;
                        let qual = item.qual_name.clone();
                        // Row styled like a search result: kind badge + bold name,
                        // signature, and the doc comment (rendered iframe or raw text).
                        view! {
                            <li class="group-item">
                                <A href={href}>
                                    <span class="group-kind">{item.kind.clone()}</span>
                                    <span class="group-name">{item.name.clone()}</span>
                                </A>
                                <SignatureView
                                    blob=item.blob.clone()
                                    plain=item.signature.clone()
                                    class="group-signature"
                                />
                                <Show when=move || has_doc>
                                    {if has_doc_html {
                                        // `loading="lazy"` defers each iframe until it scrolls near
                                        // the viewport — a module page can list many items, so we
                                        // don't want every doc loaded up front.
                                        view! {
                                            <iframe
                                                class="doc-html"
                                                // `loading` isn't in Leptos's typed `Iframe` attribute set,
                                                // so it's added as a custom attribute.
                                                {custom_attribute("loading", "lazy")}
                                                src=format!("/doc-html/{}", qual)
                                                on:load=move |ev| {
                                                    if let Some(target) = ev.target() {
                                                        if let Ok(iframe) =
                                                            target
                                                                .dyn_into::<web_sys::HtmlIFrameElement>()
                                                        {
                                                            set_doc_iframe_height(&iframe);
                                                        }
                                                    }
                                                }
                                                title="Rendered documentation"
                                            />
                                        }
                                        .into_any()
                                    } else {
                                        // No rendered HTML (DB built without rendering): fall
                                        // back to the raw doc text. Cloned, not moved, because
                                        // the `<Show>` children closure is re-runnable.
                                        view! { <p class="group-doc">{doc.clone()}</p> }.into_any()
                                    }}
                                </Show>
                            </li>
                        }
                    }).collect::<Vec<_>>()}
                </ul>
            </section>
        }
        .into_any()
    }
}
