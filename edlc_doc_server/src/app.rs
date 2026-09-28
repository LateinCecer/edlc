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

use leptos::hydration::{AutoReload, HydrationScripts};
use leptos::prelude::*;
use leptos_meta::{provide_meta_context, MetaTags, Stylesheet, Title};
use leptos_router::{
    components::{Route, Router, Routes, A},
    hooks::{use_navigate, use_params_map, use_query_map},
    NavigateOptions, ParamSegment, StaticSegment,
};

use crate::server::{get_doc, get_module_items, list_modules, search_docs, DocError, DocSummary};
use crate::signature::SignatureView;

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
    let results = Resource::new(
        move || query.get(),
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
                                                <p class="result-doc">{doc.clone()}</p>
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
                                let doc_text = doc.doc_text.clone();
                                let module = doc.module.clone();
                                let kind = doc.kind.clone();
                                let name = doc.name.clone();
                                let has_qual = qual != name;
                                let has_doc_text = !doc_text.is_empty();
                                let has_module = module.is_some();
                                let module_link = module.clone().unwrap_or_default();
                                view! {
                                    <div class="item-header">
                                        <span class="item-kind">{kind.clone()}</span>
                                        <h1>{name.clone()}</h1>
                                        <Show when=move || has_qual>
                                            <span class="item-qual">{qual.clone()}</span>
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
                                            <pre class="doc-text">{doc_text.clone()}</pre>
                                        </div>
                                    </Show>
                                    <div class="item-meta">
                                        <Show when=move || has_module>
                                            <A href=format!("/module/{}", module_link.clone())>
                                                {format!("Module: {}", module_link)}
                                            </A>
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
    redirect_on_error(move || items.get().and_then(|res| res.err()).map(|e| e.to_string()));

    view! {
        <div class="module-page">
            <Suspense fallback=|| "Loading...".into_view()>
                {move || {
                    let n = name.get();
                    items.get().map(|res| {
                        match res {
                            Err(e) => view! { <ErrorView message={e.to_string()}/> }.into_any(),
                            Ok(items) if items.is_empty() && !n.is_empty() => view! {
                                <h1>{n.clone()}</h1>
                                <p>"No items found in this module."</p>
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
                                    <h1>"Module: " {n.clone()}</h1>
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
                        view! {
                            <li>
                                <A href={href}>
                                    {item.name.clone()}
                                </A>
                                <SignatureView
                                    blob=item.blob.clone()
                                    plain=item.signature.clone()
                                    class="group-signature"
                                />
                            </li>
                        }
                    }).collect::<Vec<_>>()}
                </ul>
            </section>
        }
        .into_any()
    }
}
