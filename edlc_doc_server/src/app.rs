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

use leptos::prelude::*;
use leptos::hydration::{AutoReload, HydrationScripts};
use leptos_meta::{provide_meta_context, MetaTags, Stylesheet, Title};
use leptos_router::{
    components::{Route, Router, Routes, A, Outlet},
    hooks::{use_query_map, use_params_map},
    StaticSegment, ParamSegment,
};

// DocSummary is shared between SSR and hydrate builds.
#[cfg(feature = "ssr")]
use crate::server::DocSummary;

#[cfg(not(feature = "ssr"))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq)]
struct DocSummary {
    pub id: i64,
    pub kind: String,
    pub name: String,
    pub qual_name: String,
    pub module: Option<String>,
    pub signature: String,
    pub doc_text: String,
    pub blob: String,
}

// On the client side (hydrate), server functions are stubs that call the server.
// The `server` module is only available with the `ssr` feature, so we provide
// client-side stubs here for the hydrate build.
#[cfg(not(feature = "ssr"))]
mod server_stubs {
    use super::DocSummary;
    pub async fn search_docs(_query: String, _limit: usize) -> Result<Vec<DocSummary>, String> {
        Err("server functions require SSR".to_string())
    }
    pub async fn get_doc(_name: String) -> Result<Option<DocSummary>, String> {
        Err("server functions require SSR".to_string())
    }
    pub async fn list_modules() -> Result<Vec<DocSummary>, String> {
        Err("server functions require SSR".to_string())
    }
    pub async fn get_module_items(_name: String) -> Result<Vec<DocSummary>, String> {
        Err("server functions require SSR".to_string())
    }
}

#[cfg(not(feature = "ssr"))]
use server_stubs::{search_docs, get_doc, list_modules, get_module_items};
#[cfg(feature = "ssr")]
use crate::server::{search_docs, get_doc, list_modules, get_module_items};

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
                        </Routes>
                    </main>
                </div>
            </div>
        </Router>
    }
}

// --- Sidebar ---

/// Left sidebar showing all modules (like docs.rs crate navigation).
#[component]
fn Sidebar() -> impl IntoView {
    let modules = Resource::new(move || (), |_| async { list_modules().await.unwrap_or_default() });

    view! {
        <aside class="sidebar">
            <div class="sidebar-header">
                <h1>"EDL Docs"</h1>
            </div>
            <Suspense fallback=|| "Loading...".into_view()>
                <ul class="module-list">
                    {move || {
                        modules.get().map(|mods| {
                            mods.into_iter().map(|m| {
                                view! {
                                    <li>
                                        <A href=format!("/module/{}", m.name)>
                                            {m.name}
                                        </A>
                                    </li>
                                }
                            }).collect::<Vec<_>>()
                        })
                    }}
                </ul>
            </Suspense>
        </aside>
    }
}

// --- Search bar ---

/// Top search bar. Navigates to `/search?q=...` on submit.
#[component]
fn SearchBar() -> impl IntoView {
    let (query, set_query) = signal(String::new());
    let navigate = leptos_router::hooks::use_navigate();

    view! {
        <div class="search-bar">
            <input
                type="text"
                placeholder="Search documentation..."
                prop:value=move || query.get()
                on:input=move |ev| set_query.set(event_target_value(&ev))
                on:keydown=move |ev| {
                    if ev.key() == "Enter" && !query.get().is_empty() {
                        navigate(&format!("/search?q={}", query.get()), Default::default());
                    }
                }
            />
        </div>
    }
}

// --- Pages ---

/// Home page: shows a welcome message and module list.
#[component]
fn HomePage() -> impl IntoView {
    let modules = Resource::new(move || (), |_| async { list_modules().await.unwrap_or_default() });

    view! {
        <div class="home">
            <h1>"EDL Documentation"</h1>
            <p>"Browse the documentation by selecting a module from the sidebar, or use the search bar above."</p>
            <h2>"Modules"</h2>
            <Suspense fallback=|| "Loading...".into_view()>
                {move || {
                    modules.get().map(|mods| {
                        if mods.is_empty() {
                            view! { <p>"No modules found."</p> }.into_any()
                        } else {
                            view! {
                                <ul class="module-grid">
                                    {mods.into_iter().map(|m| {
                                        let desc = m.doc_text.clone();
                                        let name = m.name.clone();
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
                            }.into_any()
                        }
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
        query_map.get().get("q").map(|s| s.clone()).unwrap_or_default()
    });
    let results = Resource::new(
        move || query.get(),
        move |q| async move {
            if q.is_empty() {
                Vec::new()
            } else {
                search_docs(q, 50).await.unwrap_or_default()
            }
        },
    );

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
                    results.get().map(|res| {
                        if res.is_empty() {
                            view! { <p>"No results found."</p> }.into_any()
                        } else {
                            view! {
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
                                                <pre class="result-signature">{item.signature.clone()}</pre>
                                                <Show when=move || has_doc>
                                                    <p class="result-doc">{doc.clone()}</p>
                                                </Show>
                                            </li>
                                        }
                                    }).collect::<Vec<_>>()}
                                </ul>
                            }.into_any()
                        }
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
        params.get().get("name").map(|s| s.clone()).unwrap_or_default()
    });
    let item = Resource::new(
        move || name.get(),
        move |n| async move {
            if n.is_empty() {
                None
            } else {
                get_doc(n).await.unwrap_or(None)
            }
        },
    );

    view! {
        <div class="item-page">
            <Suspense fallback=|| "Loading...".into_view()>
                {move || {
                    item.get().map(|opt| {
                        match opt {
                            None => view! {
                                <h1>"Item not found"</h1>
                                <p>"The requested documentation item could not be found."</p>
                            }.into_any(),
                            Some(doc) => {
                                let qual = doc.qual_name.clone();
                                let sig = doc.signature.clone();
                                let doc_text = doc.doc_text.clone();
                                let module = doc.module.clone();
                                let kind = doc.kind.clone();
                                let name = doc.name.clone();
                                let has_qual = qual != name;
                                let has_doc_text = !doc_text.is_empty();
                                let has_module = module.is_some();
                                let module_name = module.clone().unwrap_or_default();
                                let module_link = module_name.clone();
                                view! {
                                    <div class="item-header">
                                        <span class="item-kind">{kind.clone()}</span>
                                        <h1>{name.clone()}</h1>
                                        <Show when=move || has_qual>
                                            <span class="item-qual">{qual.clone()}</span>
                                        </Show>
                                    </div>
                                    <pre class="item-signature">{sig.clone()}</pre>
                                    <Show when=move || has_doc_text>
                                        <div class="item-doc">
                                            <h2>"Documentation"</h2>
                                            <pre class="doc-text">{doc_text.clone()}</pre>
                                        </div>
                                    </Show>
                                    <div class="item-meta">
                                        <Show when=move || has_module>
                                            <span class="item-module">
                                                "Module: "
                                                {module_link.clone()}
                                            </span>
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
        params.get().get("name").map(|s| s.clone()).unwrap_or_default()
    });
    let items = Resource::new(
        move || name.get(),
        move |n| async move {
            if n.is_empty() {
                Vec::new()
            } else {
                get_module_items(n).await.unwrap_or_default()
            }
        },
    );

    view! {
        <div class="module-page">
            <Suspense fallback=|| "Loading...".into_view()>
                {move || {
                    let n = name.get();
                    items.get().map(|items| {
                        if items.is_empty() && !n.is_empty() {
                            view! {
                                <h1>{n.clone()}</h1>
                                <p>"No items found in this module."</p>
                            }.into_any()
                        } else {
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
                        view! {
                            <li>
                                <A href=format!("/item/{}", item.qual_name)>
                                    {item.name.clone()}
                                </A>
                                <pre class="group-signature">{item.signature.clone()}</pre>
                            </li>
                        }
                    }).collect::<Vec<_>>()}
                </ul>
            </section>
        }.into_any()
    }
}
