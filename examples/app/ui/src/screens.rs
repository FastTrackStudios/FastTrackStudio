//! Routed screens for the example app.
//!
//! Every screen pulls the established vox clients from context
//! ([`use_conn`]) and reads data through `use_resource`. Mutations call
//! the client, then navigate with the typed `Route`.

use dioxus::prelude::*;
use example::architect::{Page, Sort, SortOrder};
use example::{ExampleCreate, ExampleUpdate};
use example_ui::{ExampleCreateForm, ExampleEditForm, SearchBar};
use uuid::Uuid;

use crate::Route;
use crate::client::{ConnState, ExampleClients, use_conn};

/// Render the connection banner (Connecting / Failed). Returns `Some`
/// when the socket isn't ready yet, so screens can early-return it.
fn conn_banner() -> Option<Element> {
    match &*use_conn().read() {
        ConnState::Ready(_) => None,
        ConnState::Connecting => Some(rsx! {
            p { class: "status", "Connecting to the server…" }
        }),
        ConnState::Failed(e) => Some(rsx! {
            div { class: "status error",
                p { "Could not reach the server:" }
                code { "{e}" }
                p { class: "hint", "Start it with " code { "just server" } "." }
            }
        }),
    }
}

fn parse_id(id: &str) -> Result<Uuid, String> {
    id.parse::<Uuid>()
        .map_err(|e| format!("bad id `{id}`: {e}"))
}

// ── Home: browse + search ───────────────────────────────────────────────

#[component]
pub fn Home() -> Element {
    let conn = use_conn();
    let mut query = use_signal(String::new);

    // Re-runs when the socket comes up (reads `conn`) or `query` changes.
    let results = use_resource(move || async move {
        let clients = match &*conn.read() {
            ConnState::Ready(c) => c.clone(),
            _ => return None,
        };
        let q = query();
        let res = if q.trim().is_empty() {
            clients
                .repo
                .list(
                    Page {
                        index: 0,
                        size: 100,
                    },
                    Some(Sort {
                        field: "name".into(),
                        order: SortOrder::Asc,
                    }),
                    None,
                )
                .await
                .map(|list| list.items)
                .map_err(|e| format!("{e:?}"))
        } else {
            clients
                .service
                .search(q, 100)
                .await
                .map_err(|e| format!("{e:?}"))
        };
        Some(res)
    });

    rsx! {
        div { class: "toolbar",
            SearchBar { on_search: move |q: String| query.set(q) }
            Link { class: "btn primary", to: Route::NewExample {}, "+ New" }
        }
        if let Some(banner) = conn_banner() {
            {banner}
        } else {
            match &*results.read() {
                None | Some(None) => rsx! { p { class: "status", "Loading…" } },
                Some(Some(Ok(items))) if items.is_empty() => rsx! {
                    p { class: "status empty", "No examples yet — create one." }
                },
                Some(Some(Ok(items))) => rsx! {
                    ul { class: "example-list",
                        for ex in items.clone() {
                            li { key: "{ex.id}", class: "example-link-row",
                                Link { to: Route::ExampleDetail { id: ex.id.to_string() },
                                    span { class: "example-row__name", "{ex.name}" }
                                    span { class: "example-row__description", "{ex.description}" }
                                }
                            }
                        }
                    }
                },
                Some(Some(Err(e))) => rsx! { p { class: "status error", "{e}" } },
            }
        }
    }
}

// ── Detail: view + delete + duplicate ───────────────────────────────────

#[component]
pub fn ExampleDetail(id: String) -> Element {
    let conn = use_conn();
    let nav = use_navigator();
    let mut action_err = use_signal(|| Option::<String>::None);

    let row = use_resource(move || {
        let id = id.clone();
        async move {
            let clients = match &*conn.read() {
                ConnState::Ready(c) => c.clone(),
                _ => return None,
            };
            let uuid = match parse_id(&id) {
                Ok(u) => u,
                Err(e) => return Some(Err(e)),
            };
            Some(clients.repo.get(uuid).await.map_err(|e| format!("{e:?}")))
        }
    });

    rsx! {
        if let Some(banner) = conn_banner() {
            {banner}
        } else {
            match &*row.read() {
                None | Some(None) => rsx! { p { class: "status", "Loading…" } },
                Some(Some(Ok(ex))) => {
                    let ex = ex.clone();
                    let target = ex.id;
                    let id_for_edit = target.to_string();
                    rsx! {
                        article { class: "detail",
                            h2 { "{ex.name}" }
                            p { class: "muted", "{ex.description}" }
                            dl { class: "meta",
                                dt { "id" } dd { code { "{ex.id}" } }
                                dt { "created" } dd { "{ex.created_at}" }
                                dt { "updated" } dd { "{ex.updated_at}" }
                            }
                            if let Some(e) = action_err() {
                                p { class: "status error", "{e}" }
                            }
                            div { class: "actions",
                                Link { class: "btn", to: Route::EditExample { id: id_for_edit }, "Edit" }
                                button {
                                    class: "btn",
                                    onclick: move |_| {
                                        spawn(async move {
                                            let Some(clients) = clients_of(conn) else { return };
                                            match clients.service.duplicate(target, None).await {
                                                Ok(copy) => { nav.push(Route::ExampleDetail { id: copy.id.to_string() }); }
                                                Err(e) => action_err.set(Some(format!("duplicate: {e:?}"))),
                                            }
                                        });
                                    },
                                    "Duplicate"
                                }
                                button {
                                    class: "btn danger",
                                    onclick: move |_| {
                                        spawn(async move {
                                            let Some(clients) = clients_of(conn) else { return };
                                            match clients.repo.delete(target).await {
                                                Ok(()) => { nav.push(Route::Home {}); }
                                                Err(e) => action_err.set(Some(format!("delete: {e:?}"))),
                                            }
                                        });
                                    },
                                    "Delete"
                                }
                            }
                            Link { class: "back", to: Route::Home {}, "← back" }
                        }
                    }
                }
                Some(Some(Err(e))) => rsx! {
                    div { class: "status error",
                        p { "{e}" }
                        Link { class: "back", to: Route::Home {}, "← back" }
                    }
                },
            }
        }
    }
}

// ── New ─────────────────────────────────────────────────────────────────

#[component]
pub fn NewExample() -> Element {
    let conn = use_conn();
    let nav = use_navigator();
    let mut err = use_signal(|| Option::<String>::None);

    rsx! {
        h2 { "New example" }
        if let Some(banner) = conn_banner() {
            {banner}
        } else {
            if let Some(e) = err() {
                p { class: "status error", "{e}" }
            }
            ExampleCreateForm {
                on_submit: move |(name, description): (String, String)| {
                    spawn(async move {
                        let Some(clients) = clients_of(conn) else { return };
                        match clients.repo.create(ExampleCreate { name, description }).await {
                            Ok(ex) => { nav.push(Route::ExampleDetail { id: ex.id.to_string() }); }
                            Err(e) => err.set(Some(format!("create: {e:?}"))),
                        }
                    });
                }
            }
            Link { class: "back", to: Route::Home {}, "← back" }
        }
    }
}

// ── Edit ─────────────────────────────────────────────────────────────────

#[component]
pub fn EditExample(id: String) -> Element {
    let conn = use_conn();
    let nav = use_navigator();
    let mut err = use_signal(|| Option::<String>::None);

    let row = use_resource(move || {
        let id = id.clone();
        async move {
            let clients = match &*conn.read() {
                ConnState::Ready(c) => c.clone(),
                _ => return None,
            };
            let uuid = match parse_id(&id) {
                Ok(u) => u,
                Err(e) => return Some(Err(e)),
            };
            Some(clients.repo.get(uuid).await.map_err(|e| format!("{e:?}")))
        }
    });

    rsx! {
        h2 { "Edit example" }
        if let Some(banner) = conn_banner() {
            {banner}
        } else {
            match &*row.read() {
                None | Some(None) => rsx! { p { class: "status", "Loading…" } },
                Some(Some(Ok(ex))) => {
                    let ex = ex.clone();
                    let target = ex.id;
                    rsx! {
                        if let Some(e) = err() {
                            p { class: "status error", "{e}" }
                        }
                        ExampleEditForm {
                            initial_name: ex.name.clone(),
                            initial_description: ex.description.clone(),
                            on_submit: move |(name, description): (String, String)| {
                                spawn(async move {
                                    let Some(clients) = clients_of(conn) else { return };
                                    let update = ExampleUpdate {
                                        name: Some(name),
                                        description: Some(description),
                                    };
                                    match clients.repo.update(target, update).await {
                                        Ok(ex) => { nav.push(Route::ExampleDetail { id: ex.id.to_string() }); }
                                        Err(e) => err.set(Some(format!("update: {e:?}"))),
                                    }
                                });
                            }
                        }
                        Link { class: "back", to: Route::ExampleDetail { id: target.to_string() }, "← cancel" }
                    }
                }
                Some(Some(Err(e))) => rsx! { p { class: "status error", "{e}" } },
            }
        }
    }
}

// ── Not found ─────────────────────────────────────────────────────────────

#[component]
pub fn NotFound(route: Vec<String>) -> Element {
    rsx! {
        div { class: "status",
            h2 { "Not found" }
            p { "No route for /{route.join(\"/\")}" }
            Link { class: "back", to: Route::Home {}, "← home" }
        }
    }
}

/// Read the clients out of a connection signal inside an event handler
/// (a plain read, not a hook). Returns `None` if the socket isn't ready —
/// handlers just no-op in that window.
fn clients_of(conn: Signal<ConnState>) -> Option<ExampleClients> {
    match &*conn.read() {
        ConnState::Ready(c) => Some(c.clone()),
        _ => None,
    }
}
