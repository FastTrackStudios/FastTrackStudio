//! Runtime UI shell — the full example app, shared verbatim by the web
//! (wasm) and desktop targets.
//!
//! Architecture:
//! - [`App`] establishes the vox clients once (see [`client`]) and
//!   provides them to the tree as a `Signal<ConnState>`, then mounts the
//!   router. Data never flows through Dioxus server functions — every
//!   read/write is a typed vox call. See `docs/.../idioms.md`.
//! - [`Route`] is the typed route table; [`AppShell`] is the layout that
//!   wraps every page with the header + nav.
//! - The screens live in [`screens`]; the feature-scoped presentational
//!   components (list, row, forms, search) come from `example_ui`.
//!
//! A real project pulls in more feature-ui crates here (timeline_ui,
//! mixing_ui, …) and adds their routes; the shell shape is the same.

mod client;
mod screens;

use dioxus::prelude::*;

pub use client::{ConnState, DEFAULT_SERVER_URL, ExampleClients, Transport};

use screens::{EditExample, ExampleDetail, Home, NewExample, NotFound};

/// Bundled stylesheet. `dx` collects it for both web and desktop.
static MAIN_CSS: Asset = asset!("/assets/main.css");

/// Typed route table. `AppShell` wraps the four content routes; anything
/// else falls through to `NotFound`.
#[derive(Routable, Clone, PartialEq)]
#[rustfmt::skip]
pub enum Route {
    #[layout(AppShell)]
        #[route("/")]
        Home {},
        #[route("/new")]
        NewExample {},
        #[route("/example/:id")]
        ExampleDetail { id: String },
        #[route("/example/:id/edit")]
        EditExample { id: String },
    #[end_layout]
    #[route("/:..route")]
    NotFound { route: Vec<String> },
}

/// App root. Establishes the vox clients once, hands them to the tree,
/// and mounts the router.
#[component]
pub fn App() -> Element {
    // The transport is chosen at the app root (web → Remote, desktop →
    // Local). Default to Remote so a bare `App` mount (tests) still works.
    let transport = use_hook(|| {
        try_consume_context::<Transport>()
            .unwrap_or_else(|| Transport::Remote(DEFAULT_SERVER_URL.to_string()))
    });

    // Provided unconditionally so every screen's `use_conn()` is stable.
    let conn = use_context_provider(|| Signal::new(ConnState::Connecting));

    // One-shot connect on mount. Screens react when this flips to Ready.
    use_future(move || {
        let transport = transport.clone();
        async move {
            let mut conn = conn;
            match ExampleClients::connect(&transport).await {
                Ok(clients) => conn.set(ConnState::Ready(clients)),
                Err(e) => conn.set(ConnState::Failed(e)),
            }
        }
    });

    rsx! {
        document::Stylesheet { href: MAIN_CSS }
        Router::<Route> {}
    }
}

/// Layout shared by every content route: header/brand + the page outlet.
#[component]
fn AppShell() -> Element {
    rsx! {
        header { class: "app-header",
            Link { class: "brand", to: Route::Home {}, "architect" }
            span { class: "tagline", "reference example" }
        }
        main { class: "app-main",
            Outlet::<Route> {}
        }
    }
}
