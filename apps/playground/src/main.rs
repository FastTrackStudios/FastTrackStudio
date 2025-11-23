use dioxus::prelude::*;
mod blog;
mod navbar;
mod counter;
mod transport_display;
mod reaper_connection;
mod setlist_connection;
mod setlist_display;
use crate::blog::Blog;
use crate::navbar::Navbar;
use crate::counter::{CounterButton, start_auto_increment };
use crate::transport_display::TransportDisplay;
use crate::setlist_display::SetlistDisplay;

#[derive(Debug, Clone, Routable, PartialEq)]
#[rustfmt::skip]
enum Route {
    #[layout(Navbar)]
    #[route("/")]
    Home {},
    #[route("/blog/:id")]
    Blog { id: i32 },
}

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const HEADER_SVG: Asset = asset!("/assets/header.svg");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    // Initialize tracing subscriber for logging (only for desktop builds)
    #[cfg(not(target_arch = "wasm32"))]
    {
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::EnvFilter;

        let filter = if std::env::var("RUST_LOG").is_ok() {
            // User specified RUST_LOG - use it directly
            EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into())
        } else {
            // Default filter
            EnvFilter::new("info")
        };

        tracing_subscriber::registry()
            .with(filter)
            .with(tracing_subscriber::fmt::layer())
            .init();
    }

    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    // Start the auto-increment background task when the app component mounts
    use_effect(move || {
        #[cfg(not(target_arch = "wasm32"))]
        {
            start_auto_increment();
            
            // Connect to REAPER extension and stream transport updates
            spawn(async move {
                if let Err(e) = reaper_connection::connect_to_reaper().await {
                    tracing::warn!("Failed to connect to REAPER transport: {}", e);
                }
            });
            
            // Connect to REAPER extension and stream setlist updates
            spawn(async move {
                if let Err(e) = setlist_connection::connect_to_reaper_setlist().await {
                    tracing::warn!("Failed to connect to REAPER setlist: {}", e);
                }
            });
        }
    });

    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: MAIN_CSS } document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        Router::<Route> {}
    }
}

/// Home page
#[component]
fn Home() -> Element {
    rsx! {
        // Hero {}
        div {
            class: "flex flex-col items-center justify-center h-screen gap-8",

            CounterButton {  }
            TransportDisplay {}
            SetlistDisplay {}

        }

    }
}
