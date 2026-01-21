//! FastTrackStudio Documentation Site
//!
//! Interactive documentation with live chart rendering for the keyflow parser.
//! Test patterns are displayed as interactive examples using WebGPU.

mod components;
mod renderer;
mod routes;
mod state;

use dioxus::prelude::*;
use lucide_dioxus::{BookOpen, Github, ArrowLeft, PenTool};

// Static assets
const FAVICON: Asset = asset!("/assets/favicon.ico");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

/// Application routes
#[derive(Debug, Clone, Routable, PartialEq)]
#[rustfmt::skip]
pub enum Route {
    #[layout(Layout)]
    #[route("/")]
    Home {},
    #[route("/keyflow/chart")]
    ChartEditor {},
    #[route("/docs/keyflow/chart/tests")]
    PatternBrowser {},
    #[route("/docs/keyflow/chart/tests/:id")]
    PatternView { id: String },
}

fn main() {
    // Initialize panic hook for better WASM debugging
    #[cfg(target_arch = "wasm32")]
    {
        console_error_panic_hook::set_once();
        tracing_wasm::set_as_global_default();
    }

    // Initialize logging for non-WASM
    #[cfg(not(target_arch = "wasm32"))]
    {
        tracing_subscriber::fmt::init();
    }

    tracing::info!("Starting FastTrackStudio Documentation Site");

    dioxus::launch(App);
}

/// Root application component
#[component]
fn App() -> Element {
    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        Router::<Route> {}
    }
}

/// Main layout with navigation
#[component]
fn Layout() -> Element {
    rsx! {
        div {
            class: "min-h-screen flex flex-col bg-background text-foreground",

            // Navigation header
            nav {
                class: "bg-card border-b border-border sticky top-0 z-50",

                div {
                    class: "max-w-7xl mx-auto px-4 py-3 flex items-center justify-between",

                    // Logo and title
                    Link {
                        to: Route::Home {},
                        class: "flex items-center gap-2 text-foreground text-xl font-bold hover:text-primary transition-colors",
                        span { "FastTrack" }
                        span { class: "text-muted-foreground font-normal", "Docs" }
                    }

                    // Navigation links
                    div {
                        class: "flex items-center gap-4",

                        Link {
                            to: Route::ChartEditor {},
                            class: "flex items-center gap-2 text-muted-foreground hover:text-foreground transition-colors px-3 py-2 rounded-md hover:bg-accent",
                            PenTool { class: "w-4 h-4" }
                            span { "Editor" }
                        }

                        Link {
                            to: Route::PatternBrowser {},
                            class: "flex items-center gap-2 text-muted-foreground hover:text-foreground transition-colors px-3 py-2 rounded-md hover:bg-accent",
                            BookOpen { class: "w-4 h-4" }
                            span { "Patterns" }
                        }

                        a {
                            href: "https://github.com/codywright/FastTrackStudio",
                            target: "_blank",
                            class: "flex items-center gap-2 text-muted-foreground hover:text-foreground transition-colors px-3 py-2 rounded-md hover:bg-accent",
                            Github { class: "w-4 h-4" }
                            span { "GitHub" }
                        }
                    }
                }
            }

            // Main content area
            main {
                class: "flex-1",
                Outlet::<Route> {}
            }

            // Footer
            footer {
                class: "bg-card border-t border-border py-4",

                div {
                    class: "max-w-7xl mx-auto px-4 text-center text-muted-foreground text-sm",
                    "FastTrackStudio Documentation"
                }
            }
        }
    }
}

/// Home page component
#[component]
fn Home() -> Element {
    rsx! {
        div {
            class: "max-w-4xl mx-auto px-4 py-16",

            // Hero section
            div {
                class: "text-center mb-16",

                h1 {
                    class: "text-5xl font-bold text-foreground mb-4",
                    "Keyflow Documentation"
                }

                p {
                    class: "text-xl text-muted-foreground mb-8",
                    "Music notation parser with GPU-accelerated rendering"
                }

                Link {
                    to: Route::PatternBrowser {},
                    class: "inline-flex items-center gap-2 bg-primary text-primary-foreground hover:bg-primary/90 px-6 py-3 rounded-lg font-semibold transition-colors",
                    BookOpen { class: "w-5 h-5" }
                    "Browse Patterns"
                }
            }

            // Features grid
            div {
                class: "grid md:grid-cols-2 gap-6",

                // Feature cards
                FeatureCard {
                    title: "Pattern Library",
                    description: "Browse interactive examples of chart syntax and notation features.",
                    icon: rsx! { lucide_dioxus::Library { class: "w-8 h-8 text-primary" } }
                }

                FeatureCard {
                    title: "Live Rendering",
                    description: "See charts rendered in real-time using WebGPU.",
                    icon: rsx! { lucide_dioxus::Palette { class: "w-8 h-8 text-primary" } }
                }

                FeatureCard {
                    title: "Source Code",
                    description: "View the keyflow source alongside the rendered output.",
                    icon: rsx! { lucide_dioxus::Code { class: "w-8 h-8 text-primary" } }
                }

                FeatureCard {
                    title: "Test Coverage",
                    description: "Patterns serve as both documentation and regression tests.",
                    icon: rsx! { lucide_dioxus::CircleCheck { class: "w-8 h-8 text-primary" } }
                }
            }
        }
    }
}

#[component]
fn FeatureCard(title: &'static str, description: &'static str, icon: Element) -> Element {
    rsx! {
        div {
            class: "bg-card rounded-lg p-6 border border-border hover:border-primary/50 transition-colors",

            div {
                class: "mb-4",
                {icon}
            }

            h3 {
                class: "text-lg font-semibold text-card-foreground mb-2",
                "{title}"
            }

            p {
                class: "text-muted-foreground",
                "{description}"
            }
        }
    }
}

/// Pattern browser page - lists all patterns by category
#[component]
fn PatternBrowser() -> Element {
    use keyflow::patterns::{PatternCategory, patterns_by_category};

    rsx! {
        div {
            class: "flex min-h-[calc(100vh-8rem)]",

            // Sidebar
            aside {
                class: "w-64 bg-sidebar border-r border-sidebar-border p-4 overflow-y-auto",

                h2 {
                    class: "text-lg font-semibold text-sidebar-foreground mb-4",
                    "Categories"
                }

                nav {
                    class: "space-y-1",

                    for category in PatternCategory::all() {
                        CategoryLink {
                            category: *category
                        }
                    }
                }
            }

            // Main content
            div {
                class: "flex-1 p-8 overflow-y-auto",

                h1 {
                    class: "text-3xl font-bold text-foreground mb-2",
                    "Pattern Library"
                }

                p {
                    class: "text-muted-foreground mb-8",
                    "Interactive examples demonstrating keyflow chart syntax and notation features."
                }

                // Pattern grid grouped by category
                for category in PatternCategory::all() {
                    div {
                        class: "mb-10",

                        h2 {
                            class: "text-xl font-semibold text-foreground mb-4 flex items-center gap-2",
                            id: category.slug(),
                            span { "{category.label()}" }
                            span {
                                class: "text-sm font-normal text-muted-foreground",
                                "({patterns_by_category(*category).len()} patterns)"
                            }
                        }

                        div {
                            class: "grid md:grid-cols-2 lg:grid-cols-3 gap-4",

                            for pattern in patterns_by_category(*category) {
                                PatternCard {
                                    pattern: pattern
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn CategoryLink(category: keyflow::patterns::PatternCategory) -> Element {
    rsx! {
        a {
            href: "#{category.slug()}",
            class: "block px-3 py-2 rounded text-sidebar-foreground hover:bg-sidebar-accent hover:text-sidebar-accent-foreground transition-colors",
            "{category.label()}"
        }
    }
}

#[component]
fn PatternCard(pattern: &'static keyflow::patterns::Pattern) -> Element {
    rsx! {
        Link {
            to: Route::PatternView { id: pattern.id.to_string() },
            class: "block bg-card border border-border rounded-lg p-4 hover:border-primary transition-colors group",

            h3 {
                class: "font-semibold text-card-foreground group-hover:text-primary mb-2",
                "{pattern.title}"
            }

            p {
                class: "text-sm text-muted-foreground line-clamp-2",
                "{pattern.description}"
            }

            div {
                class: "mt-3 text-xs text-muted-foreground",
                "{pattern.category.label()}"
            }
        }
    }
}

/// Chart editor with live preview
#[component]
fn ChartEditor() -> Element {
    rsx! {
        components::ChartEditor {}
    }
}

/// Individual pattern view with chart rendering
#[component]
fn PatternView(id: String) -> Element {
    use keyflow::patterns::find_pattern;

    let pattern = find_pattern(&id);

    match pattern {
        Some(pattern) => {
            rsx! {
                div {
                    class: "flex min-h-[calc(100vh-8rem)]",

                    // Sidebar with navigation
                    aside {
                        class: "w-64 bg-sidebar border-r border-sidebar-border p-4 overflow-y-auto",

                        Link {
                            to: Route::PatternBrowser {},
                            class: "flex items-center gap-2 text-muted-foreground hover:text-foreground mb-6 transition-colors",
                            ArrowLeft { class: "w-4 h-4" }
                            span { "All Patterns" }
                        }

                        h3 {
                            class: "text-sm font-semibold text-muted-foreground uppercase tracking-wider mb-2",
                            "Current Pattern"
                        }

                        div {
                            class: "text-sidebar-foreground font-semibold mb-4",
                            "{pattern.title}"
                        }

                        div {
                            class: "text-xs text-muted-foreground mb-6",
                            "{pattern.category.label()}"
                        }
                    }

                    // Main content
                    div {
                        class: "flex-1 flex flex-col",

                        // Header
                        header {
                            class: "px-8 py-6 border-b border-border",

                            h1 {
                                class: "text-2xl font-bold text-foreground mb-2",
                                "{pattern.title}"
                            }

                            p {
                                class: "text-muted-foreground",
                                "{pattern.description}"
                            }
                        }

                        // Content area
                        div {
                            class: "flex-1 flex",

                            // Chart rendering area
                            div {
                                class: "flex-1 p-8",

                                div {
                                    class: "bg-card rounded-lg border border-border overflow-hidden",

                                    components::ChartRenderer {
                                        source: pattern.source
                                    }
                                }
                            }

                            // Source code panel
                            div {
                                class: "w-96 border-l border-border p-4 overflow-y-auto bg-card/50",

                                h3 {
                                    class: "text-sm font-semibold text-muted-foreground uppercase tracking-wider mb-4",
                                    "Source Code"
                                }

                                components::SourceViewer {
                                    source: pattern.source
                                }
                            }
                        }
                    }
                }
            }
        }
        None => {
            rsx! {
                div {
                    class: "flex flex-col items-center justify-center min-h-[50vh]",

                    h1 {
                        class: "text-2xl font-bold text-foreground mb-4",
                        "Pattern Not Found"
                    }

                    p {
                        class: "text-muted-foreground mb-8",
                        "The pattern \"{id}\" does not exist."
                    }

                    Link {
                        to: Route::PatternBrowser {},
                        class: "inline-flex items-center gap-2 border border-border text-foreground hover:bg-accent hover:text-accent-foreground px-4 py-2 rounded-md transition-colors",
                        ArrowLeft { class: "w-4 h-4" }
                        "Browse all patterns"
                    }
                }
            }
        }
    }
}
