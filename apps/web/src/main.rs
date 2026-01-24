//! FastTrackStudio Documentation Site
//!
//! Interactive documentation with live chart rendering for the keyflow parser.
//! Test patterns are displayed as interactive examples using WebGPU.

mod components;
mod renderer;
mod routes;
mod state;

use dioxus::prelude::*;
use lucide_dioxus::{
    ArrowLeft, BookOpen, ChevronDown, ChevronLeft, ChevronRight, Code, FileCode,
    FileText, Github, Music, PenLine, PenTool, Users,
};

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
    #[route("/docs")]
    DocsHome {},
    #[route("/keyflow/chart")]
    ChartEditor {},
    // Unified snippets browser - optional pattern ID for deep linking
    #[route("/docs/keyflow/snippets")]
    SnippetsBrowser {},
    #[route("/docs/keyflow/snippets/:id")]
    SnippetsView { id: String },
    // Legacy routes - redirect to new ones
    #[route("/docs/keyflow/chart/tests")]
    PatternBrowser {},
    #[route("/docs/keyflow/chart/tests/:id")]
    PatternView { id: String },
    #[route("/test/render")]
    TestRender {},
}

fn main() {
    // Initialize panic hook for better WASM debugging
    #[cfg(target_arch = "wasm32")]
    {
        use tracing_subscriber::layer::SubscriberExt;
        use tracing_subscriber::util::SubscriberInitExt;

        console_error_panic_hook::set_once();

        // Configure tracing with filtering:
        // - Our app (web) at debug level
        // - keyflow at info level (filter out trace/debug spam from parsing)
        // - dioxus at warn level (filter out signal tracing)
        // - Everything else at warn level
        let filter = tracing_subscriber::EnvFilter::new(
            "warn,web=debug,keyflow=info,keyflow::engraver=debug",
        );

        tracing_subscriber::registry()
            .with(filter)
            .with(tracing_wasm::WASMLayer::new(
                tracing_wasm::WASMLayerConfig::default(),
            ))
            .init();
    }

    // Initialize logging for non-WASM
    #[cfg(not(target_arch = "wasm32"))]
    {
        use tracing_subscriber::EnvFilter;
        tracing_subscriber::fmt()
            .with_env_filter(
                EnvFilter::try_from_default_env()
                    .unwrap_or_else(|_| EnvFilter::new("warn,web=debug,keyflow=info")),
            )
            .init();
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
            class: "min-h-screen flex flex-col text-foreground",

            // Navigation header
            nav {
                class: "bg-card border-b border-border sticky top-0 z-50",

                div {
                    class: "max-w-7xl mx-auto px-4 py-3 flex items-center justify-between",

                    // Logo and title
                    Link {
                        to: Route::Home {},
                        class: "flex items-center gap-2 text-foreground text-xl font-bold hover:text-primary transition-colors",
                        span { "FastTrackStudio" }
                        {
                            // Show section name based on current route
                            let route = use_route::<Route>();
                            let section = match route {
                                Route::ChartEditor {} => Some("Keyflow"),
                                Route::DocsHome {}
                                | Route::SnippetsBrowser {}
                                | Route::SnippetsView { .. }
                                | Route::PatternBrowser {}
                                | Route::PatternView { .. } => Some("Docs"),
                                _ => None,
                            };
                            if let Some(name) = section {
                                rsx! {
                                    span { class: "text-muted-foreground font-normal", "{name}" }
                                }
                            } else {
                                rsx! {}
                            }
                        }
                    }

                    // Navigation links
                    div {
                        class: "flex items-center gap-4",

                        Link {
                            to: Route::DocsHome {},
                            class: "flex items-center gap-2 text-muted-foreground hover:text-foreground transition-colors px-3 py-2 rounded-md hover:bg-accent",
                            BookOpen { class: "w-4 h-4" }
                            span { "Docs" }
                        }

                        Link {
                            to: Route::ChartEditor {},
                            class: "flex items-center gap-2 text-muted-foreground hover:text-foreground transition-colors px-3 py-2 rounded-md hover:bg-accent",
                            PenTool { class: "w-4 h-4" }
                            span { "Editor" }
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

/// Chart examples for the typewriter animation on the landing page.
/// Each showcases different keyflow features.
const DEMO_CHARTS: &[&str] = &[
    // Complex funk/jazz - Thriller (Dirty Loops)
    r#"Thriller
120bpm 4/4 #Ab
/push = triplet

HITS
r8t >Ab9_8t r8t r8t r8t >F9_8t r2
s1

IN
>'Cm . . .

VS
>'F/C . Cm .

CH
>Cm/Eb / 'Eb /// | 'Eb / 'F/C / 'Cm // |
'F/A //// | 'Fm9  ////
>Cm/Eb / 'Eb /// | 'Eb / 'F/C / 'Cm // | 'F/A |
r8t >Ab9_8t r8t r8t >'F9_8t r8t r4 >Fm/Ab_4

INST
Cm . F6 // Abdim7 'Csus2
"#,
    // Pop ballad in A major
    r#"Midnight Dreams
72bpm 4/4 #A

VS
A D F#m E

CH
D E A F#m
D E C#m F#m

BR
F#m E D A/C#

OUT
A
"#,
    // Jazz standard in Bb
    r#"Autumn Leaves
140bpm 4/4 #Bb

VS
Cm7 F7 Bbmaj7 Ebmaj7
Am7b5 D7 Gm //

CH
Am7b5 D7 Gm //
Cm7 F7 Bbmaj7 //

OUT
Gm
"#,
    // R&B groove in C minor
    r#"City Lights
95bpm 4/4 #Cm

VS
Cm9 Fm9 Bb13 Ebmaj7
Abmaj7 G7#9 Cm9 //

CH
Fm9 G7 Cm9 //
Abmaj7 Bb9 Cm9 //

BR
Abmaj7 G7 Cm //

OUT
Cm9
"#,
];

/// Landing page for FastTrackStudio
#[component]
fn Home() -> Element {
    // Source text state driven by typewriter animation
    let source = use_signal(|| DEMO_CHARTS[0].to_string());
    // Use Page mode for proper A4 document appearance
    let preview_mode = use_signal(|| components::PreviewMode::Page);

    // Create the list of charts for the typewriter
    let charts: Vec<String> = DEMO_CHARTS.iter().map(|s| s.to_string()).collect();

    rsx! {
        div {
            class: "relative",

            // Decorative gradient overlays - diagonal beams like reference
            div {
                aria_hidden: "true",
                class: "z-[2] absolute inset-0 pointer-events-none isolate hidden lg:block overflow-hidden",

                // Main beam - upper left
                div {
                    class: "absolute left-0 top-0 rounded-full",
                    style: "width: 35rem; height: 80rem; transform: translateY(-87.5%) rotate(-45deg); background: radial-gradient(68.54% 68.72% at 55.02% 31.46%, hsla(0,0%,85%,.04) 0, hsla(0,0%,55%,.01) 50%, hsla(0,0%,45%,0) 80%);"
                }

                // Secondary beam
                div {
                    class: "absolute left-0 top-0 rounded-full",
                    style: "width: 14rem; height: 80rem; transform: translate(5%, -50%) rotate(-45deg); background: radial-gradient(50% 50% at 50% 50%, hsla(0,0%,85%,.03) 0, hsla(0,0%,45%,.01) 80%, transparent 100%);"
                }

                // Third beam
                div {
                    class: "absolute left-0 top-0 rounded-full",
                    style: "width: 14rem; height: 80rem; transform: translateY(-87.5%) rotate(-45deg); background: radial-gradient(50% 50% at 50% 50%, hsla(0,0%,85%,.02) 0, hsla(0,0%,45%,.01) 80%, transparent 100%);"
                }

                // Additional beam - center
                div {
                    class: "absolute rounded-full",
                    style: "width: 20rem; height: 60rem; left: 30%; top: 0; transform: translateY(-60%) rotate(-45deg); background: radial-gradient(50% 50% at 50% 50%, hsla(0,0%,85%,.025) 0, hsla(0,0%,45%,.008) 80%, transparent 100%);"
                }

                // Additional beam - right side
                div {
                    class: "absolute rounded-full",
                    style: "width: 18rem; height: 50rem; right: 10%; top: 0; transform: translateY(-40%) rotate(-45deg); background: radial-gradient(50% 50% at 50% 50%, hsla(0,0%,85%,.02) 0, hsla(0,0%,45%,.006) 80%, transparent 100%);"
                }

                // Subtle beam - lower
                div {
                    class: "absolute rounded-full",
                    style: "width: 12rem; height: 40rem; left: 50%; top: 30%; transform: rotate(-45deg); background: radial-gradient(50% 50% at 50% 50%, hsla(0,0%,85%,.015) 0, transparent 80%);"
                }
            }

            // Hero section
            section {
                class: "overflow-hidden",

                div {
                    class: "relative mx-auto max-w-5xl px-6 py-28 lg:py-24",

                    div {
                        class: "relative z-10 mx-auto max-w-2xl text-center",

                        h1 {
                            class: "text-balance text-4xl font-semibold md:text-5xl lg:text-6xl text-foreground",
                            "Fast-Efficient Workflow"
                        }

                        p {
                            class: "mx-auto my-8 max-w-2xl text-lg md:text-xl text-muted-foreground",
                            "A complete music production toolset built in Rust. Seamless REAPER integration, powerful chart notation, and real-time P2P collaboration."
                        }

                        div {
                            class: "flex flex-col sm:flex-row items-center justify-center gap-3",

                            Link {
                                to: Route::ChartEditor {},
                                class: "inline-flex items-center justify-center gap-2 bg-primary text-primary-foreground hover:bg-primary/90 h-11 px-8 rounded-md font-medium transition-colors",
                                "Try the Editor"
                            }

                            Link {
                                to: Route::DocsHome {},
                                class: "inline-flex items-center justify-center gap-2 border border-border bg-background hover:bg-accent hover:text-accent-foreground h-11 px-8 rounded-md font-medium transition-colors",
                                "Read the Docs"
                            }
                        }
                    }
                }

                // Showcase section with perspective transform - live editor demo
                div {
                    class: "mx-auto -mt-16 max-w-7xl",
                    style: "-webkit-mask-image: linear-gradient(to bottom, black 50%, transparent 100%); mask-image: linear-gradient(to bottom, black 50%, transparent 100%);",

                    div {
                        class: "-mr-16 pl-16 lg:-mr-56 lg:pl-56",
                        style: "perspective: 1200px; -webkit-mask-image: linear-gradient(to right, black 50%, transparent 100%); mask-image: linear-gradient(to right, black 50%, transparent 100%);",

                        div {
                            style: "transform: rotateX(20deg);",

                            // Live editor card with skew (sized to content)
                            div {
                                class: "lg:h-[44rem] relative inline-block",
                                style: "transform: skewX(0.36rad);",

                                div {
                                    class: "rounded-lg z-[2] relative border border-border bg-card overflow-hidden h-full w-fit",

                                    // Editor header
                                    div {
                                        class: "flex items-center gap-2 px-4 py-3 border-b border-border bg-zinc-900/80",

                                        div { class: "w-3 h-3 rounded-full bg-red-500/80" }
                                        div { class: "w-3 h-3 rounded-full bg-yellow-500/80" }
                                        div { class: "w-3 h-3 rounded-full bg-green-500/80" }

                                        span {
                                            class: "ml-4 text-sm text-muted-foreground",
                                            "song_title.kf"
                                        }
                                    }

                                    // Typewriter animation drives the source signal
                                    components::ChartTypewriter {
                                        output: source,
                                        charts: charts.clone(),
                                        speed_ms: 35,
                                        delay_between_charts_ms: 4000
                                    }

                                    // Split view: Editor on left, Page preview on right (sized to A4 width)
                                    div {
                                        class: "flex h-[calc(100%-3rem)]",

                                        // Left side - Text editor (fixed width, read-only animation)
                                        div {
                                            class: "w-72 border-r border-border overflow-hidden shrink-0",

                                            components::HighlightedEditor {
                                                value: source(),
                                                on_change: move |_: String| {}, // Read-only for demo
                                                placeholder: "",
                                                textarea_id: Some("landing-editor".to_string())
                                            }
                                        }

                                        // Right side - Live chart preview (matches rendered page width)
                                        div {
                                            class: "overflow-hidden shrink-0",
                                            style: "width: 480px;",

                                            components::StaticChartRenderer {
                                                source: source,
                                                mode: preview_mode,
                                                canvas_id: Some("landing-chart-canvas".to_string())
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Features section
            section {
                class: "relative z-10 py-24",

                div {
                    class: "mx-auto max-w-5xl px-6",

                    h2 {
                        class: "text-center text-lg font-medium text-muted-foreground mb-16",
                        "Everything you need for modern music production"
                    }

                    div {
                        class: "grid md:grid-cols-3 gap-8",

                        HomeFeature {
                            title: "REAPER Integration",
                            description: "Deep DAW integration for transport control, MIDI routing, and real-time state synchronization.",
                            icon: rsx! { Music { class: "w-8 h-8" } }
                        }

                        HomeFeature {
                            title: "Keyflow Notation",
                            description: "Intuitive chart syntax with smart chord memory, section numbering, and GPU-accelerated rendering.",
                            icon: rsx! { FileText { class: "w-8 h-8" } }
                        }

                        HomeFeature {
                            title: "P2P Collaboration",
                            description: "Real-time collaboration powered by iroh for seamless peer-to-peer networking.",
                            icon: rsx! { Users { class: "w-8 h-8" } }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn HomeFeature(title: &'static str, description: &'static str, icon: Element) -> Element {
    rsx! {
        div {
            class: "group relative rounded-2xl border border-border/50 bg-card/30 p-8 transition-all hover:border-primary/30 hover:bg-card/50",

            div {
                class: "mb-4 inline-flex rounded-lg bg-primary/10 p-3 text-primary",
                {icon}
            }

            h3 {
                class: "text-lg font-semibold text-foreground mb-2",
                "{title}"
            }

            p {
                class: "text-muted-foreground leading-relaxed",
                "{description}"
            }
        }
    }
}

/// Docs home page - Keyflow documentation
#[component]
fn DocsHome() -> Element {
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
                    to: Route::SnippetsBrowser {},
                    class: "inline-flex items-center gap-2 bg-primary text-primary-foreground hover:bg-primary/90 px-6 py-3 rounded-lg font-semibold transition-colors",
                    BookOpen { class: "w-5 h-5" }
                    "Browse Snippets"
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

// =============================================================================
// Unified Snippets Browser - Better viewing experience with sidebar navigation
// =============================================================================

/// Unified snippets browser - shows all patterns with persistent sidebar
#[component]
fn SnippetsBrowser() -> Element {
    // Default to first pattern
    let patterns = keyflow::patterns::all_patterns();
    let first_id = patterns.first().map(|p| p.id).unwrap_or("minimal-chart");

    rsx! {
        UnifiedSnippetsView { selected_id: first_id.to_string() }
    }
}

/// Snippets view with specific pattern selected (for deep linking)
#[component]
fn SnippetsView(id: String) -> Element {
    rsx! {
        UnifiedSnippetsView { selected_id: id }
    }
}

/// The unified snippets viewer component
#[component]
fn UnifiedSnippetsView(selected_id: String) -> Element {
    use components::PreviewMode;
    use keyflow::patterns::{PatternCategory, all_patterns, find_pattern, patterns_by_category};

    // Get all patterns for navigation
    let patterns = all_patterns();

    // Find selected pattern
    let pattern = find_pattern(&selected_id);

    // Track expanded categories in sidebar
    let mut expanded_categories = use_signal(|| {
        // Start with all categories expanded
        PatternCategory::all().iter().map(|c| (*c, true)).collect::<std::collections::HashMap<_, _>>()
    });

    // Find current pattern index for prev/next navigation
    let current_index = patterns.iter().position(|p| p.id == selected_id).unwrap_or(0);
    let prev_pattern = if current_index > 0 { patterns.get(current_index - 1) } else { None };
    let next_pattern = patterns.get(current_index + 1);

    // Source state for editing
    let mut source = use_signal(|| {
        pattern.map(|p| p.source.to_string()).unwrap_or_default()
    });

    // Reset source when pattern changes
    let pattern_id = selected_id.clone();
    use_effect(move || {
        if let Some(p) = find_pattern(&pattern_id) {
            source.set(p.source.to_string());
        }
    });

    // Preview mode - snippet for most, page for examples
    let preview_mode = use_signal(|| {
        pattern.map(|p| {
            if p.category == PatternCategory::Examples {
                PreviewMode::Page
            } else {
                PreviewMode::Snippet
            }
        }).unwrap_or(PreviewMode::Snippet)
    });

    // Show/hide source panel
    let mut show_source = use_signal(|| true);

    rsx! {
        div {
            class: "flex h-[calc(100vh-4rem)]",

            // Sidebar - Pattern navigation
            aside {
                class: "w-72 bg-sidebar border-r border-sidebar-border flex flex-col",

                // Sidebar header
                div {
                    class: "p-4 border-b border-sidebar-border",
                    h2 {
                        class: "text-lg font-semibold text-sidebar-foreground",
                        "Snippets"
                    }
                    p {
                        class: "text-xs text-muted-foreground mt-1",
                        "{patterns.len()} interactive examples"
                    }
                }

                // Pattern list grouped by category
                nav {
                    class: "flex-1 overflow-y-auto p-2",

                    for category in PatternCategory::all() {
                        {
                            let category = *category;
                            let cat_patterns = patterns_by_category(category);
                            let is_expanded = expanded_categories.read().get(&category).copied().unwrap_or(true);
                            let has_selected = cat_patterns.iter().any(|p| p.id == selected_id);

                            rsx! {
                                div {
                                    class: "mb-2",

                                    // Category header (collapsible)
                                    button {
                                        class: "w-full flex items-center justify-between px-3 py-2 rounded-md text-sm font-medium text-sidebar-foreground hover:bg-sidebar-accent transition-colors",
                                        onclick: move |_| {
                                            let mut cats = expanded_categories.write();
                                            let current = cats.get(&category).copied().unwrap_or(true);
                                            cats.insert(category, !current);
                                        },

                                        span {
                                            class: if has_selected { "text-primary" } else { "" },
                                            "{category.label()}"
                                        }

                                        span {
                                            class: "text-xs text-muted-foreground",
                                            if is_expanded {
                                                lucide_dioxus::ChevronDown { class: "w-4 h-4" }
                                            } else {
                                                lucide_dioxus::ChevronRight { class: "w-4 h-4" }
                                            }
                                        }
                                    }

                                    // Pattern list (collapsible)
                                    if is_expanded {
                                        div {
                                            class: "ml-2 mt-1 space-y-0.5",

                                            for pattern in cat_patterns {
                                                {
                                                    let is_selected = pattern.id == selected_id;
                                                    rsx! {
                                                        Link {
                                                            to: Route::SnippetsView { id: pattern.id.to_string() },
                                                            class: if is_selected {
                                                                "block px-3 py-1.5 rounded-md text-sm bg-primary/10 text-primary font-medium border-l-2 border-primary"
                                                            } else {
                                                                "block px-3 py-1.5 rounded-md text-sm text-sidebar-foreground hover:bg-sidebar-accent transition-colors border-l-2 border-transparent"
                                                            },
                                                            "{pattern.title}"
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Sidebar footer with link to full editor
                div {
                    class: "p-4 border-t border-sidebar-border",
                    Link {
                        to: Route::ChartEditor {},
                        class: "flex items-center gap-2 text-sm text-muted-foreground hover:text-foreground transition-colors",
                        lucide_dioxus::PenLine { class: "w-4 h-4" }
                        "Open Full Editor"
                    }
                }
            }

            // Main content area
            div {
                class: "flex-1 flex flex-col min-w-0",

                match pattern {
                    Some(pattern) => rsx! {
                        // Header with pattern info and navigation
                        header {
                            class: "px-6 py-4 border-b border-border flex items-center justify-between shrink-0 bg-card/50",

                            // Pattern info
                            div {
                                class: "min-w-0",
                                div {
                                    class: "flex items-center gap-3",
                                    span {
                                        class: "text-xs font-medium text-primary bg-primary/10 px-2 py-0.5 rounded",
                                        "{pattern.category.label()}"
                                    }
                                    h1 {
                                        class: "text-xl font-semibold text-foreground truncate",
                                        "{pattern.title}"
                                    }
                                }
                                p {
                                    class: "text-sm text-muted-foreground mt-1 line-clamp-1",
                                    "{pattern.description}"
                                }
                            }

                            // Navigation and controls
                            div {
                                class: "flex items-center gap-2 shrink-0 ml-4",

                                // Prev/Next navigation
                                div {
                                    class: "flex items-center gap-1",

                                    if let Some(prev) = prev_pattern {
                                        Link {
                                            to: Route::SnippetsView { id: prev.id.to_string() },
                                            class: "p-2 rounded-md hover:bg-accent text-muted-foreground hover:text-foreground transition-colors",
                                            title: "Previous: {prev.title}",
                                            lucide_dioxus::ChevronLeft { class: "w-4 h-4" }
                                        }
                                    } else {
                                        span {
                                            class: "p-2 text-muted-foreground/30",
                                            lucide_dioxus::ChevronLeft { class: "w-4 h-4" }
                                        }
                                    }

                                    span {
                                        class: "text-xs text-muted-foreground px-2",
                                        "{current_index + 1} / {patterns.len()}"
                                    }

                                    if let Some(next) = next_pattern {
                                        Link {
                                            to: Route::SnippetsView { id: next.id.to_string() },
                                            class: "p-2 rounded-md hover:bg-accent text-muted-foreground hover:text-foreground transition-colors",
                                            title: "Next: {next.title}",
                                            lucide_dioxus::ChevronRight { class: "w-4 h-4" }
                                        }
                                    } else {
                                        span {
                                            class: "p-2 text-muted-foreground/30",
                                            lucide_dioxus::ChevronRight { class: "w-4 h-4" }
                                        }
                                    }
                                }

                                // Divider
                                div { class: "w-px h-6 bg-border mx-2" }

                                // Toggle source button
                                button {
                                    class: if *show_source.read() {
                                        "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-xs font-medium bg-primary/10 text-primary"
                                    } else {
                                        "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-xs font-medium text-muted-foreground hover:bg-accent transition-colors"
                                    },
                                    onclick: move |_| {
                                        let current = *show_source.peek();
                                        show_source.set(!current);
                                    },
                                    Code { class: "w-3.5 h-3.5" }
                                    "Source"
                                }
                            }
                        }

                        // Content area with chart and optional source panel
                        div {
                            class: "flex-1 flex overflow-hidden",

                            // Chart preview - takes remaining space
                            div {
                                class: "flex-1 overflow-hidden bg-muted/30",
                                components::DynamicChartRenderer {
                                    source: source,
                                    mode: preview_mode,
                                    canvas_id: Some(format!("snippet-canvas-{}", pattern.id))
                                }
                            }

                            // Source panel (collapsible)
                            if *show_source.read() {
                                div {
                                    class: "w-96 border-l border-border flex flex-col bg-card shrink-0",

                                    // Source header
                                    div {
                                        class: "px-4 py-3 border-b border-border flex items-center justify-between shrink-0",

                                        div {
                                            class: "flex items-center gap-2",
                                            lucide_dioxus::FileCode { class: "w-4 h-4 text-muted-foreground" }
                                            span {
                                                class: "text-sm font-medium text-foreground",
                                                "Source"
                                            }
                                        }

                                        // Reset button if modified
                                        if source.read().as_str() != pattern.source {
                                            button {
                                                class: "text-xs text-muted-foreground hover:text-foreground px-2 py-1 rounded hover:bg-accent transition-colors",
                                                onclick: move |_| source.set(pattern.source.to_string()),
                                                "Reset"
                                            }
                                        }
                                    }

                                    // Editable source
                                    div {
                                        class: "flex-1 overflow-hidden",
                                        components::HighlightedEditor {
                                            value: source(),
                                            on_change: move |v: String| source.set(v),
                                            placeholder: "Enter keyflow notation...",
                                            textarea_id: Some(format!("snippet-editor-{}", pattern.id))
                                        }
                                    }
                                }
                            }
                        }
                    },
                    None => {
                        // Pattern not found
                        rsx! {
                            div {
                                class: "flex-1 flex flex-col items-center justify-center text-center p-8",
                                lucide_dioxus::CircleAlert { class: "w-16 h-16 text-muted-foreground/50 mb-4" }
                                h2 {
                                    class: "text-xl font-semibold text-foreground mb-2",
                                    "Pattern Not Found"
                                }
                                p {
                                    class: "text-muted-foreground mb-6",
                                    "The pattern \"{selected_id}\" doesn't exist."
                                }
                                Link {
                                    to: Route::SnippetsBrowser {},
                                    class: "inline-flex items-center gap-2 px-4 py-2 rounded-md bg-primary text-primary-foreground hover:bg-primary/90 transition-colors",
                                    "Browse All Snippets"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// =============================================================================
// Legacy Routes - Redirect to new snippets browser
// =============================================================================

/// Legacy pattern browser - redirects to new snippets browser
#[component]
fn PatternBrowser() -> Element {
    // Use navigator for redirect
    let nav = use_navigator();
    use_effect(move || {
        nav.push(Route::SnippetsBrowser {});
    });
    rsx! {
        div { class: "flex items-center justify-center h-64 text-muted-foreground", "Redirecting..." }
    }
}

/// Chart editor with live preview
#[component]
fn ChartEditor() -> Element {
    rsx! {
        components::ChartEditor {}
    }
}

/// Legacy pattern view - redirects to new snippets view
#[component]
fn PatternView(id: String) -> Element {
    // Use navigator for redirect
    let nav = use_navigator();
    let id_clone = id.clone();
    use_effect(move || {
        nav.push(Route::SnippetsView { id: id_clone.clone() });
    });
    rsx! {
        div { class: "flex items-center justify-center h-64 text-muted-foreground", "Redirecting..." }
    }
}

/// Test page for debugging chart rendering - no transforms, just raw output
#[component]
fn TestRender() -> Element {
    // Source text state
    let mut source = use_signal(|| DEMO_CHARTS[0].to_string());
    // Use Page mode to see the full A4 page layout
    let preview_mode = use_signal(|| components::PreviewMode::Page);

    rsx! {
        div {
            class: "flex h-[calc(100vh-8rem)]",

            // Left side - Text editor
            div {
                class: "w-96 border-r border-border overflow-hidden",

                div {
                    class: "px-4 py-3 border-b border-border bg-card",
                    h3 {
                        class: "text-sm font-semibold",
                        "Source Code"
                    }
                }

                div {
                    class: "h-[calc(100%-3rem)]",
                    components::HighlightedEditor {
                        value: source(),
                        on_change: move |new_value: String| source.set(new_value),
                        placeholder: "Enter keyflow chart notation...",
                        textarea_id: Some("test-render-editor".to_string())
                    }
                }
            }

            // Right side - Chart preview (no transforms)
            div {
                class: "flex-1 bg-zinc-800 overflow-auto p-8",

                div {
                    class: "inline-block",
                    style: "border: 1px solid red;",

                    components::StaticChartRenderer {
                        source: source,
                        mode: preview_mode,
                        canvas_id: Some("test-render-canvas".to_string())
                    }
                }
            }
        }
    }
}
