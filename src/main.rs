//! FastTrackStudio Documentation Site
//!
//! Interactive documentation with live chart rendering for the keyflow parser.
//! Test patterns are displayed as interactive examples using WebGPU.

mod components;
mod renderer;
mod state;

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    ArrowLeft, BookOpen, ChevronDown, ChevronLeft, ChevronRight, Circle, Code, ExternalLink,
    FileCode, FileText, Github, ListMusic, Music, PenLine, PenTool, Play, SkipBack, SkipForward,
    Square, Users,
};
use fts_ui::prelude::*;

use audio_controls::widgets::{
    CompressorGraph, CompressorMetering, CompressorMode, CompressorParams, CompressorWidget,
    DbRange, EqBand, EqBandShape, EqGraph, GateDbRange, GateGraph, GateMetering, GateMode,
    GateParams,
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
    #[route("/mission")]
    Mission {},
    #[route("/projects")]
    Projects {},
    #[route("/docs")]
    DocsHome {},
    #[route("/keyflow/chart")]
    ChartEditor {},
    // Keyflow docs
    #[route("/docs/keyflow")]
    DocsKeyflow {},
    #[route("/docs/keyflow/snippets")]
    SnippetsBrowser {},
    #[route("/docs/keyflow/snippets/:id")]
    SnippetsView { id: String },
    // Other docs sections
    #[route("/docs/reaper")]
    DocsReaper {},
    #[route("/docs/desktop")]
    DocsDesktop {},
    #[route("/docs/plugins")]
    DocsPlugins {},
    // Legacy routes - redirect to new ones
    #[route("/docs/keyflow/chart/tests")]
    PatternBrowser {},
    #[route("/docs/keyflow/chart/tests/:id")]
    PatternView { id: String },
    #[route("/test/render")]
    TestRender {},
    #[route("/test/fx-ui")]
    TestFxUi {},
}

fn main() {
    // Initialize panic hook for better WASM debugging
    #[cfg(target_arch = "wasm32")]
    {
        use tracing_subscriber::layer::SubscriberExt;
        use tracing_subscriber::util::SubscriberInitExt;

        console_error_panic_hook::set_once();

        let filter = tracing_subscriber::EnvFilter::new(
            "warn,fasttrackstudio_web=debug,keyflow=info,keyflow::engraver=debug",
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
                EnvFilter::try_from_default_env().unwrap_or_else(|_| {
                    EnvFilter::new("warn,fasttrackstudio_web=debug,keyflow=info")
                }),
            )
            .init();
    }

    tracing::info!("Starting FastTrackStudio Documentation Site");

    dioxus::launch(App);
}

/// Root application component
#[component]
fn App() -> Element {
    let theme_state = use_signal(|| ThemeState::new(default_theme_preset(), ThemeMode::Dark));

    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        ThemeProvider {
            state: theme_state,
            Router::<Route> {}
        }
    }
}

/// Main layout with navigation
#[component]
fn Layout() -> Element {
    let route = use_route::<Route>();
    let is_home = matches!(route, Route::Home {});

    rsx! {
        div {
            class: "min-h-screen flex flex-col text-foreground",

            // Floating glass navbar
            nav {
            class: if is_home {
                "fixed top-0 left-0 right-0 z-50"
            } else {
                "sticky top-0 z-50 bg-background/80 backdrop-blur-xl border-b border-border/50"
            },

            div {
                class: "max-w-7xl mx-auto px-4 lg:px-8",

                div {
                    class: if is_home {
                        "flex items-center justify-between py-4 px-6 my-4 rounded-2xl bg-card/60 backdrop-blur-xl border border-white/[0.08] shadow-lg shadow-black/5"
                    } else {
                        "flex items-center justify-between py-3"
                    },

                    // Logo with icon
                    Link {
                        to: Route::Home {},
                        class: "flex items-center gap-3 group",

                        // Logo icon - stylized "F" with music note aesthetic
                        div {
                            class: "w-8 h-8 rounded-lg bg-gradient-to-br from-primary to-primary/60 flex items-center justify-center shadow-lg shadow-primary/20 group-hover:shadow-primary/40 transition-shadow",
                            span {
                                class: "text-primary-foreground font-black text-sm",
                                "F"
                            }
                        }

                        div {
                            class: "flex flex-col",
                            span {
                                class: "text-foreground font-semibold text-sm leading-tight group-hover:text-primary transition-colors",
                                "FastTrackStudio"
                            }
                            // Show section name based on current route
                            {
                                let section = match route {
                                    Route::ChartEditor {} => Some("Keyflow Editor"),
                                    Route::Mission {} => Some("Our Mission"),
                                    Route::Projects {} => Some("Projects"),
                                    Route::DocsHome {}
                                    | Route::DocsKeyflow {}
                                    | Route::DocsReaper {}
                                    | Route::DocsDesktop {}
                                    | Route::DocsPlugins {}
                                    | Route::SnippetsBrowser {}
                                    | Route::SnippetsView { .. }
                                    | Route::PatternBrowser {}
                                    | Route::PatternView { .. } => Some("Documentation"),
                                    _ => None,
                                };
                                if let Some(name) = section {
                                    rsx! {
                                        span {
                                            class: "text-muted-foreground text-xs leading-tight",
                                            "{name}"
                                        }
                                    }
                                } else {
                                    rsx! {}
                                }
                            }
                        }
                    }

                    // Center navigation (hidden on mobile)
                    div {
                        class: "hidden md:flex items-center gap-1 px-1.5 py-1.5 rounded-xl bg-muted/50",

                        NavLink {
                            to: Route::Mission {},
                            icon: rsx! { Users { class: "w-4 h-4" } },
                            label: "Mission"
                        }

                        NavLink {
                            to: Route::Projects {},
                            icon: rsx! { Code { class: "w-4 h-4" } },
                            label: "Projects"
                        }

                        NavLink {
                            to: Route::DocsHome {},
                            icon: rsx! { BookOpen { class: "w-4 h-4" } },
                            label: "Docs"
                        }

                        NavLink {
                            to: Route::ChartEditor {},
                            icon: rsx! { PenTool { class: "w-4 h-4" } },
                            label: "Editor"
                        }

                        NavLink {
                            to: Route::SnippetsBrowser {},
                            icon: rsx! { FileCode { class: "w-4 h-4" } },
                            label: "Snippets"
                        }
                    }

                    // Right side actions
                    div {
                        class: "flex items-center gap-2",

                        // GitHub link
                        a {
                            href: "https://github.com/codywright/FastTrackStudio",
                            target: "_blank",
                            class: "p-2 rounded-lg text-muted-foreground hover:text-foreground hover:bg-accent/50 transition-all",
                            title: "View on GitHub",
                            Github { class: "w-5 h-5" }
                        }

                        // Primary CTA button
                        Link {
                            to: Route::ChartEditor {},
                            class: "hidden sm:flex items-center gap-2 px-4 py-2 rounded-lg bg-primary text-primary-foreground font-medium text-sm hover:bg-primary/90 transition-colors shadow-lg shadow-primary/20 hover:shadow-primary/30",
                            PenTool { class: "w-4 h-4" }
                            span { "Try Editor" }
                        }
                    }
                }
            }
            }

            // Main content area
            main {
                class: if is_home { "flex-1" } else { "flex-1" },
                Outlet::<Route> {}
            }
        }
    }
}

/// Navigation link component with active state
#[component]
fn NavLink(to: Route, icon: Element, label: &'static str) -> Element {
    let current_route = use_route::<Route>();
    let is_active = std::mem::discriminant(&current_route) == std::mem::discriminant(&to);

    rsx! {
        Link {
            to: to,
            class: if is_active {
                "flex items-center gap-2 px-3 py-1.5 rounded-lg text-foreground bg-background shadow-sm text-sm font-medium transition-all"
            } else {
                "flex items-center gap-2 px-3 py-1.5 rounded-lg text-muted-foreground hover:text-foreground hover:bg-background/50 text-sm transition-all"
            },
            {icon}
            span { "{label}" }
        }
    }
}

/// Chart examples for the typewriter animation on the landing page.
const DEMO_CHARTS: &[&str] = &[
    // Complex funk/jazz - Thriller (Dirty Loops)
    r#"Thriller
120bpm 4/4 #Eb
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
            class: "relative pt-24",

            DecorativeBeams {}

            // Hero section
            section {
                class: "overflow-hidden",

                div {
                    class: "relative mx-auto max-w-5xl px-6 py-28 lg:py-24",

                    div {
                        class: "relative z-10 mx-auto max-w-4xl text-center",

                        h1 {
                            class: "text-balance",
                            div {
                                class: "text-5xl md:text-6xl lg:text-7xl font-semibold text-primary tracking-tight",
                                "Workflow-Driven"
                            }
                            div {
                                class: "mt-2 text-3xl md:text-4xl lg:text-5xl font-light text-muted-foreground tracking-tight",
                                "Made for Professionals."
                            }
                        }

                        div {
                            class: "mx-auto mt-12 grid gap-5 text-left md:grid-cols-2",

                            PillarCard {
                                number: "01",
                                title: "Open Source",
                                body: "Licensing practices in the audio software space are heavily anti-consumer. We need highly capable, pleasant tools that respect their users.",
                                icon: rsx! { Github { class: "w-5 h-5" } }
                            }

                            PillarCard {
                                number: "02",
                                title: "Cross-Platform",
                                body: "Designed for Linux, macOS, Windows, and embedded use cases. The fracture of available software is degrading quality everywhere.",
                                icon: rsx! { fts_ui::lucide_dioxus::Monitor { class: "w-5 h-5" } }
                            }

                            PillarCard {
                                number: "03",
                                title: "Cross-DAW",
                                body: "Built around Reaper today, on a core that extends to any DAW exposing the needed APIs. Your workflow shouldn\u{2019}t be locked to one vendor.",
                                icon: rsx! { Music { class: "w-5 h-5" } }
                            }

                            PillarCard {
                                number: "04",
                                title: "Open Format",
                                body: "Charts, project state, and protocols are documented and free to implement. Interoperability as a foundation, not an afterthought.",
                                icon: rsx! { FileCode { class: "w-5 h-5" } }
                            }
                        }
                    }
                }
            }

            // 3D Carousel — outside the overflow-hidden hero section so sticky works
            ShowcaseCarousel {
                source: source,
                preview_mode: preview_mode,
                charts: charts.clone()
            }

            ProjectTilesGrid {}

            // Setlist Control Preview section
            section {
                class: "relative z-10 py-24 bg-card/30",

                div {
                    class: "mx-auto max-w-5xl px-6",

                    div {
                        class: "text-center mb-12",
                        h2 {
                            class: "text-3xl font-bold text-foreground mb-4",
                            "Live Performance Control"
                        }
                        p {
                            class: "text-lg text-muted-foreground max-w-2xl mx-auto",
                            "Navigate your setlist with ease. Real-time section tracking, song navigation, and transport controls all in one unified view."
                        }
                    }

                    SetlistPreviewCard {}
                }
            }
        }
    }
}

/// Diagonal radial-gradient beams behind the home hero. Decorative only.
#[component]
fn DecorativeBeams() -> Element {
    // (width, height, position-css, transform-css, opacity_main, opacity_mid)
    // position-css is the corner anchor (e.g. "left-0 top-0" or arbitrary).
    let beams: &[(&str, &str, &str, &str, &str, &str)] = &[
        ("35rem", "80rem", "left: 0; top: 0;", "translateY(-87.5%) rotate(-45deg)", ".04", ".01"),
        ("14rem", "80rem", "left: 0; top: 0;", "translate(5%, -50%) rotate(-45deg)", ".03", ".01"),
        ("14rem", "80rem", "left: 0; top: 0;", "translateY(-87.5%) rotate(-45deg)", ".02", ".01"),
        ("20rem", "60rem", "left: 30%; top: 0;", "translateY(-60%) rotate(-45deg)", ".025", ".008"),
        ("18rem", "50rem", "right: 10%; top: 0;", "translateY(-40%) rotate(-45deg)", ".02", ".006"),
        ("12rem", "40rem", "left: 50%; top: 30%;", "rotate(-45deg)", ".015", "0"),
    ];

    rsx! {
        div {
            aria_hidden: "true",
            class: "z-[2] absolute inset-0 pointer-events-none isolate hidden lg:block overflow-hidden",
            for (w, h, pos, transform, op_main, op_mid) in beams.iter().copied() {
                div {
                    class: "absolute rounded-full",
                    style: "width: {w}; height: {h}; {pos} transform: {transform}; background: radial-gradient(50% 50% at 50% 50%, hsla(0,0%,85%,{op_main}) 0, hsla(0,0%,45%,{op_mid}) 80%, transparent 100%);"
                }
            }
        }
    }
}

/// Mission / About Us page
#[component]
fn Mission() -> Element {
    rsx! {
        div {
            class: "relative pt-24 pb-16",

            // Page header
            section {
                class: "relative mx-auto max-w-3xl px-6 py-16 text-center",

                h1 {
                    class: "text-4xl font-semibold md:text-5xl text-foreground mb-6",
                    "Our Mission"
                }

                p {
                    class: "text-xl text-muted-foreground",
                    "Why we build FastTrackStudio, and who we build it for."
                }
            }

            // Main content
            article {
                class: "mx-auto max-w-3xl px-6 space-y-12",

                // The Problem
                section {
                    class: "space-y-4",

                    h2 {
                        class: "text-2xl font-semibold text-foreground",
                        "The Problem We See"
                    }

                    p {
                        class: "text-lg leading-relaxed text-muted-foreground",
                        "Music production software has long been fragmented along platform lines. High-quality, thoughtfully designed tools are often locked to a single operating system, leaving musicians on Linux \u{2014} and increasingly macOS and Windows \u{2014} choosing between powerful software with poor user experience, or pleasant software that only runs on one platform."
                    }

                    p {
                        class: "text-lg leading-relaxed text-muted-foreground",
                        "Meanwhile, the formats and protocols that connect these tools remain proprietary, making interoperability an afterthought rather than a foundation."
                    }
                }

                // Our Approach
                section {
                    class: "space-y-4",

                    h2 {
                        class: "text-2xl font-semibold text-foreground",
                        "What We Believe"
                    }

                    div {
                        class: "space-y-6",

                        MissionPrinciple {
                            title: "Quality and Craft Are Not Optional",
                            body: "Every interaction should feel intentional. We invest in GPU-accelerated rendering, sub-frame latency, and refined typography not because they are trendy, but because musicians deserve tools that respect their craft with the same seriousness they bring to their art."
                        }

                        MissionPrinciple {
                            title: "Open Standards Move the Whole Industry Forward",
                            body: "Keyflow is an open chart notation format. Our protocols are documented. Our rendering pipeline is built on open GPU standards. When one tool improves, the ecosystem benefits \u{2014} not just our users, but anyone building for musicians."
                        }

                        MissionPrinciple {
                            title: "Every Platform, First-Class",
                            body: "Linux, macOS, and Windows are equal citizens. We don\u{2019}t port an afterthought \u{2014} we build cross-platform from day one using Rust, ensuring native performance and consistent behavior everywhere musicians work."
                        }

                        MissionPrinciple {
                            title: "Pleasant Experience Is a Feature",
                            body: "Powerful tools don\u{2019}t have to be hostile. We believe the best software disappears into the workflow \u{2014} fast enough that you never wait, intuitive enough that you rarely reach for the manual, and beautiful enough that you enjoy opening it."
                        }
                    }
                }

                // What We're Building
                section {
                    class: "space-y-4",

                    h2 {
                        class: "text-2xl font-semibold text-foreground",
                        "What We\u{2019}re Building"
                    }

                    p {
                        class: "text-lg leading-relaxed text-muted-foreground",
                        "FastTrackStudio is a suite of audio production tools: a chart notation language and renderer (Keyflow), deep REAPER DAW integration, a desktop performance app, audio plugins, and real-time peer-to-peer collaboration. Everything is built in Rust for reliability and performance, and designed to work together seamlessly or stand alone."
                    }

                    p {
                        class: "text-lg leading-relaxed text-muted-foreground",
                        "We\u{2019}re not trying to replace your DAW. We\u{2019}re building the tools that sit alongside it \u{2014} the chart on your music stand, the setlist on your iPad, the shared session with your bandmate across town \u{2014} and making them as good as they can possibly be."
                    }
                }

                // Call to action
                section {
                    class: "pt-8 pb-4 border-t border-border/30 text-center space-y-6",

                    p {
                        class: "text-lg text-muted-foreground",
                        "Want to see what we\u{2019}re building? Try the chart editor or explore the documentation."
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
        }
    }
}

/// A single mission principle with title and body text
#[component]
fn MissionPrinciple(title: &'static str, body: &'static str) -> Element {
    rsx! {
        div {
            class: "pl-4 border-l-2 border-primary/40",

            h3 {
                class: "text-lg font-medium text-foreground mb-2",
                "{title}"
            }

            p {
                class: "text-base leading-relaxed text-muted-foreground",
                "{body}"
            }
        }
    }
}

/// Projects overview page
#[component]
fn Projects() -> Element {
    rsx! {
        div {
            class: "relative pt-24 pb-24",

            // Page header
            section {
                class: "relative mx-auto max-w-5xl px-6 pt-16 pb-12 text-center",

                p {
                    class: "text-sm font-medium tracking-widest uppercase text-primary mb-4",
                    "Open Source"
                }

                h1 {
                    class: "text-4xl font-semibold md:text-5xl text-foreground mb-6",
                    "Projects"
                }

                p {
                    class: "text-lg text-muted-foreground max-w-xl mx-auto",
                    "Professional Workflow, Open Ecosystem."
                }
            }

            // Project grid
            div {
                class: "mx-auto max-w-5xl px-6 grid md:grid-cols-2 gap-5",

                ProjectCard {
                    name: "Keyflow",
                    tagline: "Chart Notation & Rendering",
                    description: "Plain-text notation for lead sheets, chord charts, and rhythm charts. Parses to a structured model, lays out with sub-millisecond passes, and renders publication-quality output via Vello\u{2019}s GPU pipeline.",
                    tags: vec!["notation", "rendering", "vello", "open-format"],
                    github: "https://github.com/FastTrackStudios/keyflow",
                    icon: rsx! { FileText { class: "w-5 h-5" } },
                    accent: "emerald",
                }

                ProjectCard {
                    name: "Session",
                    tagline: "Transport & State Coordination",
                    description: "The synchronization layer. Manages transport state, playback position, and shared session data across every tool in the ecosystem \u{2014} local or networked.",
                    tags: vec!["session", "transport", "sync", "protocol"],
                    github: "https://github.com/FastTrackStudios/session",
                    icon: rsx! { Music { class: "w-5 h-5" } },
                    accent: "violet",
                }

                ProjectCard {
                    name: "Signal",
                    tagline: "Audio Plugins & DSP",
                    description: "Instruments and effects targeting CLAP and VST3 from a single Rust codebase via nih-plug. Custom GPU-accelerated UIs built with the FTS design system.",
                    tags: vec!["audio", "plugins", "clap", "vst3", "nih-plug"],
                    github: "https://github.com/FastTrackStudios/signal",
                    icon: rsx! { fts_ui::lucide_dioxus::SlidersHorizontal { class: "w-5 h-5" } },
                    accent: "amber",
                }

                ProjectCard {
                    name: "DAW",
                    tagline: "REAPER Integration",
                    description: "Deep DAW bridge for REAPER. Bidirectional transport control, marker-driven chart navigation, MIDI routing, and real-time state broadcast over the FTS protocol.",
                    tags: vec!["reaper", "daw", "midi", "extension"],
                    github: "https://github.com/FastTrackStudios/daw",
                    icon: rsx! { fts_ui::lucide_dioxus::Monitor { class: "w-5 h-5" } },
                    accent: "blue",
                }

                ProjectCard {
                    name: "Input",
                    tagline: "Keyboard Design & Ergonomics",
                    description: "A wiki and design journal documenting keyboard layout decisions for REAPER and the fts-input system. Exploring efficient, ergonomic input mappings for professional audio workflows.",
                    tags: vec!["keyboard", "ergonomics", "reaper", "input", "wiki"],
                    github: "https://github.com/FastTrackStudios/input",

                    icon: rsx! { PenTool { class: "w-5 h-5" } },
                    accent: "cyan",
                }
            }

            // Bottom note
            section {
                class: "mx-auto max-w-5xl px-6 mt-16 text-center",

                p {
                    class: "text-sm text-muted-foreground/70",
                    "Built in Rust. Contributions welcome."
                }
            }
        }
    }
}

/// A project card for the Projects grid
#[component]
fn ProjectCard(
    name: &'static str,
    tagline: &'static str,
    description: &'static str,
    tags: Vec<&'static str>,
    github: &'static str,
    icon: Element,
    accent: &'static str,
) -> Element {
    let accent_glow = match accent {
        "emerald" => "group-hover:shadow-emerald-500/5",
        "violet" => "group-hover:shadow-violet-500/5",
        "blue" => "group-hover:shadow-blue-500/5",
        "amber" => "group-hover:shadow-amber-500/5",
        "cyan" => "group-hover:shadow-cyan-500/5",
        _ => "group-hover:shadow-primary/5",
    };
    let accent_icon_bg = match accent {
        "emerald" => "bg-emerald-500/10 text-emerald-400",
        "violet" => "bg-violet-500/10 text-violet-400",
        "blue" => "bg-blue-500/10 text-blue-400",
        "amber" => "bg-amber-500/10 text-amber-400",
        "cyan" => "bg-cyan-500/10 text-cyan-400",
        _ => "bg-primary/10 text-primary",
    };
    let accent_tag = match accent {
        "emerald" => "text-emerald-400/60",
        "violet" => "text-violet-400/60",
        "blue" => "text-blue-400/60",
        "amber" => "text-amber-400/60",
        "cyan" => "text-cyan-400/60",
        _ => "text-primary/60",
    };

    rsx! {
        a {
            href: "{github}",
            target: "_blank",
            class: "group rounded-2xl border border-border/50 bg-card/30 hover:bg-card/60 hover:border-border transition-all duration-200 shadow-lg shadow-black/5 {accent_glow} flex flex-col overflow-hidden",

            // Card body
            div {
                class: "p-7 flex-1 flex flex-col",

                // Icon bubble + name row
                div {
                    class: "flex items-center gap-4 mb-5",

                    div {
                        class: "w-10 h-10 rounded-xl {accent_icon_bg} flex items-center justify-center shrink-0",
                        {icon}
                    }

                    div {
                        h3 {
                            class: "text-lg font-semibold text-foreground group-hover:text-foreground transition-colors",
                            "{name}"
                        }
                        p {
                            class: "text-xs font-medium tracking-wide uppercase text-muted-foreground/70",
                            "{tagline}"
                        }
                    }
                }

                // Description
                p {
                    class: "text-sm leading-relaxed text-muted-foreground mb-6 flex-1",
                    "{description}"
                }

                // Tags row
                div {
                    class: "flex flex-wrap gap-x-3 gap-y-1 mb-5",
                    for tag in tags.iter() {
                        span {
                            class: "text-xs font-mono {accent_tag}",
                            "#{tag}"
                        }
                    }
                }

                // Footer: GitHub
                div {
                    class: "flex items-center gap-2 pt-4 border-t border-border/30 text-sm text-muted-foreground/60 group-hover:text-muted-foreground transition-colors",
                    Github { class: "w-4 h-4" }
                    span { class: "flex-1", "View on GitHub" }
                    ExternalLink { class: "w-3.5 h-3.5 opacity-0 -translate-x-1 group-hover:opacity-100 group-hover:translate-x-0 transition-all duration-200" }
                }
            }
        }
    }
}

/// Preview card for the setlist control feature on the landing page
#[component]
fn SetlistPreviewCard() -> Element {
    rsx! {
        div {
            class: "relative rounded-2xl border border-border/50 bg-card overflow-hidden",

            // Mock window chrome
            div {
                class: "flex items-center gap-2 px-4 py-3 border-b border-border bg-zinc-900/80",

                div { class: "w-3 h-3 rounded-full bg-red-500/80" }
                div { class: "w-3 h-3 rounded-full bg-yellow-500/80" }
                div { class: "w-3 h-3 rounded-full bg-green-500/80" }

                span {
                    class: "ml-4 text-sm text-muted-foreground",
                    "Setlist Control"
                }
            }

            // Preview of the setlist view (constrained height)
            div {
                class: "h-96 overflow-hidden flex items-center justify-center bg-muted/30 rounded-lg",
                div { class: "text-center text-muted-foreground",
                    p { "Performance View Preview" }
                    p { class: "text-sm mt-2 opacity-50", "(Coming soon)" }
                }
            }

            // Overlay with CTA
            div {
                class: "absolute bottom-0 left-0 right-0 bg-gradient-to-t from-card via-card/95 to-transparent pt-16 pb-6 px-6 text-center",

                Link {
                    to: Route::DocsDesktop {},
                    class: "inline-flex items-center justify-center gap-2 bg-primary text-primary-foreground hover:bg-primary/90 h-11 px-8 rounded-md font-medium transition-colors",
                    ListMusic { class: "w-5 h-5" }
                    "Learn More"
                }
            }
        }
    }
}

#[component]
fn HomeFeature(title: &'static str, description: &'static str, icon: Element) -> Element {
    rsx! {
        Card {
            class: "group relative border-border/50 bg-card/30 p-8 transition-all hover:border-primary/30 hover:bg-card/50",

            div {
                class: "mb-4 inline-flex rounded-lg bg-primary/10 p-3 text-primary",
                {icon}
            }

            Heading { level: HeadingLevel::H5, class: "text-foreground mb-2", "{title}" }

            Text { variant: TextVariant::Muted, class: "leading-relaxed", "{description}" }
        }
    }
}

/// One of the four hero-pillar cards sitting beneath the tagline.
/// Numbered manifesto-style card with icon, animated top accent, hover lift.
#[component]
fn PillarCard(
    number: &'static str,
    title: &'static str,
    body: &'static str,
    icon: Element,
) -> Element {
    rsx! {
        div {
            class: "group relative overflow-hidden rounded-xl border border-border/40 bg-gradient-to-br from-card/50 to-card/10 p-6 transition-all duration-300 hover:-translate-y-0.5 hover:border-primary/40 hover:from-card/70 hover:to-card/20 hover:shadow-xl hover:shadow-primary/5",

            // Animated top accent line — fades in on hover
            div {
                class: "absolute inset-x-0 top-0 h-px bg-gradient-to-r from-transparent via-primary/70 to-transparent opacity-0 transition-opacity duration-300 group-hover:opacity-100",
            }

            div {
                class: "mb-5 flex items-center justify-between",

                div {
                    class: "inline-flex h-10 w-10 items-center justify-center rounded-lg bg-primary/10 text-primary transition-colors duration-300 group-hover:bg-primary/20",
                    {icon}
                }

                span {
                    class: "font-mono text-xs tracking-[0.2em] text-muted-foreground/50",
                    "{number}"
                }
            }

            h3 {
                class: "mb-2 text-lg font-semibold text-foreground transition-colors duration-300 group-hover:text-primary",
                "{title}"
            }

            p {
                class: "text-sm leading-relaxed text-muted-foreground",
                "{body}"
            }
        }
    }
}

/// Landing page docs section card with colored accent
#[component]
fn LandingDocsCard(
    to: Route,
    title: &'static str,
    description: &'static str,
    icon: Element,
) -> Element {
    rsx! {
        Link {
            to: to,
            class: "group flex items-center gap-4 p-5 rounded-xl border border-border/50 bg-card/30 transition-all hover:border-primary/40 hover:shadow-lg hover:bg-card/50",

            div {
                class: "shrink-0 rounded-lg p-2.5 bg-primary/10 text-primary",
                {icon}
            }

            div {
                class: "min-w-0",
                h3 {
                    class: "font-semibold text-foreground group-hover:text-primary transition-colors",
                    "{title}"
                }
                p {
                    class: "text-sm text-muted-foreground truncate",
                    "{description}"
                }
            }

            ChevronRight {
                class: "w-5 h-5 text-muted-foreground/50 group-hover:text-muted-foreground ml-auto shrink-0 transition-colors"
            }
        }
    }
}

// =============================================================================
// Project Tiles — six animated tiles, one per project, themed individually.
// Ported from the rockstars-expansion deck (slides/decks/_project-themes.tsx)
// using pure CSS keyframes for the looping animations.
// =============================================================================

#[derive(Clone, Copy, PartialEq)]
enum BgKind {
    Keyflow,
    Session,
    Signal,
    Input,
    Daw,
    Plugins,
}

#[derive(Clone, Copy, PartialEq)]
struct ProjectTheme {
    num: &'static str,
    name: &'static str,
    tagline: &'static str,
    desc: &'static str,
    glyph: &'static str,
    accent: &'static str,
    bg: &'static str,
    version: &'static str,
    bg_kind: BgKind,
}

const PROJECT_THEMES: &[ProjectTheme] = &[
    ProjectTheme {
        num: "01", name: "Keyflow", tagline: "Charts as code",
        desc: "Plain-text music format that compiles into real lead sheets.",
        glyph: ".kf", accent: "#a78bfa", bg: "#0d0a14", version: "alpha v.0.0.1", bg_kind: BgKind::Keyflow,
    },
    ProjectTheme {
        num: "02", name: "Session", tagline: "Performance brain",
        desc: "Setlist \u{00B7} song \u{00B7} section navigation across the network.",
        glyph: "\u{2192}\u{2192}", accent: "#86efac", bg: "#0a1310", version: "alpha v.0.0.1", bg_kind: BgKind::Session,
    },
    ProjectTheme {
        num: "03", name: "Signal", tagline: "The audio rig",
        desc: "Plugin chains, profiles, snapshots, live morphing.",
        glyph: "\u{224B}", accent: "#60a5fa", bg: "#0a1018", version: "alpha v.0.0.1", bg_kind: BgKind::Signal,
    },
    ProjectTheme {
        num: "04", name: "Input", tagline: "Wiring closet",
        desc: "MIDI, keys, hardware controllers \u{2014} into the action system.",
        glyph: "I/O", accent: "#a1a1aa", bg: "#0f0f12", version: "alpha v.0.0.1", bg_kind: BgKind::Input,
    },
    ProjectTheme {
        num: "05", name: "DAW", tagline: "REAPER layer",
        desc: "Unified API. Transport, tracks, FX, project files.",
        glyph: "\u{23F5}", accent: "#52525b", bg: "#050507", version: "alpha v.0.0.1", bg_kind: BgKind::Daw,
    },
    ProjectTheme {
        num: "06", name: "Plugins", tagline: "DSP suite",
        desc: "In-house CLAP/VST3 plugins with detachable GUI.",
        glyph: "FX", accent: "#b54234", bg: "#140a08", version: "alpha v.0.0.1", bg_kind: BgKind::Plugins,
    },
];

/// Convert "#rrggbb" + alpha to "rgba(r,g,b,a)" for inline gradients.
fn hex_rgba(hex: &str, alpha: f32) -> String {
    let h = hex.trim_start_matches('#');
    let r = u8::from_str_radix(h.get(0..2).unwrap_or("00"), 16).unwrap_or(0);
    let g = u8::from_str_radix(h.get(2..4).unwrap_or("00"), 16).unwrap_or(0);
    let b = u8::from_str_radix(h.get(4..6).unwrap_or("00"), 16).unwrap_or(0);
    format!("rgba({r},{g},{b},{alpha})")
}

/// Keyframes used by all the project-tile backgrounds. Injected once.
const PROJECT_TILE_KEYFRAMES: &str = "
@keyframes pt-scan-x { from { left: 0%; } to { left: 100%; } }
@keyframes pt-drift  { from { left: -12%; } to { left: 112%; } }
@keyframes pt-eq     { 0%,100% { height: 6px; } 50% { height: var(--pt-peak, 30px); } }
";

#[component]
fn ProjectTilesGrid() -> Element {
    rsx! {
        document::Style { {PROJECT_TILE_KEYFRAMES} }

        section {
            class: "relative z-10 py-24",

            div {
                class: "mx-auto max-w-[96rem] px-6",

                div {
                    class: "mb-14 text-center",
                    p {
                        class: "font-mono text-xs uppercase tracking-[0.25em] text-muted-foreground/70 mb-3",
                        "Currently in development"
                    }
                    h2 {
                        class: "text-3xl md:text-4xl lg:text-5xl font-semibold text-foreground tracking-tight",
                        "Check Out the Currently Active Projects"
                    }
                }

                div {
                    class: "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-5 lg:gap-6",
                    for theme in PROJECT_THEMES.iter().copied() {
                        ProjectTile { theme: theme }
                    }
                }
            }
        }
    }
}

#[component]
fn ProjectTile(theme: ProjectTheme) -> Element {
    let style = format!(
        "background-color: {bg}; --pt-accent: {accent}; border-color: rgba(255,255,255,0.06);",
        bg = theme.bg, accent = theme.accent
    );
    rsx! {
        div {
            class: "group relative overflow-hidden rounded-md border p-7 md:p-8 lg:min-h-[20rem] h-full transition-all duration-300 hover:-translate-y-0.5 hover:[border-color:var(--pt-accent)]",
            style: "{style}",

            // Animated motif painted behind the content
            match theme.bg_kind {
                BgKind::Keyflow => rsx! { BgKeyflow { color: theme.accent } },
                BgKind::Session => rsx! { BgSession { color: theme.accent } },
                BgKind::Signal  => rsx! { BgSignal  { color: theme.accent } },
                BgKind::Input   => rsx! { BgInput   { color: theme.accent } },
                BgKind::Daw     => rsx! { BgDaw     { color: theme.accent } },
                BgKind::Plugins => rsx! { BgPlugins { color: theme.accent } },
            }

            // Top row: project number + version (left), glyph (right)
            div {
                class: "relative z-10 flex items-start justify-between mb-8 lg:mb-10",
                div {
                    class: "flex flex-col gap-1",
                    div {
                        class: "font-mono text-[0.65rem] lg:text-xs uppercase tracking-[0.25em] text-muted-foreground",
                        "Project {theme.num}"
                    }
                    div {
                        class: "font-mono text-[0.6rem] lg:text-[0.65rem] uppercase tracking-[0.25em]",
                        style: "color: {theme.accent}; opacity: 0.75;",
                        "{theme.version}"
                    }
                }
                div {
                    class: "font-mono text-lg lg:text-xl tracking-tight opacity-80",
                    style: "color: {theme.accent};",
                    "{theme.glyph}"
                }
            }

            // Name, tagline, description
            div {
                class: "relative z-10",
                div {
                    class: "text-4xl lg:text-5xl font-semibold tracking-tight text-foreground leading-none",
                    "{theme.name}"
                }
                div {
                    class: "font-mono text-[0.7rem] lg:text-xs tracking-[0.18em] uppercase mt-3 lg:mt-4",
                    style: "color: {theme.accent};",
                    "{theme.tagline}"
                }
                p {
                    class: "text-sm lg:text-base leading-relaxed text-muted-foreground mt-4 lg:mt-5 max-w-[32ch]",
                    "{theme.desc}"
                }
            }

            // Accent bar (bottom-left, wider on desktop)
            div {
                class: "absolute bottom-0 left-0 h-[2px] w-12 lg:w-16 z-10",
                style: "background-color: {theme.accent};",
            }
        }
    }
}

// ----- Per-project animated backgrounds (CSS keyframes only) -----

/// Keyflow — drifting musical glyphs over five-line staff.
#[component]
fn BgKeyflow(color: String) -> Element {
    let glyphs: [(&str, f32); 12] = [
        ("\u{1D11E}", 1.6),
        ("\u{2669}", 1.0),
        ("\u{266A}", 1.0),
        ("\u{266B}", 1.1),
        ("\u{266C}", 1.1),
        ("\u{266D}", 0.9),
        ("\u{266F}", 0.9),
        ("\u{1D122}", 1.4),
        ("\u{266E}", 0.9),
        ("\u{1D110}", 1.0),
        ("\u{1D13B}", 0.95),
        ("\u{1D13D}", 0.95),
    ];
    rsx! {
        div {
            class: "absolute inset-0 overflow-hidden pointer-events-none",
            // Five-line staff
            svg {
                class: "absolute inset-x-0 top-1/2 -translate-y-1/2 w-full opacity-[0.07]",
                style: "height: 3rem;",
                preserve_aspect_ratio: "none",
                view_box: "0 0 100 40",
                for i in 0..5 {
                    line {
                        x1: "0", y1: "{4 + i * 8}", x2: "100", y2: "{4 + i * 8}",
                        stroke: "{color}", stroke_width: "0.2",
                    }
                }
            }
            // Drifting glyphs
            for (i, (ch, size)) in glyphs.iter().enumerate() {
                span {
                    key: "{i}",
                    class: "absolute select-none leading-none",
                    style: "color: {color}; opacity: 0.22; top: {10 + (i * 17) % 70}%; font-size: {size}rem; animation: pt-drift {18 + (i % 5) * 3}s linear infinite; animation-delay: -{i * 2}s;",
                    "{ch}"
                }
            }
        }
    }
}

/// Session — section labels along the bottom, scanning playhead.
#[component]
fn BgSession(color: String) -> Element {
    let sections = ["INTRO", "VS 1", "CH", "VS 2", "CH", "BR", "OUT"];
    rsx! {
        div {
            class: "absolute inset-0 overflow-hidden pointer-events-none flex items-end pb-6",
            div {
                class: "relative w-full px-6 flex justify-between",
                for (i, s) in sections.iter().enumerate() {
                    span {
                        key: "{i}",
                        class: "font-mono tracking-[0.25em]",
                        style: "color: {color}; opacity: 0.18; font-size: 0.55rem;",
                        "{s}"
                    }
                }
                // Scanning playhead — vertical hairline crossing the tile
                div {
                    class: "absolute w-px",
                    style: "background: {color}; opacity: 0.5; top: -30px; bottom: -30px; animation: pt-scan-x 7s linear infinite;",
                }
            }
        }
    }
}

/// Signal — full-width pulsing equalizer bars.
#[component]
fn BgSignal(color: String) -> Element {
    let count = 42usize;
    rsx! {
        div {
            class: "absolute inset-0 overflow-hidden pointer-events-none flex items-end justify-between pb-1 px-1",
            for i in 0..count {
                {
                    let peak = 10 + ((i * 17) % 52);
                    let dur = 1.6 + ((i % 7) as f32) * 0.2;
                    let delay = ((i as f32 * 0.05) % 1.4) - 1.4; // negative so animations start mid-cycle
                    rsx! {
                        span {
                            key: "{i}",
                            class: "block rounded-t-sm",
                            style: "background: {color}; opacity: 0.18; width: calc((100% - {count}px) / {count}); min-width: 2px; --pt-peak: {peak}px; animation: pt-eq {dur}s ease-in-out infinite; animation-delay: {delay}s;",
                        }
                    }
                }
            }
        }
    }
}

/// Input — static keyboard render (key-press timeline deferred to dioxus-motion port).
#[component]
fn BgInput(color: String) -> Element {
    let rows: &[&[(f32, &str)]] = &[
        &[(1.5, "esc"), (1.0, "1"), (1.0, "2"), (1.0, "3"), (1.0, "4"), (1.0, "5"), (1.0, "6"), (1.0, "7"), (1.0, "8"), (1.0, "9"), (1.0, "0"), (1.5, "\u{232B}")],
        &[(1.5, "\u{21E5}"), (1.0, "Q"), (1.0, "W"), (1.0, "E"), (1.0, "R"), (1.0, "T"), (1.0, "Y"), (1.0, "U"), (1.0, "I"), (1.0, "O"), (1.0, "P"), (1.5, "\\")],
        &[(1.75, "\u{21EA}"), (1.0, "A"), (1.0, "S"), (1.0, "D"), (1.0, "F"), (1.0, "G"), (1.0, "H"), (1.0, "J"), (1.0, "K"), (1.0, "L"), (2.25, "\u{21B5}")],
        &[(1.5, "\u{21E7}"), (1.0, "Z"), (1.0, "X"), (1.0, "C"), (1.0, "V"), (1.0, "B"), (1.0, "N"), (1.0, "M"), (1.0, ","), (1.0, "."), (1.0, "/"), (1.5, "\u{21E7}")],
        &[(1.25, "ctrl"), (1.25, "\u{2325}"), (1.5, "\u{2318}"), (6.25, ""), (1.5, "\u{2318}"), (1.25, "\u{2325}")],
    ];
    let unit = 14.0_f32;
    let gap = 2.0_f32;
    let bg_grad = format!(
        "linear-gradient(180deg, {} 0%, transparent 100%)",
        hex_rgba(&color, 0.05)
    );
    rsx! {
        div {
            class: "absolute inset-0 overflow-hidden pointer-events-none flex items-end justify-end pb-3 pr-3",
            div {
                class: "flex flex-col",
                style: "gap: {gap}px; padding: 4px;",
                for (r_idx, row) in rows.iter().enumerate() {
                    div {
                        key: "{r_idx}",
                        class: "flex",
                        style: "gap: {gap}px;",
                        for (k_idx, (w, label)) in row.iter().enumerate() {
                            {
                                let width = w * unit + (w - 1.0) * gap;
                                rsx! {
                                    div {
                                        key: "{k_idx}",
                                        class: "flex items-center justify-center font-mono select-none",
                                        style: "width: {width}px; height: {unit}px; font-size: 0.5rem; color: {color}; border-radius: 3px; border: 1px solid {color}; background: {bg_grad}; opacity: 0.4;",
                                        "{label}"
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

/// DAW — tape transport ruler with scanning playhead.
#[component]
fn BgDaw(color: String) -> Element {
    rsx! {
        div {
            class: "absolute inset-0 overflow-hidden pointer-events-none",
            // Ruler ticks along the bottom
            div {
                class: "absolute inset-x-0 flex items-end gap-[6px] px-4",
                style: "bottom: 0.75rem; height: 0.75rem;",
                for i in 0..36 {
                    {
                        let h = if i % 4 == 0 { "10px" } else { "5px" };
                        rsx! {
                            span {
                                key: "{i}",
                                class: "block w-px",
                                style: "background: {color}; opacity: 0.22; height: {h};",
                            }
                        }
                    }
                }
            }
            // Playhead
            div {
                class: "absolute",
                style: "top: 0.5rem; bottom: 0.5rem; width: 1.5px; background: {color}; opacity: 0.5; animation: pt-scan-x 9s linear infinite;",
            }
        }
    }
}

/// Plugins — static EQ curve + compressor + GR meter (path morphing deferred).
#[component]
fn BgPlugins(color: String) -> Element {
    rsx! {
        div {
            class: "absolute inset-0 overflow-hidden pointer-events-none",
            // EQ curve (top half)
            svg {
                class: "absolute inset-x-0 top-0 w-full opacity-30",
                style: "height: 50%;",
                view_box: "0 0 200 80",
                preserve_aspect_ratio: "none",
                line { x1: "0", y1: "40", x2: "200", y2: "40", stroke: "{color}", stroke_width: "0.4", stroke_dasharray: "2,2", opacity: "0.4" }
                for x in [40, 80, 120, 160].iter() {
                    line { key: "{x}", x1: "{x}", y1: "36", x2: "{x}", y2: "44", stroke: "{color}", stroke_width: "0.3", opacity: "0.4" }
                }
                path {
                    fill: "none",
                    stroke: "{color}",
                    stroke_width: "1.1",
                    d: "M0,55 C20,55 30,28 50,30 S80,55 100,40 S140,18 160,30 S190,52 200,46",
                }
                circle { cx: "50",  cy: "30", r: "2.5", fill: "{color}", opacity: "0.6" }
                circle { cx: "100", cy: "40", r: "2.5", fill: "{color}", opacity: "0.6" }
                circle { cx: "160", cy: "30", r: "2.5", fill: "{color}", opacity: "0.6" }
            }
            // Midline divider
            div {
                class: "absolute left-2 right-2 h-px",
                style: "top: 50%; background: {color}; opacity: 0.15;",
            }
            // Compressor curve (bottom-left)
            svg {
                class: "absolute opacity-30",
                style: "bottom: 0.5rem; left: 0.5rem; width: 66%; height: 42%;",
                view_box: "0 0 100 60",
                preserve_aspect_ratio: "none",
                line { x1: "0", y1: "60", x2: "100", y2: "60", stroke: "{color}", stroke_width: "0.4", opacity: "0.6" }
                line { x1: "0", y1: "0",  x2: "0",   y2: "60", stroke: "{color}", stroke_width: "0.4", opacity: "0.6" }
                line { x1: "0", y1: "60", x2: "100", y2: "0",  stroke: "{color}", stroke_width: "0.3", stroke_dasharray: "2,2", opacity: "0.3" }
                path { fill: "none", stroke: "{color}", stroke_width: "1.2", d: "M0,60 L42,18 Q50,12 60,12 L100,2" }
            }
            // GR meter (bottom-right)
            div {
                class: "absolute flex flex-col items-center gap-1",
                style: "bottom: 0.5rem; right: 0.75rem;",
                div {
                    class: "font-mono tracking-[0.15em]",
                    style: "color: {color}; opacity: 0.5; font-size: 0.5rem;",
                    "GR"
                }
                div {
                    class: "rounded-sm relative overflow-hidden",
                    style: "width: 6px; height: 36px; border: 1px solid {color}; opacity: 0.5;",
                    div {
                        class: "absolute inset-x-0 top-0",
                        style: "background: {color}; height: 40%;",
                    }
                }
            }
        }
    }
}

// =============================================================================
// Horizontal Showcase Strip with Perspective
// =============================================================================

/// Number of cards in the showcase
const SHOWCASE_CARD_COUNT: usize = 4;

/// Individual card widths in pixels
const CARD_WIDTHS: [i32; 4] = [724, 480, 480, 480];

/// Gap between cards in pixels
const CARD_GAP: i32 = 24;

/// Horizontal showcase strip. Native horizontal scroll — does not hijack page scroll.
#[component]
fn ShowcaseCarousel(
    source: Signal<String>,
    preview_mode: Signal<components::PreviewMode>,
    charts: Vec<String>,
) -> Element {
    let n = SHOWCASE_CARD_COUNT;

    rsx! {
        // Outer wrapper hosts native horizontal scroll. No height runway, no sticky,
        // no scroll hijack — user controls vertical scroll, can pan horizontally if curious.
        div {
            class: "relative w-full overflow-x-auto overflow-y-hidden",
            style: "-webkit-mask-image: linear-gradient(to bottom, black 40%, transparent 100%); mask-image: linear-gradient(to bottom, black 40%, transparent 100%);",

            div {
                style: "perspective: 1200px;",

                div {
                    class: "pl-8 md:pl-16 lg:pl-32 pr-8",
                    style: "transform: rotateX(20deg) skewX(0.36rad);",

                    div {
                        class: "h-[44rem] lg:h-[52rem]",

                        div {
                            class: "flex",
                            style: "gap: {CARD_GAP}px;",

                            for i in 0..n {
                                {
                                    let card_width = CARD_WIDTHS[i];
                                    rsx! {
                                        div {
                                            key: "{i}",
                                            class: "shrink-0",
                                            style: "width: {card_width}px;",

                                            match i {
                                                0 => rsx! {
                                                    KeyflowShowcaseCard {
                                                        source: source,
                                                        preview_mode: preview_mode,
                                                        charts: charts.clone()
                                                    }
                                                },
                                                1 => rsx! { DesktopShowcaseCard {} },
                                                2 => rsx! { ReaperShowcaseCard {} },
                                                _ => rsx! { PluginsShowcaseCard {} },
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
}

/// Keyflow Editor showcase card
#[component]
fn KeyflowShowcaseCard(
    source: Signal<String>,
    preview_mode: Signal<components::PreviewMode>,
    charts: Vec<String>,
) -> Element {
    rsx! {
        div {
            class: "rounded-lg border border-border bg-card overflow-hidden shadow-2xl w-full",
            style: "height: 600px;",

            // Window header
            div {
                class: "flex items-center gap-2 px-4 py-3 border-b border-border bg-zinc-900/80",

                div { class: "w-3 h-3 rounded-full bg-red-500/80" }
                div { class: "w-3 h-3 rounded-full bg-yellow-500/80" }
                div { class: "w-3 h-3 rounded-full bg-green-500/80" }

                span {
                    class: "ml-4 text-sm text-muted-foreground",
                    "song_title.kf"
                }

                span {
                    class: "ml-auto text-xs text-emerald-500 font-medium px-2 py-0.5 rounded bg-emerald-500/10",
                    "Keyflow"
                }
            }

            // Typewriter animation (hidden, drives source signal)
            components::ChartTypewriter {
                output: source,
                charts: charts,
                speed_ms: 35,
                delay_between_charts_ms: 4000
            }

            // Split view content
            div {
                class: "flex h-[calc(100%-3rem)]",

                // Editor side
                div {
                    class: "w-56 border-r border-border overflow-hidden shrink-0",

                    components::HighlightedEditor {
                        value: source(),
                        on_change: move |_: String| {},
                        placeholder: "",
                        textarea_id: Some("carousel-editor".to_string())
                    }
                }

                // Preview side
                div {
                    class: "overflow-hidden flex-1",

                    components::StaticChartRenderer {
                        source: source,
                        mode: preview_mode,
                        canvas_id: Some("carousel-chart-canvas".to_string()),
                        fixed_layout_width: Some(180.0)
                    }
                }
            }
        }
    }
}

/// Desktop App showcase card with tempo UI
#[component]
fn DesktopShowcaseCard() -> Element {
    let tempo = 120;

    rsx! {
        div {
            class: "rounded-lg border border-border bg-card overflow-hidden shadow-2xl w-full",
            style: "height: 600px;",

            div {
                class: "flex items-center gap-2 px-4 py-3 border-b border-border bg-zinc-900/80",
                div { class: "w-3 h-3 rounded-full bg-red-500/80" }
                div { class: "w-3 h-3 rounded-full bg-yellow-500/80" }
                div { class: "w-3 h-3 rounded-full bg-green-500/80" }
                span { class: "ml-4 text-sm text-muted-foreground", "Session" }
                span { class: "ml-auto text-xs text-blue-500 font-medium px-2 py-0.5 rounded bg-blue-500/10", "Session" }
            }

            div {
                class: "relative h-[calc(100%-3rem)] bg-zinc-900 flex flex-col items-center justify-center",
                span { class: "text-xs uppercase tracking-wider text-muted-foreground mb-3", "Tempo" }
                div {
                    class: "flex items-baseline",
                    span { class: "text-9xl font-bold tabular-nums text-foreground", "{tempo}" }
                    span { class: "text-3xl font-medium text-muted-foreground ml-3", "BPM" }
                }
                div {
                    class: "flex items-center gap-4 mt-10",
                    for i in 0..4 {
                        div {
                            key: "{i}",
                            class: if i == 0 { "w-5 h-5 rounded-full bg-primary animate-pulse" } else { "w-4 h-4 rounded-full bg-muted-foreground/30" }
                        }
                    }
                }
                div {
                    class: "absolute bottom-8 flex items-center gap-10 text-muted-foreground/50",
                    SkipBack { class: "w-7 h-7" }
                    div {
                        class: "p-4 rounded-full bg-primary/20 text-primary",
                        Play { class: "w-10 h-10" }
                    }
                    SkipForward { class: "w-7 h-7" }
                }
            }
        }
    }
}

/// REAPER Extension showcase card
#[component]
fn ReaperShowcaseCard() -> Element {
    rsx! {
        div {
            class: "rounded-lg border border-border bg-card overflow-hidden shadow-2xl w-full",
            style: "height: 600px;",

            div {
                class: "flex items-center gap-2 px-4 py-3 border-b border-border bg-zinc-900/80",
                div { class: "w-3 h-3 rounded-full bg-red-500/80" }
                div { class: "w-3 h-3 rounded-full bg-yellow-500/80" }
                div { class: "w-3 h-3 rounded-full bg-green-500/80" }
                span { class: "ml-4 text-sm text-muted-foreground", "Reaper" }
                span { class: "ml-auto text-xs text-violet-500 font-medium px-2 py-0.5 rounded bg-violet-500/10", "Extension" }
            }

            div {
                class: "relative h-[calc(100%-3rem)] bg-zinc-900 p-5",

                div {
                    class: "flex items-center gap-4 mb-5 p-4 rounded-lg bg-zinc-800/50 border border-border/50",
                    div {
                        class: "flex items-center gap-2",
                        div { class: "w-10 h-10 rounded bg-red-500/20 flex items-center justify-center text-red-500",
                            Circle { class: "w-5 h-5 fill-current" }
                        }
                        div { class: "w-10 h-10 rounded bg-primary/20 flex items-center justify-center text-primary",
                            Play { class: "w-5 h-5" }
                        }
                        div { class: "w-10 h-10 rounded bg-zinc-700 flex items-center justify-center text-muted-foreground",
                            Square { class: "w-5 h-5" }
                        }
                    }
                    div { class: "ml-4 font-mono text-xl text-foreground tabular-nums", "00:01:24.15" }
                    div { class: "ml-auto text-sm text-muted-foreground", "4/4 • 120 BPM" }
                }

                div {
                    class: "space-y-3 flex-1",
                    for i in 0..6 {
                        div {
                            key: "{i}",
                            class: "flex items-center gap-3 p-3 rounded bg-zinc-800/30 border border-border/30",
                            div {
                                class: "w-20 text-xs text-muted-foreground truncate",
                                { match i {
                                    0 => "Drums",
                                    1 => "Bass",
                                    2 => "Keys",
                                    3 => "Guitar",
                                    4 => "Synth",
                                    _ => "Vocals"
                                }}
                            }
                            div {
                                class: "flex-1 h-10 rounded bg-zinc-700/50 overflow-hidden flex items-center gap-px px-1",
                                for j in 0..50 {
                                    div {
                                        key: "{j}",
                                        class: "w-1 bg-violet-500/40 rounded-full",
                                        style: "height: {((j * 7 + i * 13) % 24 + 6)}px;"
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

/// Audio Plugins showcase card
#[component]
fn PluginsShowcaseCard() -> Element {
    rsx! {
        div {
            class: "rounded-lg border border-border bg-card overflow-hidden shadow-2xl w-full",
            style: "height: 600px;",

            div {
                class: "flex items-center gap-2 px-4 py-3 border-b border-border bg-zinc-900/80",
                div { class: "w-3 h-3 rounded-full bg-red-500/80" }
                div { class: "w-3 h-3 rounded-full bg-yellow-500/80" }
                div { class: "w-3 h-3 rounded-full bg-green-500/80" }
                span { class: "ml-4 text-sm text-muted-foreground", "FTS-EQ" }
                span { class: "ml-auto text-xs text-amber-500 font-medium px-2 py-0.5 rounded bg-amber-500/10", "VST3 / CLAP" }
            }

            div {
                class: "relative h-[calc(100%-3rem)] bg-gradient-to-b from-zinc-900 to-zinc-950 flex flex-col items-center justify-center p-6",
                div {
                    class: "relative w-32 h-32 rounded-full border-4 border-zinc-700 bg-zinc-800 flex items-center justify-center",
                    div {
                        class: "absolute w-2 h-8 bg-amber-500 rounded-full",
                        style: "top: 8px; transform-origin: bottom center; transform: rotate(-45deg);"
                    }
                    div {
                        class: "text-center",
                        div { class: "text-2xl font-bold text-foreground", "+3.2" }
                        div { class: "text-xs text-muted-foreground", "dB" }
                    }
                }
                div { class: "mt-4 text-sm font-medium text-muted-foreground uppercase tracking-wider", "Gain" }
                div {
                    class: "absolute right-8 top-1/2 -translate-y-1/2 flex gap-2",
                    for channel in ["L", "R"] {
                        div {
                            key: "{channel}",
                            class: "flex flex-col items-center gap-1",
                            div {
                                class: "w-4 h-48 bg-zinc-800 rounded overflow-hidden flex flex-col-reverse",
                                div {
                                    class: "w-full bg-gradient-to-t from-green-500 via-yellow-500 to-red-500",
                                    style: "height: 65%;"
                                }
                            }
                            span { class: "text-xs text-muted-foreground", "{channel}" }
                        }
                    }
                }
            }
        }
    }
}

// =============================================================================
// Documentation Pages
// =============================================================================

/// Docs home page
#[component]
fn DocsHome() -> Element {
    rsx! {
        div {
            class: "max-w-5xl mx-auto px-6 py-16",
            div {
                class: "text-center mb-16",
                Heading { level: HeadingLevel::H1, class: "text-foreground mb-4", "Documentation" }
                Text { variant: TextVariant::Muted, class: "text-xl", "Everything you need to build with FastTrackStudio" }
            }
            div {
                class: "grid md:grid-cols-2 gap-6",

                DocsSection { to: Route::DocsKeyflow {}, title: "Keyflow", description: "Chart notation language with GPU-accelerated rendering. Write chord charts with intuitive syntax.", icon: rsx! { FileText { class: "w-8 h-8" } }, color: "emerald" }
                DocsSection { to: Route::DocsReaper {}, title: "REAPER Extension", description: "Deep DAW integration for transport control, MIDI routing, and real-time state synchronization.", icon: rsx! { Music { class: "w-8 h-8" } }, color: "violet" }
                DocsSection { to: Route::DocsDesktop {}, title: "Desktop App", description: "Cross-platform application built with Dioxus for setlist management, lyrics, and live performance.", icon: rsx! { fts_ui::lucide_dioxus::Monitor { class: "w-8 h-8" } }, color: "blue" }
                DocsSection { to: Route::DocsPlugins {}, title: "Audio Plugins", description: "CLAP and VST3 plugins built with nih-plug for audio processing and instrument control.", icon: rsx! { fts_ui::lucide_dioxus::SlidersHorizontal { class: "w-8 h-8" } }, color: "amber" }
            }
        }
    }
}

#[component]
fn DocsSection(
    to: Route,
    title: &'static str,
    description: &'static str,
    icon: Element,
    color: &'static str,
) -> Element {
    let (bg_color, border_color, text_color) = match color {
        "emerald" => (
            "bg-emerald-500/10",
            "hover:border-emerald-500/50",
            "text-emerald-500",
        ),
        "violet" => (
            "bg-violet-500/10",
            "hover:border-violet-500/50",
            "text-violet-500",
        ),
        "blue" => (
            "bg-blue-500/10",
            "hover:border-blue-500/50",
            "text-blue-500",
        ),
        "amber" => (
            "bg-amber-500/10",
            "hover:border-amber-500/50",
            "text-amber-500",
        ),
        _ => ("bg-primary/10", "hover:border-primary/50", "text-primary"),
    };

    rsx! {
        Link {
            to: to,
            class: "group block rounded-xl border border-border bg-card p-6 transition-all {border_color} hover:shadow-lg",
            div {
                class: "flex items-start gap-4",
                div { class: "shrink-0 rounded-lg p-3 {bg_color} {text_color}", {icon} }
                div {
                    h3 { class: "text-xl font-semibold text-foreground mb-2 group-hover:text-primary transition-colors", "{title}" }
                    p { class: "text-muted-foreground leading-relaxed", "{description}" }
                    div { class: "mt-4 flex items-center gap-1 text-sm font-medium {text_color}", "Learn more", ChevronRight { class: "w-4 h-4" } }
                }
            }
        }
    }
}

/// Keyflow documentation landing page
#[component]
fn DocsKeyflow() -> Element {
    rsx! {
        div {
            class: "max-w-5xl mx-auto px-6 py-12",
            Link { to: Route::DocsHome {}, class: "inline-flex items-center gap-2 text-sm text-muted-foreground hover:text-foreground mb-8 transition-colors", ArrowLeft { class: "w-4 h-4" }, "Back to Docs" }

            div {
                class: "mb-12",
                div { class: "inline-flex items-center gap-3 mb-4",
                    div { class: "rounded-lg p-3 bg-emerald-500/10 text-emerald-500", FileText { class: "w-8 h-8" } }
                    Heading { level: HeadingLevel::H1, class: "text-foreground", "Keyflow" }
                }
                Text { variant: TextVariant::Muted, class: "text-xl max-w-2xl", "A domain-specific language for writing chord charts with GPU-accelerated rendering." }
            }

            div {
                class: "grid md:grid-cols-2 gap-6 mb-12",
                Link { to: Route::SnippetsBrowser {}, class: "group flex items-center gap-4 p-6 rounded-xl border border-border bg-card hover:border-emerald-500/50 transition-all",
                    div { class: "rounded-lg p-3 bg-emerald-500/10 text-emerald-500", FileCode { class: "w-6 h-6" } }
                    div {
                        h3 { class: "font-semibold text-foreground group-hover:text-emerald-500 transition-colors", "Interactive Snippets" }
                        p { class: "text-sm text-muted-foreground", "Browse and edit example charts" }
                    }
                    ChevronRight { class: "w-5 h-5 text-muted-foreground ml-auto" }
                }
                Link { to: Route::ChartEditor {}, class: "group flex items-center gap-4 p-6 rounded-xl border border-border bg-card hover:border-emerald-500/50 transition-all",
                    div { class: "rounded-lg p-3 bg-emerald-500/10 text-emerald-500", PenLine { class: "w-6 h-6" } }
                    div {
                        h3 { class: "font-semibold text-foreground group-hover:text-emerald-500 transition-colors", "Chart Editor" }
                        p { class: "text-sm text-muted-foreground", "Write and export your own charts" }
                    }
                    ChevronRight { class: "w-5 h-5 text-muted-foreground ml-auto" }
                }
            }

            div {
                class: "prose prose-invert max-w-none",
                Heading { level: HeadingLevel::H3, class: "text-foreground mb-6", "Features" }
                div {
                    class: "grid md:grid-cols-3 gap-6",
                    KeyflowFeatureCard { title: "Intuitive Syntax", description: "Write chord charts as naturally as you'd read them. No complex markup required." }
                    KeyflowFeatureCard { title: "Smart Memory", description: "Automatically remembers chord voicings within sections for consistent playback." }
                    KeyflowFeatureCard { title: "Section Structure", description: "Verse, Chorus, Bridge, and more - with automatic numbering and theming." }
                    KeyflowFeatureCard { title: "Complex Rhythms", description: "Triplet pushes, syncopation, and explicit durations for any rhythmic pattern." }
                    KeyflowFeatureCard { title: "GPU Rendering", description: "WebGPU-accelerated rendering for crisp, publication-quality output." }
                    KeyflowFeatureCard { title: "PDF Export", description: "Export charts as print-ready PDFs with customizable settings." }
                }
            }
        }
    }
}

#[component]
fn KeyflowFeatureCard(title: &'static str, description: &'static str) -> Element {
    rsx! {
        Card { class: "border-border/50 bg-card/30",
            CardContent { class: "p-4",
                CardTitle { class: "text-sm mb-1", "{title}" }
                CardDescription { "{description}" }
            }
        }
    }
}

/// REAPER Extension documentation page
#[component]
fn DocsReaper() -> Element {
    rsx! {
        div {
            class: "max-w-5xl mx-auto px-6 py-12",
            Link { to: Route::DocsHome {}, class: "inline-flex items-center gap-2 text-sm text-muted-foreground hover:text-foreground mb-8 transition-colors", ArrowLeft { class: "w-4 h-4" }, "Back to Docs" }
            div { class: "mb-12",
                div { class: "inline-flex items-center gap-3 mb-4",
                    div { class: "rounded-lg p-3 bg-violet-500/10 text-violet-500", Music { class: "w-8 h-8" } }
                    Heading { level: HeadingLevel::H1, class: "text-foreground", "REAPER Extension" }
                }
                Text { variant: TextVariant::Muted, class: "text-xl max-w-2xl", "Deep DAW integration for transport control, MIDI routing, and real-time state synchronization." }
            }
            div { class: "space-y-6",
                ReaperFeatureSection { title: "Transport Sync", description: "Real-time synchronization with REAPER's transport for play/pause/stop controls, timeline position, and tempo changes." }
                ReaperFeatureSection { title: "MIDI Routing", description: "Flexible MIDI routing between tracks, virtual instruments, and external hardware." }
                ReaperFeatureSection { title: "Key Input", description: "Intercept and redirect keyboard input for custom shortcuts and live performance controls." }
                ReaperFeatureSection { title: "IPC Communication", description: "Bidirectional communication with the desktop app via iroh for seamless integration." }
            }
            Card { class: "mt-12 p-6 border-violet-500/30 bg-violet-500/5",
                CardTitle { class: "mb-2", "Documentation Coming Soon" }
                CardDescription { "Detailed installation guides, API reference, and usage examples are being written." }
            }
        }
    }
}

#[component]
fn ReaperFeatureSection(title: &'static str, description: &'static str) -> Element {
    rsx! {
        Card { class: "p-6",
            CardTitle { class: "mb-2", "{title}" }
            CardDescription { "{description}" }
        }
    }
}

/// Desktop App documentation page
#[component]
fn DocsDesktop() -> Element {
    rsx! {
        div {
            class: "max-w-5xl mx-auto px-6 py-12",
            Link { to: Route::DocsHome {}, class: "inline-flex items-center gap-2 text-sm text-muted-foreground hover:text-foreground mb-8 transition-colors", ArrowLeft { class: "w-4 h-4" }, "Back to Docs" }
            div { class: "mb-12",
                div { class: "inline-flex items-center gap-3 mb-4",
                    div { class: "rounded-lg p-3 bg-blue-500/10 text-blue-500", fts_ui::lucide_dioxus::Monitor { class: "w-8 h-8" } }
                    Heading { level: HeadingLevel::H1, class: "text-foreground", "Desktop App" }
                }
                Text { variant: TextVariant::Muted, class: "text-xl max-w-2xl", "Cross-platform application for setlist management, lyrics display, and live performance." }
            }
            div { class: "grid md:grid-cols-2 gap-6",
                DesktopFeatureCard { title: "Setlist Management", description: "Organize songs into setlists with drag-and-drop ordering and quick access." }
                DesktopFeatureCard { title: "Lyrics Display", description: "Full-screen lyrics view with auto-scroll synced to transport position." }
                DesktopFeatureCard { title: "Chart Viewer", description: "View and edit Keyflow charts with real-time preview and PDF export." }
                DesktopFeatureCard { title: "P2P Sync", description: "Share setlists and charts with bandmates in real-time over peer-to-peer connections." }
            }
            Card { class: "mt-12 p-6 border-blue-500/30 bg-blue-500/5",
                CardTitle { class: "mb-2", "Documentation Coming Soon" }
                CardDescription { "Installation guides, feature walkthroughs, and configuration options are being written." }
            }
        }
    }
}

#[component]
fn DesktopFeatureCard(title: &'static str, description: &'static str) -> Element {
    rsx! {
        Card { class: "p-6",
            CardTitle { class: "mb-2", "{title}" }
            CardDescription { "{description}" }
        }
    }
}

/// Audio Plugins documentation page
#[component]
fn DocsPlugins() -> Element {
    rsx! {
        div {
            class: "max-w-5xl mx-auto px-6 py-12",
            Link { to: Route::DocsHome {}, class: "inline-flex items-center gap-2 text-sm text-muted-foreground hover:text-foreground mb-8 transition-colors", ArrowLeft { class: "w-4 h-4" }, "Back to Docs" }
            div { class: "mb-12",
                div { class: "inline-flex items-center gap-3 mb-4",
                    div { class: "rounded-lg p-3 bg-amber-500/10 text-amber-500", fts_ui::lucide_dioxus::SlidersHorizontal { class: "w-8 h-8" } }
                    Heading { level: HeadingLevel::H1, class: "text-foreground", "Audio Plugins" }
                }
                Text { variant: TextVariant::Muted, class: "text-xl max-w-2xl", "CLAP and VST3 plugins built with nih-plug for audio processing and instrument control." }
            }
            div { class: "space-y-6",
                PluginCard { name: "fts-guide", description: "Click track, count-in, and section guide for live performance navigation." }
                PluginCard { name: "fts-macros", description: "Macro parameter surface with 8 automatable parameters for REAPER integration." }
            }
            Card { class: "mt-12 p-6 border-amber-500/30 bg-amber-500/5",
                CardTitle { class: "mb-2", "Built with nih-plug" }
                CardDescription { class: "mb-4", "All plugins are built using nih-plug, a Rust framework for creating CLAP and VST3 plugins with minimal boilerplate." }
                a {
                    href: "https://github.com/robbert-vdh/nih-plug",
                    target: "_blank",
                    class: "inline-flex items-center gap-2 text-amber-500 hover:text-amber-400 transition-colors",
                    "Learn more about nih-plug"
                    ExternalLink { class: "w-4 h-4" }
                }
            }
        }
    }
}

#[component]
fn PluginCard(name: &'static str, description: &'static str) -> Element {
    rsx! {
        Card { class: "p-6",
            div { class: "flex items-center gap-3 mb-2",
                Badge { class: "border-amber-500/30 bg-amber-500/10 text-amber-500 font-mono", "{name}" }
            }
            CardDescription { "{description}" }
        }
    }
}

#[component]
fn FeatureCard(title: &'static str, description: &'static str, icon: Element) -> Element {
    rsx! {
        Card {
            class: "p-6 hover:border-primary/50 transition-colors",
            div { class: "mb-4", {icon} }
            Heading { level: HeadingLevel::H5, class: "text-card-foreground mb-2", "{title}" }
            CardDescription { "{description}" }
        }
    }
}

// =============================================================================
// Snippets Browser
// =============================================================================

/// Unified snippets browser
#[component]
fn SnippetsBrowser() -> Element {
    let patterns = keyflow::patterns::all_patterns();
    let first_id = patterns.first().map(|p| p.id).unwrap_or("minimal-chart");
    rsx! { UnifiedSnippetsView { selected_id: first_id.to_string() } }
}

#[component]
fn SnippetsView(id: String) -> Element {
    rsx! { UnifiedSnippetsView { selected_id: id } }
}

#[component]
fn UnifiedSnippetsView(selected_id: String) -> Element {
    use components::PreviewMode;
    use keyflow::patterns::{PatternCategory, all_patterns, find_pattern, patterns_by_category};

    let patterns = all_patterns();
    let pattern = find_pattern(&selected_id);

    let mut expanded_categories = use_signal(|| {
        PatternCategory::all()
            .iter()
            .map(|c| (*c, true))
            .collect::<std::collections::HashMap<_, _>>()
    });

    let current_index = patterns
        .iter()
        .position(|p| p.id == selected_id)
        .unwrap_or(0);
    let prev_pattern = if current_index > 0 {
        patterns.get(current_index - 1)
    } else {
        None
    };
    let next_pattern = patterns.get(current_index + 1);

    let mut last_pattern_id = use_signal(|| selected_id.clone());
    let mut source = use_signal(|| pattern.map(|p| p.source.to_string()).unwrap_or_default());
    let mut preview_mode = use_signal(|| {
        pattern
            .map(|p| {
                if p.category == PatternCategory::Examples {
                    PreviewMode::Page
                } else {
                    PreviewMode::Snippet
                }
            })
            .unwrap_or(PreviewMode::Snippet)
    });

    if *last_pattern_id.peek() != selected_id {
        if let Some(p) = pattern {
            source.set(p.source.to_string());
            let new_mode = if p.category == PatternCategory::Examples {
                PreviewMode::Page
            } else {
                PreviewMode::Snippet
            };
            preview_mode.set(new_mode);
        }
        last_pattern_id.set(selected_id.clone());
    }

    let mut show_source = use_signal(|| true);

    rsx! {
        div {
            class: "flex h-[calc(100vh-4rem)]",

            // Sidebar
            aside {
                class: "w-72 bg-sidebar border-r border-sidebar-border flex flex-col",
                div { class: "p-4 border-b border-sidebar-border",
                    h2 { class: "text-lg font-semibold text-sidebar-foreground", "Snippets" }
                    p { class: "text-xs text-muted-foreground mt-1", "{patterns.len()} interactive examples" }
                }
                nav {
                    class: "flex-1 overflow-y-auto p-2",
                    for category in PatternCategory::all() {
                        {
                            let category = *category;
                            let cat_patterns = patterns_by_category(category);
                            let is_expanded = expanded_categories.read().get(&category).copied().unwrap_or(true);
                            let has_selected = cat_patterns.iter().any(|p| p.id == selected_id);
                            rsx! {
                                div { class: "mb-2",
                                    button {
                                        class: "w-full flex items-center justify-between px-3 py-2 rounded-md text-sm font-medium text-sidebar-foreground hover:bg-sidebar-accent transition-colors",
                                        onclick: move |_| {
                                            let mut cats = expanded_categories.write();
                                            let current = cats.get(&category).copied().unwrap_or(true);
                                            cats.insert(category, !current);
                                        },
                                        span { class: if has_selected { "text-primary" } else { "" }, "{category.label()}" }
                                        span { class: "text-xs text-muted-foreground",
                                            if is_expanded {
                                                ChevronDown { class: "w-4 h-4" }
                                            } else {
                                                ChevronRight { class: "w-4 h-4" }
                                            }
                                        }
                                    }
                                    if is_expanded {
                                        div { class: "ml-2 mt-1 space-y-0.5",
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
                div { class: "p-4 border-t border-sidebar-border",
                    Link { to: Route::ChartEditor {}, class: "flex items-center gap-2 text-sm text-muted-foreground hover:text-foreground transition-colors",
                        PenLine { class: "w-4 h-4" }
                        "Open Full Editor"
                    }
                }
            }

            // Main content
            div {
                class: "flex-1 flex flex-col min-w-0",
                match pattern {
                    Some(pattern) => rsx! {
                        header {
                            class: "px-6 py-4 border-b border-border flex items-center justify-between shrink-0 bg-card/50",
                            div { class: "min-w-0",
                                div { class: "flex items-center gap-3",
                                    span { class: "text-xs font-medium text-primary bg-primary/10 px-2 py-0.5 rounded", "{pattern.category.label()}" }
                                    h1 { class: "text-xl font-semibold text-foreground truncate", "{pattern.title}" }
                                }
                                p { class: "text-sm text-muted-foreground mt-1 line-clamp-1", "{pattern.description}" }
                            }
                            div { class: "flex items-center gap-2 shrink-0 ml-4",
                                div { class: "flex items-center gap-1",
                                    if let Some(prev) = prev_pattern {
                                        Link { to: Route::SnippetsView { id: prev.id.to_string() }, class: "p-2 rounded-md hover:bg-accent text-muted-foreground hover:text-foreground transition-colors", title: "Previous: {prev.title}", ChevronLeft { class: "w-4 h-4" } }
                                    } else {
                                        span { class: "p-2 text-muted-foreground/30", ChevronLeft { class: "w-4 h-4" } }
                                    }
                                    span { class: "text-xs text-muted-foreground px-2", "{current_index + 1} / {patterns.len()}" }
                                    if let Some(next) = next_pattern {
                                        Link { to: Route::SnippetsView { id: next.id.to_string() }, class: "p-2 rounded-md hover:bg-accent text-muted-foreground hover:text-foreground transition-colors", title: "Next: {next.title}", ChevronRight { class: "w-4 h-4" } }
                                    } else {
                                        span { class: "p-2 text-muted-foreground/30", ChevronRight { class: "w-4 h-4" } }
                                    }
                                }
                                div { class: "w-px h-6 bg-border mx-2" }
                                button {
                                    class: if *show_source.read() {
                                        "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-xs font-medium bg-primary/10 text-primary"
                                    } else {
                                        "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-xs font-medium text-muted-foreground hover:bg-accent transition-colors"
                                    },
                                    onclick: move |_| { let current = *show_source.peek(); show_source.set(!current); },
                                    Code { class: "w-3.5 h-3.5" }
                                    "Source"
                                }
                            }
                        }
                        div {
                            class: "flex-1 flex overflow-hidden",
                            div { class: "flex-1 overflow-hidden bg-muted/30",
                                components::DynamicChartRenderer { source: source, mode: preview_mode, canvas_id: Some(format!("snippet-canvas-{}", pattern.id)) }
                            }
                            if *show_source.read() {
                                div { class: "w-96 border-l border-border flex flex-col bg-card shrink-0",
                                    div { class: "px-4 py-3 border-b border-border flex items-center justify-between shrink-0",
                                        div { class: "flex items-center gap-2",
                                            FileCode { class: "w-4 h-4 text-muted-foreground" }
                                            span { class: "text-sm font-medium text-foreground", "Source" }
                                        }
                                        if source.read().as_str() != pattern.source {
                                            button { class: "text-xs text-muted-foreground hover:text-foreground px-2 py-1 rounded hover:bg-accent transition-colors", onclick: move |_| source.set(pattern.source.to_string()), "Reset" }
                                        }
                                    }
                                    div { class: "flex-1 overflow-hidden",
                                        components::HighlightedEditor { value: source(), on_change: move |v: String| source.set(v), placeholder: "Enter keyflow notation...", textarea_id: Some(format!("snippet-editor-{}", pattern.id)) }
                                    }
                                }
                            }
                        }
                    },
                    None => {
                        rsx! {
                            div { class: "flex-1 flex flex-col items-center justify-center text-center p-8",
                                fts_ui::lucide_dioxus::CircleAlert { class: "w-16 h-16 text-muted-foreground/50 mb-4" }
                                h2 { class: "text-xl font-semibold text-foreground mb-2", "Pattern Not Found" }
                                p { class: "text-muted-foreground mb-6", "The pattern \"{selected_id}\" doesn't exist." }
                                Link { to: Route::SnippetsBrowser {}, class: "inline-flex items-center gap-2 px-4 py-2 rounded-md bg-primary text-primary-foreground hover:bg-primary/90 transition-colors", "Browse All Snippets" }
                            }
                        }
                    }
                }
            }
        }
    }
}

// =============================================================================
// Legacy Routes
// =============================================================================

#[component]
fn PatternBrowser() -> Element {
    let nav = use_navigator();
    use_effect(move || {
        nav.push(Route::SnippetsBrowser {});
    });
    rsx! { div { class: "flex items-center justify-center h-64 text-muted-foreground", "Redirecting..." } }
}

#[component]
fn ChartEditor() -> Element {
    rsx! { components::ChartEditor {} }
}

#[component]
fn PatternView(id: String) -> Element {
    let nav = use_navigator();
    let id_clone = id.clone();
    use_effect(move || {
        nav.push(Route::SnippetsView {
            id: id_clone.clone(),
        });
    });
    rsx! { div { class: "flex items-center justify-center h-64 text-muted-foreground", "Redirecting..." } }
}

// =============================================================================
// Test Pages
// =============================================================================

/// Test page for debugging chart rendering
#[component]
fn TestRender() -> Element {
    let mut source = use_signal(|| DEMO_CHARTS[0].to_string());
    let preview_mode = use_signal(|| components::PreviewMode::Page);

    rsx! {
        div {
            class: "flex h-[calc(100vh-8rem)]",
            div {
                class: "w-96 border-r border-border overflow-hidden",
                div { class: "px-4 py-3 border-b border-border bg-card",
                    h3 { class: "text-sm font-semibold", "Source Code" }
                }
                div { class: "h-[calc(100%-3rem)]",
                    components::HighlightedEditor { value: source(), on_change: move |new_value: String| source.set(new_value), placeholder: "Enter keyflow chart notation...", textarea_id: Some("test-render-editor".to_string()) }
                }
            }
            div {
                class: "flex-1 bg-zinc-800 overflow-auto p-8",
                div { class: "inline-block", style: "border: 1px solid red;",
                    components::StaticChartRenderer { source: source, mode: preview_mode, canvas_id: Some("test-render-canvas".to_string()) }
                }
            }
        }
    }
}

/// Combined test page for EQ and Compressor graph components
#[component]
fn TestFxUi() -> Element {
    // === EQ State ===
    let mut bands = use_signal(|| {
        vec![
            EqBand {
                index: 0,
                used: true,
                enabled: true,
                frequency: 80.0,
                gain: 3.0,
                q: 0.7,
                shape: EqBandShape::LowShelf,
                ..Default::default()
            },
            EqBand {
                index: 1,
                used: true,
                enabled: true,
                frequency: 250.0,
                gain: -2.5,
                q: 1.5,
                shape: EqBandShape::Bell,
                ..Default::default()
            },
            EqBand {
                index: 2,
                used: true,
                enabled: true,
                frequency: 1000.0,
                gain: 1.5,
                q: 2.0,
                shape: EqBandShape::Bell,
                ..Default::default()
            },
            EqBand {
                index: 3,
                used: true,
                enabled: true,
                frequency: 4000.0,
                gain: 2.0,
                q: 1.0,
                shape: EqBandShape::Bell,
                ..Default::default()
            },
            EqBand {
                index: 4,
                used: true,
                enabled: true,
                frequency: 12000.0,
                gain: 4.0,
                q: 0.8,
                shape: EqBandShape::HighShelf,
                ..Default::default()
            },
        ]
    });
    let mut selected_band = use_signal(|| 0_usize);
    let mut eq_db_range = use_signal(|| 24.0_f64);

    // === Compressor State ===
    let mut params = use_signal(CompressorParams::default);
    let mut metering = use_signal(CompressorMetering::default);
    let mut sim_time = use_signal(|| 0.0_f64);
    let mut comp_db_range = use_signal(|| DbRange::Range48);

    // Animation effect for simulated compressor levels
    use_effect(move || {
        #[cfg(target_arch = "wasm32")]
        {
            use wasm_bindgen::prelude::*;

            let closure = Closure::wrap(Box::new(move || {
                let t = *sim_time.peek();
                sim_time.set(t + 0.05);

                let input = -24.0 + 18.0 * (t * 0.7).sin() as f32 + 6.0 * (t * 2.3).sin() as f32;
                let input_clamped = input.clamp(-60.0, 0.0);

                let p = params.peek();
                let threshold = p.threshold;
                let ratio = p.ratio;
                let knee_w = p.knee / 2.0;

                let output = if input_clamped <= threshold - knee_w {
                    input_clamped
                } else if input_clamped < threshold + knee_w {
                    let a0 = (1.0 / ratio - 1.0) / (4.0 * knee_w);
                    let x_offset = input_clamped - (threshold - knee_w);
                    input_clamped + a0 * x_offset * x_offset
                } else {
                    threshold + (input_clamped - threshold) / ratio
                };

                let gr = output - input_clamped;

                let prev = metering.peek();
                let input_peak = input_clamped.max(prev.input_peak * 0.99);
                let output_peak = output.max(prev.output_peak * 0.99);
                let gr_peak = gr.min(prev.gr_peak * 0.99);
                let mut gr_history = prev.gr_history.clone();
                let mut input_history = prev.input_history.clone();
                drop(prev);

                gr_history.push(gr);
                input_history.push(input_clamped);

                const MAX_HISTORY: usize = 128;
                if gr_history.len() > MAX_HISTORY {
                    gr_history.drain(0..gr_history.len() - MAX_HISTORY);
                }
                if input_history.len() > MAX_HISTORY {
                    input_history.drain(0..input_history.len() - MAX_HISTORY);
                }

                metering.set(CompressorMetering {
                    input_level: input_clamped,
                    output_level: output,
                    gain_reduction: gr,
                    input_peak,
                    output_peak,
                    gr_peak,
                    gr_history,
                    input_history,
                });
            }) as Box<dyn FnMut()>);

            let window = web_sys::window().unwrap();
            let _ = window.set_interval_with_callback_and_timeout_and_arguments_0(
                closure.as_ref().unchecked_ref(),
                50,
            );
            closure.forget();
        }
    });

    rsx! {
        div {
            class: "min-h-screen bg-background p-6",
            div { class: "max-w-7xl mx-auto mb-6",
                h1 { class: "text-2xl font-bold text-foreground mb-2", "Audio FX Controls" }
                p { class: "text-muted-foreground text-sm", "Interactive EQ and Compressor widgets for audio production interfaces." }
            }
            div {
                class: "max-w-7xl mx-auto grid grid-cols-1 xl:grid-cols-2 gap-6",

                // EQ Section
                div { class: "bg-card rounded-xl border border-border p-4",
                    div { class: "flex items-center justify-between mb-4",
                        h2 { class: "text-lg font-semibold text-foreground", "Parametric EQ" }
                        div { class: "flex items-center gap-2",
                            label { class: "text-xs text-muted-foreground", "Range:" }
                            select {
                                class: "px-2 py-1 rounded bg-muted border border-border text-foreground text-xs",
                                value: "{*eq_db_range.read()}",
                                onchange: move |evt: Event<FormData>| { if let Ok(v) = evt.value().parse::<f64>() { eq_db_range.set(v); } },
                                option { value: "6", "±6 dB" }
                                option { value: "12", "±12 dB" }
                                option { value: "18", "±18 dB" }
                                option { value: "24", "±24 dB" }
                                option { value: "30", "±30 dB" }
                            }
                        }
                    }
                    div { class: "w-full mb-4", style: "aspect-ratio: 800 / 350;",
                        EqGraph {
                            bands: bands,
                            db_range: *eq_db_range.read(),
                            show_grid: true,
                            fill_curve: true,
                            on_band_change: move |(idx, band): (usize, EqBand)| { let mut b = bands.write(); if idx < b.len() { b[idx] = band; } },
                            on_band_add: move |band: EqBand| { bands.write().push(band); },
                            on_band_remove: move |idx: usize| { let mut b = bands.write(); if idx < b.len() { b.remove(idx); let sel_idx = *selected_band.peek(); if sel_idx >= b.len() && !b.is_empty() { selected_band.set(b.len() - 1); } } },
                            on_begin: move |idx: usize| { selected_band.set(idx); },
                        }
                    }
                    div { class: "flex flex-wrap gap-2 text-xs",
                        for (i, band) in bands.read().iter().enumerate() {
                            div {
                                class: if i == *selected_band.read() { "px-2 py-1 rounded bg-primary/20 border border-primary/40 text-foreground" } else { "px-2 py-1 rounded bg-muted border border-border text-muted-foreground" },
                                onclick: move |_| selected_band.set(i),
                                "{format_frequency(band.frequency)} • {band.gain:+.1}dB"
                            }
                        }
                    }
                }

                // Compressor Section
                div { class: "bg-card rounded-xl border border-border p-4",
                    div { class: "flex items-center justify-between mb-4",
                        h2 { class: "text-lg font-semibold text-foreground", "Compressor" }
                        span { class: "text-xs text-muted-foreground", "Pro-C Style" }
                    }
                    div { class: "flex justify-center",
                        CompressorWidget {
                            params: params,
                            metering: metering.read().clone(),
                            db_range: *comp_db_range.read(),
                            graph_size: 200,
                            show_grid: true,
                            show_gr_meter: true,
                            show_levels: true,
                            show_gr_trace: true,
                            show_controls: true,
                            interactive: true,
                        }
                    }
                }
            }

            // Gate Section
            GateSection {}

            // Instructions
            div {
                class: "max-w-7xl mx-auto mt-6 p-4 bg-muted/50 rounded-lg border border-border",
                h3 { class: "text-sm font-semibold text-foreground mb-2", "Interaction Guide" }
                div { class: "grid grid-cols-1 md:grid-cols-3 gap-4 text-xs text-muted-foreground",
                    div {
                        h4 { class: "font-medium text-foreground mb-1", "EQ" }
                        ul { class: "space-y-0.5 list-disc list-inside",
                            li { "Drag bands to adjust frequency/gain" }
                            li { "Mouse wheel to adjust Q" }
                            li { "Double-click to add new band" }
                            li { "Drag outside to remove band" }
                            li { "Shift+drag for fine control" }
                        }
                    }
                    div {
                        h4 { class: "font-medium text-foreground mb-1", "Compressor" }
                        ul { class: "space-y-0.5 list-disc list-inside",
                            li { "Drag threshold point to adjust threshold" }
                            li { "Drag above threshold to adjust ratio" }
                            li { "Mouse wheel to adjust knee width" }
                            li { "Shift+drag for fine control" }
                        }
                    }
                    div {
                        h4 { class: "font-medium text-foreground mb-1", "Gate" }
                        ul { class: "space-y-0.5 list-disc list-inside",
                            li { "Use knobs to adjust parameters" }
                            li { "Large knob controls threshold" }
                            li { "Small knobs for ratio/range" }
                            li { "Right panel for envelope controls" }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn GateSection() -> Element {
    let gate_params = use_signal(GateParams::default);
    rsx! {
        div { class: "max-w-7xl mx-auto mt-6 bg-card rounded-xl border border-border p-4",
            div { class: "flex items-center justify-between mb-4",
                h2 { class: "text-lg font-semibold text-foreground", "Noise Gate" }
                span { class: "text-xs text-muted-foreground", "Pro-G Style" }
            }
            div { class: "flex justify-center",
                GateGraph {
                    params: gate_params,
                    metering: GateMetering::default(),
                    db_range: GateDbRange::Range60,
                    graph_size: 200,
                    show_grid: true,
                    show_gr_meter: true,
                    show_levels: false,
                    show_gr_trace: false,
                    show_controls: true,
                    interactive: true,
                }
            }
        }
    }
}

fn format_frequency(freq: f32) -> String {
    if freq >= 1000.0 {
        format!("{:.1}k", freq / 1000.0)
    } else {
        format!("{:.0}", freq)
    }
}
