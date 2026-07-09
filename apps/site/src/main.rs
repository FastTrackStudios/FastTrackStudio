//! FastTrackStudio Documentation Site
//!
//! Interactive documentation with live chart rendering for the keyflow parser.
//! Test patterns are displayed as interactive examples using WebGPU.

mod components;
mod renderer;

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{FileCode, Github, Music};
use fts_ui::prelude::*;

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
    #[route("/editor")]
    KeyflowEditorPage {},
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

        let filter = tracing_subscriber::EnvFilter::new(
            "warn,fts_site=debug,keyflow=info,keyflow::engraver=debug",
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
                    EnvFilter::new("warn,fts_site=debug,keyflow=info")
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

        // Site-wide SEO / social metadata. Per-page <title> is set via
        // `document::Title` in each route component.
        document::Meta { name: "description", content: SITE_DESCRIPTION }
        document::Meta { name: "theme-color", content: "#0d0a14" }
        document::Meta { property: "og:type", content: "website" }
        document::Meta { property: "og:site_name", content: "FastTrackStudio" }
        document::Meta { property: "og:title", content: "FastTrackStudio" }
        document::Meta { property: "og:description", content: SITE_DESCRIPTION }
        document::Meta { name: "twitter:card", content: "summary_large_image" }
        document::Meta { name: "twitter:title", content: "FastTrackStudio" }
        document::Meta { name: "twitter:description", content: SITE_DESCRIPTION }

        ThemeProvider {
            state: theme_state,
            Router::<Route> {}
        }
    }
}

/// Shared description used across SEO/social meta tags.
const SITE_DESCRIPTION: &str = "Open-source, cross-platform, cross-DAW music production tools — built around Reaper, \
     with an open chart format (Keyflow) and documented protocols.";

/// Main layout with navigation
#[component]
fn Layout() -> Element {
    let route = use_route::<Route>();
    let is_home = matches!(route, Route::Home {});
    // The editor is a full-screen app: fix the shell to the viewport height
    // (instead of growing with content) so the studio's own toolbar/split
    // layout can fill exactly the space below the nav, no page scroll.
    let is_editor = matches!(route, Route::KeyflowEditorPage {});

    rsx! {
        div {
            class: if is_editor {
                "h-screen flex flex-col text-foreground overflow-hidden"
            } else {
                "min-h-screen flex flex-col text-foreground"
            },

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
                        }
                    }

                    // Right side actions
                    div {
                        class: "flex items-center gap-2",

                        // Keyflow Editor tab
                        Link {
                            to: Route::KeyflowEditorPage {},
                            class: if matches!(route, Route::KeyflowEditorPage {}) {
                                "px-3 py-1.5 rounded-lg text-sm font-medium text-foreground bg-accent/60 transition-colors"
                            } else {
                                "px-3 py-1.5 rounded-lg text-sm font-medium text-muted-foreground hover:text-foreground hover:bg-accent/50 transition-colors"
                            },
                            "Keyflow Editor"
                        }

                        // GitHub link
                        a {
                            href: "https://codeberg.org/FastTrackStudios",
                            target: "_blank",
                            rel: "noopener noreferrer",
                            aria_label: "FastTrackStudio on Codeberg",
                            class: "p-2 rounded-lg text-muted-foreground hover:text-foreground hover:bg-accent/50 transition-all",
                            title: "View on Codeberg",
                            Github { class: "w-5 h-5" }
                        }

                    }
                }
            }
            }

            // Main content area
            main {
                class: if is_editor { "flex-1 min-h-0 overflow-hidden" } else { "flex-1" },
                Outlet::<Route> {}
            }
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

/// Page-scoped keyframes used by the Home hero text + pillar card shine.
const HOME_KEYFRAMES: &str = "
@keyframes home-rise { from { opacity: 0; transform: translateY(24px); } to { opacity: 1; transform: translateY(0); } }
@keyframes home-gradient { 0%, 100% { background-position: 0% 50%; } 50% { background-position: 100% 50%; } }
@keyframes pillar-shine { 0% { transform: translateX(-150%) skewX(-20deg); } 100% { transform: translateX(350%) skewX(-20deg); } }
";

/// Landing page for FastTrackStudio
#[component]
fn Home() -> Element {
    rsx! {
        document::Title { "FastTrackStudio — Workflow-driven tools for professionals" }
        document::Style { {HOME_KEYFRAMES} }

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
                                class: "text-5xl md:text-6xl lg:text-7xl font-semibold tracking-tight bg-clip-text text-transparent",
                                style: "background-image: linear-gradient(110deg, oklch(0.72 0.18 264) 0%, oklch(0.78 0.16 320) 35%, oklch(0.72 0.18 264) 70%); background-size: 220% 100%; animation: home-gradient 9s ease-in-out infinite, home-rise 0.9s ease-out backwards;",
                                "Workflow-Driven"
                            }
                            div {
                                class: "mt-2 text-3xl md:text-4xl lg:text-5xl font-light text-muted-foreground tracking-tight",
                                style: "animation: home-rise 0.9s ease-out 0.25s backwards;",
                                "Made for Professionals."
                            }
                        }

                        div {
                            class: "mx-auto mt-12 grid gap-5 text-left md:grid-cols-2",

                            PillarCard {
                                number: "01",
                                title: "Open Source",
                                body: "Licensing practices in the audio software space are heavily anti-consumer. We need highly capable, pleasant tools that respect their users.",
                                icon: rsx! { Github { class: "w-5 h-5" } },
                                accent: "#10b981",
                                delay_ms: 100
                            }

                            PillarCard {
                                number: "02",
                                title: "Cross-Platform",
                                body: "Designed for Linux, macOS, Windows, and embedded use cases. The fracture of available software is degrading quality everywhere.",
                                icon: rsx! { fts_ui::lucide_dioxus::Monitor { class: "w-5 h-5" } },
                                accent: "#38bdf8",
                                delay_ms: 180
                            }

                            PillarCard {
                                number: "03",
                                title: "Cross-DAW",
                                body: "Built around Reaper today, on a core that extends to any DAW exposing the needed APIs. Your workflow shouldn\u{2019}t be locked to one vendor.",
                                icon: rsx! { Music { class: "w-5 h-5" } },
                                accent: "#a78bfa",
                                delay_ms: 260
                            }

                            PillarCard {
                                number: "04",
                                title: "Open Format",
                                body: "Charts, project state, and protocols are documented and free to implement. Interoperability as a foundation, not an afterthought.",
                                icon: rsx! { FileCode { class: "w-5 h-5" } },
                                accent: "#fbbf24",
                                delay_ms: 340
                            }
                        }
                    }
                }
            }

            ProjectTilesGrid {}

        }
    }
}

/// Dedicated page for the live Keyflow editor demo.
#[component]
fn KeyflowEditorPage() -> Element {
    rsx! {
        document::Title { "Keyflow Editor — FastTrackStudio" }
        document::Meta { name: "description", content: "Keyflow Editor — an open, plain-text chart format that compiles into real lead sheets, with real-time engraving and PDF export." }

        components::LiveEditor {}
    }
}

/// Diagonal radial-gradient beams behind the home hero. Decorative only.
#[component]
fn DecorativeBeams() -> Element {
    // (width, height, position-css, transform-css, opacity_main, opacity_mid)
    // position-css is the corner anchor (e.g. "left-0 top-0" or arbitrary).
    let beams: &[(&str, &str, &str, &str, &str, &str)] = &[
        (
            "35rem",
            "80rem",
            "left: 0; top: 0;",
            "translateY(-87.5%) rotate(-45deg)",
            ".04",
            ".01",
        ),
        (
            "14rem",
            "80rem",
            "left: 0; top: 0;",
            "translate(5%, -50%) rotate(-45deg)",
            ".03",
            ".01",
        ),
        (
            "14rem",
            "80rem",
            "left: 0; top: 0;",
            "translateY(-87.5%) rotate(-45deg)",
            ".02",
            ".01",
        ),
        (
            "20rem",
            "60rem",
            "left: 30%; top: 0;",
            "translateY(-60%) rotate(-45deg)",
            ".025",
            ".008",
        ),
        (
            "18rem",
            "50rem",
            "right: 10%; top: 0;",
            "translateY(-40%) rotate(-45deg)",
            ".02",
            ".006",
        ),
        (
            "12rem",
            "40rem",
            "left: 50%; top: 30%;",
            "rotate(-45deg)",
            ".015",
            "0",
        ),
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

/// One of the four hero-pillar cards sitting beneath the tagline.
/// Numbered manifesto-style card with icon, animated top accent, shine on hover,
/// per-card accent color, and a staggered entrance animation.
#[component]
fn PillarCard(
    number: &'static str,
    title: &'static str,
    body: &'static str,
    icon: Element,
    accent: &'static str,
    delay_ms: u32,
) -> Element {
    let soft = hex_rgba(accent, 0.10);
    let border = hex_rgba(accent, 0.4);
    let glow = hex_rgba(accent, 0.18);
    let style = format!(
        "--pc-accent: {accent}; --pc-soft: {soft}; --pc-border: {border}; --pc-glow: {glow}; animation: home-rise 0.8s ease-out {delay_ms}ms backwards;"
    );

    rsx! {
        div {
            class: "group relative overflow-hidden rounded-xl border border-border/40 bg-gradient-to-br from-card/50 to-card/10 p-6 transition-all duration-300 hover:-translate-y-1 hover:[border-color:var(--pc-border)] hover:from-card/70 hover:to-card/20 hover:[box-shadow:0_18px_40px_-12px_var(--pc-glow),0_0_0_1px_var(--pc-border)]",
            style: "{style}",

            // Animated top accent line — fades in on hover, tinted to the card accent
            div {
                class: "absolute inset-x-0 top-0 h-px opacity-0 transition-opacity duration-300 group-hover:opacity-100",
                style: "background: linear-gradient(to right, transparent, var(--pc-accent), transparent);",
            }

            // Shine sweep — only runs on hover
            div {
                class: "pointer-events-none absolute inset-0 overflow-hidden",
                div {
                    class: "absolute -inset-y-4 left-0 w-1/3 opacity-0 group-hover:opacity-100 group-hover:[animation:pillar-shine_1.1s_ease-out_forwards]",
                    style: "background: linear-gradient(90deg, transparent 0%, rgba(255,255,255,0.06) 50%, transparent 100%); transform: translateX(-150%) skewX(-20deg);",
                }
            }

            div {
                class: "relative z-10 mb-5 flex items-center justify-between",

                div {
                    class: "inline-flex h-10 w-10 items-center justify-center rounded-lg transition-colors duration-300",
                    style: "background-color: var(--pc-soft); color: var(--pc-accent);",
                    {icon}
                }

                span {
                    class: "font-mono text-xs tracking-[0.2em] text-muted-foreground/50 transition-colors duration-300 group-hover:[color:var(--pc-accent)]",
                    "{number}"
                }
            }

            h3 {
                class: "relative z-10 mb-2 text-lg font-semibold text-foreground transition-colors duration-300 group-hover:[color:var(--pc-accent)]",
                "{title}"
            }

            p {
                class: "relative z-10 text-sm leading-relaxed text-muted-foreground",
                "{body}"
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
    /// Codeberg repository URL the tile links to.
    repo: &'static str,
}

const PROJECT_THEMES: &[ProjectTheme] = &[
    ProjectTheme {
        num: "01",
        name: "Keyflow",
        tagline: "Charts as code",
        desc: "Plain-text music format that compiles into real lead sheets.",
        glyph: ".kf",
        accent: "#a78bfa",
        bg: "#0d0a14",
        version: "alpha v.0.0.1",
        bg_kind: BgKind::Keyflow,
        repo: "https://codeberg.org/FastTrackStudios/keyflow",
    },
    ProjectTheme {
        num: "02",
        name: "Session",
        tagline: "Performance brain",
        desc: "Setlist \u{00B7} song \u{00B7} section navigation across the network.",
        glyph: "\u{2192}\u{2192}",
        accent: "#86efac",
        bg: "#0a1310",
        version: "alpha v.0.0.1",
        bg_kind: BgKind::Session,
        repo: "https://codeberg.org/FastTrackStudios/session",
    },
    ProjectTheme {
        num: "03",
        name: "Signal",
        tagline: "The audio rig",
        desc: "Plugin chains, profiles, snapshots, live morphing.",
        glyph: "\u{224B}",
        accent: "#60a5fa",
        bg: "#0a1018",
        version: "alpha v.0.0.1",
        bg_kind: BgKind::Signal,
        repo: "https://codeberg.org/FastTrackStudios/signal",
    },
    ProjectTheme {
        num: "04",
        name: "Input",
        tagline: "Wiring closet",
        desc: "MIDI, keys, hardware controllers \u{2014} into the action system.",
        glyph: "I/O",
        accent: "#a1a1aa",
        bg: "#0f0f12",
        version: "alpha v.0.0.1",
        bg_kind: BgKind::Input,
        repo: "https://codeberg.org/FastTrackStudios/input",
    },
    ProjectTheme {
        num: "05",
        name: "DAW",
        tagline: "REAPER layer",
        desc: "Unified API. Transport, tracks, FX, project files.",
        glyph: "\u{23F5}",
        accent: "#52525b",
        bg: "#050507",
        version: "alpha v.0.0.1",
        bg_kind: BgKind::Daw,
        repo: "https://codeberg.org/FastTrackStudios/daw",
    },
    ProjectTheme {
        num: "06",
        name: "Plugins",
        tagline: "DSP suite",
        desc: "In-house CLAP/VST3 plugins with detachable GUI.",
        glyph: "FX",
        accent: "#b54234",
        bg: "#140a08",
        version: "alpha v.0.0.1",
        bg_kind: BgKind::Plugins,
        repo: "https://codeberg.org/FastTrackStudios/FTS-Plugins",
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
        bg = theme.bg,
        accent = theme.accent
    );
    rsx! {
        a {
            href: theme.repo,
            target: "_blank",
            rel: "noopener noreferrer",
            aria_label: "{theme.name} on Codeberg",
            class: "group relative block overflow-hidden rounded-md border p-7 md:p-8 lg:min-h-[20rem] h-full transition-all duration-300 hover:-translate-y-0.5 hover:[border-color:var(--pt-accent)]",
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

            // Top row: version (left), glyph (right)
            div {
                class: "relative z-10 flex items-start justify-between mb-8 lg:mb-10",
                div {
                    class: "font-mono text-[0.6rem] lg:text-[0.65rem] uppercase tracking-[0.25em]",
                    style: "color: {theme.accent}; opacity: 0.75;",
                    "{theme.version}"
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
        &[
            (1.5, "esc"),
            (1.0, "1"),
            (1.0, "2"),
            (1.0, "3"),
            (1.0, "4"),
            (1.0, "5"),
            (1.0, "6"),
            (1.0, "7"),
            (1.0, "8"),
            (1.0, "9"),
            (1.0, "0"),
            (1.5, "\u{232B}"),
        ],
        &[
            (1.5, "\u{21E5}"),
            (1.0, "Q"),
            (1.0, "W"),
            (1.0, "E"),
            (1.0, "R"),
            (1.0, "T"),
            (1.0, "Y"),
            (1.0, "U"),
            (1.0, "I"),
            (1.0, "O"),
            (1.0, "P"),
            (1.5, "\\"),
        ],
        &[
            (1.75, "\u{21EA}"),
            (1.0, "A"),
            (1.0, "S"),
            (1.0, "D"),
            (1.0, "F"),
            (1.0, "G"),
            (1.0, "H"),
            (1.0, "J"),
            (1.0, "K"),
            (1.0, "L"),
            (2.25, "\u{21B5}"),
        ],
        &[
            (1.5, "\u{21E7}"),
            (1.0, "Z"),
            (1.0, "X"),
            (1.0, "C"),
            (1.0, "V"),
            (1.0, "B"),
            (1.0, "N"),
            (1.0, "M"),
            (1.0, ","),
            (1.0, "."),
            (1.0, "/"),
            (1.5, "\u{21E7}"),
        ],
        &[
            (1.25, "ctrl"),
            (1.25, "\u{2325}"),
            (1.5, "\u{2318}"),
            (6.25, ""),
            (1.5, "\u{2318}"),
            (1.25, "\u{2325}"),
        ],
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

#[component]
fn TestRender() -> Element {
    let mut source = use_signal(|| DEMO_CHARTS[0].to_string());
    let preview_mode = use_signal(|| components::PreviewMode::Page);

    rsx! {
        document::Title { "Chart Renderer — FastTrackStudio" }
        div {
            class: "flex h-[calc(100vh-8rem)]",
            div {
                class: "w-96 border-r border-border overflow-hidden",
                div { class: "px-4 py-3 border-b border-border bg-card flex items-center justify-between gap-2",
                    h3 { class: "text-sm font-semibold", "Source Code" }
                    components::ExportButton { source: source }
                }
                div { class: "h-[calc(100%-3rem)]",
                    components::HighlightedEditor { value: source(), on_change: move |new_value: String| source.set(new_value), placeholder: "Enter keyflow chart notation...", textarea_id: Some("test-render-editor".to_string()) }
                }
            }
            div {
                class: "flex-1 bg-zinc-800 overflow-auto p-8",
                div { class: "inline-block",
                    components::StaticChartRenderer { source: source, mode: preview_mode, canvas_id: Some("test-render-canvas".to_string()) }
                }
            }
        }
    }
}
