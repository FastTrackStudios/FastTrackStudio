//! Root application component for Signal Mobile.
//!
//! Layout:
//!   ┌─────────────────────────────────────────┐
//!   │  Header (logo + live/stop + settings)    │
//!   ├─────────────────────────────────────────┤
//!   │  SceneBar (horizontal scene selector)    │
//!   ├─────────────────────────────────────────┤
//!   │  MixerView (channel strips + detail)     │
//!   └─────────────────────────────────────────┘

use dioxus::prelude::*;

use signal_ui::ProcessingChain;

use crate::channel::{ChannelKind, ChannelState};
use crate::mixer::MixerView;
use crate::piano_view::PianoView;
use crate::scene_bar::{SceneBar, SceneEntry};
use crate::styles as s;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Tab {
    Mixer,
    Piano,
}

const GLOBAL_CSS: &str = r#"
*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
html, body {
    width: 100%; height: 100%;
    overflow: hidden;
    background: #0a0a0b;
    color: #e4e4e7;
    -webkit-text-size-adjust: 100%;
    -webkit-font-smoothing: antialiased;
    font-family: -apple-system, BlinkMacSystemFont, 'SF Pro Display', sans-serif;
}
button { -webkit-tap-highlight-color: transparent; }
::-webkit-scrollbar { display: none; }
"#;

#[component]
pub fn App() -> Element {
    let chain = use_context::<ProcessingChain>();

    let mut tab = use_signal(|| Tab::Mixer);
    let mut active_scene = use_signal(|| 0usize);
    let scenes = use_signal(|| {
        vec![
            SceneEntry {
                id: 0,
                name: "Verse".into(),
            },
            SceneEntry {
                id: 1,
                name: "Chorus".into(),
            },
            SceneEntry {
                id: 2,
                name: "Bridge".into(),
            },
            SceneEntry {
                id: 3,
                name: "Outro".into(),
            },
        ]
    });

    let channels = use_signal(|| {
        vec![
            ChannelState::new(ChannelKind::Drums),
            ChannelState::new(ChannelKind::Vocals),
            ChannelState::new(ChannelKind::Guitar),
            ChannelState::new(ChannelKind::Bass),
            ChannelState::new(ChannelKind::Keys),
        ]
    });

    let active_channel: Signal<Option<usize>> = use_signal(|| None);
    let active_scene_id = *active_scene.read();

    rsx! {
        document::Style { {GLOBAL_CSS} }

        div {
            style: format!(
                "width:100vw; height:100dvh; display:flex; flex-direction:column; \
                 background:{bg}; color:{text}; overflow:hidden; \
                 padding-top:env(safe-area-inset-top); \
                 padding-bottom:env(safe-area-inset-bottom);",
                bg = s::BG_APP, text = s::TEXT,
            ),

            Header {}

            // Tab bar (Mixer / Piano)
            TabBar { active_tab: *tab.read(), on_select: move |t| { *tab.write() = t; } }

            // Scene bar — only relevant in Mixer tab
            if *tab.read() == Tab::Mixer {
                SceneBar {
                    scenes: scenes.read().clone(),
                    active_id: active_scene_id,
                    on_select: move |id| { *active_scene.write() = id; },
                }
            }

            // Main content
            match *tab.read() {
                Tab::Mixer => rsx! {
                    MixerView {
                        chain: chain.clone(),
                        channels: channels,
                        active_channel: active_channel,
                    }
                },
                Tab::Piano => rsx! {
                    PianoView { chain: chain.clone() }
                },
            }
        }
    }
}

// ── Tab bar ───────────────────────────────────────────────────────────────────

#[component]
fn TabBar(active_tab: Tab, on_select: EventHandler<Tab>) -> Element {
    rsx! {
        div {
            style: format!(
                "display:flex; flex-direction:row; background:{bg}; \
                 border-bottom:1px solid {border}; flex-shrink:0;",
                bg = s::BG_APP, border = s::BORDER,
            ),
            for (tab, label) in [(Tab::Mixer, "Mixer"), (Tab::Piano, "Piano")] {
                button {
                    onclick: move |_| on_select.call(tab),
                    style: format!(
                        "flex:1; height:{h}; border:none; background:transparent; \
                         font-size:13px; font-weight:600; cursor:pointer; \
                         color:{col}; border-bottom:2px solid {accent};",
                        h = s::TOUCH_TARGET,
                        col = if active_tab == tab { s::TEXT_BRIGHT } else { s::TEXT_DIM },
                        accent = if active_tab == tab { s::ACCENT } else { "transparent" },
                    ),
                    "{label}"
                }
            }
        }
    }
}

// ── Header ────────────────────────────────────────────────────────────────────

#[component]
fn Header() -> Element {
    let mut running = use_signal(|| false);

    rsx! {
        div {
            style: format!(
                "display:flex; align-items:center; justify-content:space-between; \
                 padding:0 16px; height:52px; flex-shrink:0; \
                 background:{bg}; border-bottom:1px solid {border};",
                bg = s::BG_APP, border = s::BORDER,
            ),

            // Wordmark
            span {
                style: format!(
                    "font-size:17px; font-weight:800; letter-spacing:0.1em; color:{c};",
                    c = s::TEXT_BRIGHT,
                ),
                "SIGNAL"
            }

            div {
                style: "display:flex; align-items:center; gap:10px;",

                // Live indicator dot
                div {
                    style: format!(
                        "width:8px; height:8px; border-radius:50%; background:{col};",
                        col = if *running.read() { s::SUCCESS } else { s::BORDER },
                    ),
                }

                // Live / Stop button
                button {
                    onclick: move |_| { let v = *running.read(); *running.write() = !v; },
                    style: format!(
                        "height:{h}; padding:0 18px; border-radius:{r}; border:none; \
                         font-size:13px; font-weight:700; cursor:pointer; \
                         background:{bg}; color:{col};",
                        h = s::TOUCH_TARGET, r = s::RADIUS_SM,
                        bg = if *running.read() { s::DANGER } else { s::ACCENT },
                        col = s::TEXT_BRIGHT,
                    ),
                    { if *running.read() { "■ STOP" } else { "▶ LIVE" } }
                }
            }
        }
    }
}
