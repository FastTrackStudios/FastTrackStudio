//! FTS UI test panels — Native vs Desktop renderer demo.
//!
//! Two panel definitions wrapping `fts_ui::showcase::Showcase`, plus the two
//! toggle actions that show/hide them. Designed to live in this repo so any
//! REAPER extension that hosts `daw-reaper-dioxus` can mount them by calling
//! `panel_defs()` + `action_defs()`.
//!
//! Originally lived in `fts-extensions`; promoted here so the demo is a
//! first-class part of the daw API surface.

use daw_module::{ActionDef, DockPosition, PanelComponent, PanelDef, PanelRenderer};
use daw_reaper_dioxus::prelude::*;

const TAILWIND_CSS: &str = include_str!("../assets/tailwind.css");
const FTS_THEME_CSS: &str = include_str!("../assets/fts-theme.css");

const BLITZ_FIXES: &str = r#"
input, textarea, select, button { cursor: auto !important; }
input:disabled, textarea:disabled, button:disabled { cursor: not-allowed !important; }
:root { color-scheme: dark; }
"#;

pub const FTS_UI_NATIVE_PANEL_ID: &str = "FTS_UI_NATIVE";
pub const FTS_UI_DESKTOP_PANEL_ID: &str = "FTS_UI_DESKTOP";
pub const FTS_UI_NATIVE_ACTION_ID: &str = "fts-ui-native";
pub const FTS_UI_DESKTOP_ACTION_ID: &str = "fts-ui-desktop";

pub const FTS_DEMO_NATIVE_PANEL_ID: &str = "FTS_DEMO_NATIVE";
pub const FTS_DEMO_DESKTOP_PANEL_ID: &str = "FTS_DEMO_DESKTOP";
pub const FTS_DEMO_NATIVE_ACTION_ID: &str = "fts-demo-native";
pub const FTS_DEMO_DESKTOP_ACTION_ID: &str = "fts-demo-desktop";

#[component]
pub fn UiTestPanel() -> Element {
    rsx! {
        document::Style { {TAILWIND_CSS} }
        document::Style { {FTS_THEME_CSS} }
        document::Style { {BLITZ_FIXES} }

        fts_ui::showcase::Showcase {}
    }
}

/// Bare-minimum Dioxus demo — counter + animated time, no fts-ui, no
/// Tailwind. Lets us compare renderer perf with and without the design
/// system loaded.
#[component]
pub fn DemoPanel() -> Element {
    let mut count = use_signal(|| 0i32);
    rsx! {
        document::Style {
            "
            html, body {{ margin: 0; padding: 0; height: 100%; }}
            body {{
                font-family: system-ui, sans-serif;
                background: #0d1117;
                color: #e6edf3;
                display: flex;
                align-items: center;
                justify-content: center;
                flex-direction: column;
                gap: 16px;
            }}
            button {{
                background: #2a6df5;
                color: white;
                border: 0;
                border-radius: 8px;
                padding: 12px 24px;
                font-size: 18px;
                cursor: pointer;
            }}
            button:hover {{ background: #3b7df8; }}
            "
        }
        h1 { "Dioxus demo panel" }
        p { "count = {count}" }
        button {
            onclick: move |_| { count += 1; },
            "increment"
        }
        button {
            onclick: move |_| { count.set(0); },
            "reset"
        }
    }
}

/// Panel defs for both the full Showcase (`UiTestPanel`) and a minimal
/// counter (`DemoPanel`), each in Native (Blitz) and Desktop (WebView)
/// flavours. The counter pair is the renderer-perf baseline — anything
/// slower in `UiTestPanel` is on `fts-ui`, not the renderer.
pub fn panel_defs() -> [PanelDef; 4] {
    [
        PanelDef {
            id: FTS_UI_NATIVE_PANEL_ID,
            title: "FTS UI Native",
            component: PanelComponent::from_fn_ptr(UiTestPanel as fn() -> _ as *const ()),
            default_dock: DockPosition::Floating,
            renderer: PanelRenderer::Native,
            default_size: (900.0, 700.0),
            toggle_action: Some(FTS_UI_NATIVE_ACTION_ID),
        },
        PanelDef {
            id: FTS_UI_DESKTOP_PANEL_ID,
            title: "FTS UI Desktop",
            component: PanelComponent::from_fn_ptr(UiTestPanel as fn() -> _ as *const ()),
            default_dock: DockPosition::Floating,
            renderer: PanelRenderer::Desktop,
            default_size: (900.0, 700.0),
            toggle_action: Some(FTS_UI_DESKTOP_ACTION_ID),
        },
        PanelDef {
            id: FTS_DEMO_NATIVE_PANEL_ID,
            title: "FTS Demo Native",
            component: PanelComponent::from_fn_ptr(DemoPanel as fn() -> _ as *const ()),
            default_dock: DockPosition::Floating,
            renderer: PanelRenderer::Native,
            default_size: (480.0, 320.0),
            toggle_action: Some(FTS_DEMO_NATIVE_ACTION_ID),
        },
        PanelDef {
            id: FTS_DEMO_DESKTOP_PANEL_ID,
            title: "FTS Demo Desktop",
            component: PanelComponent::from_fn_ptr(DemoPanel as fn() -> _ as *const ()),
            default_dock: DockPosition::Floating,
            renderer: PanelRenderer::Desktop,
            default_size: (480.0, 320.0),
            toggle_action: Some(FTS_DEMO_DESKTOP_ACTION_ID),
        },
    ]
}

/// Action defs that toggle the corresponding panels. Handlers call straight
/// into the dock module so consumers don't need a `daw` facade in scope.
pub fn action_defs() -> [ActionDef; 4] {
    [
        ActionDef::new(FTS_UI_NATIVE_ACTION_ID, "FTS: UI Native", || {
            daw_reaper_dioxus::dock::toggle_panel(FTS_UI_NATIVE_PANEL_ID);
        })
        .in_menu(),
        ActionDef::new(FTS_UI_DESKTOP_ACTION_ID, "FTS: UI Desktop", || {
            daw_reaper_dioxus::dock::toggle_panel(FTS_UI_DESKTOP_PANEL_ID);
        })
        .in_menu(),
        ActionDef::new(FTS_DEMO_NATIVE_ACTION_ID, "FTS: Demo Native", || {
            daw_reaper_dioxus::dock::toggle_panel(FTS_DEMO_NATIVE_PANEL_ID);
        })
        .in_menu(),
        ActionDef::new(FTS_DEMO_DESKTOP_ACTION_ID, "FTS: Demo Desktop", || {
            daw_reaper_dioxus::dock::toggle_panel(FTS_DEMO_DESKTOP_PANEL_ID);
        })
        .in_menu(),
    ]
}
