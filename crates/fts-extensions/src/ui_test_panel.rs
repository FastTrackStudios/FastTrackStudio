//! Reaper-Dioxus UI Component Test Panel
//!
//! Stress-tests input handling and fts-ui components in the native
//! Dioxus/Blitz renderer inside REAPER's docker system.

use daw::module::{ActionDef, DockPosition, PanelComponent, PanelDef};
use reaper_dioxus::prelude::*;

// Embed Tailwind CSS + FTS theme at compile time
const TAILWIND_CSS: &str = include_str!("../assets/tailwind.css");
const FTS_THEME_CSS: &str = include_str!("../assets/fts-theme.css");

// Blitz CSS workarounds — nested selectors like `&:disabled` aren't fully supported
const BLITZ_FIXES: &str = r#"
/* Fix Blitz applying disabled:cursor-not-allowed unconditionally */
input, textarea, select, button {
    cursor: auto !important;
}
input:disabled, textarea:disabled, button:disabled {
    cursor: not-allowed !important;
}
/* Force dark mode colors (Blitz has no prefers-color-scheme) */
:root {
    color-scheme: dark;
}
"#;

fn push_log(log: &mut Signal<Vec<String>>, entry: String) {
    log.write().push(entry);
    let len = log.read().len();
    if len > 30 {
        log.write().drain(0..len - 30);
    }
}

/// Root component for the UI test panel.
#[component]
pub fn UiTestPanel() -> Element {
    let mut native_value = use_signal(|| String::new());
    let mut event_log = use_signal(|| Vec::<String>::new());

    rsx! {
        document::Style { {TAILWIND_CSS} }
        document::Style { {FTS_THEME_CSS} }
        document::Style { {BLITZ_FIXES} }

        div {
            class: "dark min-h-full bg-background text-foreground p-4 font-sans",

            h2 { class: "text-xl font-bold mb-4", "Input Focus Test" }

            div { class: "grid grid-cols-2 gap-4",

                // Left column: inputs
                div { class: "flex flex-col gap-4",

                    // 1. Raw HTML input
                    div { class: "flex flex-col gap-1",
                        label { class: "text-sm font-medium", "Native <input> (raw HTML)" }
                        input {
                            class: "h-9 w-full rounded-lg border border-zinc-700 bg-zinc-800 px-3 py-1 text-sm text-white",
                            r#type: "text",
                            placeholder: "Type here...",
                            value: "{native_value}",
                            oninput: move |e: FormEvent| {
                                let v = e.value();
                                push_log(&mut event_log, format!("oninput: '{v}'"));
                                native_value.set(v);
                            },
                            onfocus: move |_| push_log(&mut event_log, "input: onfocus".into()),
                            onblur: move |_| push_log(&mut event_log, "input: onblur".into()),
                        }
                        p { class: "text-xs text-zinc-400", "Value: \"{native_value}\"" }
                    }

                    // 2. Raw HTML textarea
                    div { class: "flex flex-col gap-1",
                        label { class: "text-sm font-medium", "Native <textarea>" }
                        textarea {
                            class: "w-full rounded-lg border border-zinc-700 bg-zinc-800 px-3 py-2 text-sm text-white min-h-[60px]",
                            placeholder: "Multi-line text...",
                            onfocus: move |_| push_log(&mut event_log, "textarea: onfocus".into()),
                        }
                    }

                    // 3. fts-ui Input
                    div { class: "flex flex-col gap-1",
                        label { class: "text-sm font-medium", "fts-ui Input component" }
                        {
                            use fts_ui::prelude::*;
                            let fts_value = use_signal(|| String::new());
                            rsx! {
                                Input {
                                    value: fts_value,
                                    placeholder: "fts-ui input...",
                                }
                                p { class: "text-xs text-zinc-400", "Value: \"{fts_value}\"" }
                            }
                        }
                    }

                    // 4. Button test
                    div { class: "flex gap-2",
                        button {
                            class: "px-4 py-2 bg-white text-black rounded-lg text-sm font-medium",
                            onclick: move |_| push_log(&mut event_log, "Button clicked!".into()),
                            "Click Me"
                        }
                        button {
                            class: "px-4 py-2 bg-zinc-700 text-white rounded-lg text-sm font-medium",
                            onclick: move |_| event_log.write().clear(),
                            "Clear Log"
                        }
                    }
                }

                // Right column: event log
                div { class: "flex flex-col gap-1",
                    h3 { class: "text-sm font-medium", "Event Log" }
                    div {
                        class: "font-mono text-xs bg-black rounded-lg p-3 min-h-[300px] max-h-[500px] overflow-y-auto border border-zinc-800",
                        if event_log.read().is_empty() {
                            span { class: "text-zinc-600", "Click inputs and type..." }
                        }
                        for (i, entry) in event_log.read().iter().enumerate() {
                            div { key: "{i}", class: "text-green-400 leading-relaxed", "{entry}" }
                        }
                    }
                }
            }
        }
    }
}

/// Panel definition for the UI test panel.
pub fn panel_def() -> PanelDef {
    PanelDef {
        id: "FTS_UI_TEST",
        title: "UI Component Test",
        component: PanelComponent::from_fn_ptr(UiTestPanel as fn() -> _ as *const ()),
        default_dock: DockPosition::Floating,
        default_size: (800.0, 600.0),
        toggle_action: Some("FTS_UI_TEST_TOGGLE"),
    }
}

/// Action definition for toggling the UI test panel.
pub fn action_def() -> ActionDef {
    ActionDef::new(
        "FTS_UI_TEST_TOGGLE",
        "FTS: Toggle UI Component Test Panel",
        || {
            reaper_dioxus::toggle_panel("FTS_UI_TEST");
        },
    )
    .in_menu()
}
