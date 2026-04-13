//! Standalone Dioxus-native test — same UI as the REAPER panel
//! but running in a normal window. Tests if input handling works
//! outside of REAPER's event system.

use dioxus_native::prelude::*;

const TAILWIND_CSS: &str = include_str!("../../fts-extensions/assets/tailwind.css");
const FTS_THEME_CSS: &str = include_str!("../../fts-extensions/assets/fts-theme.css");

fn push_log(log: &mut Signal<Vec<String>>, entry: String) {
    log.write().push(entry);
    let len = log.read().len();
    if len > 30 {
        log.write().drain(0..len - 30);
    }
}

#[component]
fn App() -> Element {
    let mut native_value = use_signal(|| String::new());
    let mut event_log = use_signal(|| Vec::<String>::new());

    rsx! {
        document::Style { {TAILWIND_CSS} }
        document::Style { {FTS_THEME_CSS} }

        div {
            class: "dark min-h-full bg-background text-foreground p-4 font-sans",

            h2 { class: "text-xl font-bold mb-4", "Standalone Input Test (no REAPER)" }

            div { class: "grid grid-cols-2 gap-4",
                div { class: "flex flex-col gap-4",

                    // Native input
                    div { class: "flex flex-col gap-1",
                        label { class: "text-sm font-medium", "Native <input>" }
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

                    // Native textarea
                    div { class: "flex flex-col gap-1",
                        label { class: "text-sm font-medium", "Native <textarea>" }
                        textarea {
                            class: "w-full rounded-lg border border-zinc-700 bg-zinc-800 px-3 py-2 text-sm text-white min-h-[60px]",
                            placeholder: "Multi-line...",
                            onfocus: move |_| push_log(&mut event_log, "textarea: onfocus".into()),
                        }
                    }

                    // fts-ui Input
                    div { class: "flex flex-col gap-1",
                        label { class: "text-sm font-medium", "fts-ui Input" }
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

                    div { class: "flex gap-2",
                        button {
                            class: "px-4 py-2 bg-white text-black rounded-lg text-sm",
                            onclick: move |_| push_log(&mut event_log, "Button clicked!".into()),
                            "Click Me"
                        }
                        button {
                            class: "px-4 py-2 bg-zinc-700 text-white rounded-lg text-sm",
                            onclick: move |_| event_log.write().clear(),
                            "Clear Log"
                        }
                    }
                }

                div { class: "flex flex-col gap-1",
                    h3 { class: "text-sm font-medium", "Event Log" }
                    div {
                        class: "font-mono text-xs bg-black rounded-lg p-3 min-h-[300px] border border-zinc-800",
                        if event_log.read().is_empty() {
                            span { class: "text-zinc-600", "Click inputs and type..." }
                        }
                        for (i, entry) in event_log.read().iter().enumerate() {
                            div { key: "{i}", class: "text-green-400", "{entry}" }
                        }
                    }
                }
            }
        }
    }
}

fn main() {
    dioxus_native::launch(App);
}
