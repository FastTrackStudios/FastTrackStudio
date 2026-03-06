use dioxus::prelude::*;
use signal_ui::views::{BrowserAssignment, CollectionBrowser};

// Browser dialog (near-full-screen)
// ---------------------------------------------------------------------------

#[component]
pub(crate) fn SignalBrowserDialog(
    on_close: Callback<()>,
    #[props(default)] on_assign: Option<EventHandler<BrowserAssignment>>,
) -> Element {
    let pick_mode = on_assign.is_some();
    let title = if pick_mode {
        "Assign from Browser"
    } else {
        "Collection Browser"
    };

    rsx! {
        // Overlay
        div {
            class: "fixed inset-0 z-50 bg-black/80 animate-fade-in",
            onclick: move |_| on_close.call(()),
        }

        // Content — near-full-screen
        div {
            class: "fixed inset-4 z-50 flex flex-col border border-white/[0.06] bg-zinc-950 rounded-xl shadow-2xl animate-scale-in overflow-hidden",
            style: "transform-origin: center center;",
            onclick: move |evt: MouseEvent| { evt.stop_propagation(); },

            // Header
            div { class: "flex items-center justify-between px-4 py-2 border-b border-white/[0.06] bg-white/[0.02] flex-shrink-0",
                div { class: "flex items-center gap-2",
                    h2 { class: "text-sm font-semibold", "{title}" }
                    if pick_mode {
                        span { class: "px-1.5 py-0.5 text-[10px] rounded bg-amber-900/40 text-amber-400 font-medium",
                            "Pick Mode"
                        }
                    }
                }
                button {
                    class: "px-2 py-1 text-xs rounded hover:bg-white/[0.06] text-zinc-500 hover:text-zinc-300 transition-colors",
                    onclick: move |_| on_close.call(()),
                    "\u{2715} Close"
                }
            }

            // Browser body
            div { class: "flex-1 min-h-0 overflow-hidden",
                CollectionBrowser {
                    on_assign: on_assign.clone(),
                }
            }
        }
    }
}
