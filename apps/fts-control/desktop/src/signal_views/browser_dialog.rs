use dioxus::prelude::*;
use fts_ui::prelude::*;
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
        "Signal Browser"
    };

    rsx! {
        // Overlay
        div {
            class: "fixed inset-0 z-50 animate-fade-in bg-black/80",
            onclick: move |_| on_close.call(()),
        }

        // Content — near-full-screen
        div {
            class: "fixed inset-4 z-50 flex flex-col rounded-xl shadow-2xl animate-scale-in overflow-hidden bg-background border border-border",
            style: "transform-origin: center center;",
            onclick: move |evt: MouseEvent| { evt.stop_propagation(); },

            // Header
            div {
                class: "flex items-center justify-between px-4 py-2 flex-shrink-0 border-b border-border bg-card/50",
                div { class: "flex items-center gap-2",
                    h2 { class: "text-sm font-semibold", "{title}" }
                    if pick_mode {
                        Badge { variant: BadgeVariant::Secondary, class: "text-amber-400 bg-amber-900/40",
                            "Pick Mode"
                        }
                    }
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_close.call(()),
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
