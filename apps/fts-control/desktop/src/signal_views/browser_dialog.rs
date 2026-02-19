use dioxus::prelude::*;
use signal_ui::views::CollectionBrowser;

// Browser dialog (near-full-screen)
// ---------------------------------------------------------------------------

#[component]
pub(crate) fn SignalBrowserDialog(on_close: Callback<()>) -> Element {
    rsx! {
        // Overlay
        div {
            class: "fixed inset-0 z-50 bg-black/80 animate-fade-in",
            onclick: move |_| on_close.call(()),
        }

        // Content — near-full-screen
        div {
            class: "fixed inset-4 z-50 flex flex-col border border-border bg-background rounded-lg shadow-2xl animate-scale-in overflow-hidden",
            style: "transform-origin: center center;",
            onclick: move |evt: MouseEvent| { evt.stop_propagation(); },

            // Header
            div { class: "flex items-center justify-between px-4 py-2 border-b border-border bg-muted/30 flex-shrink-0",
                h2 { class: "text-sm font-semibold", "Collection Browser" }
                button {
                    class: "px-2 py-1 text-xs rounded hover:bg-muted text-muted-foreground hover:text-foreground transition-colors",
                    onclick: move |_| on_close.call(()),
                    "\u{2715} Close"
                }
            }

            // Browser body
            div { class: "flex-1 min-h-0 overflow-hidden",
                CollectionBrowser {}
            }
        }
    }
}
