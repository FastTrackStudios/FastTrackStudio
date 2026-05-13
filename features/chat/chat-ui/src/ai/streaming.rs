//! Three-dot pulse used inline beside a streaming assistant message.

use dioxus::prelude::*;

#[component]
pub fn StreamingIndicator() -> Element {
    rsx! {
        span { class: "inline-flex items-center gap-1 px-1 align-middle",
            span { class: "h-1.5 w-1.5 rounded-full bg-current animate-pulse" }
            span { class: "h-1.5 w-1.5 rounded-full bg-current animate-pulse [animation-delay:120ms]" }
            span { class: "h-1.5 w-1.5 rounded-full bg-current animate-pulse [animation-delay:240ms]" }
        }
    }
}
