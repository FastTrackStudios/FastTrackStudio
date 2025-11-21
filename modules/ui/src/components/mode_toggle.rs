//! Mode toggle component for switching between local and server modes

use dioxus::prelude::*;
use lumen_blocks::components::button::{Button, ButtonVariant, ButtonSize};

/// Mode toggle component
/// 
/// Displays a toggle button that switches between Local and Server modes.
/// This is a pure UI component - all logic is handled via callbacks.
#[component]
pub fn ModeToggle(
    is_server_mode: bool,
    is_connected: bool,
    on_toggle: Callback<()>,
) -> Element {
    rsx! {
        div {
            class: "flex items-center gap-2",
            Button {
                variant: if is_server_mode {
                    if is_connected {
                        ButtonVariant::Primary
                    } else {
                        ButtonVariant::Destructive
                    }
                } else {
                    ButtonVariant::Secondary
                },
                size: ButtonSize::Small,
                on_click: move |_| {
                    on_toggle.call(());
                },
                if is_server_mode {
                    if is_connected {
                        "Server (Connected)"
                    } else {
                        "Server (Disconnected)"
                    }
                } else {
                    "Local"
                }
            }
        }
    }
}

