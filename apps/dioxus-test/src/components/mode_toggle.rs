//! Mode toggle component for switching between local and server modes

use dioxus::prelude::*;
use lumen_blocks::components::button::{Button, ButtonVariant, ButtonSize};
use crate::state::StateManagerMode;

/// Mode toggle component
/// 
/// Displays a toggle button that switches between Local and Server modes.
/// In Local mode, state is managed independently.
/// In Server mode, state comes from the server (desktop app).
#[component]
pub fn ModeToggle(
    mode: Signal<StateManagerMode>,
    connection_status: Signal<bool>,
) -> Element {
    let is_server_mode = matches!(mode(), StateManagerMode::Server { .. });
    
    rsx! {
        div {
            class: "flex items-center gap-2",
            Button {
                variant: if is_server_mode {
                    if connection_status() {
                        ButtonVariant::Primary
                    } else {
                        ButtonVariant::Destructive
                    }
                } else {
                    ButtonVariant::Secondary
                },
                size: ButtonSize::Small,
                onclick: move |_| {
                    match mode() {
                        StateManagerMode::Local => {
                            // Switch to server mode (connect to desktop app)
                            // Default to localhost - could be configurable
                            mode.set(StateManagerMode::Server { 
                                url: "localhost:8080".to_string() 
                            });
                        }
                        StateManagerMode::Server { .. } => {
                            // Switch to local mode
                            mode.set(StateManagerMode::Local);
                            connection_status.set(false);
                        }
                    }
                },
                if is_server_mode {
                    if connection_status() {
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

