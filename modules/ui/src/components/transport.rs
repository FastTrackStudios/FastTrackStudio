use dioxus::prelude::*;
use lucide_dioxus::{SkipBack, Play, Pause, Repeat2, SkipForward, CircleCheck, CircleX};
use lumen_blocks::components::button::{Button, ButtonVariant, ButtonSize};

/// Connection status badge component
/// 
/// Displays a connection status indicator with an icon and label.
#[component]
pub fn ConnectionStatus(is_connected: Signal<bool>) -> Element {
    rsx! {
        div {
            class: "flex items-center gap-2",
            Button {
                variant: if is_connected() {
                    ButtonVariant::Primary
                } else {
                    ButtonVariant::Destructive
                },
                size: ButtonSize::Small,
                disabled: true,
                icon_left: rsx! {
                    if is_connected() {
                        CircleCheck {
                            size: 16,
                            color: "currentColor",
                        }
                    } else {
                        CircleX {
                            size: 16,
                            color: "currentColor",
                        }
                    }
                },
                if is_connected() { "Connected" } else { "Disconnected" }
            }
        }
    }
}

/// Transport control bar component with 4 buttons
/// 
/// Provides play/pause, loop, back, and forward controls.
/// All actions are handled via callbacks to keep the component domain-agnostic.
#[component]
pub fn TransportControlBar(
    is_playing: Signal<bool>, 
    is_looping: Signal<bool>,
    on_play_pause: Callback<()>,
    on_loop_toggle: Callback<()>,
    on_back: Callback<()>,
    on_forward: Callback<()>,
) -> Element {
    rsx! {
        div {
            class: "h-36 flex-shrink-0 border-t border-border bg-card grid grid-cols-4 divide-x divide-border",
            
            // Back Button
            div {
                class: "flex items-center justify-center gap-2 cursor-pointer hover:bg-accent transition-colors",
                onclick: move |_| {
                    if !is_playing() {
                        on_back.call(());
                    }
                },
                SkipBack {
                    size: 20,
                    color: "currentColor",
                }
                "Back"
            }
            
            // Play/Pause Button
            div {
                class: if is_playing() {
                    "flex items-center justify-center gap-2 cursor-pointer bg-primary text-primary-foreground hover:bg-primary/90 transition-colors"
                } else {
                    "flex items-center justify-center gap-2 cursor-pointer border border-border hover:bg-accent transition-colors"
                },
                onclick: move |_| {
                    on_play_pause.call(());
                },
                if is_playing() {
                    Pause {
                        size: 20,
                        color: "currentColor",
                    }
                } else {
                    Play {
                        size: 20,
                        color: "currentColor",
                    }
                }
                if is_playing() { "Pause" } else { "Play" }
            }
            
            // Loop Button
            div {
                class: if is_looping() {
                    "flex items-center justify-center gap-2 cursor-pointer bg-primary text-primary-foreground hover:bg-primary/90 transition-colors"
                } else {
                    "flex items-center justify-center gap-2 cursor-pointer border border-border hover:bg-accent transition-colors"
                },
                onclick: move |_| {
                    on_loop_toggle.call(());
                },
                Repeat2 {
                    size: 20,
                    color: "currentColor",
                }
                "Loop"
            }
            
            // Advance Button
            div {
                class: "flex items-center justify-center gap-2 cursor-pointer hover:bg-accent transition-colors",
                onclick: move |_| {
                    if !is_playing() {
                        on_forward.call(());
                    }
                },
                "Advance"
                SkipForward {
                    size: 20,
                    color: "currentColor",
                }
            }
        }
    }
}

