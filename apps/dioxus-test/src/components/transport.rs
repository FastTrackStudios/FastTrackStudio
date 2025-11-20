use dioxus::prelude::*;
use lucide_dioxus::{SkipBack, Play, Pause, Repeat2, SkipForward, CircleCheck, CircleX};
use lumen_blocks::components::button::{Button, ButtonVariant, ButtonSize};

/// Connection status badge component
#[component]
pub fn ConnectionStatus() -> Element {
    // For now, using a signal to track connection status
    // Later this can be connected to actual WebSocket state
    let is_connected = use_signal(|| true);
    
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
#[component]
pub fn TransportControlBar(is_playing: Signal<bool>, is_looping: Signal<bool>) -> Element {
    rsx! {
        div {
            class: "h-36 flex-shrink-0 border-t border-border bg-card grid grid-cols-4 divide-x divide-border",
            
            // Back Button
            div {
                class: "flex items-center justify-center p-2",
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Large,
                    full_width: true,
                    on_click: move |_| {
                        // TODO: Implement back functionality
                    },
                    icon_left: rsx! {
                        SkipBack {
                            size: 20,
                            color: "currentColor",
                        }
                    },
                    "Back"
                }
            }
            
            // Play/Pause Button
            div {
                class: "flex items-center justify-center p-2",
                Button {
                    variant: if is_playing() {
                        ButtonVariant::Primary
                    } else {
                        ButtonVariant::Outline
                    },
                    size: ButtonSize::Large,
                    full_width: true,
                    on_click: move |_| {
                        is_playing.set(!is_playing());
                    },
                    icon_left: rsx! {
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
                    },
                    if is_playing() { "Pause" } else { "Play" }
                }
            }
            
            // Loop Button
            div {
                class: "flex items-center justify-center p-2",
                Button {
                    variant: if is_looping() {
                        ButtonVariant::Primary
                    } else {
                        ButtonVariant::Outline
                    },
                    size: ButtonSize::Large,
                    full_width: true,
                    on_click: move |_| {
                        is_looping.set(!is_looping());
                    },
                    icon_left: rsx! {
                        Repeat2 {
                            size: 20,
                            color: "currentColor",
                        }
                    },
                    "Loop"
                }
            }
            
            // Advance Button
            div {
                class: "flex items-center justify-center p-2",
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Large,
                    full_width: true,
                    on_click: move |_| {
                        // TODO: Implement advance functionality
                    },
                    icon_right: rsx! {
                        SkipForward {
                            size: 20,
                            color: "currentColor",
                        }
                    },
                    "Advance"
                }
            }
        }
    }
}

