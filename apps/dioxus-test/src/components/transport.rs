use dioxus::prelude::*;
use lucide_dioxus::{SkipBack, Play, Pause, Repeat2, SkipForward, CircleCheck, CircleX, ChevronDown};
use lumen_blocks::components::button::{Button, ButtonVariant, ButtonSize};
use crate::utils::navigation::{navigate_to_next_section, navigate_to_previous_section};
use crate::config::WebSocketMode;

/// Simple connection state enum
#[derive(Debug, Clone, PartialEq)]
pub enum ConnectionState {
    Connected,
    Disconnected,
}

/// Connection mode dropdown component
#[component]
pub fn ConnectionStatus(mut connection_mode: Signal<WebSocketMode>, connection_state: Signal<ConnectionState>) -> Element {
    let mut is_open = use_signal(|| false);
    let is_connected = matches!(connection_state(), ConnectionState::Connected);
    
    let mode_display = match connection_mode() {
        WebSocketMode::Local => "Local",
        WebSocketMode::Server => "Server",
        WebSocketMode::Reaper { .. } => "REAPER",
        WebSocketMode::Custom { .. } => "Custom",
    };
    
    // Close dropdown when clicking outside
    use_effect(move || {
        if is_open() {
            spawn(async move {
                // This will be handled by the onclick handler on the document
            });
        }
    });
    
    rsx! {
        div {
            class: "relative",
            // Dropdown trigger button
            div {
                class: "cursor-pointer",
                onclick: move |e| {
                    e.stop_propagation();
                    is_open.set(!is_open());
                },
                Button {
                    variant: if is_connected {
                        ButtonVariant::Primary
                    } else {
                        ButtonVariant::Destructive
                    },
                    size: ButtonSize::Small,
                    icon_left: rsx! {
                        if is_connected {
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
                    icon_right: rsx! {
                        ChevronDown {
                            size: 16,
                            color: "currentColor",
                        }
                    },
                    "{mode_display}"
                }
            }
            
            // Backdrop to close dropdown when clicking outside
            if is_open() {
                div {
                    class: "fixed inset-0 z-40",
                    onclick: move |_| {
                        is_open.set(false);
                    },
                }
            }
            
            // Dropdown menu
            if is_open() {
                div {
                    class: "absolute right-0 top-full mt-1 z-50 min-w-[120px] bg-card border border-border rounded-md shadow-lg",
                    onclick: move |e| {
                        e.stop_propagation();
                    },
                    div {
                        class: "py-1",
                        // Local option
                        div {
                            class: "px-3 py-2 text-sm cursor-pointer hover:bg-accent transition-colors flex items-center gap-2",
                            onclick: move |_| {
                                connection_mode.set(WebSocketMode::Local);
                                is_open.set(false);
                            },
                            if matches!(connection_mode(), WebSocketMode::Local) {
                                CircleCheck {
                                    size: 14,
                                    color: "currentColor",
                                }
                            } else {
                                div { class: "w-[14px]" }
                            }
                            "Local"
                        }
                        // Server option (native Dioxus server)
                        div {
                            class: "px-3 py-2 text-sm cursor-pointer hover:bg-accent transition-colors flex items-center gap-2",
                            onclick: move |_| {
                                connection_mode.set(WebSocketMode::Server);
                                is_open.set(false);
                            },
                            if matches!(connection_mode(), WebSocketMode::Server) {
                                CircleCheck {
                                    size: 14,
                                    color: "currentColor",
                                }
                            } else {
                                div { class: "w-[14px]" }
                            }
                            "Server"
                        }
                        // REAPER option
                        div {
                            class: "px-3 py-2 text-sm cursor-pointer hover:bg-accent transition-colors flex items-center gap-2",
                            onclick: move |_| {
                                connection_mode.set(WebSocketMode::Reaper { 
                                    url: "ws://localhost:8081".to_string() 
                                });
                                is_open.set(false);
                            },
                            if matches!(connection_mode(), WebSocketMode::Reaper { .. }) {
                                CircleCheck {
                                    size: 14,
                                    color: "currentColor",
                                }
                            } else {
                                div { class: "w-[14px]" }
                            }
                            "REAPER"
                        }
                    }
                }
            }
        }
    }
}

/// Transport control bar component with 4 buttons
#[component]
pub fn TransportControlBar(
    is_playing: Signal<bool>, 
    is_looping: Signal<bool>,
    setlist: setlist::Setlist,
    mut current_song_index: Signal<Option<usize>>,
    mut current_section_index: Signal<Option<usize>>,
    mut transport_positions: Signal<std::collections::HashMap<String, f64>>,
    mut song_positions: Signal<std::collections::HashMap<String, f64>>,
) -> Element {
    // Clone setlist for use in closures
    let setlist_back = setlist.clone();
    let setlist_advance = setlist.clone();
    rsx! {
        div {
            class: "h-36 flex-shrink-0 border-t border-border bg-card grid grid-cols-4 divide-x divide-border",
            
            // Back Button
            div {
                class: "flex items-center justify-center gap-2 cursor-pointer hover:bg-accent transition-colors",
                onclick: move |_| {
                    if !is_playing() {
                        let (new_song_idx, new_section_idx) = transport_positions.with_mut(|transport_pos| {
                            song_positions.with_mut(|song_pos| {
                                navigate_to_previous_section(
                                    &setlist_back,
                                    current_song_index(),
                                    current_section_index(),
                                    transport_pos,
                                    song_pos,
                                )
                            })
                        });
                        current_song_index.set(new_song_idx);
                        current_section_index.set(new_section_idx);
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
                    is_playing.set(!is_playing());
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
                    is_looping.set(!is_looping());
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
                        let (new_song_idx, new_section_idx) = transport_positions.with_mut(|transport_pos| {
                            song_positions.with_mut(|song_pos| {
                                navigate_to_next_section(
                                    &setlist_advance,
                                    current_song_index(),
                                    current_section_index(),
                                    transport_pos,
                                    song_pos,
                                )
                            })
                        });
                        current_song_index.set(new_song_idx);
                        current_section_index.set(new_section_idx);
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

