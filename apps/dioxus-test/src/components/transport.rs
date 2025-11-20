use dioxus::prelude::*;
use lucide_dioxus::{SkipBack, Play, Pause, Repeat2, SkipForward, CircleCheck, CircleX};
use lumen_blocks::components::button::{Button, ButtonVariant, ButtonSize};
use crate::utils::navigation::{navigate_to_next_section, navigate_to_previous_section};

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

