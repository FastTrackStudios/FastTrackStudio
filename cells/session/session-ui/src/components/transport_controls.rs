//! Transport Control Components
//!
//! Transport control bar with grid layout.
//! Copied from FastTrackStudio for exact styling match.

use crate::prelude::*;
use lucide_dioxus::{
    Pause as PauseIcon, Play as PlayIcon, Repeat2 as LoopIcon, SkipBack as BackIcon,
    SkipForward as ForwardIcon,
};

/// Transport control bar component with 4 buttons
///
/// Provides play/pause, loop, back, and forward controls.
/// All actions are handled via callbacks to keep the component domain-agnostic.
#[component]
pub fn TransportControlBar(
    is_playing: bool,
    is_looping: bool,
    on_play_pause: Callback<()>,
    on_loop_toggle: Callback<()>,
    on_back: Callback<()>,
    on_forward: Callback<()>,
) -> Element {
    let playing = is_playing;
    let looping = is_looping;

    rsx! {
        div {
            class: "h-36 flex-shrink-0 border-t border-border bg-card grid grid-cols-4 divide-x divide-border",

            // Back Button
            div {
                class: "flex items-center justify-center gap-2 cursor-pointer hover:bg-accent transition-colors",
                onclick: move |_| {
                    if !playing {
                        on_back.call(());
                    }
                },
                BackIcon {
                    size: 20,
                    color: "currentColor",
                }
                "Back"
            }

            // Play/Pause Button
            div {
                class: if playing {
                    "flex items-center justify-center gap-2 cursor-pointer bg-primary text-primary-foreground hover:bg-primary/90 transition-colors"
                } else {
                    "flex items-center justify-center gap-2 cursor-pointer border border-border hover:bg-accent transition-colors"
                },
                onclick: move |_| {
                    on_play_pause.call(());
                },
                if playing {
                    PauseIcon {
                        size: 20,
                        color: "currentColor",
                    }
                } else {
                    PlayIcon {
                        size: 20,
                        color: "currentColor",
                    }
                }
                if playing { "Pause" } else { "Play" }
            }

            // Loop Button
            div {
                class: if looping {
                    "flex items-center justify-center gap-2 cursor-pointer bg-primary text-primary-foreground hover:bg-primary/90 transition-colors"
                } else {
                    "flex items-center justify-center gap-2 cursor-pointer border border-border hover:bg-accent transition-colors"
                },
                onclick: move |_| {
                    on_loop_toggle.call(());
                },
                LoopIcon {
                    size: 20,
                    color: "currentColor",
                }
                "Loop"
            }

            // Advance Button
            div {
                class: "flex items-center justify-center gap-2 cursor-pointer hover:bg-accent transition-colors",
                onclick: move |_| {
                    if !playing {
                        on_forward.call(());
                    }
                },
                "Advance"
                ForwardIcon {
                    size: 20,
                    color: "currentColor",
                }
            }
        }
    }
}
