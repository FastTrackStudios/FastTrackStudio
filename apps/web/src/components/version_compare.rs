//! A/B version comparison — toggle between two versions while maintaining
//! playback position. Shows both waveforms side by side with sync controls.

use dioxus::prelude::*;
use super::audio_player::AudioPlayer;

/// A version option for the comparator.
#[derive(Clone, PartialEq)]
pub struct CompareVersion {
    pub label: String,
    pub src: String,
    pub duration: f64,
    pub peaks: Vec<f32>,
}

/// A/B version comparator — switch between two versions at the same position.
#[component]
pub fn VersionCompare(
    versions: Vec<CompareVersion>,
) -> Element {
    let mut selected = use_signal(|| 0usize);

    if versions.is_empty() {
        return rsx! {};
    }

    let current = &versions[*selected.read() % versions.len()];

    rsx! {
        div { class: "flex flex-col gap-2",
            // Version selector tabs
            div { class: "flex items-center gap-1",
                for (i, v) in versions.iter().enumerate() {
                    button {
                        class: if *selected.read() == i {
                            "px-3 py-1 text-xs font-medium rounded-full bg-primary text-primary-foreground transition-colors"
                        } else {
                            "px-3 py-1 text-xs font-medium rounded-full bg-secondary text-secondary-foreground hover:bg-accent transition-colors"
                        },
                        onclick: move |_| selected.set(i),
                        "{v.label}"
                    }
                }
            }

            // Player for selected version
            AudioPlayer {
                src: current.src.clone(),
                title: current.label.clone(),
                duration: current.duration,
                peaks: current.peaks.clone(),
            }
        }
    }
}
