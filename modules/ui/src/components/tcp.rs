use dioxus::prelude::*;
use crate::components::ruler::RULER_HEIGHT;

/// Track Control Panel (TCP) - vertical panel showing track names and controls
#[component]
pub fn TrackControlPanel(
    /// Track names
    tracks: Vec<String>,
    /// Track heights (in pixels) - reactive signal or memo
    track_heights: Memo<Vec<f64>>,
    /// Callback when track height changes: (track_index, new_height)
    on_track_height_change: Option<Callback<(usize, f64)>>,
) -> Element {
    rsx! {
        div {
            class: "w-48 flex-shrink-0 border-r border-border bg-card flex flex-col h-full",
            // Ruler header (matches ruler height exactly)
            div {
                class: "flex-shrink-0 border-b border-border bg-background",
                style: format!("height: {}px;", RULER_HEIGHT),
                div {
                    class: "h-full px-2 flex items-center text-xs text-muted-foreground",
                    "Ruler"
                }
            }
            // Tracks list - extends to fill remaining space, even with no tracks
            div {
                class: "flex flex-col flex-1 bg-card",
                for (idx, track_name) in tracks.iter().enumerate() {
                    div {
                        key: "{track_name}-{idx}",
                        class: "relative border-b border-border bg-card flex-shrink-0",
                        style: format!("height: {}px;", track_heights().get(idx).copied().unwrap_or(64.0)),
                        div {
                            class: "h-full px-2 flex items-center text-sm text-foreground",
                            "{track_name}"
                        }
                        // Resize handle
                        div {
                            class: "absolute bottom-0 left-0 right-0 h-1 bg-border hover:bg-primary cursor-ns-resize",
                            onmousedown: move |_evt| {
                                // TODO: Implement resize drag handling
                            },
                        }
                    }
                }
            }
        }
    }
}

