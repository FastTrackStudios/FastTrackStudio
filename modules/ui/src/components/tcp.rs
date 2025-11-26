use dioxus::prelude::*;
use crate::components::ruler::RULER_HEIGHT;
use daw::tracks::Track;

/// Track Control Panel (TCP) - vertical panel showing track names and controls
#[component]
pub fn TrackControlPanel(
    /// Tracks from the project
    tracks: Vec<Track>,
    /// Track heights (in pixels) - reactive signal or memo
    track_heights: Memo<Vec<f64>>,
    /// Collapsed folder state - track index -> collapsed
    collapsed_folders: Signal<Vec<bool>>,
    /// Callback when track height changes: (track_index, new_height)
    on_track_height_change: Option<Callback<(usize, f64)>>,
    /// Callback when folder is toggled: (track_index)
    on_folder_toggle: Option<Callback<usize>>,
) -> Element {
                // Compute visible tracks (filter out children of collapsed folders)
                let collapsed_state = collapsed_folders();
                let visible_indices: Vec<usize> = tracks.iter().enumerate()
                    .filter_map(|(idx, track)| {
                        // Check if any parent folder is collapsed
                        for (prev_idx, prev_track) in tracks.iter().take(idx).enumerate() {
                            if prev_track.is_folder && collapsed_state.get(prev_idx).copied().unwrap_or(false) {
                                // Check if this track is a child of the collapsed folder
                                if track.track_depth > prev_track.track_depth {
                                    // Find the parent folder
                                    let mut check_idx = prev_idx;
                                    while check_idx > 0 {
                                        if tracks[check_idx].is_folder && collapsed_state.get(check_idx).copied().unwrap_or(false) {
                                            return None; // Hidden by collapsed parent
                                        }
                                        if tracks[check_idx].track_depth < track.track_depth {
                                            break;
                                        }
                                        check_idx -= 1;
                                    }
                                    return None; // Hidden by collapsed folder
                                }
                            }
                        }
                        Some(idx)
                    })
                    .collect();
                
    rsx! {
        div {
            class: "w-64 flex-shrink-0 border-r border-border bg-card flex flex-col h-full",
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
            TrackList {
                tracks: tracks.clone(),
                visible_indices: visible_indices.clone(),
                track_heights: track_heights.clone(),
                collapsed_folders: collapsed_folders.clone(),
                on_folder_toggle: on_folder_toggle.clone(),
            }
        }
    }
}

/// Tracks list component
#[component]
fn TrackList(
    tracks: Vec<Track>,
    visible_indices: Vec<usize>,
    track_heights: Memo<Vec<f64>>,
    collapsed_folders: Signal<Vec<bool>>,
    on_folder_toggle: Option<Callback<usize>>,
) -> Element {
                rsx! {
                    div {
                        class: "flex flex-col flex-1 bg-card overflow-y-auto",
                        for original_idx in visible_indices.iter() {
                TrackRow {
                    track: tracks[*original_idx].clone(),
                    original_idx: *original_idx,
                    track_heights: track_heights.clone(),
                    collapsed_folders: collapsed_folders.clone(),
                    on_folder_toggle: on_folder_toggle.clone(),
                }
            }
        }
    }
}

/// Individual track row component
#[component]
fn TrackRow(
    track: Track,
    original_idx: usize,
    track_heights: Memo<Vec<f64>>,
    collapsed_folders: Signal<Vec<bool>>,
    on_folder_toggle: Option<Callback<usize>>,
) -> Element {
                            let track_color = track.color.map(|c| {
                                let r = ((c >> 16) & 0xFF) as u8;
                                let g = ((c >> 8) & 0xFF) as u8;
                                let b = (c & 0xFF) as u8;
                                format!("rgb({}, {}, {})", r, g, b)
                            }).unwrap_or_else(|| "transparent".to_string());
                            
                            let indent_px = (track.track_depth * 16) as f64; // 16px per depth level
    let collapsed_state = collapsed_folders();
    let is_collapsed = collapsed_state.get(original_idx).copied().unwrap_or(false);
    let track_height = track_heights().get(original_idx).copied().unwrap_or(64.0);
                            
    rsx! {
                            div {
                                key: "{original_idx}",
                                class: "relative border-b border-border flex-shrink-0",
                                style: format!(
                                    "height: {}px; background-color: {};",
                track_height,
                                    track_color
                                ),
                                div {
                                    class: "h-full flex items-center gap-1 px-2",
                                    style: format!("padding-left: {}px;", indent_px + 8.0),
                                    // Folder collapse button
                                    if track.is_folder {
                                        button {
                                            class: "w-4 h-4 flex items-center justify-center text-xs text-muted-foreground hover:text-foreground",
                                            onclick: move |_| {
                                                if let Some(cb) = on_folder_toggle {
                                cb.call(original_idx);
                                                }
                                            },
                                            if is_collapsed { "▶" } else { "▼" }
                                        }
                                    } else {
                                        div { class: "w-4" } // Spacer for non-folder tracks
                                    }
                                    // Mute button
                                    button {
                                        class: format!(
                                            "w-6 h-6 flex items-center justify-center text-xs rounded {}",
                                            if track.muted {
                                                "bg-destructive text-destructive-foreground"
                                            } else {
                                                "bg-muted text-muted-foreground hover:bg-muted/80"
                                            }
                                        ),
                                        onclick: move |_| {
                                            // TODO: Implement mute toggle
                                        },
                                        "M"
                                    }
                                    // Solo button
                                    button {
                                        class: format!(
                                            "w-6 h-6 flex items-center justify-center text-xs rounded {}",
                                            match track.solo_state {
                                                daw::tracks::api::solo::SoloMode::Solo => "bg-yellow-500 text-white",
                                                daw::tracks::api::solo::SoloMode::SoloInPlace => "bg-yellow-400 text-white",
                                                _ => "bg-muted text-muted-foreground hover:bg-muted/80"
                                            }
                                        ),
                                        onclick: move |_| {
                                            // TODO: Implement solo toggle
                                        },
                                        "S"
                                    }
                                    // Track name
                                    div {
                                        class: "flex-1 text-sm text-foreground truncate",
                                        "{track.name}"
                                    }
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
