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
    /// Project name for track commands
    project_name: String,
    /// Callback when track mute is toggled: (project_name, track_index, new_muted_state)
    on_track_mute: Option<Callback<(String, usize, bool)>>,
    /// Callback when track solo is toggled: (project_name, track_index, new_solo_mode)
    on_track_solo: Option<Callback<(String, usize, daw::tracks::api::solo::SoloMode)>>,
) -> Element {
                // Compute visible tracks (filter out children of collapsed folders)
                let collapsed_state = collapsed_folders();
                let visible_indices: Vec<usize> = tracks.iter().enumerate()
                    .filter_map(|(idx, track)| {
                        // Check all ancestor folders (walk backwards through tracks)
                        // A track is hidden if ANY of its ancestor folders is collapsed
                        // We need to check all ancestors, not just the immediate parent,
                        // because a grandparent could be collapsed even if parent is not
                        for check_idx in (0..idx).rev() {
                            let check_track = &tracks[check_idx];
                            
                            // Skip tracks that are not ancestors (same or higher depth)
                            if check_track.track_depth.value() >= track.track_depth.value() {
                                continue;
                            }
                            
                            // This is a potential ancestor - check if it's a folder
                            if check_track.is_folder {
                                // Check if this ancestor folder is collapsed
                                if collapsed_state.get(check_idx).copied().unwrap_or(false) {
                                    // Found a collapsed ancestor - hide this track
                                    return None;
                                        }
                                // Continue checking - don't break, as we need to check all ancestors
                            }
                        }
                        
                        // No collapsed ancestor folders found - track is visible
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
                project_name: project_name.clone(),
                on_track_mute: on_track_mute.clone(),
                on_track_solo: on_track_solo.clone(),
                on_track_height_change: on_track_height_change.clone(),
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
    project_name: String,
    on_track_mute: Option<Callback<(String, usize, bool)>>,
    on_track_solo: Option<Callback<(String, usize, daw::tracks::api::solo::SoloMode)>>,
    on_track_height_change: Option<Callback<(usize, f64)>>,
) -> Element {
    // Precompute which tracks have children (for collapse button visibility)
    // A track has children if any following track has higher depth
    let tracks_with_children: Vec<bool> = tracks.iter().enumerate().map(|(idx, track)| {
        // Check if any following track has higher depth
        tracks.iter().skip(idx + 1).any(|next_track| {
            next_track.track_depth.value() > track.track_depth.value()
        })
    }).collect();
    
                rsx! {
                    div {
                        class: "flex flex-col flex-1 bg-card/50 overflow-y-auto",
                        for original_idx in visible_indices.iter() {
                TrackRow {
                    track: tracks[*original_idx].clone(),
                    original_idx: *original_idx,
                    has_children: tracks_with_children[*original_idx],
                    track_heights: track_heights.clone(),
                    collapsed_folders: collapsed_folders.clone(),
                    on_folder_toggle: on_folder_toggle.clone(),
                    project_name: project_name.clone(),
                    on_track_mute: on_track_mute.clone(),
                    on_track_solo: on_track_solo.clone(),
                    on_track_height_change: on_track_height_change.clone(),
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
    has_children: bool,
    track_heights: Memo<Vec<f64>>,
    collapsed_folders: Signal<Vec<bool>>,
    on_folder_toggle: Option<Callback<usize>>,
    project_name: String,
    on_track_mute: Option<Callback<(String, usize, bool)>>,
    on_track_solo: Option<Callback<(String, usize, daw::tracks::api::solo::SoloMode)>>,
    on_track_height_change: Option<Callback<(usize, f64)>>,
) -> Element {
                            let track_color = track.color.map(|c| {
                                let r = ((c >> 16) & 0xFF) as u8;
                                let g = ((c >> 8) & 0xFF) as u8;
                                let b = (c & 0xFF) as u8;
                                format!("rgb({}, {}, {})", r, g, b)
                            }).unwrap_or_else(|| "transparent".to_string());
                            
                            let indent_px = (track.track_depth.value() * 20) as f64; // 20px per depth level for better spacing
    let collapsed_state = collapsed_folders();
    let is_collapsed = collapsed_state.get(original_idx).copied().unwrap_or(false);
    let track_height = track_heights().get(original_idx).copied().unwrap_or(64.0);
    
    // Clone values needed for button handlers (to avoid move issues)
    let on_mute_cb = on_track_mute.clone();
    let on_solo_cb = on_track_solo.clone();
    let on_folder_toggle_cb = on_folder_toggle.clone();
    let on_height_change_cb = on_track_height_change.clone();
    let project_name_mute = project_name.clone();
    let project_name_solo = project_name.clone();
    let track_idx_mute = original_idx;
    let track_idx_solo = original_idx;
    let track_idx_height = original_idx;
    // Clone track state for button handlers (will be updated when track prop changes)
    let track_muted = track.muted;
    let track_solo_state = track.solo_state.clone();
                            
    rsx! {
                            div {
                                key: "{original_idx}",
                                class: "relative border-b border-border/60 flex-shrink-0 hover:bg-background/30 transition-colors",
                                style: format!(
                                    "height: {}px; background: linear-gradient(to right, {} 0%, {} 3%, transparent 3%);",
                track_height,
                                    track_color,
                                    track_color
                                ),
            // Depth indicator lines (vertical lines showing hierarchy) - more subtle
                    if track.track_depth.is_nested() {
                        for depth_level in 0..track.track_depth.value() {
                    div {
                        key: "{depth_level}",
                        class: "absolute top-0 bottom-0 w-0.5 bg-border/15",
                        style: format!("left: {}px; z-index: 0;", (depth_level * 20) as f64 + 12.0),
                    }
                }
            }
            
                                div {
                class: "relative h-full flex items-center gap-2 px-3 z-10",
                                    style: format!("padding-left: {}px;", indent_px + 12.0),
                
                // Collapse button - show for folders or any track with children
                                    if track.is_folder || has_children {
                                        button {
                        class: format!(
                            "w-6 h-6 flex items-center justify-center text-xs font-semibold rounded-md transition-all flex-shrink-0 {}",
                            if is_collapsed {
                                "text-muted-foreground/70 hover:bg-muted/60 hover:text-foreground"
                            } else {
                                "text-foreground/80 hover:bg-muted/60 hover:text-foreground"
                            }
                        ),
                                            onclick: move |_| {
                            if let Some(cb) = on_folder_toggle_cb {
                                cb.call(original_idx);
                                                }
                                            },
                                            title: if is_collapsed { "Expand" } else { "Collapse" },
                                            if is_collapsed { "▶" } else { "▼" }
                                        }
                                    } else {
                    // Show depth connector for non-folder tracks without children (horizontal line connecting to parent)
                    if track.track_depth.is_nested() {
                        div {
                            class: "w-6 h-6 flex items-center justify-center flex-shrink-0",
                            div {
                                class: "w-4 h-0.5 bg-border/30 rounded-full",
                            }
                        }
                    } else {
                        div { class: "w-6 flex-shrink-0" } // Spacer for top-level tracks
                                    }
                }
                
                                    // Mute button - use track.muted directly for reactivity
                                    button {
                                        class: format!(
                        "w-8 h-8 flex items-center justify-center text-xs font-bold rounded-md transition-all shadow-sm {}",
                                            if track.muted {
                            "bg-destructive/90 text-destructive-foreground hover:bg-destructive shadow-destructive/20"
                                            } else {
                                                "bg-muted/50 text-muted-foreground hover:bg-muted/70 hover:shadow-md"
                                            }
                                        ),
                                        onclick: move |_| {
                                            if let Some(cb) = on_mute_cb {
                                                let new_muted = !track.muted;
                                                cb.call((project_name_mute.clone(), track_idx_mute, new_muted));
                                            }
                                        },
                                        title: if track.muted { "Unmute" } else { "Mute" },
                                        "M"
                                    }
                
                                    // Solo button - use track.solo_state directly for reactivity
                                    button {
                                        class: format!(
                        "w-8 h-8 flex items-center justify-center text-xs font-bold rounded-md transition-all shadow-sm {}",
                                            match track.solo_state {
                            daw::tracks::api::solo::SoloMode::Solo => "bg-yellow-500/90 text-white hover:bg-yellow-600 shadow-yellow-500/20",
                            daw::tracks::api::solo::SoloMode::SoloInPlace => "bg-yellow-400/90 text-white hover:bg-yellow-500 shadow-yellow-400/20",
                                                _ => "bg-muted/50 text-muted-foreground hover:bg-muted/70 hover:shadow-md"
                                            }
                                        ),
                                        onclick: move |_| {
                                            if let Some(cb) = on_solo_cb {
                                                use daw::tracks::api::solo::SoloMode;
                                                let new_solo_mode = match track.solo_state {
                                                    SoloMode::Off | SoloMode::Unknown(_) => SoloMode::Solo,
                                                    _ => SoloMode::Off,
                                                };
                                                cb.call((project_name_solo.clone(), track_idx_solo, new_solo_mode));
                                            }
                                        },
                                        title: match track.solo_state {
                                            daw::tracks::api::solo::SoloMode::Solo => "Unsolo",
                                            daw::tracks::api::solo::SoloMode::SoloInPlace => "Unsolo (SIP)",
                                            _ => "Solo",
                                        },
                                        "S"
                                    }
                
                // Track name with better styling
                                    div {
                    class: "flex-1 text-sm text-foreground truncate flex items-center gap-2 min-w-0 pl-1",
                    // Show depth indicator for nested tracks - more visible
                    if track.track_depth.is_nested() {
                        div {
                            class: "flex-shrink-0 flex items-center gap-1",
                            for depth_idx in 0..track.track_depth.value() {
                                div {
                                    key: "{depth_idx}",
                                    class: "w-1.5 h-1.5 rounded-full bg-foreground/25",
                                }
                            }
                        }
                    }
                    span {
                        class: format!(
                            "truncate select-none {}",
                            if track.is_folder { 
                                "font-semibold text-foreground" 
                            } else { 
                                "text-foreground/90" 
                            }
                        ),
                                        "{track.name}"
                                    }
                                }
            }
            
            // Resize handle with drag functionality
                                div {
                class: "absolute bottom-0 left-0 right-0 h-3 bg-transparent hover:bg-primary/10 cursor-ns-resize transition-colors group",
                onmousedown: move |evt: Event<MouseData>| {
                    if let Some(cb) = on_height_change_cb {
                        let start_y = evt.data.client_coordinates().y;
                        let start_height = track_height;
                        let track_idx = track_idx_height;
                        
                        // Prevent default to avoid text selection
                        evt.prevent_default();
                        
                        // Set up drag handling
                        spawn(async move {
                            use dioxus::prelude::*;
                            
                            // For now, implement a simple click-to-cycle approach
                            // Full drag would require global mouse event listeners
                            // which is more complex in Dioxus
                            let min_height = 32.0;
                            let max_height = 256.0;
                            
                            // Cycle through common heights: 32, 48, 64, 96, 128
                            let heights = vec![32.0, 48.0, 64.0, 96.0, 128.0, 192.0, 256.0];
                            let current = start_height;
                            
                            // Find next height in sequence
                            let new_height = heights.iter()
                                .find(|&&h| h > current)
                                .copied()
                                .unwrap_or(heights[0])
                                .max(min_height)
                                .min(max_height);
                            
                            cb.call((track_idx, new_height));
                        });
                    }
                },
                // Visual indicator - more visible
                div {
                    class: "h-1 w-12 bg-border/40 group-hover:bg-primary/60 rounded-full mx-auto transition-colors",
                }
            }
        }
    }
}
