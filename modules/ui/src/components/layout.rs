use dioxus::prelude::*;
use crate::components::transport::ConnectionStatus;
use crate::components::progress::{SongProgressBar, ProgressSection, TempoMarker};
use crate::components::song::SongTitle;
use crate::components::transport::TransportControlBar;
use crate::components::sidebar_items::{SongItem, SongItemData};
use crate::components::mode_toggle::ModeToggle;

/// Top bar component
#[component]
pub fn TopBar(
    is_connected: Signal<bool>,
    #[props(default = false)]
    is_server_mode: bool,
    #[props(default)]
    on_toggle_mode: Option<Callback<()>>,
) -> Element {
    rsx! {
        div {
            class: "h-12 flex-shrink-0 border-b border-border bg-card flex items-center justify-between px-4",
            div {
                class: "flex items-center gap-4",
                if let Some(on_toggle) = on_toggle_mode {
                    ModeToggle {
                        is_server_mode: is_server_mode,
                        is_connected: is_connected(),
                        on_toggle: on_toggle,
                    }
                }
                ConnectionStatus { is_connected }
            }
            h1 {
                class: "text-lg font-semibold text-card-foreground",
                "FastTrackStudio"
            }
            div {
                class: "w-16", // Placeholder for right-aligned items
            }
        }
    }
}

/// Left sidebar component (1/3 width) - Navigator
#[component]
pub fn Sidebar(
    songs: Vec<SongItemData>,
    current_song_index: Signal<Option<usize>>,
    current_section_index: Signal<Option<usize>>,
    is_playing: Signal<bool>,
    on_song_click: Callback<usize>,
    on_section_click: Callback<(usize, usize)>,
) -> Element {
    rsx! {
        div {
            class: "w-1/3 border-r border-sidebar-border bg-sidebar overflow-y-auto",
            div {
                class: "p-4",
                h2 {
                    class: "text-xl font-bold text-sidebar-foreground mb-4",
                    "Navigator"
                }
                div {
                    class: "space-y-1",
                    for (index, song_data) in songs.iter().enumerate() {
                        SongItem {
                            song_data: song_data.clone(),
                            index: index,
                            is_expanded: current_song_index() == Some(index),
                            current_section_index: current_section_index(),
                            on_song_click: {
                                let index = index;
                                let on_click = on_song_click.clone();
                                Callback::new(move |_| {
                                    on_click.call(index);
                                })
                            },
                            on_section_click: {
                                let song_idx = index;
                                let on_click = on_section_click.clone();
                                Callback::new(move |section_idx| {
                                    on_click.call((song_idx, section_idx));
                                })
                            },
                        }
                    }
                }
            }
        }
    }
}

/// Detail badges data
#[derive(Clone, Debug, PartialEq)]
pub struct DetailBadge {
    pub label: String,
    pub value: String,
}

/// Main content area component (2/3 width)
#[component]
pub fn MainContent(
    song_name: String,
    song_position: Option<usize>,
    song_total: Option<usize>,
    progress: Signal<f64>,
    sections: Vec<ProgressSection>,
    detail_badges: Option<Vec<DetailBadge>>,
    is_playing: Signal<bool>,
    is_looping: Signal<bool>,
    on_section_click: Option<Callback<usize>>,
    on_play_pause: Callback<()>,
    on_loop_toggle: Callback<()>,
    on_back: Callback<()>,
    on_forward: Callback<()>,
    #[props(default)]
    tempo_markers: Vec<TempoMarker>,
) -> Element {
    rsx! {
        div {
            class: "flex-1 flex flex-col overflow-hidden bg-background",
            // Scrollable content area
            div {
                class: "flex-1 overflow-y-auto",
                div {
                    class: "p-6 relative flex items-center justify-center h-full",
                    // Song Title Component (positioned above progress bar)
                    div {
                        class: "absolute left-0 right-0",
                        style: "bottom: calc(50% + 4rem);",
                        SongTitle {
                            song_name: song_name.clone(),
                            position: song_position,
                            total: song_total,
                        }
                    }
                    // Progress Bar Component (centered independently)
                    // Use key wrapper to force remount when song changes, preventing cross-song animations
                    div {
                        key: "{song_position.map(|i| i.to_string()).unwrap_or_else(|| \"none\".to_string())}",
                        class: "w-full",
                        SongProgressBar {
                            progress: progress,
                            sections: sections,
                            on_section_click: on_section_click,
                            tempo_markers: tempo_markers.clone(),
                            song_key: song_position.map(|i| i.to_string()),
                        }
                    }
                    // Detail Badges Component (positioned below progress bar)
                    if let Some(badges) = detail_badges {
                        div {
                            class: "absolute left-0 right-0",
                            style: "top: calc(50% + 4rem);",
                            div {
                                class: "flex flex-wrap gap-2 justify-center",
                                for badge in badges {
                                    div {
                                        class: "px-3 py-1 rounded-full bg-secondary text-secondary-foreground text-sm",
                                        span {
                                            class: "font-medium",
                                            "{badge.label}: "
                                        }
                                        "{badge.value}"
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // Transport control bar at bottom
            TransportControlBar {
                is_playing: is_playing,
                is_looping: is_looping,
                on_play_pause: on_play_pause.clone(),
                on_loop_toggle: on_loop_toggle.clone(),
                on_back: on_back.clone(),
                on_forward: on_forward.clone(),
            }
        }
    }
}

