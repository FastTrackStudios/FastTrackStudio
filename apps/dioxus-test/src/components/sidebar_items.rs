use dioxus::prelude::*;
use setlist::{Setlist, Song};
use crate::components::progress::CompactProgressBar;
use crate::utils::{get_song_color_bright, get_song_color_muted, get_section_color_bright, get_section_color_muted, get_project_name, calculate_song_progress};
use crate::utils::sidebar_helpers::{get_song_progress_for_sidebar, get_section_progress_for_sidebar, reset_previous_song_position, update_position_on_section_click};
use std::collections::HashMap;

/// Vertical progress bar component for sidebar
#[component]
pub fn VerticalProgressBar(
    song: Song,
    song_idx: usize,
    current_song_idx: Option<usize>,
    transport_positions: HashMap<String, f64>,
) -> Element {
    if current_song_idx != Some(song_idx) || song.sections.is_empty() {
        return rsx! {
            div { class: "w-0" }
        };
    }

    let project_name = get_project_name(&song).unwrap_or_else(|| "default".to_string());
    let position = transport_positions.get(&project_name).copied().unwrap_or(0.0);
    let progress = calculate_song_progress(&song, position);

    rsx! {
        div {
            class: "relative w-1 flex-shrink-0 mr-2",
            style: format!(
                "height: calc(3rem + {} * 3rem + {} * 0.25rem);",
                song.sections.len(),
                song.sections.len()
            ),
            // Background (unfilled)
            div {
                class: "absolute inset-0 rounded-full",
                style: format!(
                    "background-color: {}; opacity: 0.3;",
                    get_song_color_muted(&song)
                ),
            }
            // Filled portion
            div {
                class: "absolute top-0 left-0 right-0 rounded-full transition-all duration-300 ease-in-out",
                style: format!(
                    "height: {}%; background-color: {}; opacity: 0.8;",
                    progress,
                    get_song_color_bright(&song)
                ),
            }
        }
    }
}

/// Song item component for sidebar
#[component]
pub fn SongItem(
    song: Song,
    index: usize,
    setlist: Setlist,
    current_song_index: Signal<Option<usize>>,
    current_section_index: Signal<Option<usize>>,
    mut transport_positions: Signal<HashMap<String, f64>>,
    mut song_positions: Signal<HashMap<String, f64>>,
    is_playing: Signal<bool>,
) -> Element {
    let is_expanded = current_song_index() == Some(index);
    let setlist_clone = setlist.clone();

    rsx! {
        div {
            key: "{index}",
            class: "relative",
            div {
                class: "flex",
                // Vertical progress bar
                VerticalProgressBar {
                    song: song.clone(),
                    song_idx: index,
                    current_song_idx: current_song_index(),
                    transport_positions: transport_positions.read().clone(),
                }
                div {
                    class: "flex-1 space-y-1",
                    // Song progress bar
                    CompactProgressBar {
                        label: song.name.clone(),
                        progress: get_song_progress_for_sidebar(
                            &song,
                            index,
                            current_song_index(),
                            &transport_positions.read(),
                        ),
                        bright_color: get_song_color_bright(&song),
                        muted_color: get_song_color_muted(&song),
                        is_selected: is_expanded,
                        is_inactive: !is_expanded,
                        always_black_bg: true,
                        on_click: move |_| {
                            if !is_playing() {
                                transport_positions.with_mut(|positions| {
                                    reset_previous_song_position(
                                        &setlist_clone,
                                        current_song_index(),
                                        positions,
                                    );
                                });
                            }
                            current_song_index.set(Some(index));
                            current_section_index.set(Some(0));
                        },
                    }
                    // Sections (only if expanded)
                    if is_expanded {
                        for (section_idx, section) in song.sections.iter().enumerate() {
                            div {
                                key: "{index}-{section_idx}",
                                class: "pl-4",
                                CompactProgressBar {
                                    label: section.display_name(),
                                    progress: get_section_progress_for_sidebar(
                                        section,
                                        &song,
                                        index,
                                        current_song_index(),
                                        &transport_positions.read(),
                                    ),
                                    bright_color: get_section_color_bright(section),
                                    muted_color: get_section_color_muted(section),
                                    is_selected: current_section_index() == Some(section_idx),
                                    is_inactive: !is_expanded,
                                    on_click: {
                                        let song_clone = song.clone();
                                        move |_| {
                                            current_section_index.set(Some(section_idx));
                                            if !is_playing() {
                                                transport_positions.with_mut(|transport_pos| {
                                                    song_positions.with_mut(|song_pos| {
                                                        update_position_on_section_click(
                                                            &song_clone,
                                                            index,
                                                            section_idx,
                                                            transport_pos,
                                                            song_pos,
                                                        );
                                                    });
                                                });
                                            }
                                        }
                                    },
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

