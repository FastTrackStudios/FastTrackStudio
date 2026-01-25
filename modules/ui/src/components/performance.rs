//! Shared PerformanceView component
//!
//! This component renders the main setlist performance view with a sidebar
//! navigator and main content area. It uses the setlist service via hooks
//! for all actions and reads from global signals for state.

use daw::primitives::MusicalPosition;
use dioxus::prelude::*;
use fts::setlist::{ACTIVE_INDICES, IS_PLAYING, SETLIST_STRUCTURE};

use crate::components::layout::{MainContent, Sidebar};
use crate::hooks::{use_setlist_actions, use_setlist_subscription};

/// Shared performance view component for setlist navigation
///
/// This component:
/// - Subscribes to the setlist service to populate global signals
/// - Provides action callbacks via `use_setlist_actions()`
/// - Renders the Sidebar and MainContent components
///
/// Must be wrapped in a `SetlistServiceProvider` to work.
///
/// # Example
/// ```ignore
/// rsx! {
///     SetlistServiceProvider { service: Arc::new(MockSetlist::with_sample_data()),
///         PerformanceView {}
///     }
/// }
/// ```
#[component]
pub fn PerformanceView() -> Element {
    // Subscribe to service and populate global signals
    use_setlist_subscription();

    // Get action callbacks from service
    let actions = use_setlist_actions();

    // Use memos to reactively track global signals
    // Memos automatically subscribe to any signals read inside them
    let current_song_index_memo = use_memo(move || ACTIVE_INDICES.read().0);
    let current_section_index_memo = use_memo(move || ACTIVE_INDICES.read().1);
    let is_playing_memo = use_memo(move || *IS_PLAYING.read());

    // Create local signals that we'll sync with the memos
    // We need signals (not memos) because child components expect Signal<T> props
    let mut reactive_song_index = use_signal(|| current_song_index_memo());
    let mut reactive_section_index = use_signal(|| current_section_index_memo());
    let mut is_playing_signal = use_signal(|| is_playing_memo());

    // Sync local signals when memos change
    // The memos track the global signals, and these effects sync to local signals
    use_effect(move || {
        reactive_song_index.set(current_song_index_memo());
    });
    use_effect(move || {
        reactive_section_index.set(current_section_index_memo());
    });
    use_effect(move || {
        is_playing_signal.set(is_playing_memo());
    });

    // Wrap action callbacks to match expected signatures
    let on_song_click = actions.go_to_song.clone();

    let on_section_click = {
        let go_to_section = actions.go_to_section.clone();
        Callback::new(move |(_song_idx, section_idx): (usize, usize)| {
            go_to_section.call(section_idx);
        })
    };

    let on_main_section_click = {
        let go_to_section = actions.go_to_section.clone();
        Callback::new(move |idx: usize| {
            go_to_section.call(idx);
        })
    };

    // Callback to seek to a specific musical position (measure)
    // Looks up the measure's time from SETLIST_STRUCTURE and calls seek_to
    let on_seek_to_musical_position = {
        let seek_to = actions.seek_to.clone();
        Callback::new(move |(song_idx, musical_pos): (usize, MusicalPosition)| {
            // Look up the song's measure_positions to find the time for this measure
            if let Some(setlist) = SETLIST_STRUCTURE.read().as_ref() {
                if let Some(song) = setlist.songs.get(song_idx) {
                    // Find the measure position matching the musical position
                    // The musical_pos.measure is the measure number we want to jump to
                    if let Some(pos) = song.measure_positions.iter().find(|p| {
                        p.musical.measure == musical_pos.measure
                    }) {
                        let seconds = pos.time.to_seconds();
                        tracing::debug!(
                            "Seeking to measure {} at {:.2}s in song {}",
                            musical_pos.measure,
                            seconds,
                            song_idx
                        );
                        seek_to.call(seconds);
                    } else {
                        tracing::warn!(
                            "Could not find measure {} in song {} (has {} measures)",
                            musical_pos.measure,
                            song_idx,
                            song.measure_positions.len()
                        );
                    }
                }
            }
        })
    };

    // Create is_looping signal (static for now)
    let is_looping_signal = use_signal(|| false);

    rsx! {
        div {
            class: "flex-1 flex overflow-hidden",
            Sidebar {
                current_song_index: reactive_song_index,
                current_section_index: reactive_section_index,
                is_playing: is_playing_signal,
                on_song_click: on_song_click,
                on_section_click: on_section_click,
            }

            MainContent {
                current_song_index: reactive_song_index,
                current_section_index: reactive_section_index,
                on_section_click: Some(on_main_section_click),
                is_playing: is_playing_signal,
                is_looping: is_looping_signal,
                on_play_pause: actions.toggle_playback.clone(),
                on_loop_toggle: actions.toggle_song_loop,
                on_back: actions.prev_section,
                on_forward: actions.next_section,
                on_seek_to_musical_position: Some(on_seek_to_musical_position),
            }
        }
    }
}
