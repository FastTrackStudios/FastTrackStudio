//! Hook providing setlist action callbacks
//!
//! This hook wraps the SetlistService.execute() method, providing
//! convenient callbacks for UI components to trigger setlist commands.
//! Uses ROAM's async interface for full compatibility with all service implementations.

use crate::context::setlist::use_setlist_service;
use dioxus::prelude::*;
use fts::setlist::service::SetlistCommand;
use fts::setlist::{ACTIVE_INDICES, SETLIST_STRUCTURE};

/// Collection of setlist action callbacks
#[derive(Clone)]
pub struct SetlistActions {
    /// Navigate to a specific song by index
    pub go_to_song: Callback<usize>,
    /// Navigate to a specific section by index
    pub go_to_section: Callback<usize>,
    /// Navigate to the next song
    pub next_song: Callback<()>,
    /// Navigate to the previous song
    pub prev_song: Callback<()>,
    /// Navigate to the next section
    pub next_section: Callback<()>,
    /// Navigate to the previous section
    pub prev_section: Callback<()>,
    /// Smart advance: next section, or next song if at last section
    pub smart_next: Callback<()>,
    /// Smart back: previous section, or previous song (last section) if at first section
    pub smart_prev: Callback<()>,
    /// Seek to a specific time position (seconds)
    pub seek_to: Callback<f64>,
    /// Toggle loop for the current song
    pub toggle_song_loop: Callback<()>,
    /// Toggle loop for the current section
    pub toggle_section_loop: Callback<()>,
    /// Toggle playback (play/pause)
    pub toggle_playback: Callback<()>,
}

/// Hook that provides setlist action callbacks
///
/// Uses the setlist service from context to execute commands via ROAM.
/// Commands are executed asynchronously using `spawn()`.
///
/// # Example
/// ```ignore
/// let actions = use_setlist_actions();
///
/// rsx! {
///     button {
///         onclick: move |_| actions.next_song.call(()),
///         "Next Song"
///     }
/// }
/// ```
pub fn use_setlist_actions() -> SetlistActions {
    let ctx = use_setlist_service();

    SetlistActions {
        go_to_song: {
            let client = ctx.client.clone();
            Callback::new(move |index: usize| {
                let client = client.clone();
                spawn(async move {
                    client.execute(SetlistCommand::GoToSong { index }).await;
                });
            })
        },
        go_to_section: {
            let client = ctx.client.clone();
            Callback::new(move |index: usize| {
                let client = client.clone();
                spawn(async move {
                    client.execute(SetlistCommand::GoToSection { index }).await;
                });
            })
        },
        next_song: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                spawn(async move {
                    client.execute(SetlistCommand::NextSong).await;
                });
            })
        },
        prev_song: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                spawn(async move {
                    client.execute(SetlistCommand::PreviousSong).await;
                });
            })
        },
        next_section: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                spawn(async move {
                    client.execute(SetlistCommand::NextSection).await;
                });
            })
        },
        prev_section: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                spawn(async move {
                    client.execute(SetlistCommand::PreviousSection).await;
                });
            })
        },
        smart_next: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                // Check if we're at the last section of the current song
                let indices = *ACTIVE_INDICES.read();
                let (song_idx, section_idx, _) = indices;

                let is_last_section = if let (Some(song_idx), Some(section_idx)) = (song_idx, section_idx) {
                    SETLIST_STRUCTURE.read().as_ref().map_or(false, |setlist| {
                        setlist.songs.get(song_idx).map_or(false, |song| {
                            section_idx >= song.sections.len().saturating_sub(1)
                        })
                    })
                } else {
                    false
                };

                spawn(async move {
                    if is_last_section {
                        // Go to next song (first section)
                        client.execute(SetlistCommand::NextSong).await;
                    } else {
                        // Go to next section
                        client.execute(SetlistCommand::NextSection).await;
                    }
                });
            })
        },
        smart_prev: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                // Check if we're at the first section of the current song
                let indices = *ACTIVE_INDICES.read();
                let (song_idx, section_idx, _) = indices;

                let is_first_section = section_idx.map_or(true, |idx| idx == 0);
                let has_prev_song = song_idx.map_or(false, |idx| idx > 0);

                spawn(async move {
                    if is_first_section && has_prev_song {
                        // Go to previous song (will go to first section, then we could go to last)
                        // For now, just go to previous song
                        client.execute(SetlistCommand::PreviousSong).await;
                    } else {
                        // Go to previous section
                        client.execute(SetlistCommand::PreviousSection).await;
                    }
                });
            })
        },
        seek_to: {
            let client = ctx.client.clone();
            Callback::new(move |seconds: f64| {
                let client = client.clone();
                spawn(async move {
                    client.execute(SetlistCommand::SeekTo { seconds }).await;
                });
            })
        },
        toggle_song_loop: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                spawn(async move {
                    client.execute(SetlistCommand::ToggleSongLoop).await;
                });
            })
        },
        toggle_section_loop: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                spawn(async move {
                    client.execute(SetlistCommand::ToggleSectionLoop).await;
                });
            })
        },
        toggle_playback: {
            let client = ctx.client.clone();
            Callback::new(move |_: ()| {
                let client = client.clone();
                spawn(async move {
                    client.execute(SetlistCommand::TogglePlayback).await;
                });
            })
        },
    }
}
