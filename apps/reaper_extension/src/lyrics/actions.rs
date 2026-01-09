//! Lyrics actions for REAPER extension

use crate::infrastructure::action_registry::{ActionDef, register_actions};
use fts::lyrics::infra::reaper::{create_text_items_from_lyrics, read_lyrics_from_reaper};
use reaper_high::Reaper;
use tracing::{error, info};

/// Handler for reading lyrics from REAPER
fn read_lyrics_from_reaper_handler() {
    let reaper = Reaper::get();

    info!("Reading lyrics from REAPER project");

    match read_lyrics_from_reaper() {
        Ok(lyrics_data) => {
            let success_msg = format!(
                "FastTrackStudio: Successfully read lyrics from REAPER\n  - {} slides found\n  - {} MIDI tracks found\n",
                lyrics_data.slides.len(),
                lyrics_data.midi_tracks.len()
            );
            reaper.show_console_msg(success_msg.as_str());

            info!(
                slides_count = lyrics_data.slides.len(),
                midi_tracks_count = lyrics_data.midi_tracks.len(),
                "Successfully read lyrics from REAPER"
            );

            // Log detailed information about what was found
            for (idx, slide) in lyrics_data.slides.iter().enumerate() {
                info!(
                    slide_index = idx,
                    position = slide.position,
                    length = slide.length,
                    text_preview = %slide.text.chars().take(50).collect::<String>(),
                    "Slide data"
                );
            }

            for track in &lyrics_data.midi_tracks {
                info!(
                    track_name = %track.name,
                    items_count = track.items.len(),
                    "MIDI track data"
                );

                for (item_idx, item) in track.items.iter().enumerate() {
                    info!(
                        track_name = %track.name,
                        item_index = item_idx,
                        position = item.position,
                        length = item.length,
                        events_count = item.events.len(),
                        "MIDI item data"
                    );
                }
            }
        }
        Err(e) => {
            let error_msg = format!(
                "FastTrackStudio: Failed to read lyrics from REAPER: {}\n",
                e
            );
            reaper.show_console_msg(error_msg.as_str());
            error!(error = %e, "Failed to read lyrics from REAPER");
        }
    }
}

/// Handler for creating text items from lyrics input
fn create_text_items_from_lyrics_handler() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    let captions_csv = "Enter lyrics text,separator=|,extrawidth=500";
    let initial_value = "";

    let user_input_result = medium_reaper.get_user_inputs(
        "FastTrackStudio: Create Text Items from Lyrics",
        1,
        captions_csv,
        initial_value,
        4096, // Max input length
    );

    let lyrics_text = match user_input_result {
        Some(input) => {
            let text = input.to_str().trim().to_string();
            if text.is_empty() {
                reaper.show_console_msg("FastTrackStudio: No lyrics text provided. Cancelled.\n");
                info!("User cancelled or provided empty lyrics text");
                return;
            }
            text
        }
        None => {
            reaper.show_console_msg("FastTrackStudio: User cancelled input dialog.\n");
            info!("User cancelled lyrics input dialog");
            return;
        }
    };

    match create_text_items_from_lyrics(lyrics_text) {
        Ok(_) => info!("Successfully created text items from lyrics"),
        Err(e) => error!("Failed to create text items from lyrics: {:#}", e),
    }
}

/// Register all lyrics actions
pub fn register_lyrics_actions() {
    let actions = vec![
        ActionDef {
            command_id: "FTS_LYRICS_CREATE_TEXT_ITEMS",
            display_name: "Create Text Items from Lyrics".to_string(),
            handler: create_text_items_from_lyrics_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LYRICS_READ_FROM_REAPER",
            display_name: "Read Lyrics from REAPER".to_string(),
            handler: read_lyrics_from_reaper_handler,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
    ];

    register_actions(&actions, "Lyrics");
}
