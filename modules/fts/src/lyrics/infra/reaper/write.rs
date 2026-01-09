//! Functions for writing lyrics data to REAPER

use crate::lyrics::{Lyrics, output::Slides};
use enumflags2::BitFlags;
use reaper_high::{Project, Reaper};
use reaper_medium::{MasterTrackBehavior, PositionInSeconds, ProjectPart, UndoScope};
use std::ffi::CString;
use tracing::{error, info, warn};

/// Create text items from lyrics input - one bar per slide
#[allow(unsafe_code)] // Required for low-level REAPER API
pub fn create_text_items_from_lyrics(lyrics_text: String) -> anyhow::Result<()> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let current_project = reaper.current_project();

    // Parse lyrics
    let lyrics = match crate::lyrics::parse_lyrics(&lyrics_text, "Untitled Song".to_string()) {
        Ok(lyrics) => lyrics,
        Err(e) => {
            let error_msg = format!("FastTrackStudio: Failed to parse lyrics: {}\n", e);
            reaper.show_console_msg(error_msg);
            error!(error = %e, "Failed to parse lyrics text");
            return Err(anyhow::anyhow!("Failed to parse lyrics: {}", e));
        }
    };

    // Generate slides
    let slides = Slides::generate(&lyrics);

    if slides.is_empty() {
        reaper.show_console_msg("FastTrackStudio: No slides generated from lyrics.\n");
        warn!("No slides generated from lyrics");
        return Err(anyhow::anyhow!("No slides generated from lyrics"));
    }

    // Get first selected track
    let selected_track = match medium_reaper.get_selected_track_2(
        current_project.context(),
        0,
        MasterTrackBehavior::ExcludeMasterTrack,
    ) {
        Some(track) => track,
        None => {
            reaper.show_console_msg("FastTrackStudio: Please select a destination track first.\n");
            warn!("No track selected");
            return Err(anyhow::anyhow!("No track selected"));
        }
    };

    // Get current playhead position or time selection start
    let start_position = {
        let time_selection = current_project.time_selection();
        if let Some(selection) = time_selection {
            selection.start.get()
        } else {
            // Use edit cursor position if no time selection
            current_project
                .edit_cursor_position()
                .unwrap_or(PositionInSeconds::ZERO)
                .get()
        }
    };

    // Get tempo and time signature at start position
    let tempo_bpm = {
        // Try to get tempo from tempo markers at start position
        let marker_count =
            medium_reaper.count_tempo_time_sig_markers(current_project.context()) as i32;
        let mut active_tempo: Option<f64> = None;

        for i in 0..marker_count {
            let mut timepos_out: f64 = 0.0;
            let mut _measurepos_out: i32 = 0;
            let mut _beatpos_out: f64 = 0.0;
            let mut bpm_out: f64 = 0.0;
            let mut _timesig_num_out: i32 = 0;
            let mut _timesig_denom_out: i32 = 0;
            let mut _lineartempo_out: bool = false;

            let success = unsafe {
                medium_reaper.low().GetTempoTimeSigMarker(
                    current_project.context().to_raw(),
                    i,
                    &mut timepos_out,
                    &mut _measurepos_out,
                    &mut _beatpos_out,
                    &mut bpm_out,
                    &mut _timesig_num_out,
                    &mut _timesig_denom_out,
                    &mut _lineartempo_out,
                )
            };

            if success && timepos_out <= start_position && bpm_out > 0.0 {
                active_tempo = Some(bpm_out);
            }
        }

        active_tempo.unwrap_or_else(|| medium_reaper.master_get_tempo().get())
    };

    // Get time signature at start position
    let (time_sig_num, time_sig_denom) = {
        let zero_pos = PositionInSeconds::new(start_position).unwrap_or(PositionInSeconds::ZERO);
        let beat_info = current_project.beat_info_at(zero_pos);
        (
            beat_info.time_signature.numerator.get() as i32,
            beat_info.time_signature.denominator.get() as i32,
        )
    };

    // Calculate one bar length in seconds
    // One bar = (60.0 / tempo_bpm) * time_sig_num beats
    let bar_length_seconds = (60.0 / tempo_bpm) * time_sig_num as f64;

    info!(
        slides_count = slides.len(),
        start_position = start_position,
        tempo_bpm = tempo_bpm,
        time_sig = format!("{}/{}", time_sig_num, time_sig_denom),
        bar_length_seconds = bar_length_seconds,
        "Creating text items from lyrics"
    );

    // Begin undo block
    medium_reaper.undo_begin_block_2(current_project.context());

    // Create text items - one bar per slide
    let mut current_position = start_position;
    let mut created_items = Vec::new();

    for (idx, slide) in slides.iter().enumerate() {
        // Create empty media item
        let item = unsafe {
            medium_reaper
                .low()
                .AddMediaItemToTrack(selected_track.as_ptr())
        };

        if item.is_null() {
            warn!(slide_index = idx, "Failed to create media item");
            continue;
        }

        // Set item position and length (one bar) using direct API calls
        unsafe {
            // Set position
            medium_reaper.low().SetMediaItemPosition(
                item,
                current_position,
                false, // refreshUI = false (we'll refresh at the end)
            );
            // Set length
            medium_reaper.low().SetMediaItemLength(
                item,
                bar_length_seconds,
                false, // refreshUI = false
            );
        }

        // Set item note (slide text) using GetSetMediaItemInfo_String
        let slide_text = slide.text.clone();
        unsafe {
            let notes_key = CString::new("P_NOTES").expect("CString::new failed");
            let text_cstring = CString::new(slide_text.as_str()).expect("CString::new failed");
            // GetSetMediaItemInfo_String requires a mutable buffer when setting
            let mut buffer = text_cstring.into_bytes_with_nul();
            let buffer_ptr = buffer.as_mut_ptr() as *mut std::os::raw::c_char;
            medium_reaper.low().GetSetMediaItemInfo_String(
                item,
                notes_key.as_ptr(),
                buffer_ptr,
                true, // setNewValue = true
            );
            // Keep buffer alive
            std::mem::forget(buffer);
        }

        created_items.push(item);
        current_position += bar_length_seconds;

        info!(
            slide_index = idx,
            position = current_position - bar_length_seconds,
            length = bar_length_seconds,
            text = %slide.text,
            "Created text item"
        );
    }

    // Select the created items
    unsafe {
        // Deselect all items first
        medium_reaper
            .low()
            .SelectAllMediaItems(current_project.context().to_raw(), false);

        // Select created items
        for item in &created_items {
            medium_reaper.low().SetMediaItemSelected(*item, true);
        }
    }

    // Refresh UI once at the end
    medium_reaper.update_arrange();

    // End undo block
    let undo_description = format!("Create {} text items from lyrics", slides.len());
    medium_reaper.undo_end_block_2(
        current_project.context(),
        undo_description,
        UndoScope::Scoped(BitFlags::from(ProjectPart::Items)),
    );

    let success_msg = format!(
        "FastTrackStudio: Created {} text items (one bar each) starting at {:.2}s\n",
        slides.len(),
        start_position
    );
    reaper.show_console_msg(success_msg);

    info!(
        items_created = created_items.len(),
        "Successfully created text items from lyrics"
    );
    Ok(())
}

/// Update lyrics in REAPER project for a specific song
/// This updates both the Song struct and the REAPER project's SLIDES track
#[allow(unsafe_code)]
pub fn update_lyrics_in_reaper(song_index: usize, lyrics: Lyrics) -> Result<(), String> {
    use crate::setlist::infra::traits::SetlistBuilder;
    use reaper_medium::ProjectRef;

    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    // Build setlist to find the song - use trait method
    let setlist = reaper
        .build_setlist_from_open_projects(None)
        .map_err(|e| format!("Failed to build setlist: {}", e))?;

    if song_index >= setlist.songs.len() {
        return Err(format!("Song index {} out of range", song_index));
    }

    let song = &setlist.songs[song_index];
    let project_name = song.project_name_from_metadata();

    // Find the project by name
    let mut target_project: Option<Project> = None;
    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            let tab_name = result
                .file_path
                .as_ref()
                .and_then(|p| p.as_std_path().file_stem())
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("Tab {}", i));

            let normalized_tab = tab_name.to_uppercase().replace('_', "-");
            let normalized_target = project_name.to_uppercase().replace('_', "-");

            if normalized_tab == normalized_target {
                target_project = Some(Project::new(result.project));
                break;
            }
        }
    }

    let project = target_project
        .ok_or_else(|| format!("Could not find project for song: {}", project_name))?;

    // Find LYRICS folder and SLIDES track
    let lyrics_folder_tracks = super::read::find_folder_tracks(project, "LYRICS")
        .ok_or_else(|| "LYRICS folder not found".to_string())?;

    let slides_track = lyrics_folder_tracks
        .iter()
        .find(|track| {
            track
                .name()
                .map(|n| n.to_str().eq_ignore_ascii_case("SLIDES"))
                .unwrap_or(false)
        })
        .ok_or_else(|| "SLIDES track not found in LYRICS folder".to_string())?;

    // Generate slides from updated lyrics
    let slides = Slides::generate(&lyrics);

    // Get tempo and time signature (use song start position)
    let start_position = song
        .start_marker
        .as_ref()
        .map(|m| m.position.time.to_seconds())
        .unwrap_or(0.0);

    let tempo_bpm = song
        .starting_tempo
        .unwrap_or_else(|| medium_reaper.master_get_tempo().get());
    let (time_sig_num, time_sig_denom) = song
        .starting_time_signature
        .map(|ts| (ts.numerator as i32, ts.denominator as i32))
        .unwrap_or((4, 4));

    let bar_length_seconds = (60.0 / tempo_bpm) * time_sig_num as f64;

    info!(
        song_index = song_index,
        song_name = %song.name,
        slides_count = slides.len(),
        start_position = start_position,
        "Updating lyrics in REAPER project"
    );

    // Begin undo block
    medium_reaper.undo_begin_block_2(project.context());

    // Remove all existing items from SLIDES track
    let existing_items: Vec<_> = slides_track.items().collect();
    let track_raw = slides_track
        .raw()
        .map_err(|e| format!("Failed to get track raw pointer: {}", e))?;
    for item in existing_items {
        unsafe {
            let item_raw = item.raw();
            medium_reaper
                .low()
                .DeleteTrackMediaItem(track_raw.as_ptr(), item_raw.as_ptr());
        }
    }

    // Create new text items from updated lyrics
    let mut current_position = start_position;
    let mut created_items = Vec::new();

    for (idx, slide) in slides.iter().enumerate() {
        // Create empty media item
        let item = unsafe { medium_reaper.low().AddMediaItemToTrack(track_raw.as_ptr()) };

        if item.is_null() {
            warn!(slide_index = idx, "Failed to create media item");
            continue;
        }

        // Set item position and length (one bar)
        unsafe {
            medium_reaper
                .low()
                .SetMediaItemPosition(item, current_position, false);
            medium_reaper
                .low()
                .SetMediaItemLength(item, bar_length_seconds, false);
        }

        // Set item note (slide text)
        let slide_text = slide.text.clone();
        unsafe {
            let notes_key = CString::new("P_NOTES").expect("CString::new failed");
            let text_cstring = CString::new(slide_text.as_str()).expect("CString::new failed");
            let mut buffer = text_cstring.into_bytes_with_nul();
            let buffer_ptr = buffer.as_mut_ptr() as *mut std::os::raw::c_char;
            medium_reaper.low().GetSetMediaItemInfo_String(
                item,
                notes_key.as_ptr(),
                buffer_ptr,
                true,
            );
            std::mem::forget(buffer);
        }

        created_items.push(item);
        current_position += bar_length_seconds;
    }

    // Refresh UI
    medium_reaper.update_arrange();

    // End undo block
    let undo_description = format!("Update lyrics for {}", song.name);
    medium_reaper.undo_end_block_2(
        project.context(),
        undo_description,
        UndoScope::Scoped(BitFlags::from(ProjectPart::Items)),
    );

    info!(
        song_index = song_index,
        items_created = created_items.len(),
        "Successfully updated lyrics in REAPER project"
    );

    // Note: The Song struct's lyrics field will be updated when the setlist is rebuilt
    // The setlist is rebuilt on the next poll, so the change will be reflected in the stream

    Ok(())
}
