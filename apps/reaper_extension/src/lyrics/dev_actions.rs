//! Dev actions for debugging lyrics and MIDI

use crate::infrastructure::action_registry::{ActionDef, register_actions};
use reaper_high::{Item, Reaper};
use reaper_medium::{MediaItemTake, PositionInQuarterNotes, ProjectContext, ReaperStringArg};
use tracing::{info, warn};

/// Log selected MIDI items with their notes
fn log_selected_midi_items_handler() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let current_project = reaper.current_project();

    reaper.show_console_msg("\n=== FastTrackStudio: Selected MIDI Items ===\n");
    info!("Logging selected MIDI items");

    // Get selected items
    let selected_items: Vec<Item> = current_project.selected_items().collect();

    if selected_items.is_empty() {
        reaper.show_console_msg("No items selected.\n");
        warn!("No items selected");
        return;
    }

    info!(items_count = selected_items.len(), "Found selected items");

    for (item_idx, item) in selected_items.iter().enumerate() {
        // Get item name from notes (P_NOTES)
        let item_name = unsafe {
            let notes_key = std::ffi::CString::new("P_NOTES").expect("CString::new failed");
            let mut buffer = vec![0u8; 4096];
            let buffer_ptr = buffer.as_mut_ptr() as *mut std::os::raw::c_char;

            let success = medium_reaper.low().GetSetMediaItemInfo_String(
                item.raw().as_ptr(),
                notes_key.as_ptr(),
                buffer_ptr,
                false, // setNewValue = false (get value)
            );

            if success {
                let c_str = std::ffi::CStr::from_ptr(buffer_ptr);
                let notes = c_str.to_string_lossy().to_string();
                if !notes.trim().is_empty() {
                    notes.chars().take(50).collect::<String>()
                } else {
                    format!("Item {}", item_idx + 1)
                }
            } else {
                format!("Item {}", item_idx + 1)
            }
        };

        let position = item.position().get();
        let length = item.length().get();

        info!(
            item_index = item_idx,
            item_name = %item_name,
            position = position,
            length = length,
            "Processing selected item"
        );

        let msg = format!(
            "\n--- Item {}: {} ---\n  Position: {:.3}s\n  Length: {:.3}s\n",
            item_idx + 1,
            item_name,
            position,
            length
        );
        reaper.show_console_msg(ReaperStringArg::from(msg.as_str()));

        // Get active take
        let take = match item.active_take() {
            Some(take) => take,
            None => {
                let msg = "  No active take\n";
                reaper.show_console_msg(ReaperStringArg::from(msg));
                warn!(item_index = item_idx, "Item has no active take");
                continue;
            }
        };

        let take_name = take.name();
        let msg = format!("  Take: {}\n", take_name);
        reaper.show_console_msg(ReaperStringArg::from(msg.as_str()));

        // Check if take is MIDI
        let is_midi = unsafe { medium_reaper.low().TakeIsMIDI(take.raw().as_ptr()) };

        if !is_midi {
            let msg = "  Not a MIDI take\n";
            reaper.show_console_msg(ReaperStringArg::from(msg));
            warn!(item_index = item_idx, "Take is not MIDI");
            continue;
        }

        // Count MIDI events
        // MIDI_CountEvts(take, notecntOut, ccevtcntOut, textsyxevtcntOut)
        let mut note_count: i32 = 0;
        let mut cc_count: i32 = 0;
        let mut text_sysex_count: i32 = 0;

        unsafe {
            medium_reaper.low().MIDI_CountEvts(
                take.raw().as_ptr(),
                &mut note_count,
                &mut cc_count,
                &mut text_sysex_count,
            );
        }

        info!(
            item_index = item_idx,
            note_count = note_count,
            cc_count = cc_count,
            text_sysex_count = text_sysex_count,
            "MIDI event counts"
        );

        let msg = format!(
            "  MIDI Notes: {}, CC Events: {}, Text/Sysex: {}\n",
            note_count, cc_count, text_sysex_count
        );
        reaper.show_console_msg(ReaperStringArg::from(msg.as_str()));

        // Get all notes
        if note_count > 0 {
            reaper.show_console_msg("  Notes:\n");

            for note_idx in 0..(note_count as usize) {
                let mut selected: bool = false;
                let mut muted: bool = false;
                let mut start_ppq: f64 = 0.0;
                let mut end_ppq: f64 = 0.0;
                let mut channel: i32 = 0;
                let mut pitch: i32 = 0;
                let mut velocity: i32 = 0;

                let success = unsafe {
                    medium_reaper.low().MIDI_GetNote(
                        take.raw().as_ptr(),
                        note_idx as i32,
                        &mut selected,
                        &mut muted,
                        &mut start_ppq,
                        &mut end_ppq,
                        &mut channel,
                        &mut pitch,
                        &mut velocity,
                    )
                };

                if success {
                    // Convert PPQ positions to project time
                    let start_time = unsafe {
                        medium_reaper
                            .low()
                            .MIDI_GetProjTimeFromPPQPos(take.raw().as_ptr(), start_ppq)
                    };
                    let end_time = unsafe {
                        medium_reaper
                            .low()
                            .MIDI_GetProjTimeFromPPQPos(take.raw().as_ptr(), end_ppq)
                    };

                    // Convert PPQ to quarter notes, then to measure/beat/tick
                    let start_qn = unsafe {
                        medium_reaper
                            .low()
                            .MIDI_GetProjQNFromPPQPos(take.raw().as_ptr(), start_ppq)
                    };
                    let end_qn = unsafe {
                        medium_reaper
                            .low()
                            .MIDI_GetProjQNFromPPQPos(take.raw().as_ptr(), end_ppq)
                    };

                    // Get musical position (measure.beat.tick)
                    let start_musical = unsafe {
                        ppq_to_musical_position(
                            &medium_reaper,
                            current_project.context(),
                            take.raw(),
                            start_ppq,
                            start_qn,
                        )
                    };
                    let end_musical = unsafe {
                        ppq_to_musical_position(
                            &medium_reaper,
                            current_project.context(),
                            take.raw(),
                            end_ppq,
                            end_qn,
                        )
                    };

                    // Convert pitch to note name
                    let note_name = pitch_to_note_name(pitch);

                    let msg = format!(
                        "    Note {}: {} (ch {}, vel {}) - {:.3}s ({}) to {:.3}s ({}) (PPQ: {:.1} to {:.1}){}\n",
                        note_idx + 1,
                        note_name,
                        channel + 1,
                        velocity,
                        start_time,
                        start_musical,
                        end_time,
                        end_musical,
                        start_ppq,
                        end_ppq,
                        if muted { " [MUTED]" } else { "" }
                    );
                    reaper.show_console_msg(ReaperStringArg::from(msg.as_str()));

                    info!(
                        item_index = item_idx,
                        note_index = note_idx,
                        pitch = pitch,
                        note_name = %note_name,
                        channel = channel,
                        velocity = velocity,
                        start_time = start_time,
                        end_time = end_time,
                        start_ppq = start_ppq,
                        end_ppq = end_ppq,
                        selected = selected,
                        muted = muted,
                        "MIDI note"
                    );
                } else {
                    warn!(
                        item_index = item_idx,
                        note_index = note_idx,
                        "Failed to get MIDI note"
                    );
                }
            }
        }
    }

    reaper.show_console_msg("\n=== End of MIDI Items Log ===\n\n");
    info!("Finished logging selected MIDI items");
}

/// Convert MIDI pitch to note name (e.g., 60 -> "C4")
fn pitch_to_note_name(pitch: i32) -> String {
    let note_names = [
        "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B",
    ];
    let octave = (pitch / 12) - 1;
    let note = pitch % 12;
    format!("{}{}", note_names[note as usize], octave)
}

/// Convert PPQ position to musical position (measure.beat.tick)
/// Returns format like "1.1.000" (measure.beat.tick)
unsafe fn ppq_to_musical_position(
    medium_reaper: &reaper_medium::Reaper,
    project: ProjectContext,
    take: MediaItemTake,
    ppq: f64,
    qn: f64,
) -> String {
    // Get the measure that contains this quarter note position
    let measure_result =
        medium_reaper.time_map_qn_to_measures(project, PositionInQuarterNotes::new_panic(qn));

    let measure_idx = measure_result.measure_index;
    let measure_start_qn = measure_result.start.get();
    let measure_end_qn = measure_result.end.get();

    // Get time signature for this measure
    let measure_info = medium_reaper.time_map_get_measure_info(project, measure_idx);
    let time_sig_num = measure_info.time_signature.numerator.get() as f64;
    let time_sig_denom = measure_info.time_signature.denominator.get() as f64;

    // Calculate beats per measure (e.g., 4/4 = 4 beats, 3/4 = 3 beats)
    let beats_per_measure = time_sig_num;

    // Calculate quarter notes per beat (usually 1.0, but can vary with time signature)
    let qn_per_beat = 4.0 / time_sig_denom;

    // Calculate quarter notes into the measure
    let qn_into_measure = qn - measure_start_qn;

    // Calculate beat number (1-based)
    let beat_number = (qn_into_measure / qn_per_beat).floor() + 1.0;

    // Calculate tick (sub-beat position, 0-959 for 960 ticks per quarter note)
    // First, get PPQ position of measure start
    let measure_start_ppq = medium_reaper
        .low()
        .MIDI_GetPPQPosFromProjQN(take.as_ptr(), measure_start_qn);
    let ppq_into_measure = ppq - measure_start_ppq;

    // Get PPQ resolution (usually 960 PPQ per quarter note)
    // We'll calculate it from the beat position
    let beat_start_ppq = medium_reaper.low().MIDI_GetPPQPosFromProjQN(
        take.as_ptr(),
        measure_start_qn + ((beat_number - 1.0) * qn_per_beat),
    );
    let ppq_per_beat = medium_reaper.low().MIDI_GetPPQPosFromProjQN(
        take.as_ptr(),
        measure_start_qn + (beat_number * qn_per_beat),
    ) - beat_start_ppq;

    // Calculate tick (0-959 per quarter note, or scaled to beat)
    let ppq_into_beat = ppq - beat_start_ppq;
    let ticks_per_beat = ppq_per_beat as i32;
    let tick = (ppq_into_beat as i32) % ticks_per_beat.max(1);

    format!("{}.{}.{:03}", measure_idx + 1, beat_number as i32, tick)
}

/// Assign syllables to selected MIDI notes from input text
fn assign_syllables_to_notes_handler() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let current_project = reaper.current_project();

    // Get user input for syllables (comma or newline separated)
    let captions_csv = "Enter syllables (comma or newline separated),separator=|,extrawidth=500";
    let initial_value = "";

    let user_input_result = medium_reaper.get_user_inputs(
        "FastTrackStudio: Assign Syllables to MIDI Notes",
        1,
        captions_csv,
        initial_value,
        4096,
    );

    let syllables_text = match user_input_result {
        Some(input) => {
            let text = input.to_str().trim().to_string();
            if text.is_empty() {
                reaper.show_console_msg("FastTrackStudio: No syllables provided. Cancelled.\n");
                info!("User cancelled or provided empty syllables");
                return;
            }
            text
        }
        None => {
            reaper.show_console_msg("FastTrackStudio: User cancelled input dialog.\n");
            info!("User cancelled syllables input dialog");
            return;
        }
    };

    // Parse syllables (split by comma or newline)
    let syllables: Vec<String> = syllables_text
        .split(&[',', '\n', '\r'][..])
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    if syllables.is_empty() {
        reaper.show_console_msg("FastTrackStudio: No valid syllables found.\n");
        warn!("No valid syllables parsed");
        return;
    }

    info!(syllables_count = syllables.len(), "Parsed syllables");

    // Get selected items
    let selected_items: Vec<Item> = current_project.selected_items().collect();

    if selected_items.is_empty() {
        reaper.show_console_msg("FastTrackStudio: No items selected.\n");
        warn!("No items selected");
        return;
    }

    // Process each selected item
    let mut total_notes_assigned = 0;

    for item in selected_items.iter() {
        let take = match item.active_take() {
            Some(take) => take,
            None => continue,
        };

        // Check if take is MIDI
        let is_midi = unsafe { medium_reaper.low().TakeIsMIDI(take.raw().as_ptr()) };

        if !is_midi {
            continue;
        }

        // Count notes
        let mut note_count: i32 = 0;
        let mut _cc_count: i32 = 0;
        let mut _text_sysex_count: i32 = 0;

        unsafe {
            medium_reaper.low().MIDI_CountEvts(
                take.raw().as_ptr(),
                &mut note_count,
                &mut _cc_count,
                &mut _text_sysex_count,
            );
        }

        if note_count == 0 {
            continue;
        }

        // Get selected notes and assign syllables
        let mut syllable_idx = 0;

        for note_idx in 0..(note_count as usize) {
            let mut selected: bool = false;
            let mut _muted: bool = false;
            let mut start_ppq: f64 = 0.0;
            let mut _end_ppq: f64 = 0.0;
            let mut _channel: i32 = 0;
            let mut _pitch: i32 = 0;
            let mut _velocity: i32 = 0;

            let success = unsafe {
                medium_reaper.low().MIDI_GetNote(
                    take.raw().as_ptr(),
                    note_idx as i32,
                    &mut selected,
                    &mut _muted,
                    &mut start_ppq,
                    &mut _end_ppq,
                    &mut _channel,
                    &mut _pitch,
                    &mut _velocity,
                )
            };

            if success && selected && syllable_idx < syllables.len() {
                let syllable = &syllables[syllable_idx];

                // Insert text/sysex event at note start position
                // Type 1 = text event (lyrics)
                let syllable_cstring = match std::ffi::CString::new(syllable.as_str()) {
                    Ok(cstr) => cstr,
                    Err(_) => {
                        warn!(syllable = %syllable, "Failed to create CString for syllable");
                        continue;
                    }
                };

                unsafe {
                    medium_reaper.low().MIDI_InsertTextSysexEvt(
                        take.raw().as_ptr(),
                        false, // selected
                        false, // muted
                        start_ppq,
                        1, // type: 1 = text (lyrics)
                        syllable_cstring.as_ptr(),
                        syllable_cstring.as_bytes_with_nul().len() as i32,
                    );
                }

                syllable_idx += 1;
                total_notes_assigned += 1;

                info!(
                    note_index = note_idx,
                    syllable = %syllable,
                    ppq = start_ppq,
                    "Assigned syllable to note"
                );
            }
        }

        // Sort MIDI events after insertion
        unsafe {
            medium_reaper.low().MIDI_Sort(take.raw().as_ptr());
        }
    }

    let msg = format!(
        "FastTrackStudio: Assigned {} syllables to selected MIDI notes.\n",
        total_notes_assigned
    );
    reaper.show_console_msg(ReaperStringArg::from(msg.as_str()));
    info!(
        total_assigned = total_notes_assigned,
        "Finished assigning syllables"
    );
}

/// Register dev actions for lyrics/MIDI debugging
pub fn register_lyrics_dev_actions() {
    use crate::infrastructure::action_registry::ActionSection;

    let actions = vec![
        ActionDef {
            command_id: "FTS_DEV_LOG_SELECTED_MIDI_ITEMS",
            display_name: "Log Selected MIDI Items".to_string(),
            handler: log_selected_midi_items_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_DEV_ASSIGN_SYLLABLES_TO_NOTES",
            display_name: "Assign Syllables to Selected MIDI Notes".to_string(),
            handler: assign_syllables_to_notes_handler,
            appears_in_menu: true,
            section: ActionSection::MidiEditor, // Register to MIDI editor section
            ..Default::default()
        },
    ];

    register_actions(&actions, "Lyrics Dev");
}
