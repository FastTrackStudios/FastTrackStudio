//! Actions for Dynamic-Template REAPER Extension
//!
//! Provides actions for organizing selected items into hierarchical track structures.

use crate::infrastructure::action_registry::{ActionDef, ActionSection};
use daw::tracks::Track;
use dynamic_template::{OrganizeIntoTracks, default_config};
use reaper_high::Reaper;
use reaper_low::raw::{MediaItem, MediaTrack};
use std::collections::HashSet;
use std::ffi::CString;
use tracing::{info, warn};

/// Item info including the media item pointer, name, and original track
struct ItemInfo {
    media_item: *mut MediaItem,
    name: String,
    original_track: *mut MediaTrack,
}

/// Get names of all selected items in the current project
fn get_selected_item_info() -> Vec<ItemInfo> {
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut items = Vec::new();

    unsafe {
        let num_selected = low.CountSelectedMediaItems(std::ptr::null_mut());

        for i in 0..num_selected {
            let item = low.GetSelectedMediaItem(std::ptr::null_mut(), i);
            if item.is_null() {
                continue;
            }

            // Get the track this item is on (so we can delete it later)
            let original_track = low.GetMediaItem_Track(item);

            // Get the active take to retrieve the item name
            let take = low.GetActiveTake(item);
            let name = if take.is_null() {
                format!("Item {}", i)
            } else {
                let take_name_ptr = low.GetTakeName(take);
                if take_name_ptr.is_null() {
                    format!("Item {}", i)
                } else {
                    std::ffi::CStr::from_ptr(take_name_ptr)
                        .to_string_lossy()
                        .into_owned()
                }
            };

            items.push(ItemInfo {
                media_item: item,
                name,
                original_track,
            });
        }
    }

    items
}

/// Get names of ALL items in the current project (not just selected)
fn get_all_item_info() -> Vec<ItemInfo> {
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut items = Vec::new();

    unsafe {
        let num_items = low.CountMediaItems(std::ptr::null_mut());

        for i in 0..num_items {
            let item = low.GetMediaItem(std::ptr::null_mut(), i);
            if item.is_null() {
                continue;
            }

            // Get the track this item is on (so we can delete it later)
            let original_track = low.GetMediaItem_Track(item);

            // Get the active take to retrieve the item name
            let take = low.GetActiveTake(item);
            let name = if take.is_null() {
                format!("Item {}", i)
            } else {
                let take_name_ptr = low.GetTakeName(take);
                if take_name_ptr.is_null() {
                    format!("Item {}", i)
                } else {
                    std::ffi::CStr::from_ptr(take_name_ptr)
                        .to_string_lossy()
                        .into_owned()
                }
            };

            items.push(ItemInfo {
                media_item: item,
                name,
                original_track,
            });
        }
    }

    items
}

/// Delete empty tracks from the given set of track pointers
/// Only deletes tracks that have no items on them
fn delete_empty_tracks(tracks_to_check: &HashSet<*mut MediaTrack>) {
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut deleted_count = 0;

    unsafe {
        // We need to iterate in reverse order to avoid index shifting issues
        // First collect valid tracks to delete
        let mut tracks_to_delete: Vec<*mut MediaTrack> = tracks_to_check
            .iter()
            .filter(|&&track| {
                if track.is_null() {
                    return false;
                }
                // Check if track is empty (no items)
                let item_count = low.CountTrackMediaItems(track);
                item_count == 0
            })
            .copied()
            .collect();

        // Delete tracks (order doesn't matter since we're using pointers)
        for track in tracks_to_delete {
            low.DeleteTrack(track);
            deleted_count += 1;
        }
    }

    if deleted_count > 0 {
        reaper.show_console_msg(
            format!("Deleted {} empty original track(s)\n", deleted_count).as_str()
        );
    }
}

/// Core sorting logic shared by both selected and all items handlers
fn sort_items_core(items: Vec<ItemInfo>, action_name: &str, undo_name: &str) {
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    if items.is_empty() {
        let msg = "No items found to sort.\n";
        reaper.show_console_msg(msg);
        warn!("No items to sort");
        return;
    }

    reaper.show_console_msg(
        format!("Found {} item(s):\n", items.len()).as_str()
    );

    // Log item names (limit to first 20 for large projects)
    let display_count = items.len().min(20);
    for (i, item) in items.iter().take(display_count).enumerate() {
        reaper.show_console_msg(format!("  {}. {}\n", i + 1, item.name).as_str());
    }
    if items.len() > 20 {
        reaper.show_console_msg(format!("  ... and {} more\n", items.len() - 20).as_str());
    }
    reaper.show_console_msg("\n");

    // Collect original tracks before sorting (so we can delete them after)
    let original_tracks: HashSet<*mut MediaTrack> = items
        .iter()
        .map(|item| item.original_track)
        .filter(|track| !track.is_null())
        .collect();

    // Extract just the names for sorting
    let item_names: Vec<String> = items.iter()
        .map(|item| item.name.clone())
        .collect();

    // Use dynamic-template to organize items
    let config = default_config();
    match item_names.organize_into_tracks(&config, None) {
        Ok(tracks) => {
            reaper.show_console_msg("Organized structure:\n");

            // Display the track structure
            let structure_output = format_track_structure(&tracks);
            reaper.show_console_msg(structure_output.as_str());

            info!(track_count = tracks.len(), "Items organized into tracks");

            // Begin undo block
            unsafe {
                let undo_cstring = CString::new(undo_name).unwrap();
                low.Undo_BeginBlock();

                // Create the tracks in REAPER and move items
                create_tracks_in_reaper(&tracks, &items);

                // Delete original tracks that are now empty
                delete_empty_tracks(&original_tracks);

                low.Undo_EndBlock(undo_cstring.as_ptr(), 0);
            }
        }
        Err(e) => {
            let error_msg = format!("Error organizing items: {}\n", e);
            reaper.show_console_msg(error_msg.as_str());
            warn!(error = %e, "Failed to organize items");
        }
    }

    reaper.show_console_msg("\n=====================================\n\n");
}

/// Sort selected items into a template structure and create new tracks
fn sort_selected_items_handler() {
    info!("\n=== FTS / Dynamic-Template: Sort Selected Items ===\n");
    let reaper = Reaper::get();
    reaper.show_console_msg("\n=== Sort Selected Items into Template ===\n\n");

    let items = get_selected_item_info();

    if items.is_empty() {
        reaper.show_console_msg("No items selected. Please select some items to sort.\n");
        warn!("No items selected");
        return;
    }

    sort_items_core(items, "Sort Selected Items", "Sort selected items into template");
}

/// Sort ALL items in the project into a template structure
fn sort_all_items_handler() {
    info!("\n=== FTS / Dynamic-Template: Sort All Items ===\n");
    let reaper = Reaper::get();
    reaper.show_console_msg("\n=== Sort All Items into Template ===\n\n");

    let items = get_all_item_info();

    if items.is_empty() {
        reaper.show_console_msg("No items in project.\n");
        warn!("No items in project");
        return;
    }

    sort_items_core(items, "Sort All Items", "Sort all items into template");
}

/// Format track structure for display (flat list with depth indication)
fn format_track_structure(tracks: &[Track]) -> String {
    let mut output = String::new();
    let mut depth: i32 = 0;

    for track in tracks {
        let indent = "  ".repeat(depth.max(0) as usize);
        let track_type = if track.is_folder { "[Folder]" } else { "[Track]" };

        output.push_str(&format!("{}{} {}", indent, track_type, track.name.0));

        // Show items if any
        if !track.items.is_empty() {
            output.push_str(&format!(" ({} item(s))", track.items.len()));
        }
        output.push('\n');

        // Update depth based on folder_depth_change
        depth += track.folder_depth_change.to_reaper_value();
    }

    output
}

/// Create tracks in REAPER based on the organized structure
fn create_tracks_in_reaper(
    tracks: &[Track],
    items: &[ItemInfo],
) {
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    // Create a mapping from item name to MediaItem for quick lookup
    use std::collections::HashMap;
    let mut item_map: HashMap<&str, Vec<*mut MediaItem>> = HashMap::new();
    for item in items {
        item_map.entry(item.name.as_str()).or_default().push(item.media_item);
    }

    // Track the current insertion index (at end of current tracks)
    let mut track_index = unsafe { low.CountTracks(std::ptr::null_mut()) };
    let mut depth: i32 = 0;

    reaper.show_console_msg("\nCreating tracks in REAPER...\n");

    unsafe {
        for track in tracks {
            let indent = "  ".repeat(depth.max(0) as usize);

            // Insert a new track at the end
            low.InsertTrackAtIndex(track_index, true);

            // Get the newly created track
            let reaper_track = low.GetTrack(std::ptr::null_mut(), track_index);
            if reaper_track.is_null() {
                track_index += 1;
                depth += track.folder_depth_change.to_reaper_value();
                continue;
            }

            // Set track name
            let track_name = CString::new(track.name.0.as_str()).unwrap_or_default();
            let param_name = CString::new("P_NAME").unwrap();
            low.GetSetMediaTrackInfo_String(
                reaper_track,
                param_name.as_ptr(),
                track_name.as_ptr() as *mut _,
                true,
            );

            // Set folder depth using the track's folder_depth_change value
            let param_folder = CString::new("I_FOLDERDEPTH").unwrap();
            let folder_value = track.folder_depth_change.to_reaper_value() as f64;
            low.SetMediaTrackInfo_Value(
                reaper_track,
                param_folder.as_ptr(),
                folder_value,
            );

            // Move items to this track
            for item in &track.items {
                // Look up the MediaItem by name
                if let Some(media_items) = item_map.get(item.name.as_str()) {
                    for &media_item in media_items {
                        // Move item to this track
                        low.MoveMediaItemToTrack(media_item, reaper_track);
                    }
                }
            }

            reaper.show_console_msg(
                format!("{}Created track: {}\n", indent, track.name.0).as_str()
            );

            // Update depth for next track
            depth += track.folder_depth_change.to_reaper_value();
            track_index += 1;
        }
    }

    reaper.show_console_msg("Done!\n");
}

/// Import files from a file dialog and sort them into template
fn import_and_sort_handler() {
    info!("\n=== FTS / Dynamic-Template: Import and Sort ===\n");
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    reaper.show_console_msg("\n=== Import and Sort into Template ===\n\n");

    // Use REAPER's native file browser to select files
    // GetUserFileNameForRead returns a path to a selected file
    unsafe {
        // Create a buffer for the file path
        let mut file_buf = vec![0u8; 4096];
        let title = CString::new("Select audio files to import and sort").unwrap();
        let filter = CString::new("Audio Files\0*.wav;*.mp3;*.aiff;*.flac;*.ogg;*.m4a\0All Files\0*.*\0\0").unwrap();

        // Use GetUserFileNameForRead which allows multiple selection
        let result = low.GetUserFileNameForRead(
            file_buf.as_mut_ptr() as *mut i8,
            title.as_ptr(),
            std::ptr::null(),
        );

        if !result {
            reaper.show_console_msg("Import cancelled.\n");
            return;
        }

        // Parse the selected file path
        let file_path = std::ffi::CStr::from_ptr(file_buf.as_ptr() as *const i8)
            .to_string_lossy()
            .into_owned();

        if file_path.is_empty() {
            reaper.show_console_msg("No file selected.\n");
            return;
        }

        reaper.show_console_msg(format!("Importing: {}\n", file_path).as_str());

        // Begin undo block
        let undo_name = CString::new("Import and sort files into template").unwrap();
        low.Undo_BeginBlock();

        // Insert the media file - this creates a new item
        // Use InsertMedia with mode 0 (current track) or 1 (new track)
        let file_cstr = CString::new(file_path.clone()).unwrap();

        // Insert to a new track (mode 1)
        // The flags parameter: 0 = default behavior
        low.InsertMedia(file_cstr.as_ptr(), 1);

        // After import, get the newly created item(s) and sort
        // The imported item should now be selected, so get selected items
        let items = get_selected_item_info();

        if items.is_empty() {
            reaper.show_console_msg("No items were imported.\n");
            low.Undo_EndBlock(undo_name.as_ptr(), 0);
            return;
        }

        reaper.show_console_msg(format!("Imported {} item(s), now sorting...\n\n", items.len()).as_str());

        // Collect original tracks
        let original_tracks: HashSet<*mut MediaTrack> = items
            .iter()
            .map(|item| item.original_track)
            .filter(|track| !track.is_null())
            .collect();

        // Extract names and sort
        let item_names: Vec<String> = items.iter()
            .map(|item| item.name.clone())
            .collect();

        let config = default_config();
        match item_names.organize_into_tracks(&config, None) {
            Ok(tracks) => {
                reaper.show_console_msg("Organized structure:\n");
                let structure_output = format_track_structure(&tracks);
                reaper.show_console_msg(structure_output.as_str());

                // Create the tracks in REAPER and move items
                create_tracks_in_reaper(&tracks, &items);

                // Delete original tracks that are now empty
                delete_empty_tracks(&original_tracks);
            }
            Err(e) => {
                let error_msg = format!("Error organizing items: {}\n", e);
                reaper.show_console_msg(error_msg.as_str());
                warn!(error = %e, "Failed to organize items");
            }
        }

        low.Undo_EndBlock(undo_name.as_ptr(), 0);
    }

    reaper.show_console_msg("\n=====================================\n\n");
}

/// Get Dynamic-Template actions for registration with main action batch
pub fn actions() -> Vec<ActionDef> {
    vec![
        ActionDef {
            command_id: "FTS_DT_SORT_SELECTED",
            display_name: "Sort Selected Items into Template".to_string(),
            handler: sort_selected_items_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_DT_SORT_ALL",
            display_name: "Sort All Items into Template".to_string(),
            handler: sort_all_items_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_DT_IMPORT_AND_SORT",
            display_name: "Import and Sort into Template".to_string(),
            handler: import_and_sort_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
    ]
}

/// Register Dynamic-Template actions (called from main registration)
pub fn register_dynamic_template_actions() {
    // Note: Actions should be registered via the main batch in actions.rs
    // This function exists for compatibility but the actions() function
    // should be used to add to all_actions instead
    crate::infrastructure::action_registry::register_actions(&actions(), "Dynamic-Template");
}
