//! Auto Color REAPER Actions
//!
//! Provides REAPER actions for automatic track coloring based on
//! dynamic-template instrument classification.

use crate::infrastructure::action_registry::ActionDef;
use dynamic_template::colors::GradientDirection;
use reaper_high::Reaper;
use std::ffi::CString;
use tracing::info;

use super::state;

/// Get all auto-color actions for batch registration
pub fn actions() -> Vec<ActionDef> {
    vec![
        // === Track Coloring ===
        // Apply auto-color to all tracks
        ActionDef {
            command_id: "FTS_AUTOCOLOR_APPLY_ALL",
            display_name: "FTS-Auto Format: Auto-color all tracks".to_string(),
            handler: apply_all_handler,
            ..Default::default()
        },
        // Apply auto-color to selected tracks
        ActionDef {
            command_id: "FTS_AUTOCOLOR_APPLY_SELECTED",
            display_name: "FTS-Auto Format: Auto-color selected tracks".to_string(),
            handler: apply_selected_handler,
            ..Default::default()
        },
        // Toggle auto-color on project changes
        ActionDef {
            command_id: "FTS_AUTOCOLOR_TOGGLE",
            display_name: "FTS-Auto Format: Toggle auto-color enable".to_string(),
            handler: toggle_handler,
            toggle_state: Some(state::is_enabled),
            ..Default::default()
        },
        // Clear colors from all tracks
        ActionDef {
            command_id: "FTS_AUTOCOLOR_CLEAR_ALL",
            display_name: "FTS-Auto Format: Clear all track colors".to_string(),
            handler: clear_all_handler,
            ..Default::default()
        },
        // Clear colors from selected tracks
        ActionDef {
            command_id: "FTS_AUTOCOLOR_CLEAR_SELECTED",
            display_name: "FTS-Auto Format: Clear selected track colors".to_string(),
            handler: clear_selected_handler,
            ..Default::default()
        },
        // Re-classify and refresh colors
        ActionDef {
            command_id: "FTS_AUTOCOLOR_REFRESH",
            display_name: "FTS-Auto Format: Refresh track classification and colors".to_string(),
            handler: refresh_handler,
            ..Default::default()
        },
        // === Region Coloring ===
        // Apply auto-color to all regions
        ActionDef {
            command_id: "FTS_AUTOCOLOR_REGIONS_APPLY",
            display_name: "FTS-Auto Format: Auto-color all regions".to_string(),
            handler: apply_regions_handler,
            ..Default::default()
        },
        // Toggle auto-region-color on project changes
        ActionDef {
            command_id: "FTS_AUTOCOLOR_REGIONS_TOGGLE",
            display_name: "FTS-Auto Format: Toggle auto-color regions enable".to_string(),
            handler: toggle_regions_handler,
            toggle_state: Some(state::is_region_color_enabled),
            ..Default::default()
        },
        // Clear colors from all regions
        ActionDef {
            command_id: "FTS_AUTOCOLOR_REGIONS_CLEAR",
            display_name: "FTS-Auto Format: Clear all region colors".to_string(),
            handler: clear_regions_handler,
            ..Default::default()
        },
        // === Marker Coloring ===
        // Apply auto-color to all markers
        ActionDef {
            command_id: "FTS_AUTOCOLOR_MARKERS_APPLY",
            display_name: "FTS-Auto Format: Auto-color all markers".to_string(),
            handler: apply_markers_handler,
            ..Default::default()
        },
        // Toggle auto-marker-color on project changes
        ActionDef {
            command_id: "FTS_AUTOCOLOR_MARKERS_TOGGLE",
            display_name: "FTS-Auto Format: Toggle auto-color markers enable".to_string(),
            handler: toggle_markers_handler,
            toggle_state: Some(state::is_marker_color_enabled),
            ..Default::default()
        },
        // Clear colors from all markers
        ActionDef {
            command_id: "FTS_AUTOCOLOR_MARKERS_CLEAR",
            display_name: "FTS-Auto Format: Clear all marker colors".to_string(),
            handler: clear_markers_handler,
            ..Default::default()
        },
        // === Track Icons ===
        // Apply auto-icons to all tracks
        ActionDef {
            command_id: "FTS_AUTOICON_APPLY_ALL",
            display_name: "FTS-Auto Format: Auto-icon all tracks".to_string(),
            handler: apply_icons_all_handler,
            ..Default::default()
        },
        // Apply auto-icons to selected tracks
        ActionDef {
            command_id: "FTS_AUTOICON_APPLY_SELECTED",
            display_name: "FTS-Auto Format: Auto-icon selected tracks".to_string(),
            handler: apply_icons_selected_handler,
            ..Default::default()
        },
        // Toggle auto-icon on project changes
        ActionDef {
            command_id: "FTS_AUTOICON_TOGGLE",
            display_name: "FTS-Auto Format: Toggle auto-icon enable".to_string(),
            handler: toggle_icons_handler,
            toggle_state: Some(state::is_icon_enabled),
            ..Default::default()
        },
        // Clear icons from all tracks
        ActionDef {
            command_id: "FTS_AUTOICON_CLEAR_ALL",
            display_name: "FTS-Auto Format: Clear all track icons".to_string(),
            handler: clear_icons_all_handler,
            ..Default::default()
        },
        // Clear icons from selected tracks
        ActionDef {
            command_id: "FTS_AUTOICON_CLEAR_SELECTED",
            display_name: "FTS-Auto Format: Clear selected track icons".to_string(),
            handler: clear_icons_selected_handler,
            ..Default::default()
        },
        // === Track Layouts ===
        // Apply auto-layouts to all tracks
        ActionDef {
            command_id: "FTS_AUTOLAYOUT_APPLY_ALL",
            display_name: "FTS-Auto Format: Auto-layout all tracks".to_string(),
            handler: apply_layouts_all_handler,
            ..Default::default()
        },
        // Apply auto-layouts to selected tracks
        ActionDef {
            command_id: "FTS_AUTOLAYOUT_APPLY_SELECTED",
            display_name: "FTS-Auto Format: Auto-layout selected tracks".to_string(),
            handler: apply_layouts_selected_handler,
            ..Default::default()
        },
        // Toggle auto-layout on project changes
        ActionDef {
            command_id: "FTS_AUTOLAYOUT_TOGGLE",
            display_name: "FTS-Auto Format: Toggle auto-layout enable".to_string(),
            handler: toggle_layouts_handler,
            toggle_state: Some(state::is_layout_enabled),
            ..Default::default()
        },
        // Clear layouts from all tracks
        ActionDef {
            command_id: "FTS_AUTOLAYOUT_CLEAR_ALL",
            display_name: "FTS-Auto Format: Clear all track layouts".to_string(),
            handler: clear_layouts_all_handler,
            ..Default::default()
        },
        // Clear layouts from selected tracks
        ActionDef {
            command_id: "FTS_AUTOLAYOUT_CLEAR_SELECTED",
            display_name: "FTS-Auto Format: Clear selected track layouts".to_string(),
            handler: clear_layouts_selected_handler,
            ..Default::default()
        },
        // === Combined Actions ===
        // Apply colors to everything (tracks, regions, markers)
        ActionDef {
            command_id: "FTS_AUTOCOLOR_APPLY_EVERYTHING",
            display_name: "FTS-Auto Format: Auto-color everything (tracks, regions, markers)".to_string(),
            handler: apply_everything_handler,
            ..Default::default()
        },
        // Clear all colors (tracks, regions, markers)
        ActionDef {
            command_id: "FTS_AUTOCOLOR_CLEAR_EVERYTHING",
            display_name: "FTS-Auto Format: Clear all colors (tracks, regions, markers)".to_string(),
            handler: clear_everything_handler,
            ..Default::default()
        },
        // Apply everything (colors + icons + layouts)
        ActionDef {
            command_id: "FTS_AUTO_APPLY_ALL",
            display_name: "FTS-Auto Format: Apply all auto-formatting (colors, icons, layouts)".to_string(),
            handler: apply_all_formatting_handler,
            ..Default::default()
        },
        // === Color Children (Gradient) ===
        // Apply gradient to selected folder children (darken)
        ActionDef {
            command_id: "FTS_AUTOCOLOR_CHILDREN_DARKEN",
            display_name: "FTS-Auto Format: Color children gradient (darken)".to_string(),
            handler: color_children_darken_handler,
            ..Default::default()
        },
        // Apply gradient to selected folder children (lighten)
        ActionDef {
            command_id: "FTS_AUTOCOLOR_CHILDREN_LIGHTEN",
            display_name: "FTS-Auto Format: Color children gradient (lighten)".to_string(),
            handler: color_children_lighten_handler,
            ..Default::default()
        },
        // Apply gradient to all folder children (darken)
        ActionDef {
            command_id: "FTS_AUTOCOLOR_ALL_CHILDREN_DARKEN",
            display_name: "FTS-Auto Format: Color all folder children gradient (darken)".to_string(),
            handler: color_all_children_darken_handler,
            ..Default::default()
        },
        // Apply gradient to all folder children (lighten)
        ActionDef {
            command_id: "FTS_AUTOCOLOR_ALL_CHILDREN_LIGHTEN",
            display_name: "FTS-Auto Format: Color all folder children gradient (lighten)".to_string(),
            handler: color_all_children_lighten_handler,
            ..Default::default()
        },
        // Set children to same color as parent
        ActionDef {
            command_id: "FTS_AUTOCOLOR_CHILDREN_SAME",
            display_name: "FTS-Auto Format: Set children to parent color".to_string(),
            handler: color_children_same_handler,
            ..Default::default()
        },
    ]
}

/// Register auto-color actions (called from main actions registration)
pub fn register_auto_color_actions() {
    info!("FTS / Auto Color: Actions registered");
}

/// Handler: Apply auto-color to all tracks
fn apply_all_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Applying colors to all tracks");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Auto-color all tracks").unwrap();
        low.Undo_BeginBlock();

        let (colored, skipped) = state::apply_colors_to_all_tracks();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Auto-colored {} tracks ({} skipped - no matching classification)\n",
                colored, skipped
            )
            .as_str(),
        );
    }
}

/// Handler: Apply auto-color to selected tracks
fn apply_selected_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Applying colors to selected tracks");

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());
        if num_selected == 0 {
            reaper.show_console_msg("No tracks selected\n");
            return;
        }

        let undo_name = CString::new("FTS-Auto Format: Auto-color selected tracks").unwrap();
        low.Undo_BeginBlock();

        let (colored, skipped) = state::apply_colors_to_selected_tracks();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Auto-colored {} of {} selected tracks ({} skipped)\n",
                colored, num_selected, skipped
            )
            .as_str(),
        );
    }
}

/// Handler: Toggle auto-color enable
fn toggle_handler() {
    let reaper = Reaper::get();

    let enabled = state::toggle_enabled();

    let status = if enabled { "ENABLED" } else { "DISABLED" };
    info!("FTS / Auto Color: {}", status);
    reaper.show_console_msg(format!("Auto-color {}\n", status).as_str());

    // If enabling, apply colors immediately
    if enabled {
        let (colored, _) = state::apply_colors_to_all_tracks();
        reaper.show_console_msg(format!("Applied colors to {} tracks\n", colored).as_str());
    }
}

/// Handler: Clear all track colors
fn clear_all_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Clearing all track colors");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Clear all track colors").unwrap();
        low.Undo_BeginBlock();

        let cleared = state::clear_all_track_colors();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(format!("Cleared colors from {} tracks\n", cleared).as_str());
    }
}

/// Handler: Clear selected track colors
fn clear_selected_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Clearing selected track colors");

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());
        if num_selected == 0 {
            reaper.show_console_msg("No tracks selected\n");
            return;
        }

        let undo_name = CString::new("FTS-Auto Format: Clear selected track colors").unwrap();
        low.Undo_BeginBlock();

        let cleared = state::clear_selected_track_colors();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(format!("Cleared colors from {} tracks\n", cleared).as_str());
    }
}

/// Handler: Refresh classification and colors
fn refresh_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Refreshing classification and colors");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Refresh auto-colors").unwrap();
        low.Undo_BeginBlock();

        // Get track count for reporting
        let num_tracks = low.CountTracks(std::ptr::null_mut());

        // Apply colors based on fresh classification
        let (colored, skipped) = state::apply_colors_to_all_tracks();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Refreshed {} tracks: colored {}, skipped {}\n",
                num_tracks, colored, skipped
            )
            .as_str(),
        );
    }
}

// === Region Handlers ===

/// Handler: Apply auto-color to all regions
fn apply_regions_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Applying colors to all regions");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Auto-color all regions").unwrap();
        low.Undo_BeginBlock();

        let (colored, skipped) = state::apply_colors_to_all_regions();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Auto-colored {} regions ({} skipped - no matching section)\n",
                colored, skipped
            )
            .as_str(),
        );
    }
}

/// Handler: Toggle auto-region-color enable
fn toggle_regions_handler() {
    let reaper = Reaper::get();

    let enabled = state::toggle_region_color_enabled();

    let status = if enabled { "ENABLED" } else { "DISABLED" };
    info!("FTS / Auto Color Regions: {}", status);
    reaper.show_console_msg(format!("Auto-color regions {}\n", status).as_str());

    // If enabling, apply colors immediately
    if enabled {
        let (colored, _) = state::apply_colors_to_all_regions();
        reaper.show_console_msg(format!("Applied colors to {} regions\n", colored).as_str());
    }
}

/// Handler: Clear all region colors
fn clear_regions_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Clearing all region colors");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Clear all region colors").unwrap();
        low.Undo_BeginBlock();

        let cleared = state::clear_all_region_colors();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(format!("Cleared colors from {} regions\n", cleared).as_str());
    }
}

// === Marker Handlers ===

/// Handler: Apply auto-color to all markers
fn apply_markers_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Applying colors to all markers");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Auto-color all markers").unwrap();
        low.Undo_BeginBlock();

        let (colored, skipped) = state::apply_colors_to_all_markers();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Auto-colored {} markers ({} skipped - no matching cue)\n",
                colored, skipped
            )
            .as_str(),
        );
    }
}

/// Handler: Toggle auto-marker-color enable
fn toggle_markers_handler() {
    let reaper = Reaper::get();

    let enabled = state::toggle_marker_color_enabled();

    let status = if enabled { "ENABLED" } else { "DISABLED" };
    info!("FTS / Auto Color Markers: {}", status);
    reaper.show_console_msg(format!("Auto-color markers {}\n", status).as_str());

    // If enabling, apply colors immediately
    if enabled {
        let (colored, _) = state::apply_colors_to_all_markers();
        reaper.show_console_msg(format!("Applied colors to {} markers\n", colored).as_str());
    }
}

/// Handler: Clear all marker colors
fn clear_markers_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Clearing all marker colors");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Clear all marker colors").unwrap();
        low.Undo_BeginBlock();

        let cleared = state::clear_all_marker_colors();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(format!("Cleared colors from {} markers\n", cleared).as_str());
    }
}

// === Combined Handlers ===

/// Handler: Apply colors to everything (tracks, regions, markers)
fn apply_everything_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Applying colors to everything");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Auto-color everything").unwrap();
        low.Undo_BeginBlock();

        let (tracks_colored, tracks_skipped) = state::apply_colors_to_all_tracks();
        let (regions_colored, regions_skipped) = state::apply_colors_to_all_regions();
        let (markers_colored, markers_skipped) = state::apply_colors_to_all_markers();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Auto-colored: {} tracks ({} skipped), {} regions ({} skipped), {} markers ({} skipped)\n",
                tracks_colored, tracks_skipped,
                regions_colored, regions_skipped,
                markers_colored, markers_skipped
            )
            .as_str(),
        );
    }
}

/// Handler: Clear all colors (tracks, regions, markers)
fn clear_everything_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Clearing all colors");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Clear all colors").unwrap();
        low.Undo_BeginBlock();

        let tracks_cleared = state::clear_all_track_colors();
        let regions_cleared = state::clear_all_region_colors();
        let markers_cleared = state::clear_all_marker_colors();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Cleared colors from: {} tracks, {} regions, {} markers\n",
                tracks_cleared, regions_cleared, markers_cleared
            )
            .as_str(),
        );
    }
}

// === Icon Handlers ===

/// Handler: Apply auto-icons to all tracks
fn apply_icons_all_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Icon: Applying icons to all tracks");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Auto-icon all tracks").unwrap();
        low.Undo_BeginBlock();

        let (iconified, skipped) = state::apply_icons_to_all_tracks();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Auto-iconified {} tracks ({} skipped - no matching classification)\n",
                iconified, skipped
            )
            .as_str(),
        );
    }
}

/// Handler: Apply auto-icons to selected tracks
fn apply_icons_selected_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Icon: Applying icons to selected tracks");

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());
        if num_selected == 0 {
            reaper.show_console_msg("No tracks selected\n");
            return;
        }

        let undo_name = CString::new("FTS-Auto Format: Auto-icon selected tracks").unwrap();
        low.Undo_BeginBlock();

        let (iconified, skipped) = state::apply_icons_to_selected_tracks();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Auto-iconified {} of {} selected tracks ({} skipped)\n",
                iconified, num_selected, skipped
            )
            .as_str(),
        );
    }
}

/// Handler: Toggle auto-icon enable
fn toggle_icons_handler() {
    let reaper = Reaper::get();

    let enabled = state::toggle_icon_enabled();

    let status = if enabled { "ENABLED" } else { "DISABLED" };
    info!("FTS / Auto Icon: {}", status);
    reaper.show_console_msg(format!("Auto-icon {}\n", status).as_str());

    // If enabling, apply icons immediately
    if enabled {
        let (iconified, _) = state::apply_icons_to_all_tracks();
        reaper.show_console_msg(format!("Applied icons to {} tracks\n", iconified).as_str());
    }
}

/// Handler: Clear all track icons
fn clear_icons_all_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Icon: Clearing all track icons");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Clear all track icons").unwrap();
        low.Undo_BeginBlock();

        let cleared = state::clear_all_track_icons();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(format!("Cleared icons from {} tracks\n", cleared).as_str());
    }
}

/// Handler: Clear selected track icons
fn clear_icons_selected_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Icon: Clearing selected track icons");

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());
        if num_selected == 0 {
            reaper.show_console_msg("No tracks selected\n");
            return;
        }

        let undo_name = CString::new("FTS-Auto Format: Clear selected track icons").unwrap();
        low.Undo_BeginBlock();

        let cleared = state::clear_selected_track_icons();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(format!("Cleared icons from {} tracks\n", cleared).as_str());
    }
}

/// Handler: Apply all auto-formatting (colors + icons + layouts)
fn apply_all_formatting_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Format: Applying all formatting");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Apply all auto-formatting").unwrap();
        low.Undo_BeginBlock();

        let (tracks_colored, _) = state::apply_colors_to_all_tracks();
        let (regions_colored, _) = state::apply_colors_to_all_regions();
        let (markers_colored, _) = state::apply_colors_to_all_markers();
        let (icons_applied, _) = state::apply_icons_to_all_tracks();
        let (layouts_applied, _) = state::apply_layouts_to_all_tracks();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Applied: colors to {} tracks, {} regions, {} markers; icons to {} tracks; layouts to {} tracks\n",
                tracks_colored, regions_colored, markers_colored, icons_applied, layouts_applied
            )
            .as_str(),
        );
    }
}

// === Layout Handlers ===

/// Handler: Apply auto-layouts to all tracks
fn apply_layouts_all_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Layout: Applying layouts to all tracks");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Auto-layout all tracks").unwrap();
        low.Undo_BeginBlock();

        let (layoutified, skipped) = state::apply_layouts_to_all_tracks();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Auto-layoutified {} tracks ({} skipped - no matching classification)\n",
                layoutified, skipped
            )
            .as_str(),
        );
    }
}

/// Handler: Apply auto-layouts to selected tracks
fn apply_layouts_selected_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Layout: Applying layouts to selected tracks");

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());
        if num_selected == 0 {
            reaper.show_console_msg("No tracks selected\n");
            return;
        }

        let undo_name = CString::new("FTS-Auto Format: Auto-layout selected tracks").unwrap();
        low.Undo_BeginBlock();

        let (layoutified, skipped) = state::apply_layouts_to_selected_tracks();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Auto-layoutified {} of {} selected tracks ({} skipped)\n",
                layoutified, num_selected, skipped
            )
            .as_str(),
        );
    }
}

/// Handler: Toggle auto-layout enable
fn toggle_layouts_handler() {
    let reaper = Reaper::get();

    let enabled = state::toggle_layout_enabled();

    let status = if enabled { "ENABLED" } else { "DISABLED" };
    info!("FTS / Auto Layout: {}", status);
    reaper.show_console_msg(format!("Auto-layout {}\n", status).as_str());

    // If enabling, apply layouts immediately
    if enabled {
        let (layoutified, _) = state::apply_layouts_to_all_tracks();
        reaper.show_console_msg(format!("Applied layouts to {} tracks\n", layoutified).as_str());
    }
}

/// Handler: Clear all track layouts
fn clear_layouts_all_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Layout: Clearing all track layouts");

    unsafe {
        let undo_name = CString::new("FTS-Auto Format: Clear all track layouts").unwrap();
        low.Undo_BeginBlock();

        let cleared = state::clear_all_track_layouts();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(format!("Cleared layouts from {} tracks\n", cleared).as_str());
    }
}

/// Handler: Clear selected track layouts
fn clear_layouts_selected_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Layout: Clearing selected track layouts");

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());
        if num_selected == 0 {
            reaper.show_console_msg("No tracks selected\n");
            return;
        }

        let undo_name = CString::new("FTS-Auto Format: Clear selected track layouts").unwrap();
        low.Undo_BeginBlock();

        let cleared = state::clear_selected_track_layouts();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(format!("Cleared layouts from {} tracks\n", cleared).as_str());
    }
}

// === Color Children (Gradient) Handlers ===

/// Handler: Apply darkening gradient to children of selected folders
fn color_children_darken_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Applying darken gradient to selected folder children");

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());
        if num_selected == 0 {
            reaper.show_console_msg("No tracks selected\n");
            return;
        }

        let undo_name = CString::new("FTS-Auto Format: Color children gradient (darken)").unwrap();
        low.Undo_BeginBlock();

        let (folders, children) =
            state::apply_gradient_to_selected_folder_children(GradientDirection::Darken);

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        if folders == 0 {
            reaper.show_console_msg("No folder tracks selected\n");
        } else {
            reaper.show_console_msg(
                format!(
                    "Applied darken gradient to {} children in {} folders\n",
                    children, folders
                )
                .as_str(),
            );
        }
    }
}

/// Handler: Apply lightening gradient to children of selected folders
fn color_children_lighten_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Applying lighten gradient to selected folder children");

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());
        if num_selected == 0 {
            reaper.show_console_msg("No tracks selected\n");
            return;
        }

        let undo_name = CString::new("FTS-Auto Format: Color children gradient (lighten)").unwrap();
        low.Undo_BeginBlock();

        let (folders, children) =
            state::apply_gradient_to_selected_folder_children(GradientDirection::Lighten);

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        if folders == 0 {
            reaper.show_console_msg("No folder tracks selected\n");
        } else {
            reaper.show_console_msg(
                format!(
                    "Applied lighten gradient to {} children in {} folders\n",
                    children, folders
                )
                .as_str(),
            );
        }
    }
}

/// Handler: Apply darkening gradient to children of ALL folders
fn color_all_children_darken_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Applying darken gradient to all folder children");

    unsafe {
        let undo_name =
            CString::new("FTS-Auto Format: Color all folder children gradient (darken)").unwrap();
        low.Undo_BeginBlock();

        let (folders, children) =
            state::apply_gradient_to_all_folder_children(GradientDirection::Darken);

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Applied darken gradient to {} children in {} folders\n",
                children, folders
            )
            .as_str(),
        );
    }
}

/// Handler: Apply lightening gradient to children of ALL folders
fn color_all_children_lighten_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Applying lighten gradient to all folder children");

    unsafe {
        let undo_name =
            CString::new("FTS-Auto Format: Color all folder children gradient (lighten)").unwrap();
        low.Undo_BeginBlock();

        let (folders, children) =
            state::apply_gradient_to_all_folder_children(GradientDirection::Lighten);

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        reaper.show_console_msg(
            format!(
                "Applied lighten gradient to {} children in {} folders\n",
                children, folders
            )
            .as_str(),
        );
    }
}

/// Handler: Set children to same color as their parent folder
fn color_children_same_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Auto Color: Setting children to parent color");

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());
        if num_selected == 0 {
            reaper.show_console_msg("No tracks selected\n");
            return;
        }

        let undo_name = CString::new("FTS-Auto Format: Set children to parent color").unwrap();
        low.Undo_BeginBlock();

        let (folders, children) = state::set_children_to_parent_color();

        low.Undo_EndBlock(undo_name.as_ptr(), 0);

        if folders == 0 {
            reaper.show_console_msg("No folder tracks with custom colors selected\n");
        } else {
            reaper.show_console_msg(
                format!(
                    "Set {} children to parent color in {} folders\n",
                    children, folders
                )
                .as_str(),
            );
        }
    }
}
