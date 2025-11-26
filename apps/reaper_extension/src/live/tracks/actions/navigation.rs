//! Navigation Actions
//!
//! Actions for navigating between songs and sections in the setlist

use crate::implementation::setlist::build_setlist_from_open_projects;
use crate::implementation::markers::read_markers_from_project;
use crate::live::tracks::actions::zoom::zoom_horizontally_to_song;
use crate::live::tracks::tab_navigation::TabNavigator;
use reaper_high::{BookmarkType, Project, Reaper};
use reaper_medium::{PositionInSeconds, ProjectRef, SetEditCurPosOptions};
use tracing::{debug, info, warn};

/// Helper: Find Count-In marker position in a project
fn find_count_in_marker(project: &Project) -> Option<f64> {
    let markers = read_markers_from_project(project).ok()?;
    markers.iter()
        .find(|m| m.name.trim().eq_ignore_ascii_case("Count-In"))
        .map(|m| m.position.time.to_seconds())
}

/// Helper: Switch to a project tab by index using TabNavigator
fn switch_to_tab(tab_index: u32) -> Result<(), String> {
    let tab_navigator = TabNavigator::new();
    tab_navigator.switch_to_tab(tab_index as usize)
        .map_err(|e| e.to_string())
}

/// Helper: Find tab index for a project name
fn find_tab_index_by_project_name(project_name: &str) -> Option<u32> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            let tab_name = if let Some(file_path) = result.file_path.as_ref() {
                file_path
                    .as_std_path()
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| format!("Tab {}", i))
            } else {
                format!("Tab {}", i)
            };
            
            // Normalize names for comparison (handle both hyphens and underscores)
            let normalized_tab = tab_name.to_uppercase().replace('_', "-");
            let normalized_target = project_name.to_uppercase().replace('_', "-");
            
            if normalized_tab == normalized_target {
                return Some(i);
            }
        }
    }
    None
}

/// Go to previous song in setlist
pub fn go_to_previous_song() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    info!("Go To Previous Song action executed");
    
    // Build setlist
    let setlist = match build_setlist_from_open_projects(None) {
        Ok(s) => s,
        Err(e) => {
            warn!(error = %e, "Failed to build setlist");
            reaper.show_console_msg("Go To Previous Song: Failed to build setlist\n");
            return;
        }
    };
    
    if setlist.songs.is_empty() {
        warn!("Setlist is empty");
        reaper.show_console_msg("Go To Previous Song: Setlist is empty\n");
        return;
    }
    
    // Find current song by matching project name
    let current_project_name = {
        let current_project = reaper.current_project();
        let mut found_name = None;
        for i in 0..128u32 {
            if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
                if result.project == current_project.raw() {
                    found_name = result.file_path.as_ref()
                        .and_then(|p| p.as_std_path().file_stem())
                        .and_then(|s| s.to_str())
                        .map(|s| s.to_string());
                    break;
                }
            }
        }
        found_name
    };
    
    // Find current song index
    let current_index = current_project_name.and_then(|name| {
        setlist.songs.iter().position(|song| song.project_name_from_metadata() == name)
    }).unwrap_or(0);
    
    let previous_index = if current_index > 0 {
        current_index - 1
    } else {
        setlist.songs.len() - 1 // Wrap around
    };
    
    let previous_song = &setlist.songs[previous_index];
    let previous_project_name = previous_song.project_name_from_metadata();
    
    // Find tab index for previous song
    let tab_index = match find_tab_index_by_project_name(&previous_project_name) {
        Some(idx) => idx,
        None => {
            warn!(project_name = %previous_project_name, "Previous song project not found");
            reaper.show_console_msg(format!("Go To Previous Song: Project '{}' not found\n", previous_project_name));
            return;
        }
    };
    
    // Switch to previous tab
    if let Err(e) = switch_to_tab(tab_index) {
        warn!(error = %e, "Failed to switch to previous song");
        reaper.show_console_msg(format!("Go To Previous Song: Failed to switch to tab {}\n", tab_index));
        return;
    }
    
    // Small delay for tab switch
    std::thread::sleep(std::time::Duration::from_millis(50));
    
    // Move cursor to Count-In or beginning
    if let Some(result) = medium_reaper.enum_projects(ProjectRef::Current, 0) {
        let project = Project::new(result.project);
        let start_pos = if let Some(count_in) = find_count_in_marker(&project) {
            PositionInSeconds::new(count_in).unwrap_or(PositionInSeconds::ZERO)
        } else {
            PositionInSeconds::ZERO
        };
        
        project.set_edit_cursor_position(start_pos, SetEditCurPosOptions { move_view: false, seek_play: false });
        
        // Zoom to song
        zoom_horizontally_to_song();
    }
    
    info!(from_song = current_index, to_song = previous_index, "Went to previous song");
    reaper.show_console_msg(format!("Go To Previous Song: {}\n", previous_project_name));
}

/// Go to a specific song by index in setlist
pub fn go_to_song(song_index: usize) {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    info!(song_index, "Go To Song action executed");
    
    // Build setlist
    let setlist = match build_setlist_from_open_projects(None) {
        Ok(s) => s,
        Err(e) => {
            warn!(error = %e, "Failed to build setlist");
            reaper.show_console_msg(format!("Go To Song {}: Failed to build setlist\n", song_index));
            return;
        }
    };
    
    if setlist.songs.is_empty() {
        warn!("Setlist is empty");
        reaper.show_console_msg(format!("Go To Song {}: Setlist is empty\n", song_index));
        return;
    }
    
    if song_index >= setlist.songs.len() {
        warn!(song_index, song_count = setlist.songs.len(), "Song index out of range");
        reaper.show_console_msg(format!("Go To Song {}: Index out of range (setlist has {} songs)\n", song_index, setlist.songs.len()));
        return;
    }
    
    let target_song = &setlist.songs[song_index];
    let target_project_name = target_song.project_name_from_metadata();
    
    // Find tab index for target song
    let tab_index = match find_tab_index_by_project_name(&target_project_name) {
        Some(idx) => idx,
        None => {
            warn!(project_name = %target_project_name, "Target song project not found");
            reaper.show_console_msg(format!("Go To Song {}: Project '{}' not found\n", song_index, target_project_name));
            return;
        }
    };
    
    // Switch to target tab
    if let Err(e) = switch_to_tab(tab_index) {
        warn!(error = %e, "Failed to switch to target song");
        reaper.show_console_msg(format!("Go To Song {}: Failed to switch to tab {}\n", song_index, tab_index));
        return;
    }
    
    // Move cursor to Count-In or beginning
    if let Some(result) = medium_reaper.enum_projects(ProjectRef::Current, 0) {
        let project = Project::new(result.project);
        let start_pos = if let Some(count_in) = find_count_in_marker(&project) {
            PositionInSeconds::new(count_in).unwrap_or(PositionInSeconds::ZERO)
        } else {
            PositionInSeconds::ZERO
        };
        
        project.set_edit_cursor_position(start_pos, SetEditCurPosOptions { move_view: false, seek_play: false });
        
        // Log position and musical position
        let edit_pos = project.edit_cursor_position().unwrap_or(PositionInSeconds::ZERO);
        let edit_beat_info = project.beat_info_at(edit_pos);
        let musical_pos = format!("{}.{}.{:03}", 
            edit_beat_info.measure_index + 1,
            edit_beat_info.beats_since_measure.get().floor() as i32 + 1,
            ((edit_beat_info.beats_since_measure.get() - edit_beat_info.beats_since_measure.get().floor()) * 1000.0).round() as i32
        );
        info!(
            song_index,
            project_name = %target_project_name,
            time_position = edit_pos.get(),
            musical_position = %musical_pos,
            "Went to song"
        );
        
        // Zoom to song
        zoom_horizontally_to_song();
    }
    
    reaper.show_console_msg(format!("Go To Song {}: {}\n", song_index, target_project_name));
}

/// Go to next song in setlist
pub fn go_to_next_song() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    info!("Go To Next Song action executed");
    
    // Build setlist
    let setlist = match build_setlist_from_open_projects(None) {
        Ok(s) => s,
        Err(e) => {
            warn!(error = %e, "Failed to build setlist");
            reaper.show_console_msg("Go To Next Song: Failed to build setlist\n");
            return;
        }
    };
    
    if setlist.songs.is_empty() {
        warn!("Setlist is empty");
        reaper.show_console_msg("Go To Next Song: Setlist is empty\n");
        return;
    }
    
    // Find current song by matching project name
    let current_project_name = {
        let current_project = reaper.current_project();
        let mut found_name = None;
        for i in 0..128u32 {
            if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
                if result.project == current_project.raw() {
                    found_name = result.file_path.as_ref()
                        .and_then(|p| p.as_std_path().file_stem())
                        .and_then(|s| s.to_str())
                        .map(|s| s.to_string());
                    break;
                }
            }
        }
        found_name
    };
    
    // Find current song index
    let current_index = current_project_name.and_then(|name| {
        setlist.songs.iter().position(|song| song.project_name_from_metadata() == name)
    }).unwrap_or(0);
    
    let next_index = if current_index < setlist.songs.len() - 1 {
        current_index + 1
    } else {
        0 // Wrap around
    };
    
    let next_song = &setlist.songs[next_index];
    let next_project_name = next_song.project_name_from_metadata();
    
    // Find tab index for next song
    let tab_index = match find_tab_index_by_project_name(&next_project_name) {
        Some(idx) => idx,
        None => {
            warn!(project_name = %next_project_name, "Next song project not found");
            reaper.show_console_msg(format!("Go To Next Song: Project '{}' not found\n", next_project_name));
            return;
        }
    };
    
    // Switch to next tab
    if let Err(e) = switch_to_tab(tab_index) {
        warn!(error = %e, "Failed to switch to next song");
        reaper.show_console_msg(format!("Go To Next Song: Failed to switch to tab {}\n", tab_index));
        return;
    }
    
    // Small delay for tab switch
    std::thread::sleep(std::time::Duration::from_millis(50));
    
    // Move cursor to Count-In or beginning
    if let Some(result) = medium_reaper.enum_projects(ProjectRef::Current, 0) {
        let project = Project::new(result.project);
        let start_pos = if let Some(count_in) = find_count_in_marker(&project) {
            PositionInSeconds::new(count_in).unwrap_or(PositionInSeconds::ZERO)
        } else {
            PositionInSeconds::ZERO
        };
        
        project.set_edit_cursor_position(start_pos, SetEditCurPosOptions { move_view: false, seek_play: false });
        
        // Zoom to song
        zoom_horizontally_to_song();
    }
    
    info!(from_song = current_index, to_song = next_index, "Went to next song");
    reaper.show_console_msg(format!("Go To Next Song: {}\n", next_project_name));
}

/// Get all sections in the current song (regions, excluding SONGSTART and =END markers)
fn get_sections_for_current_song(project: &Project) -> Vec<(String, PositionInSeconds, Option<PositionInSeconds>)> {
    let mut sections = Vec::new();
    let bookmark_count = project.bookmark_count();
    
    // Find SONGSTART and SONGEND to define song boundaries
    let mut song_start: Option<f64> = None;
    let mut song_end: Option<f64> = None;
    
    for i in 0..bookmark_count.total_count {
        if let Some(bookmark) = project.find_bookmark_by_index(i) {
            let name = bookmark.name();
            let info = bookmark.basic_info();
            
            // Only check markers (not regions) for SONGSTART and SONGEND
            if info.bookmark_type() == BookmarkType::Marker {
                // Look for SONGSTART (song start)
                if name.trim().eq_ignore_ascii_case("SONGSTART") {
                    song_start = Some(info.position.get());
                }
                
                // Look for SONGEND (song end)
                if name.trim().eq_ignore_ascii_case("SONGEND") {
                    song_end = Some(info.position.get());
                }
            }
        }
    }
    
    // If no SONGSTART, use 0.0
    let start_boundary = song_start.unwrap_or(0.0);
    // If no SONGEND, use a large number (or find last region)
    let end_boundary = song_end.unwrap_or(f64::MAX);
    
    debug!(start_boundary, end_boundary, "Song boundaries determined");
    
    // Find all regions between start and end (these are sections)
    for i in 0..bookmark_count.total_count {
        if let Some(bookmark) = project.find_bookmark_by_index(i) {
            let info = bookmark.basic_info();
            
            // Only check regions (not markers like SONGSTART, =END, SONGEND)
            if info.bookmark_type() == BookmarkType::Region {
                if let Some(region_end) = info.region_end_position {
                    let region_start = info.position.get();
                    let region_end_val = region_end.get();
                    
                    let region_name = bookmark.name();
                    let region_name_trimmed = region_name.trim();
                    
                    // Skip if this region is named "SONGSTART" or "=END" (these are markers, not sections)
                    if region_name_trimmed.eq_ignore_ascii_case("SONGSTART") || region_name_trimmed == "=END" {
                        continue;
                    }
                    
                    // Include region if it's within or overlaps song boundaries
                    // Region is included if it starts before or at end_boundary and ends after or at start_boundary
                    if region_end_val >= start_boundary && region_start <= end_boundary {
                        debug!(
                            section_name = %region_name,
                            start = region_start,
                            end = region_end_val,
                            "Added section"
                        );
                        sections.push((
                            region_name,
                            info.position,
                            Some(region_end),
                        ));
                    }
                }
            }
        }
    }
    
    // Sort sections by start position
    sections.sort_by(|a, b| a.1.get().partial_cmp(&b.1.get()).unwrap_or(std::cmp::Ordering::Equal));
    
    debug!(sections_count = sections.len(), "Found sections");
    
    sections
}

/// Find current section index based on cursor position
/// Returns the section that contains the cursor, considering markers at section boundaries
/// 
/// When the cursor is at a marker position that's at the start of a section, it belongs to that section.
fn find_current_section_index(project: &Project, cursor_pos: PositionInSeconds) -> Option<usize> {
    let sections = get_sections_for_current_song(project);
    if sections.is_empty() {
        return None;
    }
    
    let cursor = cursor_pos.get();
    
    // Log all sections for debugging
    debug!(
        cursor = cursor,
        sections_count = sections.len(),
        sections = ?sections.iter().enumerate().map(|(i, (name, start, end))| {
            format!("[{}] {}: {:.3}-{:.3}", i, name, start.get(), end.map(|e| e.get()).unwrap_or(start.get()))
        }).collect::<Vec<_>>(),
        "Finding current section"
    );
    
    // Use a larger epsilon to handle floating point precision and marker placement
    // Markers can be placed slightly before or after region boundaries
    let epsilon = 0.5; // Increased to 500ms to handle markers that are before section starts
    
    // First, check if cursor is very close to any section start (for markers)
    // This handles the case where a marker (like SONGSTART) is placed before the section region starts
    for (i, (_name, start, _end_opt)) in sections.iter().enumerate() {
        let start_val = start.get();
        let distance_to_start = (cursor - start_val).abs();
        
        // If cursor is within epsilon of section start, consider it part of that section
        if distance_to_start <= epsilon {
            debug!(
                section_index = i,
                cursor = cursor,
                section_start = start_val,
                distance = distance_to_start,
                epsilon = epsilon,
                "Cursor very close to section start (marker position)"
            );
            return Some(i);
        }
    }
    
    // Iterate through sections to find which one contains the cursor
    // We want to be inclusive of section boundaries (including start markers)
    for (i, (_name, start, end_opt)) in sections.iter().enumerate() {
        let start_val = start.get();
        let end_val = end_opt.map(|e| e.get()).unwrap_or(start_val);
        
        debug!(
            checking_section = i,
            section_start = start_val,
            section_end = end_val,
            cursor = cursor,
            cursor_after_start = cursor >= (start_val - epsilon),
            cursor_before_end = cursor < (end_val + epsilon),
            "Checking section"
        );
        
        // Check if cursor is within this section's boundaries
        // For non-last sections: cursor >= start (with epsilon) and cursor < end (with epsilon for boundary)
        // For last section: cursor >= start and cursor <= end (both inclusive)
        let is_last = i == sections.len() - 1;
        
        if is_last {
            // Last section: inclusive of both start and end
            if cursor >= (start_val - epsilon) && cursor <= (end_val + epsilon) {
                debug!(
                    section_index = i,
                    cursor = cursor,
                    start = start_val,
                    end = end_val,
                    "Found cursor in last section"
                );
                return Some(i);
            }
        } else {
            // Not last section
            // Cursor is in this section if it's >= section start (inclusive with epsilon)
            // and < section end (exclusive, unless we're at the exact boundary where next section starts)
            
            // First check: is cursor at or after this section's start?
            let at_or_after_start = cursor >= (start_val - epsilon);
            
            // Second check: is cursor before this section's end?
            // If cursor is exactly at the end, we need to check if next section starts there
            let before_end = cursor < (end_val - epsilon);
            
            // Check if cursor is exactly at the end boundary (within epsilon)
            let at_end_boundary = cursor >= (end_val - epsilon) && cursor <= (end_val + epsilon);
            
            debug!(
                section_index = i,
                at_or_after_start,
                before_end,
                at_end_boundary,
                "Section boundary checks"
            );
            
            if at_or_after_start {
                // If cursor is at the end boundary, check if next section starts at that exact position
                if at_end_boundary {
                    if let Some((_next_name, next_start, _next_end)) = sections.get(i + 1) {
                        let next_start_val = next_start.get();
                        // If next section starts at this end position (or very close), cursor is in next section
                        if (next_start_val - end_val).abs() < epsilon {
                            // Skip this section, will check next one in next iteration
                            debug!(
                                section_index = i,
                                next_section_start = next_start_val,
                                "Skipping section - next section starts at end boundary"
                            );
                            continue;
                        }
                    }
                    // Next section doesn't start here, so cursor is still in this section
                    debug!(
                        section_index = i,
                        cursor = cursor,
                        start = start_val,
                        end = end_val,
                        at_end_boundary = true,
                        "Found cursor at section end (next section doesn't start here)"
                    );
                    return Some(i);
                }
                
                // Cursor is clearly within this section (before end)
                if before_end {
                    debug!(
                        section_index = i,
                        cursor = cursor,
                        start = start_val,
                        end = end_val,
                        "Found cursor in section (within range)"
                    );
                    return Some(i);
                }
            }
        }
    }
    
    // Cursor is not within any section boundaries - find closest
    // If cursor is before first section
    if let Some((_name, first_start, _end)) = sections.first() {
        let first_start_val = first_start.get();
        if cursor < (first_start_val - epsilon) {
            // Check if we should go to Count-In instead
            if let Some(count_in) = find_count_in_marker(project) {
                let count_in_pos = PositionInSeconds::new(count_in).unwrap_or(PositionInSeconds::ZERO);
                if cursor <= (count_in_pos.get() + epsilon) {
                    // At or before Count-In, treat as "before first section"
                    debug!(cursor = cursor, count_in = count_in_pos.get(), "Cursor before first section, at Count-In");
                    return Some(0);
                }
            }
            debug!(cursor = cursor, first_start = first_start_val, "Cursor before first section");
            return Some(0);
        }
    }
    
    // Cursor is after all sections, return last index
    debug!(
        cursor = cursor,
        last_section_end = sections.last().and_then(|s| s.2.map(|e| e.get())),
        "Cursor after all sections"
    );
    Some(sections.len().saturating_sub(1))
}

/// Go to previous section in current song
pub fn go_to_previous_section() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    info!("Go To Previous Section action executed");
    
    // Get current project - check if it's FTS-ROUTING and skip if so
    let project_result = match medium_reaper.enum_projects(ProjectRef::Current, 0) {
        Some(result) => {
            // Filter out FTS-ROUTING tab (handles both hyphens and underscores, case-insensitive)
            if let Some(path) = result.file_path.as_ref() {
                if let Some(file_name) = path.as_std_path().file_stem().and_then(|s| s.to_str()) {
                    let normalized = file_name.to_uppercase().replace('_', "-");
                    if normalized == "FTS-ROUTING" {
                        warn!("Cannot navigate sections in FTS-ROUTING tab");
                        reaper.show_console_msg("Go To Previous Section: FTS-ROUTING tab is not a song\n");
                        return;
                    }
                }
            }
            result
        },
        None => {
            warn!("No current project");
            reaper.show_console_msg("Go To Previous Section: No current project\n");
            return;
        }
    };
    
    let project = Project::new(project_result.project);
    let cursor_pos = project.edit_cursor_position().unwrap_or(PositionInSeconds::ZERO);
    
    // Get sections and find current one
    let sections = get_sections_for_current_song(&project);
    if sections.is_empty() {
        warn!("No sections found in current song");
        reaper.show_console_msg("Go To Previous Section: No sections found\n");
        return;
    }
    
    let current_section = find_current_section_index(&project, cursor_pos)
        .unwrap_or(0);
    
    let previous_section = if current_section > 0 {
        current_section - 1
    } else {
        // Already at first section, stay there or go to Count-In
        if let Some(count_in) = find_count_in_marker(&project) {
            let count_in_pos = PositionInSeconds::new(count_in).unwrap_or(PositionInSeconds::ZERO);
            project.set_edit_cursor_position(count_in_pos, SetEditCurPosOptions { move_view: false, seek_play: false });
            info!("Already at first section, moved to Count-In");
            reaper.show_console_msg("Go To Previous Section: Already at first section, moved to Count-In\n");
            return;
        } else {
            // Stay at first section
            current_section
        }
    };
    
    // Move to previous section start (or Count-In if available)
    let target_pos = if previous_section < sections.len() {
        let (_name, start, _end) = &sections[previous_section];
        *start
    } else {
        // Fallback to Count-In or 0
        if let Some(count_in) = find_count_in_marker(&project) {
            PositionInSeconds::new(count_in).unwrap_or(PositionInSeconds::ZERO)
        } else {
            PositionInSeconds::ZERO
        }
    };
    
    project.set_edit_cursor_position(target_pos, SetEditCurPosOptions { move_view: false, seek_play: false });
    
    info!(from_section = current_section, to_section = previous_section, "Went to previous section");
    reaper.show_console_msg(format!("Go To Previous Section: Section {}\n", previous_section + 1));
}

/// Go to next section in current song
pub fn go_to_next_section() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    info!("Go To Next Section action executed");
    
    // Get current project - check if it's FTS-ROUTING and skip if so
    let project_result = match medium_reaper.enum_projects(ProjectRef::Current, 0) {
        Some(result) => {
            // Filter out FTS-ROUTING tab (handles both hyphens and underscores, case-insensitive)
            if let Some(path) = result.file_path.as_ref() {
                if let Some(file_name) = path.as_std_path().file_stem().and_then(|s| s.to_str()) {
                    let normalized = file_name.to_uppercase().replace('_', "-");
                    if normalized == "FTS-ROUTING" {
                        warn!("Cannot navigate sections in FTS-ROUTING tab");
                        reaper.show_console_msg("Go To Next Section: FTS-ROUTING tab is not a song\n");
                        return;
                    }
                }
            }
            result
        },
        None => {
            warn!("No current project");
            reaper.show_console_msg("Go To Next Section: No current project\n");
            return;
        }
    };
    
    let project = Project::new(project_result.project);
    let cursor_pos = project.edit_cursor_position().unwrap_or(PositionInSeconds::ZERO);
    
    // Get sections and find current one
    let sections = get_sections_for_current_song(&project);
    if sections.is_empty() {
        warn!("No sections found in current song");
        reaper.show_console_msg("Go To Next Section: No sections found\n");
        return;
    }
    
    debug!(sections_count = sections.len(), cursor_pos = cursor_pos.get(), "Finding current section");
    
    let current_section = find_current_section_index(&project, cursor_pos)
        .unwrap_or(0);
    
    debug!(current_section = current_section, sections_count = sections.len(), "Current section determined");
    
    let next_section = if current_section < sections.len() - 1 {
        current_section + 1
    } else {
        // Already at last section, stay there
        current_section
    };
    
    // Move to next section start
    let target_pos = if next_section < sections.len() {
        let (_name, start, _end) = &sections[next_section];
        *start
    } else {
        // Fallback to Count-In or 0
        if let Some(count_in) = find_count_in_marker(&project) {
            PositionInSeconds::new(count_in).unwrap_or(PositionInSeconds::ZERO)
        } else {
            PositionInSeconds::ZERO
        }
    };
    
    project.set_edit_cursor_position(target_pos, SetEditCurPosOptions { move_view: false, seek_play: false });
    
    // Give REAPER a moment to actually move the cursor
    // This is important because REAPER might need to process the cursor movement
    std::thread::sleep(std::time::Duration::from_millis(10));
    
    // Verify we moved correctly by reading cursor again
    let new_cursor = project.edit_cursor_position().unwrap_or(PositionInSeconds::ZERO);
    debug!(
        target_pos = target_pos.get(), 
        actual_cursor = new_cursor.get(),
        diff = (new_cursor.get() - target_pos.get()).abs(),
        "Cursor moved"
    );
    
    // Re-check current section after moving to verify detection works
    let verified_section = find_current_section_index(&project, new_cursor);
    debug!(
        original_section = current_section,
        target_section = next_section,
        verified_section = ?verified_section,
        "Section detection after move"
    );
    
    info!(from_section = current_section, to_section = next_section, target_pos = target_pos.get(), "Went to next section");
    reaper.show_console_msg(format!("Go To Next Section: Section {} ({} sections total)\n", next_section + 1, sections.len()));
}

/// Go to next section, or next song if at last section (Smart)
pub fn go_to_next_section_song_smart() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    info!("Go To Next Section/Song (Smart) action executed");
    
    // Get current project - check if it's FTS-ROUTING and skip if so
    let project_result = match medium_reaper.enum_projects(ProjectRef::Current, 0) {
        Some(result) => {
            // Filter out FTS-ROUTING tab (handles both hyphens and underscores, case-insensitive)
            if let Some(path) = result.file_path.as_ref() {
                if let Some(file_name) = path.as_std_path().file_stem().and_then(|s| s.to_str()) {
                    let normalized = file_name.to_uppercase().replace('_', "-");
                    if normalized == "FTS-ROUTING" {
                        warn!("Cannot navigate sections in FTS-ROUTING tab");
                        reaper.show_console_msg("Go To Next Section/Song (Smart): FTS-ROUTING tab is not a song\n");
                        return;
                    }
                }
            }
            result
        },
        None => {
            warn!("No current project");
            reaper.show_console_msg("Go To Next Section/Song (Smart): No current project\n");
            return;
        }
    };
    
    let project = Project::new(project_result.project);
    let cursor_pos = project.edit_cursor_position().unwrap_or(PositionInSeconds::ZERO);
    
    // Get sections and find current one
    let sections = get_sections_for_current_song(&project);
    
    if sections.is_empty() {
        // No sections, go to next song
        warn!("No sections found, going to next song");
        go_to_next_song();
        return;
    }
    
    let current_section = find_current_section_index(&project, cursor_pos)
        .unwrap_or(0);
    
    // Check if we're at the last section
    if current_section >= sections.len() - 1 {
        // At last section, go to next song
        info!("At last section, going to next song");
        go_to_next_song();
    } else {
        // Go to next section
        go_to_next_section();
    }
}

/// Go to previous section, or previous song if at first section (Smart)
/// When going to previous song, goes to the LAST section of that song
pub fn go_to_previous_section_song_smart() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    info!("Go To Previous Section/Song (Smart) action executed");
    
    // Get current project - check if it's FTS-ROUTING and skip if so
    let project_result = match medium_reaper.enum_projects(ProjectRef::Current, 0) {
        Some(result) => {
            // Filter out FTS-ROUTING tab (handles both hyphens and underscores, case-insensitive)
            if let Some(path) = result.file_path.as_ref() {
                if let Some(file_name) = path.as_std_path().file_stem().and_then(|s| s.to_str()) {
                    let normalized = file_name.to_uppercase().replace('_', "-");
                    if normalized == "FTS-ROUTING" {
                        warn!("Cannot navigate sections in FTS-ROUTING tab");
                        reaper.show_console_msg("Go To Previous Section/Song (Smart): FTS-ROUTING tab is not a song\n");
                        return;
                    }
                }
            }
            result
        },
        None => {
            warn!("No current project");
            reaper.show_console_msg("Go To Previous Section/Song (Smart): No current project\n");
            return;
        }
    };
    
    let project = Project::new(project_result.project);
    let cursor_pos = project.edit_cursor_position().unwrap_or(PositionInSeconds::ZERO);
    
    // Get sections and find current one
    let sections = get_sections_for_current_song(&project);
    
    if sections.is_empty() {
        // No sections, go to previous song
        warn!("No sections found, going to previous song");
        go_to_previous_song();
        return;
    }
    
    let current_section = find_current_section_index(&project, cursor_pos)
        .unwrap_or(0);
    
    // Check if we're at the first section (or before it, at Count-In)
    if current_section == 0 {
        // Check if we're at Count-In or before first section
        if let Some(count_in) = find_count_in_marker(&project) {
            let count_in_pos = PositionInSeconds::new(count_in).unwrap_or(PositionInSeconds::ZERO);
            if cursor_pos.get() <= (count_in_pos.get() + 0.01) {
                // At or before Count-In, go to previous song's LAST section
                info!("At or before Count-In, going to previous song's last section");
                
                // Build setlist to find previous song
                let setlist = match build_setlist_from_open_projects(None) {
                    Ok(s) => s,
                    Err(e) => {
                        warn!(error = %e, "Failed to build setlist for smart previous");
                        go_to_previous_song(); // Fallback to regular previous song
                        return;
                    }
                };
                
                if setlist.songs.is_empty() {
                    warn!("Setlist is empty");
                    go_to_previous_song(); // Fallback
                    return;
                }
                
                // Find current song index
                let current_project_name = {
                    let current_project = reaper.current_project();
                    let mut found_name = None;
                    for i in 0..128u32 {
                        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
                            if result.project == current_project.raw() {
                                found_name = result.file_path.as_ref()
                                    .and_then(|p| p.as_std_path().file_stem())
                                    .and_then(|s| s.to_str())
                                    .map(|s| s.to_string());
                                break;
                            }
                        }
                    }
                    found_name
                };
                
                let current_index = current_project_name.and_then(|name| {
                    setlist.songs.iter().position(|song| song.project_name_from_metadata() == name)
                }).unwrap_or(0);
                
                let prev_index = if current_index > 0 {
                    current_index - 1
                } else {
                    setlist.songs.len() - 1 // Wrap around
                };
                
                let prev_song = &setlist.songs[prev_index];
                let prev_project_name = prev_song.project_name_from_metadata();
                
                // Find tab index for previous song
                let tab_index = match find_tab_index_by_project_name(&prev_project_name) {
                    Some(idx) => idx,
                    None => {
                        warn!(project_name = %prev_project_name, "Previous song project not found");
                        go_to_previous_song(); // Fallback
                        return;
                    }
                };
                
                // Switch to previous song's tab
                if let Err(e) = switch_to_tab(tab_index) {
                    warn!(error = %e, "Failed to switch to previous song");
                    go_to_previous_song(); // Fallback
                    return;
                }
                
                // Small delay for tab switch
                std::thread::sleep(std::time::Duration::from_millis(50));
                
                // Get the previous song's project (now current)
                let prev_project_result = match medium_reaper.enum_projects(ProjectRef::Current, 0) {
                    Some(result) => result,
                    None => {
                        warn!("No current project after switching to previous song");
                        return;
                    }
                };
                let prev_project = Project::new(prev_project_result.project);
                
                // Get sections for the previous song
                let prev_sections = get_sections_for_current_song(&prev_project);
                
                if prev_sections.is_empty() {
                    // No sections in previous song, go to Count-In or 0
                    let start_pos = if let Some(count_in) = find_count_in_marker(&prev_project) {
                        PositionInSeconds::new(count_in).unwrap_or(PositionInSeconds::ZERO)
                    } else {
                        PositionInSeconds::ZERO
                    };
                    prev_project.set_edit_cursor_position(start_pos, SetEditCurPosOptions { move_view: false, seek_play: false });
                    zoom_horizontally_to_song();
                    info!("Went to previous song (no sections, at Count-In or 0)");
                    return;
                }
                
                // Go to the LAST section of the previous song
                let last_section_idx = prev_sections.len() - 1;
                let (_name, last_section_start, _end) = &prev_sections[last_section_idx];
                
                prev_project.set_edit_cursor_position(*last_section_start, SetEditCurPosOptions { move_view: false, seek_play: false });
                zoom_horizontally_to_song();
                
                info!(
                    from_song = current_index,
                    to_song = prev_index,
                    to_section = last_section_idx,
                    "Went to previous song's last section"
                );
                reaper.show_console_msg(format!(
                    "Go To Previous Section/Song (Smart): Previous song, section {}\n",
                    last_section_idx + 1
                ));
                return;
            }
        } else {
            // No Count-In, if at first section start, go to previous song's last section
            if let Some((_name, first_start, _end)) = sections.first() {
                if cursor_pos.get() <= (first_start.get() + 0.01) {
                    // Similar logic as above but without Count-In check
                    info!("At first section start (no Count-In), going to previous song's last section");
                    
                    // Build setlist to find previous song
                    let setlist = match build_setlist_from_open_projects(None) {
                        Ok(s) => s,
                        Err(e) => {
                            warn!(error = %e, "Failed to build setlist for smart previous");
                            go_to_previous_song();
                            return;
                        }
                    };
                    
                    if setlist.songs.is_empty() {
                        go_to_previous_song();
                        return;
                    }
                    
                    let current_project_name = {
                        let current_project = reaper.current_project();
                        let mut found_name = None;
                        for i in 0..128u32 {
                            if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
                                if result.project == current_project.raw() {
                                    found_name = result.file_path.as_ref()
                                        .and_then(|p| p.as_std_path().file_stem())
                                        .and_then(|s| s.to_str())
                                        .map(|s| s.to_string());
                                    break;
                                }
                            }
                        }
                        found_name
                    };
                    
                    let current_index = current_project_name.and_then(|name| {
                        setlist.songs.iter().position(|song| song.project_name_from_metadata() == name)
                    }).unwrap_or(0);
                    
                    let prev_index = if current_index > 0 {
                        current_index - 1
                    } else {
                        setlist.songs.len() - 1
                    };
                    
                    let prev_song = &setlist.songs[prev_index];
                    let prev_project_name = prev_song.project_name_from_metadata();
                    
                    let tab_index = match find_tab_index_by_project_name(&prev_project_name) {
                        Some(idx) => idx,
                        None => {
                            go_to_previous_song();
                            return;
                        }
                    };
                    
                    if let Err(_e) = switch_to_tab(tab_index) {
                        go_to_previous_song();
                        return;
                    }
                    
                    std::thread::sleep(std::time::Duration::from_millis(50));
                    
                    let prev_project_result = match medium_reaper.enum_projects(ProjectRef::Current, 0) {
                        Some(result) => result,
                        None => {
                            warn!("No current project after switching to previous song");
                            return;
                        }
                    };
                    let prev_project = Project::new(prev_project_result.project);
                    let prev_sections = get_sections_for_current_song(&prev_project);
                    
                    if prev_sections.is_empty() {
                        prev_project.set_edit_cursor_position(PositionInSeconds::ZERO, SetEditCurPosOptions { move_view: false, seek_play: false });
                        zoom_horizontally_to_song();
                        return;
                    }
                    
                    let last_section_idx = prev_sections.len() - 1;
                    let (_name, last_section_start, _end) = &prev_sections[last_section_idx];
                    prev_project.set_edit_cursor_position(*last_section_start, SetEditCurPosOptions { move_view: false, seek_play: false });
                    zoom_horizontally_to_song();
                    
                    info!(from_song = current_index, to_song = prev_index, to_section = last_section_idx, "Went to previous song's last section");
                    reaper.show_console_msg(format!("Go To Previous Section/Song (Smart): Previous song, section {}\n", last_section_idx + 1));
                    return;
                }
            }
        }
    }
    
    // Go to previous section
    go_to_previous_section();
}

