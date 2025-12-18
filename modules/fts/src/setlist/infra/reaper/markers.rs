//! REAPER Marker and Region Reading
//!
//! Reads markers and regions from REAPER projects and converts them to marker-region structs.

use daw::marker_region::core::{Marker, Region};
use reaper_high::{Project, BookmarkType, Reaper};
use reaper_medium::PositionInSeconds;
use daw::primitives::{Position, TimePosition, MusicalPosition, TimeRange};
use super::color_utils::extract_color_from_native;

/// Get the project measure offset for a project
/// Returns the offset value, or 0 if not found
fn get_project_measure_offset(project: &Project) -> i32 {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    // Get the project measure offset using project_config_var_get_offs
    if let Some(offs_result) = medium_reaper.project_config_var_get_offs("projmeasoffs") {
        // Get the actual value using the offset
        if let Some(addr) = medium_reaper.project_config_var_addr(project.context(), offs_result.offset) {
            // Read the integer value directly from the pointer (it's a 32-bit integer)
            unsafe { *(addr.as_ptr() as *const i32) }
        } else {
            0
        }
    } else {
        0
    }
}

/// Read all markers from a REAPER project
pub fn read_markers_from_project(project: &Project) -> Result<Vec<Marker>, String> {
    let bookmark_count = project.bookmark_count();
    let mut markers = Vec::new();

    // Enumerate all bookmarks and extract markers (BookmarkType::Marker)
    for i in 0..bookmark_count.total_count {
        if let Some(bookmark) = project.find_bookmark_by_index(i) {
            let info = bookmark.basic_info();

            // Only include markers (not regions - regions have an end position)
            if info.bookmark_type() == BookmarkType::Marker {
                // PositionInSeconds is a newtype wrapper - convert to f64
                let position_seconds: f64 = info.position.into();

                // Get name - convert ReaperString to String and clean it up
                let name = bookmark.name().to_string().trim_matches('"').trim().to_string();

                // Get color if available - convert NativeColor to RGB and then to u32
                let color = extract_color_from_native(info.color);

                // Get ID if available (convert BookmarkId to Option<u32>)
                let id = Some(info.id.get() as u32);

                // Convert time position to musical position using REAPER's beat info
                let time_pos = TimePosition::from_seconds(position_seconds);
                
                // Get musical position using REAPER's beat info
                let position_pos = PositionInSeconds::new(position_seconds)
                    .map_err(|e| format!("Invalid position: {:?}", e))?;
                let beat_info = project.beat_info_at(position_pos);
                let measure_offset = get_project_measure_offset(project);
                let measure = beat_info.measure_index + measure_offset; // Apply project measure offset
                let beats_since_measure = beat_info.beats_since_measure.get();
                let beat = beats_since_measure.floor() as i32;
                let subdivision = ((beats_since_measure - beats_since_measure.floor()) * 1000.0).round() as i32;
                let subdivision = subdivision.max(0).min(999);
                
                let musical = MusicalPosition::try_new(measure, beat, subdivision)
                    .map_err(|e| format!("Invalid musical position: {}", e))?;
                
                let position = Position::new(musical, time_pos);

                // Create marker with full metadata
                let marker = Marker::new_full(
                    id,
                    position,
                    name,
                    color,
                    None, // flags - not available from basic_info
                    None, // locked - not available from basic_info
                    None, // guid - not available from basic_info
                );

                markers.push(marker);
            }
        }
    }

    // Sort markers by position
    markers.sort_by(|a, b| {
        a.position_seconds()
            .partial_cmp(&b.position_seconds())
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    Ok(markers)
}

/// Read all regions from a REAPER project
pub fn read_regions_from_project(project: &Project) -> Result<Vec<Region>, String> {
    let bookmark_count = project.bookmark_count();
    let mut regions = Vec::new();

    // Enumerate all bookmarks and extract regions (BookmarkType::Region)
    for i in 0..bookmark_count.total_count {
        if let Some(bookmark) = project.find_bookmark_by_index(i) {
            let info = bookmark.basic_info();

            // Only include regions (BookmarkType::Region, which have an end position)
            if info.bookmark_type() == BookmarkType::Region {
                if let Some(end_position) = info.region_end_position {
                    // PositionInSeconds is a newtype wrapper - convert to f64
                    let start_position_seconds: f64 = info.position.into();
                    let end_position_seconds: f64 = end_position.into();

                    // Get name - convert ReaperString to String and clean it up
                    let name = bookmark.name().to_string().trim_matches('"').trim().to_string();

                    // Get color if available - convert NativeColor to RGB and then to u32
                    let color = extract_color_from_native(info.color);

                    // Get ID if available (convert BookmarkId to Option<u32>)
                    let id = Some(info.id.get() as u32);

                    // Convert time positions to musical positions using REAPER's beat info
                    let start_time_pos = TimePosition::from_seconds(start_position_seconds);
                    let end_time_pos = TimePosition::from_seconds(end_position_seconds);
                    
                    // Get musical position for start
                    let start_position_pos = PositionInSeconds::new(start_position_seconds)
                        .map_err(|e| format!("Invalid start position: {:?}", e))?;
                    let start_beat_info = project.beat_info_at(start_position_pos);
                    let measure_offset = get_project_measure_offset(project);
                    let start_measure = start_beat_info.measure_index + measure_offset; // Apply project measure offset
                    let start_beats_since_measure = start_beat_info.beats_since_measure.get();
                    let start_beat = start_beats_since_measure.floor() as i32;
                    let start_subdivision = ((start_beats_since_measure - start_beats_since_measure.floor()) * 1000.0).round() as i32;
                    let start_subdivision = start_subdivision.max(0).min(999);
                    
                    let start_musical = MusicalPosition::try_new(start_measure, start_beat, start_subdivision)
                        .map_err(|e| format!("Invalid musical position: {}", e))?;
                    let start_position = Position::new(start_musical, start_time_pos);
                    
                    // Get musical position for end
                    let end_position_pos = PositionInSeconds::new(end_position_seconds)
                        .map_err(|e| format!("Invalid end position: {:?}", e))?;
                    let end_beat_info = project.beat_info_at(end_position_pos);
                    let end_measure = end_beat_info.measure_index + measure_offset; // Apply project measure offset
                    let end_beats_since_measure = end_beat_info.beats_since_measure.get();
                    let end_beat = end_beats_since_measure.floor() as i32;
                    let end_subdivision = ((end_beats_since_measure - end_beats_since_measure.floor()) * 1000.0).round() as i32;
                    let end_subdivision = end_subdivision.max(0).min(999);
                    
                    let end_musical = MusicalPosition::try_new(end_measure, end_beat, end_subdivision)
                        .map_err(|e| format!("Invalid musical position: {}", e))?;
                    let end_position = Position::new(end_musical, end_time_pos);
                    
                    let time_range = TimeRange::new(start_position, end_position);

                    // Create region with full metadata
                    let region = Region::new_full(
                        id,
                        time_range,
                        name,
                        color,
                        None, // flags - not available from basic_info
                        None, // locked - not available from basic_info
                        None, // guid - not available from basic_info
                    )
                    .map_err(|e| format!("Failed to create region: {}", e))?;

                    regions.push(region);
                }
            }
        }
    }

    // Sort regions by start position
    regions.sort_by(|a, b| {
        a.start_seconds()
            .partial_cmp(&b.start_seconds())
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    Ok(regions)
}
