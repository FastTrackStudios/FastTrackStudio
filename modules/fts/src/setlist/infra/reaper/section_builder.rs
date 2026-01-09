//! Section Building Logic
//!
//! Handles building sections from regions within songs.

use super::parser::{extract_number_from_name, parse_section_type_from_name};
use super::stats::record_section_created;
use crate::setlist::core::{Section, SectionType, Song};
use daw::marker_region::core::{Marker, Region};
use tracing::{debug, warn};

/// Build sections from regions within a song region
pub fn build_sections_from_regions(
    all_regions: &[Region],
    song: &Song,
    song_region: &Region,
) -> Vec<Section> {
    // Get song boundaries
    let song_start = song
        .start_position()
        .map(|p| p.time.to_seconds())
        .or_else(|| song.song_region_start().map(|p| p.time.to_seconds()))
        .unwrap_or(song_region.start_seconds());
    let song_end = song
        .song_end_position()
        .map(|p| p.time.to_seconds())
        .or_else(|| song.song_region_end().map(|p| p.time.to_seconds()))
        .unwrap_or(song_region.end_seconds());

    let mut sections: Vec<Section> = all_regions
        .iter()
        .filter_map(|region| {
            // Section must be within the song region boundaries
            if region.start_seconds() < song_region.start_seconds()
                || region.end_seconds() > song_region.end_seconds()
            {
                return None;
            }

            // Don't include the song region itself
            if region.start_seconds() == song_region.start_seconds()
                && region.end_seconds() == song_region.end_seconds()
            {
                return None;
            }

            // Section must be within song boundaries (SONGSTART to SONGEND)
            if region.start_seconds() < song_start || region.end_seconds() > song_end {
                return None;
            }

            build_section_from_region(region)
        })
        .collect();

    // Add count-in section if count-in marker exists and is before song start
    if let Some(ref count_in_marker) = song.count_in_marker {
        let count_in_start = count_in_marker.position.time.to_seconds();
        let song_start_pos = song
            .start_position()
            .map(|p| p.time.to_seconds())
            .or_else(|| song.song_region_start().map(|p| p.time.to_seconds()))
            .unwrap_or(song_start);

        if count_in_start < song_start_pos {
            // Create count-in section from count-in marker to song start
            let count_in_start_pos = count_in_marker.position.clone();
            let song_start_pos_clone = song
                .start_marker
                .as_ref()
                .map(|m| m.position.clone())
                .or_else(|| {
                    song.song_region_start_marker
                        .as_ref()
                        .map(|m| m.position.clone())
                })
                .unwrap_or_else(|| {
                    // Fallback: create a marker from seconds and use its position
                    Marker::from_seconds(song_start_pos, "SONGSTART".to_string()).position
                });

            if let Some(count_in_section) =
                create_count_in_section(count_in_start_pos, song_start_pos_clone)
            {
                sections.push(count_in_section);
            }
        }
    }

    // Add end section (from SONGEND to =END) if both exist
    if let Some(ref song_end_marker) = song.song_end_marker {
        let end_marker = song
            .end_marker
            .as_ref()
            .or_else(|| song.render_end_marker.as_ref());
        if let Some(end_marker) = end_marker {
            let song_end_pos = song_end_marker.position.time.to_seconds();
            let end_pos = end_marker.position.time.to_seconds();

            if song_end_pos < end_pos {
                if let Some(end_section) = create_end_section(
                    song_end_marker.position.clone(),
                    end_marker.position.clone(),
                ) {
                    sections.push(end_section);
                }
            }
        }
    }

    // Sort sections by start position
    sections.sort_by(|a, b| {
        a.start_seconds()
            .partial_cmp(&b.start_seconds())
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    sections
}

/// Build a section from a region
fn build_section_from_region(region: &Region) -> Option<Section> {
    // Try to parse section type from region name
    let section_type = parse_section_type_from_name(&region.name);

    // If we couldn't parse a section type, use Custom to indicate it's an unrecognized section
    let section_type = section_type.unwrap_or_else(|| SectionType::Custom(region.name.clone()));

    // Extract number from name if present (but not for Custom sections)
    let number = if matches!(section_type, SectionType::Custom(_)) {
        None // Custom sections should not have numbers extracted
    } else {
        extract_number_from_name(&region.name)
    };

    // Create section with musical positions
    let start_pos = region.time_range.start.clone();
    let end_pos = region.time_range.end.clone();

    match Section::new_with_positions(
        section_type,
        start_pos,
        end_pos,
        region.name.clone(),
        number,
    ) {
        Ok(mut section) => {
            // Store color from region directly in section
            section.color = region.color;
            // Log color for debugging
            if let Some(color_val) = region.color {
                debug!(
                    section_name = ?section.name,
                    region_name = %region.name,
                    color = color_val,
                    "Stored color in section"
                );
            }
            // Calculate and store length_measures in metadata (computed field, not stored in struct)
            if let Some(length_measures) = section.length_measures() {
                section
                    .metadata
                    .insert("length_measures".to_string(), length_measures.to_string());
            }
            record_section_created();
            Some(section)
        }
        Err(e) => {
            warn!(error = %e, name = %region.name, "Failed to create section");
            None
        }
    }
}

/// Create a count-in section
fn create_count_in_section(
    count_in_start_pos: daw::primitives::Position,
    song_start_pos: daw::primitives::Position,
) -> Option<Section> {
    match Section::new_with_positions(
        SectionType::Custom("Count-In".to_string()),
        count_in_start_pos,
        song_start_pos,
        "Count-In".to_string(),
        None,
    ) {
        Ok(mut count_in_section) => {
            // Calculate and store length_measures in metadata
            if let Some(length_measures) = count_in_section.length_measures() {
                count_in_section
                    .metadata
                    .insert("length_measures".to_string(), length_measures.to_string());
            }
            record_section_created();
            Some(count_in_section)
        }
        Err(e) => {
            warn!(error = %e, "Failed to create count-in section");
            None
        }
    }
}

/// Create an end section (from SONGEND to =END)
fn create_end_section(
    song_end_pos: daw::primitives::Position,
    end_pos: daw::primitives::Position,
) -> Option<Section> {
    match Section::new_with_positions(
        SectionType::Custom("End".to_string()),
        song_end_pos,
        end_pos,
        "End".to_string(),
        None,
    ) {
        Ok(mut end_section) => {
            // Calculate and store length_measures in metadata
            if let Some(length_measures) = end_section.length_measures() {
                end_section
                    .metadata
                    .insert("length_measures".to_string(), length_measures.to_string());
            }
            record_section_created();
            Some(end_section)
        }
        Err(e) => {
            warn!(error = %e, "Failed to create end section");
            None
        }
    }
}

/// Build sections from regions (simple version for projects without song regions)
pub fn build_sections_from_regions_simple(regions: &[Region], song: &Song) -> Vec<Section> {
    let song_start = song
        .start_position()
        .map(|p| p.time.to_seconds())
        .or_else(|| song.song_region_start().map(|p| p.time.to_seconds()))
        .unwrap_or(0.0);
    let song_end = song
        .song_end_position()
        .map(|p| p.time.to_seconds())
        .or_else(|| song.song_region_end().map(|p| p.time.to_seconds()))
        .unwrap_or(f64::MAX);

    let mut sections: Vec<Section> = regions
        .iter()
        .filter_map(|region| {
            // Section must be within the song boundaries
            if region.start_seconds() < song_start || region.end_seconds() > song_end {
                return None;
            }

            // Don't include the song region itself
            if let Some(song_region_start) = song.song_region_start() {
                if let Some(song_region_end) = song.song_region_end() {
                    if region.start_seconds() == song_region_start.time.to_seconds()
                        && region.end_seconds() == song_region_end.time.to_seconds()
                    {
                        return None;
                    }
                }
            }

            build_section_from_region(region)
        })
        .collect();

    // Add count-in section if count-in marker exists and is before song start
    if let Some(ref count_in_marker) = song.count_in_marker {
        let count_in_start = count_in_marker.position.time.to_seconds();
        let song_start_pos = song
            .start_position()
            .map(|p| p.time.to_seconds())
            .or_else(|| song.song_region_start().map(|p| p.time.to_seconds()))
            .unwrap_or(0.0);

        if count_in_start < song_start_pos {
            // Create count-in section from count-in marker to song start
            let count_in_start_pos = count_in_marker.position.clone();
            let song_start_pos_clone = song
                .start_marker
                .as_ref()
                .map(|m| m.position.clone())
                .or_else(|| {
                    song.song_region_start_marker
                        .as_ref()
                        .map(|m| m.position.clone())
                })
                .unwrap_or_else(|| {
                    // Fallback: create a marker from seconds and use its position
                    Marker::from_seconds(song_start_pos, "SONGSTART".to_string()).position
                });

            if let Some(count_in_section) =
                create_count_in_section(count_in_start_pos, song_start_pos_clone)
            {
                sections.push(count_in_section);
            }
        }
    }

    // Add end section (from SONGEND to =END) if both exist
    if let Some(ref song_end_marker) = song.song_end_marker {
        let end_marker = song
            .end_marker
            .as_ref()
            .or_else(|| song.render_end_marker.as_ref());
        if let Some(end_marker) = end_marker {
            let song_end_pos = song_end_marker.position.time.to_seconds();
            let end_pos = end_marker.position.time.to_seconds();

            if song_end_pos < end_pos {
                if let Some(end_section) = create_end_section(
                    song_end_marker.position.clone(),
                    end_marker.position.clone(),
                ) {
                    sections.push(end_section);
                }
            }
        }
    }

    // Sort sections by start position
    sections.sort_by(|a, b| {
        a.start_seconds()
            .partial_cmp(&b.start_seconds())
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    sections
}
