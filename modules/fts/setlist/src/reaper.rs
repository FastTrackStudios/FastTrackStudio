//! REAPER API implementation of SetlistSource
//!
//! This module provides a REAPER-specific implementation of SetlistSource
//! using the ReaperMarkerRegionSource.

use crate::setlist::{Setlist, SetlistSource, Song, Section, SectionType};
use crate::marker_region::{MarkerRegionSource, ReaperMarkerRegionSource, Marker, Region};

/// REAPER API implementation of SetlistSource
pub struct ReaperSetlistSource;

impl ReaperSetlistSource {
    /// Create a new REAPER setlist source
    pub fn new() -> Self {
        Self
    }


    /// Try to parse a section type from a region name
    fn parse_section_type_from_name(name: &str) -> Option<SectionType> {
        // Try to extract section type from region name
        // Examples: "Verse 1", "Chorus", "VS 2", "CH", "Intro", etc.
        let name_lower = name.to_lowercase();
        let name_trimmed = name_lower.trim();
        
        // Try direct match first
        if let Some(section_type) = SectionType::from_str(name_trimmed) {
            return Some(section_type);
        }

        // Try to extract section type from name with number
        // Split by whitespace and try each part
        let parts: Vec<&str> = name_trimmed.split_whitespace().collect();
        if parts.is_empty() {
            return None;
        }

        // Try first part as section type
        if let Some(section_type) = SectionType::from_str(parts[0]) {
            return Some(section_type);
        }

        // Try first two parts combined (e.g., "Pre-Chorus")
        if parts.len() >= 2 {
            let combined = format!("{}-{}", parts[0], parts[1]);
            if let Some(section_type) = SectionType::from_str(&combined) {
                return Some(section_type);
            }
        }

        None
    }

    /// Build a song from markers and regions
    fn build_song(
        song_region: Region,
        all_markers: &[Marker],
        all_regions: &[Region],
    ) -> Result<Song, Box<dyn std::error::Error>> {
        let song_name = song_region.name.clone();
        let mut song = Song::new(song_name);
        song.song_region = Some(song_region.clone());

        // Find special markers for this song
        // Count-In marker
        let count_in = all_markers.iter()
            .find(|m| {
                m.name.trim().eq_ignore_ascii_case("Count-In") &&
                m.position_seconds() >= song_region.start_seconds() &&
                m.position_seconds() <= song_region.end_seconds()
            });
        if let Some(marker) = count_in {
            song.count_in_position = Some(marker.position_seconds());
        }

        // SONGSTART marker
        let start_marker = all_markers.iter()
            .find(|m| {
                m.name.trim().eq_ignore_ascii_case("SONGSTART") &&
                m.position_seconds() >= song_region.start_seconds() &&
                m.position_seconds() <= song_region.end_seconds()
            });
        if let Some(marker) = start_marker {
            song.start_position = Some(marker.position_seconds());
        }

        // SONGEND marker
        let song_end = all_markers.iter()
            .find(|m| {
                m.name.trim().eq_ignore_ascii_case("SONGEND") &&
                m.position_seconds() >= song_region.start_seconds() &&
                m.position_seconds() <= song_region.end_seconds()
            });
        if let Some(marker) = song_end {
            song.song_end_position = Some(marker.position_seconds());
        }

        // =END marker
        let end_marker = all_markers.iter()
            .find(|m| {
                m.name.trim() == "=END" &&
                m.position_seconds() >= song_region.start_seconds() &&
                m.position_seconds() <= song_region.end_seconds()
            });
        if let Some(marker) = end_marker {
            song.end_position = Some(marker.position_seconds());
        }

        // Find sections (regions within the song, up to SONGEND)

        let mut sections: Vec<Section> = all_regions.iter()
            .filter_map(|region| {
                // Section must be within the song region
                if region.start_seconds() < song_region.start_seconds() ||
                   region.end_seconds() > song_region.end_seconds() {
                    return None;
                }

                // Don't include the song region itself
                if region.start_seconds() == song_region.start_seconds() &&
                   region.end_seconds() == song_region.end_seconds() {
                    return None;
                }

                // Try to parse section type from region name
                let section_type = Self::parse_section_type_from_name(&region.name)
                    .unwrap_or(SectionType::Instrumental); // Default to Instrumental if can't parse

                // If SONGEND exists, section end must be <= SONGEND
                if let Some(song_end_pos) = song.song_end_position {
                    if region.end_seconds() > song_end_pos {
                        return None;
                    }
                }

                // Try to extract number from name (e.g., "Verse 1", "CH 2")
                let name_lower = region.name.to_lowercase();
                let parts: Vec<&str> = name_lower.split_whitespace().collect();
                let mut number = None;

                // Look for number in name
                for part in &parts {
                    if let Ok(num) = part.parse::<u32>() {
                        number = Some(num);
                        break;
                    }
                    // Also check if number follows section type abbreviation
                    if part.len() > 2 {
                        if let Ok(num) = part[2..].parse::<u32>() {
                            number = Some(num);
                            break;
                        }
                    }
                }

                Some(Section::new(
                    section_type,
                    region.start_seconds(),
                    region.end_seconds(),
                    region.name.clone(),
                    number,
                ))
            })
            .collect();

        // Sort sections by start position
        sections.sort_by(|a, b| a.start_seconds.partial_cmp(&b.start_seconds).unwrap_or(std::cmp::Ordering::Equal));

        // If we couldn't parse section types from names, try to infer from position
        // First section is likely Intro, last section is likely Outro
        if !sections.is_empty() {
            // Check first section - if it's Instrumental (default), try to infer Intro
            if let Some(first_section) = sections.first_mut() {
                if first_section.section_type == SectionType::Instrumental {
                    // Check if name contains any intro-related terms
                    let name_lower = first_section.name.to_lowercase();
                    if name_lower.contains("intro") || name_lower.contains("introduction") ||
                       name_lower.contains("in") || name_lower.contains("start") {
                        first_section.section_type = SectionType::Intro;
                    } else if first_section.start_seconds <= song.start_position.unwrap_or(f64::MAX) {
                        // If it starts very close to the song start, likely an intro
                        if let Some(start_pos) = song.start_position {
                            let distance = (first_section.start_seconds - start_pos).abs();
                            if distance < 10.0 { // Within 10 seconds of SONGSTART
                                first_section.section_type = SectionType::Intro;
                            }
                        }
                    }
                }
            }
            
            // Check last section - if it's Instrumental (default), try to infer Outro
            if let Some(last_section) = sections.last_mut() {
                if last_section.section_type == SectionType::Instrumental {
                    // Check if name contains any outro-related terms
                    let name_lower = last_section.name.to_lowercase();
                    if name_lower.contains("outro") || name_lower.contains("outroduction") ||
                       name_lower.contains("out") || name_lower.contains("end") {
                        last_section.section_type = SectionType::Outro;
                    } else if let Some(song_end) = song.song_end_position {
                        // If it ends very close to SONGEND, likely an outro
                        let distance = (last_section.end_seconds - song_end).abs();
                        if distance < 10.0 { // Within 10 seconds of SONGEND
                            last_section.section_type = SectionType::Outro;
                        }
                    }
                }
            }
        }

        song.sections = sections;

        // Auto-number sections
        song.auto_number_sections();

        Ok(song)
    }

    /// Find song regions by identifying regions that contain other regions (sections)
    /// 
    /// A song region is the largest region that contains multiple other regions within it.
    /// This detects parent regions that encompass sections.
    fn find_song_regions(all_regions: &[Region]) -> Vec<Region> {
        let mut song_regions = Vec::new();
        
        for potential_song_region in all_regions {
            // Count how many other regions are contained within this region
            let contained_regions: Vec<&Region> = all_regions.iter()
                .filter(|other_region| {
                    // Don't count self
                    if other_region.start_seconds() == potential_song_region.start_seconds() &&
                       other_region.end_seconds() == potential_song_region.end_seconds() {
                        return false;
                    }
                    
                    // Check if other_region is contained within potential_song_region
                    // A region is contained if it starts after or at the song region start
                    // and ends before or at the song region end
                    other_region.start_seconds() >= potential_song_region.start_seconds() &&
                    other_region.end_seconds() <= potential_song_region.end_seconds()
                })
                .collect();
            
            // If this region contains at least 2 other regions (sections), it's likely a song
            // Also check if it's significantly larger than the sections it contains
            if contained_regions.len() >= 2 {
                let song_duration = potential_song_region.end_seconds() - potential_song_region.start_seconds();
                let avg_section_duration: f64 = contained_regions.iter()
                    .map(|r| r.end_seconds() - r.start_seconds())
                    .sum::<f64>() / contained_regions.len() as f64;
                
                // Song region should be at least 1.5x the average section duration
                // This ensures we're identifying a container, not just a larger section
                if song_duration >= avg_section_duration * 1.5 {
                    song_regions.push(potential_song_region.clone());
                }
            }
        }
        
        // Sort by start position for consistency
        song_regions.sort_by(|a, b| a.start_seconds().partial_cmp(&b.start_seconds()).unwrap_or(std::cmp::Ordering::Equal));
        
        song_regions
    }
    
    /// Find the best song region for a given SONGSTART marker by finding the largest containing region
    fn find_song_region_for_marker(start_marker: &Marker, all_regions: &[Region]) -> Option<Region> {
        // Find all regions that contain this marker
        let containing_regions: Vec<&Region> = all_regions.iter()
            .filter(|r| {
                r.start_seconds() <= start_marker.position_seconds() &&
                r.end_seconds() >= start_marker.position_seconds()
            })
            .collect();
        
        if containing_regions.is_empty() {
            return None;
        }
        
        // If only one, use it
        if containing_regions.len() == 1 {
            return Some(containing_regions[0].clone());
        }
        
        // If multiple, find the one that contains the most other regions (sections)
        // This is the parent container (song region)
        let mut best_region: Option<&Region> = None;
        let mut max_contained_count = 0;
        
        for candidate in &containing_regions {
            let contained_count = all_regions.iter()
                .filter(|other| {
                    // Don't count self
                    if other.start_seconds() == candidate.start_seconds() &&
                       other.end_seconds() == candidate.end_seconds() {
                        return false;
                    }
                    
                    // Check if contained
                    other.start_seconds() >= candidate.start_seconds() &&
                    other.end_seconds() <= candidate.end_seconds()
                })
                .count();
            
            if contained_count > max_contained_count {
                max_contained_count = contained_count;
                best_region = Some(candidate);
            } else if contained_count == max_contained_count && max_contained_count > 0 {
                // If tied, prefer the larger one (longer duration)
                if let Some(current_best) = best_region {
                    let candidate_duration = candidate.end_seconds() - candidate.start_seconds();
                    let best_duration = current_best.end_seconds() - current_best.start_seconds();
                    if candidate_duration > best_duration {
                        best_region = Some(candidate);
                    }
                }
            }
        }
        
        best_region.map(|r| r.clone())
    }
}

impl SetlistSource for ReaperSetlistSource {
    fn build_setlist(&self) -> Result<Setlist, Box<dyn std::error::Error>> {
        let marker_source = ReaperMarkerRegionSource::new();
        let markers = marker_source.get_markers()?;
        let regions = marker_source.get_regions()?;

        let mut setlist = Setlist::new();

        // Strategy: Find song regions first, then build songs from them
        // A song region is typically a region that doesn't match a section type
        // OR we can look for regions that contain SONGSTART and SONGEND markers
        
        // First, try to find song regions (regions that contain SONGSTART markers)
        let start_markers: Vec<&Marker> = markers.iter()
            .filter(|m| m.name.trim().eq_ignore_ascii_case("SONGSTART"))
            .collect();

        if !start_markers.is_empty() {
            // Build songs based on SONGSTART markers
            for start_marker in &start_markers {
                // Find the song region that contains this SONGSTART marker
                // This will be the largest region that contains the most sections
                let song_region = Self::find_song_region_for_marker(start_marker, &regions);

                if let Some(region) = song_region {
                    match Self::build_song(region.clone(), &markers, &regions) {
                        Ok(song) => setlist.add_song(song),
                        Err(e) => eprintln!("Warning: Failed to build song from region {}: {}", region.name, e),
                    }
                } else {
                    // No song region found, create a song from markers alone
                    // Look for SONGEND or =END to find the song boundaries
                    let song_end = markers.iter()
                        .find(|m| {
                            (m.name.trim().eq_ignore_ascii_case("SONGEND") || m.name.trim() == "=END") &&
                            m.position_seconds() >= start_marker.position_seconds()
                        });

                    let end_pos = song_end.map(|m| m.position_seconds())
                        .unwrap_or_else(|| start_marker.position_seconds() + 120.0); // Default 2 minutes

                    // Create a synthetic region for this song
                    let synthetic_region = Region::from_seconds(
                        start_marker.position_seconds() - 4.0, // Assume 4 seconds before SONGSTART for song start
                        end_pos,
                        "Song".to_string(),
                    );

                    match Self::build_song(synthetic_region, &markers, &regions) {
                        Ok(song) => setlist.add_song(song),
                        Err(e) => eprintln!("Warning: Failed to build song from markers: {}", e),
                    }
                }
            }
        } else {
            // No SONGSTART markers found, try to infer songs from regions
            // Use all regions as potential song regions and let build_song figure it out
            let song_regions = Self::find_song_regions(&regions);
            
            for region in song_regions {
                match Self::build_song(region, &markers, &regions) {
                    Ok(song) => setlist.add_song(song),
                    Err(e) => eprintln!("Warning: Failed to build song: {}", e),
                }
            }
        }

        Ok(setlist)
    }

    fn source_name(&self) -> &'static str {
        "REAPER API"
    }
}

impl Default for ReaperSetlistSource {
    fn default() -> Self {
        Self::new()
    }
}

