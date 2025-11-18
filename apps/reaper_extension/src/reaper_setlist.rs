//! REAPER Setlist Building
//!
//! Builds setlists from open REAPER projects, reading markers and regions from each project.

use marker_region::core::{Marker, Region};
use reaper_high::{Project, Reaper};
use reaper_medium::ProjectRef;
use setlist::core::{Section, SectionType, Setlist, SetlistError, Song};
use tracing::{debug, info, warn};

use crate::reaper_markers::{read_markers_from_project, read_regions_from_project};
use crate::reaper_project::create_reaper_project_wrapper;
use marker_region::application::TempoTimePoint;

/// Read all tempo/time signature markers from a REAPER project
/// Returns Vec<(time_position_seconds, tempo_bpm, time_signature_option)>
#[allow(unsafe_code)] // Required for low-level REAPER API
fn read_tempo_time_sig_markers_from_project(project: &Project) -> Vec<(f64, f64, Option<(i32, i32)>)> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let project_context = project.context();
    
    let marker_count = medium_reaper.count_tempo_time_sig_markers(project_context) as i32;
    let mut markers = Vec::new();
    
    for i in 0..marker_count {
        let mut timepos_out: f64 = 0.0;
        let mut _measurepos_out: i32 = 0;
        let mut _beatpos_out: f64 = 0.0;
        let mut bpm_out: f64 = 0.0;
        let mut timesig_num_out: i32 = 0;
        let mut timesig_denom_out: i32 = 0;
        let mut _lineartempo_out: bool = false;
        
        let success = unsafe {
            medium_reaper.low().GetTempoTimeSigMarker(
                project_context.to_raw(),
                i,
                &mut timepos_out,
                &mut _measurepos_out,
                &mut _beatpos_out,
                &mut bpm_out,
                &mut timesig_num_out,
                &mut timesig_denom_out,
                &mut _lineartempo_out,
            )
        };
        
        if success {
            let time_sig = if timesig_num_out > 0 && timesig_denom_out > 0 {
                Some((timesig_num_out, timesig_denom_out))
            } else {
                None
            };
            markers.push((timepos_out, bpm_out, time_sig));
        }
    }
    
    // Sort by time position
    markers.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
    
    markers
}

/// Parse a song name that may be in the format "Song Name" - Artist or Song Name - Artist
/// Returns (song_name, artist_option)
fn parse_song_name(full_name: &str) -> (String, Option<String>) {
    let trimmed = full_name.trim();
    
    // Look for " - " separator
    if let Some(separator_pos) = trimmed.find(" - ") {
        let song_part = trimmed[..separator_pos].trim();
        let artist_part = trimmed[separator_pos + 3..].trim();
        
        // Remove quotes from song name if present
        let song_name = if song_part.starts_with('"') && song_part.ends_with('"') {
            song_part[1..song_part.len() - 1].trim().to_string()
        } else {
            song_part.to_string()
        };
        
        let artist = if artist_part.is_empty() {
            None
        } else {
            Some(artist_part.to_string())
        };
        
        (song_name, artist)
    } else {
        // No separator found, treat entire string as song name
        let song_name = if trimmed.starts_with('"') && trimmed.ends_with('"') {
            trimmed[1..trimmed.len() - 1].trim().to_string()
        } else {
            trimmed.to_string()
        };
        
        (song_name, None)
    }
}

/// Build a song from the current active project
pub fn build_song_from_current_project() -> Result<Song, SetlistError> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let current_project = reaper.current_project();
    let current_project_raw = current_project.raw();
    
    // Find the current project's tab index and file path
    let mut current_tab_index = None;
    let mut current_file_path: Option<std::path::PathBuf> = None;
    
    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            if result.project == current_project_raw {
                current_tab_index = Some(i);
                current_file_path = result.file_path.as_ref().map(|p| p.as_std_path().to_path_buf());
                break;
            }
        }
    }
    
    let project_name = current_file_path
        .as_ref()
        .and_then(|p| p.file_stem())
        .and_then(|s| s.to_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| "Current Project".to_string());
    
    // Read markers and regions from current project
    let markers = read_markers_from_project(&current_project)
        .map_err(|e| SetlistError::validation_error(format!("Failed to read markers: {}", e)))?;
    let regions = read_regions_from_project(&current_project)
        .map_err(|e| SetlistError::validation_error(format!("Failed to read regions: {}", e)))?;
    
    // Build songs from markers and regions (should be just one for current project)
    let songs = build_songs_from_project(
        &project_name,
        &markers,
        &regions,
        &current_project,
        current_file_path.as_deref(),
    );
    
    // Return the first song, or an error if none found
    songs.into_iter().next().unwrap_or_else(|| {
        Err(SetlistError::validation_error("No songs found in current project".to_string()))
    })
}

/// Build a setlist from all open REAPER projects
pub fn build_setlist_from_open_projects() -> Result<Setlist, SetlistError> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let mut setlist = Setlist::new("REAPER Setlist".to_string())?;

    info!("Building setlist from open REAPER projects...");

    // Enumerate all project tabs
    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            // Get project name first to check for FTS-ROUTING
            let project_name = if let Some(file_path) = result.file_path.as_ref() {
                file_path
                    .as_std_path()
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| format!("Tab {}", i))
            } else {
                format!("Tab {}", i)
            };
            
            // Skip FTS-ROUTING tab (case-insensitive, check both file name and project name)
            let normalized_name = project_name.to_uppercase().replace('_', "-");
            if normalized_name == "FTS-ROUTING" {
                debug!(tab_index = i, project_name = %project_name, "Skipping FTS-ROUTING tab");
                continue;
            }

            let project = Project::new(result.project);

            debug!(tab_index = i, project_name = %project_name, "Processing project");

            // Read markers and regions from this project
            let markers = read_markers_from_project(&project)
                .map_err(|e| SetlistError::validation_error(format!("Failed to read markers: {}", e)))?;
            let regions = read_regions_from_project(&project)
                .map_err(|e| SetlistError::validation_error(format!("Failed to read regions: {}", e)))?;

            // Build songs from this project (can have multiple songs if multiple song regions)
            let songs = build_songs_from_project(
                &project_name,
                &markers,
                &regions,
                &project,
                result.file_path.as_ref().map(|p| p.as_std_path()),
            );
            
            for song_result in songs {
                match song_result {
                    Ok(song) => {
                        debug!(song_name = %song.name, project_name = %project_name, "Built song from project");
                        if let Err(e) = setlist.add_song(song) {
                            warn!(error = %e, project_name = %project_name, "Failed to add song to setlist");
                        }
                    }
                    Err(e) => {
                        warn!(error = %e, project_name = %project_name, "Failed to build song from project");
                    }
                }
            }
        } else {
            break; // No more tabs
        }
    }

    info!(song_count = setlist.song_count(), "Built setlist with {} songs", setlist.song_count());
    Ok(setlist)
}

/// Build multiple songs from markers and regions in a project
/// A project can have multiple songs if it has multiple song regions (each with a SONGSTART or =START marker)
fn build_songs_from_project(
    project_name: &str,
    markers: &[Marker],
    regions: &[Region],
    project: &Project,
    project_path: Option<&std::path::Path>,
) -> Vec<Result<Song, SetlistError>> {
    let mut songs = Vec::new();
    
    // Find all SONGSTART markers
    let songstart_markers: Vec<&Marker> = markers
        .iter()
        .filter(|m| m.name.trim().eq_ignore_ascii_case("SONGSTART"))
        .collect();
    
    // Find all =START markers (RENDERSTART) that don't have a corresponding SONGSTART
    // These can also indicate song starts
    let renderstart_markers: Vec<&Marker> = markers
        .iter()
        .filter(|m| {
            m.name.trim() == "=START" &&
            !songstart_markers.iter().any(|sm| {
                // Check if there's a SONGSTART near this =START (within 1 second)
                (sm.position_seconds() - m.position_seconds()).abs() < 1.0
            })
        })
        .collect();
    
    // Combine both types of start markers (SONGSTART takes precedence)
    let mut start_markers = songstart_markers;
    start_markers.extend(renderstart_markers);
    
    if !start_markers.is_empty() {
        // Build songs based on SONGSTART markers
        for start_marker in start_markers {
            // Find the song region that contains this SONGSTART marker
            let song_region = find_song_region_for_marker(start_marker, markers, regions);
            
            match song_region {
                Some(region) => {
                    // Build song from this region
                    match build_song_from_region(
                        &region.name,
                        start_marker,
                        &region,
                        markers,
                        regions,
                        project,
                        project_path,
                        Some(project_name),
                    ) {
                        Ok(song) => songs.push(Ok(song)),
                        Err(e) => {
                            warn!(error = %e, region_name = %region.name, "Failed to build song from region");
                            songs.push(Err(e));
                        }
                    }
                }
                None => {
                    // No song region found, create a song from markers alone
                    // Look for SONGEND or =END to find the song boundaries
                    let song_end = markers.iter().find(|m| {
                        (m.name.trim().eq_ignore_ascii_case("SONGEND") || m.name.trim() == "=END") &&
                        m.position_seconds() >= start_marker.position_seconds()
                    });
                    
                    let end_pos = song_end.map(|m| m.position_seconds())
                        .unwrap_or_else(|| start_marker.position_seconds() + 120.0); // Default 2 minutes
                    
                    // Create a synthetic region for this song
                    let synthetic_region = Region::from_seconds(
                        start_marker.position_seconds() - 4.0, // Assume 4 seconds before SONGSTART for song start
                        end_pos,
                        format!("{} (Song)", project_name),
                    ).unwrap();
                    
                    match build_song_from_region(
                        &synthetic_region.name,
                        start_marker,
                        &synthetic_region,
                        markers,
                        regions,
                        project,
                        project_path,
                        Some(project_name),
                    ) {
                        Ok(song) => songs.push(Ok(song)),
                        Err(e) => {
                            warn!(error = %e, "Failed to build song from synthetic region");
                            songs.push(Err(e));
                        }
                    }
                }
            }
        }
    } else {
        // No SONGSTART or =START markers found, check for SONGEND markers
        // A song might have SONGEND but no SONGSTART (use =START or region start)
        let songend_markers: Vec<&Marker> = markers
            .iter()
            .filter(|m| m.name.trim().eq_ignore_ascii_case("SONGEND"))
            .collect();
        
        if !songend_markers.is_empty() {
            // Found SONGEND markers but no SONGSTART - try to find =START or use region start
            for songend_marker in songend_markers {
                // Look for =START marker before this SONGEND
                let renderstart = markers.iter()
                    .find(|m| {
                        m.name.trim() == "=START" &&
                        m.position_seconds() < songend_marker.position_seconds()
                    });
                
                // Find song region that contains this SONGEND
                let song_region = find_song_region_for_marker(songend_marker, markers, regions);
                
                if let Some(region) = song_region {
                    // Use =START if found, otherwise use region start as synthetic SONGSTART
                    let synthetic_marker = if let Some(rstart) = renderstart {
                        // Clone the =START marker to use as SONGSTART
                        Marker::from_seconds(rstart.position_seconds(), "SONGSTART".to_string())
                    } else {
                        // Create synthetic SONGSTART at region start
                        Marker::from_seconds(region.start_seconds(), "SONGSTART".to_string())
                    };
                    
                    match build_song_from_region(
                        &region.name,
                        &synthetic_marker,
                        &region,
                        markers,
                        regions,
                        project,
                        project_path,
                        Some(project_name),
                    ) {
                        Ok(song) => songs.push(Ok(song)),
                        Err(e) => {
                            warn!(error = %e, region_name = %region.name, "Failed to build song from region with SONGEND");
                            songs.push(Err(e));
                        }
                    }
                } else if let Some(rstart) = renderstart {
                    // No region found, but we have =START and SONGEND - create synthetic region
                    let synthetic_region = Region::from_seconds(
                        rstart.position_seconds() - 4.0,
                        songend_marker.position_seconds(),
                        format!("{} (Song)", project_name),
                    ).unwrap();
                    
                    let synthetic_marker = Marker::from_seconds(rstart.position_seconds(), "SONGSTART".to_string());
                    
                    match build_song_from_region(
                        &synthetic_region.name,
                        &synthetic_marker,
                        &synthetic_region,
                        markers,
                        regions,
                        project,
                        project_path,
                        Some(project_name),
                    ) {
                        Ok(song) => songs.push(Ok(song)),
                        Err(e) => {
                            warn!(error = %e, "Failed to build song from synthetic region with =START/SONGEND");
                            songs.push(Err(e));
                        }
                    }
                }
            }
        } else {
            // No SONGSTART, =START, or SONGEND markers found, try to infer songs from regions
            // Use all regions as potential song regions and let build_song figure it out
            let song_regions = find_song_regions(regions);
            
            if song_regions.is_empty() {
                // No song regions found by strict criteria, but if we have regions, 
                // try to create songs from them anyway (be more lenient)
                if !regions.is_empty() {
                    debug!(project_name = %project_name, region_count = regions.len(), "No song markers found, trying to create songs from all regions");
                    
                    // If there's only one region, treat it as a song
                    // If there are multiple regions, try to find the largest one (likely the song container)
                    if regions.len() == 1 {
                        // Single region - treat as song
                        let region = &regions[0];
                        let synthetic_marker = Marker::from_seconds(region.start_seconds(), "SONGSTART".to_string());
                        
                        match build_song_from_region(
                            &region.name,
                            &synthetic_marker,
                            region,
                            markers,
                            regions,
                            project,
                            project_path,
                            Some(project_name),
                        ) {
                            Ok(song) => songs.push(Ok(song)),
                            Err(e) => {
                                warn!(error = %e, region_name = %region.name, "Failed to build song from single region");
                                songs.push(Err(e));
                            }
                        }
                    } else {
                        // Multiple regions - find the largest one that contains others (song container)
                        let largest_region = regions.iter()
                            .max_by(|a, b| {
                                let a_duration = a.end_seconds() - a.start_seconds();
                                let b_duration = b.end_seconds() - b.start_seconds();
                                a_duration.partial_cmp(&b_duration).unwrap_or(std::cmp::Ordering::Equal)
                            });
                        
                        if let Some(region) = largest_region {
                            // Check if this region contains other regions (sections)
                            let contains_others = regions.iter().any(|r| {
                                r.start_seconds() != region.start_seconds() &&
                                r.end_seconds() != region.end_seconds() &&
                                r.start_seconds() >= region.start_seconds() &&
                                r.end_seconds() <= region.end_seconds()
                            });
                            
                            if contains_others {
                                // This is likely a song container
                                let synthetic_marker = Marker::from_seconds(region.start_seconds(), "SONGSTART".to_string());
                                
                                match build_song_from_region(
                                    &region.name,
                                    &synthetic_marker,
                                    region,
                                    markers,
                                    regions,
                                    project,
                                    project_path,
                                    Some(project_name),
                                ) {
                                    Ok(song) => songs.push(Ok(song)),
                                    Err(e) => {
                                        warn!(error = %e, region_name = %region.name, "Failed to build song from largest region");
                                        songs.push(Err(e));
                                    }
                                }
                            } else {
                                // No clear song container, try build_song_from_project_simple as fallback
                                match build_song_from_project_simple(
                                    project_name,
                                    markers,
                                    regions,
                                    project,
                                    project_path,
                                ) {
                                    Ok(song) => songs.push(Ok(song)),
                                    Err(e) => {
                                        debug!(error = %e, project_name = %project_name, "Failed to build song from project (no clear song structure)");
                                        songs.push(Err(e));
                                    }
                                }
                            }
                        } else {
                            // No regions found at all - skip
                            debug!(project_name = %project_name, "No regions found, skipping project");
                        }
                    }
                } else {
                    // No regions found - skip this project (it's not a song)
                    debug!(project_name = %project_name, "No song markers or regions found, skipping project");
                }
            } else {
                // Build a song for each song region
                for region in song_regions {
                    // Find the closest SONGSTART or =START marker (if any) for this region
                    let closest_start = markers.iter()
                        .find(|m| {
                            (m.name.trim().eq_ignore_ascii_case("SONGSTART") || m.name.trim() == "=START") &&
                            m.position_seconds() >= region.start_seconds() &&
                            m.position_seconds() <= region.end_seconds()
                        });
                    
                    // Create a synthetic marker if needed (owned, not a reference)
                    let synthetic_marker = Marker::from_seconds(region.start_seconds(), "SONGSTART".to_string());
                    let start_marker = closest_start.unwrap_or(&synthetic_marker);
                    
                    match build_song_from_region(
                        &region.name,
                        start_marker,
                        &region,
                        markers,
                        regions,
                        project,
                        project_path,
                        Some(project_name),
                    ) {
                        Ok(song) => songs.push(Ok(song)),
                        Err(e) => {
                            warn!(error = %e, region_name = %region.name, "Failed to build song from region");
                            songs.push(Err(e));
                        }
                    }
                }
            }
        }
    }
    
    songs
}

/// Build a song from a specific song region
fn build_song_from_region(
    song_name: &str,
    start_marker: &Marker,
    song_region: &Region,
    all_markers: &[Marker],
    all_regions: &[Region],
    project: &Project,
    project_path: Option<&std::path::Path>,
    project_name: Option<&str>,
) -> Result<Song, SetlistError> {
    // Parse song name to extract actual song name and artist
    let (parsed_song_name, artist_from_region) = parse_song_name(song_name);
    
    // If no artist found in region name, try to extract from project name
    let artist = if artist_from_region.is_some() {
        artist_from_region
    } else if let Some(proj_name) = project_name {
        // Try to parse artist from project name
        let (_, artist_from_project) = parse_song_name(proj_name);
        artist_from_project
    } else {
        None
    };
    
    let mut song = Song::new(parsed_song_name.clone())?;
    
    // Store artist in metadata if present
    if let Some(artist_name) = artist {
        song.metadata.insert("artist".to_string(), artist_name);
    }
    
    // Set SONGSTART marker
    song.set_start_marker(start_marker.clone());
    
    // Find special markers within this song region
    // Count-In marker
    if let Some(marker) = all_markers.iter().find(|m| {
        m.name.trim().eq_ignore_ascii_case("Count-In") &&
        m.position_seconds() >= song_region.start_seconds() &&
        m.position_seconds() <= song_region.end_seconds()
    }) {
        song.set_count_in_marker(marker.clone());
    }
    
    // =START marker (RENDERSTART)
    if let Some(marker) = all_markers.iter().find(|m| {
        m.name.trim() == "=START" &&
        m.position_seconds() >= song_region.start_seconds() &&
        m.position_seconds() <= song_region.end_seconds()
    }) {
        song.set_render_start_marker(marker.clone());
    }
    
    // SONGEND marker
    if let Some(marker) = all_markers.iter().find(|m| {
        m.name.trim().eq_ignore_ascii_case("SONGEND") &&
        m.position_seconds() >= song_region.start_seconds() &&
        m.position_seconds() <= song_region.end_seconds()
    }) {
        song.set_song_end_marker(marker.clone());
    }
    
    // =END marker (RENDEREND)
    if let Some(marker) = all_markers.iter().find(|m| {
        m.name.trim() == "=END" &&
        m.position_seconds() >= song_region.start_seconds() &&
        m.position_seconds() <= song_region.end_seconds()
    }) {
        song.set_render_end_marker(marker.clone());
    }
    
    // Create markers for song region boundaries
    let start_pos = song_region.time_range.start.clone();
    let end_pos = song_region.time_range.end.clone();
    
    let start_marker = Marker::new(start_pos, format!("{}_REGION_START", song_name));
    let end_marker = Marker::new(end_pos, format!("{}_REGION_END", song_name));
    
    song.set_song_region_start_marker(start_marker);
    song.set_song_region_end_marker(end_marker);
    
    // Find sections (regions within the song region)
    let song_start = song.start_position()
        .map(|p| p.time.to_seconds())
        .or_else(|| song.song_region_start().map(|p| p.time.to_seconds()))
        .unwrap_or(song_region.start_seconds());
    let song_end = song.song_end_position()
        .map(|p| p.time.to_seconds())
        .or_else(|| song.song_region_end().map(|p| p.time.to_seconds()))
        .unwrap_or(song_region.end_seconds());
    
    let mut sections: Vec<Section> = all_regions
        .iter()
        .filter_map(|region| {
            // Section must be within the song region boundaries
            if region.start_seconds() < song_region.start_seconds() || 
               region.end_seconds() > song_region.end_seconds() {
                return None;
            }
            
            // Don't include the song region itself
            if region.start_seconds() == song_region.start_seconds() &&
               region.end_seconds() == song_region.end_seconds() {
                return None;
            }
            
            // Section must be within song boundaries (SONGSTART to SONGEND)
            if region.start_seconds() < song_start || region.end_seconds() > song_end {
                return None;
            }
            
            // Try to parse section type from region name
            let section_type = parse_section_type_from_name(&region.name);
            
            // If we couldn't parse a section type, use Custom to indicate it's an unrecognized section
            let section_type = section_type.unwrap_or(SectionType::Custom);
            
            // Extract number from name if present (but not for Custom sections)
            let number = if matches!(section_type, SectionType::Custom) {
                None // Custom sections should not have numbers extracted
            } else {
                extract_number_from_name(&region.name)
            };
            
            // Create section with musical positions
            let start_pos = region.time_range.start.clone();
            let end_pos = region.time_range.end.clone();
            
            // Clone section_type for debug logging (it will be moved into Section::new)
            let section_type_debug = section_type.clone();
            match Section::new(
                section_type,
                start_pos,
                end_pos,
                region.name.clone(),
                number,
            ) {
                Ok(section) => {
                    debug!(
                        name = %region.name,
                        section_type = ?section_type_debug,
                        start = region.start_seconds(),
                        end = region.end_seconds(),
                        "Created section"
                    );
                    Some(section)
                }
                Err(e) => {
                    warn!(error = %e, name = %region.name, "Failed to create section");
                    None
                }
            }
        })
        .collect();
    
    // Sort sections by start position
    sections.sort_by(|a, b| {
        a.start_seconds()
            .partial_cmp(&b.start_seconds())
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    
    // Add sections to song
    for section in sections {
        if let Err(e) = song.add_section(section) {
            warn!(error = %e, "Failed to add section to song");
        }
    }
    
    // Auto-number sections
    song.auto_number_sections();
    
    // Get song start position for calculating song-relative tempo change positions
    let song_start_seconds = song.start_position()
        .map(|p| p.time.to_seconds())
        .or_else(|| song.song_region_start().map(|p| p.time.to_seconds()))
        .unwrap_or(song_region.start_seconds());
    
    // Read tempo/time signature changes from the project and filter to song range
    let all_tempo_changes = read_tempo_time_sig_markers_from_project(project);
    let song_tempo_changes: Vec<TempoTimePoint> = all_tempo_changes
        .into_iter()
        .filter_map(|(time_pos, tempo, time_sig)| {
            // Only include changes within the song region
            if time_pos >= song_region.start_seconds() && time_pos <= song_region.end_seconds() {
                // Convert to song-relative position (0.0 = start of song)
                let song_relative_pos = time_pos - song_start_seconds;
                Some(TempoTimePoint::new_full(
                    song_relative_pos,
                    tempo,
                    None, // shape
                    time_sig,
                    None, // selected
                    None, // bezier_tension
                    None, // metronome_pattern
                ))
            } else {
                None
            }
        })
        .collect();
    
    song.set_tempo_time_sig_changes(song_tempo_changes);
    
    // Attach project to song (Project<ReaperTransport> implements TransportActions)
    let project_wrapper = create_reaper_project_wrapper(project.clone(), project_path);
    song.set_project_wrapper(project_wrapper);
    
    Ok(song)
}

/// Build a song from markers and regions in a project (simple version when no song regions found)
fn build_song_from_project_simple(
    project_name: &str,
    markers: &[Marker],
    regions: &[Region],
    project: &Project,
    project_path: Option<&std::path::Path>,
) -> Result<Song, SetlistError> {
    // Parse song name to extract actual song name and artist
    let (parsed_song_name, artist) = parse_song_name(project_name);
    
    let mut song = Song::new(parsed_song_name.clone())?;
    
    // Store artist in metadata if present
    if let Some(artist_name) = artist {
        song.metadata.insert("artist".to_string(), artist_name);
    }

    // Find special markers
    // Count-In marker
    if let Some(marker) = find_marker_by_name(markers, "Count-In") {
        song.set_count_in_marker(marker.clone());
        debug!(position = marker.position_seconds(), "Found Count-In marker");
    }

    // =START marker (RENDERSTART)
    if let Some(marker) = find_marker_by_name(markers, "=START") {
        song.set_render_start_marker(marker.clone());
        debug!(position = marker.position_seconds(), "Found =START (RENDERSTART) marker");
    }

    // SONGSTART marker
    if let Some(marker) = find_marker_by_name(markers, "SONGSTART") {
        song.set_start_marker(marker.clone());
        debug!(position = marker.position_seconds(), "Found SONGSTART marker");
    }

    // SONGEND marker
    if let Some(marker) = find_marker_by_name(markers, "SONGEND") {
        song.set_song_end_marker(marker.clone());
        debug!(position = marker.position_seconds(), "Found SONGEND marker");
    }

    // =END marker (RENDEREND)
    if let Some(marker) = find_marker_by_name(markers, "=END") {
        song.set_render_end_marker(marker.clone());
        debug!(position = marker.position_seconds(), "Found =END (RENDEREND) marker");
    }

    // Find song region (largest region that contains SONGSTART or contains multiple sections)
    let song_region = find_song_region(markers, regions);
    if let Some(region) = song_region {
        // Create markers for song region boundaries
        let start_pos = region.time_range.start.clone();
        let end_pos = region.time_range.end.clone();
        
        let start_marker = Marker::new(start_pos, format!("{}_REGION_START", project_name));
        let end_marker = Marker::new(end_pos, format!("{}_REGION_END", project_name));
        
        song.set_song_region_start_marker(start_marker);
        song.set_song_region_end_marker(end_marker);
        
        debug!(
            start = region.start_seconds(),
            end = region.end_seconds(),
            "Found song region"
        );
    }

    // Find sections (regions within the song)
    let song_start = song.start_position()
        .map(|p| p.time.to_seconds())
        .or_else(|| song.song_region_start().map(|p| p.time.to_seconds()))
        .unwrap_or(0.0);
    let song_end = song.song_end_position()
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
                    if region.start_seconds() == song_region_start.time.to_seconds() &&
                       region.end_seconds() == song_region_end.time.to_seconds() {
                        return None;
                    }
                }
            }

            // Try to parse section type from region name
            let section_type = parse_section_type_from_name(&region.name);

            // Extract number from name if present
            let number = extract_number_from_name(&region.name);

            // Create section with musical positions
            let start_pos = region.time_range.start.clone();
            let end_pos = region.time_range.end.clone();

            // If we couldn't parse a section type, use Custom to indicate it's an unrecognized section
            let section_type = section_type.unwrap_or(SectionType::Custom);

            // Clone section_type for debug logging (it will be moved into Section::new)
            let section_type_debug = section_type.clone();
            match Section::new(
                section_type,
                start_pos,
                end_pos,
                region.name.clone(),
                number,
            ) {
                Ok(section) => {
                    debug!(
                        name = %region.name,
                        section_type = ?section_type_debug,
                        start = region.start_seconds(),
                        end = region.end_seconds(),
                        "Created section"
                    );
                    Some(section)
                }
                Err(e) => {
                    warn!(error = %e, name = %region.name, "Failed to create section");
                    None
                }
            }
        })
        .collect();

    // Sort sections by start position
    sections.sort_by(|a, b| {
        a.start_seconds()
            .partial_cmp(&b.start_seconds())
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    // Add sections to song
    for section in sections {
        if let Err(e) = song.add_section(section) {
            warn!(error = %e, "Failed to add section to song");
        }
    }

    // Auto-number sections
    song.auto_number_sections();

    // Attach project to song (Project<ReaperTransport> implements TransportActions)
    let project_wrapper = create_reaper_project_wrapper(project.clone(), project_path);
    song.set_project_wrapper(project_wrapper);

    Ok(song)
}

/// Find a marker by name (case-insensitive)
fn find_marker_by_name<'a>(markers: &'a [Marker], name: &str) -> Option<&'a Marker> {
    markers.iter().find(|m| {
        m.name.trim().eq_ignore_ascii_case(name)
    })
}

/// Find song regions by identifying regions that contain other regions (sections)
/// A song region is the largest region that contains multiple other regions within it.
fn find_song_regions(regions: &[Region]) -> Vec<Region> {
    let mut song_regions = Vec::new();
    
    for potential_song_region in regions {
        // Count how many other regions are contained within this region
        let contained_regions: Vec<&Region> = regions.iter()
            .filter(|other_region| {
                // Don't count self
                if other_region.start_seconds() == potential_song_region.start_seconds() &&
                   other_region.end_seconds() == potential_song_region.end_seconds() {
                    return false;
                }
                
                // Check if other_region is contained within potential_song_region
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
    song_regions.sort_by(|a, b| {
        a.start_seconds().partial_cmp(&b.start_seconds())
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    
    song_regions
}

/// Find the song region for a specific SONGSTART marker
/// Returns the largest region that contains the marker and the most sections
fn find_song_region_for_marker<'a>(
    start_marker: &Marker,
    _markers: &'a [Marker],
    regions: &'a [Region],
) -> Option<Region> {
    // Find all regions that contain this marker
    let containing_regions: Vec<&Region> = regions.iter()
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
        let contained_count = regions.iter()
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

/// Find the song region (largest region that contains SONGSTART or contains multiple sections)
/// This is the old single-song-per-project version - kept for backward compatibility
fn find_song_region<'a>(markers: &'a [Marker], regions: &'a [Region]) -> Option<&'a Region> {
    // First, try to find a region that contains SONGSTART
    if let Some(start_marker) = find_marker_by_name(markers, "SONGSTART") {
        let containing_regions: Vec<&Region> = regions
            .iter()
            .filter(|r| {
                r.start_seconds() <= start_marker.position_seconds() &&
                r.end_seconds() >= start_marker.position_seconds()
            })
            .collect();

        if !containing_regions.is_empty() {
            // Return the largest containing region
            return containing_regions
                .iter()
                .max_by(|a, b| {
                    let a_duration = a.end_seconds() - a.start_seconds();
                    let b_duration = b.end_seconds() - b.start_seconds();
                    a_duration.partial_cmp(&b_duration).unwrap_or(std::cmp::Ordering::Equal)
                })
                .copied();
        }
    }

    // If no SONGSTART, find regions that contain multiple other regions (sections)
    for potential_song_region in regions {
        let contained_count = regions
            .iter()
            .filter(|other| {
                // Don't count self
                if other.start_seconds() == potential_song_region.start_seconds() &&
                   other.end_seconds() == potential_song_region.end_seconds() {
                    return false;
                }
                // Check if contained
                other.start_seconds() >= potential_song_region.start_seconds() &&
                other.end_seconds() <= potential_song_region.end_seconds()
            })
            .count();

        // If this region contains at least 2 other regions, it's likely a song
        if contained_count >= 2 {
            return Some(potential_song_region);
        }
    }

    None
}

/// Parse section type from region name
/// Handles names like "INST 1a", "VS 1", "CH 2", "BR 1", etc.
/// Extracts the section type prefix before any numbers or letters
fn parse_section_type_from_name(name: &str) -> Option<SectionType> {
    let name = name.trim();
    
    // Try exact match first
    if let Ok(section_type) = SectionType::from_str(name) {
        return Some(section_type);
    }
    
    // Extract the prefix before any number or letter suffix
    // Examples: "INST 1a" -> "INST", "VS 1" -> "VS", "CH 2" -> "CH"
    // Split by whitespace and take the first part
    let parts: Vec<&str> = name.split_whitespace().collect();
    if let Some(prefix) = parts.first() {
        // Try matching the prefix
        if let Ok(section_type) = SectionType::from_str(prefix) {
            return Some(section_type);
        }
    }
    
    // Try matching common patterns like "INST1a", "VS1", "CH2" (no space)
    // Check if it starts with a known abbreviation followed by a digit or letter
    let name_lower = name.to_lowercase();
    
    // Check for full words first (longer matches first to avoid false positives)
    if name_lower.starts_with("instrumental") {
        return Some(SectionType::Instrumental);
    }
    if name_lower.starts_with("intro") {
        return Some(SectionType::Intro);
    }
    if name_lower.starts_with("outro") {
        return Some(SectionType::Outro);
    }
    if name_lower.starts_with("verse") {
        return Some(SectionType::Verse);
    }
    if name_lower.starts_with("chorus") {
        return Some(SectionType::Chorus);
    }
    if name_lower.starts_with("bridge") {
        return Some(SectionType::Bridge);
    }
    
    // Check for abbreviations (check longer ones first)
    // "inst" must be checked before "in" to avoid false matches
    if name_lower.starts_with("inst") {
        // Check if it's exactly "inst" or followed by a digit/letter (like "inst1a")
        if name_lower.len() == 4 || (name_lower.len() > 4 && name_lower.chars().nth(4).map(|c| c.is_alphanumeric()).unwrap_or(false)) {
            return Some(SectionType::Instrumental);
        }
    }
    if name_lower.starts_with("vs") {
        // Check if it's exactly "vs" or followed by a digit/letter (like "vs1")
        if name_lower.len() == 2 || (name_lower.len() > 2 && name_lower.chars().nth(2).map(|c| c.is_alphanumeric()).unwrap_or(false)) {
            return Some(SectionType::Verse);
        }
    }
    if name_lower.starts_with("ch") {
        // Check if it's exactly "ch" or followed by a digit/letter (like "ch2")
        if name_lower.len() == 2 || (name_lower.len() > 2 && name_lower.chars().nth(2).map(|c| c.is_alphanumeric()).unwrap_or(false)) {
            return Some(SectionType::Chorus);
        }
    }
    if name_lower.starts_with("br") {
        // Check if it's exactly "br" or followed by a digit/letter (like "br1")
        if name_lower.len() == 2 || (name_lower.len() > 2 && name_lower.chars().nth(2).map(|c| c.is_alphanumeric()).unwrap_or(false)) {
            return Some(SectionType::Bridge);
        }
    }
    if name_lower.starts_with("in") && !name_lower.starts_with("inst") {
        // Only match "in" if it's not "inst" (already checked above)
        if name_lower.len() == 2 || (name_lower.len() > 2 && name_lower.chars().nth(2).map(|c| c.is_alphanumeric()).unwrap_or(false)) {
            return Some(SectionType::Intro);
        }
    }
    if name_lower.starts_with("out") {
        // Check if it's exactly "out" or followed by a digit/letter (like "out1")
        if name_lower.len() == 3 || (name_lower.len() > 3 && name_lower.chars().nth(3).map(|c| c.is_alphanumeric()).unwrap_or(false)) {
            return Some(SectionType::Outro);
        }
    }
    
    // Try Pre/Post prefixes
    if name_lower.starts_with("pre-") {
        if let Some(rest) = name_lower.strip_prefix("pre-") {
            if let Some(inner) = parse_section_type_from_name(rest) {
                return Some(SectionType::Pre(Box::new(inner)));
            }
        }
    }
    if name_lower.starts_with("post-") {
        if let Some(rest) = name_lower.strip_prefix("post-") {
            if let Some(inner) = parse_section_type_from_name(rest) {
                return Some(SectionType::Post(Box::new(inner)));
            }
        }
    }
    
    None
}

/// Extract number from name (e.g., "Verse 1", "CH 2")
fn extract_number_from_name(name: &str) -> Option<u32> {
    let name_lower = name.to_lowercase();
    let parts: Vec<&str> = name_lower.split_whitespace().collect();

    for part in &parts {
        if let Ok(num) = part.parse::<u32>() {
            return Some(num);
        }
        // Also check if number follows section type abbreviation
        if part.len() > 2 {
            if let Ok(num) = part[2..].parse::<u32>() {
                return Some(num);
            }
        }
    }

    None
}

