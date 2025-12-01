//! Song Building Logic
//!
//! Handles building songs and setlists from REAPER projects.

use daw::marker_region::core::{Marker, Region};
use reaper_high::{Project, Reaper};
use reaper_medium::ProjectRef;
use fts::setlist::core::{Setlist, SetlistError, Song};
use daw::primitives::TimeSignature;
use tracing::{debug, warn};
use daw::marker_region::application::TempoTimePoint;

use crate::implementation::markers::{read_markers_from_project, read_regions_from_project};
use crate::implementation::setlist::stats::{record_song_built, flush_build_stats_if_needed};
use crate::implementation::setlist::tempo::read_tempo_time_sig_markers_from_project;
use crate::implementation::setlist::parser::parse_song_name;
use crate::implementation::setlist::region_finder::{find_marker_by_name, find_song_regions, find_song_region_for_marker, find_song_region};
use crate::implementation::setlist::section_builder::{build_sections_from_regions, build_sections_from_regions_simple};
use crate::lyrics::read::{read_lyrics_from_project, convert_lyrics_data_to_lyrics};

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
/// 
/// If `existing_setlist` is provided, checks for existing songs before building new ones
/// to avoid redundant work when rebuilding unchanged projects.
pub fn build_setlist_from_open_projects(existing_setlist: Option<&Setlist>) -> Result<Setlist, SetlistError> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let mut setlist = Setlist::new("REAPER Setlist".to_string())?;

    // Track projects processed for batched logging
    let mut projects_processed = 0;
    let mut projects_skipped = 0;

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
                projects_skipped += 1;
                continue;
            }

            let project = Project::new(result.project);
            projects_processed += 1;

            // Read markers and regions from this project
            let markers = read_markers_from_project(&project)
                .map_err(|e| SetlistError::validation_error(format!("Failed to read markers: {}", e)))?;
            let regions = read_regions_from_project(&project)
                .map_err(|e| SetlistError::validation_error(format!("Failed to read regions: {}", e)))?;

            // Build songs from this project (can have multiple songs if multiple song regions)
            // Pass the existing setlist to avoid rebuilding unchanged songs
            let songs = build_songs_from_project_with_cache(
                &project_name,
                &markers,
                &regions,
                &project,
                result.file_path.as_ref().map(|p| p.as_std_path()),
                existing_setlist,
            );
            
            for song_result in songs {
                match song_result {
                    Ok(song) => {
                        record_song_built();
                        if let Err(e) = setlist.add_song(song) {
                            // Duplicate songs can happen if a project has multiple songs with the same name
                            // This is expected during rebuilds, so log at debug level
                            debug!(error = %e, project_name = %project_name, "Song already exists in setlist (skipping duplicate)");
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

    // Setlist built silently - no logging
    
    // Flush batched stats
    flush_build_stats_if_needed();
    
    Ok(setlist)
}

/// Build multiple songs from markers and regions in a project
/// A project can have multiple songs if it has multiple song regions (each with a SONGSTART or =START marker)
pub fn build_songs_from_project(
    project_name: &str,
    markers: &[Marker],
    regions: &[Region],
    project: &Project,
    project_path: Option<&std::path::Path>,
) -> Vec<Result<Song, SetlistError>> {
    build_songs_from_project_with_cache(project_name, markers, regions, project, project_path, None)
}

/// Build multiple songs from markers and regions in a project, with caching support
/// If `existing_setlist` is provided, reuses existing songs instead of rebuilding them
pub fn build_songs_from_project_with_cache(
    project_name: &str,
    markers: &[Marker],
    regions: &[Region],
    project: &Project,
    project_path: Option<&std::path::Path>,
    existing_setlist: Option<&Setlist>,
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
                    // Debug: Log the found song region and its color
                    debug!(
                        region_name = %region.name,
                        region_color = ?region.color,
                        start_marker_pos = start_marker.position_seconds(),
                        "Found song region for SONGSTART marker"
                    );
                    
                    // Extract song name from region name before building
                    let (song_name, _) = parse_song_name(&region.name);
                    
                    // Check if song already exists in cached setlist
                    if let Some(existing) = existing_setlist {
                        if let Some(existing_song) = existing.songs.iter().find(|s| {
                            s.name == song_name && 
                            s.metadata.get("project_name").map(|s| s.as_str()) == Some(project_name)
                        }) {
                            debug!(
                                song_name = %song_name,
                                project_name = %project_name,
                                "Song already exists in cached setlist, reusing without rebuild"
                            );
                            songs.push(Ok(existing_song.clone()));
                            continue;
                        }
                    }
                    
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
                    let synthetic_region_name = format!("{} (Song)", project_name);
                    let (song_name, _) = parse_song_name(&synthetic_region_name);
                    
                    // Check if song already exists in cached setlist
                    if let Some(existing) = existing_setlist {
                        if let Some(existing_song) = existing.songs.iter().find(|s| {
                            s.name == song_name && 
                            s.metadata.get("project_name").map(|s| s.as_str()) == Some(project_name)
                        }) {
                            debug!(
                                song_name = %song_name,
                                project_name = %project_name,
                                "Song already exists in cached setlist, reusing without rebuild"
                            );
                            songs.push(Ok(existing_song.clone()));
                            continue;
                        }
                    }
                    
                    let synthetic_region = Region::from_seconds(
                        start_marker.position_seconds() - 4.0, // Assume 4 seconds before SONGSTART for song start
                        end_pos,
                        synthetic_region_name,
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
pub fn build_song_from_region(
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
    
    // Store project name in metadata
    if let Some(proj_name) = project_name {
        song.metadata.insert("project_name".to_string(), proj_name.to_string());
    }
    
    // Set SONGSTART marker (color is already stored in the marker struct)
    song.set_start_marker(start_marker.clone());
    
    // Store start marker position for potential count-in section creation
    let start_marker_position = start_marker.position.clone();
    
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
    // Preserve the color from the song region, or fall back to start_marker color
    let start_pos = song_region.time_range.start.clone();
    let end_pos = song_region.time_range.end.clone();
    
    // Determine color: prefer song region color, fall back to start_marker color
    let region_color = song_region.color
        .or_else(|| start_marker.color);
    
    // Debug: Log the color determination
    debug!(
        song_name = %parsed_song_name,
        region_color = ?song_region.color,
        start_marker_color = ?start_marker.color,
        final_color = ?region_color,
        region_name = %song_region.name,
        "Song region color determination"
    );
    
    // Create markers with the determined color
    let start_marker = Marker::new_full(
        None, // id
        start_pos,
        format!("{}_REGION_START", song_name),
        region_color, // Use region color or fall back to start_marker color
        None, // flags
        None, // locked
        None, // guid
    );
    let end_marker = Marker::new_full(
        None, // id
        end_pos,
        format!("{}_REGION_END", song_name),
        region_color, // Use region color or fall back to start_marker color
        None, // flags
        None, // locked
        None, // guid
    );
    
    song.set_song_region_start_marker(start_marker);
    song.set_song_region_end_marker(end_marker);
    
    // Build sections from regions
    let sections = build_sections_from_regions(all_regions, &song, song_region);
    
    // Add sections to song
    for section in sections {
        let section_name = section.name.clone();
        if let Err(e) = song.add_section(section) {
            // Section overlaps can happen if regions overlap in REAPER
            // This is expected during rebuilds, so log at debug level
            debug!(error = %e, section_name = %section_name, "Section overlaps with existing sections (skipping)");
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
    
    // Calculate starting tempo and time signature at count-in position (or song start if no count-in)
    let starting_position = song.count_in_marker.as_ref()
        .map(|m| m.position.time.to_seconds())
        .unwrap_or(song_start_seconds);
    
    // Convert to song-relative position (0.0 = song start)
    let starting_position_song_relative = starting_position - song_start_seconds;
    
    // Find the last tempo/time sig change at or before the starting position
    let mut starting_tempo: Option<f64> = None;
    let mut starting_time_sig: Option<TimeSignature> = None;
    
    for point in song.tempo_time_sig_changes.iter() {
        if point.position <= starting_position_song_relative {
            if point.tempo > 0.0 {
                starting_tempo = Some(point.tempo);
            }
            if let Some((n, d)) = point.time_signature {
                starting_time_sig = Some(TimeSignature::new(n, d));
            }
        } else {
            break;
        }
    }
    
    // If no tempo/time sig found in changes, we could fallback to project defaults,
    // but for now we'll leave it as None and let the desktop app handle the fallback
    song.starting_tempo = starting_tempo;
    song.starting_time_signature = starting_time_sig;
    
    // Read lyrics from the project
    match read_lyrics_from_project(project.clone()) {
        Ok(lyrics_data) => {
            match convert_lyrics_data_to_lyrics(lyrics_data, parsed_song_name.clone(), &song) {
                Ok(lyrics) => {
                    song.lyrics = Some(lyrics);
                    debug!(song_name = %parsed_song_name, "Successfully read lyrics from project");
                }
                Err(e) => {
                    warn!(song_name = %parsed_song_name, error = %e, "Failed to convert lyrics from slides");
                }
            }
        }
        Err(e) => {
            // Lyrics folder not found is expected for songs without lyrics, so log at debug level
            debug!(song_name = %parsed_song_name, error = %e, "No lyrics found in project (this is OK if song has no lyrics)");
        }
    }
    
    // Read current transport info and tracks, then store them in the song's project field
    // This ensures transport state (playhead position, play state, tempo, etc.) and all track information is included in the setlist
    use crate::implementation::transport::ReaperTransport;
    
    
    let reaper_project = project.clone();
    let transport_adapter = ReaperTransport::new(reaper_project.clone());
    
    // Read current transport state and tracks
    match transport_adapter.read_transport() {
        Ok(transport) => {
            // Create a Project<Transport> with the current transport state
            let project_name_str = project_name.unwrap_or(parsed_song_name.as_str());
            let mut project_with_transport = daw::project::Project::new(project_name_str, transport);
            if let Some(path) = project_path {
                project_with_transport.set_path(path.to_string_lossy().to_string());
            }
            
            // Read lightweight track summaries (performant approach, similar to CSI)
            // Only essential track info is included - no items, envelopes, FX chains, etc.
            use crate::implementation::tracks::get_track_summaries;
            let track_summaries = get_track_summaries(project);
            // Convert summaries to minimal tracks for Project compatibility
            let minimal_tracks: Vec<daw::tracks::Track> = track_summaries
                .iter()
                .map(|summary| summary.to_minimal_track())
                .collect();
            project_with_transport.set_tracks(minimal_tracks);
            debug!(song_name = %parsed_song_name, track_count = project_with_transport.tracks().len(), "Stored transport info and lightweight track summaries in song");
            
            song.set_project(project_with_transport);
        }
        Err(e) => {
            warn!(song_name = %parsed_song_name, error = %e, "Failed to read transport info for song");
        }
    }
    
    Ok(song)
}

/// Build a song from markers and regions in a project (simple version when no song regions found)
pub fn build_song_from_project_simple(
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
    
    // Store project name in metadata
    song.metadata.insert("project_name".to_string(), project_name.to_string());

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
        // Preserve the color from the song region, or fall back to start_marker color
        let start_pos = region.time_range.start.clone();
        let end_pos = region.time_range.end.clone();
        
        // Determine color: prefer song region color, fall back to start_marker color
        let start_marker_color = song.start_marker.as_ref().and_then(|m| m.color);
        let region_color = region.color
            .or(start_marker_color);
        
        // Create markers with the determined color
        let start_marker = Marker::new_full(
            None, // id
            start_pos,
            format!("{}_REGION_START", project_name),
            region_color, // Use region color or fall back to start_marker color
            None, // flags
            None, // locked
            None, // guid
        );
        let end_marker = Marker::new_full(
            None, // id
            end_pos,
            format!("{}_REGION_END", project_name),
            region_color, // Use region color or fall back to start_marker color
            None, // flags
            None, // locked
            None, // guid
        );
        
        song.set_song_region_start_marker(start_marker);
        song.set_song_region_end_marker(end_marker);
        
        debug!(
            start = region.start_seconds(),
            end = region.end_seconds(),
            region_color = ?region.color,
            start_marker_color = ?start_marker_color,
            final_color = ?region_color,
            "Found song region"
        );
    }

    // Build sections from regions
    let sections = build_sections_from_regions_simple(regions, &song);

    // Add sections to song
    for section in sections {
        let section_name = section.name.clone();
        if let Err(e) = song.add_section(section) {
            // Section overlaps can happen if regions overlap in REAPER
            // This is expected during rebuilds, so log at debug level
            debug!(error = %e, section_name = %section_name, "Section overlaps with existing sections (skipping)");
        }
    }

    // Auto-number sections
    song.auto_number_sections();

    // Read lyrics from the project
    match read_lyrics_from_project(project.clone()) {
        Ok(lyrics_data) => {
            match convert_lyrics_data_to_lyrics(lyrics_data, parsed_song_name.clone(), &song) {
                Ok(lyrics) => {
                    song.lyrics = Some(lyrics);
                    debug!(song_name = %parsed_song_name, "Successfully read lyrics from project");
                }
                Err(e) => {
                    warn!(song_name = %parsed_song_name, error = %e, "Failed to convert lyrics from slides");
                }
            }
        }
        Err(e) => {
            // Lyrics folder not found is expected for songs without lyrics, so log at debug level
            debug!(song_name = %parsed_song_name, error = %e, "No lyrics found in project (this is OK if song has no lyrics)");
        }
    }

    // Read current transport info and tracks, then store them in the song's project field
    // This ensures transport state (playhead position, play state, tempo, etc.) and all track information is included in the setlist
    use crate::implementation::transport::ReaperTransport;
    
    
    let reaper_project = project.clone();
    let transport_adapter = ReaperTransport::new(reaper_project.clone());
    
    // Read current transport state and tracks
    match transport_adapter.read_transport() {
        Ok(transport) => {
            // Create a Project<Transport> with the current transport state
            let mut project_with_transport = daw::project::Project::new(project_name, transport);
            if let Some(path) = project_path {
                project_with_transport.set_path(path.to_string_lossy().to_string());
            }
            
            // Read lightweight track summaries (performant approach, similar to CSI)
            // Only essential track info is included - no items, envelopes, FX chains, etc.
            use crate::implementation::tracks::get_track_summaries;
            let track_summaries = get_track_summaries(project);
            // Convert summaries to minimal tracks for Project compatibility
            let minimal_tracks: Vec<daw::tracks::Track> = track_summaries
                .iter()
                .map(|summary| summary.to_minimal_track())
                .collect();
            project_with_transport.set_tracks(minimal_tracks);
            debug!(song_name = %parsed_song_name, track_count = project_with_transport.tracks().len(), "Stored transport info and lightweight track summaries in song");
            
            song.set_project(project_with_transport);
        }
        Err(e) => {
            warn!(song_name = %parsed_song_name, error = %e, "Failed to read transport info for song");
        }
    }

    Ok(song)
}

