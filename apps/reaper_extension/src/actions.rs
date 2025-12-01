//! Actions for FastTrackStudio REAPER Extension

use crate::infrastructure::action_registry::{ActionDef, register_actions};
use crate::implementation::project::create_reaper_project_wrapper;
use crate::implementation::markers::{read_markers_from_project, read_regions_from_project};
use crate::implementation::setlist::{build_setlist_from_open_projects, build_song_from_current_project};
use reaper_high::{Project, Reaper, BookmarkType};
use reaper_medium::ProjectRef;
use tracing::{info, warn};
use daw::transport::{TransportActions, PlayState};
use crate::lyrics;

/// Dummy action handler - shows a message in REAPER console
fn dummy_action_handler() {
    let reaper = Reaper::get();
    reaper.show_console_msg("FastTrackStudio: Dummy action executed!\n");
    info!("Dummy action executed");
}

/// Log all open projects
fn log_open_projects_handler() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let current_project = reaper.current_project();
    let current_project_raw = current_project.raw();
    
    info!("Enumerating open REAPER projects...");
    reaper.show_console_msg("\n=== FastTrackStudio: Open Projects ===\n");
    
    let mut project_count = 0;
    let mut projects_info = Vec::new();
    
    // Enumerate all project tabs
    // Pass 512 as buffer size to retrieve file path (0 means don't retrieve it)
    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            project_count += 1;
            let is_active = result.project == current_project_raw;
            
            // Get project name from path
            let (name, path) = if let Some(file_path) = result.file_path.as_ref() {
                let name = file_path
                    .file_stem()
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| "unsaved".to_string());
                (name, Some(file_path.to_string()))
            } else {
                ("unsaved".to_string(), None)
            };
            
            // Create REAPER project wrapper
            let reaper_project = Project::new(result.project);
            let project_wrapper = create_reaper_project_wrapper(
                reaper_project,
                result.file_path.as_ref().map(|p| p.as_std_path()),
            );
            
            // Get transport state
            let transport_result = project_wrapper.get_transport();
            let play_state_str = match transport_result {
                Ok(transport) => {
                    let is_playing = matches!(
                        transport.play_state,
                        PlayState::Playing | PlayState::Recording
                    );
                    let time_pos = transport.playhead_position.time.to_seconds();
                    let musical_pos = transport.playhead_position.musical_position_string();
                    format!(
                        "Playing: {}, Tempo: {:.1} BPM, Position: {:.2}s ({})",
                        is_playing,
                        transport.tempo.bpm,
                        time_pos,
                        musical_pos
                    )
                }
                Err(e) => format!("Error reading transport: {}", e),
            };
            
            let status = if is_active { "ACTIVE" } else { "inactive" };
            
            let info_msg = format!(
                "[{}] Tab {}: {}\n  {}\n",
                status, i, name, play_state_str
            );
            
            reaper.show_console_msg(info_msg.as_str());
            info!(
                tab_index = i,
                name = %name,
                is_active,
                "Open project"
            );
            
            projects_info.push((i, name, path, is_active));
        } else {
            break;
        }
    }
    
    if project_count == 0 {
        reaper.show_console_msg("No projects are currently open.\n");
        warn!("No open projects found");
    } else {
        reaper.show_console_msg(format!("\nTotal: {} project(s) open\n", project_count).as_str());
        info!(project_count, "Found {} open projects", project_count);
    }
    
    reaper.show_console_msg("=====================================\n\n");
}

/// Log all markers in the current project
fn log_current_project_markers_handler() {
    let reaper = Reaper::get();
    let current_project = reaper.current_project();
    
    info!("Reading markers from current REAPER project...");
    reaper.show_console_msg("\n=== FastTrackStudio: Current Project Markers ===\n");
    
    match read_markers_from_project(&current_project) {
        Ok(markers) => {
            if markers.is_empty() {
                reaper.show_console_msg("No markers found in current project.\n");
                info!("No markers found");
            } else {
                reaper.show_console_msg(format!("Found {} marker(s):\n\n", markers.len()).as_str());
                info!(marker_count = markers.len(), "Found {} markers", markers.len());
                
                for (idx, marker) in markers.iter().enumerate() {
                    let time_pos = marker.position.time.to_seconds();
                    let musical_pos = marker.position.musical_position_string();
                    let id_str = marker.id.map(|id| format!("#{} ", id)).unwrap_or_default();
                    
                    let marker_msg = format!(
                        "  {}{}: {:.2}s ({})\n",
                        id_str,
                        marker.name,
                        time_pos,
                        musical_pos
                    );
                    
                    reaper.show_console_msg(marker_msg.as_str());
                    info!(
                        marker_index = idx,
                        marker_id = ?marker.id,
                        name = %marker.name,
                        position_seconds = time_pos,
                        musical_position = %musical_pos,
                        "Marker"
                    );
                }
            }
        }
        Err(e) => {
            let error_msg = format!("Error reading markers: {}\n", e);
            reaper.show_console_msg(error_msg.as_str());
            warn!(error = %e, "Failed to read markers");
        }
    }
    
    reaper.show_console_msg("=====================================\n\n");
}

/// Log all regions in the current project
fn log_current_project_regions_handler() {
    let reaper = Reaper::get();
    let current_project = reaper.current_project();
    
    info!("Reading regions from current REAPER project...");
    reaper.show_console_msg("\n=== FastTrackStudio: Current Project Regions ===\n");
    
    match read_regions_from_project(&current_project) {
        Ok(regions) => {
            if regions.is_empty() {
                reaper.show_console_msg("No regions found in current project.\n");
                info!("No regions found");
            } else {
                reaper.show_console_msg(format!("Found {} region(s):\n\n", regions.len()).as_str());
                info!(region_count = regions.len(), "Found {} regions", regions.len());
                
                for (idx, region) in regions.iter().enumerate() {
                    let start_time = region.start_seconds();
                    let end_time = region.end_seconds();
                    let duration = region.duration_seconds();
                    let start_musical = region.musical_start_position();
                    let end_musical = region.musical_end_position();
                    let id_str = region.id.map(|id| format!("#{} ", id)).unwrap_or_default();
                    
                    let region_msg = format!(
                        "  {}{}: {:.2}s - {:.2}s ({:.2}s) [{} - {}]\n",
                        id_str,
                        region.name,
                        start_time,
                        end_time,
                        duration,
                        start_musical,
                        end_musical
                    );
                    
                    reaper.show_console_msg(region_msg.as_str());
                    info!(
                        region_index = idx,
                        region_id = ?region.id,
                        name = %region.name,
                        start_seconds = start_time,
                        end_seconds = end_time,
                        duration_seconds = duration,
                        musical_start = %start_musical,
                        musical_end = %end_musical,
                        "Region"
                    );
                }
            }
        }
        Err(e) => {
            let error_msg = format!("Error reading regions: {}\n", e);
            reaper.show_console_msg(error_msg.as_str());
            warn!(error = %e, "Failed to read regions");
        }
    }
    
    reaper.show_console_msg("=====================================\n\n");
}

/// Log colors of all regions in the current project
fn log_region_colors_handler() {
    let reaper = Reaper::get();
    let current_project = reaper.current_project();
    let medium_reaper = reaper.medium_reaper();
    
    info!("Reading region colors from current REAPER project...");
    reaper.show_console_msg("\n=== FastTrackStudio: Region Colors Debug ===\n");
    
    let bookmark_count = current_project.bookmark_count();
    reaper.show_console_msg(format!("Total bookmarks: {}\n\n", bookmark_count.total_count).as_str());
    
    let mut region_count = 0;
    
    // Enumerate all bookmarks and extract regions
    for i in 0..bookmark_count.total_count {
        if let Some(bookmark) = current_project.find_bookmark_by_index(i) {
            let info = bookmark.basic_info();
            
            // Only include regions (BookmarkType::Region)
            if info.bookmark_type() == BookmarkType::Region {
                region_count += 1;
                let name = bookmark.name().to_string().trim_matches('"').trim().to_string();
                
                // Get native color from REAPER
                let native_color = info.color;
                let native_color_raw = native_color.to_raw();
                
                // Convert to RGB using standardized color conversion
                let rgb = medium_reaper.color_from_native(native_color);
                
                // Convert to packed u32 using standardized function
                let packed_color = crate::infrastructure::color_utils::pack_rgb_to_u32(rgb);
                
                // Format color info
                let color_info = format!(
                    "Region {}: \"{}\"\n",
                    region_count, name
                );
                reaper.show_console_msg(color_info.as_str());
                
                let color_details = format!(
                    "  Native Color (raw): {}\n",
                    native_color_raw
                );
                reaper.show_console_msg(color_details.as_str());
                
                let rgb_details = format!(
                    "  RGB: R={}, G={}, B={}\n",
                    rgb.r, rgb.g, rgb.b
                );
                reaper.show_console_msg(rgb_details.as_str());
                
                let packed_details = format!(
                    "  Packed u32: {} (0x{:08X})\n",
                    packed_color, packed_color
                );
                reaper.show_console_msg(packed_details.as_str());
                
                // Show hex color
                let hex_color = format!(
                    "#{:02X}{:02X}{:02X}\n",
                    rgb.r, rgb.g, rgb.b
                );
                let hex_details = format!("  Hex: {}\n", hex_color.trim());
                reaper.show_console_msg(hex_details.as_str());
                
                reaper.show_console_msg("\n");
                
                info!(
                    region_index = region_count,
                    name = %name,
                    native_color_raw = native_color_raw,
                    r = rgb.r,
                    g = rgb.g,
                    b = rgb.b,
                    packed_color = packed_color,
                    hex = %hex_color.trim(),
                    "Region color"
                );
            }
        }
    }
    
    if region_count == 0 {
        reaper.show_console_msg("No regions found in current project.\n");
        info!("No regions found");
    } else {
        reaper.show_console_msg(format!("Found {} region(s) with color information.\n", region_count).as_str());
    }
    
    reaper.show_console_msg("=====================================\n\n");
}

/// Build and log setlist from all open projects
/// This can be called from any context, but REAPER APIs will only work from main thread
pub fn build_setlist_from_projects_handler() {
    let reaper = Reaper::get();
    
    info!("Building setlist from open REAPER projects...");
    reaper.show_console_msg("\n=== FastTrackStudio: Building Setlist from Open Projects ===\n");
    
    match build_setlist_from_open_projects(None) {
        Ok(setlist) => {
            let song_count = setlist.song_count();
            reaper.show_console_msg(format!("Built setlist with {} song(s):\n\n", song_count).as_str());
            info!(song_count, "Built setlist with {} songs", song_count);
            
            
            for (idx, song) in setlist.songs.iter().enumerate() {
                // Calculate song duration in minutes:seconds format
                let duration_seconds = song.duration();
                let minutes = (duration_seconds / 60.0) as u32;
                let seconds = (duration_seconds % 60.0) as u32;
                let duration_str = format!("{}:{:02}", minutes, seconds);
                
                // Format song name with artist if available
                let song_display_name = if let Some(artist) = song.metadata.get("artist") {
                    format!("{} - {}", song.name, artist)
                } else {
                    song.name.clone()
                };
                
                let song_info = format!(
                    "  {}. {} ({} sections, {})\n",
                    idx + 1,
                    song_display_name,
                    song.sections.len(),
                    duration_str
                );
                reaper.show_console_msg(song_info.as_str());
                
                // Log markers with musical positions
                let mut marker_info = Vec::new();
                if let Some(marker) = &song.count_in_marker {
                    let musical_pos = marker.position.musical_position_string();
                    marker_info.push(format!("Count-In ({})", musical_pos));
                }
                if let Some(marker) = &song.render_start_marker {
                    let musical_pos = marker.position.musical_position_string();
                    marker_info.push(format!("=START (RENDERSTART) ({})", musical_pos));
                }
                if let Some(marker) = &song.start_marker {
                    let musical_pos = marker.position.musical_position_string();
                    marker_info.push(format!("SONGSTART ({})", musical_pos));
                }
                if let Some(marker) = &song.song_end_marker {
                    let musical_pos = marker.position.musical_position_string();
                    marker_info.push(format!("SONGEND ({})", musical_pos));
                }
                if let Some(marker) = &song.render_end_marker {
                    let musical_pos = marker.position.musical_position_string();
                    marker_info.push(format!("=END (RENDEREND) ({})", musical_pos));
                }
                
                if !marker_info.is_empty() {
                    let markers_str = format!("    Markers: {}\n", marker_info.join(", "));
                    reaper.show_console_msg(markers_str.as_str());
                }
                
                // Log sections with musical positions
                if !song.sections.is_empty() {
                    reaper.show_console_msg("    Sections:\n");
                    for section in &song.sections {
                        let start_musical = section.start_position.as_ref()
                            .map(|pos| pos.musical_position_string())
                            .unwrap_or_else(|| "N/A".to_string());
                        let end_musical = section.end_position.as_ref()
                            .map(|pos| pos.musical_position_string())
                            .unwrap_or_else(|| "N/A".to_string());
                        let section_info = format!(
                            "      - {} ({} - {})\n",
                            section.display_name(),
                            start_musical,
                            end_musical
                        );
                        reaper.show_console_msg(section_info.as_str());
                    }
                }
                
                reaper.show_console_msg("\n");
                
                info!(
                    song_index = idx,
                    song_name = %song.name,
                    section_count = song.sections.len(),
                    has_count_in = song.count_in_marker.is_some(),
                    has_render_start = song.render_start_marker.is_some(),
                    has_start = song.start_marker.is_some(),
                    has_song_end = song.song_end_marker.is_some(),
                    has_render_end = song.render_end_marker.is_some(),
                    "Song in setlist"
                );
            }
        }
        Err(e) => {
            let error_msg = format!("Error building setlist: {}\n", e);
            reaper.show_console_msg(error_msg.as_str());
            warn!(error = %e, "Failed to build setlist");
        }
    }
    
    reaper.show_console_msg("=====================================\n\n");
}

/// Log all songs in the setlist (summary view)
fn log_setlist_songs_handler() {
    let reaper = Reaper::get();
    
    info!("Building and logging setlist songs...");
    reaper.show_console_msg("\n=== FastTrackStudio: Setlist Songs ===\n");
    
    match build_setlist_from_open_projects(None) {
        Ok(setlist) => {
            let song_count = setlist.song_count();
            if song_count == 0 {
                reaper.show_console_msg("No songs found in setlist.\n");
                info!("No songs in setlist");
            } else {
                reaper.show_console_msg(format!("Found {} song(s) in setlist:\n\n", song_count).as_str());
                info!(song_count, "Found {} songs in setlist", song_count);
                
                
                    for (idx, song) in setlist.songs.iter().enumerate() {
                        let duration = song.duration();
                        let section_count = song.sections.len();
                        
                        // Format song name with artist if available
                        let song_display_name = if let Some(artist) = song.metadata.get("artist") {
                            format!("\"{}\" - {}", song.name, artist)
                        } else {
                            song.name.clone()
                        };
                        
                        // Get project name from song metadata
                        let project_name = song.project_name_from_metadata();
                        let project_info = if project_name != "default" {
                            format!(" [from: {}]", project_name)
                        } else {
                            String::new()
                        };
                        
                        let song_info = format!(
                            "  {}. {} ({:.1}s, {} sections){}\n",
                            idx + 1,
                            song_display_name,
                            duration,
                            section_count,
                            project_info
                        );
                        reaper.show_console_msg(song_info.as_str());
                    
                    info!(
                        song_index = idx,
                        song_name = %song.name,
                        duration = duration,
                        section_count = section_count,
                        "Song in setlist"
                    );
                }
            }
        }
        Err(e) => {
            let error_msg = format!("Error building setlist: {}\n", e);
            reaper.show_console_msg(error_msg.as_str());
            warn!(error = %e, "Failed to build setlist");
        }
    }
    
    reaper.show_console_msg("=====================================\n\n");
}

/// Log detailed information about the current song
fn log_current_song_details_handler() {
    let reaper = Reaper::get();
    
    info!("Building and logging current song details...");
    reaper.show_console_msg("\n=== FastTrackStudio: Current Song Details ===\n");
    
    match build_song_from_current_project() {
        Ok(song) => {
            reaper.show_console_msg(format!("Song: {}\n\n", song.name).as_str());
            info!(song_name = %song.name, "Logging current song details");
            
            // Log markers
            reaper.show_console_msg("Markers:\n");
            
            if let Some(marker) = &song.count_in_marker {
                let time_pos = marker.position.time.to_seconds();
                let musical_pos = marker.position.musical_position_string();
                reaper.show_console_msg(format!(
                    "  Count-In: {:.2}s ({})\n",
                    time_pos,
                    musical_pos
                ).as_str());
            }
            
            if let Some(marker) = &song.render_start_marker {
                let time_pos = marker.position.time.to_seconds();
                let musical_pos = marker.position.musical_position_string();
                reaper.show_console_msg(format!(
                    "  =START (RENDERSTART): {:.2}s ({})\n",
                    time_pos,
                    musical_pos
                ).as_str());
            }
            
            if let Some(marker) = &song.start_marker {
                let time_pos = marker.position.time.to_seconds();
                let musical_pos = marker.position.musical_position_string();
                reaper.show_console_msg(format!(
                    "  SONGSTART: {:.2}s ({})\n",
                    time_pos,
                    musical_pos
                ).as_str());
            }
            
            if let Some(marker) = &song.song_end_marker {
                let time_pos = marker.position.time.to_seconds();
                let musical_pos = marker.position.musical_position_string();
                reaper.show_console_msg(format!(
                    "  SONGEND: {:.2}s ({})\n",
                    time_pos,
                    musical_pos
                ).as_str());
            }
            
            if let Some(marker) = &song.render_end_marker {
                let time_pos = marker.position.time.to_seconds();
                let musical_pos = marker.position.musical_position_string();
                reaper.show_console_msg(format!(
                    "  =END (RENDEREND): {:.2}s ({})\n",
                    time_pos,
                    musical_pos
                ).as_str());
            }
            
            // Log song region boundaries
            if let Some(start_marker) = &song.song_region_start_marker {
                if let Some(end_marker) = &song.song_region_end_marker {
                    let start_time = start_marker.position.time.to_seconds();
                    let end_time = end_marker.position.time.to_seconds();
                    let start_musical = start_marker.position.musical_position_string();
                    let end_musical = end_marker.position.musical_position_string();
                    reaper.show_console_msg(format!(
                        "\nSong Region: {:.2}s ({}) - {:.2}s ({})\n",
                        start_time,
                        start_musical,
                        end_time,
                        end_musical
                    ).as_str());
                }
            }
            
            // Log sections
            if song.sections.is_empty() {
                reaper.show_console_msg("\nSections: None\n");
            } else {
                reaper.show_console_msg(format!("\nSections ({}):\n", song.sections.len()).as_str());
                for (idx, section) in song.sections.iter().enumerate() {
                    let start_time = section.start_seconds().unwrap_or(0.0);
                    let end_time = section.end_seconds().unwrap_or(0.0);
                    let duration = section.duration();
                    let start_musical = section.start_position.as_ref()
                        .map(|p| p.musical_position_string())
                        .unwrap_or_else(|| "N/A".to_string());
                    let end_musical = section.end_position.as_ref()
                        .map(|p| p.musical_position_string())
                        .unwrap_or_else(|| "N/A".to_string());
                    
                    let section_info = format!(
                        "  {}. {} ({:.2}s - {:.2}s, {:.2}s) [{} - {}]\n",
                        idx + 1,
                        section.display_name(),
                        start_time,
                        end_time,
                        duration,
                        start_musical,
                        end_musical
                    );
                    reaper.show_console_msg(section_info.as_str());
                }
            }
            
            // Log song statistics
            let duration = song.duration();
            let render_duration = song.render_duration();
            reaper.show_console_msg(format!(
                "\nStatistics:\n  Duration: {:.2}s\n  Render Duration: {:.2}s\n",
                duration,
                render_duration
            ).as_str());
            
            info!(
                song_name = %song.name,
                duration = duration,
                render_duration = render_duration,
                section_count = song.sections.len(),
                has_count_in = song.count_in_marker.is_some(),
                has_render_start = song.render_start_marker.is_some(),
                has_start = song.start_marker.is_some(),
                has_song_end = song.song_end_marker.is_some(),
                has_render_end = song.render_end_marker.is_some(),
                "Current song details"
            );
        }
        Err(e) => {
            let error_msg = format!("Error building current song: {}\n", e);
            reaper.show_console_msg(error_msg.as_str());
            warn!(error = %e, "Failed to build current song");
        }
    }
    
    reaper.show_console_msg("=====================================\n\n");
}

/// Read all tempo/time signature markers from a REAPER project
#[allow(unsafe_code)] // Required for low-level REAPER API
fn read_tempo_time_sig_markers(project: &Project) -> Vec<(f64, f64, Option<(i32, i32)>)> {
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


/// Get the project measure offset for a project
/// Returns the offset value, or 0 if not found
fn get_project_measure_offset(project: &reaper_high::Project) -> i32 {
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

/// Log current musical position (including measure offset)
fn log_current_musical_position_handler() {
    let reaper = Reaper::get();
    let current_project = reaper.current_project();
    
    info!("Reading current musical position from REAPER...");
    reaper.show_console_msg("\n=== FastTrackStudio: Current Musical Position ===\n");
    
    // Get transport info to get current playhead position
    let transport_adapter = crate::implementation::transport::ReaperTransport::new(current_project.clone());
    
    match transport_adapter.read_transport() {
        Ok(transport) => {
            let playhead_pos = &transport.playhead_position;
            
            // Get the project measure offset
            let measure_offset = get_project_measure_offset(&current_project);
            
            // The musical position already has the offset applied (from reaper_transport.rs)
            // But let's also show the raw measure index for reference
            let time_pos = playhead_pos.time.to_seconds();
            let time_pos_obj = reaper_medium::PositionInSeconds::new(time_pos)
                .unwrap_or(reaper_medium::PositionInSeconds::ZERO);
            let beat_info = current_project.beat_info_at(time_pos_obj);
            let raw_measure = beat_info.measure_index;
            let adjusted_measure = playhead_pos.musical.measure;
            
            info!(
                time_position = time_pos,
                musical_position = %playhead_pos.musical_position_string(),
                measure_offset = measure_offset,
                raw_measure_index = raw_measure,
                adjusted_measure_index = adjusted_measure,
                "Current musical position"
            );
            
            reaper.show_console_msg(format!("Time Position: {:.3}s\n", time_pos).as_str());
            reaper.show_console_msg(format!("Musical Position: {}\n", playhead_pos.musical_position_string()).as_str());
            reaper.show_console_msg(format!("Project Measure Offset: {}\n", measure_offset).as_str());
            reaper.show_console_msg(format!("Raw Measure Index (from REAPER): {}\n", raw_measure).as_str());
            reaper.show_console_msg(format!("Adjusted Measure Index (with offset): {}\n", adjusted_measure).as_str());
            reaper.show_console_msg(format!("Beat: {}\n", playhead_pos.musical.beat + 1).as_str());
            reaper.show_console_msg(format!("Subdivision: {:03}\n", playhead_pos.musical.subdivision).as_str());
        }
        Err(e) => {
            warn!(error = %e, "Failed to read transport state");
            reaper.show_console_msg(format!("Failed to read transport state: {}\n", e).as_str());
        }
    }
}

/// Log project measure offset
fn log_project_measure_offset_handler() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let current_project = reaper.current_project();
    
    info!("Reading project measure offset from REAPER...");
    reaper.show_console_msg("\n=== FastTrackStudio: Project Measure Offset ===\n");
    
    // Get the project measure offset using project_config_var_get_offs (proper way for project vars)
    // The config var name is "projmeasoffs" (project measure offset)
    if let Some(offs_result) = medium_reaper.project_config_var_get_offs("projmeasoffs") {
        // Get the actual value using the offset
        if let Some(addr) = medium_reaper.project_config_var_addr(current_project.context(), offs_result.offset) {
            // Read the integer value directly from the pointer (it's a 32-bit integer)
            let offset_value = unsafe { *(addr.as_ptr() as *const i32) };
            
            info!(project_measure_offset = offset_value, "Project measure offset read successfully");
            reaper.show_console_msg(format!("Project Measure Offset: {}\n", offset_value).as_str());
            reaper.show_console_msg(format!("Offset index: {}\n\n", offs_result.offset).as_str());
            
            // Calculate measure correction (same logic as Lua script)
            // If offset is 0, correction is 1, otherwise 0
            let measure_correction = if offset_value == 0 { 1 } else { 0 };
            reaper.show_console_msg(format!("Measure Correction: {}\n", measure_correction).as_str());
            reaper.show_console_msg("(Measure correction = 1 if offset is 0, otherwise 0)\n");
        } else {
            warn!("Failed to get project config var address");
            reaper.show_console_msg("Failed to get project config var address.\n");
        }
    } else {
        warn!("Failed to get project measure offset config var");
        reaper.show_console_msg("Failed to get project measure offset config var.\n");
        reaper.show_console_msg("This may indicate the config var doesn't exist.\n");
    }
}

/// Log all tempo and time signature changes in the current project
fn log_tempo_time_sig_changes_handler() {
    let reaper = Reaper::get();
    let current_project = reaper.current_project();
    
    info!("Reading tempo/time signature markers from current REAPER project...");
    reaper.show_console_msg("\n=== FastTrackStudio: Tempo and Time Signature Changes ===\n");
    
    // Get default tempo and time signature from project
    let medium_reaper = reaper.medium_reaper();
    let default_tempo = medium_reaper.master_get_tempo().get();
    
    // Get default time signature from beat info at position 0
    let zero_pos = reaper_medium::PositionInSeconds::ZERO;
    let beat_info = current_project.beat_info_at(zero_pos);
    let default_time_sig = (
        beat_info.time_signature.numerator.get() as i32,
        beat_info.time_signature.denominator.get() as i32,
    );
    
    reaper.show_console_msg(format!(
        "Default: {:.1} BPM, {}/{}\n\n",
        default_tempo, default_time_sig.0, default_time_sig.1
    ).as_str());
    
    // Read all tempo/time signature markers
    let markers = read_tempo_time_sig_markers(&current_project);
    
    if markers.is_empty() {
        reaper.show_console_msg("No tempo/time signature changes found.\n");
        info!("No tempo/time signature markers found");
    } else {
        // Filter to only show actual changes (tempo or time signature changed)
        let mut changes = Vec::new();
        let mut prev_tempo = default_tempo;
        let mut prev_time_sig = Some(default_time_sig);
        
        for (time_pos, tempo, time_sig) in &markers {
            let tempo_changed = (tempo - prev_tempo).abs() > 0.01; // Allow small floating point differences
            let time_sig_changed = match (&prev_time_sig, time_sig) {
                (Some(prev), Some(current)) => prev != current,
                (None, Some(_)) => true,
                (Some(_), None) => false, // Time sig removed, but we'll still show it
                (None, None) => false,
            };
            
            // Only include if tempo or time signature actually changed
            if tempo_changed || time_sig_changed {
                changes.push((*time_pos, *tempo, *time_sig));
                prev_tempo = *tempo;
                prev_time_sig = *time_sig;
            } else {
                // No change, but update our tracking for next iteration
                prev_tempo = *tempo;
                if let Some(ts) = time_sig {
                    prev_time_sig = Some(*ts);
                }
            }
        }
        
        if changes.is_empty() {
            reaper.show_console_msg("No tempo/time signature changes found (all markers have same values).\n");
            info!("No tempo/time signature changes found");
        } else {
            reaper.show_console_msg(format!("Found {} tempo/time signature change(s):\n\n", changes.len()).as_str());
            info!(change_count = changes.len(), "Found {} tempo/time signature changes", changes.len());
            
            // Track previous values for comparison
            let mut display_prev_tempo = default_tempo;
            let mut display_prev_time_sig = Some(default_time_sig);
            
            for (idx, (time_pos, tempo, time_sig)) in changes.iter().enumerate() {
                // Convert time position to musical position for display
                let time_pos_obj = reaper_medium::PositionInSeconds::new(*time_pos)
                    .unwrap_or(reaper_medium::PositionInSeconds::ZERO);
                let beat_info = current_project.beat_info_at(time_pos_obj);
                let measure_offset = get_project_measure_offset(&current_project);
                let measure = beat_info.measure_index + measure_offset; // Apply project measure offset
                let beats_since_measure = beat_info.beats_since_measure.get();
                let beat = beats_since_measure.floor() as i32;
                let subdivision = ((beats_since_measure - beats_since_measure.floor()) * 1000.0).round() as i32;
                let subdivision = subdivision.max(0).min(999);
                let musical_pos = format!("{}.{}.{:03}", measure, beat, subdivision);
                
                // Determine what changed from previous value
                let mut change_parts = Vec::new();
                
                // Check if tempo changed from previous
                let tempo_changed = (tempo - display_prev_tempo).abs() > 0.01;
                if tempo_changed {
                    change_parts.push(format!("{:.1} BPM", tempo));
                }
                
                // Check if time signature changed from previous
                let time_sig_changed = match (&display_prev_time_sig, time_sig) {
                    (Some(prev), Some(current)) => prev != current,
                    (None, Some(_)) => true,
                    (Some(_), None) => true,
                    (None, None) => false,
                };
                
                if time_sig_changed {
                    if let Some((num, den)) = time_sig {
                        change_parts.push(format!("Time Signature: {}/{}", num, den));
                    }
                }
                
                // Update tracking for next iteration
                display_prev_tempo = *tempo;
                display_prev_time_sig = *time_sig;
                
                let change_description = change_parts.join(", ");
                
                let marker_info = format!(
                    "  {}. {:.2}s ({}) - {}\n",
                    idx + 1,
                    time_pos,
                    musical_pos,
                    change_description
                );
                
                reaper.show_console_msg(marker_info.as_str());
                
                info!(
                    change_index = idx,
                    time_position = time_pos,
                    tempo = tempo,
                    time_signature = ?time_sig,
                    musical_position = %musical_pos,
                    "Tempo/time signature change"
                );
            }
        }
    }
    
    reaper.show_console_msg("=====================================\n\n");
}

/// Create text items from lyrics input - one bar per slide
fn create_text_items_from_lyrics_handler() {
    lyrics::write::create_text_items_from_lyrics("".to_string()).unwrap_or_else(|e| {
        tracing::error!("Failed to create text items from lyrics: {}", e);
    });
}

/// Register all actions for FastTrackStudio extension
pub fn register_all_actions() {
    let actions = vec![
        ActionDef {
            command_id: "FTS_DEV_DUMMY_ACTION",
            display_name: "Dummy Action".to_string(),
            handler: dummy_action_handler,
            appears_in_menu: true, // Show in menu
            section: crate::infrastructure::action_registry::ActionSection::Main,
        },
        ActionDef {
            command_id: "FTS_DEV_LOG_OPEN_PROJECTS",
            display_name: "Log Open Projects".to_string(),
            handler: log_open_projects_handler,
            appears_in_menu: true, // Show in menu
            section: crate::infrastructure::action_registry::ActionSection::Main,
        },
        ActionDef {
            command_id: "FTS_DEV_LOG_CURRENT_MARKERS",
            display_name: "Log Current Project Markers".to_string(),
            handler: log_current_project_markers_handler,
            appears_in_menu: true, // Show in menu
            section: crate::infrastructure::action_registry::ActionSection::Main,
        },
        ActionDef {
            command_id: "FTS_DEV_LOG_CURRENT_REGIONS",
            display_name: "Log Current Project Regions".to_string(),
            handler: log_current_project_regions_handler,
            appears_in_menu: true, // Show in menu
            section: crate::infrastructure::action_registry::ActionSection::Main,
        },
        ActionDef {
            command_id: "FTS_DEV_LOG_REGION_COLORS",
            display_name: "Log Region Colors (Debug)".to_string(),
            handler: log_region_colors_handler,
            appears_in_menu: true, // Show in menu
            section: crate::infrastructure::action_registry::ActionSection::Main,
        },
        ActionDef {
            command_id: "FTS_DEV_BUILD_SETLIST",
            display_name: "Build Setlist from Open Projects".to_string(),
            handler: build_setlist_from_projects_handler,
            appears_in_menu: true, // Show in menu
            section: crate::infrastructure::action_registry::ActionSection::Main,
        },
        ActionDef {
            command_id: "FTS_DEV_LOG_SETLIST_SONGS",
            display_name: "Log Setlist Songs".to_string(),
            handler: log_setlist_songs_handler,
            appears_in_menu: true, // Show in menu
            section: crate::infrastructure::action_registry::ActionSection::Main,
        },
        ActionDef {
            command_id: "FTS_DEV_LOG_CURRENT_SONG",
            display_name: "Log Current Song Details".to_string(),
            handler: log_current_song_details_handler,
            appears_in_menu: true, // Show in menu
            section: crate::infrastructure::action_registry::ActionSection::Main,
        },
        ActionDef {
            command_id: "FTS_DEV_LOG_TEMPO_TIME_SIG",
            display_name: "Log Tempo and Time Signature Changes".to_string(),
            handler: log_tempo_time_sig_changes_handler,
            appears_in_menu: true, // Show in menu
            section: crate::infrastructure::action_registry::ActionSection::Main,
        },
        ActionDef {
            command_id: "FTS_DEV_LOG_PROJECT_MEASURE_OFFSET",
            display_name: "Log Project Measure Offset".to_string(),
            handler: log_project_measure_offset_handler,
            appears_in_menu: true, // Show in menu
            section: crate::infrastructure::action_registry::ActionSection::Main,
        },
        ActionDef {
            command_id: "FTS_DEV_LOG_CURRENT_MUSICAL_POSITION",
            display_name: "Log Current Musical Position".to_string(),
            handler: log_current_musical_position_handler,
            appears_in_menu: true, // Show in menu
            section: crate::infrastructure::action_registry::ActionSection::Main,
        },
    ];
    
    register_actions(&actions, "FastTrackStudio");
}
