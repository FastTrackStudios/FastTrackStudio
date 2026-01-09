//! Zoom Actions
//!
//! Actions for zooming the arrange view to song regions and markers

use fts::setlist::infra::reaper::read_markers_from_project;
use fts::setlist::infra::traits::SetlistBuilder;
use reaper_high::{Project, Reaper};
use reaper_medium::{PositionInSeconds, ProjectContext};
use tracing::{info, warn};

/// Find Count-In marker position in a project
fn find_count_in_marker(project: &Project) -> Option<f64> {
    let markers = read_markers_from_project(project).ok()?;
    markers
        .iter()
        .find(|m| m.name.trim().eq_ignore_ascii_case("Count-In"))
        .map(|m| m.position.time.to_seconds())
}

/// Zoom horizontally to song (2 measures before Count-In, 2 measures after Song Region end)
pub fn zoom_horizontally_to_song() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    info!("Zoom Horizontally to Song action executed");

    // Get current project
    let project_result = match medium_reaper.enum_projects(reaper_medium::ProjectRef::Current, 0) {
        Some(result) => result,
        None => {
            warn!("No current project for zoom to song");
            reaper.show_console_msg("Zoom to Song: No current project\n");
            return;
        }
    };

    let project = Project::new(project_result.project);
    let project_context = project.context();

    // Find Count-In marker
    let count_in_pos = match find_count_in_marker(&project) {
        Some(pos) => PositionInSeconds::new(pos).unwrap_or(PositionInSeconds::ZERO),
        None => {
            warn!("Count-In marker not found");
            reaper.show_console_msg("Zoom to Song: Count-In marker not found. Please add a 'Count-In' marker to your project.\n");
            return;
        }
    };

    // Use FastTrackStudio's song building to find the song region end
    // Use the trait method directly on the current project
    // Use trait method on Reaper (operates on current project)
    let reaper = Reaper::get();
    let song_region_end_pos = match reaper.build_song_from_current_project() {
        Ok(song) => {
            // Use song_region_end() to get the actual song region end marker position
            // This is the end of the song region, which usually extends past the =END marker
            match song.song_region_end() {
                Some(position) => {
                    let region_end_seconds = position.time.to_seconds();
                    match PositionInSeconds::new(region_end_seconds) {
                        Ok(pos) => pos,
                        Err(_) => {
                            warn!("Invalid song_region_end position, using default");
                            PositionInSeconds::new(count_in_pos.get() + 120.0)
                                .unwrap_or(count_in_pos)
                        }
                    }
                }
                None => {
                    // Fall back to render_end() if song_region_end_marker is not set
                    let render_end_seconds = song.render_end();
                    if render_end_seconds > 0.0 {
                        match PositionInSeconds::new(render_end_seconds) {
                            Ok(pos) => pos,
                            Err(_) => {
                                warn!("Invalid render_end position, using default");
                                PositionInSeconds::new(count_in_pos.get() + 120.0)
                                    .unwrap_or(count_in_pos)
                            }
                        }
                    } else {
                        warn!("Song region end not found, using default range around Count-In");
                        reaper.show_console_msg("Zoom to Song: Song Region end not found. Using default range around Count-In marker.\n");
                        PositionInSeconds::new(count_in_pos.get() + 120.0).unwrap_or(count_in_pos)
                    }
                }
            }
        }
        Err(e) => {
            warn!(error = %e, "Failed to build song from current project, using default range");
            reaper.show_console_msg(
                "Zoom to Song: Failed to build song. Using default range around Count-In marker.\n",
            );
            PositionInSeconds::new(count_in_pos.get() + 120.0).unwrap_or(count_in_pos)
        }
    };

    // Calculate 2 measures before Count-In
    let zoom_start = calculate_measures_before(medium_reaper, project_context, count_in_pos, 2);

    // Calculate 2 measures after Song Region end
    let zoom_end = calculate_measures_after(medium_reaper, project_context, song_region_end_pos, 2);

    // Ensure zoom_start is not negative
    let zoom_start = if zoom_start.get() < 0.0 {
        PositionInSeconds::ZERO
    } else {
        zoom_start
    };

    // Set arrange view using low-level API (reaper-rs doesn't have a wrapper for set)
    unsafe {
        let mut start_time = zoom_start.get();
        let mut end_time = zoom_end.get();

        // GetSet_ArrangeView2 with isSet=true to set the view
        // Use screen_x_start=screen_x_end=0 to set the full arrange view
        medium_reaper.low().GetSet_ArrangeView2(
            project_context.to_raw(),
            true, // isSet
            0,    // screen_x_start
            0,    // screen_x_end
            &mut start_time,
            &mut end_time,
        );
    }

    // Update arrange view
    medium_reaper.update_arrange();

    info!(
        zoom_start = zoom_start.get(),
        zoom_end = zoom_end.get(),
        "Zoomed horizontally to song"
    );
    reaper.show_console_msg(format!(
        "Zoom to Song: {}s - {}s\n",
        zoom_start.get(),
        zoom_end.get()
    ));
}

/// Calculate a position N measures before a given position
fn calculate_measures_before(
    medium_reaper: &reaper_medium::Reaper,
    project_context: ProjectContext,
    position: PositionInSeconds,
    num_measures: i32,
) -> PositionInSeconds {
    // Get beat info at position to find current measure
    let beat_info = medium_reaper.time_map_2_time_to_beats(project_context, position);
    let current_measure_index = beat_info.measure_index;

    // Calculate target measure index
    let target_measure_index = current_measure_index.saturating_sub(num_measures);

    // Get measure info for the target measure
    let measure_info =
        medium_reaper.time_map_get_measure_info(project_context, target_measure_index);

    // Return the start time of that measure
    measure_info.start_time
}

/// Calculate a position N measures after a given position
fn calculate_measures_after(
    medium_reaper: &reaper_medium::Reaper,
    project_context: ProjectContext,
    position: PositionInSeconds,
    num_measures: i32,
) -> PositionInSeconds {
    // Get beat info at position to find current measure
    let beat_info = medium_reaper.time_map_2_time_to_beats(project_context, position);
    let current_measure_index = beat_info.measure_index;

    // Calculate target measure index (use end measure of current position + num_measures)
    // If we're at the start of a measure, we want that measure + num_measures
    // If we're in the middle, we want the next measure + num_measures
    let target_measure_index = current_measure_index + num_measures;

    // Get measure info for the target measure
    let measure_info =
        medium_reaper.time_map_get_measure_info(project_context, target_measure_index);

    // Return the end time of that measure (start of next measure)
    // The end_qn represents the start of the next measure
    // We need to convert it to time using time_map_qn_to_time
    let end_qn = measure_info.end_qn;

    // Use time_map_qn_to_time (note: this might be different from time_map_2_qn_to_time)
    // Check what's available in reaper-medium
    // For now, get the start time of the next measure (target_measure_index + 1)
    let next_measure_info =
        medium_reaper.time_map_get_measure_info(project_context, target_measure_index + 1);

    next_measure_info.start_time
}
