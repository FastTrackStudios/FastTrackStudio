//! Tempo and Time Signature Reading
//!
//! Reads tempo and time signature markers from REAPER projects.

use reaper_high::{Project, Reaper};

/// Read all tempo/time signature markers from a REAPER project
/// Returns Vec<(time_position_seconds, tempo_bpm, time_signature_option)>
#[allow(unsafe_code)] // Required for low-level REAPER API
pub fn read_tempo_time_sig_markers_from_project(
    project: &Project,
) -> Vec<(f64, f64, Option<(i32, i32)>)> {
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
