//! Load an RPP project + decode its audio sources via the unified
//! `project_loader::load_rpp` path, then render a short stereo block
//! and print the resulting peak levels.
//!
//! Run with:
//!
//! ```bash
//! cargo run -p daw-standalone --features rpp-loader --example play_rpp -- /path/to/song.rpp
//! ```
//!
//! Note: this example does NOT drive the cpal output stream from the
//! project renderer yet — that integration lives in a follow-up task.
//! For now it demonstrates the load + materialize + render pipeline.

use std::path::PathBuf;

use daw_standalone::audio_engine::render::ProjectRenderer;
use daw_standalone::project_loader::load_rpp;
use daw_standalone::sync::Standalone;

fn main() -> Result<(), String> {
    let path = std::env::args()
        .nth(1)
        .ok_or("usage: play_rpp <rpp-file>")?;
    let rpp_path = PathBuf::from(&path);
    let rpp_text = std::fs::read_to_string(&rpp_path).map_err(|e| e.to_string())?;
    let project_dir = rpp_path
        .parent()
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("."));

    let daw = Standalone::new();
    let project_name = rpp_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("project");

    println!("Loading {path}...");
    let (proj, audio) = load_rpp(
        &daw,
        project_name,
        rpp_path.to_string_lossy().as_ref(),
        &rpp_text,
        |file_path| {
            // Resolve relative paths against the project dir.
            let pb = PathBuf::from(file_path);
            let abs = if pb.is_absolute() {
                pb
            } else {
                project_dir.join(pb)
            };
            std::fs::read(&abs).map_err(|e| format!("read {}: {e}", abs.display()))
        },
    )?;

    println!(
        "  tracks={} items={} takes={} markers={} regions={} tempo_points={} hw_outs={}",
        proj.track_count,
        proj.item_count,
        proj.take_count,
        proj.marker_count,
        proj.region_count,
        proj.tempo_point_count,
        proj.hw_output_count,
    );
    println!(
        "  decoded {} audio sources ({} failed, {} no source)",
        audio.loaded,
        audio.failed.len(),
        audio.skipped_no_source,
    );
    for (take, err) in &audio.failed {
        eprintln!("    ! {take}: {err}");
    }

    // Render the first 2 seconds and report peak.
    let sample_rate = 48_000;
    let frames = (sample_rate as usize) * 2;
    let block = ProjectRenderer::new(&daw, &proj.project_guid, sample_rate).render_block(0, frames);
    let (peak_l, peak_r) = peak_stereo(&block.samples);
    println!("first 2s peak — L={peak_l:.4}, R={peak_r:.4}");

    Ok(())
}

fn peak_stereo(samples: &[f32]) -> (f32, f32) {
    let mut l: f32 = 0.0;
    let mut r: f32 = 0.0;
    for (i, s) in samples.iter().enumerate() {
        let v = s.abs();
        if i & 1 == 0 {
            if v > l {
                l = v;
            }
        } else if v > r {
            r = v;
        }
    }
    (l, r)
}
