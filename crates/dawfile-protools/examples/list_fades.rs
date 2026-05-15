//! List all fades on each track using the parser, plus the referenced region.

use std::env;

fn main() {
    let path = env::args().nth(1).expect("usage: list_fades <file>");
    let session = dawfile_protools::read_session(&path, 0).expect("parse");
    println!(
        "session: {} tracks  sr={}",
        session.audio_tracks.len(),
        session.session_sample_rate
    );
    let sr = session.session_sample_rate as f64;
    for t in &session.audio_tracks {
        if t.fades.is_empty() {
            continue;
        }
        println!("\nTrack: {}", t.name);
        for f in &t.fades {
            let region_name = session
                .audio_regions
                .iter()
                .find(|r| r.index == f.region_index)
                .map(|r| (&r.name[..], r.length, r.start_pos))
                .unwrap_or(("<missing>", 0, 0));
            println!(
                "  fade start={} ({:.3}s)  length={} ({:.3}s)  region#{}={:?} (region.length={}, region.start={})",
                f.start_pos,
                f.start_pos as f64 / sr,
                f.length,
                f.length as f64 / sr,
                f.region_index,
                region_name.0,
                region_name.1,
                region_name.2,
            );
        }
    }
}
