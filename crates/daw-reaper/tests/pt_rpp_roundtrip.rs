//! Round-trip integrity for the Pro Tools → REAPER conversion.
//!
//! 1. `ptx → parsed → rpp → parsed`: convert a PT session to an RPP string,
//!    parse that RPP back into a typed `ReaperProject`, and assert the
//!    structure survives the RPP serialize/parse round-trip (track count,
//!    names, and item counts match what the converter emitted). This is a
//!    pure-Rust, CI-runnable check.
//!
//! 2. `ptx → parsed → rpp → parsed → ptx → parsed`: additionally write the RPP
//!    back to a PTX via the official converter and re-parse it. The converter
//!    is macOS-only, so this test skips when it isn't installed.

use dawfile_reaper::types::project::ReaperProject;

const FIXTURES: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../dawfile-protools/tests/fixtures/"
);

/// Fixtures exercised by the round-trip. Pure parsing, so all of them run on CI.
const SESSIONS: &[&str] = &[
    "TestPTX.ptx",
    "HeyLady.ptx",
    "RegionTest.ptx",
    "choir-session.ptx",
];

#[test]
fn ptx_to_rpp_to_parsed_preserves_structure() {
    for name in SESSIONS {
        let path = format!("{FIXTURES}{name}");

        // ptx → parsed → rpp
        let rpp = daw_reaper::project_import::protools_to_rpp(&path)
            .unwrap_or_else(|e| panic!("{name}: ptx→rpp failed: {e}"));

        // What the converter emitted, counted from the RPP text.
        let emitted_track_blocks = count_blocks(&rpp, "<TRACK");
        let emitted_items = count_blocks(&rpp, "<ITEM");

        // rpp → parsed
        let rpp_proj = dawfile_reaper::parse_rpp_file(&rpp)
            .unwrap_or_else(|e| panic!("{name}: rpp parse failed: {e}"));
        let proj = ReaperProject::from_rpp_project(&rpp_proj)
            .unwrap_or_else(|e| panic!("{name}: typed decode failed: {e}"));

        // Track count survives the round-trip.
        assert_eq!(
            proj.tracks.len(),
            emitted_track_blocks,
            "{name}: track count changed through RPP round-trip \
             (emitted {emitted_track_blocks}, re-parsed {})",
            proj.tracks.len()
        );

        // Every track keeps a name.
        assert!(
            proj.tracks.iter().all(|t| !t.name.is_empty()),
            "{name}: a track lost its name through the RPP round-trip"
        );

        // Item count survives the round-trip.
        let reparsed_items: usize = proj.tracks.iter().map(|t| t.items.len()).sum();
        assert_eq!(
            reparsed_items, emitted_items,
            "{name}: item count changed through RPP round-trip \
             (emitted {emitted_items}, re-parsed {reparsed_items})"
        );
    }
}

/// `ptx → rpp → ptx → parsed` — exercises the RPP→PTX writer (official
/// converter). Skipped when the converter binary isn't installed (CI/Linux).
#[test]
fn ptx_to_rpp_to_ptx_to_parsed_via_converter() {
    if dawfile_protools::write::find_converter_binary().is_err() {
        eprintln!("skip: PT Reaper Converter not installed");
        return;
    }

    let src = format!("{FIXTURES}TestPTX.ptx");
    let rpp = daw_reaper::project_import::protools_to_rpp(&src).expect("ptx→rpp");

    let tmp = std::env::temp_dir();
    let rpp_path = tmp.join("roundtrip.rpp");
    let ptx_path = tmp.join("roundtrip.ptx");
    std::fs::write(&rpp_path, &rpp).unwrap();
    dawfile_protools::write::rpp_to_ptx_via_converter(&rpp_path, &ptx_path)
        .expect("rpp→ptx via converter");

    // Re-parse the written PTX: it must round-trip to a non-empty session
    // whose track count matches the RPP we fed in.
    let session = dawfile_protools::read_session(ptx_path.to_str().unwrap(), 0).unwrap();
    let total = session.audio_tracks.len() + session.midi_tracks.len();
    let rpp_tracks = count_blocks(&rpp, "<TRACK");
    assert!(total > 0, "written PTX parsed to zero tracks");
    assert_eq!(
        total, rpp_tracks,
        "track count differs after ptx→rpp→ptx→parsed (rpp {rpp_tracks}, ptx {total})"
    );
}

/// Count opening RPP blocks of the given tag (e.g. `<TRACK`, `<ITEM`).
fn count_blocks(rpp: &str, tag: &str) -> usize {
    rpp.lines()
        .filter(|l| l.trim_start().starts_with(tag))
        .count()
}
