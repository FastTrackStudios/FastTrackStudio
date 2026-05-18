//! End-to-end: parse a small synthetic RPP and verify the loaded
//! `ProjectState` carries tracks, items, fades, markers, regions,
//! tempo points, hardware outputs, and folder nesting.

#![cfg(feature = "rpp-project")]

use daw_proto::project::ProjectContext;
use daw_proto::{Items, Routing, TrackRef, Tracks};
use daw_standalone::project_loader::load_rpp_text;
use daw_standalone::sync::Standalone;

const RPP: &str = r#"<REAPER_PROJECT 0.1 "7.21/linux-x86_64" 1759700000
  TEMPO 96 4 4
  <TEMPOENVEX
    PT 0 96 4 262148 0 0 0
    PT 8 120 0 0 0 0 0
  >
  MARKER 1 4.0 "Verse" 0 0 1 R 0
  MARKER 2 8.0 "Chorus" 0 0 1 R 0
  MARKER 3 0.0 "Loop" 0 1 1 R 0 16.0
  <TRACK
    NAME "Drums"
    PEAKCOL 16576
    SEL 0
    VOLPAN 1.0 0.0 -1 -1 1
    MUTESOLO 0 0 0
    IPHASE 0
    ISBUS 1 1
    BUSCOMP 0 0 0 0 0
    SHOWINMIX 1 0.6667 0.5 1 0.5 0 -1 -1
    MAINSEND 1 0
    NCHAN 4
    HWOUT 0 0 1.0 0.0 0 0 0 -1.0 -1
    HWOUT 2 0 1.0 0.0 0 0 2 -1.0 -1
    <ITEM
      POSITION 0.0
      LENGTH 2.0
      FADEIN 0 0.05 0 0 0 0 0
      FADEOUT 0 0.1 0 0 0 0 0
      NAME "Kick"
      VOLPAN 1.0 0.0 1.0 -1.0
      IGUID {12345678-1234-1234-1234-123456789ABC}
      GUID {ABCDEFAB-CDEF-ABCD-EFAB-CDEFABCDEFAB}
      <SOURCE WAVE
        FILE "drums.wav"
      >
    >
  >
  <TRACK
    NAME "Kick"
    SEL 0
    VOLPAN 1.0 0.0 -1 -1 1
    MUTESOLO 0 0 0
    ISBUS 0 1
    NCHAN 2
    MAINSEND 0 0
  >
  <TRACK
    NAME "Snare"
    SEL 0
    VOLPAN 1.0 0.0 -1 -1 1
    MUTESOLO 0 0 0
    ISBUS 2 1
    NCHAN 2
  >
>
"#;

#[test]
fn loads_tracks_items_markers_tempo_routing() {
    let daw = Standalone::new();
    let summary = load_rpp_text(&daw, "Test", "/tmp/test.rpp", RPP).unwrap();

    assert_eq!(summary.track_count, 3);
    assert_eq!(summary.item_count, 1);
    assert!(summary.marker_count >= 2, "got {}", summary.marker_count);
    assert!(summary.region_count >= 1, "got {}", summary.region_count);
    assert_eq!(summary.tempo_point_count, 2);
    assert_eq!(summary.hw_output_count, 2);

    let ctx = ProjectContext::Project(summary.project_guid.clone());
    let tracks = Tracks::all(&daw, ctx.clone());
    assert_eq!(tracks.len(), 3);
    assert_eq!(tracks[0].name, "Drums");
    assert!(tracks[0].is_folder);
    assert_eq!(tracks[1].name, "Kick");
    assert_eq!(tracks[2].name, "Snare");

    // Kick + Snare should both have Drums as parent.
    let drums_guid = tracks[0].guid.clone();
    assert_eq!(tracks[1].parent_guid.as_ref(), Some(&drums_guid));
    assert_eq!(tracks[2].parent_guid.as_ref(), Some(&drums_guid));

    // 4-channel drums bus.
    assert_eq!(
        daw.track_num_channels(&ctx, &TrackRef::Guid(drums_guid.clone())),
        Some(4)
    );

    // Master send disabled on Kick (MAINSEND 0).
    assert!(!Routing::parent_send_enabled(
        &daw,
        ctx.clone(),
        TrackRef::Guid(tracks[1].guid.clone())
    ));

    // Hardware outputs on Drums.
    let hw = Routing::hardware_outputs(&daw, ctx.clone(), TrackRef::Guid(drums_guid.clone()));
    assert_eq!(hw.len(), 2);

    // Item with fades.
    let items = Items::get_items(&daw, ctx.clone(), TrackRef::Guid(drums_guid));
    assert_eq!(items.len(), 1);
    let it = &items[0];
    assert!((it.length.as_seconds() - 2.0).abs() < 1e-6);
    assert!((it.fade_in_length.as_seconds() - 0.05).abs() < 1e-6);
    assert!((it.fade_out_length.as_seconds() - 0.1).abs() < 1e-6);

    // Take name lives on the active take.
    use daw_proto::{ItemRef, Takes};
    let active = Takes::get_active_take(&daw, ctx, ItemRef::Guid(it.guid.clone())).unwrap();
    assert_eq!(active.name, "Kick");
}
