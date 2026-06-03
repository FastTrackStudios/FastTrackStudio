//! Standalone native (Blitz) showcase for the reusable DAW panels.
//!
//! Renders [`DawWorkspace`] — ArrangeView (with its TrackControlPanel sidebar)
//! over a MixerControlPanel — against a set of sample tracks. Run with:
//!
//! ```sh
//! cargo run -p ui-showcase          # native Blitz window
//! # or, with hot-reload:  dx serve -p ui-showcase
//! ```

use audio_controls::panels::model::ClipView;
use audio_controls::panels::{DawWorkspace, TrackView};
use dioxus::prelude::*;

fn main() {
    dioxus_native::launch(App);
}

#[component]
fn App() -> Element {
    // Built once (the init closure runs in this component's scope, so the
    // per-track signals live here and survive re-renders).
    let tracks = use_signal(sample_tracks);

    rsx! {
        div {
            style: "position:fixed; inset:0; display:flex; flex-direction:column; \
                    background:#09090b; color:#e4e4e7; \
                    font-family:'Inter','SF Pro Display',system-ui,sans-serif; overflow:hidden;",

            // Title bar.
            div {
                style: "flex:0 0 auto; display:flex; align-items:center; gap:10px; \
                        padding:6px 12px; background:#101013; border-bottom:1px solid #27272a; \
                        font-size:12px; font-weight:700; letter-spacing:0.04em;",
                span { style: "color:#f4f4f5;", "FastTrackStudio" }
                span { style: "color:#71717a; font-weight:500;", "UI Showcase — DawWorkspace" }
            }

            // The workspace fills the rest.
            div {
                style: "flex:1 1 0; min-height:0;",
                DawWorkspace { tracks: tracks() }
            }
        }
    }
}

/// A small, folder-nested sample project to exercise all three panels.
fn sample_tracks() -> Vec<TrackView> {
    let clip = |start: f64, len: f64, name: &str| ClipView {
        start,
        length: len,
        name: name.to_string(),
        color: None,
    };

    vec![
        TrackView::new(0, "DRUMS", Some("#ef4444"))
            .folder()
            .levels(0.6, 0.55, 0.7),
        TrackView::new(1, "Kick", Some("#f97316"))
            .depth(1)
            .fader(0.8)
            .levels(0.7, 0.0, 0.8)
            .clips(vec![clip(0.0, 16.0, "Kick"), clip(20.0, 24.0, "Kick B")]),
        TrackView::new(2, "Snare", Some("#f59e0b"))
            .depth(1)
            .fader(0.7)
            .levels(0.5, 0.0, 0.6)
            .clips(vec![clip(0.0, 44.0, "Snare")]),
        TrackView::new(3, "OHs", Some("#eab308"))
            .depth(1)
            .levels(0.4, 0.42, 0.5)
            .clips(vec![clip(2.0, 40.0, "Overheads")]),
        TrackView::new(4, "BASS", Some("#22c55e"))
            .fader(0.72)
            .levels(0.65, 0.0, 0.75)
            .clips(vec![
                clip(0.0, 32.0, "Bass DI"),
                clip(36.0, 20.0, "Bass Amp"),
            ]),
        TrackView::new(5, "GTRS", Some("#38bdf8"))
            .folder()
            .levels(0.5, 0.48, 0.6),
        TrackView::new(6, "Rhythm L", Some("#0ea5e9"))
            .depth(1)
            .routing(true, false)
            .clips(vec![clip(8.0, 48.0, "Rhythm L")]),
        TrackView::new(7, "Rhythm R", Some("#0284c7"))
            .depth(1)
            .routing(true, false)
            .clips(vec![clip(8.0, 48.0, "Rhythm R")]),
        TrackView::new(8, "VOX", Some("#a855f7"))
            .fader(0.78)
            .levels(0.55, 0.5, 0.85)
            .routing(false, true)
            .clips(vec![clip(16.0, 40.0, "Lead Vox"), clip(60.0, 30.0, "Dbl")]),
    ]
}
