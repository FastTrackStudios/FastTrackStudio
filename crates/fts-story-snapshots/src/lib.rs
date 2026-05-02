//! Headless visual regression runner backed by Blitz.
//!
//! Stub. Planned shape:
//!
//! ```ignore
//! use fts_story_snapshots::{Runner, RunnerConfig};
//! let runner = Runner::new(RunnerConfig::default());
//! runner.snapshot_all(); // iterates STORIES × auto_states, writes PNGs
//! runner.assert_against_baseline(); // dssim diff against snapshots/
//! ```
//!
//! Expected pieces (cribbed from blitz-vrt's design — pending license
//! resolution we either vendor or rewrite from scratch):
//!
//! - `render::render_story(story, knob_values, viewport) -> Vec<u8>` —
//!   builds a `DioxusDocument`, runs the settling loop (resolve → drain
//!   net queue → resolve, capped at 100 iters), paints via
//!   `anyrender_vello_cpu`, encodes as PNG.
//! - `interaction::execute(doc, &Interaction)` — translates each
//!   `Interaction` step into the corresponding `UiEvent` and pumps the
//!   `EventDriver`. Reuses our patched `dioxus-native-dom` so the
//!   RefCell-borrow fix is in play.
//! - `diff::compare(baseline, candidate, threshold)` — DSSIM perceptual
//!   diff with red-overlay diff PNG written to `diff_output/` on failure.
//! - `harness::generate_tests!()` — declarative macro consumers expand
//!   in their own `tests/` to emit one `#[test]` per story × state.
//!
//! Critical guard (lifted from blitz-vrt): refuse to run with
//! `debug_assertions` on. Stylo/Parley produce *wrong* renders in debug
//! mode, not just slow ones.

#[cfg(debug_assertions)]
compile_error!(
    "fts-story-snapshots must be built with `--release`. Stylo/Parley produce \
     incorrect renders under debug_assertions, not merely slow ones."
);
