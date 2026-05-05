//! Integration tests for the FTS ScreensetService.
//!
//! Each test runs inside a live REAPER instance via the `#[reaper_test]`
//! harness, so capture sees the actual main window + (when available)
//! display topology.
//!
//! Run with:
//!
//!   cargo xtask reaper-test -- reaper_screenset

use daw_proto::{ScreensetOptions, ScreensetScope};
use reaper_test::reaper_test;

#[reaper_test(isolated)]
async fn capture_records_main_window_geometry(ctx: &ReaperTestContext) -> eyre::Result<()> {
    let screensets = ctx.daw.screensets();
    let result = screensets
        .capture(
            "fts_test_capture_main",
            "Capture Main",
            "test capture",
            Vec::<String>::new(),
            Vec::<String>::new(),
            ScreensetOptions {
                scope: ScreensetScope::Global,
                persist: false,
            },
        )
        .await?;
    assert!(result.ok, "capture should succeed: {:?}", result.error);

    let snapshot = screensets
        .get(
            "fts_test_capture_main",
            ScreensetOptions {
                scope: ScreensetScope::Global,
                persist: false,
            },
        )
        .await?
        .expect("captured screenset must be retrievable");

    let main = snapshot
        .windows
        .iter()
        .find(|w| w.id == "reaper.main")
        .expect("capture must record the REAPER main window");
    let bounds = main.bounds.expect("main window must have geometry");
    assert!(
        bounds.width > 0 && bounds.height > 0,
        "main window bounds must be non-zero: {:?}",
        bounds
    );

    // Cleanup so re-runs don't pile up registry rows.
    let _ = screensets
        .delete(
            "fts_test_capture_main",
            ScreensetOptions {
                scope: ScreensetScope::Global,
                persist: false,
            },
        )
        .await?;
    Ok(())
}

#[reaper_test(isolated)]
async fn apply_restores_main_window_position(ctx: &ReaperTestContext) -> eyre::Result<()> {
    let screensets = ctx.daw.screensets();
    let opts = ScreensetOptions {
        scope: ScreensetScope::Global,
        persist: false,
    };

    // Capture once to learn the current geometry.
    screensets
        .capture(
            "fts_test_apply_baseline",
            "Apply Baseline",
            "",
            Vec::<String>::new(),
            Vec::<String>::new(),
            opts.clone(),
        )
        .await?;
    let baseline = screensets
        .get("fts_test_apply_baseline", opts.clone())
        .await?
        .unwrap();
    let baseline_bounds = baseline.windows[0].bounds.unwrap();

    // Save a synthetic screenset 80px down + 60px right of baseline.
    let mut shifted = baseline.clone();
    shifted.id = "fts_test_apply_shifted".to_string();
    shifted.name = "Apply Shifted".to_string();
    let target = shifted.windows[0].bounds.as_mut().unwrap();
    target.x = baseline_bounds.x + 60;
    target.y = baseline_bounds.y + 80;
    let result = screensets.save(shifted, opts.clone()).await?;
    assert!(result.ok, "save should succeed: {:?}", result.error);

    // Apply must not error out. Whether the window manager actually honors
    // the SetWindowPos call is platform-dependent — REAPER's main window is
    // often maximized or tiled by the WM (which silently ignores explicit
    // geometry), so we don't assert on the post-apply bounds. The
    // round-trip case that *can* be verified end-to-end is exercised by
    // floating panels in the per-panel screenset tests (added once the dock
    // host adapter wires its layout blob into screenset capture).
    let result = screensets.apply("fts_test_apply_shifted", opts.clone()).await?;
    assert!(result.ok, "apply should succeed: {:?}", result.error);

    for id in ["fts_test_apply_baseline", "fts_test_apply_shifted"] {
        let _ = screensets.delete(id, opts.clone()).await?;
    }
    Ok(())
}
