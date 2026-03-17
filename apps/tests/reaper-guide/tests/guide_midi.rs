//! Integration test: guide MIDI generation via session::guide_gen.
//!
//! Runs inside a REAPER instance via the reaper-test framework.
//! Creates regions and guide tracks, then verifies fill_guide_midi works.
//!
//! Run: cargo test -p reaper-guide-test --test guide_midi -- --ignored --nocapture

use reaper_test::reaper_test;

#[reaper_test(isolated)]
async fn fill_guide_creates_midi_items(
    ctx: &reaper_test::ReaperTestContext,
) -> eyre::Result<()> {
    let project = ctx.project().clone();

    // Step 1: Create section regions
    ctx.log("Creating test regions...");
    project.regions().add(10.0, 30.0, "Intro").await?;
    project.regions().add(30.0, 60.0, "VS 1").await?;
    project.regions().add(60.0, 90.0, "CH 1").await?;

    let regions = project.regions().all().await?;
    ctx.log(&format!("Regions: {}", regions.len()));
    assert!(regions.len() >= 3);

    // Step 2: Create guide tracks
    ctx.log("Creating Click, Count, Guide tracks...");
    let click = project.tracks().add("Click", None).await?;
    let count = project.tracks().add("Count", None).await?;
    let guide = project.tracks().add("Guide", None).await?;

    // Verify empty
    assert_eq!(click.items().count().await?, 0);
    assert_eq!(count.items().count().await?, 0);
    assert_eq!(guide.items().count().await?, 0);

    // Step 3: Run fill_guide_midi
    ctx.log("Running fill_guide_midi...");
    let (generated, skipped) = session::guide_gen::fill_guide_midi()
        .await
        .map_err(|e| eyre::eyre!("fill_guide_midi failed: {e}"))?;

    ctx.log(&format!("Result: {} generated, {} skipped", generated, skipped));
    assert!(generated > 0, "Should generate for at least one region");

    // Step 4: Verify items created
    let click_count = click.items().count().await?;
    let count_count = count.items().count().await?;
    let guide_count = guide.items().count().await?;
    ctx.log(&format!(
        "Items: click={}, count={}, guide={}",
        click_count, count_count, guide_count
    ));

    assert!(click_count > 0, "Click should have items");
    assert!(count_count > 0, "Count should have items");
    assert!(guide_count > 0, "Guide should have items");

    // Step 5: Run again — should skip all (idempotent)
    ctx.log("Running fill_guide_midi again...");
    let (gen2, skip2) = session::guide_gen::fill_guide_midi()
        .await
        .map_err(|e| eyre::eyre!("second fill_guide_midi failed: {e}"))?;

    ctx.log(&format!("Second run: {} generated, {} skipped", gen2, skip2));
    assert_eq!(gen2, 0, "Second run should generate nothing");

    // Verify count unchanged
    assert_eq!(click.items().count().await?, click_count);

    ctx.log("PASSED");
    Ok(())
}

#[reaper_test(isolated)]
async fn fill_guide_errors_without_tracks(
    ctx: &reaper_test::ReaperTestContext,
) -> eyre::Result<()> {
    let project = ctx.project().clone();

    // Create regions but NO guide tracks
    project.regions().add(10.0, 30.0, "Verse").await?;

    let result = session::guide_gen::fill_guide_midi().await;
    assert!(result.is_err(), "Should fail without guide tracks");
    ctx.log(&format!("Expected error: {}", result.unwrap_err()));

    Ok(())
}
