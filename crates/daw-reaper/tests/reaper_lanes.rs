//! Integration smoke tests for marker/region ruler-lane setters.
//!
//! Covers the previously-stubbed `MarkerService::set_marker_lane`
//! and `RegionService::set_region_lane`.
//!
//! REAPER's API quirk: `I_LANENUMBER` "can be set, but returned value
//! is read-only" — i.e. the getter returns the *displayed* lane index
//! (recomputed by REAPER from layout), not the user's set value. So
//! these tests only assert that the calls succeed and the marker /
//! region survives; they don't round-trip the lane number.
//!
//! Run with: `cargo xtask reaper-test lane_`

use reaper_test::reaper_test;

#[reaper_test(isolated)]
async fn lane_set_marker_lane_does_not_lose_marker(
    ctx: &reaper_test::ReaperTestContext,
) -> eyre::Result<()> {
    let project = ctx.project().clone();
    let markers = project.markers();

    let id = markers.add(2.5, "lane-test").await?;
    markers.set_lane(id, Some(3)).await?;

    let m = markers
        .get(id)
        .await?
        .ok_or_else(|| eyre::eyre!("marker disappeared after set_lane"))?;
    assert_eq!(m.id, Some(id), "marker id should survive set_lane");

    markers.set_lane(id, None).await?;
    let m2 = markers
        .get(id)
        .await?
        .ok_or_else(|| eyre::eyre!("marker disappeared after lane reset"))?;
    assert_eq!(m2.id, Some(id), "marker should survive lane=None reset");

    Ok(())
}

#[reaper_test(isolated)]
async fn lane_set_region_lane_does_not_lose_region(
    ctx: &reaper_test::ReaperTestContext,
) -> eyre::Result<()> {
    let project = ctx.project().clone();
    let regions = project.regions();

    let id = regions.add(1.0, 4.0, "region-lane-test").await?;
    regions.set_lane(id, Some(2)).await?;

    let r = regions
        .get(id)
        .await?
        .ok_or_else(|| eyre::eyre!("region disappeared after set_lane"))?;
    assert_eq!(r.id, Some(id), "region id should survive set_lane");

    Ok(())
}
