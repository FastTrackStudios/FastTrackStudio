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

// `lane_set_marker_lane_does_not_lose_marker` retired alongside the
// `MarkerService::set_marker_lane` surface — the architect::rpc port
// trimmed `Markers` to the canonical CRUD verbs. Lane placement will
// return on a sibling trait when there's a real use case driving it.

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
