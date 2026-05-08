//! Integration tests for the Routing service (#24).
//!
//! Round-trip coverage:
//! - add send → list → assert presence
//! - add multiple sends → delete middle → assert other sends survive
//! - add receive on dest track via reverse direction
//!
//! Run with: `cargo xtask reaper-test -- reaper_routing`

use reaper_test::reaper_test;

#[reaper_test(isolated)]
async fn add_send_then_list(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let project = ctx.project().clone();
    let tracks = project.tracks();
    let src = tracks.add("Send Source", None).await?;
    let dst = tracks.add("Send Destination", None).await?;

    let before = src.sends().all().await?;
    assert_eq!(
        before.len(),
        0,
        "newly created track should have zero sends"
    );

    let _route = src.sends().add_to(dst.guid()).await?;

    let after = src.sends().all().await?;
    assert_eq!(after.len(), 1, "should have exactly one send after add_to");

    Ok(())
}

#[reaper_test(isolated)]
async fn delete_middle_send_preserves_others(
    ctx: &reaper_test::ReaperTestContext,
) -> eyre::Result<()> {
    let project = ctx.project().clone();
    let tracks = project.tracks();
    let src = tracks.add("Send Source", None).await?;
    let a = tracks.add("Dst A", None).await?;
    let b = tracks.add("Dst B", None).await?;
    let c = tracks.add("Dst C", None).await?;

    let _r0 = src.sends().add_to(a.guid()).await?;
    let r1 = src.sends().add_to(b.guid()).await?;
    let _r2 = src.sends().add_to(c.guid()).await?;

    assert_eq!(src.sends().all().await?.len(), 3, "three sends added");

    r1.remove().await?;

    let after = src.sends().all().await?;
    assert_eq!(
        after.len(),
        2,
        "deleting one send should leave the other two intact"
    );

    Ok(())
}

#[reaper_test(isolated)]
async fn send_appears_as_receive_on_destination(
    ctx: &reaper_test::ReaperTestContext,
) -> eyre::Result<()> {
    let project = ctx.project().clone();
    let tracks = project.tracks();
    let src = tracks.add("Source", None).await?;
    let dst = tracks.add("Dest", None).await?;

    let recv_before = dst.receives().all().await?;
    assert_eq!(
        recv_before.len(),
        0,
        "destination should have no receives initially"
    );

    src.sends().add_to(dst.guid()).await?;

    let recv_after = dst.receives().all().await?;
    assert_eq!(
        recv_after.len(),
        1,
        "destination should report a receive matching the source's send"
    );

    Ok(())
}
