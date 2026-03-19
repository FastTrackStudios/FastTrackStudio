//! REAPER integration test for end-to-end action triggering.
//!
//! Verifies that when a sync-domain action is triggered in REAPER,
//! the sync-extension guest receives the event and executes the handler.
//!
//! Flow:
//! 1. sync-extension registers actions (e.g. FTS_SYNC_TOGGLE_LINK)
//! 2. Test subscribes to action events AND triggers the action via run_command
//! 3. sync-extension receives the trigger and writes to ExtState
//! 4. Test polls ExtState to confirm the handler executed
//!
//! Run with:
//!   cargo xtask reaper-test -- sync_action_trigger

use std::time::Duration;

use reaper_test::reaper_test;
use sync::actions::sync_actions;

/// Wait for the sync-extension to be ready.
async fn wait_for_sync_extension(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let ext_state = ctx.daw.ext_state();
    for _ in 0..100 {
        let status = ext_state.get("FTS_SYNC_EXT", "status").await?;
        if status.as_deref() == Some("ready") {
            return Ok(());
        }
        tokio::time::sleep(Duration::from_millis(100)).await;
    }
    eyre::bail!("sync-extension did not become ready within 10s");
}

/// Trigger a sync action in REAPER and verify the guest extension handled it.
///
/// This tests the full round-trip:
///   test → run_command → REAPER action handler → broadcast →
///   daw-bridge SHM → sync-extension → ExtState write → test reads
#[reaper_test(isolated)]
async fn sync_action_trigger_round_trip(ctx: &ReaperTestContext) -> eyre::Result<()> {
    wait_for_sync_extension(ctx).await?;
    println!("sync-extension is ready");

    // Clear any stale last_action from a previous run
    ctx.daw
        .ext_state()
        .delete("FTS_SYNC_EXT", "last_action", false)
        .await?;

    // Pick the first sync action to trigger
    let defs = sync_actions::definitions();
    let def = &defs[0];
    let cmd_name = def.id.to_command_id();
    println!("Triggering action: {cmd_name}");

    // Verify the action is registered
    let registered = ctx.daw.action_registry().is_registered(&cmd_name).await?;
    assert!(
        registered,
        "Action {cmd_name} should be registered by sync-extension"
    );

    // Trigger the action via REAPER's command system.
    // Custom actions are looked up with underscore prefix.
    let project = ctx.project();
    let triggered = project.run_command(&format!("_{cmd_name}")).await?;
    assert!(
        triggered,
        "run_command should return true for registered action"
    );
    println!("Action triggered in REAPER, waiting for handler...");

    // Poll ExtState for the sync-extension to write last_action.
    // The round-trip goes: REAPER main thread → broadcast → SHM → guest → ExtState RPC
    let ext = ctx.daw.ext_state();
    let mut last_action = None;
    for i in 0..50 {
        last_action = ext.get("FTS_SYNC_EXT", "last_action").await?;
        if last_action.is_some() {
            break;
        }
        if i < 49 {
            tokio::time::sleep(Duration::from_millis(100)).await;
        }
    }

    let last_action =
        last_action.expect("sync-extension should have written last_action to ExtState");
    assert_eq!(
        last_action, cmd_name,
        "last_action should match the triggered command"
    );
    println!("Handler confirmed: last_action={last_action}");

    // Clean up
    ext.delete("FTS_SYNC_EXT", "last_action", false).await?;

    Ok(())
}
