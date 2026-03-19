//! REAPER integration test for sync action registration.
//!
//! Verifies that the sync-extension registers its actions with REAPER
//! via the ActionRegistryService RPC, and that they're discoverable
//! via `is_registered` / `lookup_command_id`.
//!
//! Action definitions come from `sync_proto::actions::sync_actions` —
//! the same source the extension uses — so the test can never drift.
//!
//! Run with:
//!   cargo xtask reaper-test -- sync_actions

use reaper_test::reaper_test;
use sync::actions::sync_actions;

/// Wait for the sync-extension to be ready before checking actions.
async fn wait_for_sync_extension(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let ext_state = ctx.daw.ext_state();
    for _ in 0..100 {
        let status = ext_state.get("FTS_SYNC_EXT", "status").await?;
        if status.as_deref() == Some("ready") {
            return Ok(());
        }
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
    eyre::bail!("sync-extension did not become ready within 10s");
}

/// Verify that all sync actions are registered with REAPER.
#[reaper_test]
async fn sync_actions_check(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    // Wait for sync-extension to connect and register its actions
    wait_for_sync_extension(ctx).await?;
    println!("sync-extension is ready, checking actions...\n");

    let registry = ctx.daw.action_registry();
    let definitions = sync_actions::definitions();

    let mut missing = Vec::new();
    let mut found = 0u32;

    for def in &definitions {
        let cmd_name = def.id.to_command_id();
        let registered = registry.is_registered(&cmd_name).await?;
        if registered {
            found += 1;
            let cmd_id = registry.lookup_command_id(&cmd_name).await?;
            assert!(
                cmd_id.is_some() && cmd_id.unwrap() > 0,
                "Action {} is registered but has no valid command ID",
                cmd_name
            );
            println!("  OK  {} (cmd_id={})", cmd_name, cmd_id.unwrap());
        } else {
            println!("  MISSING  {}", cmd_name);
            missing.push(cmd_name);
        }
    }

    println!("\n{found}/{} sync actions registered", definitions.len());

    assert!(
        missing.is_empty(),
        "Missing {} sync action(s):\n{}",
        missing.len(),
        missing
            .iter()
            .map(|id| format!("  - {id}"))
            .collect::<Vec<_>>()
            .join("\n")
    );

    Ok(())
}
