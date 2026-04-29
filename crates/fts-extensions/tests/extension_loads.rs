//! Integration tests for FTS Extensions.
//!
//! These tests run against a live REAPER instance via the daw test harness.
//! The FTS Extensions plugin must be installed in REAPER's UserPlugins.

use reaper_test::{ReaperTestContext, reaper_test};
use std::time::Duration;

/// Wait for FTS Extensions to finish registering actions.
/// The extension registers asynchronously after REAPER starts (~1-2s).
async fn wait_for_ready(ctx: &ReaperTestContext) -> eyre::Result<()> {
    let actions = ctx.daw.action_registry();

    for i in 0..30 {
        // Probe a well-known action as a readiness check
        if let Ok(Some(_id)) = actions.lookup_command_id("FTS_LAUNCHER_TOGGLE").await {
            ctx.log(&format!("Actions ready after {}ms", i * 500));
            return Ok(());
        }
        tokio::time::sleep(Duration::from_millis(500)).await;
    }
    eyre::bail!("FTS actions not registered after 15s — is the extension loaded?");
}

/// Wait specifically for an input action to appear in the host registry.
async fn wait_for_input_action(
    ctx: &ReaperTestContext,
    command_name: &str,
) -> eyre::Result<u32> {
    let actions = ctx.daw.action_registry();

    for i in 0..30 {
        if let Ok(Some(id)) = actions.lookup_command_id(command_name).await {
            ctx.log(&format!(
                "{command_name} registered after {}ms (cmd_id={id})",
                i * 500
            ));
            return Ok(id);
        }
        tokio::time::sleep(Duration::from_millis(500)).await;
    }

    eyre::bail!("{command_name} was not registered after 15s");
}

/// Wait for an input action to appear in REAPER's main action list.
async fn wait_for_input_action_list(
    ctx: &ReaperTestContext,
    command_name: &str,
) -> eyre::Result<()> {
    let actions = ctx.daw.action_registry();

    for i in 0..30 {
        if actions
            .is_in_action_list(command_name)
            .await
            .unwrap_or(false)
        {
            ctx.log(&format!(
                "{command_name} appeared in REAPER's action list after {}ms",
                i * 500
            ));
            return Ok(());
        }
        tokio::time::sleep(Duration::from_millis(500)).await;
    }

    eyre::bail!("{command_name} did not appear in REAPER's action list after 15s");
}

/// Verify the extension loaded and at least one action is registered.
#[reaper_test]
async fn extension_loaded(ctx: &ReaperTestContext) -> eyre::Result<()> {
    wait_for_ready(ctx).await?;
    ctx.log("FTS Extensions loaded and actions registered");
    Ok(())
}

/// Verify the FTS-Input passthrough toggle is registered as an action in the host.
#[reaper_test]
async fn input_toggle_passthrough_is_registered(ctx: &ReaperTestContext) -> eyre::Result<()> {
    wait_for_ready(ctx).await?;
    let actions = ctx.daw.action_registry();

    let cmd_id = wait_for_input_action(ctx, "FTS_INPUT_TOGGLE_PASSTHROUGH").await?;

    let registered = actions.is_registered("FTS_INPUT_TOGGLE_PASSTHROUGH").await?;
    assert!(registered, "FTS_INPUT_TOGGLE_PASSTHROUGH should be registered");

    wait_for_input_action_list(ctx, "FTS_INPUT_TOGGLE_PASSTHROUGH").await?;

    ctx.log(&format!(
        "FTS_INPUT_TOGGLE_PASSTHROUGH registered with cmd_id={cmd_id}"
    ));
    Ok(())
}

/// Verify the legacy test toggle is also present in REAPER's action list.
///
/// This is a control case for comparing the legacy action registration path
/// with the module-based input registration path.
#[reaper_test]
async fn legacy_test_toggle_is_in_action_list(ctx: &ReaperTestContext) -> eyre::Result<()> {
    wait_for_ready(ctx).await?;
    let actions = ctx.daw.action_registry();

    let cmd_id = wait_for_input_action(ctx, "FTS_TEST_TOGGLE").await?;
    let in_list = actions.is_in_action_list("FTS_TEST_TOGGLE").await?;
    assert!(in_list, "FTS_TEST_TOGGLE should appear in REAPER's action list");

    ctx.log(&format!("FTS_TEST_TOGGLE registered with cmd_id={cmd_id}"));
    Ok(())
}

/// Verify another input-module toggle action is present in REAPER's action list.
///
/// This distinguishes a single broken action from a broader module-registration issue.
#[reaper_test]
async fn input_profile_selector_is_in_action_list(ctx: &ReaperTestContext) -> eyre::Result<()> {
    wait_for_ready(ctx).await?;
    let actions = ctx.daw.action_registry();

    let cmd_id = wait_for_input_action(ctx, "FTS_INPUT_PROFILE_SELECTOR").await?;
    let in_list = actions.is_in_action_list("FTS_INPUT_PROFILE_SELECTOR").await?;
    assert!(
        in_list,
        "FTS_INPUT_PROFILE_SELECTOR should appear in REAPER's action list"
    );

    ctx.log(&format!(
        "FTS_INPUT_PROFILE_SELECTOR registered with cmd_id={cmd_id}"
    ));
    Ok(())
}

/// Verify the input actions panel toggle is also present in REAPER's action list.
#[reaper_test]
async fn input_actions_panel_is_in_action_list(ctx: &ReaperTestContext) -> eyre::Result<()> {
    wait_for_ready(ctx).await?;
    let actions = ctx.daw.action_registry();

    let cmd_id = wait_for_input_action(ctx, "FTS_INPUT_TOGGLE_ACTIONS_PANEL").await?;
    let in_list = actions.is_in_action_list("FTS_INPUT_TOGGLE_ACTIONS_PANEL").await?;
    assert!(
        in_list,
        "FTS_INPUT_TOGGLE_ACTIONS_PANEL should appear in REAPER's action list"
    );

    ctx.log(&format!(
        "FTS_INPUT_TOGGLE_ACTIONS_PANEL registered with cmd_id={cmd_id}"
    ));
    Ok(())
}

/// Verify all expected static actions are registered across all modules.
///
/// This list is the single source of truth for what fts-extensions should
/// register. If a module adds or removes actions, update this test.
#[reaper_test]
async fn all_actions_registered(ctx: &ReaperTestContext) -> eyre::Result<()> {
    wait_for_ready(ctx).await?;
    let actions = ctx.daw.action_registry();

    // ── Legacy actions (fts-extensions/src/actions.rs) ──
    let legacy = [
        "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE",
        "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_CONSTRAINED",
        "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_FULLY_CONSTRAINED",
        "FTS_TEMPO_MOVE_GRID_TO_MOUSE",
        "FTS_TEMPO_MOVE_MARKER_TO_MOUSE",
        "FTS_TEMPO_SNAP_GRID_TO_TRANSIENT",
        "FTS_TEMPO_SNAP_GRID_TO_TRANSIENT_CONSTRAINED",
        "FTS_TEMPO_SNAP_GRID_TO_TRANSIENT_FULLY_CONSTRAINED",
        "FTS_SPLIT_ITEMS_CROSSFADE_LEFT",
        "FTS_TEST_TOGGLE",
        "FTS_INFO",
    ];

    // ── Launcher module ──
    let launcher = [
        "FTS_LAUNCHER_TOGGLE",
    ];

    // ── Session module (prefix: fts.session) ──
    let session = [
        "FTS_SESSION_TOGGLE_PLAYBACK",
        "FTS_SESSION_TOGGLE_SONG_LOOP",
        "FTS_SESSION_SMART_NEXT",
        "FTS_SESSION_SMART_PREVIOUS",
        "FTS_SESSION_NEXT_SONG",
        "FTS_SESSION_PREVIOUS_SONG",
        "FTS_SESSION_NEXT_SECTION",
        "FTS_SESSION_PREVIOUS_SECTION",
        "FTS_SESSION_LOG_HELLO",
        "FTS_SESSION_LOG_STATUS",
        "FTS_SESSION_BUILD_SETLIST",
        "FTS_SESSION_LOAD_DEMO_SETLIST",
    ];

    // ── Sync module (prefix: fts.sync) ──
    let sync = [
        "FTS_SYNC_TOGGLE_LINK",
        "FTS_SYNC_LINK_PUPPET",
        "FTS_SYNC_LINK_MASTER",
        "FTS_SYNC_LINK_OFF",
        "FTS_SYNC_TOGGLE_SYNC",
        "FTS_SYNC_SETLIST_TOGGLE",
    ];

    // ── Dynamic Template module (prefix: fts.dynamic_template) ──
    let dynamic_template = [
        "FTS_DYNAMIC_TEMPLATE_SORT_SELECTED",
        "FTS_DYNAMIC_TEMPLATE_SORT_ALL",
        "FTS_DYNAMIC_TEMPLATE_IMPORT_AND_SORT",
        "FTS_DYNAMIC_TEMPLATE_ORGANIZE_DEMO",
        "FTS_DYNAMIC_TEMPLATE_LOG_STATUS",
        "FTS_DYNAMIC_TEMPLATE_LOG_GROUPS",
    ];

    // ── Auto Color module (prefix: fts.auto_color) ──
    let auto_color = [
        "FTS_AUTO_COLOR_COLOR_ALL",
        "FTS_AUTO_COLOR_COLOR_SELECTED",
        "FTS_AUTO_COLOR_TOGGLE",
        "FTS_AUTO_COLOR_CLEAR_ALL",
        "FTS_AUTO_COLOR_CLEAR_SELECTED",
    ];

    // ── Visibility Manager module (prefix: fts.visibility_manager) ──
    let visibility = [
        "FTS_VISIBILITY_MANAGER_TOGGLE_DRUMS",
        "FTS_VISIBILITY_MANAGER_TOGGLE_PERCUSSION",
        "FTS_VISIBILITY_MANAGER_TOGGLE_BASS",
        "FTS_VISIBILITY_MANAGER_TOGGLE_GUITARS",
        "FTS_VISIBILITY_MANAGER_TOGGLE_KEYS",
        "FTS_VISIBILITY_MANAGER_TOGGLE_SYNTHS",
        "FTS_VISIBILITY_MANAGER_TOGGLE_HORNS",
        "FTS_VISIBILITY_MANAGER_TOGGLE_HARMONICA",
        "FTS_VISIBILITY_MANAGER_TOGGLE_VOCALS",
        "FTS_VISIBILITY_MANAGER_TOGGLE_CHOIR",
        "FTS_VISIBILITY_MANAGER_TOGGLE_ORCHESTRA",
        "FTS_VISIBILITY_MANAGER_TOGGLE_SFX",
        "FTS_VISIBILITY_MANAGER_TOGGLE_GUIDE",
        "FTS_VISIBILITY_MANAGER_TOGGLE_REFERENCE",
        "FTS_VISIBILITY_MANAGER_SHOW_ALL",
        "FTS_VISIBILITY_MANAGER_HIDE_ALL",
        "FTS_VISIBILITY_MANAGER_REBUILD_CACHE",
    ];

    // ── Input module (static actions only; dynamic presets/workflows depend on config) ──
    let input = [
        "FTS_INPUT_TOGGLE",
        "FTS_INPUT_TOGGLE_PASSTHROUGH",
        "FTS_INPUT_TOGGLE_DEBUG_LOGGING",
        "FTS_INPUT_DEBUG_MOUSE_CONTEXT",
        "FTS_INPUT_RESET_ALL_OVERRIDES",
        "FTS_INPUT_MOUSE_RESET_TO_PROFILE",
        "FTS_INPUT_PROFILE_SELECTOR",
        "FTS_INPUT_WORKFLOW_SELECTOR",
        "FTS_INPUT_TOGGLE_ACTIONS_PANEL",
        "FTS_INPUT_TOGGLE_KEYBOARD_PANEL",
        "FTS_INPUT_TOGGLE_STATUS_PANEL",
        "FTS_INPUT_DEV_TEST_MOUSE_MODIFIER_IDS",
        "FTS_INPUT_DEV_RESET_ITEM_CLICK_MODIFIERS",
        // FTS_INPUT_WORKFLOW_DEACTIVATE is dynamic — only registered when workflows are loaded
    ];

    // Collect all expected actions
    let all_expected: Vec<&str> = [
        &legacy[..],
        &launcher[..],
        &session[..],
        &sync[..],
        &dynamic_template[..],
        &auto_color[..],
        &visibility[..],
        &input[..],
    ]
    .concat();

    let mut missing = Vec::new();
    let mut found = 0usize;

    for name in &all_expected {
        match actions.lookup_command_id(name).await {
            Ok(Some(id)) => {
                ctx.log(&format!("  {name}: OK (cmd_id={id})"));
                found += 1;
            }
            _ => {
                ctx.log(&format!("  {name}: MISSING"));
                missing.push(*name);
            }
        }
    }

    ctx.log(&format!(
        "\n  {found}/{} actions registered",
        all_expected.len()
    ));

    assert!(
        missing.is_empty(),
        "Missing {} actions: {:?}",
        missing.len(),
        missing
    );
    Ok(())
}
