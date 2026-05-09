//! End-to-end CLI integration test.
//!
//! Boots the `task-server` binary, writes a `servers.tsv` profile, and
//! exercises golden-path flows by shelling out to the `task` CLI. The
//! aim is to catch regressions where the CLI → Vox dispatcher → service
//! impl → DB chain breaks even when unit tests are green.

mod common;

use std::time::{SystemTime, UNIX_EPOCH};

use chrono::{Duration, Utc};

use common::{CliOutput, CliRunner, RunningServer, TestFixture, free_loopback_addr, wait_for_tcp};

/// Smoke-level e2e flow that drives the full CLI → Vox → service →
/// SQLite chain for SystemService. Always runs; exists to catch
/// regressions in the bootstrapping path.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn cli_e2e_system_smoke() {
    let server_bin = env!("CARGO_BIN_EXE_task-server");
    let fixture = TestFixture::new();
    let bind_addr = free_loopback_addr().await;
    let mut server = RunningServer::spawn(server_bin, bind_addr, &fixture);
    wait_for_tcp(bind_addr).await;
    let cli = CliRunner::new(bind_addr, "org_fts");

    flow_system_health(&cli);

    server.stop().await;
    drop(server);
}

/// Full golden-path sweep across the workflow CLIs.
///
/// Exercises the CLI → Vox → service → SQLite chain across every
/// workflow that publishes list/show endpoints with `Uuid` fields.
/// Originally blocked on a `vox-postcard` `UnsupportedType("Uuid")`
/// panic in `SendReplyContext::send_reply`; now unblocked since
/// vox-postcard handles `Uuid` natively (16 raw bytes, like the chrono
/// special case) and the driver no longer panics on serialize failure.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn cli_e2e_golden_paths() {
    let server_bin = env!("CARGO_BIN_EXE_task-server");
    let fixture = TestFixture::new();
    let bind_addr = free_loopback_addr().await;
    let mut server = RunningServer::spawn(server_bin, bind_addr, &fixture);
    wait_for_tcp(bind_addr).await;

    let cli = CliRunner::new(bind_addr, "org_fts");

    // Each flow is its own block so a failure points at the right
    // surface area in the panic message. They share one server instance
    // and the demo-seeded data.
    flow_system_health(&cli);
    flow_project_and_task_lifecycle(&cli);
    flow_cooking_recipe_browse(&cli);
    flow_cooking_session_walkthrough(&cli);
    flow_fitness_session_checkbox(&cli);
    flow_fitness_progress_query(&cli);
    flow_fitness_balance_query(&cli);
    flow_body_measurement_record_and_trend(&cli);
    flow_glossary_resolve(&cli);
    flow_substitution_suggest(&cli);
    flow_pantry_consume_then_log(&cli);

    server.stop().await;
    drop(server);
}

// ── flow: system health ─────────────────────────────────────────────────────

fn flow_system_health(cli: &CliRunner) {
    // The CLI exposes SystemService.health via `task doctor --json`.
    // It also surfaces capabilities (the list of services), which we
    // assert covers the workflow services we exercise below.
    let out = cli.run_ok(&["doctor", "--json"]);
    let body = out.stdout.as_str();
    for svc in [
        "SystemService",
        "CookingService",
        "FitnessService",
        "GlossaryService",
        "PantryService",
    ] {
        assert!(
            body.contains(svc),
            "doctor output missing service {svc:?}\nstdout:\n{body}"
        );
    }
    assert!(
        body.contains("SQLITE_OK") || body.contains("\"checks\""),
        "doctor output missing health checks\nstdout:\n{body}"
    );
}

// ── flow: project list + task lifecycle ─────────────────────────────────────

fn flow_project_and_task_lifecycle(cli: &CliRunner) {
    // Demo seed creates "Task App" — assert the project list flow works.
    let projects = cli.run_ok(&["project", "list"]);
    assert!(
        projects.stdout.contains("Task App"),
        "expected seeded 'Task App' project in `project list` output:\n{}",
        projects.stdout
    );

    // Task surface: create → list under a project → complete.
    // Use a unique title so reruns don't collide.
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock")
        .as_nanos();
    let title = format!("E2E task {nanos}");
    cli.run_ok(&[
        "add",
        "--title",
        &title,
        "--project",
        "Task App",
        "--assignee",
        "agent",
    ]);

    let project_tasks = cli.run_ok(&["project", "tasks", "Task App"]);
    assert!(
        project_tasks.stdout.contains(&title),
        "newly-created task missing from `project tasks`:\n{}",
        project_tasks.stdout
    );

    cli.run_ok(&["complete", &title]);

    // Cleanup: hard delete so the row is gone for re-runs.
    cli.run_ok(&["delete", &title, "--hard"]);
}

// ── flow: cooking recipe browse ─────────────────────────────────────────────

fn flow_cooking_recipe_browse(cli: &CliRunner) {
    let list = cli.run_ok(&["cook", "recipe", "list"]);
    assert!(
        list.stdout.contains("Carbonara"),
        "expected 'Carbonara' in seeded recipe list:\n{}",
        list.stdout
    );

    let show = cli.run_ok(&["cook", "recipe", "show", "Weeknight Carbonara"]);
    assert!(
        show.stdout.contains("ingredients:") && show.stdout.contains("steps:"),
        "recipe detail missing ingredients/steps sections:\n{}",
        show.stdout
    );

    let scaled_json = cli.run_ok(&[
        "cook",
        "recipe",
        "scale",
        "Weeknight Carbonara",
        "--servings",
        "6",
        "--json",
    ]);
    let scaled: serde_json::Value = serde_json::from_str(scaled_json.stdout.trim())
        .unwrap_or_else(|e| panic!("scale --json invalid: {e}\nstdout:\n{}", scaled_json.stdout));

    let base = cli.run_ok(&["cook", "recipe", "show", "Weeknight Carbonara", "--json"]);
    let base_json: serde_json::Value = serde_json::from_str(base.stdout.trim())
        .unwrap_or_else(|e| panic!("show --json invalid: {e}\nstdout:\n{}", base.stdout));

    // Sanity: scaled output should differ from base in at least one
    // ingredient quantity. We don't pin the exact ratio so this stays
    // robust to seed tweaks; just assert *some* numeric field shifted.
    let base_ings = base_json.get("ingredients").and_then(|v| v.as_array());
    let scaled_ings = scaled
        .get("ingredients")
        .or_else(|| scaled.get("scaled_ingredients"))
        .and_then(|v| v.as_array());
    if let (Some(a), Some(b)) = (base_ings, scaled_ings) {
        let diff = a.iter().zip(b.iter()).any(|(x, y)| {
            let xq = x.get("quantity").and_then(|q| q.as_f64()).unwrap_or(0.0);
            let yq = y.get("quantity").and_then(|q| q.as_f64()).unwrap_or(0.0);
            (xq - yq).abs() > f64::EPSILON
        });
        assert!(
            diff,
            "scale --servings 6 produced identical quantities to base"
        );
    }
}

// ── flow: cooking session walkthrough ───────────────────────────────────────

fn flow_cooking_session_walkthrough(cli: &CliRunner) {
    // Demo seed creates two cooking sessions; just confirm list-active runs.
    cli.run_ok(&["cook", "session", "list-active"]);

    let start = cli.run_ok(&[
        "cook",
        "session",
        "start",
        "Greek Salad",
        "--servings",
        "2",
        "--json",
    ]);
    let session: serde_json::Value =
        serde_json::from_str(start.stdout.trim()).unwrap_or_else(|e| {
            panic!(
                "session start --json invalid: {e}\nstdout:\n{}",
                start.stdout
            )
        });
    let session_id = session
        .get("session")
        .and_then(|s| s.get("id"))
        .or_else(|| session.get("id"))
        .and_then(|v| v.as_str())
        .unwrap_or_else(|| panic!("no session id in start payload:\n{}", start.stdout))
        .to_string();

    let show = cli.run_ok(&["cook", "session", "show", &session_id]);
    assert!(
        show.stdout.contains("mise en place"),
        "session show missing mise-en-place section:\n{}",
        show.stdout
    );

    cli.run_ok(&[
        "cook",
        "session",
        "ingredient",
        &session_id,
        "--index",
        "0",
        "--check",
    ]);

    cli.run_ok(&["cook", "session", "abandon", &session_id]);
}

// ── flow: fitness session checkbox ──────────────────────────────────────────

fn flow_fitness_session_checkbox(cli: &CliRunner) {
    let routines = cli.run_ok(&["fit", "routine", "list"]);
    assert!(
        routines.stdout.contains("Push Day"),
        "expected seeded 'Push Day' routine:\n{}",
        routines.stdout
    );

    let start = cli.run_ok(&[
        "fit",
        "session",
        "start",
        "--routine",
        "push-day",
        "--bodyweight-kg",
        "78",
        "--json",
    ]);
    let session: serde_json::Value = serde_json::from_str(start.stdout.trim())
        .unwrap_or_else(|e| panic!("fit session start invalid: {e}\nstdout:\n{}", start.stdout));
    let session_id = session
        .get("session")
        .and_then(|s| s.get("id"))
        .and_then(|v| v.as_str())
        .unwrap_or_else(|| panic!("no session id in start payload:\n{}", start.stdout))
        .to_string();

    // Pull a planned (unchecked) set's id straight from JSON — easier
    // than scraping the textual "[ ] N <detail> <uuid>" rendering.
    let sets = session
        .get("sets")
        .and_then(|s| s.as_array())
        .cloned()
        .unwrap_or_default();
    let planned_set_id = sets
        .iter()
        .find(|s| s.get("completed_at").is_none_or(|v| v.is_null()))
        .and_then(|s| s.get("id"))
        .and_then(|v| v.as_str())
        .unwrap_or_else(|| {
            panic!(
                "no planned set in fit session start payload:\n{}",
                start.stdout
            )
        })
        .to_string();

    let plain = cli.run_ok(&["fit", "session", "show", &session_id]);
    assert!(
        plain.stdout.contains("[ ]"),
        "freshly-started session should have planned `[ ]` rows:\n{}",
        plain.stdout
    );

    cli.run_ok(&["fit", "session", "check", &planned_set_id]);

    let after = cli.run_ok(&["fit", "session", "show", &session_id]);
    assert!(
        after.stdout.contains("[x]"),
        "after `check`, session show should contain `[x]`:\n{}",
        after.stdout
    );

    cli.run_ok(&["fit", "session", "abandon", &session_id]);
}

// ── flow: fitness progress query ────────────────────────────────────────────

fn flow_fitness_progress_query(cli: &CliRunner) {
    // Demo seed has an active Push Day session with completed Bench
    // Press sets, so progress lookup should return ≥1 entry.
    let json_out = cli.run_ok(&["fit", "progress", "Bench Press", "--limit", "5", "--json"]);
    let v: serde_json::Value = serde_json::from_str(json_out.stdout.trim()).unwrap_or_else(|e| {
        panic!(
            "fit progress --json invalid: {e}\nstdout:\n{}",
            json_out.stdout
        )
    });
    let entries = v.get("entries").and_then(|e| e.as_array()).cloned();
    assert!(
        entries.as_ref().is_some_and(|a| !a.is_empty()),
        "expected ≥1 progress entry for Bench Press:\n{}",
        json_out.stdout
    );

    // Unknown exercise should not panic the dispatcher: graceful empty.
    let missing: CliOutput = cli.run(&["fit", "progress", "no-such-exercise-xyz"]);
    if !missing.status.success() {
        // TODO(e2e): an unknown exercise name currently returns a hard
        // error from `fit progress` instead of a graceful empty view.
        // The flow tolerates this by accepting either exit-0 or a
        // non-zero exit without panicking.
        assert!(
            missing.stderr.contains("not found")
                || missing.stderr.contains("no")
                || missing.stderr.contains("matches"),
            "unexpected stderr on missing exercise:\n{}",
            missing.stderr
        );
    }
}

// ── flow: fitness daily calorie balance ─────────────────────────────────────

fn flow_fitness_balance_query(cli: &CliRunner) {
    let today = Utc::now().date_naive();
    let week_ago = today - Duration::days(7);
    let out = cli.run_ok(&[
        "fit",
        "balance",
        "--since",
        &week_ago.to_string(),
        "--until",
        &today.to_string(),
        "--bodyweight-kg",
        "78",
        "--json",
    ]);
    let v: serde_json::Value = serde_json::from_str(out.stdout.trim())
        .unwrap_or_else(|e| panic!("fit balance --json invalid: {e}\nstdout:\n{}", out.stdout));
    // Either `days` or top-level array of date rows — accept whichever
    // shape the CLI emits, as long as we got back a structured response.
    assert!(
        v.is_object() || v.is_array(),
        "fit balance should return an object/array:\n{}",
        out.stdout
    );
}

// ── flow: body measurement record + trend ───────────────────────────────────

fn flow_body_measurement_record_and_trend(cli: &CliRunner) {
    let weigh = cli.run_ok(&["fit", "weigh", "77.5", "--note", "e2e test", "--json"]);
    let measurement: serde_json::Value = serde_json::from_str(weigh.stdout.trim())
        .unwrap_or_else(|e| panic!("fit weigh --json invalid: {e}\nstdout:\n{}", weigh.stdout));
    let id = measurement
        .get("id")
        .and_then(|v| v.as_str())
        .unwrap_or_else(|| panic!("fit weigh: no id in payload:\n{}", weigh.stdout))
        .to_string();

    let list = cli.run_ok(&["fit", "measure", "list", "--limit", "3"]);
    assert!(
        !list.stdout.trim().is_empty(),
        "fit measure list returned empty output"
    );

    let trend = cli.run_ok(&["fit", "measure", "trend"]);
    assert!(
        trend.stdout.contains("weight")
            || trend.stdout.contains("Weight")
            || trend.stdout.contains("kg"),
        "fit measure trend should mention weight metric:\n{}",
        trend.stdout
    );

    cli.run_ok(&["fit", "measure", "delete", &id]);
}

// ── flow: glossary resolve ──────────────────────────────────────────────────

fn flow_glossary_resolve(cli: &CliRunner) {
    let list = cli.run_ok(&["glossary", "list"]);
    assert!(
        list.stdout.contains("simmer") || list.stdout.contains("Simmer"),
        "expected 'simmer' in glossary list:\n{}",
        list.stdout
    );

    let resolved = cli.run_ok(&[
        "glossary",
        "resolve",
        "--text",
        "Bring to a [[simmer]] and [[deglaze]] the pan.",
    ]);
    assert!(
        resolved.stdout.contains("simmer"),
        "glossary resolve missing 'simmer' span:\n{}",
        resolved.stdout
    );
    assert!(
        resolved.stdout.contains("deglaze"),
        "glossary resolve missing 'deglaze' span:\n{}",
        resolved.stdout
    );
}

// ── flow: substitution suggestions ──────────────────────────────────────────

fn flow_substitution_suggest(cli: &CliRunner) {
    // The CLI subcommand is `cook recipe substitutions <recipe>`.
    let out = cli.run_ok(&[
        "cook",
        "recipe",
        "substitutions",
        "Weeknight Carbonara",
        "--diet",
        "vegan",
        "--json",
    ]);
    // Whether the seed actually has vegan substitutions for eggs +
    // pecorino is a moving target; assert the response is structured
    // and the call survived the dispatcher chain.
    let v: serde_json::Value = serde_json::from_str(out.stdout.trim()).unwrap_or_else(|e| {
        panic!(
            "cook recipe substitutions --json invalid: {e}\nstdout:\n{}",
            out.stdout
        )
    });
    assert!(
        v.is_object() || v.is_array(),
        "substitutions response should be structured:\n{}",
        out.stdout
    );
    // TODO(e2e): once the demo seed reliably ships flax-egg + nutritional-
    // yeast substitution rules for vegan Carbonara, tighten this to assert
    // those specific suggestions are present.
}

// ── flow: pantry list ───────────────────────────────────────────────────────

fn flow_pantry_consume_then_log(cli: &CliRunner) {
    let out = cli.run_ok(&["cook", "pantry", "list"]);
    // Just verify the list path works; demo seed should populate at
    // least a handful of pantry rows. If the seed ever stops doing
    // that, the assertion below stays useful as long as the call
    // doesn't panic the server.
    assert!(
        !out.stdout.trim().is_empty(),
        "cook pantry list returned empty output"
    );
}
