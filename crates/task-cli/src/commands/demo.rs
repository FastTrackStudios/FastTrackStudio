//! `task demo` — info about the demo seed mode.
//!
//! Demo data is seeded server-side at boot when `TASK_SEED_DEMO=1`
//! (default on). It's idempotent — re-running the server reuses the
//! same deterministic UUIDs, so duplicates are impossible. There's no
//! CLI-driven seed path because the seeder runs in-process against
//! SeaORM and the CLI talks Vox-RPC over the wire.

use clap::Subcommand;

#[derive(Subcommand)]
pub(crate) enum DemoCommands {
    /// Explain how demo seeding works in this build.
    Status,
}

pub(crate) fn run(command: DemoCommands) {
    match command {
        DemoCommands::Status => {
            println!("Demo seed mode is server-driven.");
            println!();
            println!("Boot the server with TASK_SEED_DEMO=1 (default).");
            println!("It calls task_db::seed::seed_demo_data, which is idempotent.");
            println!();
            println!("Seeded entities (deterministic UUIDs derived from a stable namespace):");
            println!("  - 6 projects (Task App, Montreal Album, ...)");
            println!("  - 17 tasks (mix of priorities, statuses, due dates, timers)");
            println!("  - 4 calendar events (today, tomorrow, past, all-day)");
            println!("  - 4 people contacts");
            println!("  - 3 comments (one resolved thread)");
            println!();
            println!("To reset: stop the server, delete the DB file (or rerun against a fresh");
            println!("in-memory DB), restart with TASK_SEED_DEMO=1.");
        }
    }
}
