//! Seed every feature's CRDT with fake data via the architect-
//! emitted `seed_fake_<entity>` helpers, then compact into a
//! snapshot the sync-demo server can hydrate clients from.

use std::time::Duration;

use agent_proto::GitRepoConnectionRepo;
use crdt::{CrdtDoc, Persistence};
use crdt_seaorm::SeaOrmPersistence;
use tracing::info;
use uuid::Uuid;

/// Run the seed flow against an already-migrated persistence
/// handle. Idempotent in that re-runs append more rows rather than
/// overwriting — wipe the SQLite file by hand for a clean baseline.
pub async fn run(persistence: SeaOrmPersistence, workspace_doc_id: Uuid) -> eyre::Result<()> {
    info!(%workspace_doc_id, "seeding…");

    // Replay any existing state before seeding.
    let cdoc = CrdtDoc::open(workspace_doc_id, persistence.clone()).await?;

    seed_all(&cdoc).await?;

    // Give the fire-and-forget `subscribe_local_update` writes a
    // moment to land in sqlite before we compact. Real production
    // would await a flush handle; for the seeder a sleep is fine.
    tokio::time::sleep(Duration::from_millis(250)).await;

    // Compact: pulls a full snapshot, atomically replaces the
    // accumulated `crdt_update` rows with one `crdt_doc` row.
    cdoc.compact(workspace_doc_id).await?;

    let snapshot = persistence.load_snapshot(workspace_doc_id).await?;
    info!(
        bytes = snapshot.as_ref().map(|s| s.len()).unwrap_or(0),
        "compacted; sync-demo can now hydrate clients from this snapshot"
    );
    Ok(())
}

/// Hand the same `CrdtDoc` to every feature's RepoLoro newtype and
/// call its architect-emitted `seed_fake_<entity>` helper. Counts
/// are tuned for "feels like real content" UI design — a few dozen
/// rows per entity, fewer for higher-cardinality types where the
/// per-row UI is heavier.
async fn seed_all(cdoc: &CrdtDoc) -> eyre::Result<()> {
    info!("seeding…");

    // ── Cross-cutting + reference data first ──────────────────
    let person_repo = person_crdt::PersonRepoLoro::new(cdoc);
    let client_repo = person_crdt::ClientRepoLoro::new(cdoc);
    let team_repo = person_crdt::TeamRepoLoro::new(cdoc);
    person_proto::seed_fake_person(&person_repo, 25usize).await?;
    person_proto::seed_fake_client(&client_repo, 8usize).await?;
    person_proto::seed_fake_team(&team_repo, 4usize).await?;
    info!("  person: 25 people, 8 clients, 4 teams");

    let location_repo = location_crdt::LocationRepoLoro::new(cdoc);
    location_proto::seed_fake_location(&location_repo, 12usize).await?;
    info!("  location: 12 locations");

    // ── Project domain ────────────────────────────────────────
    let project_repo = project_crdt::ProjectRepoLoro::new(cdoc);
    let task_repo = project_crdt::TaskRepoLoro::new(cdoc);
    let cycle_repo = project_crdt::CycleRepoLoro::new(cdoc);
    let milestone_repo = project_crdt::MilestoneRepoLoro::new(cdoc);
    project_proto::seed_fake_project(&project_repo, 10usize).await?;
    project_proto::seed_fake_task(&task_repo, 80usize).await?;
    project_proto::seed_fake_cycle(&cycle_repo, 8usize).await?;
    project_proto::seed_fake_milestone(&milestone_repo, 15usize).await?;
    info!("  project: 10 projects, 80 tasks, 8 cycles, 15 milestones");

    // ── Threads / discussion ──────────────────────────────────
    let comment_repo = threads_crdt::CommentRepoLoro::new(cdoc);
    let reaction_repo = threads_crdt::ReactionRepoLoro::new(cdoc);
    let attachment_repo = threads_crdt::AttachmentRepoLoro::new(cdoc);
    threads_proto::seed_fake_comment(&comment_repo, 50usize).await?;
    threads_proto::seed_fake_reaction(&reaction_repo, 80usize).await?;
    threads_proto::seed_fake_attachment(&attachment_repo, 20usize).await?;
    info!("  threads: 50 comments, 80 reactions, 20 attachments");

    // ── Chat ──────────────────────────────────────────────────
    let channel_repo = chat_crdt::ChannelRepoLoro::new(cdoc);
    let message_repo = chat_crdt::MessageRepoLoro::new(cdoc);
    let member_repo = chat_crdt::ChannelMemberRepoLoro::new(cdoc);
    chat_proto::seed_fake_channel(&channel_repo, 8usize).await?;
    chat_proto::seed_fake_message(&message_repo, 200usize).await?;
    chat_proto::seed_fake_channel_member(&member_repo, 30usize).await?;
    info!("  chat: 8 channels, 200 messages, 30 members");

    // ── Time + invoicing ──────────────────────────────────────
    let time_repo = timer_crdt::TimeEntryRepoLoro::new(cdoc);
    timer_proto::seed_fake_time_entry(&time_repo, 100usize).await?;
    info!("  timer: 100 entries");

    let client_repo = invoice_crdt::ClientRepoLoro::new(cdoc);
    let invoice_repo = invoice_crdt::InvoiceRepoLoro::new(cdoc);
    let line_repo = invoice_crdt::InvoiceLineRepoLoro::new(cdoc);
    let payment_repo = invoice_crdt::PaymentRepoLoro::new(cdoc);
    invoice_proto::seed_fake_client(&client_repo, 5usize).await?;
    invoice_proto::seed_fake_invoice(&invoice_repo, 12usize).await?;
    invoice_proto::seed_fake_invoice_line(&line_repo, 40usize).await?;
    invoice_proto::seed_fake_payment(&payment_repo, 20usize).await?;
    info!("  invoice: 5 clients, 12 invoices, 40 lines, 20 payments");

    let revenue_repo = finance_crdt::RevenueRepoLoro::new(cdoc);
    let expense_repo = finance_crdt::ExpenseRepoLoro::new(cdoc);
    let asset_repo = finance_crdt::FinancialAssetRepoLoro::new(cdoc);
    finance_proto::seed_fake_revenue(&revenue_repo, 30usize).await?;
    finance_proto::seed_fake_expense(&expense_repo, 50usize).await?;
    finance_proto::seed_fake_financial_asset(&asset_repo, 10usize).await?;
    info!("  finance: 30 revenue, 50 expense, 10 financial assets");

    // ── Calendar + meetings ───────────────────────────────────
    let event_repo = calendar_crdt::CalendarEventRepoLoro::new(cdoc);
    calendar_proto::seed_fake_calendar_event(&event_repo, 40usize).await?;
    info!("  calendar: 40 events");

    let meeting_repo = conference_crdt::MeetingRepoLoro::new(cdoc);
    conference_proto::seed_fake_meeting(&meeting_repo, 10usize).await?;
    info!("  conference: 10 meetings");

    // ── Cookbook (recipes + pantry + shopping + products) ─────
    let cookbook_repo = cookbook_crdt::CookbookRepoLoro::new(cdoc);
    let recipe_repo = cookbook_crdt::RecipeRepoLoro::new(cdoc);
    let ingredient_repo = cookbook_crdt::RecipeIngredientRepoLoro::new(cdoc);
    let step_repo = cookbook_crdt::RecipeStepRepoLoro::new(cdoc);
    let meal_plan_repo = cookbook_crdt::MealPlanRepoLoro::new(cdoc);
    let product_repo = cookbook_crdt::FoodProductRepoLoro::new(cdoc);
    let pantry_repo = cookbook_crdt::PantryItemRepoLoro::new(cdoc);
    let shopping_repo = cookbook_crdt::ShoppingListItemRepoLoro::new(cdoc);
    cookbook_proto::seed_fake_cookbook(&cookbook_repo, 3usize).await?;
    cookbook_proto::seed_fake_recipe(&recipe_repo, 20usize).await?;
    cookbook_proto::seed_fake_recipe_ingredient(&ingredient_repo, 100usize).await?;
    cookbook_proto::seed_fake_recipe_step(&step_repo, 80usize).await?;
    cookbook_proto::seed_fake_meal_plan(&meal_plan_repo, 14usize).await?;
    cookbook_proto::seed_fake_food_product(&product_repo, 30usize).await?;
    cookbook_proto::seed_fake_pantry_item(&pantry_repo, 50usize).await?;
    cookbook_proto::seed_fake_shopping_list_item(&shopping_repo, 25usize).await?;
    info!(
        "  cookbook: 3 cookbooks, 20 recipes, 100 ingredients, 80 steps, 14 meal plans, 30 products, 50 pantry, 25 shopping"
    );

    // ── Inventory (gear / hardware / software cataloging) ─────
    let inventory_items = inventory_crdt::InventoryItemRepoLoro::new(cdoc);
    let checkout_events = inventory_crdt::CheckoutEventRepoLoro::new(cdoc);
    inventory_proto::seed_fake_inventory_item(&inventory_items, 18usize).await?;
    inventory_proto::seed_fake_checkout_event(&checkout_events, 6usize).await?;
    info!("  inventory: 18 items, 6 checkout events");

    // ── Fitness ───────────────────────────────────────────────
    let exercise_repo = fitness_crdt::ExerciseRepoLoro::new(cdoc);
    let routine_repo = fitness_crdt::RoutineRepoLoro::new(cdoc);
    let session_repo = fitness_crdt::WorkoutSessionRepoLoro::new(cdoc);
    let set_log_repo = fitness_crdt::SetLogRepoLoro::new(cdoc);
    let measurement_repo = fitness_crdt::BodyMeasurementRepoLoro::new(cdoc);
    fitness_proto::seed_fake_exercise(&exercise_repo, 40usize).await?;
    fitness_proto::seed_fake_routine(&routine_repo, 8usize).await?;
    fitness_proto::seed_fake_workout_session(&session_repo, 30usize).await?;
    fitness_proto::seed_fake_set_log(&set_log_repo, 200usize).await?;
    fitness_proto::seed_fake_body_measurement(&measurement_repo, 20usize).await?;
    info!("  fitness: 40 exercises, 8 routines, 30 sessions, 200 set logs, 20 measurements");

    // ── Comms ─────────────────────────────────────────────────
    let email_repo = email_crdt::EmailRepoLoro::new(cdoc);
    email_proto::seed_fake_email(&email_repo, 50usize).await?;
    info!("  email: 50 emails");

    let agent_repo = agent_crdt::AgentRunRepoLoro::new(cdoc);
    agent_proto::seed_fake_agent_run(&agent_repo, 25usize).await?;
    let agent_log_repo = agent_crdt::AgentLogLineRepoLoro::new(cdoc);
    // 6 log lines, tied loosely (run_id is faker-random) to existing
    // AgentRun rows. The UI joins by `run_id` so seeded lines won't
    // line up against real runs — fine for design-time mock data.
    agent_proto::seed_fake_agent_log_line(&agent_log_repo, 6usize).await?;
    let git_conn_repo = agent_crdt::GitRepoConnectionRepoLoro::new(cdoc);
    git_conn_repo
        .create(agent_proto::GitRepoConnectionCreate {
            provider: "github".into(),
            owner: "Codys-Wright".into(),
            repo: "Task".into(),
            default_branch: "main".into(),
            project_id: None,
            // v1 stub: sealing happens in the next phase; this is a
            // placeholder hash that the server will replace once the
            // sealed-box layer lands.
            webhook_secret_hash: "0".repeat(64),
            webhook_path: "gh-7f3a".into(),
            last_event_at: None,
        })
        .await?;
    git_conn_repo
        .create(agent_proto::GitRepoConnectionCreate {
            provider: "forgejo".into(),
            owner: "cody".into(),
            repo: "starcommand".into(),
            default_branch: "main".into(),
            project_id: None,
            webhook_secret_hash: "1".repeat(64),
            webhook_path: "fj-2b8d".into(),
            last_event_at: None,
        })
        .await?;
    info!("  agent: 25 runs, 6 log lines, 2 git repo connections");

    // ── CalDAV reference data ─────────────────────────────────
    let caldav_acct = caldav_crdt::CalDavAccountRepoLoro::new(cdoc);
    let caldav_cal = caldav_crdt::CalDavCalendarRepoLoro::new(cdoc);
    caldav_proto::seed_fake_cal_dav_account(&caldav_acct, 3usize).await?;
    caldav_proto::seed_fake_cal_dav_calendar(&caldav_cal, 6usize).await?;
    info!("  caldav: 3 accounts, 6 calendars");

    info!("seed complete");
    Ok(())
}
