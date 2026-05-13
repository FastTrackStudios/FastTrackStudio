//! Seed every feature's CRDT with fake data via the architect-
//! emitted `seed_fake_<entity>` helpers, then compact into a
//! snapshot the sync-demo server can hydrate clients from.

use std::time::Duration;

use agent_proto::{AgentConversationRepo, GitRepoConnectionRepo};
use chat_proto::MessageRepo;
use crdt::{CrdtDoc, Persistence};
use crdt_seaorm::SeaOrmPersistence;
use knowledge_proto::{
    BaseRepo as _, BlockRepo as _, FolderRepo as _, KnowledgeTagRepo as _, PageRepo as _,
    VaultRepo as _,
};
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

    // AI chat conversations — Phase A. ~3 mixed-model conversations
    // with ~15 messages (alternating user/assistant) tied to them.
    // One assistant message gets explicit reasoning text so the UI
    // can exercise the extended-thinking render path.
    let conv_repo = agent_crdt::AgentConversationRepoLoro::new(cdoc);
    let conv_models = ["mock", "claude-opus-4-7", "gpt-4o-mini"];
    let conv_titles = [
        "Refactor the agent feature",
        "Plan the Phase A migration",
        "Debug the CRDT codec",
    ];
    let mut convs: Vec<agent_proto::AgentConversation> = Vec::new();
    for (title, model) in conv_titles.iter().zip(conv_models.iter()) {
        let c = conv_repo
            .create(agent_proto::AgentConversationCreate {
                title: (*title).into(),
                system_prompt: Some("You are a helpful AI pair programmer.".into()),
                default_model: (*model).into(),
                temperature_milli: 700,
                max_tokens: Some(4096),
                tool_set: vec!["shell".into(), "edit".into(), "grep".into()],
                agent_run_id: None,
                project_id: None,
                parent_conversation_id: None,
                branch_from_message_id: None,
                archived: false,
            })
            .await?;
        convs.push(c);
    }

    let chat_message_repo = chat_crdt::MessageRepoLoro::new(cdoc);
    // 15 messages alternating user/assistant across the 3 convos.
    // Reasoning text goes on one specific assistant turn so a UI
    // snapshot test can pin it.
    for i in 0..15usize {
        let conv = &convs[i % convs.len()];
        let is_user = i % 2 == 0;
        let role: &str = if is_user { "user" } else { "assistant" };
        let body = if is_user {
            "Can you take a look at this?".to_string()
        } else {
            "Sure — here's a quick rundown of the trade-offs.".to_string()
        };
        let reasoning = if !is_user && i == 3 {
            Some(
                "The user is asking about CRDT convergence. Focus on the merge semantics first."
                    .into(),
            )
        } else {
            None
        };
        chat_message_repo
            .create(chat_proto::MessageCreate {
                channel_id: None,
                author: if is_user {
                    "cody".into()
                } else {
                    "assistant".into()
                },
                body,
                reply_to: None,
                edited_at: None,
                deleted: false,
                mentions: Vec::new(),
                attachment_ids: Vec::new(),
                role: Some(role.into()),
                model: Some(conv.default_model.clone()),
                reasoning,
                tool_calls_json: None,
                finish_reason: if is_user { None } else { Some("stop".into()) },
                tokens_input: if is_user { None } else { Some(420) },
                tokens_output: if is_user { None } else { Some(180) },
                cost_cents: if is_user { None } else { Some(3) },
                streaming: false,
                agent_conversation_id: Some(conv.id),
            })
            .await?;
    }
    info!("  agent: 3 conversations, 15 AI messages");

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

    // ── Knowledge (Obsidian-compatible PKM/outliner) ───────────
    // Phase A seed: 1 demo vault, 50 pages, ~300 blocks, 8 tags,
    // 2 bases. Mix of paragraphs / headings / lists / code, with
    // ~5 pages tagged `#projects/alpha` or `#books`. Bases are
    // YAML-only; the UI dispatches them in a later phase.
    let kvaults = knowledge_crdt::VaultRepoLoro::new(cdoc);
    let kfolders = knowledge_crdt::FolderRepoLoro::new(cdoc);
    let kpages = knowledge_crdt::PageRepoLoro::new(cdoc);
    let kblocks = knowledge_crdt::BlockRepoLoro::new(cdoc);
    let ktags = knowledge_crdt::KnowledgeTagRepoLoro::new(cdoc);
    let kbases = knowledge_crdt::BaseRepoLoro::new(cdoc);

    let vault = kvaults
        .create(knowledge_proto::VaultCreate {
            name: "Demo Vault".into(),
            root_path: None,
            use_markdown_links: false,
            new_link_format: "shortest".into(),
            attachment_folder_path: "attachments".into(),
            default_view_mode: "live-preview".into(),
            config_json: "{}".into(),
        })
        .await?;

    let inbox_folder = kfolders
        .create(knowledge_proto::FolderCreate {
            vault_id: vault.id,
            path: "inbox".into(),
            parent_id: None,
        })
        .await?;
    let projects_folder = kfolders
        .create(knowledge_proto::FolderCreate {
            vault_id: vault.id,
            path: "projects".into(),
            parent_id: None,
        })
        .await?;

    for tag in [
        "projects/alpha",
        "projects/beta",
        "books",
        "research",
        "todo",
        "ideas",
        "people",
        "meetings",
    ] {
        ktags
            .create(knowledge_proto::KnowledgeTagCreate {
                vault_id: vault.id,
                tag: tag.into(),
                color: None,
                description: None,
            })
            .await?;
    }

    let now_ts = chrono::Utc::now();
    let mut block_total = 0usize;
    for i in 0..50usize {
        let folder_id = if i % 5 == 0 {
            Some(projects_folder.id)
        } else if i % 7 == 0 {
            Some(inbox_folder.id)
        } else {
            None
        };
        let basename = format!("Page-{:02}", i);
        let path = match folder_id {
            Some(fid) if fid == projects_folder.id => format!("projects/{basename}.md"),
            Some(fid) if fid == inbox_folder.id => format!("inbox/{basename}.md"),
            _ => format!("{basename}.md"),
        };
        let page = kpages
            .create(knowledge_proto::PageCreate {
                vault_id: vault.id,
                folder_id,
                path,
                basename: basename.clone(),
                ext: "md".into(),
                aliases: Vec::new(),
                frontmatter_json: "[]".into(),
                stat_ctime: now_ts,
                stat_mtime: now_ts,
                stat_size: 0,
                is_journal: false,
                journal_day: None,
                shadow_for_kind: None,
                shadow_for_id: None,
            })
            .await?;

        // 4–8 blocks per page = ~300 total across 50 pages.
        let block_count = 4 + (i % 5);
        for b in 0..block_count {
            let (kind, content) = if b == 0 {
                ("heading".to_string(), basename.clone())
            } else if b == 1 && i % 5 == 0 {
                (
                    "paragraph".into(),
                    format!("Mentions [[Page-{:02}]] and #projects/alpha.", (i + 3) % 50),
                )
            } else if b == 2 && i % 11 == 0 {
                ("code".into(), "let x = 42;".into())
            } else if b == 3 {
                (
                    "list_item".into(),
                    format!("an item about #books topic {i}"),
                )
            } else {
                (
                    "paragraph".into(),
                    format!(
                        "Paragraph {b} on page {i} — see [[Page-{:02}]].",
                        (i + 1) % 50
                    ),
                )
            };
            let heading_level = if kind == "heading" { Some(1i32) } else { None };
            let code_lang = if kind == "code" {
                Some("rust".into())
            } else {
                None
            };
            kblocks
                .create(knowledge_proto::BlockCreate {
                    vault_id: vault.id,
                    page_id: page.id,
                    parent_block_id: None,
                    sort_key: format!("{:04}", b),
                    kind,
                    content,
                    heading_level,
                    list_ordered: false,
                    list_task: None,
                    code_lang,
                    callout_kind: None,
                    callout_foldable: false,
                    properties_json: "[]".into(),
                    obsidian_block_id: None,
                    collapsed: false,
                    refs_json: "[]".into(),
                    canvas_node_json: None,
                })
                .await?;
            block_total += 1;
        }
    }

    kbases
        .create(knowledge_proto::BaseCreate {
            vault_id: vault.id,
            page_id: None,
            name: "All Projects".into(),
            definition_yaml:
                "filters:\n  and:\n    - tag: projects\nviews:\n  - type: table\n    name: All\n"
                    .into(),
            parsed_filter_json: "{}".into(),
            parsed_views_json: "[]".into(),
        })
        .await?;
    kbases
        .create(knowledge_proto::BaseCreate {
            vault_id: vault.id,
            page_id: None,
            name: "Books".into(),
            definition_yaml:
                "filters:\n  and:\n    - tag: books\nviews:\n  - type: gallery\n    name: Library\n"
                    .into(),
            parsed_filter_json: "{}".into(),
            parsed_views_json: "[]".into(),
        })
        .await?;

    info!("  knowledge: 1 vault, 2 folders, 50 pages, {block_total} blocks, 8 tags, 2 bases");

    // ── CalDAV reference data ─────────────────────────────────
    let caldav_acct = caldav_crdt::CalDavAccountRepoLoro::new(cdoc);
    let caldav_cal = caldav_crdt::CalDavCalendarRepoLoro::new(cdoc);
    caldav_proto::seed_fake_cal_dav_account(&caldav_acct, 3usize).await?;
    caldav_proto::seed_fake_cal_dav_calendar(&caldav_cal, 6usize).await?;
    info!("  caldav: 3 accounts, 6 calendars");

    info!("seed complete");
    Ok(())
}
