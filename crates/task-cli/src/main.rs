mod commands;
mod shared;

use commands::agent::AgentCommands;
use commands::asset::AssetCommands;
use commands::attachment::AttachmentCommands;
use commands::audio::AudioCommands;
use commands::calendar::CalendarCommands;
use commands::client::ClientCommands;
use commands::comment::CommentCommands;
use commands::conflict::ConflictCommands;
use commands::cooking::CookCommands;
use commands::demo::DemoCommands;
use commands::email::EmailCommands;
use commands::expense::ExpenseCommands;
use commands::github::GithubCommands;
use commands::inbox::InboxCommands;
use commands::invoice::InvoiceCommands;
use commands::operating::OperatingCommands;
use commands::people::PeopleCommands;
use commands::project::ProjectCommands;
use commands::server::ServerCommands;
use commands::talk::TalkCommands;
use commands::time::TimeCommands;

use chrono::{DateTime, Datelike, NaiveDate, TimeZone, Utc};
use clap::{Args, Parser, Subcommand};
use shared::{
    RemoteVoxConfig, ServerProfile, ServerProfiles, api_to_model, find_task_in,
    load_server_profiles, model_to_api, remote_create_expense_with_client,
    remote_find_calendar_event_with_client, remote_find_client_with_client,
    remote_find_expense_with_client, remote_find_invoice_with_client,
    remote_find_project_with_client, remote_find_task, remote_find_task_with_client,
    remote_list_clients_with_client, remote_list_expenses_with_client,
    remote_list_invoices_with_client, remote_list_projects_with_client,
    remote_list_tasks_with_client, remote_save_client_with_client,
    remote_update_calendar_event_with_client, remote_update_expense_with_client,
    remote_update_project_with_client, remote_update_task_with_client, save_server_profiles,
};
use task_core::expense::{
    ExpenseCreateRequest, ExpenseFilter, ExpensePatch, render_expense_body, render_expense_report,
};
use task_core::index::{ChangeRow, ConflictRow};
use task_core::workflows::{Comment, parse_comments, render_comments};
use task_core::{
    Asset, AssetConflict, AssetCreateRequest, AssetFilter, AssetMaintenanceRecord, AssetPatch,
    AssetReservationRecord, AssetReservationResponse, AssetReserveRequest, AssetStatus,
    BusinessFinanceReport, CalendarEvent, CalendarEventPatch, CalendarEventStatus,
    CardDavSyncCollectionRequest, ChannelConversation, ChannelMessage, ChannelSendMessageRequest,
    Client, Filter, InboxCaptureRequest, InboxItem, InboxPromoteRequest, Invoice,
    OperatingModelReport, OrganizationContext, OrganizationRecord, Person, PersonContext, Priority,
    Project, ProjectKnowledgeContext, ProviderSyncState, Query, RelationType, ReviewReport, Sort,
    Status, SyncStats, SystemCapabilities, SystemHealth, Task, TaskRelation, TimeEntryContext,
    TimeEntryFilter, WikiLink, build_agent_plan,
};
use uuid::Uuid;

#[derive(Parser)]
#[command(name = "task", about = "Task management CLI", version)]
pub(crate) struct Cli {
    /// Path to the vault directory
    #[arg(long, env = "TASK_VAULT", global = true)]
    vault: Option<String>,

    /// task-server base URL or Vox URL. When set, agent commands use remote Vox services.
    #[arg(long, env = "TASK_SERVER", global = true)]
    server: Option<String>,

    /// Better Auth session token for remote Vox connections.
    #[arg(long, env = "TASK_SESSION_TOKEN", global = true)]
    session_token: Option<String>,

    /// Organization id to route remote Vox requests.
    #[arg(long, env = "TASK_ORGANIZATION_ID", global = true)]
    organization_id: Option<String>,

    /// Act as this user — sets created_by / comment author / resolved_by.
    #[arg(long, env = "TASK_USER", global = true)]
    as_user: Option<String>,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
pub(crate) enum Commands {
    /// List tasks (applies has-started filter by default; use --all to skip)
    List {
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        context: Option<String>,
        #[arg(long)]
        tag: Option<String>,
        /// Only overdue tasks
        #[arg(long)]
        overdue: bool,
        /// Due today
        #[arg(long)]
        today: bool,
        /// Due this week
        #[arg(long)]
        week: bool,
        /// Exclude completed/cancelled/archived tasks
        #[arg(long)]
        active: bool,
        /// Substring search on title
        #[arg(long)]
        search: Option<String>,
        /// Sort by: urgency (default), priority, due, scheduled, title, status
        #[arg(long, default_value = "urgency")]
        sort: String,
        #[arg(long, short = 'n')]
        limit: Option<usize>,
        /// Skip the has-started filter (show all tasks)
        #[arg(long)]
        all: bool,
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Create a new task
    Add {
        #[arg(long)]
        title: String,
        /// Priority: none, low, normal, high, urgent
        #[arg(long)]
        priority: Option<String>,
        /// Status: open (default), in-progress, planned, on-hold
        #[arg(long)]
        status: Option<String>,
        /// Due date (YYYY-MM-DD)
        #[arg(long)]
        due: Option<String>,
        /// Scheduled date (YYYY-MM-DD)
        #[arg(long)]
        scheduled: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        context: Option<String>,
        #[arg(long)]
        tag: Option<String>,
        /// RRULE-style recurrence, e.g. "FREQ=WEEKLY;BYDAY=MO"
        #[arg(long)]
        recurrence: Option<String>,
        #[arg(long)]
        assignee: Option<String>,
    },
    /// Capture raw text into the untriaged inbox
    Capture {
        #[arg(required = true, trailing_var_arg = true)]
        text: Vec<String>,
        /// Initial kind: inbox, commitment, idea, task, waiting, reference
        #[arg(long)]
        kind: Option<String>,
        /// Capture source label
        #[arg(long, default_value = "cli")]
        source: String,
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Inbox capture and triage commands
    Inbox {
        #[command(subcommand)]
        command: InboxCommands,
    },
    /// People, organizations, and relationship context
    People {
        #[command(subcommand)]
        command: PeopleCommands,
    },
    /// Life/business operating model and review pressure
    Operate {
        #[command(subcommand)]
        command: OperatingCommands,
    },
    /// Mark a task as complete
    Complete { title: String },
    /// Show detailed info for a task
    Show {
        title: String,
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Update mutable fields on an existing task
    Update {
        /// Task title or id
        reference: String,
        #[arg(long)]
        title: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        priority: Option<String>,
        /// Due date (YYYY-MM-DD) — pass "clear" to remove
        #[arg(long)]
        due: Option<String>,
        /// Scheduled date (YYYY-MM-DD) — pass "clear" to remove
        #[arg(long)]
        scheduled: Option<String>,
        #[arg(long)]
        assignee: Option<String>,
        #[arg(long)]
        add_tag: Vec<String>,
        #[arg(long)]
        remove_tag: Vec<String>,
        #[arg(long)]
        add_project: Vec<String>,
        #[arg(long)]
        remove_project: Vec<String>,
        #[arg(long)]
        add_context: Vec<String>,
        #[arg(long)]
        remove_context: Vec<String>,
        /// RRULE-style recurrence — pass "clear" to remove
        #[arg(long)]
        recurrence: Option<String>,
        /// Replace the markdown body entirely
        #[arg(long)]
        body: Option<String>,
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Link one task to another with a typed relation
    Link {
        from: String,
        to: String,
        /// Relation type: blocks, blocked-by, relates, duplicate-of,
        /// implements, implemented-by, start-before, start-after,
        /// finish-before, finish-after
        #[arg(long, default_value = "relates")]
        kind: String,
    },
    /// List tasks assigned to a user
    For {
        user: String,
        #[arg(long)]
        json: bool,
    },
    /// List tasks due on or before a date
    DueBy {
        /// Date (YYYY-MM-DD)
        date: String,
        #[arg(long)]
        json: bool,
    },
    /// Delete a task (soft by default — sets deleted_at)
    Delete {
        reference: String,
        /// Hard delete — removes the .md file
        #[arg(long)]
        hard: bool,
    },
    /// Assign a task to a user (shortcut for update --assignee)
    Assign { reference: String, user: String },
    /// Comment on a task. Bare form adds a comment; subcommands manage existing.
    Comment {
        #[command(subcommand)]
        command: CommentCommands,
    },
    /// React to a task with an emoji (or `clear:<emoji>` to remove)
    React { reference: String, emoji: String },
    /// Subscribe to a task (start receiving notifications)
    Subscribe {
        reference: String,
        /// Who to subscribe (defaults to --as-user)
        user: Option<String>,
    },
    /// Unsubscribe from a task
    Unsubscribe {
        reference: String,
        user: Option<String>,
    },
    /// Search tasks by text query (uses FTS5 index)
    Search {
        query: String,
        #[arg(long)]
        json: bool,
        #[arg(long, short = 'n')]
        limit: Option<usize>,
    },
    /// Subscribe to live task ops streamed from the Vox server
    Watch {
        /// Restrict to a single task id
        #[arg(long)]
        task_id: Option<String>,
        /// Restrict to a project (matched by project title)
        #[arg(long)]
        project: Option<String>,
        /// Output ops as JSON Lines
        #[arg(long)]
        json: bool,
    },
    /// Trigger a Nextcloud sync cycle and print stats
    Sync {
        #[arg(long)]
        json: bool,
        /// Print persisted provider sync state instead of triggering sync
        #[arg(long)]
        state: bool,
        /// Print a dry-run sync plan instead of mutating providers
        #[arg(long)]
        plan: bool,
    },
    /// GitHub Issues sync — pull issues, push status, dry-run plan
    Github {
        #[command(subcommand)]
        command: GithubCommands,
    },
    /// Demo seed mode — explain how to populate fixture data.
    Demo {
        #[command(subcommand)]
        command: DemoCommands,
    },
    /// Obsidian-style polymorphic property management.
    Prop {
        #[command(subcommand)]
        command: commands::property::PropertyCommands,
    },
    /// Asset inventory and maintenance tracking
    Asset {
        #[command(subcommand)]
        command: AssetCommands,
    },
    /// File attachments — upload/list/download/delete files hung off entities
    Attachment {
        #[command(subcommand)]
        command: AttachmentCommands,
    },
    /// Audio production workflow — tracks, mixes, masters
    Audio {
        #[command(subcommand)]
        command: AudioCommands,
    },
    /// Cooking / meal-prep workflow — recipes, cookbooks, meal plans, shopping lists
    Cook {
        #[command(subcommand)]
        command: CookCommands,
    },
    /// Reusable venues, locations, spaces, and default files
    Location {
        #[command(subcommand)]
        command: LocationCommands,
    },
    /// Nextcloud Talk — conversational surface for bots and humans
    Talk {
        #[command(subcommand)]
        command: TalkCommands,
    },
    /// Client (billable party) management
    Client {
        #[command(subcommand)]
        command: ClientCommands,
    },
    /// Invoice generation — markdown-backed, lives in `invoices/<id>.md`
    Invoice {
        #[command(subcommand)]
        command: InvoiceCommands,
    },
    /// Expense tracking — CLI-first spend ledger, stored in `expenses/<id>.md`
    Expense {
        #[command(subcommand)]
        command: ExpenseCommands,
    },
    /// Revenue attribution — realized income ledger, stored in `revenue/<id>.md`
    Revenue {
        #[command(subcommand)]
        command: RevenueCommands,
    },
    /// Email linking — associate emails with tasks/projects. Bot-friendly.
    Email {
        #[command(subcommand)]
        command: EmailCommands,
    },
    /// Start a timer on a task (fails if another is running)
    Start {
        reference: String,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        billable: bool,
        /// Billable rate in cents per hour
        #[arg(long)]
        rate: Option<u32>,
    },
    /// Stop the running timer (optionally scoped to a specific task)
    Stop {
        reference: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Time-tracking subcommands
    Time {
        #[command(subcommand)]
        command: TimeCommands,
    },
    /// Show recent activity (audit log of changes across the vault)
    Activity {
        #[arg(long, short = 'n', default_value = "50")]
        limit: u32,
        /// Only show entries with this entity_type (e.g. "task")
        #[arg(long)]
        kind: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Conflict log (concurrent edits detected during sync)
    Conflicts {
        #[command(subcommand)]
        command: ConflictCommands,
    },
    /// First-class calendar events (VEVENT), separate from task due dates
    Calendar {
        #[command(subcommand)]
        command: CalendarCommands,
    },
    /// Stable JSON command surface for agents and automation
    Agent {
        #[command(subcommand)]
        command: AgentCommands,
    },
    /// Project subcommands
    Project {
        #[command(subcommand)]
        command: ProjectCommands,
    },
    /// Validate configuration, server capabilities, and provider health
    Doctor {
        #[arg(long)]
        json: bool,
        /// Run live provider checks against WebDAV, CalDAV, Mail, and Deck
        #[arg(long)]
        deep: bool,
    },
    /// Manage named task-server connection profiles
    Server {
        #[command(subcommand)]
        command: ServerCommands,
    },
}

#[derive(Subcommand)]
pub(crate) enum RevenueCommands {
    /// Create a new revenue attribution entry
    Create {
        description: String,
        #[arg(long)]
        amount: u64,
        #[arg(long)]
        date: Option<String>,
        #[arg(long)]
        currency: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        client: Option<String>,
        #[arg(long)]
        deliverable: Option<String>,
        #[arg(long = "invoice")]
        invoice_id: Option<String>,
        #[arg(long = "invoice-line")]
        invoice_line_id: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long = "payment-method")]
        payment_method: Option<String>,
        #[arg(long = "payment-reference")]
        payment_reference: Option<String>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List revenue entries
    List {
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        client: Option<String>,
        #[arg(long)]
        deliverable: Option<String>,
        #[arg(long = "invoice")]
        invoice_id: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show a single revenue entry
    Show {
        id: String,
        #[arg(long)]
        md: bool,
        #[arg(long)]
        json: bool,
    },
    /// Show a revenue attribution report
    Report {
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        client: Option<String>,
        #[arg(long)]
        deliverable: Option<String>,
        #[arg(long = "invoice")]
        invoice_id: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Delete a revenue entry
    Delete { id: String },
}

#[derive(Subcommand)]
pub(crate) enum LocationCommands {
    /// Create a reusable location / venue record
    Add {
        name: String,
        #[arg(long = "type")]
        venue_type: Option<String>,
        #[arg(long)]
        address1: Option<String>,
        #[arg(long)]
        address2: Option<String>,
        #[arg(long)]
        city: Option<String>,
        #[arg(long)]
        state: Option<String>,
        #[arg(long = "postal-code")]
        postal_code: Option<String>,
        #[arg(long = "country")]
        country_code: Option<String>,
        #[arg(long = "contact-name")]
        contact_name: Option<String>,
        #[arg(long = "contact-email")]
        contact_email: Option<String>,
        #[arg(long = "contact-phone")]
        contact_phone: Option<String>,
        #[arg(long = "access-notes")]
        access_notes: Option<String>,
        #[arg(long = "parking-load-in")]
        parking_load_in: Option<String>,
        #[arg(long = "network-power")]
        network_power: Option<String>,
        #[arg(long)]
        tag: Vec<String>,
        #[arg(long)]
        body: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List reusable locations
    List {
        #[arg(long)]
        json: bool,
    },
    /// Show one location by id or name
    Show {
        reference: String,
        #[arg(long = "defaults-for")]
        defaults_for: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Update mutable location fields
    Update {
        reference: String,
        #[arg(long)]
        name: Option<String>,
        #[arg(long = "type")]
        venue_type: Option<String>,
        #[arg(long)]
        address1: Option<String>,
        #[arg(long)]
        address2: Option<String>,
        #[arg(long)]
        city: Option<String>,
        #[arg(long)]
        state: Option<String>,
        #[arg(long = "postal-code")]
        postal_code: Option<String>,
        #[arg(long = "country")]
        country_code: Option<String>,
        #[arg(long = "contact-name")]
        contact_name: Option<String>,
        #[arg(long = "contact-email")]
        contact_email: Option<String>,
        #[arg(long = "contact-phone")]
        contact_phone: Option<String>,
        #[arg(long = "access-notes")]
        access_notes: Option<String>,
        #[arg(long = "parking-load-in")]
        parking_load_in: Option<String>,
        #[arg(long = "network-power")]
        network_power: Option<String>,
        #[arg(long)]
        tag: Vec<String>,
        #[arg(long)]
        body: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Add or replace a space under a location
    SpaceAdd {
        location: String,
        name: String,
        #[arg(long)]
        capacity: Option<u32>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        tag: Vec<String>,
        #[arg(long)]
        json: bool,
    },
    /// List spaces under a location
    SpaceList {
        location: String,
        #[arg(long)]
        json: bool,
    },
    /// Add or replace a venue/space default file
    DefaultAdd {
        location: String,
        #[arg(long)]
        space: Option<String>,
        #[arg(long)]
        kind: String,
        #[arg(long)]
        path: String,
        #[arg(long)]
        label: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Delete a location by id or name
    Delete { reference: String },
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    let cli = Cli::parse();
    let Cli {
        vault,
        server,
        session_token,
        organization_id,
        as_user: actor,
        command,
    } = cli;

    // Local Talk commands don't touch the vault. Remote Talk goes through Vox.
    if server.is_none() {
        if let Commands::Talk { command: talk } = command {
            return commands::talk::run_talk(talk, actor).await;
        }
    }
    if let Commands::Agent {
        command: AgentCommands::Capabilities,
    } = command
    {
        print_agent_capabilities();
        return Ok(());
    }
    if let Commands::Doctor { json, deep } = command {
        if let Some(server) = server {
            let remote = RemoteVoxConfig::new(server, session_token, organization_id)?;
            return run_remote_doctor(&remote, json, deep).await;
        }
        return run_local_doctor(vault.as_deref(), json, deep).await;
    }
    if let Commands::Server { command } = command {
        return commands::server::run_server_command(command).await;
    }
    // `task email watch` is a pure IMAP IDLE subscription — no vault
    // access needed. Handle it here so the watcher service doesn't
    // have to carry a TASK_VAULT just to emit events.
    if let Commands::Email {
        command:
            EmailCommands::Watch {
                host,
                port,
                user,
                mailbox,
                ca_bundle,
                insecure,
            },
    } = command
    {
        let password = std::env::var("IMAP_PASSWORD")
            .map_err(|_| eyre::eyre!("Set IMAP_PASSWORD env var (bridge password)"))?;
        let config = task_core::provider::ImapWatchConfig {
            host,
            port,
            user,
            password,
            mailbox,
            ca_bundle,
            insecure,
            ..Default::default()
        };
        return task_core::provider::watch_idle(config, |ev| {
            let mailbox = escape_json(&ev.mailbox);
            let raw = escape_json(&ev.raw);
            let exists = ev
                .exists
                .map(|n| n.to_string())
                .unwrap_or_else(|| "null".into());
            let ts = chrono::Utc::now().to_rfc3339();
            println!(r#"{{"ts":"{ts}","mailbox":"{mailbox}","exists":{exists},"raw":"{raw}"}}"#);
        })
        .await
        .map_err(Into::into);
    }
    if let Some(server) = server {
        let remote = RemoteVoxConfig::new(server, session_token, organization_id)?;
        return run_remote_command(&remote, actor.as_deref(), command).await;
    }

    let _ = vault;
    let _ = actor;
    let _ = command;
    eyre::bail!(
        "local markdown command execution has been removed; run task-server and pass --server/TASK_SERVER so commands use generated repo services"
    );
}

pub(crate) async fn run_remote_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: Commands,
) -> eyre::Result<()> {
    match command {
        Commands::Agent { command } => {
            commands::agent::run_remote_agent_command(remote, actor, command).await?
        }
        Commands::Doctor { json, deep } => run_remote_doctor(remote, json, deep).await?,
        Commands::Server { .. } => unreachable!("handled before remote dispatch"),

        Commands::List {
            status,
            project,
            context,
            tag,
            overdue,
            today,
            week,
            active,
            search,
            sort,
            limit,
            all,
            json,
        } => {
            let tasks = if all {
                remote_list_tasks_with_client(&remote.task_repo().await?).await?
            } else {
                let client = remote.task().await?;
                let mut filters = Vec::new();
                if let Some(s) = status {
                    let st = parse_status(&s).ok_or_else(|| eyre::eyre!("Unknown status: {s}"))?;
                    filters.push(Filter::Status(st));
                }
                if let Some(p) = project {
                    filters.push(Filter::HasProject(p));
                }
                if let Some(c) = context {
                    filters.push(Filter::HasContext(c));
                }
                if let Some(t) = tag {
                    filters.push(Filter::HasTag(t));
                }
                if overdue {
                    filters.push(Filter::Overdue);
                }
                if today {
                    filters.push(Filter::DueToday);
                }
                if week {
                    filters.push(Filter::DueThisWeek);
                }
                if active {
                    filters.push(Filter::NotComplete);
                    filters.push(Filter::NotCancelled);
                    filters.push(Filter::NotArchived);
                }
                if let Some(q) = search {
                    filters.push(Filter::TitleContains(q));
                }
                client
                    .execute_query(Query {
                        filters,
                        sort: parse_sort(&sort),
                        limit,
                        group: None,
                    })
                    .await?
            };
            if json {
                print_tasks_json(&tasks);
            } else {
                print_tasks_table(&tasks);
            }
        }

        Commands::Add {
            title,
            priority,
            status,
            due,
            scheduled,
            project,
            context,
            tag,
            recurrence,
            assignee,
        } => {
            let task = commands::task::build_new_task(commands::task::NewTaskInput {
                title,
                priority,
                status,
                due,
                scheduled,
                project,
                context,
                tag,
                recurrence,
                assignee,
                actor: actor.map(str::to_string),
            })?;
            let create: task_core::task::TaskApiCreate = model_to_api(&task)?;
            let created: Task = api_to_model(remote.task_repo().await?.create_task(create).await?)?;
            println!("Created: {}", created.title);
            println!("  id:  {}", created.id);
            if let Some(d) = created.due {
                println!("  due: {d}");
            }
        }

        Commands::Capture {
            text,
            kind,
            source,
            json,
        } => {
            let item = remote
                .inbox()
                .await?
                .capture(InboxCaptureRequest {
                    text: text.join(" "),
                    actor: actor.map(str::to_string),
                    source: Some(source),
                    kind,
                })
                .await?;
            print_inbox_capture(&item, json);
        }

        Commands::Inbox { command } => {
            let client = remote.inbox().await?;
            match command {
                InboxCommands::List { json } => {
                    let items = client.list_inbox().await?;
                    print_inbox_items(&items, json);
                }
                InboxCommands::Daily { json } => {
                    let report = client.daily_review().await?;
                    print_review_report(&report, json);
                }
                InboxCommands::Weekly { json } => {
                    let report = client.weekly_review().await?;
                    print_review_report(&report, json);
                }
                InboxCommands::Monthly { json } => {
                    let report = client.monthly_review().await?;
                    print_review_report(&report, json);
                }
                InboxCommands::Project { name, json } => {
                    let report = client.project_review(name).await?;
                    print_review_report(&report, json);
                }
                InboxCommands::Promote {
                    reference,
                    kind,
                    project,
                    status,
                    assignee,
                    due,
                    scheduled,
                    add_tags,
                    json,
                } => {
                    let item = client
                        .promote(InboxPromoteRequest {
                            reference,
                            kind,
                            project,
                            status,
                            assignee,
                            due,
                            scheduled,
                            add_tags,
                            actor: actor.map(str::to_string),
                        })
                        .await?;
                    print_inbox_capture(&item, json);
                }
            }
        }

        Commands::People { command } => {
            let client = remote.people().await?;
            match command {
                PeopleCommands::List { addressbook, json } => {
                    let people = client.list_people(addressbook).await?;
                    print_people(&people, json);
                }
                PeopleCommands::Orgs { addressbook, json } => {
                    let orgs = client.list_organizations(addressbook).await?;
                    print_organizations(&orgs, json);
                }
                PeopleCommands::Show {
                    reference,
                    addressbook,
                    json,
                } => {
                    let context = client.person_context(reference, addressbook).await?;
                    print_person_context(context.as_ref(), json);
                }
                PeopleCommands::Org {
                    reference,
                    addressbook,
                    json,
                } => {
                    let context = client.organization_context(reference, addressbook).await?;
                    print_organization_context(context.as_ref(), json);
                }
            }
        }

        Commands::Talk { command } => {
            let client = remote.conversation().await?;
            match command {
                TalkCommands::Rooms { json } => {
                    let rooms = client.list_conversations().await?;
                    print_channel_rooms(&rooms, json);
                }
                TalkCommands::Send {
                    room,
                    message,
                    reply_to,
                } => {
                    let sent = client
                        .send_message(ChannelSendMessageRequest {
                            conversation_id: room,
                            body: message,
                            reply_to: reply_to.map(|id| id.to_string()),
                        })
                        .await?;
                    println!("Sent message {} to {}.", sent.id, sent.conversation_id);
                }
                TalkCommands::History { room, limit, json } => {
                    let messages = client.recent_messages(room, limit).await?;
                    print_channel_history(&messages, json);
                }
            }
        }

        Commands::Operate { command } => {
            let client = remote.operating().await?;
            match command {
                OperatingCommands::Model { json } => {
                    let report = client.operating_model().await?;
                    print_operating_model(&report, json);
                }
            }
        }

        Commands::Complete { title } => {
            let task = remote.task().await?.complete_task(title).await?;
            if task.recurrence.is_some() {
                let next = task
                    .scheduled
                    .map(|d| d.to_string())
                    .unwrap_or_else(|| "—".to_string());
                println!("Recurring task completed. Next occurrence: {next}");
            } else {
                println!("Done: {}", task.title);
            }
        }

        Commands::Show { title, json } => {
            let task = remote_find_task(remote, &title).await?;
            if json {
                println!("{}", facet_json::to_string(&task).unwrap_or_default());
            } else {
                print_task_detail(&task);
            }
        }

        Commands::Update {
            reference,
            title,
            status,
            priority,
            due,
            scheduled,
            assignee,
            add_tag,
            remove_tag,
            add_project,
            remove_project,
            add_context,
            remove_context,
            recurrence,
            body,
            json,
        } => {
            let mut task = remote_find_task(remote, &reference).await?;
            commands::task::apply_task_update(
                &mut task,
                commands::task::TaskUpdateInput {
                    title,
                    status,
                    priority,
                    due,
                    scheduled,
                    assignee,
                    add_tag,
                    remove_tag,
                    add_project,
                    remove_project,
                    add_context,
                    remove_context,
                    recurrence,
                    body,
                },
            )?;
            let updated = remote_update_task_with_client(&remote.task_repo().await?, &task).await?;
            if json {
                println!("{}", facet_json::to_string(&updated).unwrap_or_default());
            } else {
                println!("Updated: {}", updated.title);
            }
        }

        Commands::Delete { reference, hard } => {
            let client = remote.task_repo().await?;
            if hard {
                let task = remote_find_task_with_client(&client, &reference).await?;
                client.delete_task(task.id.to_string()).await?;
                println!("Deleted (hard): {}", task.title);
            } else {
                let mut task = remote_find_task_with_client(&client, &reference).await?;
                task.deleted_at = Some(chrono::Utc::now());
                let updated = remote_update_task_with_client(&client, &task).await?;
                println!("Deleted (soft): {}", updated.title);
            }
        }

        Commands::Assign { reference, user } => {
            let client = remote.task_repo().await?;
            let mut task = remote_find_task_with_client(&client, &reference).await?;
            task.assignee = if user == "clear" || user.is_empty() {
                None
            } else {
                Some(user)
            };
            let updated = remote_update_task_with_client(&client, &task).await?;
            match &updated.assignee {
                Some(u) => println!("Assigned '{}' → {u}", updated.title),
                None => println!("Unassigned '{}'", updated.title),
            }
        }

        Commands::For { user, json } => {
            let tasks = remote.task().await?.tasks_for_user(user).await?;
            if json {
                print_tasks_json(&tasks);
            } else {
                print_tasks_table(&tasks);
            }
        }

        Commands::DueBy { date, json } => {
            let tasks = remote.calendar().await?.tasks_due_by(date).await?;
            if json {
                print_tasks_json(&tasks);
            } else {
                print_tasks_table(&tasks);
            }
        }

        Commands::Watch {
            task_id,
            project,
            json,
        } => {
            commands::task::run_watch(remote, task_id, project, json).await?;
        }

        Commands::Search { query, json, limit } => {
            let mut results = remote.task().await?.search_tasks(query).await?;
            if let Some(n) = limit {
                results.truncate(n);
            }
            if json {
                print_tasks_json(&results);
            } else {
                print_tasks_table(&results);
            }
        }

        Commands::Link { from, to, kind } => {
            let client = remote.task_repo().await?;
            let mut source = remote_find_task_with_client(&client, &from).await?;
            let target = remote_find_task_with_client(&client, &to).await?;
            let rt = parse_relation_kind(&kind)?;
            let target_ref = target.id.to_string();
            let already = source
                .relations
                .iter()
                .any(|r| r.target == target_ref && r.relation_type == rt);
            if !already {
                source.relations.push(TaskRelation {
                    target: target_ref,
                    relation_type: rt,
                });
                remote_update_task_with_client(&client, &source).await?;
            }
            println!("Linked '{from}' --{kind}--> '{to}'.");
        }

        Commands::React { reference, emoji } => {
            let user = require_actor(&actor.map(str::to_string))?;
            let client = remote.task_repo().await?;
            let mut task = remote_find_task_with_client(&client, &reference).await?;
            if let Some(e) = emoji.strip_prefix("clear:") {
                let before = task.reactions.len();
                task.reactions.retain(|r| !(r.user == user && r.emoji == e));
                if task.reactions.len() == before {
                    eyre::bail!("No {e} reaction from @{user} to remove");
                }
                remote_update_task_with_client(&client, &task).await?;
                println!("Removed {e} from @{user}.");
            } else {
                if !task
                    .reactions
                    .iter()
                    .any(|r| r.user == user && r.emoji == emoji)
                {
                    task.reactions.push(task_core::Reaction {
                        emoji: emoji.clone(),
                        user: user.clone(),
                    });
                    remote_update_task_with_client(&client, &task).await?;
                }
                println!("Reacted {emoji} from @{user}.");
            }
        }

        Commands::Subscribe { reference, user } => {
            let who = user
                .or_else(|| actor.map(str::to_string))
                .ok_or_else(|| eyre::eyre!("Specify a user or set --as-user/TASK_USER."))?;
            let client = remote.task_repo().await?;
            let mut task = remote_find_task_with_client(&client, &reference).await?;
            if !task.subscribers.contains(&who) {
                task.subscribers.push(who.clone());
                remote_update_task_with_client(&client, &task).await?;
            }
            println!("@{who} subscribed.");
        }

        Commands::Unsubscribe { reference, user } => {
            let who = user
                .or_else(|| actor.map(str::to_string))
                .ok_or_else(|| eyre::eyre!("Specify a user or set --as-user/TASK_USER."))?;
            let client = remote.task_repo().await?;
            let mut task = remote_find_task_with_client(&client, &reference).await?;
            let before = task.subscribers.len();
            task.subscribers.retain(|u| u != &who);
            if task.subscribers.len() != before {
                remote_update_task_with_client(&client, &task).await?;
            }
            println!("@{who} unsubscribed.");
        }

        Commands::Comment { command } => {
            commands::comment::run_remote_comment_command(remote, actor, command).await?
        }

        Commands::Sync { json, state, plan } => {
            if state {
                let states = remote.activity().await?.list_sync_states().await?;
                print_sync_states(&states, json);
                return Ok(());
            }
            if plan {
                let plan = remote.calendar().await?.sync_plan().await?;
                print_sync_plan(&plan, json);
                return Ok(());
            }
            let stats = remote.calendar().await?.trigger_sync().await?;
            if json {
                println!("{}", facet_json::to_string(&stats).unwrap_or_default());
            } else {
                print_sync_stats(&stats);
            }
        }

        Commands::Github { command } => {
            // GitHub sync is self-contained — doesn't need the remote Vox service.
            // We still need local tasks for plan/sync, so fetch them via remote.
            commands::github::run_github_command_remote(remote, command).await?;
        }
        Commands::Demo { command } => {
            commands::demo::run(command);
        }
        Commands::Prop { command } => {
            commands::property::run_remote_property_command(remote, command).await?
        }
        Commands::Project { command } => {
            commands::project::run_remote_project_command(remote, actor, command).await?
        }
        Commands::Client { command } => {
            commands::client::run_remote_client_command(remote, command).await?
        }
        Commands::Invoice { command } => {
            commands::invoice::run_remote_invoice_command(remote, actor, command).await?
        }
        Commands::Expense { command } => {
            commands::expense::run_remote_expense_command(remote, actor, command).await?
        }
        Commands::Revenue { .. } => {
            eyre::bail!("revenue commands are currently supported only in local vault mode")
        }
        Commands::Asset { command } => {
            commands::asset::run_remote_asset_command(remote, actor, command).await?
        }
        Commands::Attachment { command } => {
            commands::attachment::run_remote_attachment_command(remote, actor, command).await?
        }
        Commands::Cook { command } => {
            commands::cooking::run_remote_cook_command(remote, actor, command).await?
        }
        Commands::Audio { command } => {
            commands::audio::run_remote_audio_command(remote, actor, command).await?
        }
        Commands::Location { .. } => {
            eyre::bail!("location commands are currently supported only in local vault mode")
        }
        Commands::Start {
            reference,
            description,
            billable,
            rate,
        } => {
            let entry = remote
                .time()
                .await?
                .start_timer(task_core::TimeStartRequest {
                    task_ref: reference.clone(),
                    description,
                    billable,
                    billable_rate: rate,
                    user: actor.map(str::to_string),
                })
                .await?;
            println!("Started timer on '{reference}' (entry {}).", entry.id);
        }
        Commands::Stop { reference, json } => {
            let entry = remote.time().await?.stop_timer(reference).await?;
            if json {
                println!(
                    "{}",
                    facet_json::to_string(&entry.entry).unwrap_or_default()
                );
            } else {
                println!(
                    "Stopped '{}' — {} min logged.",
                    entry.task_title,
                    entry.entry.duration_minutes()
                );
            }
        }
        Commands::Time { command } => {
            commands::time::run_remote_time_command(remote, actor, command).await?
        }
        Commands::Calendar { command } => {
            commands::calendar::run_remote_calendar_command(remote, command).await?
        }
        Commands::Activity { limit, kind, json } => {
            let changes = remote.activity().await?.recent_activity(limit).await?;
            let filtered: Vec<_> = match kind {
                Some(k) => changes.into_iter().filter(|c| c.entity_type == k).collect(),
                None => changes,
            };
            if json {
                print_activity_json(&filtered);
            } else {
                print_activity_table(&filtered);
            }
        }
        Commands::Conflicts { command } => match command {
            ConflictCommands::List { all, limit, json } => {
                let rows = remote.activity().await?.list_conflicts(!all, limit).await?;
                if json {
                    print_conflicts_json(&rows);
                } else {
                    print_conflicts_table(&rows);
                }
            }
            ConflictCommands::Resolve { conflict_id, how } => {
                remote
                    .activity()
                    .await?
                    .resolve_conflict(conflict_id, actor.map(str::to_string), how.clone())
                    .await?;
                println!("Resolved conflict #{conflict_id} ({how}).");
            }
        },
        Commands::Email { command } => {
            commands::email::run_remote_email_command(remote, actor, command).await?
        }
    }
    Ok(())
}

pub(crate) async fn run_remote_doctor(
    remote: &RemoteVoxConfig,
    json: bool,
    deep: bool,
) -> eyre::Result<()> {
    let system = remote.system().await?;
    let capabilities = system.capabilities().await?;
    let health = system.health(deep).await?;
    if json {
        print_doctor_json(&capabilities, &health);
    } else {
        print_doctor_report("remote", Some(&remote.display_url), &capabilities, &health);
    }
    Ok(())
}

pub(crate) async fn run_local_doctor(
    vault: Option<&str>,
    json: bool,
    deep: bool,
) -> eyre::Result<()> {
    let vault_root = vault
        .map(String::from)
        .or_else(|| std::env::var("TASK_VAULT").ok())
        .unwrap_or_default();
    let vault_exists = !vault_root.is_empty() && std::path::Path::new(&vault_root).exists();
    let nextcloud_url = std::env::var("NEXTCLOUD_URL")
        .ok()
        .filter(|s| !s.is_empty());
    let nextcloud_user = std::env::var("NEXTCLOUD_USER")
        .ok()
        .or_else(|| std::env::var("NEXTCLOUD_USERNAME").ok())
        .filter(|s| !s.is_empty());
    let nextcloud_password_configured = std::env::var("NEXTCLOUD_PASSWORD")
        .ok()
        .filter(|s| !s.is_empty())
        .is_some()
        || std::env::var("NEXTCLOUD_PASSWORD_FILE")
            .ok()
            .filter(|s| !s.is_empty())
            .is_some()
        || std::env::var("TASK_NEXTCLOUD_CONFIG")
            .ok()
            .filter(|s| !s.is_empty())
            .is_some();
    let remote_configured = std::env::var("TASK_SERVER")
        .ok()
        .filter(|s| !s.is_empty())
        .is_some()
        || load_server_profiles()
            .ok()
            .and_then(|profiles| profiles.current())
            .is_some();
    let remote_token_configured = std::env::var("TASK_SESSION_TOKEN")
        .ok()
        .filter(|s| !s.is_empty())
        .is_some()
        || load_server_profiles()
            .ok()
            .and_then(|profiles| profiles.current())
            .and_then(|profile| profile.session_token)
            .is_some();
    let capabilities = SystemCapabilities {
        package: "task-cli".into(),
        version: env!("CARGO_PKG_VERSION").into(),
        protocol_version: 1,
        min_cli_version: "0.1.0".into(),
        min_server_version: "0.1.0".into(),
        services: Vec::new(),
        features: vec![
            "local-vault".into(),
            "remote-vox".into(),
            "task-doctor".into(),
        ],
        nextcloud: task_core::NextcloudCapability {
            configured: nextcloud_url.is_some() && nextcloud_password_configured,
            url: nextcloud_url.clone(),
            username: nextcloud_user,
            projects_path: std::env::var("NEXTCLOUD_PROJECTS_PATH").ok(),
            task_calendar: std::env::var("NEXTCLOUD_CALENDAR").ok(),
            event_calendar: std::env::var("NEXTCLOUD_EVENT_CALENDAR")
                .ok()
                .or_else(|| std::env::var("NEXTCLOUD_EVENTS_CALENDAR").ok()),
            deck_enabled: std::env::var("NEXTCLOUD_DECK_ENABLED")
                .ok()
                .map(|v| crate::commands::github::env_truthy(&v))
                .unwrap_or(true),
        },
        vault: task_core::VaultCapability {
            root: vault_root.clone(),
            exists: vault_exists,
            index_available: false,
        },
    };
    let mut checks = Vec::new();
    checks.push(task_core::HealthCheck {
        name: "vault".into(),
        code: if vault_exists {
            "VAULT_OK".into()
        } else if vault_root.is_empty() {
            "VAULT_NOT_CONFIGURED".into()
        } else {
            "VAULT_ROOT_MISSING".into()
        },
        severity: if vault_exists { "ok" } else { "error" }.into(),
        configured: !vault_root.is_empty(),
        ok: vault_exists,
        detail: if vault_root.is_empty() {
            "TASK_VAULT/--vault is not set".into()
        } else if vault_exists {
            format!("vault root exists at {vault_root}")
        } else {
            format!("vault root is missing at {vault_root}")
        },
        hint: Some("Set --vault or TASK_VAULT to an existing vault root.".into()),
    });
    checks.push(task_core::HealthCheck {
        name: "remote-server".into(),
        code: if remote_configured && remote_token_configured {
            "REMOTE_CONFIGURED".into()
        } else if remote_configured {
            "REMOTE_TOKEN_MISSING".into()
        } else {
            "REMOTE_NOT_CONFIGURED".into()
        },
        severity: if remote_configured && remote_token_configured {
            "ok"
        } else {
            "error"
        }
        .into(),
        configured: remote_configured,
        ok: remote_configured && remote_token_configured,
        detail: "set TASK_SERVER and TASK_SESSION_TOKEN, or pass --server/--session-token, for remote checks".into(),
        hint: Some("Run `task server add <name> --url <url> --session-token <token> --use-now` to save a profile.".into()),
    });
    checks.push(task_core::HealthCheck {
        name: "nextcloud-config".into(),
        code: if nextcloud_url.is_some() && nextcloud_password_configured {
            "NEXTCLOUD_CONFIGURED".into()
        } else {
            "NEXTCLOUD_NOT_CONFIGURED".into()
        },
        severity: if nextcloud_url.is_some() && nextcloud_password_configured {
            "ok"
        } else {
            "error"
        }
        .into(),
        configured: nextcloud_url.is_some() || nextcloud_password_configured,
        ok: nextcloud_url.is_some() && nextcloud_password_configured,
        detail: if nextcloud_url.is_some() && nextcloud_password_configured {
            "Nextcloud environment/config is present".into()
        } else {
            "Nextcloud URL and password/app-token are not both configured".into()
        },
        hint: Some(
            "Set NEXTCLOUD_URL and NEXTCLOUD_PASSWORD, or use TASK_NEXTCLOUD_CONFIG.".into(),
        ),
    });
    let degraded = checks
        .iter()
        .any(|check| check.configured && !check.ok && check.severity == "warning");
    let health = SystemHealth {
        ok: checks
            .iter()
            .all(|check| check.ok || !check.configured || check.severity == "warning"),
        degraded,
        deep,
        checks,
    };
    if json {
        print_doctor_json(&capabilities, &health);
    } else {
        print_doctor_report("local", None, &capabilities, &health);
    }
    Ok(())
}

// ── GitHub sync command handler ─────────────────────────────────────────────

pub(crate) fn print_projects_table(projects: &[Project]) {
    if projects.is_empty() {
        println!("No projects found.");
        return;
    }
    let name_w = projects.iter().map(|p| p.title.len()).max().unwrap_or(10) + 2;
    println!("{:<name_w$}  {:<10}  DUE", "NAME", "STATE");
    println!("{}", "─".repeat(name_w + 20));
    for p in projects {
        let state = format!("{:?}", p.status);
        let due = p
            .due
            .map(|d| d.to_string())
            .unwrap_or_else(|| "—".to_string());
        println!("{:<name_w$}  {:<10}  {}", p.title, state, due);
    }
    println!("\n{} project(s)", projects.len());
}

pub(crate) fn project_dashboard_bucket_label(
    bucket: &task_core::ProjectDashboardBucket,
) -> &'static str {
    match bucket {
        task_core::ProjectDashboardBucket::Overdue => "overdue",
        task_core::ProjectDashboardBucket::DueSoon => "due soon",
        task_core::ProjectDashboardBucket::Active => "active",
        task_core::ProjectDashboardBucket::NoOpenTasks => "done",
    }
}

pub(crate) fn project_progress_bar(percent: Option<f32>) -> String {
    let Some(percent) = percent else {
        return "—".to_string();
    };
    let width = 10usize;
    let filled = ((percent / 100.0) * width as f32)
        .round()
        .clamp(0.0, width as f32) as usize;
    format!(
        "[{}{}] {:>3.0}%",
        "█".repeat(filled),
        "░".repeat(width - filled),
        percent
    )
}

pub(crate) fn print_project_dashboard(entries: &[task_core::ProjectDashboardEntry], json: bool) {
    if json {
        println!("{}", facet_json::to_string(entries).unwrap_or_default());
        return;
    }

    if entries.is_empty() {
        println!("No active projects.");
        return;
    }

    let name_w = entries
        .iter()
        .map(|entry| entry.project.title.len())
        .max()
        .unwrap_or(10)
        .clamp(10, 36)
        + 2;
    let next_w = entries
        .iter()
        .map(|entry| {
            entry
                .next_task
                .as_ref()
                .map(|task| task.title.len())
                .unwrap_or(12)
        })
        .max()
        .unwrap_or(12)
        .clamp(12, 32)
        + 2;

    println!(
        "{:<name_w$}  {:<10}  {:<next_w$}  {:<16}  {:<5}  {:<4}  DUE",
        "PROJECT", "BUCKET", "NEXT", "PROGRESS", "OPEN", "OVD"
    );
    println!("{}", "─".repeat(name_w + next_w + 45));

    for entry in entries {
        let next = entry
            .next_task
            .as_ref()
            .map(|task| truncate(&task.title, next_w))
            .unwrap_or_else(|| "nothing left".to_string());
        let progress = project_progress_bar(entry.completion_percent);
        let due = entry
            .project
            .due
            .map(|d| {
                let due = d.to_string();
                if entry.project.is_overdue() {
                    format!("!{due}")
                } else {
                    due
                }
            })
            .unwrap_or_else(|| "—".to_string());
        println!(
            "{:<name_w$}  {:<10}  {:<next_w$}  {:<16}  {:<5}  {:<4}  {}",
            truncate(&entry.project.title, name_w),
            project_dashboard_bucket_label(&entry.bucket),
            next,
            progress,
            entry.stats.open_task_count,
            entry.overdue_task_count,
            due
        );
    }

    println!("\n{} project(s)", entries.len());
}

pub(crate) fn print_sync_stats(stats: &SyncStats) {
    println!("Sync complete.");
    println!(
        "  calendar: +{} / -{}",
        stats.calendar_pushed, stats.calendar_pulled
    );
    println!(
        "  deck:     +{} / -{}",
        stats.deck_pushed, stats.deck_pulled
    );
    println!(
        "  files:    created {}, updated {}",
        stats.files_created, stats.files_updated
    );
    if !stats.errors.is_empty() {
        println!("  errors:");
        for e in &stats.errors {
            println!("    - {e}");
        }
    }
}

pub(crate) fn print_sync_plan(plan: &task_core::SyncPlan, json: bool) {
    if json {
        println!("{}", facet_json::to_string(plan).unwrap_or_default());
        return;
    }
    println!(
        "Sync plan generated at {} ({})",
        plan.generated_at,
        if plan.safe_to_run {
            "safe"
        } else {
            "not configured"
        }
    );
    for warning in &plan.warnings {
        println!("warning: {warning}");
    }
    println!(
        "{:<18}  {:<18}  {:<14}  {:<13}  COLLECTION",
        "PROVIDER", "OPERATION", "DIRECTION", "CONFIGURED"
    );
    println!("{}", "─".repeat(88));
    for item in &plan.items {
        println!(
            "{:<18}  {:<18}  {:<14}  {:<13}  {}",
            item.provider,
            truncate(&item.operation, 18),
            item.direction,
            item.configured,
            item.collection
        );
        println!("  {}", item.detail);
    }
}

pub(crate) fn parse_optional_date(s: &str) -> eyre::Result<Option<chrono::NaiveDate>> {
    if s == "clear" || s.is_empty() {
        Ok(None)
    } else {
        Ok(Some(s.parse::<chrono::NaiveDate>()?))
    }
}

// ── Nextcloud Talk ────────────────────────────────────────────────────────────

pub(crate) fn print_channel_rooms(rooms: &[ChannelConversation], json: bool) {
    if json {
        print_channel_rooms_json(rooms);
    } else {
        print_channel_rooms_table(rooms);
    }
}

pub(crate) fn print_asset_result(asset: &Asset, json: bool) {
    if json {
        println!("{}", facet_json::to_string(asset).unwrap_or_default());
    } else {
        println!("{}  {}  {:?}", asset.id, asset.name, asset.status);
        if let Some(location) = &asset.location {
            println!("  location: {}", location.0);
        }
        if let Some(space) = &asset.space {
            println!("  space: {}", space.0);
        }
        if !asset.reservations.is_empty() {
            println!("  reservations: {}", asset.reservations.len());
        }
    }
}

pub(crate) fn print_assets(assets: &[Asset], json: bool) {
    if json {
        println!("{}", facet_json::to_string(assets).unwrap_or_default());
        return;
    }
    if assets.is_empty() {
        println!("No assets.");
        return;
    }
    println!(
        "{:<16}  {:<28}  {:<16}  {:<18}  LOCATION",
        "ID", "NAME", "STATUS", "CATEGORY"
    );
    println!("{}", "-".repeat(94));
    for asset in assets {
        println!(
            "{:<16}  {:<28}  {:<16}  {:<18}  {}",
            truncate(&asset.id, 16),
            truncate(&asset.name, 28),
            truncate(&format!("{:?}", asset.status), 16),
            truncate(asset.category.as_deref().unwrap_or("-"), 18),
            asset
                .location
                .as_ref()
                .map(|link| link.0.as_str())
                .unwrap_or("-")
        );
    }
}

pub(crate) fn print_asset_conflicts(conflicts: &[AssetConflict], json: bool) {
    if json {
        println!("{}", facet_json::to_string(conflicts).unwrap_or_default());
        return;
    }
    if conflicts.is_empty() {
        println!("No asset conflicts.");
        return;
    }
    println!(
        "{:<16}  {:<28}  {:<36}  REFERENCE",
        "ASSET", "NAME", "REASON"
    );
    println!("{}", "-".repeat(102));
    for conflict in conflicts {
        let reference = conflict
            .reservation
            .as_ref()
            .map(|reservation| reservation.reference.0.as_str())
            .unwrap_or("-");
        println!(
            "{:<16}  {:<28}  {:<36}  {}",
            truncate(&conflict.asset_id, 16),
            truncate(&conflict.asset_name, 28),
            truncate(&conflict.reason, 36),
            reference
        );
    }
}

pub(crate) fn print_channel_rooms_table(rooms: &[ChannelConversation]) {
    if rooms.is_empty() {
        println!("No rooms.");
        return;
    }
    let name_w = rooms
        .iter()
        .map(|r| r.name.len())
        .max()
        .unwrap_or(10)
        .clamp(10, 40);
    println!(
        "{:<name_w$}  {:<22}  {:>7}  {:<12}  ID",
        "NAME", "LAST ACTIVITY (UTC)", "PEOPLE", "KIND",
    );
    println!("{}", "─".repeat(name_w + 55));
    for r in rooms {
        let when = if let Some(timestamp) = r.last_activity {
            chrono::DateTime::<chrono::Utc>::from_timestamp(timestamp, 0)
                .map(|d| d.format("%Y-%m-%d %H:%M:%S").to_string())
                .unwrap_or_else(|| "—".into())
        } else {
            "—".into()
        };
        println!(
            "{:<name_w$}  {:<22}  {:>7}  {:<5}  {}",
            truncate(&r.name, name_w),
            when,
            r.participant_count,
            r.kind,
            r.id,
        );
    }
    println!("\n{} room(s)", rooms.len());
}

pub(crate) fn print_channel_rooms_json(rooms: &[ChannelConversation]) {
    println!("[");
    for (i, r) in rooms.iter().enumerate() {
        let comma = if i + 1 < rooms.len() { "," } else { "" };
        println!(
            "  {{\"provider\":\"{}\",\"account\":{},\"id\":\"{}\",\"name\":\"{}\",\"kind\":\"{}\",\"participants\":{},\"last_activity\":{},\"last_message\":{}}}{comma}",
            escape_json(&r.provider),
            opt_json(r.account.as_deref()),
            escape_json(&r.id),
            escape_json(&r.name),
            escape_json(&r.kind),
            r.participant_count,
            r.last_activity
                .map(|n| n.to_string())
                .unwrap_or_else(|| "null".into()),
            opt_json(r.last_message.as_deref()),
        );
    }
    println!("]");
}

pub(crate) fn print_channel_history(msgs: &[ChannelMessage], json: bool) {
    if json {
        print_channel_history_json(msgs);
    } else {
        print_channel_history_table(msgs);
    }
}

pub(crate) fn print_channel_history_table(msgs: &[ChannelMessage]) {
    if msgs.is_empty() {
        println!("No messages.");
        return;
    }
    let mut list: Vec<&ChannelMessage> = msgs.iter().collect();
    list.sort_by_key(|m| m.timestamp);
    for m in list {
        let when = chrono::DateTime::<chrono::Utc>::from_timestamp(m.timestamp, 0)
            .map(|d| d.format("%H:%M:%S").to_string())
            .unwrap_or_else(|| "—".into());
        let reply = match &m.reply_to {
            Some(id) => format!(" ↪#{id}"),
            None => String::new(),
        };
        println!("[{when}] @{} (#{}{}): {}", m.actor_id, m.id, reply, m.body);
    }
}

pub(crate) fn print_channel_history_json(msgs: &[ChannelMessage]) {
    println!("[");
    for (i, m) in msgs.iter().enumerate() {
        let comma = if i + 1 < msgs.len() { "," } else { "" };
        println!(
            "  {{\"provider\":\"{}\",\"account\":{},\"conversation_id\":\"{}\",\"id\":\"{}\",\"actor_id\":\"{}\",\"actor_type\":\"{}\",\"actor_display_name\":\"{}\",\"timestamp\":{},\"body\":\"{}\",\"reply_to\":{}}}{comma}",
            escape_json(&m.provider),
            opt_json(m.account.as_deref()),
            escape_json(&m.conversation_id),
            escape_json(&m.id),
            escape_json(&m.actor_id),
            escape_json(&m.actor_type),
            escape_json(&m.actor_display_name),
            m.timestamp,
            escape_json(&m.body),
            opt_json(m.reply_to.as_deref()),
        );
    }
    println!("]");
}

// ── Time helpers ──────────────────────────────────────────────────────────────

/// Parse a datetime in a few tolerant shapes. Returns UTC.
/// Accepted: RFC3339 ("2026-04-17T09:30:00Z"), naive UTC ("2026-04-17T09:30"),
/// and "YYYY-MM-DD HH:MM" (also naive UTC).
pub(crate) fn parse_datetime(s: &str) -> eyre::Result<DateTime<Utc>> {
    if let Ok(dt) = DateTime::parse_from_rfc3339(s) {
        return Ok(dt.with_timezone(&Utc));
    }
    let naive = chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M:%S")
        .or_else(|_| chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M"))
        .or_else(|_| chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%d %H:%M:%S"))
        .or_else(|_| chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%d %H:%M"))
        .map_err(|_| eyre::eyre!("Invalid datetime: {s}"))?;
    Ok(Utc.from_utc_datetime(&naive))
}

pub(crate) fn parse_calendar_boundary_start(s: &str) -> eyre::Result<DateTime<Utc>> {
    parse_datetime(s).or_else(|_| parse_date_start(s))
}

pub(crate) fn parse_calendar_boundary_end(s: &str) -> eyre::Result<DateTime<Utc>> {
    parse_datetime(s).or_else(|_| parse_date_end(s))
}

pub(crate) fn parse_calendar_status(s: &str) -> eyre::Result<CalendarEventStatus> {
    match s.to_lowercase().as_str() {
        "confirmed" | "confirm" => Ok(CalendarEventStatus::Confirmed),
        "tentative" => Ok(CalendarEventStatus::Tentative),
        "cancelled" | "canceled" => Ok(CalendarEventStatus::Cancelled),
        _ => eyre::bail!("Unknown calendar status: {s}"),
    }
}

pub(crate) fn optional_string_field(s: String) -> Option<String> {
    if s == "clear" || s.is_empty() {
        None
    } else {
        Some(s)
    }
}

pub(crate) fn parse_date_start(s: &str) -> eyre::Result<DateTime<Utc>> {
    let d = s
        .parse::<chrono::NaiveDate>()
        .map_err(|_| eyre::eyre!("Invalid date: {s}"))?;
    Ok(Utc.from_utc_datetime(&d.and_hms_opt(0, 0, 0).unwrap()))
}

pub(crate) fn parse_date_end(s: &str) -> eyre::Result<DateTime<Utc>> {
    let d = s
        .parse::<chrono::NaiveDate>()
        .map_err(|_| eyre::eyre!("Invalid date: {s}"))?;
    Ok(Utc.from_utc_datetime(&d.and_hms_opt(23, 59, 59).unwrap()))
}

pub(crate) fn escape_json(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch.is_control() => out.push_str(&format!("\\u{:04x}", ch as u32)),
            ch => out.push(ch),
        }
    }
    out
}

pub(crate) fn print_time_entries_table(entries: &[task_core::TimeEntryContext]) {
    if entries.is_empty() {
        println!("No time entries.");
        return;
    }
    let title_w = entries
        .iter()
        .map(|c| c.task_title.len())
        .max()
        .unwrap_or(10)
        .clamp(5, 35);
    println!(
        "{:<title_w$}  {:<19}  {:>6}  {:<8}  {:<12}  {:<18}  ID",
        "TASK", "START (UTC)", "MIN", "BILLABLE", "USER", "PROJECTS",
    );
    println!("{}", "─".repeat(title_w + 75));
    for ctx in entries {
        let e = &ctx.entry;
        let t = truncate(&ctx.task_title, title_w);
        let start = e.start_time.format("%Y-%m-%d %H:%M:%S").to_string();
        let mins = if e.is_running() {
            format!("▶{}", e.elapsed_minutes(Utc::now()))
        } else {
            e.duration_minutes().to_string()
        };
        let billable = if e.billable { "yes" } else { "no" };
        let user = e.user.clone().unwrap_or_else(|| "—".into());
        let projects = if ctx.task_projects.is_empty() {
            "—".into()
        } else {
            ctx.task_projects.join(",")
        };
        println!(
            "{:<title_w$}  {:<19}  {:>6}  {:<8}  {:<12}  {:<18}  {}",
            t,
            start,
            mins,
            billable,
            user,
            truncate(&projects, 18),
            e.id
        );
    }
    println!(
        "\n{} entr{}",
        entries.len(),
        if entries.len() == 1 { "y" } else { "ies" }
    );
}

pub(crate) fn print_time_entries_json(entries: &[task_core::TimeEntryContext]) {
    println!("{}", time_entries_json(entries));
}

pub(crate) fn time_entries_json(entries: &[TimeEntryContext]) -> String {
    let items = entries
        .iter()
        .map(|ctx| {
            let entry_json = facet_json::to_string(&ctx.entry).unwrap_or_default();
            let projects_json = ctx
                .task_projects
                .iter()
                .map(|p| format!("\"{}\"", escape_json(p)))
                .collect::<Vec<_>>()
                .join(",");
            format!(
                "{{\"task\":\"{}\",\"projects\":[{}],\"client\":{},\"entry\":{}}}",
                escape_json(&ctx.task_title),
                projects_json,
                opt_json(ctx.client_name.as_deref()),
                entry_json
            )
        })
        .collect::<Vec<_>>();
    format!("[{}]", items.join(","))
}

pub(crate) fn print_time_entries_csv(entries: &[task_core::TimeEntryContext]) {
    println!(
        "entry_id,task,projects,client,user,start,end,minutes,billable,rate_cents,billable_amount_cents,tags,description,invoiced_at,invoice_ninja_invoice_id"
    );
    for ctx in entries {
        let e = &ctx.entry;
        let end = e
            .end_time
            .map(|t| t.format("%Y-%m-%dT%H:%M:%SZ").to_string())
            .unwrap_or_default();
        let mins = e.duration_minutes();
        let resolved_rate = ctx.effective_rate(None);
        let amount = if e.billable {
            e.amount_cents(resolved_rate)
        } else {
            0
        };
        let invoiced_at = e
            .invoiced_at
            .map(|t| t.format("%Y-%m-%dT%H:%M:%SZ").to_string())
            .unwrap_or_default();
        let row = [
            e.id.as_str(),
            ctx.task_title.as_str(),
            &ctx.task_projects.join(";"),
            ctx.client_name.as_deref().unwrap_or(""),
            e.user.as_deref().unwrap_or(""),
            &e.start_time.format("%Y-%m-%dT%H:%M:%SZ").to_string(),
            &end,
            &mins.to_string(),
            if e.billable { "true" } else { "false" },
            &resolved_rate.to_string(),
            &amount.to_string(),
            &e.tags.join(";"),
            e.description.as_deref().unwrap_or(""),
            &invoiced_at,
            e.invoice_ninja_invoice_id.as_deref().unwrap_or(""),
        ];
        println!(
            "{}",
            row.iter()
                .map(|f| csv_escape(f))
                .collect::<Vec<_>>()
                .join(",")
        );
    }
}

// ── Output format helper ─────────────────────────────────────────────────────

#[derive(Copy, Clone, Debug)]
pub(crate) enum OutputFormat {
    Table,
    Json,
    Csv,
}

pub(crate) fn pick_format(s: &str, json_alias: bool) -> OutputFormat {
    if json_alias {
        return OutputFormat::Json;
    }
    match s.to_lowercase().as_str() {
        "json" => OutputFormat::Json,
        "csv" => OutputFormat::Csv,
        _ => OutputFormat::Table,
    }
}

pub(crate) fn csv_escape(s: &str) -> String {
    if s.chars().any(|c| matches!(c, ',' | '"' | '\n' | '\r')) {
        format!("\"{}\"", s.replace('"', "\"\""))
    } else {
        s.to_string()
    }
}

/// ({group_key}, total_minutes, billable_cents, entry_count)
type ReportRow = (String, u64, u64, usize);

pub(crate) fn aggregate_time(
    entries: &[task_core::TimeEntryContext],
    group_by: &str,
    fallback_rate: Option<u32>,
) -> eyre::Result<Vec<ReportRow>> {
    use std::collections::BTreeMap;
    let mut acc: BTreeMap<String, (u64, u64, usize)> = BTreeMap::new();

    // Helper to bump a slot by minutes and billable cents.
    let bump =
        |acc: &mut BTreeMap<String, (u64, u64, usize)>, key: String, mins: u64, cents: u64| {
            let slot = acc.entry(key).or_insert((0, 0, 0));
            slot.0 += mins;
            slot.1 += cents;
            slot.2 += 1;
        };

    for ctx in entries {
        let e = &ctx.entry;
        let mins = e.duration_minutes() as u64;
        let cents = if e.billable {
            // Full cascade: entry → project → client → --rate fallback.
            let rate = ctx.effective_rate(fallback_rate);
            (mins * rate as u64) / 60
        } else {
            0
        };

        match group_by {
            "task" => bump(&mut acc, ctx.task_title.clone(), mins, cents),
            "user" => bump(
                &mut acc,
                e.user.clone().unwrap_or_else(|| "—".into()),
                mins,
                cents,
            ),
            "client" => bump(
                &mut acc,
                ctx.client_name.clone().unwrap_or_else(|| "—".into()),
                mins,
                cents,
            ),
            "project" => {
                if ctx.task_projects.is_empty() {
                    bump(&mut acc, "—".into(), mins, cents);
                } else {
                    // Entry counted once per project it belongs to. Minutes
                    // and billable are split equally across projects so the
                    // total doesn't double-count.
                    let n = ctx.task_projects.len() as u64;
                    let per_project_min = mins / n;
                    let per_project_cents = cents / n;
                    for p in &ctx.task_projects {
                        bump(&mut acc, p.clone(), per_project_min, per_project_cents);
                    }
                }
            }
            "tag" => {
                if e.tags.is_empty() {
                    bump(&mut acc, "—".into(), mins, cents);
                } else {
                    // Tags split the same way to avoid double-counting totals.
                    let n = e.tags.len() as u64;
                    let per_tag_min = mins / n;
                    let per_tag_cents = cents / n;
                    for t in &e.tags {
                        bump(&mut acc, t.clone(), per_tag_min, per_tag_cents);
                    }
                }
            }
            other => eyre::bail!(
                "Unknown group_by: {other}. Use 'task', 'user', 'project', 'client', or 'tag'."
            ),
        }
    }

    let mut rows: Vec<ReportRow> = acc.into_iter().map(|(k, (m, c, n))| (k, m, c, n)).collect();
    rows.sort_by(|a, b| b.1.cmp(&a.1));
    Ok(rows)
}

pub(crate) fn print_report_csv(rows: &[ReportRow], group_by: &str) {
    println!(
        "{},minutes,hours,billable_cents,billable_dollars,entries",
        group_by
    );
    for (k, mins, cents, count) in rows {
        let hours = format!("{:.2}", *mins as f64 / 60.0);
        let dollars = format!("{:.2}", *cents as f64 / 100.0);
        println!(
            "{},{},{},{},{},{}",
            csv_escape(k),
            mins,
            hours,
            cents,
            dollars,
            count
        );
    }
}

pub(crate) fn print_report_table(rows: &[ReportRow]) {
    if rows.is_empty() {
        println!("No entries in range.");
        return;
    }
    let key_w = rows
        .iter()
        .map(|r| r.0.len())
        .max()
        .unwrap_or(5)
        .clamp(5, 40);
    println!(
        "{:<key_w$}  {:>8}  {:>10}  {:>7}",
        "GROUP", "HOURS", "BILLABLE", "COUNT",
    );
    println!("{}", "─".repeat(key_w + 32));
    let mut total_min: u64 = 0;
    let mut total_cents: u64 = 0;
    for (k, mins, cents, count) in rows {
        let hours = format!("{:.2}", *mins as f64 / 60.0);
        let billable = format!("${:.2}", *cents as f64 / 100.0);
        println!(
            "{:<key_w$}  {:>8}  {:>10}  {:>7}",
            truncate(k, key_w),
            hours,
            billable,
            count
        );
        total_min += mins;
        total_cents += cents;
    }
    println!("{}", "─".repeat(key_w + 32));
    println!(
        "{:<key_w$}  {:>8}  {:>10}",
        "TOTAL",
        format!("{:.2}", total_min as f64 / 60.0),
        format!("${:.2}", total_cents as f64 / 100.0)
    );
}

// ── Activity / conflicts ──────────────────────────────────────────────────────

pub(crate) fn print_activity_table(rows: &[ChangeRow]) {
    if rows.is_empty() {
        println!("No activity.");
        return;
    }
    let id_w = rows
        .iter()
        .map(|r| r.entity_id.len())
        .max()
        .unwrap_or(5)
        .clamp(5, 35);
    println!(
        "{:<19}  {:<6}  {:<id_w$}  {:<12}  {:<10}  CHANGE",
        "WHEN (UTC)", "KIND", "ENTITY", "FIELD", "BY",
    );
    println!("{}", "─".repeat(id_w + 62));
    for r in rows {
        let field = r.field.clone().unwrap_or_else(|| "—".into());
        let by = r.changed_by.clone().unwrap_or_else(|| "—".into());
        let change = match (&r.old_value, &r.new_value) {
            (Some(o), Some(n)) => format!("{o} → {n}"),
            (_, Some(n)) => n.clone(),
            (Some(o), _) => format!("{o} → ∅"),
            _ => "—".into(),
        };
        println!(
            "{:<19}  {:<6}  {:<id_w$}  {:<12}  {:<10}  {}",
            r.changed_at,
            r.entity_type,
            truncate(&r.entity_id, id_w),
            truncate(&field, 12),
            truncate(&by, 10),
            truncate(&change, 60),
        );
    }
    println!("\n{} change(s)", rows.len());
}

pub(crate) fn print_activity_json(rows: &[ChangeRow]) {
    println!("[");
    for (i, r) in rows.iter().enumerate() {
        let comma = if i + 1 < rows.len() { "," } else { "" };
        println!(
            "  {{\"entity_type\":\"{}\",\"entity_id\":\"{}\",\"field\":{},\"old\":{},\"new\":{},\"by\":{},\"at\":\"{}\"}}{comma}",
            escape_json(&r.entity_type),
            escape_json(&r.entity_id),
            opt_json(r.field.as_deref()),
            opt_json(r.old_value.as_deref()),
            opt_json(r.new_value.as_deref()),
            opt_json(r.changed_by.as_deref()),
            escape_json(&r.changed_at),
        );
    }
    println!("]");
}

pub(crate) fn print_conflicts_table(rows: &[ConflictRow]) {
    if rows.is_empty() {
        println!("No conflicts.");
        return;
    }
    println!(
        "{:<4}  {:<12}  {:<20}  {:<10}  {:<20}  {:<20}  STATE",
        "ID", "FIELD", "ENTITY", "KIND", "WINNING", "LOSING",
    );
    println!("{}", "─".repeat(110));
    for r in rows {
        let field = r.field.clone().unwrap_or_else(|| "—".into());
        let winning = r.winning_value.clone().unwrap_or_else(|| "∅".into());
        let losing = r.losing_value.clone().unwrap_or_else(|| "∅".into());
        let kind = r.kind.clone().unwrap_or_else(|| "—".into());
        let state = r.resolved.clone().unwrap_or_else(|| "open".into());
        let w_actor = r
            .winning_actor
            .as_deref()
            .map(|a| format!("@{a}"))
            .unwrap_or_default();
        let l_actor = r
            .losing_actor
            .as_deref()
            .map(|a| format!("@{a}"))
            .unwrap_or_default();
        println!(
            "{:<4}  {:<12}  {:<20}  {:<10}  {:<20}  {:<20}  {state}",
            r.id,
            truncate(&field, 12),
            truncate(&r.entity_id, 20),
            truncate(&kind, 10),
            truncate(&format!("{winning} {w_actor}"), 20),
            truncate(&format!("{losing} {l_actor}"), 20),
        );
    }
    println!("\n{} conflict(s)", rows.len());
}

pub(crate) fn print_conflicts_json(rows: &[ConflictRow]) {
    println!("[");
    for (i, r) in rows.iter().enumerate() {
        let comma = if i + 1 < rows.len() { "," } else { "" };
        println!(
            "  {{\"id\":{},\"entity_type\":\"{}\",\"entity_id\":\"{}\",\"field\":{},\"kind\":{},\"winning_value\":{},\"losing_value\":{},\"winning_actor\":{},\"losing_actor\":{},\"resolved\":{},\"resolved_by\":{},\"at\":\"{}\"}}{comma}",
            r.id,
            escape_json(&r.entity_type),
            escape_json(&r.entity_id),
            opt_json(r.field.as_deref()),
            opt_json(r.kind.as_deref()),
            opt_json(r.winning_value.as_deref()),
            opt_json(r.losing_value.as_deref()),
            opt_json(r.winning_actor.as_deref()),
            opt_json(r.losing_actor.as_deref()),
            opt_json(r.resolved.as_deref()),
            opt_json(r.resolved_by.as_deref()),
            escape_json(&r.changed_at),
        );
    }
    println!("]");
}

pub(crate) fn opt_json(s: Option<&str>) -> String {
    match s {
        Some(v) => format!("\"{}\"", escape_json(v)),
        None => "null".into(),
    }
}

pub(crate) fn print_report_json(rows: &[ReportRow]) {
    println!("[");
    for (i, (k, mins, cents, count)) in rows.iter().enumerate() {
        let comma = if i + 1 < rows.len() { "," } else { "" };
        println!(
            "  {{\"group\":\"{}\",\"minutes\":{mins},\"billable_cents\":{cents},\"entries\":{count}}}{comma}",
            escape_json(k)
        );
    }
    println!("]");
}

// ── Calendar output ─────────────────────────────────────────────────────────

pub(crate) fn print_calendar_events_table(events: &[CalendarEvent]) {
    if events.is_empty() {
        println!("No calendar events.");
        return;
    }
    let title_w = events
        .iter()
        .map(|e| e.title.len())
        .max()
        .unwrap_or(10)
        .clamp(10, 36);
    println!(
        "{:<title_w$}  {:<19}  {:<19}  {:<10}  ID",
        "TITLE", "START (UTC)", "END (UTC)", "STATUS",
    );
    println!("{}", "-".repeat(title_w + 66));
    for event in events {
        let end = event
            .end
            .map(|d| d.format("%Y-%m-%d %H:%M:%S").to_string())
            .unwrap_or_else(|| "-".into());
        println!(
            "{:<title_w$}  {:<19}  {:<19}  {:<10}  {}",
            truncate(&event.title, title_w),
            event.start.format("%Y-%m-%d %H:%M:%S"),
            end,
            calendar_status_label(&event.status),
            event.id.as_deref().unwrap_or("-"),
        );
    }
    println!("\n{} event(s)", events.len());
}

pub(crate) fn print_calendar_event_detail(event: &CalendarEvent) {
    println!("Title:    {}", event.title);
    println!("Status:   {}", calendar_status_label(&event.status));
    println!("Start:    {}", event.start.to_rfc3339());
    if let Some(end) = event.end {
        println!("End:      {}", end.to_rfc3339());
    }
    if let Some(location) = &event.location {
        println!("Location: {location}");
    }
    if let Some(description) = &event.description {
        println!("Desc:     {description}");
    }
    if !event.attendees.is_empty() {
        println!("Attendees: {}", event.attendees.join(", "));
    }
    if let Some(recurrence) = &event.recurrence {
        println!("Recurs:   {recurrence}");
    }
    if let Some(id) = &event.id {
        println!("ID:       {id}");
    }
}

pub(crate) fn print_calendar_events_json(events: &[CalendarEvent]) {
    println!("{}", calendar_events_json(events));
}

pub(crate) fn calendar_events_json(events: &[CalendarEvent]) -> String {
    let items = events
        .iter()
        .map(|event| facet_json::to_string(event).unwrap_or_default())
        .collect::<Vec<_>>();
    format!("[{}]", items.join(","))
}

pub(crate) fn calendar_status_label(status: &CalendarEventStatus) -> &'static str {
    match status {
        CalendarEventStatus::Confirmed => "confirmed",
        CalendarEventStatus::Tentative => "tentative",
        CalendarEventStatus::Cancelled => "cancelled",
    }
}

// ── Agent output ────────────────────────────────────────────────────────────

pub(crate) struct AgentSnapshot<'a> {
    source: &'a str,
    location: &'a str,
    actor: Option<&'a str>,
    tasks: &'a [Task],
    projects: &'a [Project],
    clients: &'a [Client],
    invoices: &'a [Invoice],
    calendar_events: &'a [CalendarEvent],
    time_entries: &'a [TimeEntryContext],
    active_timer: Option<AgentActiveTimer<'a>>,
    activity: &'a [ChangeRow],
    conflicts: &'a [ConflictRow],
    sync_status: Option<&'a SyncStats>,
}

pub(crate) struct AgentActiveTimer<'a> {
    title: &'a str,
    entry: &'a task_core::TimeEntry,
}

pub(crate) fn print_agent_snapshot(snapshot: AgentSnapshot<'_>) {
    println!(
        "{{\"generated_at\":\"{}\",\"source\":\"{}\",\"location\":\"{}\",\"actor\":{},\"install\":{},\"tasks\":{},\"projects\":{},\"clients\":{},\"invoices\":{},\"calendar_events\":{},\"time_entries\":{},\"active_timer\":{},\"activity\":{},\"conflicts\":{},\"sync_status\":{}}}",
        Utc::now().to_rfc3339(),
        escape_json(snapshot.source),
        escape_json(snapshot.location),
        opt_json(snapshot.actor),
        agent_install_json(),
        tasks_json(snapshot.tasks),
        projects_json(snapshot.projects),
        clients_json(snapshot.clients),
        invoices_json(snapshot.invoices),
        calendar_events_json(snapshot.calendar_events),
        time_entries_json(snapshot.time_entries),
        active_timer_json(snapshot.active_timer),
        activity_json(snapshot.activity),
        conflicts_json(snapshot.conflicts),
        snapshot
            .sync_status
            .map(|s| facet_json::to_string(s).unwrap_or_default())
            .unwrap_or_else(|| "null".into()),
    );
}

pub(crate) fn print_agent_capabilities() {
    println!(
        "{{\"binary\":\"task\",\"package\":\"task-cli\",\"install\":{},\"global_flags\":[\"--vault\",\"--server\",\"--session-token\",\"--organization-id\",\"--as-user\"],\"agent_commands\":[\"snapshot\",\"task\",\"plan\",\"project\",\"calendar\",\"time\",\"sync\",\"capabilities\",\"bootstrap\"],\"control_commands\":[\"doctor\",\"doctor --deep\",\"server add\",\"server list\",\"server use\",\"capture\",\"inbox list\",\"inbox promote\",\"add\",\"update\",\"complete\",\"delete\",\"calendar add\",\"calendar update\",\"calendar delete\",\"email accounts\",\"email search\",\"email show\",\"email link\",\"email sweep\",\"time log\",\"time edit\",\"start\",\"stop\",\"sync\"],\"remote_mode\":\"Set --server plus --session-token to route supported inbox, task, project, client, invoice, time, calendar, email, activity, conflict, system, and agent commands over Vox; --organization-id routes multi-instance organization requests.\"}}",
        agent_install_json()
    );
}

pub(crate) fn print_doctor_json(capabilities: &SystemCapabilities, health: &SystemHealth) {
    println!(
        "{{\"capabilities\":{},\"health\":{}}}",
        facet_json::to_string(capabilities).unwrap_or_default(),
        facet_json::to_string(health).unwrap_or_default()
    );
}

pub(crate) fn doctor_check_status(check: &task_core::HealthCheck) -> &str {
    if check.ok {
        "ok"
    } else if check.severity == "warning" {
        "warning"
    } else {
        "failed"
    }
}

pub(crate) fn print_doctor_report(
    mode: &str,
    remote_url: Option<&str>,
    capabilities: &SystemCapabilities,
    health: &SystemHealth,
) {
    println!(
        "Task doctor: {}",
        if !health.ok {
            "attention needed"
        } else if health.degraded {
            "degraded (usable with warnings)"
        } else {
            "ok"
        }
    );
    println!("  mode: {mode}");
    println!("  deep checks: {}", if health.deep { "yes" } else { "no" });
    if let Some(url) = remote_url {
        println!("  server: {url}");
    }
    println!(
        "  package: {} {}",
        capabilities.package, capabilities.version
    );
    if !capabilities.vault.root.is_empty() {
        println!("  vault: {}", capabilities.vault.root);
    }
    if !capabilities.services.is_empty() {
        println!("  services: {}", capabilities.services.join(", "));
    }
    if capabilities.nextcloud.configured {
        println!(
            "  nextcloud: {} ({})",
            capabilities
                .nextcloud
                .url
                .as_deref()
                .unwrap_or("configured"),
            capabilities
                .nextcloud
                .username
                .as_deref()
                .unwrap_or("unknown user")
        );
    } else {
        println!("  nextcloud: not configured");
    }
    println!();
    println!(
        "  protocol: {} (min cli {}, min server {})",
        capabilities.protocol_version,
        capabilities.min_cli_version,
        capabilities.min_server_version
    );
    println!();
    println!(
        "{:<20} {:<12} {:<8} {:<28} DETAIL",
        "CHECK", "STATUS", "CONFIG", "CODE"
    );
    println!("{}", "-".repeat(116));
    for check in &health.checks {
        println!(
            "{:<20} {:<12} {:<8} {:<28} {}",
            check.name,
            doctor_check_status(check),
            if check.configured { "yes" } else { "no" },
            check.code,
            check.detail
        );
        if !check.ok {
            if let Some(hint) = &check.hint {
                println!("{:<20} {:<12} {:<8} {:<28} hint: {}", "", "", "", "", hint);
            }
        }
    }
}

pub(crate) fn print_agent_bootstrap(
    remote: Option<&RemoteVoxConfig>,
    capabilities: Option<&SystemCapabilities>,
    json: bool,
) {
    let server = remote.map(|remote| remote.display_url.as_str());
    let profile = remote.and_then(|remote| remote.profile_name.as_deref());
    let protocol = capabilities.map(|c| c.protocol_version).unwrap_or(1);
    if json {
        println!(
            "{{\"binary\":\"task\",\"install\":{},\"profile\":{},\"server\":{},\"protocol_version\":{},\"commands\":{{\"doctor\":\"task doctor --json\",\"deep_doctor\":\"task doctor --deep --json\",\"capabilities\":\"task agent capabilities\",\"snapshot\":\"task agent snapshot\"}}}}",
            agent_install_json(),
            profile
                .map(|p| format!("\"{}\"", escape_json(p)))
                .unwrap_or_else(|| "null".into()),
            server
                .map(|s| format!("\"{}\"", escape_json(s)))
                .unwrap_or_else(|| "null".into()),
            protocol
        );
    } else {
        println!("Agent bootstrap");
        println!("  install: nix profile install .#task-cli");
        if let Some(profile) = profile {
            println!("  profile: {profile}");
        }
        if let Some(server) = server {
            println!("  server: {server}");
        }
        println!("  verify: task doctor --json");
        println!("  deep verify: task doctor --deep --json");
        println!("  snapshot: task agent snapshot");
    }
}

pub(crate) fn agent_install_json() -> String {
    "{\"nix\":\"nix profile install .#task-cli\",\"build\":\"nix build .#task-cli\",\"cargo\":\"cargo install --path crates/task-cli\"}".into()
}

pub(crate) fn tasks_json(tasks: &[Task]) -> String {
    format!(
        "[{}]",
        tasks
            .iter()
            .map(|task| facet_json::to_string(task).unwrap_or_default())
            .collect::<Vec<_>>()
            .join(",")
    )
}

pub(crate) fn projects_json(projects: &[Project]) -> String {
    format!(
        "[{}]",
        projects
            .iter()
            .map(|project| facet_json::to_string(project).unwrap_or_default())
            .collect::<Vec<_>>()
            .join(",")
    )
}

pub(crate) fn clients_json(clients: &[Client]) -> String {
    format!(
        "[{}]",
        clients
            .iter()
            .map(|client| facet_json::to_string(client).unwrap_or_default())
            .collect::<Vec<_>>()
            .join(",")
    )
}

pub(crate) fn invoices_json(invoices: &[Invoice]) -> String {
    format!(
        "[{}]",
        invoices
            .iter()
            .map(|invoice| facet_json::to_string(invoice).unwrap_or_default())
            .collect::<Vec<_>>()
            .join(",")
    )
}

pub(crate) fn active_timer_json(active: Option<AgentActiveTimer<'_>>) -> String {
    match active {
        Some(active) => format!(
            "{{\"task\":\"{}\",\"entry\":{}}}",
            escape_json(active.title),
            facet_json::to_string(active.entry).unwrap_or_default()
        ),
        None => "null".into(),
    }
}

pub(crate) fn activity_json(rows: &[ChangeRow]) -> String {
    let items = rows
        .iter()
        .map(|r| {
            format!(
                "{{\"entity_type\":\"{}\",\"entity_id\":\"{}\",\"field\":{},\"old\":{},\"new\":{},\"by\":{},\"at\":\"{}\"}}",
                escape_json(&r.entity_type),
                escape_json(&r.entity_id),
                opt_json(r.field.as_deref()),
                opt_json(r.old_value.as_deref()),
                opt_json(r.new_value.as_deref()),
                opt_json(r.changed_by.as_deref()),
                escape_json(&r.changed_at),
            )
        })
        .collect::<Vec<_>>();
    format!("[{}]", items.join(","))
}

pub(crate) fn conflicts_json(rows: &[ConflictRow]) -> String {
    let items = rows
        .iter()
        .map(|r| {
            format!(
                "{{\"id\":{},\"entity_type\":\"{}\",\"entity_id\":\"{}\",\"field\":{},\"kind\":{},\"winning_value\":{},\"losing_value\":{},\"winning_actor\":{},\"losing_actor\":{},\"resolved\":{},\"resolved_by\":{},\"at\":\"{}\"}}",
                r.id,
                escape_json(&r.entity_type),
                escape_json(&r.entity_id),
                opt_json(r.field.as_deref()),
                opt_json(r.kind.as_deref()),
                opt_json(r.winning_value.as_deref()),
                opt_json(r.losing_value.as_deref()),
                opt_json(r.winning_actor.as_deref()),
                opt_json(r.losing_actor.as_deref()),
                opt_json(r.resolved.as_deref()),
                opt_json(r.resolved_by.as_deref()),
                escape_json(&r.changed_at),
            )
        })
        .collect::<Vec<_>>();
    format!("[{}]", items.join(","))
}

// ── Parsing helpers ───────────────────────────────────────────────────────────

pub(crate) fn parse_status(s: &str) -> Option<Status> {
    match s.to_lowercase().replace('-', "").as_str() {
        "none" => Some(Status::None),
        "open" => Some(Status::Open),
        "inprogress" => Some(Status::InProgress),
        "onhold" => Some(Status::OnHold),
        "planned" => Some(Status::Planned),
        "done" => Some(Status::Done),
        "cancelled" | "canceled" => Some(Status::Cancelled),
        "archived" => Some(Status::Archived),
        _ => None,
    }
}

pub(crate) fn parse_project_status(s: &str) -> Option<task_core::project::ProjectStatus> {
    match s.to_lowercase().replace(['-', '_', ' '], "").as_str() {
        "planning" => Some(task_core::project::ProjectStatus::Planning),
        "active" => Some(task_core::project::ProjectStatus::Active),
        "onhold" | "hold" => Some(task_core::project::ProjectStatus::OnHold),
        "completed" | "done" => Some(task_core::project::ProjectStatus::Completed),
        "archived" => Some(task_core::project::ProjectStatus::Archived),
        _ => None,
    }
}

pub(crate) fn parse_priority(s: &str) -> Result<Priority, String> {
    match s.to_lowercase().as_str() {
        "none" => Ok(Priority::None),
        "low" => Ok(Priority::Low),
        "normal" | "medium" => Ok(Priority::Normal),
        "high" => Ok(Priority::High),
        "urgent" | "critical" => Ok(Priority::Urgent),
        _ => Err(format!(
            "Unknown priority: {s}. Use: none, low, normal, high, urgent"
        )),
    }
}

pub(crate) fn parse_sort(s: &str) -> Sort {
    match s.to_lowercase().as_str() {
        "priority" => Sort::Priority,
        "due" => Sort::Due,
        "scheduled" => Sort::Scheduled,
        "title" => Sort::Title,
        "status" => Sort::Status,
        "created" => Sort::DateCreated,
        "modified" => Sort::DateModified,
        _ => Sort::Urgency,
    }
}

pub(crate) fn print_project_detail(p: &task_core::Project) {
    println!("Title:       {}", p.title);
    println!("Status:      {:?}", p.status);
    if let Some(ref id) = p.identifier {
        println!("Identifier:  {id}");
    }
    if let Some(ref c) = p.client {
        println!("Client:      {}", c.0);
    }
    if let Some(r) = p.default_rate {
        println!("Rate:        ${:.2}/hr", r as f64 / 100.0);
    }
    if let Some(ref d) = p.description {
        println!("Description: {d}");
    }
    if let Some(ref a) = p.area {
        println!("Area:        {a}");
    }
    if let Some(ref o) = p.organization {
        println!("Org:         {o}");
    }
    if let Some(ref l) = p.lead {
        println!("Lead:        {l}");
    }
    if let Some(ref a) = p.default_assignee {
        println!("Assignee:    {a}");
    }
    if let Some(s) = p.start {
        println!("Start:       {s}");
    }
    if let Some(d) = p.due {
        println!("Due:         {d}");
    }
    if !p.tags.is_empty() {
        println!("Tags:        {}", p.tags.join(", "));
    }
    if !p.email_tags.is_empty() {
        println!("Email tags:  {}", p.email_tags.join(", "));
    }
    if !p.team.is_empty() {
        println!("Team:        {}", p.team.join(", "));
    }
    if let Some(ref r) = p.repo {
        println!("Repo:        {r}");
    }
    if !p.emails.is_empty() {
        println!("Emails:      {} linked", p.emails.len());
    }
}

pub(crate) fn print_project_context(context: Option<&ProjectKnowledgeContext>, json: bool) {
    let Some(context) = context else {
        if json {
            println!("null");
        } else {
            println!("Project not found.");
        }
        return;
    };
    if json {
        println!("{}", facet_json::to_string(context).unwrap_or_default());
        return;
    }

    print_project_detail(&context.project);
    println!("Path:        {}", context.project_path);
    println!("Tasks:       {}", context.tasks.len());
    if let Some(next) = &context.next_action {
        println!("Next:        {}", next.title);
    }
    if !context.references.is_empty() {
        println!("References:  {}", context.references.join(", "));
    }

    if !context.files.is_empty() {
        println!("\nFiles");
        for file in context.files.iter().take(40) {
            println!(
                "- {} [{}] {}",
                file.path,
                file.role,
                file.content_length
                    .map(|bytes| format!("{bytes} bytes"))
                    .unwrap_or_else(|| file.kind.clone())
            );
        }
    }
    if !context.decisions.is_empty() {
        println!("\nDecisions");
        for file in context.decisions.iter().take(20) {
            println!("- {}", file.path);
        }
    }
    if !context.deliverables.is_empty() {
        println!("\nDeliverables");
        for file in context.deliverables.iter().take(20) {
            println!("- {}", file.path);
        }
    }
}

pub(crate) fn print_mail_accounts_table(accounts: &[task_core::provider::MailAccount]) {
    if accounts.is_empty() {
        println!("No mail accounts configured.");
        return;
    }
    println!("{:<6}  {:<30}  NAME", "ID", "EMAIL");
    println!("{}", "─".repeat(60));
    for a in accounts {
        println!(
            "{:<6}  {:<30}  {}",
            a.id,
            truncate(&a.email, 30),
            a.name.as_deref().unwrap_or("—")
        );
    }
    println!("\n{} account(s)", accounts.len());
}

pub(crate) fn print_mail_accounts_json(accounts: &[task_core::provider::MailAccount]) {
    println!("[");
    for (i, a) in accounts.iter().enumerate() {
        let comma = if i + 1 < accounts.len() { "," } else { "" };
        println!(
            "  {{\"id\":{},\"email\":\"{}\",\"name\":{}}}{comma}",
            a.id,
            escape_json(&a.email),
            opt_json(a.name.as_deref()),
        );
    }
    println!("]");
}

pub(crate) fn print_mailboxes_table(boxes: &[task_core::provider::Mailbox]) {
    if boxes.is_empty() {
        println!("No mailboxes.");
        return;
    }
    println!("{:<6}  {:<6}  {:>6}  NAME", "ID", "ACCT", "UNREAD");
    println!("{}", "─".repeat(60));
    for m in boxes {
        println!(
            "{:<6}  {:<6}  {:>6}  {}",
            m.id,
            m.account_id,
            m.unread
                .map(|n| n.to_string())
                .unwrap_or_else(|| "—".into()),
            m.name,
        );
    }
    println!("\n{} mailbox(es)", boxes.len());
}

pub(crate) fn print_mailboxes_json(boxes: &[task_core::provider::Mailbox]) {
    println!("[");
    for (i, m) in boxes.iter().enumerate() {
        let comma = if i + 1 < boxes.len() { "," } else { "" };
        println!(
            "  {{\"id\":{},\"account_id\":{},\"name\":\"{}\",\"unread\":{}}}{comma}",
            m.id,
            m.account_id,
            escape_json(&m.name),
            m.unread
                .map(|n| n.to_string())
                .unwrap_or_else(|| "null".into()),
        );
    }
    println!("]");
}

pub(crate) fn print_mail_tags_table(tags: &[task_core::provider::MailTag]) {
    if tags.is_empty() {
        println!("No tags.");
        return;
    }
    println!(
        "{:<6}  {:<30}  {:<20}  COLOR",
        "ID", "DISPLAY NAME", "IMAP LABEL"
    );
    println!("{}", "─".repeat(80));
    for t in tags {
        println!(
            "{:<6}  {:<30}  {:<20}  {}",
            t.id,
            truncate(&t.display_name, 28),
            truncate(&t.imap_label, 18),
            t.color.as_deref().unwrap_or("—"),
        );
    }
    println!("\n{} tag(s)", tags.len());
}

pub(crate) fn print_mail_tags_json(tags: &[task_core::provider::MailTag]) {
    println!("[");
    for (i, t) in tags.iter().enumerate() {
        let comma = if i + 1 < tags.len() { "," } else { "" };
        let color = t
            .color
            .as_deref()
            .map(|c| format!("\"{}\"", escape_json(c)))
            .unwrap_or_else(|| "null".into());
        println!(
            "  {{\"id\":{},\"display_name\":\"{}\",\"imap_label\":\"{}\",\"color\":{}}}{comma}",
            t.id,
            escape_json(&t.display_name),
            escape_json(&t.imap_label),
            color,
        );
    }
    println!("]");
}

pub(crate) fn print_mail_messages_table(messages: &[task_core::provider::MailMessage]) {
    if messages.is_empty() {
        println!("No messages.");
        return;
    }
    println!("{:<8}  {:<19}  {:<25}  SUBJECT", "ID", "DATE", "FROM",);
    println!("{}", "─".repeat(90));
    for m in messages {
        let date = chrono::DateTime::<chrono::Utc>::from_timestamp(m.date, 0)
            .map(|d| d.format("%Y-%m-%d %H:%M:%S").to_string())
            .unwrap_or_else(|| "—".into());
        let atts = if m.has_attachments {
            format!(" 📎{}", m.attachment_count)
        } else {
            String::new()
        };
        println!(
            "{:<8}  {:<19}  {:<25}  {}{}",
            m.id,
            date,
            truncate(&m.from, 25),
            truncate(&m.subject, 50),
            atts,
        );
        if let Some(ref mid) = m.message_id {
            println!("          id: {mid}");
        }
    }
    println!("\n{} message(s)", messages.len());
}

pub(crate) fn print_mail_messages_json(messages: &[task_core::provider::MailMessage]) {
    println!("[");
    for (i, m) in messages.iter().enumerate() {
        let comma = if i + 1 < messages.len() { "," } else { "" };
        let to_json =
            m.to.iter()
                .map(|s| format!("\"{}\"", escape_json(s)))
                .collect::<Vec<_>>()
                .join(",");
        println!(
            "  {{\"id\":{},\"message_id\":{},\"subject\":\"{}\",\"from\":\"{}\",\"to\":[{}],\"date\":{},\"preview\":{},\"mailbox_id\":{},\"account_id\":{},\"imap_uid\":{},\"has_attachments\":{},\"attachment_count\":{}}}{comma}",
            m.id,
            opt_json(m.message_id.as_deref()),
            escape_json(&m.subject),
            escape_json(&m.from),
            to_json,
            m.date,
            opt_json(m.preview.as_deref()),
            m.mailbox_id,
            m.account_id
                .map(|n| n.to_string())
                .unwrap_or_else(|| "null".into()),
            m.imap_uid
                .map(|n| n.to_string())
                .unwrap_or_else(|| "null".into()),
            m.has_attachments,
            m.attachment_count,
        );
    }
    println!("]");
}

pub(crate) fn print_mail_detail(msg: &task_core::provider::MailMessageDetail, body: Option<&str>) {
    println!("ID:        {}", msg.id);
    if let Some(ref mid) = msg.message_id {
        println!("MessageID: {mid}");
    }
    println!("Subject:   {}", msg.subject);
    println!("From:      {}", msg.from);
    if !msg.to.is_empty() {
        println!("To:        {}", msg.to.join(", "));
    }
    if !msg.cc.is_empty() {
        println!("Cc:        {}", msg.cc.join(", "));
    }
    let date = chrono::DateTime::<chrono::Utc>::from_timestamp(msg.date, 0)
        .map(|d| d.format("%Y-%m-%d %H:%M:%S UTC").to_string())
        .unwrap_or_else(|| "—".into());
    println!("Date:      {date}");
    if let Some(ref r) = msg.in_reply_to {
        println!("In-Reply:  {r}");
    }
    if !msg.attachments.is_empty() {
        println!("\nAttachments:");
        for a in &msg.attachments {
            println!(
                "  [{}] {} ({}, {} bytes)",
                a.id, a.file_name, a.mime, a.size
            );
        }
    }
    if let Some(b) = body {
        println!("\n--- body ---");
        println!("{b}");
    } else if let Some(ref b) = msg.body_plain {
        println!("\n--- body preview ---");
        println!("{b}");
    }
}

pub(crate) fn print_mail_detail_json(
    msg: &task_core::provider::MailMessageDetail,
    body: Option<&str>,
) {
    let to_json = msg
        .to
        .iter()
        .map(|s| format!("\"{}\"", escape_json(s)))
        .collect::<Vec<_>>()
        .join(",");
    let cc_json = msg
        .cc
        .iter()
        .map(|s| format!("\"{}\"", escape_json(s)))
        .collect::<Vec<_>>()
        .join(",");
    let atts_json = msg
        .attachments
        .iter()
        .map(|a| {
            format!(
                "{{\"id\":{},\"file_name\":\"{}\",\"mime\":\"{}\",\"size\":{}}}",
                a.id,
                escape_json(&a.file_name),
                escape_json(&a.mime),
                a.size,
            )
        })
        .collect::<Vec<_>>()
        .join(",");
    println!(
        "{{\"id\":{},\"message_id\":{},\"subject\":\"{}\",\"from\":\"{}\",\"to\":[{}],\"cc\":[{}],\"date\":{},\"body\":{},\"in_reply_to\":{},\"attachments\":[{}]}}",
        msg.id,
        opt_json(msg.message_id.as_deref()),
        escape_json(&msg.subject),
        escape_json(&msg.from),
        to_json,
        cc_json,
        msg.date,
        opt_json(body.or(msg.body_plain.as_deref())),
        opt_json(msg.in_reply_to.as_deref()),
        atts_json,
    );
}

// ── Email printing ───────────────────────────────────────────────────────────

pub(crate) fn print_emails_table(emails: &[task_core::EmailRef]) {
    if emails.is_empty() {
        println!("No emails linked.");
        return;
    }
    println!("{:<19}  {:<20}  {:<40}  BY", "DATE", "FROM", "SUBJECT",);
    println!("{}", "─".repeat(90));
    for e in emails {
        let date = e.date.format("%Y-%m-%d %H:%M:%S").to_string();
        let by = e.linked_by.as_deref().unwrap_or("—");
        let atts = if e.has_attachments {
            format!(" 📎{}", e.attachment_count)
        } else {
            String::new()
        };
        println!(
            "{:<19}  {:<20}  {:<40}  {by}",
            date,
            truncate(&e.from, 20),
            format!("{}{}", truncate(&e.subject, 35), atts),
        );
        println!("  id: {}", e.message_id);
        if !e.user_tags.is_empty() {
            println!("  tags: {}", e.user_tags.join(", "));
        }
    }
    println!("\n{} email(s)", emails.len());
}

pub(crate) fn print_emails_json(emails: &[task_core::EmailRef]) {
    println!("[");
    for (i, e) in emails.iter().enumerate() {
        let comma = if i + 1 < emails.len() { "," } else { "" };
        println!("  {}{comma}", facet_json::to_string(e).unwrap_or_default());
    }
    println!("]");
}

// ── Invoice printing ─────────────────────────────────────────────────────────

pub(crate) fn print_invoices_table(invoices: &[task_core::Invoice]) {
    if invoices.is_empty() {
        println!("No invoices.");
        return;
    }
    println!(
        "{:<18}  {:<20}  {:<14}  {:<11}  {:>12}  {:>12}",
        "ID", "CLIENT", "STATUS", "DUE", "TOTAL", "BALANCE",
    );
    println!("{}", "─".repeat(95));
    for inv in invoices {
        let total = format!("${:.2}", inv.total_cents() as f64 / 100.0);
        let balance = format!("${:.2}", inv.balance_cents() as f64 / 100.0);
        println!(
            "{:<18}  {:<20}  {:<14}  {:<11}  {:>12}  {:>12}",
            inv.id,
            truncate(&inv.client.0, 20),
            format!("{:?}", inv.status),
            inv.due_date,
            total,
            balance,
        );
    }
    println!("\n{} invoice(s)", invoices.len());
}

pub(crate) fn print_invoices_json(invoices: &[task_core::Invoice]) {
    println!("[");
    for (i, inv) in invoices.iter().enumerate() {
        let comma = if i + 1 < invoices.len() { "," } else { "" };
        println!(
            "  {}{comma}",
            facet_json::to_string(inv).unwrap_or_default()
        );
    }
    println!("]");
}

pub(crate) fn print_finance_report(report: &BusinessFinanceReport, json: bool) {
    if json {
        println!("{}", facet_json::to_string(report).unwrap_or_default());
        return;
    }

    println!("Finance report for {}", report.today);
    println!(
        "billable {:.2}h | unbilled {:.2}h / ${:.2} | invoiced ${:.2} | paid ${:.2} | open ${:.2} | overdue ${:.2}",
        report.billable_minutes as f64 / 60.0,
        report.unbilled_minutes as f64 / 60.0,
        report.unbilled_cents as f64 / 100.0,
        report.invoiced_cents as f64 / 100.0,
        report.paid_cents as f64 / 100.0,
        report.open_invoice_cents as f64 / 100.0,
        report.overdue_invoice_cents as f64 / 100.0,
    );

    if !report.clients.is_empty() {
        println!("\nClients");
        for client in &report.clients {
            println!(
                "- {}: unbilled {:.2}h / ${:.2}, open ${:.2}, overdue ${:.2}",
                client.client_name,
                client.unbilled_minutes as f64 / 60.0,
                client.unbilled_cents as f64 / 100.0,
                client.open_invoice_cents as f64 / 100.0,
                client.overdue_invoice_cents as f64 / 100.0,
            );
        }
    }

    if !report.aging.is_empty() {
        println!("\nAging");
        for bucket in &report.aging {
            println!(
                "- {}: {} invoice(s), ${:.2}",
                bucket.name,
                bucket.invoice_count,
                bucket.balance_cents as f64 / 100.0
            );
        }
    }

    if !report.unbilled_entries.is_empty() {
        println!("\nUnbilled entries");
        for entry in report.unbilled_entries.iter().take(20) {
            println!(
                "- {}: {:.2}h ${:.2} ({})",
                entry.task_title,
                entry.entry.duration_minutes() as f64 / 60.0,
                super_time_entry_cents(entry) as f64 / 100.0,
                entry.client_name.as_deref().unwrap_or("Unassigned")
            );
        }
    }
}

pub(crate) fn super_time_entry_cents(entry: &TimeEntryContext) -> u64 {
    let minutes = entry.entry.duration_minutes() as u64;
    let rate = entry.effective_rate(None) as u64;
    ((minutes * rate) + 30) / 60
}

pub(crate) fn print_invoice_detail(inv: &task_core::Invoice) {
    println!("Invoice:  {}", inv.id);
    println!("Client:   {}", inv.client.0);
    println!("Status:   {:?}", inv.status);
    println!("Issued:   {}", inv.issue_date);
    println!("Due:      {}", inv.due_date);
    if let Some(po) = &inv.po_number {
        println!("PO:       {po}");
    }
    if !inv.currency_code.is_empty() {
        println!("Currency: {}", inv.currency_code);
    }
    println!();
    println!(
        "  {:<30}  {:>7}  {:>10}  {:>12}",
        "TASK", "HOURS", "RATE", "AMOUNT",
    );
    println!("  {}", "─".repeat(65));
    for l in &inv.line_items {
        println!(
            "  {:<30}  {:>7.2}  ${:>9.2}  ${:>11.2}",
            truncate(&l.task_title, 30),
            l.hours,
            l.rate_cents as f64 / 100.0,
            l.net_cents() as f64 / 100.0,
        );
    }
    println!("  {}", "─".repeat(65));
    println!(
        "  {:<30}  {:>7}  {:>10}  ${:>11.2}",
        "Subtotal",
        "",
        "",
        inv.lines_net_cents() as f64 / 100.0
    );
    if let Some(d) = inv.discount_percent {
        if d > 0.0 {
            println!(
                "  {:<30}  {:>7}  {:>10}  -${:.2}",
                format!("Discount ({}%)", d),
                "",
                "",
                (inv.lines_net_cents().saturating_sub(inv.discounted_cents())) as f64 / 100.0,
            );
        }
    }
    if inv.tax_cents() > 0 {
        println!(
            "  {:<30}  {:>7}  {:>10}  ${:>11.2}",
            "Tax",
            "",
            "",
            inv.tax_cents() as f64 / 100.0
        );
    }
    println!(
        "  {:<30}  {:>7}  {:>10}  ${:>11.2}",
        "TOTAL",
        "",
        "",
        inv.total_cents() as f64 / 100.0
    );
    if inv.paid_cents() > 0 {
        println!(
            "  {:<30}  {:>7}  {:>10}  ${:>11.2}",
            "Paid",
            "",
            "",
            inv.paid_cents() as f64 / 100.0
        );
        println!(
            "  {:<30}  {:>7}  {:>10}  ${:>11.2}",
            "BALANCE",
            "",
            "",
            inv.balance_cents() as f64 / 100.0
        );
    }
    if !inv.payments.is_empty() {
        println!("\nPayments:");
        for p in &inv.payments {
            println!(
                "  {} — ${:.2}  {} {}",
                p.received_at.format("%Y-%m-%d"),
                p.amount_cents as f64 / 100.0,
                if p.method.is_empty() {
                    "—"
                } else {
                    &p.method
                },
                p.reference.as_deref().unwrap_or(""),
            );
        }
    }
}

pub(crate) fn print_clients_table(clients: &[task_core::Client]) {
    if clients.is_empty() {
        println!("No clients.");
        return;
    }
    let name_w = clients
        .iter()
        .map(|c| c.name.len())
        .max()
        .unwrap_or(10)
        .clamp(10, 35);
    println!(
        "{:<name_w$}  {:>10}  {:<4}  {:<25}  IN ID",
        "NAME", "RATE/HR", "CCY", "EMAIL",
    );
    println!("{}", "─".repeat(name_w + 52));
    for c in clients {
        let rate = match c.default_hourly_rate {
            Some(r) => format!("${:.2}", r as f64 / 100.0),
            None => "—".into(),
        };
        let ccy = if c.currency_code.is_empty() {
            "—"
        } else {
            &c.currency_code
        };
        let email = c.email.as_deref().unwrap_or("—");
        let in_id = c.invoice_ninja_id.as_deref().unwrap_or("—");
        println!(
            "{:<name_w$}  {:>10}  {:<4}  {:<25}  {}",
            truncate(&c.name, name_w),
            rate,
            ccy,
            truncate(email, 25),
            in_id,
        );
    }
    println!("\n{} client(s)", clients.len());
}

pub(crate) fn print_clients_json(clients: &[task_core::Client]) {
    println!("[");
    for (i, c) in clients.iter().enumerate() {
        let comma = if i + 1 < clients.len() { "," } else { "" };
        let json = facet_json::to_string(c).unwrap_or_default();
        println!("  {json}{comma}");
    }
    println!("]");
}

pub(crate) fn print_client_detail(c: &task_core::Client) {
    println!("Name:      {}", c.name);
    if let Some(r) = c.default_hourly_rate {
        println!("Rate:      ${:.2}/hr", r as f64 / 100.0);
    }
    if !c.currency_code.is_empty() {
        println!("Currency:  {}", c.currency_code);
    }
    if let Some(d) = c.payment_terms_days {
        println!("Terms:     net-{d}");
    }
    if let Some(ref e) = c.email {
        println!("Email:     {e}");
    }
    if let Some(ref n) = c.contact_name {
        println!("Contact:   {n}");
    }
    if let Some(ref p) = c.phone {
        println!("Phone:     {p}");
    }
    if let Some(ref id) = c.invoice_ninja_id {
        println!("IN ID:     {id}");
    }
}

pub(crate) fn require_actor(actor: &Option<String>) -> eyre::Result<String> {
    actor
        .clone()
        .ok_or_else(|| eyre::eyre!("No actor. Use --as <user> or set TASK_USER."))
}

pub(crate) fn print_comments_table(comments: &[Comment]) {
    if comments.is_empty() {
        println!("No comments.");
        return;
    }
    for c in comments {
        let depth = c.depth(comments);
        let indent = "  ".repeat(depth);
        let date = c
            .created_at
            .map(|d| format!(" ({})", d.format("%Y-%m-%d %H:%M")))
            .unwrap_or_default();
        let tc = c
            .time_ref
            .as_ref()
            .map(|t| format!(" [{}]", t.display()))
            .unwrap_or_default();
        let resolved = if c.resolved { " ✅" } else { "" };
        println!("{indent}@{}{date}{tc}{resolved}", c.author);
        println!("{indent}  {}", c.body);
        println!("{indent}  id: {}", c.id);
    }
    println!("\n{} comment(s)", comments.len());
}

pub(crate) fn print_comments_json(comments: &[Comment]) {
    println!("[");
    for (i, c) in comments.iter().enumerate() {
        let comma = if i + 1 < comments.len() { "," } else { "" };
        let json = facet_json::to_string(c).unwrap_or_default();
        println!("  {json}{comma}");
    }
    println!("]");
}

pub(crate) fn parse_relation_kind(s: &str) -> eyre::Result<RelationType> {
    Ok(match s.to_lowercase().replace('_', "-").as_str() {
        "blocks" | "blocking" => RelationType::Blocking,
        "blocked-by" | "blockedby" => RelationType::BlockedBy,
        "relates" | "relates-to" | "relatesto" => RelationType::RelatesTo,
        "duplicate-of" | "duplicateof" | "duplicate" => RelationType::DuplicateOf,
        "implements" => RelationType::Implements,
        "implemented-by" | "implementedby" => RelationType::ImplementedBy,
        "start-before" | "startbefore" => RelationType::StartBefore,
        "start-after" | "startafter" => RelationType::StartAfter,
        "finish-before" | "finishbefore" => RelationType::FinishBefore,
        "finish-after" | "finishafter" => RelationType::FinishAfter,
        other => {
            eyre::bail!(
                "Unknown relation kind: {other}. Use: blocks, blocked-by, relates, duplicate-of, implements, implemented-by, start-before, start-after, finish-before, finish-after"
            )
        }
    })
}

// ── Output helpers ────────────────────────────────────────────────────────────

pub(crate) fn status_label(s: &Status) -> &'static str {
    match s {
        Status::None => "—",
        Status::Open => "Open",
        Status::InProgress => "In Progress",
        Status::OnHold => "On Hold",
        Status::Planned => "Planned",
        Status::Done => "Done",
        Status::Cancelled => "Cancelled",
        Status::Archived => "Archived",
    }
}

pub(crate) fn priority_label(p: &Priority) -> &'static str {
    match p {
        Priority::None => "—",
        Priority::Low => "Low",
        Priority::Normal => "Normal",
        Priority::High => "High",
        Priority::Urgent => "Urgent",
    }
}

pub(crate) fn print_tasks_table(tasks: &[Task]) {
    if tasks.is_empty() {
        println!("No tasks.");
        return;
    }

    let title_w = tasks
        .iter()
        .map(|t| t.title.len())
        .max()
        .unwrap_or(5)
        .clamp(5, 45);

    println!(
        "{:<title_w$}  {:<12}  {:<8}  {:<12}  URGENCY",
        "TITLE", "STATUS", "PRIORITY", "DUE"
    );
    println!("{}", "─".repeat(title_w + 48));

    for task in tasks {
        let title = truncate(&task.title, title_w);
        let status = status_label(&task.status);
        let priority = priority_label(&task.priority);
        let due = task
            .due
            .map(|d| d.to_string())
            .unwrap_or_else(|| "—".to_string());
        let urgency = task.urgency_score();
        println!(
            "{:<title_w$}  {:<12}  {:<8}  {:<12}  {}",
            title, status, priority, due, urgency
        );
    }

    println!("\n{} task(s)", tasks.len());
}

pub(crate) fn print_tasks_json(tasks: &[Task]) {
    println!("[");
    for (i, task) in tasks.iter().enumerate() {
        let comma = if i + 1 < tasks.len() { "," } else { "" };
        let json =
            facet_json::to_string(task).unwrap_or_else(|e| format!("{{\"error\":\"{e}}}\"}}",));
        println!("  {json}{comma}");
    }
    println!("]");
}

pub(crate) fn print_inbox_capture(item: &InboxItem, json: bool) {
    if json {
        println!("{}", facet_json::to_string(item).unwrap_or_default());
    } else {
        println!("Captured: {}", item.title);
        println!("  kind: {}", item.kind);
        println!("  id:   {}", item.id.as_deref().unwrap_or("—"));
        if let Some(due) = &item.due {
            println!("  due:  {due}");
        }
    }
}

pub(crate) fn print_inbox_items(items: &[InboxItem], json: bool) {
    if json {
        println!("[");
        for (i, item) in items.iter().enumerate() {
            let comma = if i + 1 < items.len() { "," } else { "" };
            let json = facet_json::to_string(item).unwrap_or_default();
            println!("  {json}{comma}");
        }
        println!("]");
        return;
    }

    if items.is_empty() {
        println!("Inbox is empty.");
        return;
    }

    let title_w = items
        .iter()
        .map(|item| item.title.len())
        .max()
        .unwrap_or(5)
        .clamp(5, 48);
    println!(
        "{:<title_w$}  {:<12}  {:<8}  {:<12}  SOURCE",
        "TITLE", "KIND", "PRIORITY", "DUE"
    );
    println!("{}", "─".repeat(title_w + 48));
    for item in items {
        println!(
            "{:<title_w$}  {:<12}  {:<8}  {:<12}  {}",
            truncate(&item.title, title_w),
            item.kind,
            item.priority,
            item.due.as_deref().unwrap_or("—"),
            item.source.as_deref().unwrap_or("—")
        );
    }
    println!("\n{} inbox item(s)", items.len());
}

pub(crate) fn print_review_report(report: &ReviewReport, json: bool) {
    if json {
        println!("{}", facet_json::to_string(report).unwrap_or_default());
        return;
    }

    println!(
        "Review for {} through {} (stale after {} days)",
        report.today, report.horizon_end, report.stale_after_days
    );
    println!(
        "inbox {} | overdue {} | today {} | upcoming {} | waiting {} | ideas {} | unscheduled {} | stale {}",
        report.inbox.len(),
        report.overdue.len(),
        report.due_today.len() + report.scheduled_today.len(),
        report.upcoming.len(),
        report.waiting.len(),
        report.ideas.len() + report.someday.len(),
        report.unscheduled.len(),
        report.stale.len()
    );

    if !report.inbox.is_empty() {
        println!("\nInbox");
        print_inbox_items(&report.inbox, false);
    }
    print_review_task_bucket("Overdue", &report.overdue);
    print_review_task_bucket("Due today", &report.due_today);
    print_review_task_bucket("Scheduled today", &report.scheduled_today);
    print_review_task_bucket("Upcoming", &report.upcoming);
    print_review_task_bucket("Waiting", &report.waiting);
    print_review_task_bucket("Commitments", &report.commitments);
    print_review_task_bucket("Ideas", &report.ideas);
    print_review_task_bucket("Someday / maybe", &report.someday);
    print_review_task_bucket("Unscheduled", &report.unscheduled);
    print_review_task_bucket("Stale", &report.stale);
}

pub(crate) fn print_operating_model(report: &OperatingModelReport, json: bool) {
    if json {
        println!("{}", facet_json::to_string(report).unwrap_or_default());
        return;
    }

    println!("Operating model for {}", report.today);
    println!(
        "open {} | overdue {} | today {} | waiting {} | stale {} | unscheduled {} | timers {} | upcoming events {}",
        report.open_tasks,
        report.overdue_tasks,
        report.due_today_tasks,
        report.waiting_tasks,
        report.stale_tasks,
        report.unscheduled_tasks,
        report.active_timers,
        report.upcoming_events,
    );

    if !report.areas.is_empty() {
        println!("\nAreas");
        for area in report.areas.iter().take(20) {
            let next = area
                .next_action
                .as_ref()
                .map(|task| task.title.as_str())
                .unwrap_or("—");
            println!(
                "- {}: open {} projects {} overdue {} today {} waiting {} stale {} routines {} habits {} goals {} | next {}",
                area.name,
                area.open_tasks,
                area.active_projects,
                area.overdue_tasks,
                area.due_today_tasks,
                area.waiting_tasks,
                area.stale_tasks,
                area.routine_tasks,
                area.habit_tasks,
                area.goal_tasks,
                next,
            );
        }
    }

    if !report.goals.is_empty() {
        println!("\nGoals");
        for goal in report.goals.iter().take(20) {
            let area = goal.area.as_deref().unwrap_or("—");
            let due = goal.due.as_deref().unwrap_or("—");
            let next = goal
                .next_action
                .as_ref()
                .map(|task| task.title.as_str())
                .unwrap_or("—");
            println!("- {} [{}] due {} | next {}", goal.title, area, due, next);
        }
    }

    if !report.routines.is_empty() || !report.habits.is_empty() {
        println!("\nRoutines and habits");
        for routine in report.routines.iter().chain(report.habits.iter()).take(25) {
            println!(
                "- {} ({}) recur {} due {} scheduled {}",
                routine.title,
                routine.kind,
                routine.recurrence.as_deref().unwrap_or("—"),
                routine.due.as_deref().unwrap_or("—"),
                routine.scheduled.as_deref().unwrap_or("—"),
            );
        }
    }

    if !report.inbox.is_empty() {
        println!("\nInbox");
        print_inbox_items(&report.inbox, false);
    }
}

pub(crate) fn print_review_task_bucket(label: &str, tasks: &[Task]) {
    if tasks.is_empty() {
        return;
    }
    println!("\n{label}");
    for task in tasks.iter().take(20) {
        let due = task
            .due
            .map(|date| date.to_string())
            .unwrap_or_else(|| "—".to_string());
        let scheduled = task
            .scheduled
            .map(|date| date.to_string())
            .unwrap_or_else(|| "—".to_string());
        let project = task
            .projects
            .first()
            .map(|project| project.0.as_str())
            .unwrap_or("—");
        println!(
            "  {:<46} {:<9} due {:<10} scheduled {:<10} {}",
            truncate(&task.title, 46),
            priority_label(&task.priority),
            due,
            scheduled,
            project
        );
    }
    if tasks.len() > 20 {
        println!("  ... {} more", tasks.len() - 20);
    }
}

pub(crate) fn print_people(people: &[Person], json: bool) {
    if json {
        println!("[");
        for (index, person) in people.iter().enumerate() {
            let comma = if index + 1 < people.len() { "," } else { "" };
            println!(
                "  {}{comma}",
                facet_json::to_string(person).unwrap_or_default()
            );
        }
        println!("]");
        return;
    }
    if people.is_empty() {
        println!("No people found.");
        return;
    }
    for person in people {
        let primary = person
            .contact_methods
            .iter()
            .find(|method| method.primary)
            .or_else(|| person.contact_methods.first())
            .map(|method| method.value.as_str())
            .unwrap_or("—");
        println!(
            "{:<32} {:<28} {}",
            truncate(&person.display_name, 32),
            truncate(person.organization.as_deref().unwrap_or("—"), 28),
            primary
        );
    }
    println!("\n{} people", people.len());
}

pub(crate) fn print_organizations(organizations: &[OrganizationRecord], json: bool) {
    if json {
        println!("[");
        for (index, org) in organizations.iter().enumerate() {
            let comma = if index + 1 < organizations.len() {
                ","
            } else {
                ""
            };
            println!(
                "  {}{comma}",
                facet_json::to_string(org).unwrap_or_default()
            );
        }
        println!("]");
        return;
    }
    if organizations.is_empty() {
        println!("No organizations found.");
        return;
    }
    for org in organizations {
        println!(
            "{:<36} {} people",
            truncate(&org.name, 36),
            org.people.len()
        );
    }
    println!("\n{} organizations", organizations.len());
}

pub(crate) fn print_person_context(context: Option<&PersonContext>, json: bool) {
    if json {
        match context {
            Some(context) => println!("{}", facet_json::to_string(context).unwrap_or_default()),
            None => println!("null"),
        }
        return;
    }
    let Some(context) = context else {
        println!("Person not found.");
        return;
    };
    println!("Person: {}", context.person.display_name);
    if let Some(org) = &context.person.organization {
        println!("Organization: {org}");
    }
    println!(
        "Related: {} task(s), {} project(s), {} event(s), {} communication ref(s)",
        context.tasks.len(),
        context.projects.len(),
        context.calendar_events.len(),
        context.communications.len()
    );
    print_review_task_bucket("Tasks", &context.tasks);
}

pub(crate) fn print_organization_context(context: Option<&OrganizationContext>, json: bool) {
    if json {
        match context {
            Some(context) => println!("{}", facet_json::to_string(context).unwrap_or_default()),
            None => println!("null"),
        }
        return;
    }
    let Some(context) = context else {
        println!("Organization not found.");
        return;
    };
    println!("Organization: {}", context.organization.name);
    println!(
        "Related: {} people, {} task(s), {} project(s), {} event(s), {} communication ref(s)",
        context.people.len(),
        context.tasks.len(),
        context.projects.len(),
        context.calendar_events.len(),
        context.communications.len()
    );
    print_people(&context.people, false);
    print_review_task_bucket("Tasks", &context.tasks);
}

pub(crate) fn print_sync_states(states: &[ProviderSyncState], json: bool) {
    if json {
        println!("[");
        for (index, state) in states.iter().enumerate() {
            let comma = if index + 1 < states.len() { "," } else { "" };
            println!(
                "  {}{comma}",
                facet_json::to_string(state).unwrap_or_default()
            );
        }
        println!("]");
        return;
    }
    if states.is_empty() {
        println!("No provider sync state recorded.");
        return;
    }
    for state in states {
        let status = state
            .last_error
            .as_deref()
            .map(|_| "failed")
            .unwrap_or("ok");
        println!(
            "{:<14} {:<24} {:<8} token={} updated={}",
            state.provider,
            truncate(&state.collection, 24),
            status,
            state.sync_token.as_deref().unwrap_or("—"),
            state.updated_at
        );
        if let Some(error) = &state.last_error {
            println!("  error: {error}");
        }
    }
}

pub(crate) fn print_task_detail(task: &Task) {
    println!("Title:    {}", task.title);
    println!("Status:   {}", status_label(&task.status));
    println!("Priority: {}", priority_label(&task.priority));
    if let Some(d) = task.due {
        println!("Due:      {d}");
    }
    if let Some(d) = task.scheduled {
        println!("Scheduled:{d}");
    }
    if !task.projects.is_empty() {
        let names: Vec<_> = task.projects.iter().map(|p| p.0.as_str()).collect();
        println!("Projects: {}", names.join(", "));
    }
    if !task.contexts.is_empty() {
        println!("Contexts: {}", task.contexts.join(", "));
    }
    if !task.tags.is_empty() {
        println!("Tags:     {}", task.tags.join(", "));
    }
    println!("ID:       {}", task.id);
    if task.is_overdue() {
        println!("⚠ OVERDUE");
    }
    if task.is_blocked() {
        println!("⛔ BLOCKED ({} dependencies)", task.blocked_by.len());
    }
    if let Some(r) = &task.recurrence {
        println!("Recurs:   {r}");
    }
    println!("Urgency:  {}", task.urgency_score());
}

pub(crate) fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}…", &s[..max.saturating_sub(1)])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::shared::normalize_vox_url;

    #[test]
    fn normalizes_server_urls_to_vox_websocket_endpoint() {
        assert_eq!(
            normalize_vox_url("https://tasks.example.com"),
            "wss://tasks.example.com/vox"
        );
        assert_eq!(
            normalize_vox_url("http://127.0.0.1:3000/vox"),
            "ws://127.0.0.1:3000/vox"
        );
        assert_eq!(
            normalize_vox_url("ws://localhost:3000/vox"),
            "ws://localhost:3000/vox"
        );
    }

    #[test]
    fn remote_vox_config_adds_auth_query_params() {
        let config = RemoteVoxConfig::new(
            "https://tasks.example.com".into(),
            Some("tok en+/=".into()),
            Some("org/one".into()),
        )
        .unwrap();
        assert_eq!(
            config.vox_url,
            "wss://tasks.example.com/vox?token=tok%20en%2B%2F%3D&organization_id=org%2Fone"
        );
        assert_eq!(
            config.display_url,
            "wss://tasks.example.com/vox?token=%3Credacted%3E&organization_id=org%2Fone"
        );
    }

    #[test]
    fn server_profiles_resolve_default_and_named_profiles() {
        let profiles = ServerProfiles {
            default: Some("starcommand".into()),
            servers: vec![ServerProfile {
                name: "starcommand".into(),
                url: "https://cloud.starcommand.live".into(),
                session_token: Some("token".into()),
                organization_id: Some("org".into()),
            }],
        };
        assert_eq!(
            profiles.resolve("default").unwrap().url,
            "https://cloud.starcommand.live"
        );
        assert_eq!(
            profiles
                .resolve("starcommand")
                .unwrap()
                .organization_id
                .as_deref(),
            Some("org")
        );
        assert!(profiles.resolve("missing").is_none());
    }

    #[test]
    fn server_profiles_resolve_by_configured_url() {
        let profiles = ServerProfiles {
            default: Some("starcommand".into()),
            servers: vec![ServerProfile {
                name: "starcommand".into(),
                url: "http://10.10.10.1:3456".into(),
                session_token: Some("token".into()),
                organization_id: Some("org".into()),
            }],
        };

        let profile = profiles.resolve("http://10.10.10.1:3456").unwrap();
        assert_eq!(profile.name, "starcommand");
        assert_eq!(profile.session_token.as_deref(), Some("token"));
        assert_eq!(profile.organization_id.as_deref(), Some("org"));

        assert_eq!(
            profiles.resolve("http://10.10.10.1:3456/").unwrap().name,
            "starcommand"
        );
    }
}
