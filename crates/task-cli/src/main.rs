use clap::{Parser, Subcommand};
use chrono::{DateTime, TimeZone, Utc};
use task_core::index::{ChangeRow, ConflictRow};
use task_core::workflows::{parse_comments, render_comments, Comment};
use task_core::{
    Filter, Priority, Query, RelationType, Sort, Status, Task, TaskRelation, TimeEntryFilter,
    VaultServiceImpl, WikiLink,
};

#[derive(Parser)]
#[command(name = "task", about = "Task management CLI", version)]
struct Cli {
    /// Path to the vault directory
    #[arg(long, env = "TASK_VAULT", global = true)]
    vault: Option<String>,

    /// Act as this user — sets created_by / comment author / resolved_by.
    #[arg(long, env = "TASK_USER", global = true)]
    as_user: Option<String>,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
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
    /// Mark a task as complete
    Complete {
        title: String,
    },
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
    Assign {
        reference: String,
        user: String,
    },
    /// Comment on a task. Bare form adds a comment; subcommands manage existing.
    Comment {
        #[command(subcommand)]
        command: CommentCommands,
    },
    /// React to a task with an emoji (or `clear:<emoji>` to remove)
    React {
        reference: String,
        emoji: String,
    },
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
    /// Trigger a Nextcloud sync cycle and print stats
    Sync {
        #[arg(long)]
        json: bool,
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
    /// Email linking — associate emails with tasks/projects. Bot-friendly.
    Email {
        #[command(subcommand)]
        command: EmailCommands,
    },
    /// Nextcloud instance queries (read-only smoke tests)
    Nc {
        #[command(subcommand)]
        command: NcCommands,
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
    /// Project subcommands
    Project {
        #[command(subcommand)]
        command: ProjectCommands,
    },
}

#[derive(Subcommand)]
enum ClientCommands {
    /// Create or update a client note (upserts by name)
    Add {
        name: String,
        /// Default hourly rate in cents (e.g. 12000 = $120/hr)
        #[arg(long)]
        rate: Option<u32>,
        /// ISO 4217 currency code, e.g. "USD", "EUR"
        #[arg(long)]
        currency: Option<String>,
        /// Net payment terms in days
        #[arg(long)]
        terms_days: Option<u32>,
        #[arg(long)]
        email: Option<String>,
        #[arg(long)]
        contact: Option<String>,
        #[arg(long)]
        phone: Option<String>,
        /// Invoice Ninja client hashed id (set after sync)
        #[arg(long)]
        invoice_ninja_id: Option<String>,
    },
    /// List all clients
    List {
        #[arg(long)]
        json: bool,
    },
    /// Show a single client
    Show {
        name: String,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum NcCommands {
    /// List users on the Nextcloud instance
    Users {
        #[arg(long)]
        json: bool,
    },
    /// Show display name for a specific user
    User { user_id: String },
    /// List Deck boards
    Boards {
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum EmailCommands {
    /// List configured Nextcloud Mail accounts
    Accounts {
        #[arg(long)]
        json: bool,
    },
    /// List mailboxes (folders) in an account
    Mailboxes {
        #[arg(long)]
        account: i64,
        #[arg(long)]
        json: bool,
    },
    /// Search / list messages in a mailbox
    Search {
        #[arg(long)]
        mailbox: i64,
        /// Filter: free-text or `from:`, `to:`, `subject:`, `cc:`, `bcc:` tokens
        #[arg(long)]
        filter: Option<String>,
        #[arg(long, short = 'n', default_value = "25")]
        limit: u32,
        #[arg(long)]
        cursor: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show one message (headers + body)
    Show {
        id: i64,
        /// Also fetch the body
        #[arg(long)]
        body: bool,
        #[arg(long)]
        json: bool,
    },
    /// Link an email to a task or project. Bot-friendly — every field that
    /// isn't provided is left as None / empty.
    Link {
        /// "task" or "project"
        #[arg(long)]
        to: String,
        /// Task title/id or project title
        reference: String,
        /// RFC-2822 Message-ID (with or without angle brackets)
        #[arg(long)]
        message_id: String,
        #[arg(long)]
        subject: Option<String>,
        #[arg(long)]
        from: Option<String>,
        /// Comma-separated recipient list
        #[arg(long)]
        to_recipients: Option<String>,
        /// Send date (RFC3339 or "YYYY-MM-DD HH:MM")
        #[arg(long)]
        date: Option<String>,
        #[arg(long)]
        snippet: Option<String>,
        #[arg(long)]
        account_id: Option<i64>,
        #[arg(long)]
        mailbox: Option<String>,
        #[arg(long)]
        imap_uid: Option<u32>,
        #[arg(long)]
        nc_db_id: Option<i64>,
        #[arg(long)]
        attachments: Option<u32>,
        /// Comma-separated categorization tags
        #[arg(long)]
        tags: Option<String>,
    },
    /// Unlink an email from a task or project
    Unlink {
        #[arg(long)]
        to: String,
        reference: String,
        #[arg(long)]
        message_id: String,
    },
    /// List emails linked to a task or project
    List {
        #[arg(long)]
        to: String,
        reference: String,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum InvoiceCommands {
    /// Create an invoice from uninvoiced billable entries for a client
    Create {
        client: String,
        /// Start of billing window (YYYY-MM-DD, inclusive)
        #[arg(long)]
        from: Option<String>,
        /// End of billing window (YYYY-MM-DD, inclusive)
        #[arg(long)]
        to: Option<String>,
        /// Fallback hourly rate in cents if cascade resolves to 0
        #[arg(long)]
        rate: Option<u32>,
        /// Invoice-level tax rate as a percentage, e.g. 8.5
        #[arg(long)]
        tax: Option<f64>,
        /// Invoice-level discount as a percentage
        #[arg(long)]
        discount: Option<f64>,
        #[arg(long)]
        po: Option<String>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List invoices (newest first)
    List {
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        client: Option<String>,
        #[arg(long)]
        year: Option<i32>,
        #[arg(long)]
        json: bool,
    },
    /// Show a single invoice
    Show {
        id: String,
        /// Print the full rendered markdown body instead of a summary
        #[arg(long)]
        md: bool,
        #[arg(long)]
        json: bool,
    },
    /// Mark an invoice as sent (sets sent_at, status → Sent)
    Send {
        id: String,
    },
    /// Record a payment against an invoice
    Pay {
        id: String,
        /// Amount in cents (e.g. 50000 = $500)
        #[arg(long)]
        amount: u64,
        #[arg(long, default_value = "")]
        method: String,
        #[arg(long)]
        reference: Option<String>,
        #[arg(long)]
        notes: Option<String>,
    },
    /// Cancel an invoice
    Cancel {
        id: String,
        #[arg(long)]
        reason: Option<String>,
    },
}

#[derive(Subcommand)]
enum TalkCommands {
    /// List rooms the user is a member of
    Rooms {
        #[arg(long)]
        json: bool,
    },
    /// Post a message to a room
    Send {
        /// Room token (from `talk rooms`)
        room: String,
        /// Message body
        message: String,
        /// Reply to a parent message id
        #[arg(long)]
        reply_to: Option<u64>,
    },
    /// Show recent messages in a room
    History {
        room: String,
        #[arg(long, short = 'n', default_value = "20")]
        limit: u32,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum CommentCommands {
    /// Add a comment to a task
    Add {
        reference: String,
        #[arg(long)]
        body: String,
        /// Timecode like "2:34" or "2:30-2:36" (audio/video)
        #[arg(long)]
        timecode: Option<String>,
    },
    /// List comments on a task
    List {
        reference: String,
        #[arg(long)]
        json: bool,
    },
    /// Reply to an existing comment by id
    Reply {
        reference: String,
        parent_id: String,
        #[arg(long)]
        body: String,
    },
    /// Mark a comment resolved (by id)
    Resolve { reference: String, comment_id: String },
    /// Unresolve a comment
    Reopen { reference: String, comment_id: String },
}

#[derive(Subcommand)]
enum ConflictCommands {
    /// List conflicts
    List {
        /// Include already-resolved conflicts
        #[arg(long)]
        all: bool,
        #[arg(long, short = 'n', default_value = "50")]
        limit: u32,
        #[arg(long)]
        json: bool,
    },
    /// Resolve a conflict by id
    Resolve {
        conflict_id: i64,
        /// How it was resolved — free-form tag (e.g. "picked-winning",
        /// "picked-losing", "merged", "ignored")
        #[arg(long, default_value = "resolved")]
        how: String,
    },
}

#[derive(Subcommand)]
enum TimeCommands {
    /// Show the currently-running timer, if any
    Active {
        #[arg(long)]
        json: bool,
    },
    /// Log a completed time entry manually
    Log {
        reference: String,
        /// Start time — "YYYY-MM-DDTHH:MM:SS" (UTC) or "YYYY-MM-DD HH:MM"
        #[arg(long)]
        start: String,
        /// End time in the same formats as --start
        #[arg(long)]
        end: String,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        billable: bool,
        #[arg(long)]
        rate: Option<u32>,
    },
    /// List time entries across the vault
    List {
        #[arg(long)]
        task: Option<String>,
        #[arg(long)]
        user: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        client: Option<String>,
        #[arg(long)]
        tag: Option<String>,
        /// From date (YYYY-MM-DD, inclusive)
        #[arg(long)]
        from: Option<String>,
        /// To date (YYYY-MM-DD, inclusive)
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        billable: bool,
        /// Output format: table (default), json, csv
        #[arg(long, default_value = "table")]
        format: String,
        /// Alias for --format json
        #[arg(long, conflicts_with = "format")]
        json: bool,
    },
    /// Aggregate time by task, user, project, client, or tag
    Report {
        /// task | user | project | client | tag
        #[arg(long, default_value = "task")]
        group_by: String,
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        client: Option<String>,
        #[arg(long)]
        tag: Option<String>,
        #[arg(long)]
        user: Option<String>,
        #[arg(long)]
        billable: bool,
        /// Fallback billable rate in cents per hour (used when an entry has no rate override)
        #[arg(long)]
        rate: Option<u32>,
        /// Output format: table (default), json, csv
        #[arg(long, default_value = "table")]
        format: String,
        /// Alias for --format json
        #[arg(long, conflicts_with = "format")]
        json: bool,
    },
    /// Edit an existing time entry by id
    Edit {
        entry_id: String,
        /// Start time (YYYY-MM-DDTHH:MM[:SS]Z or "YYYY-MM-DD HH:MM")
        #[arg(long)]
        start: Option<String>,
        /// End time — pass "clear" to reopen the timer
        #[arg(long)]
        end: Option<String>,
        #[arg(long)]
        description: Option<String>,
        /// Mark billable / non-billable
        #[arg(long)]
        billable: Option<bool>,
        /// Billable rate in cents per hour — pass 0 to clear the override
        #[arg(long)]
        rate: Option<u32>,
        #[arg(long)]
        user: Option<String>,
        /// Replace tags (comma-separated). Pass "" to clear.
        #[arg(long)]
        tags: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Delete a time entry by id
    Delete {
        entry_id: String,
    },
}

#[derive(Subcommand)]
enum ProjectCommands {
    /// List all projects
    List {
        #[arg(long)]
        json: bool,
    },
    /// Show task stats for a project
    Stats {
        name: String,
        #[arg(long)]
        json: bool,
    },
    /// Show the next actionable task for a project
    Next {
        name: String,
        #[arg(long)]
        json: bool,
    },
    /// List all tasks belonging to a project
    Tasks {
        name: String,
        #[arg(long)]
        json: bool,
    },
    /// Edit project fields — status, client, rate, email_tags, etc.
    Edit {
        name: String,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        area: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        /// Pass "clear" to remove.
        #[arg(long)]
        client: Option<String>,
        /// Billable rate in cents/hr; 0 clears.
        #[arg(long)]
        default_rate: Option<u32>,
        #[arg(long)]
        identifier: Option<String>,
        #[arg(long)]
        lead: Option<String>,
        #[arg(long)]
        default_assignee: Option<String>,
        #[arg(long)]
        emoji: Option<String>,
        #[arg(long)]
        repo: Option<String>,
        #[arg(long)]
        dev_path: Option<String>,
        #[arg(long)]
        project_type: Option<String>,
        #[arg(long)]
        workflow: Option<String>,
        #[arg(long)]
        workflow_stage: Option<String>,
        /// YYYY-MM-DD or "clear"
        #[arg(long)]
        due: Option<String>,
        #[arg(long)]
        start: Option<String>,
        #[arg(long)]
        add_tag: Vec<String>,
        #[arg(long)]
        remove_tag: Vec<String>,
        #[arg(long)]
        add_email_tag: Vec<String>,
        #[arg(long)]
        remove_email_tag: Vec<String>,
        #[arg(long)]
        add_team: Vec<String>,
        #[arg(long)]
        remove_team: Vec<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show a single project
    Show {
        name: String,
        #[arg(long)]
        json: bool,
    },
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    let cli = Cli::parse();
    let Cli {
        vault,
        as_user: actor,
        command,
    } = cli;

    // Talk commands don't touch the vault — handle them before requiring one.
    if let Commands::Talk { command: talk } = command {
        return run_talk(talk, actor).await;
    }
    // Nc smoke-test commands — same deal, no vault.
    if let Commands::Nc { command: nc } = command {
        return run_nc(nc, actor).await;
    }

    let vault_path = vault.ok_or_else(|| {
        eyre::eyre!("No vault specified. Use --vault <path> or set TASK_VAULT env var.")
    })?;

    let svc = VaultServiceImpl::new(&vault_path);

    match command {
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
                svc.list_tasks().await
            } else {
                let mut filters = Vec::new();
                if let Some(s) = status {
                    let st = parse_status(&s)
                        .ok_or_else(|| eyre::eyre!("Unknown status: {s}"))?;
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
                svc.execute_query(Query {
                    filters,
                    sort: parse_sort(&sort),
                    limit,
                    group: None,
                })
                .await
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
            let task = Task {
                title,
                priority: priority
                    .as_deref()
                    .map(parse_priority)
                    .transpose()
                    .map_err(|e| eyre::eyre!("{e}"))?
                    .unwrap_or(Priority::None),
                status: status
                    .as_deref()
                    .map(|s| parse_status(s).ok_or_else(|| format!("Unknown status: {s}")))
                    .transpose()
                    .map_err(|e| eyre::eyre!("{e}"))?
                    .unwrap_or(Status::Open),
                due: due
                    .as_deref()
                    .map(|d| d.parse::<chrono::NaiveDate>().map_err(|e| eyre::eyre!("{e}")))
                    .transpose()?,
                scheduled: scheduled
                    .as_deref()
                    .map(|d| d.parse::<chrono::NaiveDate>().map_err(|e| eyre::eyre!("{e}")))
                    .transpose()?,
                projects: project.map(|p| vec![WikiLink(p)]).unwrap_or_default(),
                contexts: context.map(|c| vec![c]).unwrap_or_default(),
                tags: tag.map(|t| vec![t]).unwrap_or_default(),
                recurrence,
                assignee,
                created_by: actor.clone(),
                ..Task::default()
            };

            let created = svc.create_task(task).await?;
            println!("Created: {}", created.title);
            println!("  id:  {}", created.id.as_deref().unwrap_or("—"));
            if let Some(d) = created.due {
                println!("  due: {d}");
            }
        }

        Commands::Complete { title } => {
            let task = svc.complete_task_as(title, actor.as_deref()).await?;
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
            let task = find_task(&svc, &title).await?;
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
            let mut task = find_task(&svc, &reference).await?;
            if let Some(t) = title {
                task.title = t;
            }
            if let Some(s) = status {
                task.status = parse_status(&s)
                    .ok_or_else(|| eyre::eyre!("Unknown status: {s}"))?;
            }
            if let Some(p) = priority {
                task.priority = parse_priority(&p).map_err(|e| eyre::eyre!("{e}"))?;
            }
            if let Some(d) = due {
                task.due = parse_optional_date(&d)?;
            }
            if let Some(d) = scheduled {
                task.scheduled = parse_optional_date(&d)?;
            }
            if let Some(a) = assignee {
                task.assignee = if a == "clear" || a.is_empty() { None } else { Some(a) };
            }
            for t in &remove_tag {
                task.tags.retain(|x| x != t);
            }
            for t in add_tag {
                if !task.tags.contains(&t) {
                    task.tags.push(t);
                }
            }
            for p in &remove_project {
                task.projects.retain(|x| &x.0 != p);
            }
            for p in add_project {
                if !task.projects.iter().any(|x| x.0 == p) {
                    task.projects.push(WikiLink(p));
                }
            }
            for c in &remove_context {
                task.contexts.retain(|x| x != c);
            }
            for c in add_context {
                if !task.contexts.contains(&c) {
                    task.contexts.push(c);
                }
            }
            if let Some(r) = recurrence {
                task.recurrence = if r == "clear" || r.is_empty() {
                    None
                } else {
                    Some(r)
                };
            }
            if let Some(b) = body {
                task.body = b;
            }

            let updated = svc.update_task_as(task, actor.as_deref()).await?;
            if json {
                println!("{}", facet_json::to_string(&updated).unwrap_or_default());
            } else {
                println!("Updated: {}", updated.title);
            }
        }

        Commands::Delete { reference, hard } => {
            if hard {
                let task = find_task(&svc, &reference).await?;
                svc.delete_task_as(task.title.clone(), actor.as_deref()).await?;
                println!("Deleted (hard): {}", task.title);
            } else {
                let mut task = find_task(&svc, &reference).await?;
                task.deleted_at = Some(chrono::Utc::now());
                let updated = svc.update_task_as(task, actor.as_deref()).await?;
                println!("Deleted (soft): {}", updated.title);
            }
        }

        Commands::Assign { reference, user } => {
            let mut task = find_task(&svc, &reference).await?;
            task.assignee = if user == "clear" || user.is_empty() {
                None
            } else {
                Some(user)
            };
            let updated = svc.update_task_as(task, actor.as_deref()).await?;
            match &updated.assignee {
                Some(u) => println!("Assigned '{}' → {u}", updated.title),
                None => println!("Unassigned '{}'", updated.title),
            }
        }

        Commands::Comment {
            command:
                CommentCommands::Add {
                    reference,
                    body,
                    timecode,
                },
        } => {
            let author = require_actor(&actor)?;
            let mut task = find_task(&svc, &reference).await?;

            let time_ref = match timecode.as_deref() {
                Some(tc) => Some(
                    task_core::workflows::parse_timecode(tc)
                        .ok_or_else(|| eyre::eyre!("Invalid timecode: {tc}"))?,
                ),
                None => None,
            };

            let now = chrono::Local::now().naive_local();
            let mentions = Comment::extract_mentions(&body);
            let new_comment = Comment {
                id: Comment::generate_id(&author, Some(now), &body),
                author,
                body,
                created_at: Some(now),
                time_ref,
                mentions,
                ..Default::default()
            };

            let mut comments = parse_comments(&task.body);
            comments.push(new_comment.clone());
            task.body = splice_comments(&task.body, &comments);

            svc.update_task_as(task, actor.as_deref()).await?;
            println!("Comment added ({}).", new_comment.id);
        }

        Commands::Comment {
            command: CommentCommands::List { reference, json },
        } => {
            let task = find_task(&svc, &reference).await?;
            let comments = parse_comments(&task.body);
            if json {
                print_comments_json(&comments);
            } else {
                print_comments_table(&comments);
            }
        }

        Commands::Comment {
            command:
                CommentCommands::Reply {
                    reference,
                    parent_id,
                    body,
                },
        } => {
            let author = require_actor(&actor)?;
            let mut task = find_task(&svc, &reference).await?;
            let mut comments = parse_comments(&task.body);
            if !comments.iter().any(|c| c.id == parent_id) {
                eyre::bail!("No comment with id {parent_id} on task {reference}");
            }
            let now = chrono::Local::now().naive_local();
            let mentions = Comment::extract_mentions(&body);
            let new_comment = Comment {
                id: Comment::generate_id(&author, Some(now), &body),
                author,
                body,
                created_at: Some(now),
                reply_to: Some(parent_id),
                mentions,
                ..Default::default()
            };
            comments.push(new_comment.clone());
            task.body = splice_comments(&task.body, &comments);
            svc.update_task_as(task, actor.as_deref()).await?;
            println!("Reply added ({}).", new_comment.id);
        }

        Commands::Comment {
            command: CommentCommands::Resolve { reference, comment_id },
        } => {
            let resolver = require_actor(&actor)?;
            let mut task = find_task(&svc, &reference).await?;
            let mut comments = parse_comments(&task.body);
            let Some(c) = comments.iter_mut().find(|c| c.id == comment_id) else {
                eyre::bail!("No comment with id {comment_id}");
            };
            c.resolved = true;
            c.resolved_by = Some(resolver);
            task.body = splice_comments(&task.body, &comments);
            svc.update_task_as(task, actor.as_deref()).await?;
            println!("Resolved comment {comment_id}.");
        }

        Commands::Comment {
            command: CommentCommands::Reopen { reference, comment_id },
        } => {
            let mut task = find_task(&svc, &reference).await?;
            let mut comments = parse_comments(&task.body);
            let Some(c) = comments.iter_mut().find(|c| c.id == comment_id) else {
                eyre::bail!("No comment with id {comment_id}");
            };
            c.resolved = false;
            c.resolved_by = None;
            task.body = splice_comments(&task.body, &comments);
            svc.update_task_as(task, actor.as_deref()).await?;
            println!("Reopened comment {comment_id}.");
        }

        Commands::React { reference, emoji } => {
            let user = require_actor(&actor)?;
            let mut task = find_task(&svc, &reference).await?;
            if let Some(e) = emoji.strip_prefix("clear:") {
                let before = task.reactions.len();
                task.reactions.retain(|r| !(r.user == user && r.emoji == e));
                if task.reactions.len() == before {
                    eyre::bail!("No {e} reaction from @{user} to remove");
                }
                svc.update_task_as(task, actor.as_deref()).await?;
                println!("Removed {e} from @{user}.");
            } else {
                // Dedup: one reaction per (user, emoji).
                if !task
                    .reactions
                    .iter()
                    .any(|r| r.user == user && r.emoji == emoji)
                {
                    task.reactions.push(task_core::Reaction {
                        emoji: emoji.clone(),
                        user: user.clone(),
                    });
                    svc.update_task_as(task, actor.as_deref()).await?;
                }
                println!("Reacted {emoji} from @{user}.");
            }
        }

        Commands::Subscribe { reference, user } => {
            let who = user.or(actor.clone()).ok_or_else(|| {
                eyre::eyre!("Specify a user or set --as <user>/TASK_USER.")
            })?;
            let mut task = find_task(&svc, &reference).await?;
            if !task.subscribers.contains(&who) {
                task.subscribers.push(who.clone());
                svc.update_task_as(task, actor.as_deref()).await?;
            }
            println!("@{who} subscribed.");
        }

        Commands::Talk { .. } => unreachable!("handled above"),
        Commands::Nc { .. } => unreachable!("handled above"),

        Commands::Client {
            command:
                ClientCommands::Add {
                    name,
                    rate,
                    currency,
                    terms_days,
                    email,
                    contact,
                    phone,
                    invoice_ninja_id,
                },
        } => {
            // Upsert: if a client with this name exists, preserve non-touched fields.
            let existing = svc.find_client(&name).await;
            let mut client = existing.unwrap_or_else(|| task_core::Client {
                name: name.clone(),
                ..Default::default()
            });
            if let Some(r) = rate {
                client.default_hourly_rate = Some(r);
            }
            if let Some(c) = currency {
                client.currency_code = c;
            }
            if let Some(d) = terms_days {
                client.payment_terms_days = Some(d);
            }
            if let Some(e) = email {
                client.email = Some(e);
            }
            if let Some(c) = contact {
                client.contact_name = Some(c);
            }
            if let Some(p) = phone {
                client.phone = Some(p);
            }
            if let Some(id) = invoice_ninja_id {
                client.invoice_ninja_id = Some(id);
            }
            let saved = svc.save_client(client).await?;
            println!(
                "Saved client '{}' (rate {}¢/hr).",
                saved.name,
                saved.default_hourly_rate.unwrap_or(0)
            );
        }

        Commands::Client {
            command: ClientCommands::List { json },
        } => {
            let clients = svc.list_clients().await;
            if json {
                print_clients_json(&clients);
            } else {
                print_clients_table(&clients);
            }
        }

        Commands::Client {
            command: ClientCommands::Show { name, json },
        } => {
            let client = svc
                .find_client(&name)
                .await
                .ok_or_else(|| eyre::eyre!("Client not found: {name}"))?;
            if json {
                println!("{}", facet_json::to_string(&client).unwrap_or_default());
            } else {
                print_client_detail(&client);
            }
        }

        Commands::Invoice {
            command:
                InvoiceCommands::Create {
                    client,
                    from,
                    to,
                    rate,
                    tax,
                    discount,
                    po,
                    notes,
                    json,
                },
        } => {
            let from_dt = from.as_deref().map(parse_date_start).transpose()?;
            let to_dt = to.as_deref().map(parse_date_end).transpose()?;
            let invoice = svc
                .create_invoice_from_entries(
                    &client,
                    from_dt,
                    to_dt,
                    rate,
                    tax,
                    discount,
                    po,
                    notes,
                    actor.as_deref(),
                )
                .await?;
            if json {
                println!("{}", facet_json::to_string(&invoice).unwrap_or_default());
            } else {
                print_invoice_detail(&invoice);
            }
        }

        Commands::Invoice {
            command:
                InvoiceCommands::List {
                    status,
                    client,
                    year,
                    json,
                },
        } => {
            let invoices: Vec<task_core::Invoice> = svc
                .list_invoices()
                .await
                .into_iter()
                .filter(|i| match &status {
                    Some(s) => {
                        format!("{:?}", i.status).eq_ignore_ascii_case(s)
                    }
                    None => true,
                })
                .filter(|i| match &client {
                    Some(c) => i.client.0.eq_ignore_ascii_case(c),
                    None => true,
                })
                .filter(|i| match year {
                    Some(y) => i.issue_date.format("%Y").to_string() == format!("{y:04}"),
                    None => true,
                })
                .collect();
            if json {
                print_invoices_json(&invoices);
            } else {
                print_invoices_table(&invoices);
            }
        }

        Commands::Invoice {
            command: InvoiceCommands::Show { id, md, json },
        } => {
            let invoice = svc
                .get_invoice(&id)
                .await
                .ok_or_else(|| eyre::eyre!("Invoice not found: {id}"))?;
            if json {
                println!("{}", facet_json::to_string(&invoice).unwrap_or_default());
            } else if md {
                println!("{}", task_core::invoice::render_invoice_body(&invoice));
            } else {
                print_invoice_detail(&invoice);
            }
        }

        Commands::Invoice {
            command: InvoiceCommands::Send { id },
        } => {
            let invoice = svc.send_invoice(&id, actor.as_deref()).await?;
            println!(
                "Sent invoice {} — ${:.2} due {}.",
                invoice.id,
                invoice.total_cents() as f64 / 100.0,
                invoice.due_date
            );
        }

        Commands::Invoice {
            command:
                InvoiceCommands::Pay {
                    id,
                    amount,
                    method,
                    reference,
                    notes,
                },
        } => {
            let invoice = svc
                .record_invoice_payment(
                    &id,
                    amount,
                    if method.is_empty() { None } else { Some(method) },
                    reference,
                    notes,
                    actor.as_deref(),
                )
                .await?;
            println!(
                "Recorded ${:.2} against {}. Balance: ${:.2}. Status: {:?}",
                amount as f64 / 100.0,
                invoice.id,
                invoice.balance_cents() as f64 / 100.0,
                invoice.status
            );
        }

        Commands::Invoice {
            command: InvoiceCommands::Cancel { id, reason },
        } => {
            let invoice = svc.cancel_invoice(&id, reason, actor.as_deref()).await?;
            println!("Cancelled invoice {}.", invoice.id);
        }

        Commands::Email {
            command: EmailCommands::Accounts { json },
        } => {
            let client = build_mail_client(actor.as_deref())?;
            let accounts = client.list_accounts().await?;
            if json {
                print_mail_accounts_json(&accounts);
            } else {
                print_mail_accounts_table(&accounts);
            }
        }

        Commands::Email {
            command: EmailCommands::Mailboxes { account, json },
        } => {
            let client = build_mail_client(actor.as_deref())?;
            let boxes = client.list_mailboxes(account).await?;
            if json {
                print_mailboxes_json(&boxes);
            } else {
                print_mailboxes_table(&boxes);
            }
        }

        Commands::Email {
            command:
                EmailCommands::Search {
                    mailbox,
                    filter,
                    limit,
                    cursor,
                    json,
                },
        } => {
            let client = build_mail_client(actor.as_deref())?;
            let messages = client
                .list_messages(mailbox, filter.as_deref(), limit, cursor.as_deref())
                .await?;
            if json {
                print_mail_messages_json(&messages);
            } else {
                print_mail_messages_table(&messages);
            }
        }

        Commands::Email {
            command: EmailCommands::Show { id, body, json },
        } => {
            let client = build_mail_client(actor.as_deref())?;
            let msg = client.get_message(id).await?;
            let body_text = if body {
                client.get_body(id).await.ok()
            } else {
                None
            };
            if json {
                print_mail_detail_json(&msg, body_text.as_deref());
            } else {
                print_mail_detail(&msg, body_text.as_deref());
            }
        }

        Commands::Email {
            command:
                EmailCommands::Link {
                    to,
                    reference,
                    message_id,
                    subject,
                    from,
                    to_recipients,
                    date,
                    snippet,
                    account_id,
                    mailbox,
                    imap_uid,
                    nc_db_id,
                    attachments,
                    tags,
                },
        } => {
            let now = chrono::Utc::now();
            let email = task_core::EmailRef {
                message_id: message_id.clone(),
                subject: subject.unwrap_or_default(),
                from: from.unwrap_or_default(),
                to: to_recipients
                    .map(|s| s.split(',').map(|t| t.trim().to_string()).collect())
                    .unwrap_or_default(),
                date: date
                    .as_deref()
                    .map(parse_datetime)
                    .transpose()?
                    .unwrap_or(now),
                snippet,
                account_id,
                mailbox,
                imap_uid,
                nc_db_id,
                has_attachments: attachments.map(|n| n > 0).unwrap_or(false),
                attachment_count: attachments.unwrap_or(0),
                linked_by: actor.clone(),
                linked_at: Some(now),
                user_tags: tags
                    .map(|s| s.split(',').map(|t| t.trim().to_string()).collect())
                    .unwrap_or_default(),
            };
            match to.as_str() {
                "task" => {
                    let task = svc
                        .link_email_to_task(&reference, email, actor.as_deref())
                        .await?;
                    println!(
                        "Linked {} to task '{}'. ({} emails total)",
                        message_id,
                        task.title,
                        task.emails.len()
                    );
                }
                "project" => {
                    let p = svc
                        .link_email_to_project(&reference, email, actor.as_deref())
                        .await?;
                    println!(
                        "Linked {} to project '{}'. ({} emails total)",
                        message_id,
                        p.title,
                        p.emails.len()
                    );
                }
                other => eyre::bail!("--to must be 'task' or 'project', got '{other}'"),
            }
        }

        Commands::Email {
            command:
                EmailCommands::Unlink {
                    to,
                    reference,
                    message_id,
                },
        } => match to.as_str() {
            "task" => {
                svc.unlink_email_from_task(&reference, &message_id, actor.as_deref())
                    .await?;
                println!("Unlinked {message_id} from task '{reference}'.");
            }
            "project" => {
                svc.unlink_email_from_project(&reference, &message_id, actor.as_deref())
                    .await?;
                println!("Unlinked {message_id} from project '{reference}'.");
            }
            other => eyre::bail!("--to must be 'task' or 'project', got '{other}'"),
        },

        Commands::Email {
            command:
                EmailCommands::List {
                    to,
                    reference,
                    json,
                },
        } => {
            let emails = match to.as_str() {
                "task" => svc
                    .emails_for_task(&reference)
                    .await
                    .ok_or_else(|| eyre::eyre!("Task not found: {reference}"))?,
                "project" => svc
                    .emails_for_project(&reference)
                    .await
                    .ok_or_else(|| eyre::eyre!("Project not found: {reference}"))?,
                other => eyre::bail!("--to must be 'task' or 'project', got '{other}'"),
            };
            if json {
                print_emails_json(&emails);
            } else {
                print_emails_table(&emails);
            }
        }

        Commands::Unsubscribe { reference, user } => {
            let who = user.or(actor.clone()).ok_or_else(|| {
                eyre::eyre!("Specify a user or set --as <user>/TASK_USER.")
            })?;
            let mut task = find_task(&svc, &reference).await?;
            let before = task.subscribers.len();
            task.subscribers.retain(|u| u != &who);
            if task.subscribers.len() != before {
                svc.update_task_as(task, actor.as_deref()).await?;
            }
            println!("@{who} unsubscribed.");
        }

        Commands::Link { from, to, kind } => {
            let mut source = find_task(&svc, &from).await?;
            let target = find_task(&svc, &to).await?;
            let rt = parse_relation_kind(&kind)?;
            // Dedup: don't add the same (target, kind) twice.
            let target_ref = target.id.clone().unwrap_or_else(|| target.title.clone());
            let already = source
                .relations
                .iter()
                .any(|r| r.target == target_ref && r.relation_type == rt);
            if !already {
                source.relations.push(TaskRelation {
                    target: target_ref.clone(),
                    relation_type: rt,
                });
                svc.update_task_as(source, actor.as_deref()).await?;
            }
            println!("Linked '{from}' --{kind}--> '{to}'.");
        }

        Commands::For { user, json } => {
            let tasks = svc.tasks_for_user(user).await;
            if json {
                print_tasks_json(&tasks);
            } else {
                print_tasks_table(&tasks);
            }
        }

        Commands::DueBy { date, json } => {
            let tasks = svc.tasks_due_by(date).await;
            if json {
                print_tasks_json(&tasks);
            } else {
                print_tasks_table(&tasks);
            }
        }

        Commands::Search { query, json, limit } => {
            let mut results = svc.search_tasks(query).await;
            if let Some(n) = limit {
                results.truncate(n);
            }
            if json {
                print_tasks_json(&results);
            } else {
                print_tasks_table(&results);
            }
        }

        Commands::Sync { json } => {
            let stats = svc.trigger_sync().await?;
            if json {
                println!("{}", facet_json::to_string(&stats).unwrap_or_default());
            } else {
                println!("Sync complete.");
                println!("  calendar: +{} / -{}", stats.calendar_pushed, stats.calendar_pulled);
                println!("  deck:     +{} / -{}", stats.deck_pushed, stats.deck_pulled);
                println!("  files:    created {}, updated {}", stats.files_created, stats.files_updated);
                if !stats.errors.is_empty() {
                    println!("  errors:");
                    for e in &stats.errors {
                        println!("    - {e}");
                    }
                }
            }
        }

        Commands::Project {
            command: ProjectCommands::List { json },
        } => {
            let projects = svc.list_projects().await;
            if json {
                let items: Vec<String> = projects
                    .iter()
                    .map(|p| facet_json::to_string(p).unwrap_or_default())
                    .collect();
                println!("[{}]", items.join(","));
                return Ok(());
            }
            if projects.is_empty() {
                println!("No projects found.");
                return Ok(());
            }
            let name_w = projects.iter().map(|p| p.title.len()).max().unwrap_or(10) + 2;
            println!("{:<name_w$}  {:<10}  {}", "NAME", "STATE", "DUE");
            println!("{}", "─".repeat(name_w + 20));
            for p in &projects {
                let state = format!("{:?}", p.status);
                let due = p.due.map(|d| d.to_string()).unwrap_or_else(|| "—".to_string());
                println!("{:<name_w$}  {:<10}  {}", p.title, state, due);
            }
            println!("\n{} project(s)", projects.len());
        }

        Commands::Project {
            command: ProjectCommands::Stats { name, json },
        } => {
            let stats = svc.project_stats(name.clone()).await;
            if json {
                println!("{}", facet_json::to_string(&stats).unwrap_or_default());
            } else {
                println!("Project: {name}");
                println!("  Open:      {}", stats.open_task_count);
                println!("  Completed: {}", stats.completed_task_count);
                println!("  Total:     {}", stats.total());
                if let Some(pct) = stats.completion_percent() {
                    println!("  Progress:  {:.0}%", pct);
                }
            }
        }

        Commands::Project {
            command: ProjectCommands::Next { name, json },
        } => match svc.next_task(name.clone()).await {
            Some(task) => {
                if json {
                    println!("{}", facet_json::to_string(&task).unwrap_or_default());
                } else {
                    println!("Next task for '{}'", name);
                    print_task_detail(&task);
                }
            }
            None => {
                if json {
                    println!("null");
                } else {
                    println!("No actionable tasks for '{}'.", name);
                }
            }
        },

        Commands::Project {
            command: ProjectCommands::Tasks { name, json },
        } => {
            let tasks = svc.tasks_for_project(name).await;
            if json {
                print_tasks_json(&tasks);
            } else {
                print_tasks_table(&tasks);
            }
        }

        Commands::Project {
            command:
                ProjectCommands::Edit {
                    name,
                    status,
                    description,
                    area,
                    organization,
                    client,
                    default_rate,
                    identifier,
                    lead,
                    default_assignee,
                    emoji,
                    repo,
                    dev_path,
                    project_type,
                    workflow,
                    workflow_stage,
                    due,
                    start,
                    add_tag,
                    remove_tag,
                    add_email_tag,
                    remove_email_tag,
                    add_team,
                    remove_team,
                    json,
                },
        } => {
            let patch = task_core::ProjectPatch {
                status,
                description,
                area,
                organization,
                project_type,
                workflow,
                workflow_stage,
                identifier,
                lead,
                default_assignee,
                emoji,
                repo,
                dev_path,
                client,
                default_rate,
                due,
                start,
                add_tag,
                remove_tag,
                add_email_tag,
                remove_email_tag,
                add_team,
                remove_team,
            };
            let updated = svc
                .update_project_as(&name, patch, actor.as_deref())
                .await?;
            if json {
                println!("{}", facet_json::to_string(&updated).unwrap_or_default());
            } else {
                println!("Updated project '{}'.", updated.title);
            }
        }

        Commands::Project {
            command: ProjectCommands::Show { name, json },
        } => {
            let project = svc
                .find_project(&name)
                .await
                .ok_or_else(|| eyre::eyre!("Project not found: {name}"))?;
            if json {
                println!("{}", facet_json::to_string(&project).unwrap_or_default());
            } else {
                print_project_detail(&project);
            }
        }

        Commands::Start {
            reference,
            description,
            billable,
            rate,
        } => {
            let entry = svc
                .start_timer(&reference, description, billable, rate, actor.clone())
                .await?;
            println!("Started timer on '{reference}' (entry {}).", entry.id);
        }

        Commands::Stop { reference, json } => {
            let (title, entry) = svc.stop_timer(reference.as_deref()).await?;
            if json {
                println!("{}", facet_json::to_string(&entry).unwrap_or_default());
            } else {
                println!(
                    "Stopped '{title}' — {} min logged.",
                    entry.duration_minutes()
                );
            }
        }

        Commands::Time {
            command: TimeCommands::Active { json },
        } => match svc.active_timer().await {
            Some((title, entry)) => {
                if json {
                    println!(
                        "{{\"task\":\"{}\",\"entry\":{}}}",
                        escape_json(&title),
                        facet_json::to_string(&entry).unwrap_or_default()
                    );
                } else {
                    let elapsed = entry.elapsed_minutes(chrono::Utc::now());
                    println!("Running: '{title}' — {elapsed} min");
                    if let Some(d) = &entry.description {
                        println!("  {d}");
                    }
                    println!("  id: {}", entry.id);
                }
            }
            None => {
                if json {
                    println!("null");
                } else {
                    println!("No running timer.");
                }
            }
        },

        Commands::Time {
            command:
                TimeCommands::Log {
                    reference,
                    start,
                    end,
                    description,
                    billable,
                    rate,
                },
        } => {
            let start_dt = parse_datetime(&start)?;
            let end_dt = parse_datetime(&end)?;
            let entry = svc
                .log_time(
                    &reference,
                    start_dt,
                    end_dt,
                    description,
                    billable,
                    rate,
                    actor.clone(),
                )
                .await?;
            println!(
                "Logged {} min on '{reference}' (entry {}).",
                entry.duration_minutes(),
                entry.id
            );
        }

        Commands::Time {
            command:
                TimeCommands::List {
                    task,
                    user,
                    project,
                    client,
                    tag,
                    from,
                    to,
                    billable,
                    format,
                    json,
                },
        } => {
            let fmt = pick_format(&format, json);
            let filter = TimeEntryFilter {
                task_ref: task,
                user,
                project,
                client,
                tag,
                from: from.as_deref().map(parse_date_start).transpose()?,
                to: to.as_deref().map(parse_date_end).transpose()?,
                billable_only: billable,
            };
            let entries = svc.list_time_entries(filter).await;
            match fmt {
                OutputFormat::Json => print_time_entries_json(&entries),
                OutputFormat::Csv => print_time_entries_csv(&entries),
                OutputFormat::Table => print_time_entries_table(&entries),
            }
        }

        Commands::Time {
            command:
                TimeCommands::Report {
                    group_by,
                    from,
                    to,
                    project,
                    client,
                    tag,
                    user,
                    billable,
                    rate,
                    format,
                    json,
                },
        } => {
            let fmt = pick_format(&format, json);
            let filter = TimeEntryFilter {
                task_ref: None,
                user,
                project,
                client,
                tag,
                from: from.as_deref().map(parse_date_start).transpose()?,
                to: to.as_deref().map(parse_date_end).transpose()?,
                billable_only: billable,
            };
            let entries = svc.list_time_entries(filter).await;
            let report = aggregate_time(&entries, &group_by, rate)?;
            match fmt {
                OutputFormat::Json => print_report_json(&report),
                OutputFormat::Csv => print_report_csv(&report, &group_by),
                OutputFormat::Table => print_report_table(&report),
            }
        }

        Commands::Time {
            command: TimeCommands::Delete { entry_id },
        } => {
            svc.delete_time_entry_as(&entry_id, actor.as_deref()).await?;
            println!("Deleted entry {entry_id}.");
        }

        Commands::Time {
            command:
                TimeCommands::Edit {
                    entry_id,
                    start,
                    end,
                    description,
                    billable,
                    rate,
                    user,
                    tags,
                    json,
                },
        } => {
            let mut patch = task_core::TimeEntryPatch::default();
            if let Some(s) = start {
                patch.start_time = Some(parse_datetime(&s)?);
            }
            if let Some(e) = end {
                patch.end_time = Some(if e == "clear" {
                    None
                } else {
                    Some(parse_datetime(&e)?)
                });
            }
            patch.description = description;
            patch.billable = billable;
            patch.billable_rate = rate;
            patch.user = user;
            patch.tags = tags.map(|s| {
                if s.is_empty() {
                    Vec::new()
                } else {
                    s.split(',').map(|t| t.trim().to_string()).collect()
                }
            });

            let (title, updated) = svc
                .edit_time_entry(&entry_id, patch, actor.as_deref())
                .await?;
            if json {
                println!(
                    "{{\"task\":\"{}\",\"entry\":{}}}",
                    escape_json(&title),
                    facet_json::to_string(&updated).unwrap_or_default()
                );
            } else {
                println!(
                    "Updated entry {} on '{title}' — {} min.",
                    updated.id,
                    updated.duration_minutes()
                );
            }
        }

        Commands::Activity { limit, kind, json } => {
            let changes = svc.recent_activity(limit).await?;
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

        Commands::Conflicts {
            command: ConflictCommands::List { all, limit, json },
        } => {
            let rows = svc.list_conflicts(!all, limit).await?;
            if json {
                print_conflicts_json(&rows);
            } else {
                print_conflicts_table(&rows);
            }
        }

        Commands::Conflicts {
            command: ConflictCommands::Resolve { conflict_id, how },
        } => {
            svc.resolve_conflict(conflict_id, actor.as_deref(), &how).await?;
            println!("Resolved conflict #{conflict_id} ({how}).");
        }
    }

    Ok(())
}

// ── Lookup ────────────────────────────────────────────────────────────────────

/// Find a task by id or case-insensitive title match.
async fn find_task(svc: &VaultServiceImpl, reference: &str) -> eyre::Result<Task> {
    let tasks = svc.list_tasks().await;
    tasks
        .into_iter()
        .find(|t| {
            t.id.as_deref() == Some(reference) || t.title.eq_ignore_ascii_case(reference)
        })
        .ok_or_else(|| eyre::eyre!("Task not found: {reference}"))
}

/// Splice a rendered `## Comments` block back into the body, replacing any
/// existing block or appending one.
fn splice_comments(body: &str, comments: &[Comment]) -> String {
    let rendered = render_comments(comments);

    let mut lines = body.lines().collect::<Vec<_>>();
    let start = lines.iter().position(|l| l.trim() == "## Comments");

    if let Some(s) = start {
        // Find the next `## ` heading after the comments section, or EOF.
        let end = lines
            .iter()
            .enumerate()
            .skip(s + 1)
            .find(|(_, l)| l.trim_start().starts_with("## "))
            .map(|(i, _)| i)
            .unwrap_or(lines.len());

        let mut out = lines[..s].join("\n");
        if !out.is_empty() && !out.ends_with('\n') {
            out.push('\n');
        }
        out.push_str(&rendered);
        if end < lines.len() {
            out.push('\n');
            lines.drain(0..end);
            out.push_str(&lines.join("\n"));
        }
        out
    } else {
        let mut out = body.trim_end().to_string();
        if !out.is_empty() {
            out.push_str("\n\n");
        }
        out.push_str(&rendered);
        out
    }
}

fn parse_optional_date(s: &str) -> eyre::Result<Option<chrono::NaiveDate>> {
    if s == "clear" || s.is_empty() {
        Ok(None)
    } else {
        Ok(Some(s.parse::<chrono::NaiveDate>()?))
    }
}

// ── Nextcloud Talk ────────────────────────────────────────────────────────────

/// Run a Talk subcommand. Reads credentials from NEXTCLOUD_URL / NEXTCLOUD_USER /
/// NEXTCLOUD_PASSWORD. `as_user` overrides NEXTCLOUD_USER when provided —
/// useful for Hermes-style bot identities.
async fn run_nc(cmd: NcCommands, as_user: Option<String>) -> eyre::Result<()> {
    use task_core::provider::{NextcloudConfig, NextcloudProvider};

    let url = std::env::var("NEXTCLOUD_URL")
        .map_err(|_| eyre::eyre!("Set NEXTCLOUD_URL env var."))?;
    let env_user = std::env::var("NEXTCLOUD_USER").ok();
    let username = as_user.clone().or(env_user).ok_or_else(|| {
        eyre::eyre!("Set NEXTCLOUD_USER env var or pass --as-user.")
    })?;
    let password = std::env::var("NEXTCLOUD_PASSWORD")
        .map_err(|_| eyre::eyre!("Set NEXTCLOUD_PASSWORD env var."))?;

    let provider = NextcloudProvider::new(
        "nc",
        "Nextcloud",
        NextcloudConfig {
            url,
            username,
            password,
            projects_path: std::env::var("NEXTCLOUD_PROJECTS_PATH")
                .unwrap_or_else(|_| "Projects/".to_string()),
            calendar: None,
            deck_enabled: true,
            deck_boards: std::collections::HashMap::new(),
        },
    );

    match cmd {
        NcCommands::Users { json } => {
            let users = provider.list_users().await?;
            if json {
                let items: Vec<String> = users
                    .iter()
                    .map(|u| format!("\"{}\"", escape_json(u)))
                    .collect();
                println!("[{}]", items.join(","));
            } else {
                for u in &users {
                    println!("{u}");
                }
                println!("\n{} user(s)", users.len());
            }
        }
        NcCommands::User { user_id } => {
            let display = provider.get_user_display_name(&user_id).await?;
            println!("{user_id} → {display}");
        }
        NcCommands::Boards { json } => {
            let boards = provider.list_deck_boards().await?;
            if json {
                let items: Vec<String> = boards
                    .iter()
                    .map(|p| facet_json::to_string(p).unwrap_or_default())
                    .collect();
                println!("[{}]", items.join(","));
            } else {
                if boards.is_empty() {
                    println!("No Deck boards.");
                } else {
                    for p in &boards {
                        println!("- {} ({:?})", p.title, p.status);
                    }
                    println!("\n{} board(s)", boards.len());
                }
            }
        }
    }
    Ok(())
}

async fn run_talk(cmd: TalkCommands, as_user: Option<String>) -> eyre::Result<()> {
    use task_core::provider::{TalkClient, TalkConfig};

    let url = std::env::var("NEXTCLOUD_URL")
        .map_err(|_| eyre::eyre!("Set NEXTCLOUD_URL env var."))?;
    let env_user = std::env::var("NEXTCLOUD_USER").ok();
    let username = as_user.clone().or(env_user).ok_or_else(|| {
        eyre::eyre!("Set NEXTCLOUD_USER env var or pass --as-user.")
    })?;
    let password = std::env::var("NEXTCLOUD_PASSWORD")
        .map_err(|_| eyre::eyre!("Set NEXTCLOUD_PASSWORD env var."))?;

    let client = TalkClient::new(TalkConfig {
        url,
        username,
        password,
    });

    match cmd {
        TalkCommands::Rooms { json } => {
            let rooms = client.list_rooms().await?;
            if json {
                print_talk_rooms_json(&rooms);
            } else {
                print_talk_rooms_table(&rooms);
            }
        }
        TalkCommands::Send {
            room,
            message,
            reply_to,
        } => {
            let id = client.send_message(&room, &message, reply_to).await?;
            println!("Sent message {id} to {room}.");
        }
        TalkCommands::History { room, limit, json } => {
            let msgs = client.recent_messages(&room, limit).await?;
            if json {
                print_talk_history_json(&msgs);
            } else {
                print_talk_history_table(&msgs);
            }
        }
    }
    Ok(())
}

fn print_talk_rooms_table(rooms: &[task_core::provider::TalkRoom]) {
    if rooms.is_empty() {
        println!("No rooms.");
        return;
    }
    let name_w = rooms.iter().map(|r| r.name.len()).max().unwrap_or(10).max(10).min(40);
    println!(
        "{:<name_w$}  {:<22}  {:>7}  {:<5}  TOKEN",
        "NAME", "LAST ACTIVITY (UTC)", "PEOPLE", "TYPE",
    );
    println!("{}", "─".repeat(name_w + 55));
    for r in rooms {
        let when = if r.last_activity > 0 {
            chrono::DateTime::<chrono::Utc>::from_timestamp(r.last_activity, 0)
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
            r.room_type,
            r.token,
        );
    }
    println!("\n{} room(s)", rooms.len());
}

fn print_talk_rooms_json(rooms: &[task_core::provider::TalkRoom]) {
    println!("[");
    for (i, r) in rooms.iter().enumerate() {
        let comma = if i + 1 < rooms.len() { "," } else { "" };
        println!(
            "  {{\"token\":\"{}\",\"name\":\"{}\",\"type\":{},\"participants\":{},\"last_activity\":{},\"last_message\":{}}}{comma}",
            escape_json(&r.token),
            escape_json(&r.name),
            r.room_type,
            r.participant_count,
            r.last_activity,
            opt_json(r.last_message.as_deref()),
        );
    }
    println!("]");
}

fn print_talk_history_table(msgs: &[task_core::provider::TalkMessage]) {
    if msgs.is_empty() {
        println!("No messages.");
        return;
    }
    // Messages come newest-first from the API — reverse for readability.
    let mut list: Vec<&task_core::provider::TalkMessage> = msgs.iter().collect();
    list.sort_by_key(|m| m.timestamp);
    for m in list {
        let when = chrono::DateTime::<chrono::Utc>::from_timestamp(m.timestamp, 0)
            .map(|d| d.format("%H:%M:%S").to_string())
            .unwrap_or_else(|| "—".into());
        let reply = match m.reply_to {
            Some(id) => format!(" ↪#{id}"),
            None => String::new(),
        };
        println!("[{when}] @{} (#{}{}): {}", m.actor_id, m.id, reply, m.message);
    }
}

fn print_talk_history_json(msgs: &[task_core::provider::TalkMessage]) {
    println!("[");
    for (i, m) in msgs.iter().enumerate() {
        let comma = if i + 1 < msgs.len() { "," } else { "" };
        println!(
            "  {{\"id\":{},\"token\":\"{}\",\"actor_id\":\"{}\",\"actor_type\":\"{}\",\"actor_display_name\":\"{}\",\"timestamp\":{},\"message\":\"{}\",\"reply_to\":{}}}{comma}",
            m.id,
            escape_json(&m.token),
            escape_json(&m.actor_id),
            escape_json(&m.actor_type),
            escape_json(&m.actor_display_name),
            m.timestamp,
            escape_json(&m.message),
            m.reply_to.map(|n| n.to_string()).unwrap_or_else(|| "null".into()),
        );
    }
    println!("]");
}

// ── Time helpers ──────────────────────────────────────────────────────────────

/// Parse a datetime in a few tolerant shapes. Returns UTC.
/// Accepted: RFC3339 ("2026-04-17T09:30:00Z"), naive UTC ("2026-04-17T09:30"),
/// and "YYYY-MM-DD HH:MM" (also naive UTC).
fn parse_datetime(s: &str) -> eyre::Result<DateTime<Utc>> {
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

fn parse_date_start(s: &str) -> eyre::Result<DateTime<Utc>> {
    let d = s
        .parse::<chrono::NaiveDate>()
        .map_err(|_| eyre::eyre!("Invalid date: {s}"))?;
    Ok(Utc.from_utc_datetime(&d.and_hms_opt(0, 0, 0).unwrap()))
}

fn parse_date_end(s: &str) -> eyre::Result<DateTime<Utc>> {
    let d = s
        .parse::<chrono::NaiveDate>()
        .map_err(|_| eyre::eyre!("Invalid date: {s}"))?;
    Ok(Utc.from_utc_datetime(&d.and_hms_opt(23, 59, 59).unwrap()))
}

fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

fn print_time_entries_table(entries: &[task_core::TimeEntryContext]) {
    if entries.is_empty() {
        println!("No time entries.");
        return;
    }
    let title_w = entries
        .iter()
        .map(|c| c.task_title.len())
        .max()
        .unwrap_or(10)
        .max(5)
        .min(35);
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

fn print_time_entries_json(entries: &[task_core::TimeEntryContext]) {
    println!("[");
    for (i, ctx) in entries.iter().enumerate() {
        let comma = if i + 1 < entries.len() { "," } else { "" };
        let entry_json = facet_json::to_string(&ctx.entry).unwrap_or_default();
        let projects_json = ctx
            .task_projects
            .iter()
            .map(|p| format!("\"{}\"", escape_json(p)))
            .collect::<Vec<_>>()
            .join(",");
        println!(
            "  {{\"task\":\"{}\",\"projects\":[{}],\"entry\":{}}}{comma}",
            escape_json(&ctx.task_title),
            projects_json,
            entry_json
        );
    }
    println!("]");
}

fn print_time_entries_csv(entries: &[task_core::TimeEntryContext]) {
    println!("entry_id,task,projects,client,user,start,end,minutes,billable,rate_cents,billable_amount_cents,tags,description,invoiced_at,invoice_ninja_invoice_id");
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
            row.iter().map(|f| csv_escape(f)).collect::<Vec<_>>().join(",")
        );
    }
}

// ── Output format helper ─────────────────────────────────────────────────────

#[derive(Copy, Clone, Debug)]
enum OutputFormat {
    Table,
    Json,
    Csv,
}

fn pick_format(s: &str, json_alias: bool) -> OutputFormat {
    if json_alias {
        return OutputFormat::Json;
    }
    match s.to_lowercase().as_str() {
        "json" => OutputFormat::Json,
        "csv" => OutputFormat::Csv,
        _ => OutputFormat::Table,
    }
}

fn csv_escape(s: &str) -> String {
    if s.chars().any(|c| matches!(c, ',' | '"' | '\n' | '\r')) {
        format!("\"{}\"", s.replace('"', "\"\""))
    } else {
        s.to_string()
    }
}

/// ({group_key}, total_minutes, billable_cents, entry_count)
type ReportRow = (String, u64, u64, usize);

fn aggregate_time(
    entries: &[task_core::TimeEntryContext],
    group_by: &str,
    fallback_rate: Option<u32>,
) -> eyre::Result<Vec<ReportRow>> {
    use std::collections::BTreeMap;
    let mut acc: BTreeMap<String, (u64, u64, usize)> = BTreeMap::new();

    // Helper to bump a slot by minutes and billable cents.
    let bump = |acc: &mut BTreeMap<String, (u64, u64, usize)>, key: String, mins: u64, cents: u64| {
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

fn print_report_csv(rows: &[ReportRow], group_by: &str) {
    println!("{},minutes,hours,billable_cents,billable_dollars,entries", group_by);
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

fn print_report_table(rows: &[ReportRow]) {
    if rows.is_empty() {
        println!("No entries in range.");
        return;
    }
    let key_w = rows.iter().map(|r| r.0.len()).max().unwrap_or(5).max(5).min(40);
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

fn print_activity_table(rows: &[ChangeRow]) {
    if rows.is_empty() {
        println!("No activity.");
        return;
    }
    let id_w = rows.iter().map(|r| r.entity_id.len()).max().unwrap_or(5).max(5).min(35);
    println!(
        "{:<19}  {:<6}  {:<id_w$}  {:<12}  {:<10}  {}",
        "WHEN (UTC)", "KIND", "ENTITY", "FIELD", "BY", "CHANGE",
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

fn print_activity_json(rows: &[ChangeRow]) {
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

fn print_conflicts_table(rows: &[ConflictRow]) {
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
        let state = r
            .resolved
            .clone()
            .unwrap_or_else(|| "open".into());
        let w_actor = r.winning_actor.as_deref().map(|a| format!("@{a}")).unwrap_or_default();
        let l_actor = r.losing_actor.as_deref().map(|a| format!("@{a}")).unwrap_or_default();
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

fn print_conflicts_json(rows: &[ConflictRow]) {
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

fn opt_json(s: Option<&str>) -> String {
    match s {
        Some(v) => format!("\"{}\"", escape_json(v)),
        None => "null".into(),
    }
}

fn print_report_json(rows: &[ReportRow]) {
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

// ── Parsing helpers ───────────────────────────────────────────────────────────

fn parse_status(s: &str) -> Option<Status> {
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

fn parse_priority(s: &str) -> Result<Priority, String> {
    match s.to_lowercase().as_str() {
        "none" => Ok(Priority::None),
        "low" => Ok(Priority::Low),
        "normal" | "medium" => Ok(Priority::Normal),
        "high" => Ok(Priority::High),
        "urgent" | "critical" => Ok(Priority::Urgent),
        _ => Err(format!("Unknown priority: {s}. Use: none, low, normal, high, urgent")),
    }
}

fn parse_sort(s: &str) -> Sort {
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

fn print_project_detail(p: &task_core::Project) {
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

// ── Nextcloud Mail helpers ───────────────────────────────────────────────────

fn build_mail_client(
    as_user: Option<&str>,
) -> eyre::Result<task_core::provider::MailClient> {
    use task_core::provider::{MailClient, MailConfig};
    let url = std::env::var("NEXTCLOUD_URL")
        .map_err(|_| eyre::eyre!("Set NEXTCLOUD_URL env var."))?;
    let env_user = std::env::var("NEXTCLOUD_USER").ok();
    let username = as_user
        .map(String::from)
        .or(env_user)
        .ok_or_else(|| eyre::eyre!("Set NEXTCLOUD_USER env var or pass --as-user."))?;
    let password = std::env::var("NEXTCLOUD_PASSWORD")
        .map_err(|_| eyre::eyre!("Set NEXTCLOUD_PASSWORD env var."))?;
    Ok(MailClient::new(MailConfig {
        url,
        username,
        password,
    }))
}

fn print_mail_accounts_table(accounts: &[task_core::provider::MailAccount]) {
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

fn print_mail_accounts_json(accounts: &[task_core::provider::MailAccount]) {
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

fn print_mailboxes_table(boxes: &[task_core::provider::Mailbox]) {
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
            m.unread.map(|n| n.to_string()).unwrap_or_else(|| "—".into()),
            m.name,
        );
    }
    println!("\n{} mailbox(es)", boxes.len());
}

fn print_mailboxes_json(boxes: &[task_core::provider::Mailbox]) {
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

fn print_mail_messages_table(messages: &[task_core::provider::MailMessage]) {
    if messages.is_empty() {
        println!("No messages.");
        return;
    }
    println!(
        "{:<8}  {:<19}  {:<25}  SUBJECT",
        "ID", "DATE", "FROM",
    );
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

fn print_mail_messages_json(messages: &[task_core::provider::MailMessage]) {
    println!("[");
    for (i, m) in messages.iter().enumerate() {
        let comma = if i + 1 < messages.len() { "," } else { "" };
        let to_json = m
            .to
            .iter()
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
            m.account_id.map(|n| n.to_string()).unwrap_or_else(|| "null".into()),
            m.imap_uid.map(|n| n.to_string()).unwrap_or_else(|| "null".into()),
            m.has_attachments,
            m.attachment_count,
        );
    }
    println!("]");
}

fn print_mail_detail(
    msg: &task_core::provider::MailMessageDetail,
    body: Option<&str>,
) {
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
            println!("  [{}] {} ({}, {} bytes)", a.id, a.file_name, a.mime, a.size);
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

fn print_mail_detail_json(
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

fn print_emails_table(emails: &[task_core::EmailRef]) {
    if emails.is_empty() {
        println!("No emails linked.");
        return;
    }
    println!(
        "{:<19}  {:<20}  {:<40}  BY",
        "DATE", "FROM", "SUBJECT",
    );
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

fn print_emails_json(emails: &[task_core::EmailRef]) {
    println!("[");
    for (i, e) in emails.iter().enumerate() {
        let comma = if i + 1 < emails.len() { "," } else { "" };
        println!("  {}{comma}", facet_json::to_string(e).unwrap_or_default());
    }
    println!("]");
}

// ── Invoice printing ─────────────────────────────────────────────────────────

fn print_invoices_table(invoices: &[task_core::Invoice]) {
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

fn print_invoices_json(invoices: &[task_core::Invoice]) {
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

fn print_invoice_detail(inv: &task_core::Invoice) {
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
                if p.method.is_empty() { "—" } else { &p.method },
                p.reference.as_deref().unwrap_or(""),
            );
        }
    }
}

fn print_clients_table(clients: &[task_core::Client]) {
    if clients.is_empty() {
        println!("No clients.");
        return;
    }
    let name_w = clients.iter().map(|c| c.name.len()).max().unwrap_or(10).max(10).min(35);
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
        let ccy = if c.currency_code.is_empty() { "—" } else { &c.currency_code };
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

fn print_clients_json(clients: &[task_core::Client]) {
    println!("[");
    for (i, c) in clients.iter().enumerate() {
        let comma = if i + 1 < clients.len() { "," } else { "" };
        let json = facet_json::to_string(c).unwrap_or_default();
        println!("  {json}{comma}");
    }
    println!("]");
}

fn print_client_detail(c: &task_core::Client) {
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

fn require_actor(actor: &Option<String>) -> eyre::Result<String> {
    actor
        .clone()
        .ok_or_else(|| eyre::eyre!("No actor. Use --as <user> or set TASK_USER."))
}

fn print_comments_table(comments: &[Comment]) {
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
        println!(
            "{indent}@{}{date}{tc}{resolved}",
            c.author
        );
        println!("{indent}  {}", c.body);
        println!("{indent}  id: {}", c.id);
    }
    println!("\n{} comment(s)", comments.len());
}

fn print_comments_json(comments: &[Comment]) {
    println!("[");
    for (i, c) in comments.iter().enumerate() {
        let comma = if i + 1 < comments.len() { "," } else { "" };
        let json = facet_json::to_string(c).unwrap_or_default();
        println!("  {json}{comma}");
    }
    println!("]");
}

fn parse_relation_kind(s: &str) -> eyre::Result<RelationType> {
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

fn status_label(s: &Status) -> &'static str {
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

fn priority_label(p: &Priority) -> &'static str {
    match p {
        Priority::None => "—",
        Priority::Low => "Low",
        Priority::Normal => "Normal",
        Priority::High => "High",
        Priority::Urgent => "Urgent",
    }
}

fn print_tasks_table(tasks: &[Task]) {
    if tasks.is_empty() {
        println!("No tasks.");
        return;
    }

    let title_w = tasks
        .iter()
        .map(|t| t.title.len())
        .max()
        .unwrap_or(5)
        .max(5)
        .min(45);

    println!(
        "{:<title_w$}  {:<12}  {:<8}  {:<12}  {}",
        "TITLE", "STATUS", "PRIORITY", "DUE", "URGENCY"
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

fn print_tasks_json(tasks: &[Task]) {
    println!("[");
    for (i, task) in tasks.iter().enumerate() {
        let comma = if i + 1 < tasks.len() { "," } else { "" };
        let json = facet_json::to_string(task)
            .unwrap_or_else(|e| format!("{{\"error\":\"{e}}}\"}}", ));
        println!("  {json}{comma}");
    }
    println!("]");
}

fn print_task_detail(task: &Task) {
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
    if let Some(id) = &task.id {
        println!("ID:       {id}");
    }
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

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}…", &s[..max.saturating_sub(1)])
    }
}

