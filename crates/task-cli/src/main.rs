use chrono::{DateTime, Datelike, NaiveDate, TimeZone, Utc};
use clap::{Args, Parser, Subcommand};
use serde::{Serialize, de::DeserializeOwned};
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
    Status, SyncStats, SystemCapabilities, SystemHealth, Task, TaskRelation, TimeEntry,
    TimeEntryContext, TimeEntryFilter, WikiLink, build_agent_plan, create_project,
    save_project_task,
};
use uuid::Uuid;

#[derive(Parser)]
#[command(name = "task", about = "Task management CLI", version)]
struct Cli {
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
    /// Asset inventory and maintenance tracking
    Asset {
        #[command(subcommand)]
        command: AssetCommands,
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
    /// Nextcloud instance queries (read-only smoke tests)
    Nc {
        #[command(subcommand)]
        command: NcCommands,
    },
    /// Demo data and smoke-test fixture generation
    Demo {
        #[command(subcommand)]
        command: DemoCommands,
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
enum GithubCommands {
    /// Sync GitHub Issues with the vault — pull new issues, push status changes
    Sync {
        /// Repository in owner/repo format (e.g. "FastTrackStudios/task")
        #[arg(long)]
        repo: String,
        /// GitHub token (defaults to GITHUB_TOKEN or GH_TOKEN env)
        #[arg(long, env = "GITHUB_TOKEN")]
        token: Option<String>,
        /// Print what would happen without making changes
        #[arg(long)]
        plan: bool,
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum DemoCommands {
    /// Seed deterministic demo data into an isolated project prefix
    Seed {
        /// Demo organization/workspace name
        #[arg(long, default_value = "Demo")]
        org: String,
        /// Demo billable client name
        #[arg(long, default_value = "Demo Client")]
        client: String,
        /// Project/folder prefix to create or update
        #[arg(long, default_value = "Demo Workflow Smoke")]
        prefix: String,
        /// Print machine-readable summary
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum ServerCommands {
    /// Add or update a named server profile
    Add {
        name: String,
        #[arg(long)]
        url: String,
        #[arg(long)]
        session_token: Option<String>,
        #[arg(long)]
        organization_id: Option<String>,
        #[arg(long)]
        use_now: bool,
    },
    /// List configured server profiles
    List {
        #[arg(long)]
        json: bool,
    },
    /// Select the default server profile
    Use { name: String },
    /// Show the active/default server profile
    Current {
        #[arg(long)]
        json: bool,
    },
    /// Run doctor against a configured server profile
    Doctor {
        name: Option<String>,
        #[arg(long)]
        json: bool,
        #[arg(long)]
        deep: bool,
    },
}

#[derive(Subcommand)]
enum InboxCommands {
    /// List untriaged inbox captures
    List {
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Show daily review buckets
    Daily {
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Show weekly review buckets
    Weekly {
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Show monthly review buckets
    Monthly {
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Show review buckets scoped to a project
    Project {
        name: String,
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Promote/classify an inbox capture
    Promote {
        /// Task id or title
        reference: String,
        /// commitment, idea, task, waiting, reference, someday
        #[arg(long)]
        kind: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        assignee: Option<String>,
        #[arg(long)]
        due: Option<String>,
        #[arg(long)]
        scheduled: Option<String>,
        #[arg(long = "tag")]
        add_tags: Vec<String>,
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum PeopleCommands {
    /// List CardDAV-backed people
    List {
        #[arg(long)]
        addressbook: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List CardDAV-backed organizations
    Orgs {
        #[arg(long)]
        addressbook: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show one person with related tasks/projects/events
    Show {
        reference: String,
        #[arg(long)]
        addressbook: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show one organization with related people/tasks/projects/events
    Org {
        reference: String,
        #[arg(long)]
        addressbook: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum OperatingCommands {
    /// Show the derived life/business operating model
    Model {
        #[arg(long)]
        json: bool,
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
    /// Create a mailbox (folder). To create a Proton label, pass a name
    /// under `Labels/` — e.g. `Labels/project.acme`.
    FolderCreate {
        #[arg(long)]
        account: i64,
        /// Folder name (supports `/` for nesting, e.g. `Folders/clients/acme`)
        #[arg(long)]
        name: String,
        #[arg(long)]
        json: bool,
    },
    /// Delete a mailbox (folder) by id. Removes the Proton label if the
    /// folder is under `Labels/`.
    FolderDelete {
        #[arg(long)]
        mailbox: i64,
    },
    /// Move a message to another mailbox. This is a true move — the
    /// source loses the message. For Proton-style labels that keep a
    /// message in INBOX, use `task email tag set` instead.
    Move {
        #[arg(long, value_name = "ID")]
        email_id: i64,
        #[arg(long)]
        to_folder: i64,
    },
    /// Manage NC Mail tags (IMAP keywords, NC-local)
    Tag {
        #[command(subcommand)]
        cmd: TagCommands,
    },
    /// Return messages in an inbox that are not yet linked to a task
    /// or project and are not tagged `$processed`. Curator / Hermes
    /// use this to find unsorted mail. Output is JSON by default
    /// (agent-friendly).
    Sweep {
        #[arg(long)]
        account: i64,
        /// Mailbox id to scan (default: account's INBOX)
        #[arg(long)]
        mailbox: Option<i64>,
        /// Cap on messages scanned per call
        #[arg(long, default_value = "50")]
        limit: u32,
        /// Filter string (same shape as `search --filter`)
        #[arg(long)]
        filter: Option<String>,
        /// Print a human table instead of JSON
        #[arg(long)]
        table: bool,
    },
    /// Mark a message as triaged by the curator. Applies the
    /// `$processed` NC Mail tag (auto-creating it on first call).
    /// Subsequent sweeps skip tagged messages.
    MarkProcessed {
        #[arg(long, value_name = "ID")]
        email_id: i64,
        /// Optional short note, recorded in the audit log
        #[arg(long)]
        note: Option<String>,
    },
    /// Watch an IMAP mailbox via RFC-2177 IDLE and emit one JSON line
    /// per server-pushed event. Long-running. Intended to run on
    /// starcommand (where ProtonMail Bridge is on 127.0.0.1).
    ///
    /// Credentials: IMAP_PASSWORD env var. The rest are flags.
    Watch {
        #[arg(long, default_value = "127.0.0.1")]
        host: String,
        #[arg(long, default_value = "1143")]
        port: u16,
        #[arg(long)]
        user: String,
        #[arg(long, default_value = "INBOX")]
        mailbox: String,
        /// PEM bundle to verify the server cert against. On starcommand
        /// this is `/var/lib/nc-mail-trust/ca-bundle.crt`.
        #[arg(long)]
        ca_bundle: Option<std::path::PathBuf>,
        /// Disable cert verification. Only safe for loopback.
        #[arg(long)]
        insecure: bool,
    },
}

#[derive(Subcommand)]
enum TagCommands {
    /// List NC Mail tags
    List {
        #[arg(long)]
        json: bool,
    },
    /// Create an NC Mail tag
    Create {
        /// Display name, e.g. "project/acme"
        #[arg(long)]
        name: String,
        /// 7-char hex color, e.g. `#8b5cf6`
        #[arg(long, default_value = "#8b5cf6")]
        color: String,
        #[arg(long)]
        json: bool,
    },
    /// Delete an NC Mail tag
    Delete {
        #[arg(long)]
        account: i64,
        #[arg(long)]
        tag: i64,
    },
    /// Attach an existing tag to a message (by imapLabel).
    Set {
        imap_label: String,
        #[arg(long, value_name = "ID")]
        email_id: i64,
    },
    /// Remove a tag from a message
    Unset {
        imap_label: String,
        #[arg(long, value_name = "ID")]
        email_id: i64,
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
    /// Show billable/unbilled time, invoice balances, and aging
    Report {
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
    Send { id: String },
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
enum ExpenseCommands {
    /// Create a new expense
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
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long)]
        receipt: Option<String>,
        #[arg(long)]
        reference: Option<String>,
        #[arg(long)]
        reimbursable: bool,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List expenses (newest first)
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
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        reimbursable_only: bool,
        #[arg(long)]
        json: bool,
    },
    /// Show a single expense
    Show {
        id: String,
        #[arg(long)]
        md: bool,
        #[arg(long)]
        json: bool,
    },
    /// Show an expense roll-up report
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
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        reimbursable_only: bool,
        #[arg(long)]
        json: bool,
    },
    /// Update an expense
    Update {
        id: String,
        #[arg(long)]
        amount: Option<u64>,
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
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        receipt: Option<String>,
        #[arg(long)]
        reference: Option<String>,
        #[arg(long)]
        reimbursable: Option<bool>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Delete an expense
    Delete { id: String },
}

#[derive(Subcommand)]
enum RevenueCommands {
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
enum AssetCommands {
    /// Create a new asset
    Create {
        name: String,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        manufacturer: Option<String>,
        #[arg(long)]
        model: Option<String>,
        #[arg(long = "serial-number")]
        serial_number: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        space: Option<String>,
        #[arg(long = "rack-or-case")]
        rack_or_case: Option<String>,
        #[arg(long = "assigned-to")]
        assigned_to: Option<String>,
        #[arg(long = "purchase-date")]
        purchase_date: Option<String>,
        #[arg(long = "warranty-until")]
        warranty_until: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long = "cost-cents")]
        cost_cents: Option<u64>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List assets
    List {
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        space: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        query: Option<String>,
        #[arg(long = "needs-repair-only")]
        needs_repair_only: bool,
        #[arg(long)]
        json: bool,
    },
    /// Show a single asset
    Show {
        id: String,
        #[arg(long)]
        md: bool,
        #[arg(long)]
        json: bool,
    },
    /// Show an asset inventory report
    Report {
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        space: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        query: Option<String>,
        #[arg(long = "needs-repair-only")]
        needs_repair_only: bool,
        #[arg(long)]
        json: bool,
    },
    /// Update an asset
    Update {
        id: String,
        #[arg(long)]
        name: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        manufacturer: Option<String>,
        #[arg(long)]
        model: Option<String>,
        #[arg(long = "serial-number")]
        serial_number: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        space: Option<String>,
        #[arg(long = "rack-or-case")]
        rack_or_case: Option<String>,
        #[arg(long = "assigned-to")]
        assigned_to: Option<String>,
        #[arg(long = "purchase-date")]
        purchase_date: Option<String>,
        #[arg(long = "warranty-until")]
        warranty_until: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long = "cost-cents")]
        cost_cents: Option<Option<u64>>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Move an asset to a location / space / rack or case
    Move {
        id: String,
        #[arg(long = "to")]
        location: String,
        #[arg(long)]
        space: Option<String>,
        #[arg(long = "rack-or-case")]
        rack_or_case: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Change an asset status
    Status {
        id: String,
        status: String,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Record maintenance for an asset
    Maintain {
        id: String,
        #[arg(long)]
        date: Option<String>,
        #[arg(long)]
        issue: String,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long)]
        contact: Option<String>,
        #[arg(long = "cost-cents")]
        cost_cents: Option<u64>,
        #[arg(long)]
        warranty: bool,
        #[arg(long)]
        rma: Option<String>,
        #[arg(long)]
        task: Option<String>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Open repair work for an asset and link the created task
    Repair {
        #[command(subcommand)]
        command: AssetRepairCommands,
    },
    /// Reserve an asset for an event, booking, project, or freeform reference
    Reserve {
        id: String,
        #[arg(long = "for")]
        reference: String,
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long = "reserved-by")]
        reserved_by: Option<String>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        force: bool,
        #[arg(long)]
        json: bool,
    },
    /// Release an asset reservation by id or reference
    Release {
        id: String,
        reservation: String,
        #[arg(long)]
        json: bool,
    },
    /// List reservation and availability conflicts
    Conflicts {
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        space: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        query: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Delete an asset
    Delete { id: String },
}

#[derive(Subcommand)]
enum AssetRepairCommands {
    /// Create and link a repair task
    Open {
        id: String,
        #[arg(long)]
        title: String,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long)]
        contact: Option<String>,
        #[arg(long = "cost-cents")]
        cost_cents: Option<u64>,
        #[arg(long)]
        warranty: bool,
        #[arg(long)]
        rma: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum LocationCommands {
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
    Resolve {
        reference: String,
        comment_id: String,
    },
    /// Unresolve a comment
    Reopen {
        reference: String,
        comment_id: String,
    },
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
    Delete { entry_id: String },
}

#[derive(Subcommand)]
enum CalendarCommands {
    /// List calendar events, optionally filtered by an RFC3339 or date range
    List {
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show a calendar event by id or exact title
    Show {
        reference: String,
        #[arg(long)]
        json: bool,
    },
    /// Create a calendar event
    Add {
        #[arg(long)]
        title: String,
        /// Start time: RFC3339, "YYYY-MM-DDTHH:MM", or "YYYY-MM-DD HH:MM"
        #[arg(long)]
        start: String,
        /// End time in the same formats as --start
        #[arg(long)]
        end: Option<String>,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        venue: Option<String>,
        #[arg(long)]
        space: Vec<String>,
        #[arg(long)]
        all_day: bool,
        /// confirmed, tentative, or cancelled
        #[arg(long, default_value = "confirmed")]
        status: String,
        #[arg(long)]
        recurrence: Option<String>,
        #[arg(long)]
        attendee: Vec<String>,
        #[arg(long)]
        asset: Vec<String>,
        #[arg(long = "force-assets")]
        force_assets: bool,
        #[arg(long)]
        json: bool,
    },
    /// Update mutable calendar event fields
    Update {
        reference: String,
        #[arg(long)]
        title: Option<String>,
        #[arg(long)]
        start: Option<String>,
        /// End time, or "clear"
        #[arg(long)]
        end: Option<String>,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        venue: Option<String>,
        #[arg(long)]
        space: Vec<String>,
        #[arg(long)]
        all_day: Option<bool>,
        /// confirmed, tentative, or cancelled
        #[arg(long)]
        status: Option<String>,
        /// RRULE string, or "clear"
        #[arg(long)]
        recurrence: Option<String>,
        /// Replace attendees with comma-separated list. Pass "" to clear.
        #[arg(long)]
        attendees: Option<String>,
        /// Replace markdown body
        #[arg(long)]
        body: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Delete a calendar event by id or exact title
    Delete { reference: String },
    /// CardDAV addressbook controls
    Carddav {
        #[command(subcommand)]
        command: CardDavCommands,
    },
}

#[derive(Subcommand)]
enum CardDavCommands {
    /// Discover addressbooks
    Discover {
        #[arg(long)]
        json: bool,
    },
    /// Sync an addressbook and print vCard objects
    Sync {
        #[arg(long, default_value = "contacts")]
        addressbook: String,
        #[arg(long)]
        sync_token: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum AgentCommands {
    /// Return one machine-readable snapshot of the vault
    Snapshot {
        #[arg(long, default_value = "50")]
        activity_limit: u32,
        #[arg(long, default_value = "50")]
        conflict_limit: u32,
        /// Include completed/cancelled/archived tasks
        #[arg(long)]
        include_completed: bool,
    },
    /// Return one task by id or title
    Task { reference: String },
    /// Build a machine-readable execution plan for a task
    Plan { reference: String },
    /// Return one project with stats, next task, and project tasks
    Project { name: String },
    /// Return calendar events in a range
    Calendar {
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
    },
    /// Return time entries with the same filters as `task time list`
    Time {
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
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        billable: bool,
    },
    /// Return sync status, optionally triggering a sync first
    Sync {
        #[arg(long)]
        trigger: bool,
    },
    /// Describe the installable CLI surface for agents
    Capabilities,
    /// Print machine-readable bootstrap instructions for agents
    Bootstrap {
        #[arg(long)]
        json: bool,
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
    /// Show the project dashboard / portfolio view
    Dashboard {
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
    /// Show project tasks, next action, references, and storage-backed files
    Context {
        name: String,
        #[arg(long)]
        files: bool,
        #[arg(long, default_value = "1")]
        depth: String,
        #[arg(long)]
        json: bool,
    },
    /// Threaded comments on a project
    Comment {
        #[command(subcommand)]
        command: ProjectCommentCommands,
    },
    /// Edit project fields — status, client, rate, email_tags, etc.
    Edit(Box<ProjectEditArgs>),
    /// Show a single project
    Show {
        name: String,
        #[arg(long)]
        json: bool,
    },
}

/// Arguments for `ProjectCommands::Edit`. Boxed inside the variant so
/// the [`ProjectCommands`] enum doesn't blow up its size — every other
/// variant only carries a name + a `--json` flag.
#[derive(Args)]
struct ProjectEditArgs {
    pub name: String,
    #[arg(long)]
    pub status: Option<String>,
    #[arg(long)]
    pub description: Option<String>,
    #[arg(long)]
    pub area: Option<String>,
    #[arg(long)]
    pub organization: Option<String>,
    /// Pass "clear" to remove.
    #[arg(long)]
    pub client: Option<String>,
    /// Billable rate in cents/hr; 0 clears.
    #[arg(long)]
    pub default_rate: Option<u32>,
    #[arg(long)]
    pub identifier: Option<String>,
    #[arg(long)]
    pub lead: Option<String>,
    #[arg(long)]
    pub default_assignee: Option<String>,
    #[arg(long)]
    pub emoji: Option<String>,
    #[arg(long)]
    pub repo: Option<String>,
    #[arg(long)]
    pub dev_path: Option<String>,
    #[arg(long)]
    pub project_type: Option<String>,
    #[arg(long)]
    pub workflow: Option<String>,
    #[arg(long)]
    pub workflow_stage: Option<String>,
    /// YYYY-MM-DD or "clear"
    #[arg(long)]
    pub due: Option<String>,
    #[arg(long)]
    pub start: Option<String>,
    #[arg(long)]
    pub add_tag: Vec<String>,
    #[arg(long)]
    pub remove_tag: Vec<String>,
    #[arg(long)]
    pub add_email_tag: Vec<String>,
    #[arg(long)]
    pub remove_email_tag: Vec<String>,
    #[arg(long)]
    pub add_team: Vec<String>,
    #[arg(long)]
    pub remove_team: Vec<String>,
    #[arg(long)]
    pub json: bool,
}

#[derive(Subcommand)]
enum ProjectCommentCommands {
    /// Add a comment to a project
    Add {
        project: String,
        #[arg(long)]
        body: String,
    },
    /// List comments on a project
    List {
        project: String,
        #[arg(long)]
        json: bool,
    },
    /// Reply to an existing project comment by id
    Reply {
        project: String,
        parent_id: String,
        #[arg(long)]
        body: String,
    },
    /// Mark a project comment resolved (by id)
    Resolve { project: String, comment_id: String },
    /// Unresolve a project comment
    Reopen { project: String, comment_id: String },
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
            return run_talk(talk, actor).await;
        }
    }
    // Nc smoke-test commands — same deal, no vault.
    if let Commands::Nc { command: nc } = command {
        return run_nc(nc, actor).await;
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
        return run_server_command(command).await;
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

    let vault_path = vault.ok_or_else(|| {
        eyre::eyre!("No vault specified. Use --vault <path> or set TASK_VAULT env var.")
    })?;

    if let Commands::Demo { command } = command {
        return run_demo_command(&vault_path, command, actor.as_deref());
    }

    let _ = vault_path;
    let _ = actor;
    let _ = command;
    eyre::bail!(
        "local markdown command execution has been removed; run task-server and pass --server/TASK_SERVER so commands use generated repo services"
    );
}

#[derive(Debug, Clone, Default)]
struct ServerProfiles {
    default: Option<String>,
    servers: Vec<ServerProfile>,
}

#[derive(Debug, Clone, Default)]
struct ServerProfile {
    name: String,
    url: String,
    session_token: Option<String>,
    organization_id: Option<String>,
}

impl ServerProfiles {
    fn resolve(&self, name_or_url: &str) -> Option<ServerProfile> {
        let requested_url = normalize_profile_url(name_or_url);
        self.servers
            .iter()
            .find(|profile| profile.name == name_or_url)
            .or_else(|| {
                self.servers
                    .iter()
                    .find(|profile| normalize_profile_url(&profile.url) == requested_url)
            })
            .cloned()
            .or_else(|| {
                if name_or_url == "default" {
                    self.current()
                } else {
                    None
                }
            })
    }

    fn current(&self) -> Option<ServerProfile> {
        self.default
            .as_deref()
            .and_then(|name| self.servers.iter().find(|p| p.name == name))
            .cloned()
    }
}

#[derive(Debug, Clone)]
struct RemoteVoxConfig {
    vox_url: String,
    display_url: String,
    profile_name: Option<String>,
}

impl RemoteVoxConfig {
    fn new(
        server: String,
        session_token: Option<String>,
        organization_id: Option<String>,
    ) -> eyre::Result<Self> {
        let profile = load_server_profiles()
            .ok()
            .and_then(|config| config.resolve(&server));
        let server_url = profile
            .as_ref()
            .map(|profile| profile.url.clone())
            .unwrap_or(server);
        let token = session_token
            .or_else(|| {
                profile
                    .as_ref()
                    .and_then(|profile| profile.session_token.clone())
            })
            .filter(|s| !s.is_empty())
            .ok_or_else(|| {
                eyre::eyre!("Remote mode requires --session-token or TASK_SESSION_TOKEN.")
            })?;
        let organization_id = organization_id.or_else(|| {
            profile
                .as_ref()
                .and_then(|profile| profile.organization_id.clone())
        });
        let base_vox_url = normalize_vox_url(&server_url);
        let mut vox_url = base_vox_url.clone();
        append_query_param(&mut vox_url, "token", &token);
        let mut display_url = base_vox_url;
        append_query_param(&mut display_url, "token", "<redacted>");
        if let Some(org) = organization_id.filter(|s| !s.is_empty()) {
            append_query_param(&mut vox_url, "organization_id", &org);
            append_query_param(&mut display_url, "organization_id", &org);
        }
        Ok(Self {
            vox_url,
            display_url,
            profile_name: profile.map(|profile| profile.name),
        })
    }

    async fn task_repo(&self) -> eyre::Result<task_core::task::TaskRepoClient> {
        self.connect().await
    }

    async fn project_repo(&self) -> eyre::Result<task_core::project::ProjectRepoClient> {
        self.connect().await
    }

    async fn client_repo(&self) -> eyre::Result<task_core::client::ClientRepoClient> {
        self.connect().await
    }

    async fn expense_repo(&self) -> eyre::Result<task_core::expense::ExpenseRepoClient> {
        self.connect().await
    }

    async fn invoice_repo(&self) -> eyre::Result<task_core::invoice::InvoiceRepoClient> {
        self.connect().await
    }

    async fn calendar_event_repo(
        &self,
    ) -> eyre::Result<task_core::calendar_event::CalendarEventRepoClient> {
        self.connect().await
    }

    async fn asset_repo(&self) -> eyre::Result<task_core::asset::AssetRepoClient> {
        self.connect().await
    }

    async fn task(&self) -> eyre::Result<task_core::service::TaskServiceClient> {
        self.connect().await
    }

    async fn inbox(&self) -> eyre::Result<task_core::service::InboxServiceClient> {
        self.connect().await
    }

    async fn project(&self) -> eyre::Result<task_core::service::ProjectServiceClient> {
        self.connect().await
    }

    async fn time(&self) -> eyre::Result<task_core::service::TimeServiceClient> {
        self.connect().await
    }

    async fn people(&self) -> eyre::Result<task_core::service::PeopleServiceClient> {
        self.connect().await
    }

    async fn conversation(&self) -> eyre::Result<task_core::service::ConversationServiceClient> {
        self.connect().await
    }

    async fn operating(&self) -> eyre::Result<task_core::service::OperatingServiceClient> {
        self.connect().await
    }

    async fn invoice(&self) -> eyre::Result<task_core::service::InvoiceServiceClient> {
        self.connect().await
    }

    async fn activity(&self) -> eyre::Result<task_core::service::ActivityServiceClient> {
        self.connect().await
    }

    async fn mail(&self) -> eyre::Result<task_core::service::MailServiceClient> {
        self.connect().await
    }

    async fn calendar(&self) -> eyre::Result<task_core::service::CalendarServiceClient> {
        self.connect().await
    }

    async fn system(&self) -> eyre::Result<task_core::service::SystemServiceClient> {
        self.connect().await
    }

    async fn connect<C>(&self) -> eyre::Result<C>
    where
        C: vox::FromVoxSession,
    {
        vox::connect(&self.vox_url)
            .establish()
            .await
            .map_err(|e| eyre::eyre!("Remote Vox connection failed: {e}"))
    }
}

async fn run_remote_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: Commands,
) -> eyre::Result<()> {
    match command {
        Commands::Agent { command } => run_remote_agent_command(remote, actor, command).await?,
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
            let task = build_new_task(NewTaskInput {
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
            apply_task_update(
                &mut task,
                TaskUpdateInput {
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

        Commands::Comment { command } => run_remote_comment_command(remote, actor, command).await?,

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
            run_github_command_remote(remote, command).await?;
        }
        Commands::Project { command } => run_remote_project_command(remote, actor, command).await?,
        Commands::Client { command } => run_remote_client_command(remote, command).await?,
        Commands::Invoice { command } => run_remote_invoice_command(remote, actor, command).await?,
        Commands::Expense { command } => run_remote_expense_command(remote, actor, command).await?,
        Commands::Revenue { .. } => {
            eyre::bail!("revenue commands are currently supported only in local vault mode")
        }
        Commands::Asset { command } => run_remote_asset_command(remote, actor, command).await?,
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
        Commands::Time { command } => run_remote_time_command(remote, actor, command).await?,
        Commands::Calendar { command } => run_remote_calendar_command(remote, command).await?,
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
        Commands::Email { command } => run_remote_email_command(remote, actor, command).await?,
        Commands::Demo { .. } => {
            return Err(eyre::eyre!(
                "demo commands seed local vault files; omit --server and pass --vault or TASK_VAULT"
            ));
        }
        Commands::Nc { .. } => {
            unreachable!("handled before remote dispatch")
        }
    }
    Ok(())
}

async fn remote_find_task(remote: &RemoteVoxConfig, reference: &str) -> eyre::Result<Task> {
    let client = remote.task_repo().await?;
    remote_find_task_with_client(&client, reference).await
}

async fn remote_find_task_with_client(
    client: &task_core::task::TaskRepoClient,
    reference: &str,
) -> eyre::Result<Task> {
    let tasks = remote_list_tasks_with_client(client).await?;
    find_task_in(tasks, reference)
}

async fn remote_list_tasks_with_client(
    client: &task_core::task::TaskRepoClient,
) -> eyre::Result<Vec<Task>> {
    client
        .list_tasks(None, None, None, Some(10_000))
        .await?
        .into_iter()
        .map(api_to_model)
        .collect()
}

async fn remote_update_task_with_client(
    client: &task_core::task::TaskRepoClient,
    task: &Task,
) -> eyre::Result<Task> {
    let update: task_core::task::TaskApiUpdate = model_to_api(task)?;
    let updated = client.update_task(task.id.to_string(), update).await?;
    api_to_model(updated)
}

async fn remote_list_projects_with_client(
    client: &task_core::project::ProjectRepoClient,
) -> eyre::Result<Vec<Project>> {
    client
        .list_projects(None, None, None, Some(10_000))
        .await?
        .into_iter()
        .map(api_to_model)
        .collect()
}

async fn remote_find_project_with_client(
    client: &task_core::project::ProjectRepoClient,
    reference: &str,
) -> eyre::Result<Project> {
    remote_list_projects_with_client(client)
        .await?
        .into_iter()
        .find(|project| project.title.eq_ignore_ascii_case(reference))
        .ok_or_else(|| eyre::eyre!("Project not found: {reference}"))
}

async fn remote_update_project_with_client(
    client: &task_core::project::ProjectRepoClient,
    project: &Project,
) -> eyre::Result<Project> {
    let update: task_core::project::ProjectApiUpdate = model_to_api(project)?;
    let updated = client
        .update_project(project.id.to_string(), update)
        .await?;
    api_to_model(updated)
}

async fn remote_list_clients_with_client(
    client: &task_core::client::ClientRepoClient,
) -> eyre::Result<Vec<Client>> {
    client
        .list_clients(None, None, None, Some(10_000))
        .await?
        .into_iter()
        .map(api_to_model)
        .collect()
}

async fn remote_find_client_with_client(
    client: &task_core::client::ClientRepoClient,
    name: &str,
) -> eyre::Result<Option<Client>> {
    Ok(remote_list_clients_with_client(client)
        .await?
        .into_iter()
        .find(|client| client.name.eq_ignore_ascii_case(name)))
}

async fn remote_save_client_with_client(
    client: &task_core::client::ClientRepoClient,
    item: &Client,
) -> eyre::Result<Client> {
    if item.id == Uuid::nil() {
        let create: task_core::client::ClientApiCreate = model_to_api(item)?;
        api_to_model(client.create_client(create).await?)
    } else {
        let update: task_core::client::ClientApiUpdate = model_to_api(item)?;
        api_to_model(client.update_client(item.id.to_string(), update).await?)
    }
}

async fn remote_list_invoices_with_client(
    client: &task_core::invoice::InvoiceRepoClient,
) -> eyre::Result<Vec<Invoice>> {
    client
        .list_invoices(None, None, None, Some(10_000))
        .await?
        .into_iter()
        .map(api_to_model)
        .collect()
}

async fn remote_find_invoice_with_client(
    client: &task_core::invoice::InvoiceRepoClient,
    id: &str,
) -> eyre::Result<Option<Invoice>> {
    Ok(remote_list_invoices_with_client(client)
        .await?
        .into_iter()
        .find(|invoice| invoice.id.eq_ignore_ascii_case(id) || invoice.uuid.to_string() == id))
}

async fn remote_list_expenses_with_client(
    client: &task_core::expense::ExpenseRepoClient,
) -> eyre::Result<Vec<task_core::Expense>> {
    client
        .list_expenses(None, None, None, Some(10_000))
        .await?
        .into_iter()
        .map(api_to_model)
        .collect()
}

async fn remote_find_expense_with_client(
    client: &task_core::expense::ExpenseRepoClient,
    id: &str,
) -> eyre::Result<Option<task_core::Expense>> {
    Ok(remote_list_expenses_with_client(client)
        .await?
        .into_iter()
        .find(|expense| expense.id.eq_ignore_ascii_case(id) || expense.uuid.to_string() == id))
}

async fn remote_list_calendar_events_with_client(
    client: &task_core::calendar_event::CalendarEventRepoClient,
) -> eyre::Result<Vec<CalendarEvent>> {
    client
        .list_calendar_events(None, None, None, Some(10_000))
        .await?
        .into_iter()
        .map(api_to_model)
        .collect()
}

async fn remote_find_calendar_event_with_client(
    client: &task_core::calendar_event::CalendarEventRepoClient,
    reference: &str,
) -> eyre::Result<CalendarEvent> {
    remote_list_calendar_events_with_client(client)
        .await?
        .into_iter()
        .find(|event| {
            event.uuid.to_string() == reference
                || event.id.as_deref() == Some(reference)
                || event.title.eq_ignore_ascii_case(reference)
        })
        .ok_or_else(|| eyre::eyre!("Calendar event not found: {reference}"))
}

async fn remote_update_calendar_event_with_client(
    client: &task_core::calendar_event::CalendarEventRepoClient,
    event: &CalendarEvent,
) -> eyre::Result<CalendarEvent> {
    let update: task_core::calendar_event::CalendarEventApiUpdate = model_to_api(event)?;
    api_to_model(
        client
            .update_calendar_event(event.uuid.to_string(), update)
            .await?,
    )
}

async fn remote_create_expense_with_client(
    client: &task_core::expense::ExpenseRepoClient,
    request: ExpenseCreateRequest,
) -> eyre::Result<task_core::Expense> {
    let now = Utc::now();
    let date = request.date.unwrap_or_else(|| now.date_naive());
    let number = remote_list_expenses_with_client(client)
        .await?
        .into_iter()
        .filter(|expense| expense.date.year() == date.year())
        .map(|expense| expense.number)
        .max()
        .unwrap_or(0)
        + 1;
    let expense = task_core::Expense {
        uuid: Uuid::new_v4(),
        id: task_core::expense::format_expense_id(date.year(), number),
        number,
        status: request
            .status
            .as_deref()
            .and_then(task_core::expense::parse_expense_status)
            .unwrap_or(task_core::ExpenseStatus::Draft),
        date,
        amount_cents: request.amount_cents,
        currency_code: request.currency_code.unwrap_or_else(|| "USD".into()),
        project: request.project.map(WikiLink),
        client: request.client.map(WikiLink),
        deliverable: request.deliverable,
        category: request.category,
        vendor: request.vendor,
        description: request.description,
        receipt: request.receipt,
        reference: request.reference,
        reimbursable: request.reimbursable,
        notes: request.notes,
        created_by: request.actor,
        date_created: Some(now),
        date_modified: Some(now),
        body: String::new(),
    };
    let create: task_core::expense::ExpenseApiCreate = model_to_api(&expense)?;
    api_to_model(client.create_expense(create).await?)
}

async fn remote_update_expense_with_client(
    client: &task_core::expense::ExpenseRepoClient,
    id: &str,
    patch: ExpensePatch,
) -> eyre::Result<task_core::Expense> {
    let mut expense = remote_find_expense_with_client(client, id)
        .await?
        .ok_or_else(|| eyre::eyre!("Expense not found: {id}"))?;
    if let Some(status) = patch.status.as_deref() {
        expense.status = task_core::expense::parse_expense_status(status)
            .ok_or_else(|| eyre::eyre!("invalid expense status: {status}"))?;
    }
    if let Some(date) = patch.date.as_deref() {
        expense.date = date.parse::<NaiveDate>()?;
    }
    if let Some(amount) = patch.amount_cents {
        expense.amount_cents = amount;
    }
    if let Some(currency) = patch.currency_code {
        expense.currency_code = currency;
    }
    if let Some(project) = patch.project {
        expense.project = if project.trim().is_empty() {
            None
        } else {
            Some(WikiLink(project))
        };
    }
    if let Some(client_name) = patch.client {
        expense.client = if client_name.trim().is_empty() {
            None
        } else {
            Some(WikiLink(client_name))
        };
    }
    if let Some(value) = patch.deliverable {
        expense.deliverable = if value.trim().is_empty() {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.category {
        expense.category = if value.trim().is_empty() {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.vendor {
        expense.vendor = if value.trim().is_empty() {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.description {
        expense.description = value;
    }
    if let Some(value) = patch.receipt {
        expense.receipt = if value.trim().is_empty() {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.reference {
        expense.reference = if value.trim().is_empty() {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.reimbursable {
        expense.reimbursable = value;
    }
    if let Some(value) = patch.notes {
        expense.notes = if value.trim().is_empty() {
            None
        } else {
            Some(value)
        };
    }
    expense.date_modified = Some(Utc::now());
    let update: task_core::expense::ExpenseApiUpdate = model_to_api(&expense)?;
    api_to_model(
        client
            .update_expense(expense.uuid.to_string(), update)
            .await?,
    )
}

fn apply_remote_project_patch(
    project: &mut Project,
    patch: task_core::ProjectPatch,
) -> eyre::Result<()> {
    if let Some(status) = patch.status {
        project.status = parse_project_status(&status)
            .ok_or_else(|| eyre::eyre!("Unknown project status: {status}"))?;
    }
    if let Some(value) = patch.description {
        project.description = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.body {
        project.body = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.area {
        project.area = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.organization {
        project.organization = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.project_type {
        project.project_type = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.workflow {
        project.workflow = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.workflow_stage {
        project.workflow_stage = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.identifier {
        project.identifier = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.lead {
        project.lead = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.default_assignee {
        project.default_assignee = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.emoji {
        project.emoji = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.repo {
        project.repo = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.dev_path {
        project.dev_path = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.client {
        project.client = if value.is_empty() || value == "clear" {
            None
        } else {
            Some(WikiLink(value))
        };
    }
    if let Some(value) = patch.default_rate {
        project.default_rate = if value == 0 { None } else { Some(value) };
    }
    if let Some(value) = patch.due {
        project.due = parse_optional_date(&value)?;
    }
    if let Some(value) = patch.start {
        project.start = parse_optional_date(&value)?;
    }
    for value in patch.remove_tag {
        project.tags.retain(|tag| tag != &value);
    }
    for value in patch.add_tag {
        if !project.tags.contains(&value) {
            project.tags.push(value);
        }
    }
    for value in patch.remove_email_tag {
        project.email_tags.retain(|tag| tag != &value);
    }
    for value in patch.add_email_tag {
        if !project.email_tags.contains(&value) {
            project.email_tags.push(value);
        }
    }
    for value in patch.remove_team {
        project.team.retain(|member| member != &value);
    }
    for value in patch.add_team {
        if !project.team.contains(&value) {
            project.team.push(value);
        }
    }
    Ok(())
}

/// Typed inputs for [`build_new_task`].
struct NewTaskInput {
    title: String,
    priority: Option<String>,
    status: Option<String>,
    due: Option<String>,
    scheduled: Option<String>,
    project: Option<String>,
    context: Option<String>,
    tag: Option<String>,
    recurrence: Option<String>,
    assignee: Option<String>,
    actor: Option<String>,
}

fn build_new_task(input: NewTaskInput) -> eyre::Result<Task> {
    let NewTaskInput {
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
        actor,
    } = input;
    Ok(Task {
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
            .map(|d| {
                d.parse::<chrono::NaiveDate>()
                    .map_err(|e| eyre::eyre!("{e}"))
            })
            .transpose()?,
        scheduled: scheduled
            .as_deref()
            .map(|d| {
                d.parse::<chrono::NaiveDate>()
                    .map_err(|e| eyre::eyre!("{e}"))
            })
            .transpose()?,
        projects: project
            .map(|p| vec![WikiLink(p)])
            .unwrap_or_default()
            .into(),
        contexts: context.map(|c| vec![c]).unwrap_or_default().into(),
        tags: tag.map(|t| vec![t]).unwrap_or_default().into(),
        recurrence,
        assignee,
        created_by: actor,
        ..Task::default()
    })
}

fn model_to_api<T, U>(value: &T) -> eyre::Result<U>
where
    T: Serialize,
    U: DeserializeOwned,
{
    serde_json::from_value(serde_json::to_value(value)?).map_err(Into::into)
}

fn api_to_model<T, U>(value: T) -> eyre::Result<U>
where
    T: Serialize,
    U: DeserializeOwned,
{
    model_to_api(&value)
}

/// Typed inputs for [`apply_task_update`].
struct TaskUpdateInput {
    title: Option<String>,
    status: Option<String>,
    priority: Option<String>,
    due: Option<String>,
    scheduled: Option<String>,
    assignee: Option<String>,
    add_tag: Vec<String>,
    remove_tag: Vec<String>,
    add_project: Vec<String>,
    remove_project: Vec<String>,
    add_context: Vec<String>,
    remove_context: Vec<String>,
    recurrence: Option<String>,
    body: Option<String>,
}

fn apply_task_update(task: &mut Task, input: TaskUpdateInput) -> eyre::Result<()> {
    let TaskUpdateInput {
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
    } = input;
    if let Some(t) = title {
        task.title = t;
    }
    if let Some(s) = status {
        task.status = parse_status(&s).ok_or_else(|| eyre::eyre!("Unknown status: {s}"))?;
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
        task.assignee = if a == "clear" || a.is_empty() {
            None
        } else {
            Some(a)
        };
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
    Ok(())
}

async fn run_remote_comment_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: CommentCommands,
) -> eyre::Result<()> {
    let client = remote.task_repo().await?;
    match command {
        CommentCommands::Add {
            reference,
            body,
            timecode,
        } => {
            let author = require_actor(&actor.map(str::to_string))?;
            let mut task = remote_find_task_with_client(&client, &reference).await?;
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
            remote_update_task_with_client(&client, &task).await?;
            println!("Comment added ({}).", new_comment.id);
        }
        CommentCommands::List { reference, json } => {
            let task = remote_find_task_with_client(&client, &reference).await?;
            let comments = parse_comments(&task.body);
            if json {
                print_comments_json(&comments);
            } else {
                print_comments_table(&comments);
            }
        }
        CommentCommands::Reply {
            reference,
            parent_id,
            body,
        } => {
            let author = require_actor(&actor.map(str::to_string))?;
            let mut task = remote_find_task_with_client(&client, &reference).await?;
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
            remote_update_task_with_client(&client, &task).await?;
            println!("Reply added ({}).", new_comment.id);
        }
        CommentCommands::Resolve {
            reference,
            comment_id,
        } => {
            let resolver = require_actor(&actor.map(str::to_string))?;
            let mut task = remote_find_task_with_client(&client, &reference).await?;
            let mut comments = parse_comments(&task.body);
            let Some(c) = comments.iter_mut().find(|c| c.id == comment_id) else {
                eyre::bail!("No comment with id {comment_id}");
            };
            c.resolved = true;
            c.resolved_by = Some(resolver);
            task.body = splice_comments(&task.body, &comments);
            remote_update_task_with_client(&client, &task).await?;
            println!("Resolved comment {comment_id}.");
        }
        CommentCommands::Reopen {
            reference,
            comment_id,
        } => {
            let mut task = remote_find_task_with_client(&client, &reference).await?;
            let mut comments = parse_comments(&task.body);
            let Some(c) = comments.iter_mut().find(|c| c.id == comment_id) else {
                eyre::bail!("No comment with id {comment_id}");
            };
            c.resolved = false;
            c.resolved_by = None;
            task.body = splice_comments(&task.body, &comments);
            remote_update_task_with_client(&client, &task).await?;
            println!("Reopened comment {comment_id}.");
        }
    }
    Ok(())
}

async fn run_remote_project_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: ProjectCommands,
) -> eyre::Result<()> {
    let service = remote.project().await?;
    let repo = remote.project_repo().await?;
    match command {
        ProjectCommands::List { json } => {
            let projects = remote_list_projects_with_client(&repo).await?;
            if json {
                println!("{}", projects_json(&projects));
            } else {
                print_projects_table(&projects);
            }
        }
        ProjectCommands::Dashboard { json } => {
            let dashboard = service.project_dashboard().await?;
            print_project_dashboard(&dashboard, json);
        }
        ProjectCommands::Stats { name, json } => {
            let stats = service.project_stats(name.clone()).await?;
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
        ProjectCommands::Next { name, json } => match service.next_task(name.clone()).await? {
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
        ProjectCommands::Tasks { name, json } => {
            let tasks = service.tasks_for_project(name).await?;
            if json {
                print_tasks_json(&tasks);
            } else {
                print_tasks_table(&tasks);
            }
        }
        ProjectCommands::Context {
            name,
            files,
            depth,
            json,
        } => {
            let context = service.project_context(name, files, depth).await?;
            print_project_context(context.as_ref(), json);
        }
        ProjectCommands::Comment {
            command: ProjectCommentCommands::Add { project, body },
        } => {
            let author = require_actor(&actor.map(str::to_string))?;
            let mut project_item = remote_find_project_with_client(&repo, &project).await?;
            let now = chrono::Local::now().naive_local();
            let mentions = Comment::extract_mentions(&body);
            let comments = parse_comments(project_item.body.as_deref().unwrap_or(""));
            let new_comment = Comment {
                id: Comment::generate_id(&author, Some(now), &body),
                author,
                body,
                created_at: Some(now),
                mentions,
                ..Default::default()
            };
            let mut comments = comments;
            comments.push(new_comment.clone());
            project_item.body = Some(splice_comments(
                project_item.body.as_deref().unwrap_or(""),
                &comments,
            ));
            remote_update_project_with_client(&repo, &project_item).await?;
            println!("Comment added ({}).", new_comment.id);
        }
        ProjectCommands::Comment {
            command: ProjectCommentCommands::List { project, json },
        } => {
            let project_item = remote_find_project_with_client(&repo, &project).await?;
            let comments = parse_comments(project_item.body.as_deref().unwrap_or(""));
            if json {
                print_comments_json(&comments);
            } else {
                print_comments_table(&comments);
            }
        }
        ProjectCommands::Comment {
            command:
                ProjectCommentCommands::Reply {
                    project,
                    parent_id,
                    body,
                },
        } => {
            let author = require_actor(&actor.map(str::to_string))?;
            let mut project_item = remote_find_project_with_client(&repo, &project).await?;
            let mut comments = parse_comments(project_item.body.as_deref().unwrap_or(""));
            if !comments.iter().any(|c| c.id == parent_id) {
                eyre::bail!("No comment with id {parent_id} on project {project}");
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
            project_item.body = Some(splice_comments(
                project_item.body.as_deref().unwrap_or(""),
                &comments,
            ));
            remote_update_project_with_client(&repo, &project_item).await?;
            println!("Reply added ({}).", new_comment.id);
        }
        ProjectCommands::Comment {
            command:
                ProjectCommentCommands::Resolve {
                    project,
                    comment_id,
                },
        } => {
            let resolver = require_actor(&actor.map(str::to_string))?;
            let mut project_item = remote_find_project_with_client(&repo, &project).await?;
            let mut comments = parse_comments(project_item.body.as_deref().unwrap_or(""));
            let Some(c) = comments.iter_mut().find(|c| c.id == comment_id) else {
                eyre::bail!("No comment with id {comment_id}");
            };
            c.resolved = true;
            c.resolved_by = Some(resolver);
            project_item.body = Some(splice_comments(
                project_item.body.as_deref().unwrap_or(""),
                &comments,
            ));
            remote_update_project_with_client(&repo, &project_item).await?;
            println!("Resolved comment {comment_id}.");
        }
        ProjectCommands::Comment {
            command:
                ProjectCommentCommands::Reopen {
                    project,
                    comment_id,
                },
        } => {
            let mut project_item = remote_find_project_with_client(&repo, &project).await?;
            let mut comments = parse_comments(project_item.body.as_deref().unwrap_or(""));
            let Some(c) = comments.iter_mut().find(|c| c.id == comment_id) else {
                eyre::bail!("No comment with id {comment_id}");
            };
            c.resolved = false;
            c.resolved_by = None;
            project_item.body = Some(splice_comments(
                project_item.body.as_deref().unwrap_or(""),
                &comments,
            ));
            remote_update_project_with_client(&repo, &project_item).await?;
            println!("Reopened comment {comment_id}.");
        }
        ProjectCommands::Edit(args) => {
            let ProjectEditArgs {
                name,
                status,
                description,
                area,
                organization,
                client: project_client,
                default_rate,
                identifier,
                lead,
                default_assignee,
                emoji,
                repo: project_repo_url,
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
            } = *args;
            let patch = task_core::ProjectPatch {
                status,
                description,
                body: None,
                area,
                organization,
                project_type,
                workflow,
                workflow_stage,
                identifier,
                lead,
                default_assignee,
                emoji,
                repo: project_repo_url,
                dev_path,
                client: project_client,
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
            let mut project = remote_find_project_with_client(&repo, &name).await?;
            apply_remote_project_patch(&mut project, patch)?;
            let updated = remote_update_project_with_client(&repo, &project).await?;
            if json {
                println!("{}", facet_json::to_string(&updated).unwrap_or_default());
            } else {
                println!("Updated project '{}'.", updated.title);
            }
        }
        ProjectCommands::Show { name, json } => {
            let projects = remote_list_projects_with_client(&repo).await?;
            let project = projects
                .into_iter()
                .find(|p| p.title.eq_ignore_ascii_case(&name))
                .ok_or_else(|| eyre::eyre!("Project not found: {name}"))?;
            if json {
                println!("{}", facet_json::to_string(&project).unwrap_or_default());
            } else {
                print_project_detail(&project);
            }
        }
    }
    Ok(())
}

async fn run_remote_client_command(
    remote: &RemoteVoxConfig,
    command: ClientCommands,
) -> eyre::Result<()> {
    let client = remote.client_repo().await?;
    match command {
        ClientCommands::Add {
            name,
            rate,
            currency,
            terms_days,
            email,
            contact,
            phone,
            invoice_ninja_id,
        } => {
            let existing = remote_find_client_with_client(&client, &name).await?;
            let mut item = existing.unwrap_or_else(|| task_core::Client {
                name: name.clone(),
                ..Default::default()
            });
            if let Some(r) = rate {
                item.default_hourly_rate = Some(r);
            }
            if let Some(c) = currency {
                item.currency_code = c;
            }
            if let Some(d) = terms_days {
                item.payment_terms_days = Some(d);
            }
            if let Some(e) = email {
                item.email = Some(e);
            }
            if let Some(c) = contact {
                item.contact_name = Some(c);
            }
            if let Some(p) = phone {
                item.phone = Some(p);
            }
            if let Some(id) = invoice_ninja_id {
                item.invoice_ninja_id = Some(id);
            }
            let saved = remote_save_client_with_client(&client, &item).await?;
            println!(
                "Saved client '{}' (rate {}¢/hr).",
                saved.name,
                saved.default_hourly_rate.unwrap_or(0)
            );
        }
        ClientCommands::List { json } => {
            let clients = remote_list_clients_with_client(&client).await?;
            if json {
                print_clients_json(&clients);
            } else {
                print_clients_table(&clients);
            }
        }
        ClientCommands::Show { name, json } => {
            let item = remote_find_client_with_client(&client, &name)
                .await?
                .ok_or_else(|| eyre::eyre!("Client not found: {name}"))?;
            if json {
                println!("{}", facet_json::to_string(&item).unwrap_or_default());
            } else {
                print_client_detail(&item);
            }
        }
    }
    Ok(())
}

async fn run_remote_invoice_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: InvoiceCommands,
) -> eyre::Result<()> {
    let service = remote.invoice().await?;
    let repo = remote.invoice_repo().await?;
    match command {
        InvoiceCommands::Create {
            client: client_name,
            from,
            to,
            rate,
            tax,
            discount,
            po,
            notes,
            json,
        } => {
            let invoice = service
                .create_invoice_from_entries(task_core::InvoiceCreateRequest {
                    client_name,
                    from: from.as_deref().map(parse_date_start).transpose()?,
                    to: to.as_deref().map(parse_date_end).transpose()?,
                    fallback_rate: rate,
                    tax_rate_percent: tax,
                    discount_percent: discount,
                    po_number: po,
                    public_notes: notes,
                    actor: actor.map(str::to_string),
                })
                .await?;
            if json {
                println!("{}", facet_json::to_string(&invoice).unwrap_or_default());
            } else {
                print_invoice_detail(&invoice);
            }
        }
        InvoiceCommands::List {
            status,
            client: client_name,
            year,
            json,
        } => {
            let invoices: Vec<task_core::Invoice> = remote_list_invoices_with_client(&repo)
                .await?
                .into_iter()
                .filter(|i| match &status {
                    Some(s) => format!("{:?}", i.status).eq_ignore_ascii_case(s),
                    None => true,
                })
                .filter(|i| match &client_name {
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
        InvoiceCommands::Report { json } => {
            let report = service.finance_report().await?;
            print_finance_report(&report, json);
        }
        InvoiceCommands::Show { id, md, json } => {
            let invoice = remote_find_invoice_with_client(&repo, &id)
                .await?
                .ok_or_else(|| eyre::eyre!("Invoice not found: {id}"))?;
            if json {
                println!("{}", facet_json::to_string(&invoice).unwrap_or_default());
            } else if md {
                println!("{}", task_core::invoice::render_invoice_body(&invoice));
            } else {
                print_invoice_detail(&invoice);
            }
        }
        InvoiceCommands::Send { id } => {
            let invoice = service.send_invoice(id, actor.map(str::to_string)).await?;
            println!(
                "Sent invoice {} — ${:.2} due {}.",
                invoice.id,
                invoice.total_cents() as f64 / 100.0,
                invoice.due_date
            );
        }
        InvoiceCommands::Pay {
            id,
            amount,
            method,
            reference,
            notes,
        } => {
            let invoice = service
                .record_invoice_payment(task_core::InvoicePaymentRequest {
                    invoice_id: id,
                    amount_cents: amount,
                    method: if method.is_empty() {
                        None
                    } else {
                        Some(method)
                    },
                    reference,
                    notes,
                    actor: actor.map(str::to_string),
                })
                .await?;
            println!(
                "Recorded ${:.2} against {}. Balance: ${:.2}. Status: {:?}",
                amount as f64 / 100.0,
                invoice.id,
                invoice.balance_cents() as f64 / 100.0,
                invoice.status
            );
        }
        InvoiceCommands::Cancel { id, reason } => {
            let invoice = service
                .cancel_invoice(id, reason, actor.map(str::to_string))
                .await?;
            println!("Cancelled invoice {}.", invoice.id);
        }
    }
    Ok(())
}

async fn run_remote_expense_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: ExpenseCommands,
) -> eyre::Result<()> {
    let service: task_core::service::ExpenseServiceClient = remote.connect().await?;
    let repo = remote.expense_repo().await?;
    let parse_date = |s: &str| -> eyre::Result<NaiveDate> {
        s.parse::<NaiveDate>()
            .map_err(|_| eyre::eyre!("Invalid date: {s}"))
    };

    match command {
        ExpenseCommands::Create {
            description,
            amount,
            date,
            currency,
            project,
            client: client_name,
            deliverable,
            category,
            vendor,
            receipt,
            reference,
            reimbursable,
            status,
            notes,
            json,
        } => {
            let expense = remote_create_expense_with_client(
                &repo,
                ExpenseCreateRequest {
                    description,
                    amount_cents: amount.try_into()?,
                    date: date.as_deref().map(parse_date).transpose()?,
                    currency_code: currency,
                    project,
                    client: client_name,
                    deliverable,
                    category,
                    vendor,
                    receipt,
                    reference,
                    reimbursable,
                    status,
                    notes,
                    actor: actor.map(str::to_string),
                },
            )
            .await?;
            if json {
                println!("{}", facet_json::to_string(&expense).unwrap_or_default());
            } else {
                println!(
                    "Created expense {} — ${:.2}",
                    expense.id,
                    expense.amount_cents as f64 / 100.0
                );
                println!("{}", render_expense_body(&expense));
            }
        }
        ExpenseCommands::List {
            from,
            to,
            project,
            client: client_name,
            deliverable,
            category,
            vendor,
            status,
            reimbursable_only,
            json,
        } => {
            let filter = ExpenseFilter {
                from: from.as_deref().map(parse_date).transpose()?,
                to: to.as_deref().map(parse_date).transpose()?,
                project,
                client: client_name,
                deliverable,
                category,
                vendor,
                status,
                reimbursable_only,
            };
            let expenses: Vec<_> = remote_list_expenses_with_client(&repo)
                .await?
                .into_iter()
                .filter(|expense| task_core::expense::matches_expense_filter(expense, &filter))
                .collect();
            if json {
                println!("{}", facet_json::to_string(&expenses).unwrap_or_default());
            } else if expenses.is_empty() {
                println!("No expenses.");
            } else {
                for expense in expenses {
                    println!(
                        "{}  ${:.2}  {:<10}  {}",
                        expense.date,
                        expense.amount_cents as f64 / 100.0,
                        format!("{:?}", expense.status),
                        expense.description
                    );
                }
            }
        }
        ExpenseCommands::Show { id, md: _, json } => {
            let expense = remote_find_expense_with_client(&repo, &id)
                .await?
                .ok_or_else(|| eyre::eyre!("Expense not found: {id}"))?;
            if json {
                println!("{}", facet_json::to_string(&expense).unwrap_or_default());
            } else {
                println!("{}", render_expense_body(&expense));
            }
        }
        ExpenseCommands::Report {
            from,
            to,
            project,
            client: client_name,
            deliverable,
            category,
            vendor,
            status,
            reimbursable_only,
            json,
        } => {
            let filter = ExpenseFilter {
                from: from.as_deref().map(parse_date).transpose()?,
                to: to.as_deref().map(parse_date).transpose()?,
                project,
                client: client_name,
                deliverable,
                category,
                vendor,
                status,
                reimbursable_only,
            };
            let report = service.expense_report(filter).await?;
            if json {
                println!("{}", facet_json::to_string(&report).unwrap_or_default());
            } else {
                println!("{}", render_expense_report(&report));
            }
        }
        ExpenseCommands::Update {
            id,
            amount,
            date,
            currency,
            project,
            client: client_name,
            deliverable,
            category,
            vendor,
            description,
            receipt,
            reference,
            reimbursable,
            status,
            notes,
            json,
        } => {
            let expense = remote_update_expense_with_client(
                &repo,
                &id,
                ExpensePatch {
                    status,
                    date,
                    amount_cents: amount.map(i64::try_from).transpose()?,
                    currency_code: currency,
                    project,
                    client: client_name,
                    deliverable,
                    category,
                    vendor,
                    description,
                    receipt,
                    reference,
                    reimbursable,
                    notes,
                },
            )
            .await?;
            if json {
                println!("{}", facet_json::to_string(&expense).unwrap_or_default());
            } else {
                println!("Updated expense {}.", expense.id);
                println!("{}", render_expense_body(&expense));
            }
        }
        ExpenseCommands::Delete { id } => {
            let expense = remote_find_expense_with_client(&repo, &id)
                .await?
                .ok_or_else(|| eyre::eyre!("Expense not found: {id}"))?;
            repo.delete_expense(expense.uuid.to_string()).await?;
            println!("Deleted expense {id}.");
        }
    }

    Ok(())
}

async fn run_remote_time_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: TimeCommands,
) -> eyre::Result<()> {
    let client = remote.time().await?;
    match command {
        TimeCommands::Active { json } => match client.active_timer().await? {
            Some(entry) => {
                if json {
                    println!(
                        "{{\"task\":\"{}\",\"entry\":{}}}",
                        escape_json(&entry.task_title),
                        facet_json::to_string(&entry.entry).unwrap_or_default()
                    );
                } else {
                    let elapsed = entry.entry.elapsed_minutes(chrono::Utc::now());
                    println!("Running: '{}' — {elapsed} min", entry.task_title);
                    if let Some(d) = &entry.entry.description {
                        println!("  {d}");
                    }
                    println!("  id: {}", entry.entry.id);
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
        TimeCommands::Log {
            reference,
            start,
            end,
            description,
            billable,
            rate,
        } => {
            let entry = client
                .log_time(task_core::TimeLogRequest {
                    task_ref: reference.clone(),
                    start: parse_datetime(&start)?,
                    end: parse_datetime(&end)?,
                    description,
                    billable,
                    billable_rate: rate,
                    user: actor.map(str::to_string),
                })
                .await?;
            println!(
                "Logged {} min on '{reference}' (entry {}).",
                entry.duration_minutes(),
                entry.id
            );
        }
        TimeCommands::List {
            task,
            user,
            project,
            client: client_name,
            tag,
            from,
            to,
            billable,
            format,
            json,
        } => {
            let fmt = pick_format(&format, json);
            let entries = client
                .list_time_entries(TimeEntryFilter {
                    task_ref: task,
                    user,
                    project,
                    client: client_name,
                    tag,
                    from: from.as_deref().map(parse_date_start).transpose()?,
                    to: to.as_deref().map(parse_date_end).transpose()?,
                    billable_only: billable,
                })
                .await?;
            match fmt {
                OutputFormat::Json => print_time_entries_json(&entries),
                OutputFormat::Csv => print_time_entries_csv(&entries),
                OutputFormat::Table => print_time_entries_table(&entries),
            }
        }
        TimeCommands::Report {
            group_by,
            from,
            to,
            project,
            client: client_name,
            tag,
            user,
            billable,
            rate,
            format,
            json,
        } => {
            let fmt = pick_format(&format, json);
            let entries = client
                .list_time_entries(TimeEntryFilter {
                    task_ref: None,
                    user,
                    project,
                    client: client_name,
                    tag,
                    from: from.as_deref().map(parse_date_start).transpose()?,
                    to: to.as_deref().map(parse_date_end).transpose()?,
                    billable_only: billable,
                })
                .await?;
            let report = aggregate_time(&entries, &group_by, rate)?;
            match fmt {
                OutputFormat::Json => print_report_json(&report),
                OutputFormat::Csv => print_report_csv(&report, &group_by),
                OutputFormat::Table => print_report_table(&report),
            }
        }
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
            let updated = client
                .edit_time_entry(entry_id, patch, actor.map(str::to_string))
                .await?;
            if json {
                println!(
                    "{{\"task\":\"{}\",\"entry\":{}}}",
                    escape_json(&updated.task_title),
                    facet_json::to_string(&updated.entry).unwrap_or_default()
                );
            } else {
                println!(
                    "Updated entry {} on '{}' — {} min.",
                    updated.entry.id,
                    updated.task_title,
                    updated.entry.duration_minutes()
                );
            }
        }
        TimeCommands::Delete { entry_id } => {
            client
                .delete_time_entry(entry_id.clone(), actor.map(str::to_string))
                .await?;
            println!("Deleted entry {entry_id}.");
        }
    }
    Ok(())
}

async fn run_remote_calendar_command(
    remote: &RemoteVoxConfig,
    command: CalendarCommands,
) -> eyre::Result<()> {
    let client = remote.calendar().await?;
    let repo = remote.calendar_event_repo().await?;
    match command {
        CalendarCommands::List { from, to, json } => {
            let events =
                remote_calendar_events_for_range(&client, from.as_deref(), to.as_deref()).await?;
            if json {
                print_calendar_events_json(&events);
            } else {
                print_calendar_events_table(&events);
            }
        }
        CalendarCommands::Show { reference, json } => {
            let event = remote_find_calendar_event(&client, &reference).await?;
            if json {
                println!("{}", facet_json::to_string(&event).unwrap_or_default());
            } else {
                print_calendar_event_detail(&event);
            }
        }
        CalendarCommands::Add {
            title,
            start,
            end,
            description,
            location,
            venue,
            space,
            all_day,
            status,
            recurrence,
            attendee,
            asset,
            force_assets,
            json,
        } => {
            let start = parse_datetime(&start)?;
            let end = end.as_deref().map(parse_datetime).transpose()?;
            let event = CalendarEvent {
                title,
                description,
                location,
                venue: venue.map(WikiLink),
                spaces: space.into_iter().map(WikiLink).collect(),
                start,
                end,
                all_day,
                status: parse_calendar_status(&status)?,
                recurrence,
                attendees: attendee.into(),
                ..CalendarEvent::default()
            };
            let create: task_core::calendar_event::CalendarEventApiCreate = model_to_api(&event)?;
            let created: CalendarEvent = api_to_model(repo.create_calendar_event(create).await?)?;
            let asset_reservations = if asset.is_empty() {
                Vec::new()
            } else {
                let asset_repo = remote.asset_repo().await?;
                let reference = created.id.clone().unwrap_or_else(|| created.title.clone());
                let mut reservations = Vec::new();
                for asset_ref in asset {
                    reservations.push(
                        remote_reserve_asset_with_client(
                            &asset_repo,
                            &asset_ref,
                            AssetReserveRequest {
                                reference: reference.clone(),
                                starts_at: Some(created.start),
                                ends_at: created.end,
                                force: force_assets,
                                ..AssetReserveRequest::default()
                            },
                        )
                        .await?,
                    );
                }
                reservations
            };
            if json {
                println!(
                    "{{\"event\":{},\"asset_reservations\":{}}}",
                    facet_json::to_string(&created).unwrap_or_default(),
                    facet_json::to_string(&asset_reservations).unwrap_or_default()
                );
            } else {
                println!("Created calendar event: {}", created.title);
                println!("  id: {}", created.id.as_deref().unwrap_or("—"));
                for reservation in &asset_reservations {
                    println!(
                        "  reserved asset: {} ({})",
                        reservation.asset.name, reservation.asset.id
                    );
                    print_asset_conflicts(&reservation.conflicts, false);
                }
            }
        }
        CalendarCommands::Update {
            reference,
            title,
            start,
            end,
            description,
            location,
            venue,
            space,
            all_day,
            status,
            recurrence,
            attendees,
            body,
            json,
        } => {
            let patch = build_calendar_patch(CalendarPatchInput {
                title,
                start,
                end,
                description,
                location,
                venue,
                space,
                all_day,
                status,
                recurrence,
                attendees,
                body,
            })?;
            let mut event = remote_find_calendar_event_with_client(&repo, &reference).await?;
            apply_remote_calendar_patch(&mut event, patch);
            let updated = remote_update_calendar_event_with_client(&repo, &event).await?;
            if json {
                println!("{}", facet_json::to_string(&updated).unwrap_or_default());
            } else {
                println!("Updated calendar event: {}", updated.title);
            }
        }
        CalendarCommands::Delete { reference } => {
            let event = remote_find_calendar_event_with_client(&repo, &reference).await?;
            repo.delete_calendar_event(event.uuid.to_string()).await?;
            println!("Deleted calendar event: {reference}");
        }
        CalendarCommands::Carddav { command } => match command {
            CardDavCommands::Discover { json } => {
                let discovery = client.discover_carddav().await?;
                if json {
                    println!("{}", facet_json::to_string(&discovery).unwrap_or_default());
                } else {
                    println!("Addressbook home: {}", discovery.addressbook_home_set);
                    for book in discovery.addressbooks {
                        println!(
                            "{}\t{}",
                            book.name,
                            book.display_name.as_deref().unwrap_or("—")
                        );
                    }
                }
            }
            CardDavCommands::Sync {
                addressbook,
                sync_token,
                json,
            } => {
                let sync = client
                    .addressbook_sync_collection(CardDavSyncCollectionRequest {
                        addressbook,
                        sync_token,
                    })
                    .await?;
                if json {
                    println!("{}", facet_json::to_string(&sync).unwrap_or_default());
                } else {
                    println!("sync-token: {}", sync.sync_token.as_deref().unwrap_or("—"));
                    for object in sync.objects {
                        let name = object
                            .contact
                            .as_ref()
                            .and_then(|contact| contact.full_name.as_deref())
                            .unwrap_or(object.href.as_str());
                        println!("{}\t{}", object.href, name);
                    }
                }
            }
        },
    }
    Ok(())
}

async fn run_remote_asset_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: AssetCommands,
) -> eyre::Result<()> {
    let client = remote.asset_repo().await?;
    match command {
        AssetCommands::Create {
            name,
            status,
            manufacturer,
            model,
            serial_number,
            category,
            organization,
            location,
            space,
            rack_or_case,
            assigned_to,
            purchase_date,
            warranty_until,
            vendor,
            cost_cents,
            notes,
            json,
        } => {
            let created = remote_create_asset_with_client(
                &client,
                AssetCreateRequest {
                    name,
                    status,
                    manufacturer,
                    model,
                    serial_number,
                    category,
                    organization,
                    location,
                    space,
                    rack_or_case,
                    assigned_to,
                    purchase_date: purchase_date.as_deref().map(str::parse).transpose()?,
                    warranty_until: warranty_until.as_deref().map(str::parse).transpose()?,
                    vendor,
                    cost_cents,
                    notes,
                    actor: actor.map(str::to_string),
                },
            )
            .await?;
            print_asset_result(&created, json);
        }
        AssetCommands::List {
            location,
            space,
            status,
            category,
            organization,
            query,
            needs_repair_only,
            json,
        } => {
            let assets = remote_list_assets_filtered(
                &client,
                AssetFilter {
                    location,
                    space,
                    status,
                    category,
                    organization,
                    query,
                    needs_repair_only,
                },
            )
            .await?;
            print_assets(&assets, json);
        }
        AssetCommands::Show { id, md, json } => {
            let asset = remote_find_asset_with_client(&client, &id).await?;
            if md {
                println!("{}", task_core::asset::render_asset_body(&asset));
            } else {
                print_asset_result(&asset, json);
            }
        }
        AssetCommands::Report {
            location,
            space,
            status,
            category,
            organization,
            query,
            needs_repair_only,
            json,
        } => {
            let assets = remote_list_assets_filtered(
                &client,
                AssetFilter {
                    location,
                    space,
                    status,
                    category,
                    organization,
                    query,
                    needs_repair_only,
                },
            )
            .await?;
            let report = task_core::asset::build_asset_report(&assets, Utc::now().date_naive());
            if json {
                println!("{}", facet_json::to_string(&report).unwrap_or_default());
            } else {
                println!("Assets: {}", report.asset_count);
                print_assets(&report.assets, false);
            }
        }
        AssetCommands::Update {
            id,
            name,
            status,
            manufacturer,
            model,
            serial_number,
            category,
            organization,
            location,
            space,
            rack_or_case,
            assigned_to,
            purchase_date,
            warranty_until,
            vendor,
            cost_cents,
            notes,
            json,
        } => {
            let patch = AssetPatch {
                name,
                status,
                manufacturer,
                model,
                serial_number,
                category,
                organization,
                location,
                space,
                rack_or_case,
                assigned_to,
                purchase_date,
                warranty_until,
                vendor,
                cost_cents,
                notes,
            };
            let updated = remote_update_asset_with_client(&client, &id, patch).await?;
            print_asset_result(&updated, json);
        }
        AssetCommands::Move {
            id,
            location,
            space,
            rack_or_case,
            json,
        } => {
            let updated = remote_update_asset_with_client(
                &client,
                &id,
                AssetPatch {
                    location: Some(location),
                    space,
                    rack_or_case,
                    ..AssetPatch::default()
                },
            )
            .await?;
            print_asset_result(&updated, json);
        }
        AssetCommands::Status {
            id,
            status,
            notes,
            json,
        } => {
            let updated = remote_update_asset_with_client(
                &client,
                &id,
                AssetPatch {
                    status: Some(status),
                    notes,
                    ..AssetPatch::default()
                },
            )
            .await?;
            print_asset_result(&updated, json);
        }
        AssetCommands::Maintain {
            id,
            date,
            issue,
            vendor,
            contact,
            cost_cents,
            warranty,
            rma,
            task,
            notes,
            json,
        } => {
            let mut asset = remote_find_asset_with_client(&client, &id).await?;
            asset.maintenance.push(AssetMaintenanceRecord {
                date: date
                    .as_deref()
                    .map(str::parse)
                    .transpose()?
                    .unwrap_or_else(|| Utc::now().date_naive()),
                issue,
                vendor,
                contact,
                cost_cents: cost_cents.map(|v| v as i64),
                warranty,
                rma,
                task: task.map(WikiLink),
                notes,
            });
            if asset.status == AssetStatus::Available {
                asset.status = AssetStatus::MaintenanceDue;
            }
            asset.date_modified = Some(Utc::now());
            let updated = remote_replace_asset_with_client(&client, asset).await?;
            print_asset_result(&updated, json);
        }
        AssetCommands::Repair { command } => match command {
            AssetRepairCommands::Open {
                id,
                title,
                notes,
                vendor,
                contact,
                cost_cents,
                warranty,
                rma,
                json,
            } => {
                let mut asset = remote_find_asset_with_client(&client, &id).await?;
                asset.maintenance.push(AssetMaintenanceRecord {
                    date: Utc::now().date_naive(),
                    issue: title,
                    vendor,
                    contact,
                    cost_cents: cost_cents.map(|v| v as i64),
                    warranty,
                    rma,
                    notes,
                    ..AssetMaintenanceRecord::default()
                });
                asset.status = AssetStatus::NeedsRepair;
                asset.date_modified = Some(Utc::now());
                let updated = remote_replace_asset_with_client(&client, asset).await?;
                print_asset_result(&updated, json);
            }
        },
        AssetCommands::Reserve {
            id,
            reference,
            from,
            to,
            reserved_by,
            notes,
            force,
            json,
        } => {
            let response = remote_reserve_asset_with_client(
                &client,
                &id,
                AssetReserveRequest {
                    reference,
                    starts_at: from.as_deref().map(parse_datetime).transpose()?,
                    ends_at: to.as_deref().map(parse_datetime).transpose()?,
                    reserved_by,
                    notes,
                    force,
                },
            )
            .await?;
            if json {
                println!("{}", facet_json::to_string(&response).unwrap_or_default());
            } else {
                println!(
                    "Reserved {} for {}.",
                    response.asset.name, response.reservation.reference.0
                );
                print_asset_conflicts(&response.conflicts, false);
            }
        }
        AssetCommands::Release {
            id,
            reservation,
            json,
        } => {
            let updated =
                remote_release_asset_reservation_with_client(&client, &id, &reservation).await?;
            print_asset_result(&updated, json);
        }
        AssetCommands::Conflicts {
            location,
            space,
            status,
            category,
            organization,
            query,
            json,
        } => {
            let assets = remote_list_assets_filtered(
                &client,
                AssetFilter {
                    location,
                    space,
                    status,
                    category,
                    organization,
                    query,
                    needs_repair_only: false,
                },
            )
            .await?;
            let conflicts = task_core::asset::collect_asset_conflicts(&assets);
            print_asset_conflicts(&conflicts, json);
        }
        AssetCommands::Delete { id } => {
            let asset = remote_find_asset_with_client(&client, &id).await?;
            client.delete_asset(asset.uuid.to_string()).await?;
            println!("Deleted asset: {id}");
        }
    }
    Ok(())
}

async fn remote_list_assets_with_client(
    client: &task_core::asset::AssetRepoClient,
) -> eyre::Result<Vec<Asset>> {
    let rows = client.list_assets(None, None, None, Some(10_000)).await?;
    rows.into_iter().map(api_to_model).collect()
}

async fn remote_list_assets_filtered(
    client: &task_core::asset::AssetRepoClient,
    filter: AssetFilter,
) -> eyre::Result<Vec<Asset>> {
    let mut assets = remote_list_assets_with_client(client).await?;
    assets.retain(|asset| task_core::asset::matches_asset_filter(asset, &filter));
    assets.sort_by(|a, b| a.name.cmp(&b.name).then_with(|| a.number.cmp(&b.number)));
    Ok(assets)
}

async fn remote_find_asset_with_client(
    client: &task_core::asset::AssetRepoClient,
    reference: &str,
) -> eyre::Result<Asset> {
    remote_list_assets_with_client(client)
        .await?
        .into_iter()
        .find(|asset| {
            asset.uuid.to_string() == reference
                || asset.id.eq_ignore_ascii_case(reference)
                || asset.name.eq_ignore_ascii_case(reference)
        })
        .ok_or_else(|| eyre::eyre!("Asset not found: {reference}"))
}

async fn remote_create_asset_with_client(
    client: &task_core::asset::AssetRepoClient,
    request: AssetCreateRequest,
) -> eyre::Result<Asset> {
    let now = Utc::now();
    let number = remote_list_assets_with_client(client)
        .await?
        .into_iter()
        .filter(|asset| asset.id.starts_with(&format!("AST-{:04}-", now.year())))
        .map(|asset| asset.number)
        .max()
        .unwrap_or(0)
        + 1;
    let status = request
        .status
        .as_deref()
        .and_then(task_core::asset::parse_asset_status)
        .unwrap_or_default();
    let asset = Asset {
        uuid: Uuid::new_v4(),
        id: task_core::asset::format_asset_id(now.year(), number),
        number,
        name: request.name,
        status,
        manufacturer: request.manufacturer,
        model: request.model,
        serial_number: request.serial_number,
        category: request.category,
        organization: request.organization,
        location: request.location.map(WikiLink),
        space: request.space.map(WikiLink),
        rack_or_case: request.rack_or_case,
        assigned_to: request.assigned_to,
        purchase_date: request.purchase_date,
        warranty_until: request.warranty_until,
        vendor: request.vendor,
        cost_cents: request.cost_cents.map(|value| value as i64),
        notes: request.notes,
        created_by: request.actor,
        date_created: Some(now),
        date_modified: Some(now),
        ..Asset::default()
    };
    let create: task_core::asset::AssetApiCreate = model_to_api(&asset)?;
    api_to_model(client.create_asset(create).await?)
}

async fn remote_update_asset_with_client(
    client: &task_core::asset::AssetRepoClient,
    reference: &str,
    patch: AssetPatch,
) -> eyre::Result<Asset> {
    let mut asset = remote_find_asset_with_client(client, reference).await?;
    apply_asset_patch(&mut asset, patch)?;
    remote_replace_asset_with_client(client, asset).await
}

async fn remote_replace_asset_with_client(
    client: &task_core::asset::AssetRepoClient,
    asset: Asset,
) -> eyre::Result<Asset> {
    let update: task_core::asset::AssetApiUpdate = model_to_api(&asset)?;
    api_to_model(client.update_asset(asset.uuid.to_string(), update).await?)
}

async fn remote_reserve_asset_with_client(
    client: &task_core::asset::AssetRepoClient,
    reference: &str,
    request: AssetReserveRequest,
) -> eyre::Result<AssetReservationResponse> {
    let mut asset = remote_find_asset_with_client(client, reference).await?;
    let reservation = AssetReservationRecord {
        id: Uuid::new_v4().to_string(),
        reference: WikiLink(request.reference),
        starts_at: request.starts_at,
        ends_at: request.ends_at,
        reserved_by: request.reserved_by,
        notes: request.notes,
    };
    let conflicts = task_core::asset::conflicts_for_reservation(&asset, &reservation);
    if !request.force && !conflicts.is_empty() {
        eyre::bail!(
            "asset reservation has {} conflict(s); rerun with --force to record anyway",
            conflicts.len()
        );
    }
    asset.reservations.push(reservation.clone());
    if asset.status == AssetStatus::Available {
        asset.status = AssetStatus::Reserved;
    }
    asset.date_modified = Some(Utc::now());
    let asset = remote_replace_asset_with_client(client, asset).await?;
    Ok(AssetReservationResponse {
        asset,
        reservation,
        conflicts,
    })
}

async fn remote_release_asset_reservation_with_client(
    client: &task_core::asset::AssetRepoClient,
    reference: &str,
    reservation_ref: &str,
) -> eyre::Result<Asset> {
    let mut asset = remote_find_asset_with_client(client, reference).await?;
    let before = asset.reservations.len();
    asset.reservations.retain(|reservation| {
        reservation.id != reservation_ref
            && !reservation
                .reference
                .0
                .eq_ignore_ascii_case(reservation_ref)
    });
    if asset.reservations.len() == before {
        eyre::bail!("Asset reservation not found: {reservation_ref}");
    }
    if asset.status == AssetStatus::Reserved && asset.reservations.is_empty() {
        asset.status = AssetStatus::Available;
    }
    asset.date_modified = Some(Utc::now());
    remote_replace_asset_with_client(client, asset).await
}

fn apply_asset_patch(asset: &mut Asset, patch: AssetPatch) -> eyre::Result<()> {
    if let Some(value) = patch.name {
        asset.name = value;
    }
    if let Some(value) = patch.status {
        asset.status = task_core::asset::parse_asset_status(&value)
            .ok_or_else(|| eyre::eyre!("invalid asset status: {value}"))?;
    }
    if let Some(value) = patch.manufacturer {
        asset.manufacturer = empty_to_none(value);
    }
    if let Some(value) = patch.model {
        asset.model = empty_to_none(value);
    }
    if let Some(value) = patch.serial_number {
        asset.serial_number = empty_to_none(value);
    }
    if let Some(value) = patch.category {
        asset.category = empty_to_none(value);
    }
    if let Some(value) = patch.organization {
        asset.organization = empty_to_none(value);
    }
    if let Some(value) = patch.location {
        asset.location = empty_to_none(value).map(WikiLink);
    }
    if let Some(value) = patch.space {
        asset.space = empty_to_none(value).map(WikiLink);
    }
    if let Some(value) = patch.rack_or_case {
        asset.rack_or_case = empty_to_none(value);
    }
    if let Some(value) = patch.assigned_to {
        asset.assigned_to = empty_to_none(value);
    }
    if let Some(value) = patch.purchase_date {
        asset.purchase_date = if value.trim().is_empty() || value == "clear" {
            None
        } else {
            Some(value.parse()?)
        };
    }
    if let Some(value) = patch.warranty_until {
        asset.warranty_until = if value.trim().is_empty() || value == "clear" {
            None
        } else {
            Some(value.parse()?)
        };
    }
    if let Some(value) = patch.vendor {
        asset.vendor = empty_to_none(value);
    }
    if let Some(value) = patch.cost_cents {
        asset.cost_cents = value.map(|amount| amount as i64);
    }
    if let Some(value) = patch.notes {
        asset.notes = empty_to_none(value);
    }
    asset.date_modified = Some(Utc::now());
    Ok(())
}

fn empty_to_none(value: String) -> Option<String> {
    if value.trim().is_empty() || value == "clear" {
        None
    } else {
        Some(value)
    }
}

async fn run_remote_email_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: EmailCommands,
) -> eyre::Result<()> {
    let mail = remote.mail().await?;
    match command {
        EmailCommands::Accounts { json } => {
            let accounts = mail.list_accounts().await?;
            if json {
                print_mail_accounts_json(&accounts);
            } else {
                print_mail_accounts_table(&accounts);
            }
        }
        EmailCommands::Mailboxes { account, json } => {
            let boxes = mail.list_mailboxes(account).await?;
            if json {
                print_mailboxes_json(&boxes);
            } else {
                print_mailboxes_table(&boxes);
            }
        }
        EmailCommands::Search {
            mailbox,
            filter,
            limit,
            cursor,
            json,
        } => {
            let messages = mail
                .list_messages(task_core::MailListMessagesRequest {
                    mailbox_id: mailbox,
                    filter,
                    limit,
                    cursor,
                })
                .await?;
            if json {
                print_mail_messages_json(&messages);
            } else {
                print_mail_messages_table(&messages);
            }
        }
        EmailCommands::Show { id, body, json } => {
            let msg = mail.get_message(id).await?;
            let body_text = if body {
                mail.get_body(id).await.ok()
            } else {
                None
            };
            if json {
                print_mail_detail_json(&msg, body_text.as_deref());
            } else {
                print_mail_detail(&msg, body_text.as_deref());
            }
        }
        EmailCommands::FolderCreate {
            account,
            name,
            json,
        } => {
            let mb = mail
                .create_mailbox(task_core::MailCreateMailboxRequest {
                    account_id: account,
                    name,
                })
                .await?;
            if json {
                print_mailboxes_json(&[mb]);
            } else {
                println!(
                    "Created mailbox {} (id {}, account {})",
                    mb.name, mb.id, mb.account_id
                );
            }
        }
        EmailCommands::FolderDelete { mailbox } => {
            mail.delete_mailbox(mailbox).await?;
            println!("Deleted mailbox {mailbox}.");
        }
        EmailCommands::Move {
            email_id,
            to_folder,
        } => {
            mail.move_message(task_core::MailMoveMessageRequest {
                message_id: email_id,
                dest_folder_id: to_folder,
            })
            .await?;
            println!("Moved message {email_id} to folder {to_folder}.");
        }
        EmailCommands::Tag { cmd } => match cmd {
            TagCommands::List { json } => {
                let tags = mail.list_tags().await?;
                if json {
                    print_mail_tags_json(&tags);
                } else {
                    print_mail_tags_table(&tags);
                }
            }
            TagCommands::Create { name, color, json } => {
                let tag = mail
                    .create_tag(task_core::MailCreateTagRequest {
                        display_name: name,
                        color,
                    })
                    .await?;
                if json {
                    print_mail_tags_json(&[tag]);
                } else {
                    println!(
                        "Created tag {} (id {}, imapLabel {})",
                        tag.display_name, tag.id, tag.imap_label
                    );
                }
            }
            TagCommands::Delete { account, tag } => {
                mail.delete_tag(task_core::MailDeleteTagRequest {
                    account_id: account,
                    tag_id: tag,
                })
                .await?;
                println!("Deleted tag {tag} on account {account}.");
            }
            TagCommands::Set {
                imap_label,
                email_id,
            } => {
                mail.set_tag(task_core::MailMessageTagRequest {
                    message_id: email_id,
                    imap_label: imap_label.clone(),
                })
                .await?;
                println!("Tagged message {email_id} with {imap_label}.");
            }
            TagCommands::Unset {
                imap_label,
                email_id,
            } => {
                mail.remove_tag(task_core::MailMessageTagRequest {
                    message_id: email_id,
                    imap_label: imap_label.clone(),
                })
                .await?;
                println!("Removed tag {imap_label} from message {email_id}.");
            }
        },
        EmailCommands::Sweep {
            account,
            mailbox,
            limit,
            filter,
            table,
        } => {
            let mailbox_id = match mailbox {
                Some(m) => m,
                None => {
                    mail.list_mailboxes(account)
                        .await?
                        .into_iter()
                        .find(|m| m.name.eq_ignore_ascii_case("INBOX"))
                        .ok_or_else(|| eyre::eyre!("No INBOX for account {account}"))?
                        .id
                }
            };
            let messages = mail
                .list_messages(task_core::MailListMessagesRequest {
                    mailbox_id,
                    filter,
                    limit,
                    cursor: None,
                })
                .await?;
            let linked: std::collections::HashSet<_> =
                mail.linked_message_ids().await?.into_iter().collect();
            let mut unprocessed: Vec<_> = messages
                .into_iter()
                .filter(|m| {
                    if m.tag_labels.iter().any(|t| t == "$processed") {
                        return false;
                    }
                    if let Some(mid) = m.message_id.as_deref() {
                        let key = normalize_message_id(mid);
                        if linked.contains(&key) {
                            return false;
                        }
                    }
                    true
                })
                .collect();
            unprocessed.sort_by_key(|m| m.date);
            if table {
                print_mail_messages_table(&unprocessed);
            } else {
                print_mail_messages_json(&unprocessed);
            }
        }
        EmailCommands::MarkProcessed { email_id, note } => {
            let tags = mail.list_tags().await?;
            let processed = tags.into_iter().find(|t| t.imap_label == "$processed");
            let tag = match processed {
                Some(t) => t,
                None => {
                    mail.create_tag(task_core::MailCreateTagRequest {
                        display_name: "processed".into(),
                        color: "#64748b".into(),
                    })
                    .await?
                }
            };
            mail.set_tag(task_core::MailMessageTagRequest {
                message_id: email_id,
                imap_label: tag.imap_label,
            })
            .await?;
            println!(
                "Marked message {email_id} processed{}",
                note.as_deref()
                    .map(|n| format!(" — {n}"))
                    .unwrap_or_default()
            );
        }
        EmailCommands::Link { .. } | EmailCommands::Unlink { .. } | EmailCommands::List { .. } => {
            run_remote_email_link_command(remote, actor, command).await?;
        }
        EmailCommands::Watch { .. } => unreachable!("email watch is dispatched earlier"),
    }
    Ok(())
}

async fn run_remote_email_link_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: EmailCommands,
) -> eyre::Result<()> {
    let mail = remote.mail().await?;
    match command {
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
        } => {
            let now = chrono::Utc::now();
            let email = task_core::EmailRef {
                uuid: Uuid::new_v4(),
                message_id: message_id.clone(),
                subject: subject.unwrap_or_default(),
                from: from.unwrap_or_default(),
                to: to_recipients
                    .map(|s| {
                        s.split(',')
                            .map(|t| t.trim().to_string())
                            .filter(|t| !t.is_empty())
                            .collect::<Vec<_>>()
                            .into()
                    })
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
                linked_by: actor.map(str::to_string),
                linked_at: Some(now),
                user_tags: tags
                    .map(|s| {
                        s.split(',')
                            .map(|t| t.trim().to_string())
                            .filter(|t| !t.is_empty())
                            .collect::<Vec<_>>()
                            .into()
                    })
                    .unwrap_or_default(),
            };
            let response = mail
                .link_email(task_core::EmailLinkRequest {
                    target_type: to.clone(),
                    reference,
                    email,
                    actor: actor.map(str::to_string),
                })
                .await?;
            println!(
                "Linked {} to {} '{}'. ({} emails total)",
                message_id, response.target_type, response.title, response.email_count
            );
        }
        EmailCommands::Unlink {
            to,
            reference,
            message_id,
        } => {
            mail.unlink_email(task_core::EmailUnlinkRequest {
                target_type: to.clone(),
                reference: reference.clone(),
                message_id: message_id.clone(),
                actor: actor.map(str::to_string),
            })
            .await?;
            println!("Unlinked {message_id} from {to} '{reference}'.");
        }
        EmailCommands::List {
            to,
            reference,
            json,
        } => {
            let emails = mail
                .list_linked_emails(task_core::EmailListRequest {
                    target_type: to,
                    reference,
                })
                .await?;
            if json {
                print_emails_json(&emails);
            } else {
                print_emails_table(&emails);
            }
        }
        _ => unreachable!("only email link commands are delegated here"),
    }
    Ok(())
}

fn normalize_message_id(id: &str) -> String {
    id.trim()
        .trim_start_matches('<')
        .trim_end_matches('>')
        .to_ascii_lowercase()
}

async fn run_remote_doctor(remote: &RemoteVoxConfig, json: bool, deep: bool) -> eyre::Result<()> {
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

async fn run_local_doctor(vault: Option<&str>, json: bool, deep: bool) -> eyre::Result<()> {
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
                .map(|v| env_truthy(&v))
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

async fn run_github_command_remote(
    remote: &RemoteVoxConfig,
    command: GithubCommands,
) -> eyre::Result<()> {
    match command {
        GithubCommands::Sync {
            repo,
            token,
            plan,
            json,
        } => {
            use task_core::provider::github;

            let (owner, name) = github::parse_repo(&repo).map_err(|e| eyre::eyre!("{e}"))?;
            let token_val =
                github::resolve_token(token.as_deref()).map_err(|e| eyre::eyre!("{e}"))?;

            let task_client = remote.task_repo().await?;
            let all_tasks = remote_list_tasks_with_client(&task_client).await?;
            let gh_tasks: Vec<_> = all_tasks
                .into_iter()
                .filter(|t| t.external_source.as_deref() == Some("github"))
                .collect();

            let config = github::GitHubConfig::new(token_val, format!("{owner}/{name}"));

            if plan {
                let sync_client = github::GitHubSync::new(config);
                let remote_tasks = sync_client
                    .pull_issues()
                    .await
                    .map_err(|e| eyre::eyre!("GitHub pull failed: {e}"))?;
                let sync_plan = github::build_sync_plan(&gh_tasks, &remote_tasks);
                if json {
                    let actions: Vec<String> = sync_plan
                        .actions
                        .iter()
                        .map(|a| match a {
                            github::SyncAction::Pull {
                                issue_number,
                                title,
                            } => format!(
                                r#"{{"action":"pull","issue":{},"title":{}}}"#,
                                facet_json::to_string(issue_number).unwrap_or_default(),
                                facet_json::to_string(title).unwrap_or_default(),
                            ),
                            github::SyncAction::Push {
                                issue_number,
                                title,
                                new_state,
                            } => format!(
                                r#"{{"action":"push","issue":{},"title":{},"state":{}}}"#,
                                facet_json::to_string(issue_number).unwrap_or_default(),
                                facet_json::to_string(title).unwrap_or_default(),
                                facet_json::to_string(new_state).unwrap_or_default(),
                            ),
                        })
                        .collect();
                    println!("[{}]", actions.join(","));
                } else {
                    print!("{sync_plan}");
                }
                return Ok(());
            }

            let sync_client = github::GitHubSync::new(config);
            let result = sync_client
                .sync(&gh_tasks)
                .await
                .map_err(|e| eyre::eyre!("GitHub sync failed: {e}"))?;
            if json {
                println!(
                    r#"{{"issues_pulled":{},"tasks_created":{},"statuses_pushed":{},"errors":{}}}"#,
                    result.issues_pulled,
                    result.tasks_created,
                    result.statuses_pushed,
                    facet_json::to_string(&result.errors).unwrap_or_default(),
                );
            } else {
                println!("{}", github::format_sync_result(&result));
            }
        }
    }
    Ok(())
}

fn env_truthy(value: &str) -> bool {
    matches!(
        value.to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on"
    )
}

fn run_demo_command(
    vault_path: &str,
    command: DemoCommands,
    actor: Option<&str>,
) -> eyre::Result<()> {
    match command {
        DemoCommands::Seed {
            org,
            client,
            prefix,
            json,
        } => {
            let summary = seed_demo_vault(
                std::path::Path::new(vault_path),
                &org,
                &client,
                &prefix,
                actor,
            )?;
            if json {
                println!(
                    "{{\"project\":\"{}\",\"files\":[{}]}}",
                    escape_json(&summary.project),
                    summary
                        .files
                        .iter()
                        .map(|file| format!("\"{}\"", escape_json(file)))
                        .collect::<Vec<_>>()
                        .join(",")
                );
            } else {
                println!("Seeded demo project: {}", summary.project);
                for file in &summary.files {
                    println!("  {file}");
                }
            }
        }
    }
    Ok(())
}

#[derive(Debug, Clone)]
struct DemoSeedSummary {
    project: String,
    files: Vec<String>,
}

fn seed_demo_vault(
    vault_root: &std::path::Path,
    org: &str,
    client: &str,
    prefix: &str,
    actor: Option<&str>,
) -> eyre::Result<DemoSeedSummary> {
    let actor = actor.unwrap_or("demo-agent");
    let project = Project {
        title: prefix.to_string(),
        description: Some("Deterministic Task demo/smoke project.".to_string()),
        organization: Some(org.to_string()),
        client: Some(WikiLink(client.to_string())),
        tags: vec![
            "project".to_string(),
            "demo".to_string(),
            "smoke".to_string(),
        ]
        .into(),
        identifier: Some("DEMO".to_string()),
        lead: Some(actor.to_string()),
        default_assignee: Some(actor.to_string()),
        default_rate: Some(12_000),
        ..Default::default()
    };
    let project_dir = create_project(vault_root, &project)?;
    let mut files = vec![project_dir.join("project.md")];
    let project_link = WikiLink(prefix.to_string());
    let created = Utc.with_ymd_and_hms(2026, 5, 1, 9, 0, 0).unwrap();
    let work_start = Utc.with_ymd_and_hms(2026, 5, 1, 10, 0, 0).unwrap();
    let work_end = Utc.with_ymd_and_hms(2026, 5, 1, 11, 30, 0).unwrap();

    let tasks = vec![
        Task {
            id: Uuid::parse_str("00000000-0000-4000-8000-000000000101").unwrap(),
            title: "Demo capture inbox item".to_string(),
            status: Status::Planned,
            priority: Priority::High,
            projects: vec![project_link.clone()].into(),
            tags: vec!["demo".to_string(), "inbox".to_string()].into(),
            assignee: Some(actor.to_string()),
            created_by: Some(actor.to_string()),
            date_created: Some(created),
            date_modified: Some(created),
            body: "Exercises inbox capture/promotion flows in a deterministic fixture.".to_string(),
            ..Default::default()
        },
        Task {
            id: Uuid::parse_str("00000000-0000-4000-8000-000000000102").unwrap(),
            title: "Demo billable work item".to_string(),
            status: Status::InProgress,
            priority: Priority::Normal,
            projects: vec![project_link.clone()].into(),
            tags: vec![
                "demo".to_string(),
                "time".to_string(),
                "invoice".to_string(),
            ]
            .into(),
            assignee: Some(actor.to_string()),
            created_by: Some(actor.to_string()),
            date_created: Some(created),
            date_modified: Some(created),
            time_entries: vec![TimeEntry {
                id: "demo-time-entry-billable".to_string(),
                user: Some(actor.to_string()),
                start_time: work_start,
                end_time: Some(work_end),
                description: Some("Deterministic billable smoke-test work".to_string()),
                billable: true,
                billable_rate: Some(12_000),
                tags: vec!["demo".to_string()],
                ..Default::default()
            }]
            .into(),
            body: "Provides a stable time-entry fixture for reports and invoice smoke tests."
                .to_string(),
            ..Default::default()
        },
        Task {
            id: Uuid::parse_str("00000000-0000-4000-8000-000000000103").unwrap(),
            title: "Demo review invoice lifecycle".to_string(),
            status: Status::Open,
            priority: Priority::Normal,
            projects: vec![project_link].into(),
            tags: vec!["demo".to_string(), "invoice".to_string()].into(),
            assignee: Some(actor.to_string()),
            created_by: Some(actor.to_string()),
            date_created: Some(created),
            date_modified: Some(created),
            body: "Used by smoke tests to validate draft/sent/paid invoice lifecycle paths."
                .to_string(),
            ..Default::default()
        },
    ];

    for task in &tasks {
        save_project_task(&project_dir, task)?;
        files.push(project_dir.join("tasks").join(format!("{}.md", task.title)));
    }

    let inbox_dir = vault_root.join("inbox");
    std::fs::create_dir_all(&inbox_dir)?;
    let inbox_file = inbox_dir.join("demo-capture.md");
    std::fs::write(
        &inbox_file,
        format!(
            "---\nid: demo-inbox-capture\ntitle: Demo inbox capture\nkind: inbox\nsource: demo\norganization: {}\nproject: \"[[{}]]\"\n---\nDemo capture item for smoke testing.\n",
            org, prefix
        ),
    )?;
    files.push(inbox_file);

    let invoice_dir = vault_root.join("invoices");
    std::fs::create_dir_all(&invoice_dir)?;
    let invoice_file = invoice_dir.join("demo-invoice.md");
    std::fs::write(
        &invoice_file,
        format!(
            "---\nid: demo-invoice\nclient: \"[[{}]]\"\nproject: \"[[{}]]\"\nstatus: draft\ntotal_cents: 18000\npaid_cents: 0\n---\n# Demo Invoice\n\nDeterministic invoice fixture for smoke tests.\n",
            client, prefix
        ),
    )?;
    files.push(invoice_file);

    let calendar_dir = vault_root.join("calendar");
    std::fs::create_dir_all(&calendar_dir)?;
    let event_file = calendar_dir.join("demo-event.md");
    std::fs::write(
        &event_file,
        format!(
            "---\nid: demo-event\ntitle: Demo workflow review\nstatus: confirmed\nstart: 2026-05-02T15:00:00Z\nend: 2026-05-02T16:00:00Z\nproject: \"[[{}]]\"\n---\nCalendar fixture for demo smoke testing.\n",
            prefix
        ),
    )?;
    files.push(event_file);

    let files = files
        .into_iter()
        .map(|path| path.display().to_string())
        .collect();
    Ok(DemoSeedSummary {
        project: prefix.to_string(),
        files,
    })
}

async fn run_server_command(command: ServerCommands) -> eyre::Result<()> {
    match command {
        ServerCommands::Add {
            name,
            url,
            session_token,
            organization_id,
            use_now,
        } => {
            let mut profiles = load_server_profiles().unwrap_or_default();
            profiles.servers.retain(|profile| profile.name != name);
            profiles.servers.push(ServerProfile {
                name: name.clone(),
                url,
                session_token,
                organization_id,
            });
            if use_now || profiles.default.is_none() {
                profiles.default = Some(name.clone());
            }
            save_server_profiles(&profiles)?;
            println!("Saved server profile '{name}'.");
        }
        ServerCommands::List { json } => {
            let profiles = load_server_profiles().unwrap_or_default();
            if json {
                print_server_profiles_json(&profiles);
            } else if profiles.servers.is_empty() {
                println!("No server profiles configured.");
            } else {
                println!("{:<18} {:<8} URL", "NAME", "DEFAULT");
                println!("{}", "-".repeat(72));
                for profile in profiles.servers {
                    println!(
                        "{:<18} {:<8} {}",
                        profile.name,
                        if profiles.default.as_deref() == Some(&profile.name) {
                            "yes"
                        } else {
                            ""
                        },
                        profile.url
                    );
                }
            }
        }
        ServerCommands::Use { name } => {
            let mut profiles = load_server_profiles().unwrap_or_default();
            if profiles.servers.iter().any(|profile| profile.name == name) {
                profiles.default = Some(name.clone());
                save_server_profiles(&profiles)?;
                println!("Using server profile '{name}'.");
            } else {
                eyre::bail!("Unknown server profile: {name}");
            }
        }
        ServerCommands::Current { json } => {
            let profiles = load_server_profiles().unwrap_or_default();
            let current = profiles.current();
            if json {
                print_server_profile_json(current.as_ref());
            } else if let Some(profile) = current {
                println!("{} -> {}", profile.name, profile.url);
            } else {
                println!("No default server profile configured.");
            }
        }
        ServerCommands::Doctor { name, json, deep } => {
            let profiles = load_server_profiles().unwrap_or_default();
            let profile = name
                .as_deref()
                .and_then(|name| profiles.resolve(name))
                .or_else(|| profiles.current())
                .ok_or_else(|| eyre::eyre!("No server profile configured."))?;
            let remote =
                RemoteVoxConfig::new(profile.name, profile.session_token, profile.organization_id)?;
            run_remote_doctor(&remote, json, deep).await?;
        }
    }
    Ok(())
}

fn server_profiles_path() -> eyre::Result<std::path::PathBuf> {
    let base = std::env::var("TASK_CONFIG_DIR")
        .ok()
        .map(std::path::PathBuf::from)
        .or_else(|| {
            std::env::var("HOME")
                .ok()
                .map(|home| std::path::PathBuf::from(home).join(".config/task"))
        })
        .ok_or_else(|| eyre::eyre!("Set HOME or TASK_CONFIG_DIR to store server profiles."))?;
    Ok(base.join("servers.tsv"))
}

fn load_server_profiles() -> eyre::Result<ServerProfiles> {
    let path = server_profiles_path()?;
    let Ok(content) = std::fs::read_to_string(path) else {
        return Ok(ServerProfiles::default());
    };
    let mut profiles = ServerProfiles::default();
    for line in content.lines() {
        let parts: Vec<_> = line.split('\t').collect();
        match parts.as_slice() {
            ["default", name] => profiles.default = Some((*name).to_string()),
            ["server", name, url, token, org] => profiles.servers.push(ServerProfile {
                name: (*name).to_string(),
                url: (*url).to_string(),
                session_token: if token.is_empty() {
                    None
                } else {
                    Some((*token).to_string())
                },
                organization_id: if org.is_empty() {
                    None
                } else {
                    Some((*org).to_string())
                },
            }),
            _ => {}
        }
    }
    Ok(profiles)
}

fn save_server_profiles(profiles: &ServerProfiles) -> eyre::Result<()> {
    let path = server_profiles_path()?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let mut content = String::new();
    if let Some(default) = &profiles.default {
        content.push_str(&format!("default\t{}\n", tsv_escape(default)));
    }
    for profile in &profiles.servers {
        content.push_str(&format!(
            "server\t{}\t{}\t{}\t{}\n",
            tsv_escape(&profile.name),
            tsv_escape(&profile.url),
            tsv_escape(profile.session_token.as_deref().unwrap_or("")),
            tsv_escape(profile.organization_id.as_deref().unwrap_or(""))
        ));
    }
    std::fs::write(path, content)?;
    Ok(())
}

fn tsv_escape(value: &str) -> String {
    value.replace(['\t', '\n', '\r'], " ")
}

fn print_server_profiles_json(profiles: &ServerProfiles) {
    print!("{{\"default\":");
    match &profiles.default {
        Some(default) => print!("\"{}\"", escape_json(default)),
        None => print!("null"),
    }
    print!(",\"servers\":[");
    for (idx, profile) in profiles.servers.iter().enumerate() {
        if idx > 0 {
            print!(",");
        }
        print_server_profile_object(profile);
    }
    println!("]}}");
}

fn print_server_profile_json(profile: Option<&ServerProfile>) {
    match profile {
        Some(profile) => {
            print_server_profile_object(profile);
            println!();
        }
        None => println!("null"),
    }
}

fn print_server_profile_object(profile: &ServerProfile) {
    print!(
        "{{\"name\":\"{}\",\"url\":\"{}\",\"session_token_configured\":{},\"organization_id\":{}}}",
        escape_json(&profile.name),
        escape_json(&profile.url),
        profile.session_token.is_some(),
        profile
            .organization_id
            .as_deref()
            .map(|org| format!("\"{}\"", escape_json(org)))
            .unwrap_or_else(|| "null".into())
    );
}

async fn remote_calendar_events_for_range(
    client: &task_core::service::CalendarServiceClient,
    from: Option<&str>,
    to: Option<&str>,
) -> eyre::Result<Vec<CalendarEvent>> {
    let from = from
        .map(parse_calendar_boundary_start)
        .transpose()?
        .unwrap_or_else(|| parse_datetime("1970-01-01T00:00:00Z").unwrap())
        .to_rfc3339();
    let to = to
        .map(parse_calendar_boundary_end)
        .transpose()?
        .unwrap_or_else(|| parse_datetime("9999-12-31T23:59:59Z").unwrap())
        .to_rfc3339();
    Ok(client.events_between(from, to).await?)
}

async fn remote_find_calendar_event(
    client: &task_core::service::CalendarServiceClient,
    reference: &str,
) -> eyre::Result<CalendarEvent> {
    remote_calendar_events_for_range(client, None, None)
        .await?
        .into_iter()
        .find(|e| e.id.as_deref() == Some(reference) || e.title == reference)
        .ok_or_else(|| eyre::eyre!("Calendar event not found: {reference}"))
}

/// Typed inputs for [`build_calendar_patch`].
struct CalendarPatchInput {
    title: Option<String>,
    start: Option<String>,
    end: Option<String>,
    description: Option<String>,
    location: Option<String>,
    venue: Option<String>,
    space: Vec<String>,
    all_day: Option<bool>,
    status: Option<String>,
    recurrence: Option<String>,
    attendees: Option<String>,
    body: Option<String>,
}

fn build_calendar_patch(input: CalendarPatchInput) -> eyre::Result<CalendarEventPatch> {
    let CalendarPatchInput {
        title,
        start,
        end,
        description,
        location,
        venue,
        space,
        all_day,
        status,
        recurrence,
        attendees,
        body,
    } = input;
    Ok(CalendarEventPatch {
        title,
        description: description.map(optional_string_field),
        location: location.map(optional_string_field),
        venue: venue.map(|venue| {
            if venue == "clear" || venue.is_empty() {
                None
            } else {
                Some(WikiLink(venue))
            }
        }),
        spaces: if space.is_empty() {
            None
        } else {
            Some(space.into_iter().map(WikiLink).collect())
        },
        start: start.as_deref().map(parse_datetime).transpose()?,
        end: match end {
            Some(s) if s == "clear" || s.is_empty() => Some(None),
            Some(s) => Some(Some(parse_datetime(&s)?)),
            None => None,
        },
        all_day,
        status: status.as_deref().map(parse_calendar_status).transpose()?,
        recurrence: recurrence.map(|s| {
            if s == "clear" || s.is_empty() {
                None
            } else {
                Some(s)
            }
        }),
        attendees: attendees.map(|s| {
            if s.is_empty() {
                Vec::new()
            } else {
                s.split(',').map(|a| a.trim().to_string()).collect()
            }
        }),
        body,
    })
}

fn apply_remote_calendar_patch(event: &mut CalendarEvent, patch: CalendarEventPatch) {
    if let Some(value) = patch.title {
        event.title = value;
    }
    if let Some(value) = patch.description {
        event.description = value;
    }
    if let Some(value) = patch.location {
        event.location = value;
    }
    if let Some(value) = patch.venue {
        event.venue = value;
    }
    if let Some(value) = patch.spaces {
        event.spaces = value.into();
    }
    if let Some(value) = patch.start {
        event.start = value;
    }
    if let Some(value) = patch.end {
        event.end = value;
    }
    if let Some(value) = patch.all_day {
        event.all_day = value;
    }
    if let Some(value) = patch.status {
        event.status = value;
    }
    if let Some(value) = patch.recurrence {
        event.recurrence = value;
    }
    if let Some(value) = patch.attendees {
        event.attendees = value.into();
    }
    if let Some(value) = patch.body {
        event.body = value;
    }
    event.date_modified = Some(Utc::now());
}

fn print_projects_table(projects: &[Project]) {
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

fn project_dashboard_bucket_label(bucket: &task_core::ProjectDashboardBucket) -> &'static str {
    match bucket {
        task_core::ProjectDashboardBucket::Overdue => "overdue",
        task_core::ProjectDashboardBucket::DueSoon => "due soon",
        task_core::ProjectDashboardBucket::Active => "active",
        task_core::ProjectDashboardBucket::NoOpenTasks => "done",
    }
}

fn project_progress_bar(percent: Option<f32>) -> String {
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

fn print_project_dashboard(entries: &[task_core::ProjectDashboardEntry], json: bool) {
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

fn print_sync_stats(stats: &SyncStats) {
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

fn print_sync_plan(plan: &task_core::SyncPlan, json: bool) {
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

async fn run_remote_agent_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: AgentCommands,
) -> eyre::Result<()> {
    match command {
        AgentCommands::Snapshot {
            activity_limit,
            conflict_limit,
            include_completed,
        } => {
            let task_repo = remote.task_repo().await?;
            let task_service = remote.task().await?;
            let project_repo = remote.project_repo().await?;
            let client_repo = remote.client_repo().await?;
            let invoice_repo = remote.invoice_repo().await?;
            let calendar_client = remote.calendar().await?;
            let time_client = remote.time().await?;
            let activity_client = remote.activity().await?;

            let tasks = if include_completed {
                remote_list_tasks_with_client(&task_repo).await?
            } else {
                task_service
                    .execute_query(Query {
                        filters: vec![
                            Filter::NotComplete,
                            Filter::NotCancelled,
                            Filter::NotArchived,
                        ],
                        sort: Sort::Urgency,
                        limit: None,
                        group: None,
                    })
                    .await?
            };
            let projects = remote_list_projects_with_client(&project_repo).await?;
            let clients = remote_list_clients_with_client(&client_repo).await?;
            let invoices = remote_list_invoices_with_client(&invoice_repo).await?;
            let calendar_events = calendar_client
                .events_between("1970-01-01T00:00:00Z".into(), "9999-12-31T23:59:59Z".into())
                .await?;
            let time_entries = time_client
                .list_time_entries(TimeEntryFilter::default())
                .await?;
            let active_timer = time_client.active_timer().await?;
            let activity = activity_client.recent_activity(activity_limit).await?;
            let conflicts = activity_client.list_conflicts(true, conflict_limit).await?;
            let sync_status = calendar_client.sync_status().await?;

            print_agent_snapshot(AgentSnapshot {
                source: "remote",
                location: &remote.display_url,
                actor,
                tasks: &tasks,
                projects: &projects,
                clients: &clients,
                invoices: &invoices,
                calendar_events: &calendar_events,
                time_entries: &time_entries,
                active_timer: active_timer.as_ref().map(|entry| AgentActiveTimer {
                    title: &entry.task_title,
                    entry: &entry.entry,
                }),
                activity: &activity,
                conflicts: &conflicts,
                sync_status: sync_status.as_ref(),
            });
        }
        AgentCommands::Task { reference } => {
            let client = remote.task_repo().await?;
            let tasks = remote_list_tasks_with_client(&client).await?;
            let task = find_task_in(tasks, &reference)?;
            println!("{}", facet_json::to_string(&task).unwrap_or_default());
        }
        AgentCommands::Plan { reference } => {
            let client = remote.task_repo().await?;
            let tasks = remote_list_tasks_with_client(&client).await?;
            let task = find_task_in(tasks, &reference)?;
            let plan = build_agent_plan(&task);
            println!("{}", facet_json::to_string(&plan).unwrap_or_default());
        }
        AgentCommands::Project { name } => {
            let project_service = remote.project().await?;
            let project_repo = remote.project_repo().await?;
            let projects = remote_list_projects_with_client(&project_repo).await?;
            let project = projects
                .into_iter()
                .find(|p| p.title.eq_ignore_ascii_case(&name))
                .ok_or_else(|| eyre::eyre!("Project not found: {name}"))?;
            let stats = project_service.project_stats(name.clone()).await?;
            let next = project_service.next_task(name.clone()).await?;
            let tasks = project_service.tasks_for_project(name).await?;
            println!(
                "{{\"project\":{},\"stats\":{},\"next_task\":{},\"tasks\":{}}}",
                facet_json::to_string(&project).unwrap_or_default(),
                facet_json::to_string(&stats).unwrap_or_default(),
                next.as_ref()
                    .map(|t| facet_json::to_string(t).unwrap_or_default())
                    .unwrap_or_else(|| "null".into()),
                tasks_json(&tasks),
            );
        }
        AgentCommands::Calendar { from, to } => {
            let client = remote.calendar().await?;
            let from = from
                .as_deref()
                .map(parse_calendar_boundary_start)
                .transpose()?
                .unwrap_or_else(|| parse_datetime("1970-01-01T00:00:00Z").unwrap())
                .to_rfc3339();
            let to = to
                .as_deref()
                .map(parse_calendar_boundary_end)
                .transpose()?
                .unwrap_or_else(|| parse_datetime("9999-12-31T23:59:59Z").unwrap())
                .to_rfc3339();
            let events = client.events_between(from, to).await?;
            println!("{}", calendar_events_json(&events));
        }
        AgentCommands::Time {
            task,
            user,
            project,
            client,
            tag,
            from,
            to,
            billable,
        } => {
            let time_client = remote.time().await?;
            let entries = time_client
                .list_time_entries(TimeEntryFilter {
                    task_ref: task,
                    user,
                    project,
                    client,
                    tag,
                    from: from.as_deref().map(parse_date_start).transpose()?,
                    to: to.as_deref().map(parse_date_end).transpose()?,
                    billable_only: billable,
                })
                .await?;
            println!("{}", time_entries_json(&entries));
        }
        AgentCommands::Sync { trigger } => {
            let client = remote.calendar().await?;
            if trigger {
                let stats = client.trigger_sync().await?;
                println!(
                    "{{\"triggered\":true,\"stats\":{}}}",
                    facet_json::to_string(&stats).unwrap_or_default()
                );
            } else {
                let stats = client.sync_status().await?;
                println!(
                    "{{\"triggered\":false,\"stats\":{}}}",
                    stats
                        .as_ref()
                        .map(|s| facet_json::to_string(s).unwrap_or_default())
                        .unwrap_or_else(|| "null".into())
                );
            }
        }
        AgentCommands::Capabilities => print_agent_capabilities(),
        AgentCommands::Bootstrap { json } => {
            let capabilities = remote.system().await?.capabilities().await.ok();
            print_agent_bootstrap(Some(remote), capabilities.as_ref(), json);
        }
    }
    Ok(())
}

fn find_task_in(tasks: Vec<Task>, reference: &str) -> eyre::Result<Task> {
    tasks
        .into_iter()
        .find(|t| t.matches_reference(reference))
        .ok_or_else(|| eyre::eyre!("Task not found: {reference}"))
}

fn normalize_profile_url(url: &str) -> String {
    url.trim().trim_end_matches('/').to_string()
}

fn normalize_vox_url(server: &str) -> String {
    let trimmed = server.trim().trim_end_matches('/');
    if trimmed.starts_with("ws://") || trimmed.starts_with("wss://") {
        trimmed.to_string()
    } else if let Some(rest) = trimmed.strip_prefix("https://") {
        format!("wss://{}/vox", rest.trim_end_matches("/vox"))
    } else if let Some(rest) = trimmed.strip_prefix("http://") {
        format!("ws://{}/vox", rest.trim_end_matches("/vox"))
    } else {
        format!("ws://{}/vox", trimmed.trim_end_matches("/vox"))
    }
}

fn append_query_param(url: &mut String, key: &str, value: &str) {
    let separator = if url.contains('?') { '&' } else { '?' };
    url.push(separator);
    url.push_str(key);
    url.push('=');
    url.push_str(&percent_encode_query_value(value));
}

fn percent_encode_query_value(value: &str) -> String {
    let mut out = String::new();
    for byte in value.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' => {
                out.push(byte as char);
            }
            _ => out.push_str(&format!("%{byte:02X}")),
        }
    }
    out
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

    let url =
        std::env::var("NEXTCLOUD_URL").map_err(|_| eyre::eyre!("Set NEXTCLOUD_URL env var."))?;
    let env_user = std::env::var("NEXTCLOUD_USER").ok();
    let username = as_user
        .clone()
        .or(env_user)
        .ok_or_else(|| eyre::eyre!("Set NEXTCLOUD_USER env var or pass --as-user."))?;
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
    use task_core::provider::{CommunicationChannelProvider, TalkClient, TalkConfig};

    let url =
        std::env::var("NEXTCLOUD_URL").map_err(|_| eyre::eyre!("Set NEXTCLOUD_URL env var."))?;
    let env_user = std::env::var("NEXTCLOUD_USER").ok();
    let username = as_user
        .clone()
        .or(env_user)
        .ok_or_else(|| eyre::eyre!("Set NEXTCLOUD_USER env var or pass --as-user."))?;
    let password = std::env::var("NEXTCLOUD_PASSWORD")
        .map_err(|_| eyre::eyre!("Set NEXTCLOUD_PASSWORD env var."))?;

    let client = TalkClient::new(TalkConfig {
        url,
        username,
        password,
    });

    match cmd {
        TalkCommands::Rooms { json } => {
            let rooms = client.list_conversations().await?;
            print_channel_rooms(&rooms, json);
        }
        TalkCommands::Send {
            room,
            message,
            reply_to,
        } => {
            let sent = CommunicationChannelProvider::send_message(
                &client,
                ChannelSendMessageRequest {
                    conversation_id: room,
                    body: message,
                    reply_to: reply_to.map(|id| id.to_string()),
                },
            )
            .await?;
            println!("Sent message {} to {}.", sent.id, sent.conversation_id);
        }
        TalkCommands::History { room, limit, json } => {
            let msgs = CommunicationChannelProvider::recent_messages(&client, &room, limit).await?;
            print_channel_history(&msgs, json);
        }
    }
    Ok(())
}

fn print_channel_rooms(rooms: &[ChannelConversation], json: bool) {
    if json {
        print_channel_rooms_json(rooms);
    } else {
        print_channel_rooms_table(rooms);
    }
}

fn print_asset_result(asset: &Asset, json: bool) {
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

fn print_assets(assets: &[Asset], json: bool) {
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

fn print_asset_conflicts(conflicts: &[AssetConflict], json: bool) {
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

fn print_channel_rooms_table(rooms: &[ChannelConversation]) {
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

fn print_channel_rooms_json(rooms: &[ChannelConversation]) {
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

fn print_channel_history(msgs: &[ChannelMessage], json: bool) {
    if json {
        print_channel_history_json(msgs);
    } else {
        print_channel_history_table(msgs);
    }
}

fn print_channel_history_table(msgs: &[ChannelMessage]) {
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

fn print_channel_history_json(msgs: &[ChannelMessage]) {
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

fn parse_calendar_boundary_start(s: &str) -> eyre::Result<DateTime<Utc>> {
    parse_datetime(s).or_else(|_| parse_date_start(s))
}

fn parse_calendar_boundary_end(s: &str) -> eyre::Result<DateTime<Utc>> {
    parse_datetime(s).or_else(|_| parse_date_end(s))
}

fn parse_calendar_status(s: &str) -> eyre::Result<CalendarEventStatus> {
    match s.to_lowercase().as_str() {
        "confirmed" | "confirm" => Ok(CalendarEventStatus::Confirmed),
        "tentative" => Ok(CalendarEventStatus::Tentative),
        "cancelled" | "canceled" => Ok(CalendarEventStatus::Cancelled),
        _ => eyre::bail!("Unknown calendar status: {s}"),
    }
}

fn optional_string_field(s: String) -> Option<String> {
    if s == "clear" || s.is_empty() {
        None
    } else {
        Some(s)
    }
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

fn print_time_entries_json(entries: &[task_core::TimeEntryContext]) {
    println!("{}", time_entries_json(entries));
}

fn time_entries_json(entries: &[TimeEntryContext]) -> String {
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

fn print_time_entries_csv(entries: &[task_core::TimeEntryContext]) {
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

fn print_report_csv(rows: &[ReportRow], group_by: &str) {
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

fn print_report_table(rows: &[ReportRow]) {
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

fn print_activity_table(rows: &[ChangeRow]) {
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

// ── Calendar output ─────────────────────────────────────────────────────────

fn print_calendar_events_table(events: &[CalendarEvent]) {
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

fn print_calendar_event_detail(event: &CalendarEvent) {
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

fn print_calendar_events_json(events: &[CalendarEvent]) {
    println!("{}", calendar_events_json(events));
}

fn calendar_events_json(events: &[CalendarEvent]) -> String {
    let items = events
        .iter()
        .map(|event| facet_json::to_string(event).unwrap_or_default())
        .collect::<Vec<_>>();
    format!("[{}]", items.join(","))
}

fn calendar_status_label(status: &CalendarEventStatus) -> &'static str {
    match status {
        CalendarEventStatus::Confirmed => "confirmed",
        CalendarEventStatus::Tentative => "tentative",
        CalendarEventStatus::Cancelled => "cancelled",
    }
}

// ── Agent output ────────────────────────────────────────────────────────────

struct AgentSnapshot<'a> {
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

struct AgentActiveTimer<'a> {
    title: &'a str,
    entry: &'a task_core::TimeEntry,
}

fn print_agent_snapshot(snapshot: AgentSnapshot<'_>) {
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

fn print_agent_capabilities() {
    println!(
        "{{\"binary\":\"task\",\"package\":\"task-cli\",\"install\":{},\"global_flags\":[\"--vault\",\"--server\",\"--session-token\",\"--organization-id\",\"--as-user\"],\"agent_commands\":[\"snapshot\",\"task\",\"plan\",\"project\",\"calendar\",\"time\",\"sync\",\"capabilities\",\"bootstrap\"],\"control_commands\":[\"doctor\",\"doctor --deep\",\"server add\",\"server list\",\"server use\",\"capture\",\"inbox list\",\"inbox promote\",\"add\",\"update\",\"complete\",\"delete\",\"calendar add\",\"calendar update\",\"calendar delete\",\"email accounts\",\"email search\",\"email show\",\"email link\",\"email sweep\",\"time log\",\"time edit\",\"start\",\"stop\",\"sync\"],\"remote_mode\":\"Set --server plus --session-token to route supported inbox, task, project, client, invoice, time, calendar, email, activity, conflict, system, and agent commands over Vox; --organization-id routes multi-instance organization requests.\"}}",
        agent_install_json()
    );
}

fn print_doctor_json(capabilities: &SystemCapabilities, health: &SystemHealth) {
    println!(
        "{{\"capabilities\":{},\"health\":{}}}",
        facet_json::to_string(capabilities).unwrap_or_default(),
        facet_json::to_string(health).unwrap_or_default()
    );
}

fn doctor_check_status(check: &task_core::HealthCheck) -> &str {
    if check.ok {
        "ok"
    } else if check.severity == "warning" {
        "warning"
    } else {
        "failed"
    }
}

fn print_doctor_report(
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

fn print_agent_bootstrap(
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

fn agent_install_json() -> String {
    "{\"nix\":\"nix profile install .#task-cli\",\"build\":\"nix build .#task-cli\",\"cargo\":\"cargo install --path crates/task-cli\"}".into()
}

fn tasks_json(tasks: &[Task]) -> String {
    format!(
        "[{}]",
        tasks
            .iter()
            .map(|task| facet_json::to_string(task).unwrap_or_default())
            .collect::<Vec<_>>()
            .join(",")
    )
}

fn projects_json(projects: &[Project]) -> String {
    format!(
        "[{}]",
        projects
            .iter()
            .map(|project| facet_json::to_string(project).unwrap_or_default())
            .collect::<Vec<_>>()
            .join(",")
    )
}

fn clients_json(clients: &[Client]) -> String {
    format!(
        "[{}]",
        clients
            .iter()
            .map(|client| facet_json::to_string(client).unwrap_or_default())
            .collect::<Vec<_>>()
            .join(",")
    )
}

fn invoices_json(invoices: &[Invoice]) -> String {
    format!(
        "[{}]",
        invoices
            .iter()
            .map(|invoice| facet_json::to_string(invoice).unwrap_or_default())
            .collect::<Vec<_>>()
            .join(",")
    )
}

fn active_timer_json(active: Option<AgentActiveTimer<'_>>) -> String {
    match active {
        Some(active) => format!(
            "{{\"task\":\"{}\",\"entry\":{}}}",
            escape_json(active.title),
            facet_json::to_string(active.entry).unwrap_or_default()
        ),
        None => "null".into(),
    }
}

fn activity_json(rows: &[ChangeRow]) -> String {
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

fn conflicts_json(rows: &[ConflictRow]) -> String {
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

fn parse_project_status(s: &str) -> Option<task_core::project::ProjectStatus> {
    match s.to_lowercase().replace(['-', '_', ' '], "").as_str() {
        "planning" => Some(task_core::project::ProjectStatus::Planning),
        "active" => Some(task_core::project::ProjectStatus::Active),
        "onhold" | "hold" => Some(task_core::project::ProjectStatus::OnHold),
        "completed" | "done" => Some(task_core::project::ProjectStatus::Completed),
        "archived" => Some(task_core::project::ProjectStatus::Archived),
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
        _ => Err(format!(
            "Unknown priority: {s}. Use: none, low, normal, high, urgent"
        )),
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

fn print_project_context(context: Option<&ProjectKnowledgeContext>, json: bool) {
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
            m.unread
                .map(|n| n.to_string())
                .unwrap_or_else(|| "—".into()),
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

fn print_mail_tags_table(tags: &[task_core::provider::MailTag]) {
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

fn print_mail_tags_json(tags: &[task_core::provider::MailTag]) {
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

fn print_mail_messages_table(messages: &[task_core::provider::MailMessage]) {
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

fn print_mail_messages_json(messages: &[task_core::provider::MailMessage]) {
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

fn print_mail_detail(msg: &task_core::provider::MailMessageDetail, body: Option<&str>) {
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

fn print_mail_detail_json(msg: &task_core::provider::MailMessageDetail, body: Option<&str>) {
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

fn print_finance_report(report: &BusinessFinanceReport, json: bool) {
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

fn super_time_entry_cents(entry: &TimeEntryContext) -> u64 {
    let minutes = entry.entry.duration_minutes() as u64;
    let rate = entry.effective_rate(None) as u64;
    ((minutes * rate) + 30) / 60
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

fn print_clients_table(clients: &[task_core::Client]) {
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
        println!("{indent}@{}{date}{tc}{resolved}", c.author);
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

fn print_tasks_json(tasks: &[Task]) {
    println!("[");
    for (i, task) in tasks.iter().enumerate() {
        let comma = if i + 1 < tasks.len() { "," } else { "" };
        let json =
            facet_json::to_string(task).unwrap_or_else(|e| format!("{{\"error\":\"{e}}}\"}}",));
        println!("  {json}{comma}");
    }
    println!("]");
}

fn print_inbox_capture(item: &InboxItem, json: bool) {
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

fn print_inbox_items(items: &[InboxItem], json: bool) {
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

fn print_review_report(report: &ReviewReport, json: bool) {
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

fn print_operating_model(report: &OperatingModelReport, json: bool) {
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

fn print_review_task_bucket(label: &str, tasks: &[Task]) {
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

fn print_people(people: &[Person], json: bool) {
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

fn print_organizations(organizations: &[OrganizationRecord], json: bool) {
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

fn print_person_context(context: Option<&PersonContext>, json: bool) {
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

fn print_organization_context(context: Option<&OrganizationContext>, json: bool) {
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

fn print_sync_states(states: &[ProviderSyncState], json: bool) {
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

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}…", &s[..max.saturating_sub(1)])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn seeds_deterministic_demo_vault_files() {
        let root = std::env::temp_dir().join(format!(
            "task-demo-seed-test-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir_all(&root).unwrap();

        let summary = seed_demo_vault(
            &root,
            "Acme Org",
            "Acme Client",
            "Demo Smoke Project",
            Some("tester"),
        )
        .unwrap();

        assert_eq!(summary.project, "Demo Smoke Project");
        assert_eq!(summary.files.len(), 7);
        let project_file = root.join("Demo Smoke Project").join("project.md");
        let billable_task = root
            .join("Demo Smoke Project")
            .join("tasks")
            .join("Demo billable work item.md");
        let inbox_file = root.join("inbox").join("demo-capture.md");
        let invoice_file = root.join("invoices").join("demo-invoice.md");
        let event_file = root.join("calendar").join("demo-event.md");

        for path in [
            &project_file,
            &billable_task,
            &inbox_file,
            &invoice_file,
            &event_file,
        ] {
            assert!(path.exists(), "expected seeded file {}", path.display());
        }
        let billable = std::fs::read_to_string(&billable_task).unwrap();
        assert!(billable.contains("demo-time-entry-billable"));
        assert!(billable.contains("Deterministic billable smoke-test work"));
        let invoice = std::fs::read_to_string(&invoice_file).unwrap();
        assert!(invoice.contains("client: \"[[Acme Client]]\""));
        assert!(invoice.contains("total_cents: 18000"));

        std::fs::remove_dir_all(&root).unwrap();
    }

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
