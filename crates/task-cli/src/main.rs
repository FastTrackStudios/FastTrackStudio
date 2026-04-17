use clap::{Parser, Subcommand};
use chrono::{DateTime, TimeZone, Utc};
use task_core::index::{ChangeRow, ConflictRow};
use task_core::workflows::{parse_comments, render_comments, Comment};
use task_core::{
    Filter, Priority, Query, RelationType, Sort, Status, Task, TaskRelation, TimeEntry,
    TimeEntryFilter, VaultServiceImpl, WikiLink,
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
        /// From date (YYYY-MM-DD, inclusive)
        #[arg(long)]
        from: Option<String>,
        /// To date (YYYY-MM-DD, inclusive)
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        billable: bool,
        #[arg(long)]
        json: bool,
    },
    /// Aggregate time by task or user
    Report {
        /// task | user
        #[arg(long, default_value = "task")]
        group_by: String,
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        billable: bool,
        /// Fallback billable rate in cents per hour (used when an entry has no rate override)
        #[arg(long)]
        rate: Option<u32>,
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
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    let cli = Cli::parse();

    let vault_path = cli.vault.ok_or_else(|| {
        eyre::eyre!("No vault specified. Use --vault <path> or set TASK_VAULT env var.")
    })?;

    let svc = VaultServiceImpl::new(&vault_path);
    let actor = cli.as_user;

    match cli.command {
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
                    from,
                    to,
                    billable,
                    json,
                },
        } => {
            let filter = TimeEntryFilter {
                task_ref: task,
                user,
                from: from.as_deref().map(parse_date_start).transpose()?,
                to: to.as_deref().map(parse_date_end).transpose()?,
                billable_only: billable,
            };
            let entries = svc.list_time_entries(filter).await;
            if json {
                print_time_entries_json(&entries);
            } else {
                print_time_entries_table(&entries);
            }
        }

        Commands::Time {
            command:
                TimeCommands::Report {
                    group_by,
                    from,
                    to,
                    billable,
                    rate,
                    json,
                },
        } => {
            let filter = TimeEntryFilter {
                task_ref: None,
                user: None,
                from: from.as_deref().map(parse_date_start).transpose()?,
                to: to.as_deref().map(parse_date_end).transpose()?,
                billable_only: billable,
            };
            let entries = svc.list_time_entries(filter).await;
            let report = aggregate_time(&entries, &group_by, rate)?;
            if json {
                print_report_json(&report);
            } else {
                print_report_table(&report);
            }
        }

        Commands::Time {
            command: TimeCommands::Delete { entry_id },
        } => {
            svc.delete_time_entry(&entry_id).await?;
            println!("Deleted entry {entry_id}.");
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

fn print_time_entries_table(entries: &[(String, TimeEntry)]) {
    if entries.is_empty() {
        println!("No time entries.");
        return;
    }
    let title_w = entries
        .iter()
        .map(|(t, _)| t.len())
        .max()
        .unwrap_or(10)
        .max(5)
        .min(35);
    println!(
        "{:<title_w$}  {:<19}  {:>6}  {:<8}  {:<12}  ID",
        "TASK", "START (UTC)", "MIN", "BILLABLE", "USER",
    );
    println!("{}", "─".repeat(title_w + 55));
    for (title, e) in entries {
        let t = truncate(title, title_w);
        let start = e.start_time.format("%Y-%m-%d %H:%M:%S").to_string();
        let mins = if e.is_running() {
            format!("▶{}", e.elapsed_minutes(Utc::now()))
        } else {
            e.duration_minutes().to_string()
        };
        let billable = if e.billable { "yes" } else { "no" };
        let user = e.user.clone().unwrap_or_else(|| "—".into());
        println!(
            "{:<title_w$}  {:<19}  {:>6}  {:<8}  {:<12}  {}",
            t, start, mins, billable, user, e.id
        );
    }
    println!("\n{} entr{}", entries.len(), if entries.len() == 1 { "y" } else { "ies" });
}

fn print_time_entries_json(entries: &[(String, TimeEntry)]) {
    println!("[");
    for (i, (title, e)) in entries.iter().enumerate() {
        let comma = if i + 1 < entries.len() { "," } else { "" };
        let entry_json = facet_json::to_string(e).unwrap_or_default();
        println!(
            "  {{\"task\":\"{}\",\"entry\":{}}}{comma}",
            escape_json(title),
            entry_json
        );
    }
    println!("]");
}

/// ({group_key}, total_minutes, billable_cents, entry_count)
type ReportRow = (String, u64, u64, usize);

fn aggregate_time(
    entries: &[(String, TimeEntry)],
    group_by: &str,
    fallback_rate: Option<u32>,
) -> eyre::Result<Vec<ReportRow>> {
    use std::collections::BTreeMap;
    let mut acc: BTreeMap<String, (u64, u64, usize)> = BTreeMap::new();
    for (title, e) in entries {
        let key = match group_by {
            "task" => title.clone(),
            "user" => e.user.clone().unwrap_or_else(|| "—".into()),
            other => eyre::bail!("Unknown group_by: {other}. Use 'task' or 'user'."),
        };
        let mins = e.duration_minutes() as u64;
        let cents = if e.billable {
            let rate = e.billable_rate.or(fallback_rate).unwrap_or(0);
            (mins * rate as u64) / 60
        } else {
            0
        };
        let slot = acc.entry(key).or_insert((0, 0, 0));
        slot.0 += mins;
        slot.1 += cents;
        slot.2 += 1;
    }
    let mut rows: Vec<ReportRow> = acc
        .into_iter()
        .map(|(k, (m, c, n))| (k, m, c, n))
        .collect();
    rows.sort_by(|a, b| b.1.cmp(&a.1));
    Ok(rows)
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

