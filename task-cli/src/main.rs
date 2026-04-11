use clap::{Parser, Subcommand};
use vault_core::{Filter, Priority, Query, Sort, Status, Task, VaultServiceImpl, WikiLink};

#[derive(Parser)]
#[command(name = "task", about = "Task management CLI", version)]
struct Cli {
    /// Path to the vault directory
    #[arg(long, env = "TASK_VAULT", global = true)]
    vault: Option<String>,

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
    },
    /// Mark a task as complete
    Complete {
        title: String,
    },
    /// Show detailed info for a task
    Show {
        title: String,
    },
    /// Project subcommands
    Project {
        #[command(subcommand)]
        command: ProjectCommands,
    },
}

#[derive(Subcommand)]
enum ProjectCommands {
    /// List all projects
    List,
    /// Show task stats for a project
    Stats { name: String },
    /// Show the next actionable task for a project
    Next { name: String },
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    let cli = Cli::parse();

    let vault_path = cli.vault.ok_or_else(|| {
        eyre::eyre!("No vault specified. Use --vault <path> or set TASK_VAULT env var.")
    })?;

    let svc = VaultServiceImpl::new(&vault_path);

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
                ..default_task()
            };

            let created = svc.create_task(task).await?;
            println!("Created: {}", created.title);
            println!("  id:  {}", created.id.as_deref().unwrap_or("—"));
            if let Some(d) = created.due {
                println!("  due: {d}");
            }
        }

        Commands::Complete { title } => {
            let task = svc.complete_task(title).await?;
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

        Commands::Show { title } => {
            let tasks = svc.list_tasks().await;
            let task = tasks
                .iter()
                .find(|t| t.title.eq_ignore_ascii_case(&title))
                .ok_or_else(|| eyre::eyre!("Task not found: {title}"))?;
            print_task_detail(task);
        }

        Commands::Project {
            command: ProjectCommands::List,
        } => {
            let projects = svc.list_projects().await;
            if projects.is_empty() {
                println!("No projects found.");
                return Ok(());
            }
            let name_w = projects.iter().map(|p| p.title.len()).max().unwrap_or(10) + 2;
            println!(
                "{:<name_w$}  {:<10}  {}",
                "NAME", "STATE", "DUE"
            );
            println!("{}", "─".repeat(name_w + 20));
            for p in &projects {
                let state = format!("{:?}", p.status);
                let due = p.due.map(|d| d.to_string()).unwrap_or_else(|| "—".to_string());
                println!("{:<name_w$}  {:<10}  {}", p.title, state, due);
            }
            println!("\n{} project(s)", projects.len());
        }

        Commands::Project {
            command: ProjectCommands::Stats { name },
        } => {
            let stats = svc.project_stats(name.clone()).await;
            println!("Project: {name}");
            println!("  Open:      {}", stats.open_task_count);
            println!("  Completed: {}", stats.completed_task_count);
            println!("  Total:     {}", stats.total());
            if let Some(pct) = stats.completion_percent() {
                println!("  Progress:  {:.0}%", pct);
            }
        }

        Commands::Project {
            command: ProjectCommands::Next { name },
        } => {
            match svc.next_task(name.clone()).await {
                Some(task) => {
                    println!("Next task for '{}'", name);
                    print_task_detail(&task);
                }
                None => println!("No actionable tasks for '{}'.", name),
            }
        }
    }

    Ok(())
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

fn default_task() -> Task {
    Task {
        id: None,
        title: String::new(),
        status: Status::Open,
        priority: Priority::None,
        projects: vec![],
        contexts: vec![],
        tags: vec![],
        areas: vec![],
        due: None,
        scheduled: None,
        start: None,
        due_time: None,
        date_created: None,
        date_modified: None,
        completed_date: None,
        time_estimate: None,
        time_entries: vec![],
        pomodoro_count: None,
        recurrence: None,
        recurrence_anchor: vault_core::RecurrenceAnchor::Scheduled,
        completed_instances: vec![],
        skipped_instances: vec![],
        blocked_by: vec![],
        blocking: vec![],
        reminders: vec![],
        sort_order: None,
        external_id: None,
        external_source: None,
        assignee: None,
        created_by: None,
        body: String::new(),
    }
}
