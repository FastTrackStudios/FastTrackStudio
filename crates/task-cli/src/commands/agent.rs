#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum AgentCommands {
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

pub(crate) async fn run_remote_agent_command(
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
