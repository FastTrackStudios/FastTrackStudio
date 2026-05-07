#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum TimeCommands {
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

pub(crate) async fn run_remote_time_command(
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
