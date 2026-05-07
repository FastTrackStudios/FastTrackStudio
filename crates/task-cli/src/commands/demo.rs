#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum DemoCommands {
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

pub(crate) fn run_demo_command(
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
pub(crate) struct DemoSeedSummary {
    pub(crate) project: String,
    pub(crate) files: Vec<String>,
}

pub(crate) fn seed_demo_vault(
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
