#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum ProjectCommands {
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
pub(crate) struct ProjectEditArgs {
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
pub(crate) enum ProjectCommentCommands {
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

pub(crate) fn apply_remote_project_patch(
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

pub(crate) async fn run_remote_project_command(
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
            project_item.body = Some(crate::commands::comment::splice_comments(
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
            project_item.body = Some(crate::commands::comment::splice_comments(
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
            project_item.body = Some(crate::commands::comment::splice_comments(
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
            project_item.body = Some(crate::commands::comment::splice_comments(
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
