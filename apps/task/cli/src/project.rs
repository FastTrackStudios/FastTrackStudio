//! `task project …` — projects served by the active org.
//!
//! Moved verbatim out of `main.rs`; behaviour unchanged.

use clap::Subcommand;

use crate::connect_task_client;
use crate::establish_client;
use crate::establish_for_url;
use crate::resolve_active_org;
use crate::resolve_org_vox_url;
use crate::shared::confirm;
use crate::shared::resolve_body;

#[derive(Subcommand)]
pub(crate) enum ProjectCmd {
    /// List every project the active org's vault carries.
    /// Output: one row per project with status + parent
    /// breadcrumb. Pass `--json` for machine-readable output.
    List {
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit JSON instead of the human-readable table.
        #[arg(long)]
        json: bool,
    },
    /// Fetch one project by id or by vault-relative path.
    /// Prints title + status + tags + the full details body.
    Get {
        /// Project UUID OR vault-relative path
        /// (`Projects/Health/Fitness/Fitness.md`).
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Create a new project. Title is the only required
    /// argument; sensible defaults fill the rest. The backend
    /// chooses a `Projects/<slug>.md` path unless `--path`
    /// overrides it.
    Create {
        title: String,
        /// Vault-relative path. Default: `Projects/<slug>.md`.
        #[arg(long)]
        path: Option<String>,
        /// Parent project id OR vault-relative path. Resolved
        /// against `list()` before the create call so paths
        /// work too.
        #[arg(long)]
        parent: Option<String>,
        /// One of `active|on_hold|done|cancelled`. Default
        /// `active`.
        #[arg(long)]
        status: Option<String>,
        /// `p0..p4` / `urgent|high|normal|low|lowest`. Default
        /// `normal`.
        #[arg(long)]
        priority: Option<String>,
        /// Project type / template — `code` | `general` | `personal`.
        /// Drives the overview layout. Default `general`.
        #[arg(long = "type")]
        project_type: Option<String>,
        /// Comma-separated tag list.
        #[arg(long, value_delimiter = ',')]
        tags: Vec<String>,
        /// Body / details (markdown). Reads stdin when `-`.
        #[arg(long)]
        details: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Set the project status. Convenience over `update`.
    SetStatus {
        target: String,
        status: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting project as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Show the project's state registry — every status name
    /// with its canonical group (backlog / unstarted / started /
    /// completed / cancelled). Falls back to the default
    /// registry when the project declares no `states:` config.
    States {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the registry as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Edit the project's state registry. `--from <file|->`
    /// replaces the whole registry (YAML or JSON list of
    /// `{name, group, color?, default?, order?}`); repeatable
    /// `--add <name>:<group>[:<color>]` upserts single states
    /// (starting from the current registry, or the default set
    /// when none is configured). `--clear` drops the custom
    /// registry (back to defaults).
    SetStates {
        target: String,
        /// Replace the registry from a YAML/JSON file (`-` for
        /// stdin).
        #[arg(long)]
        from: Option<String>,
        /// Upsert one state: `<name>:<group>[:<color>]`, group ∈
        /// backlog|unstarted|started|completed|cancelled.
        #[arg(long = "add", value_name = "NAME:GROUP[:COLOR]")]
        add: Vec<String>,
        /// Mark this state name as the default for new tasks.
        #[arg(long = "default", value_name = "NAME")]
        default_state: Option<String>,
        /// Drop the custom registry entirely.
        #[arg(long)]
        clear: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting registry as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Set the project's target completion date (YYYY-MM-DD, or
    /// `none`/`clear` to unset). The Linear-style roadmap field.
    SetTarget {
        target: String,
        date: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting project as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Recompute + show the project's progress from its tasks
    /// (done / total of tasks whose `projectId` is this project).
    /// Writes the rolled-up `progress_percent` back.
    Progress {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set the project priority. Convenience over `update`.
    SetPriority {
        target: String,
        priority: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting project as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Set or clear the project parent. Pass `none` / `null`
    /// to unparent.
    SetParent {
        target: String,
        /// `none`, `null`, a project UUID, name, or a
        /// vault-relative path.
        parent: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting project as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Mark the project archived (kept on disk; timer refuses
    /// new sessions against it).
    Archive {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting project as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Reverse of `archive`.
    Unarchive {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting project as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Move the backing markdown file. Preserves `id` so
    /// downstream FKs (timer rows, links) survive.
    Rename {
        target: String,
        new_path: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the renamed project as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Delete the project. Refuses if any other project lists
    /// it as parent — reparent or delete children first.
    Delete {
        target: String,
        /// Skip the y/N prompt.
        #[arg(long, short = 'y')]
        yes: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

pub(crate) async fn run_project(cmd: ProjectCmd) -> eyre::Result<()> {
    use project::ProjectServiceClient;

    match cmd {
        ProjectCmd::List { org, server, json } => {
            let slug = resolve_active_org(org)?;
            let client: ProjectServiceClient = establish_client(server, &slug).await?;
            let rows = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;

            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }

            // Group by parent for human readability: roots
            // first, then each root's subprojects indented.
            let total = rows.len();
            let roots: Vec<&project::ProjectInfo> =
                rows.iter().filter(|p| p.parent_id.is_none()).collect();
            println!("{} projects ({} top-level)\n", total, roots.len());
            for root in roots {
                print_project_row(root, 0);
                let kids: Vec<&project::ProjectInfo> = rows
                    .iter()
                    .filter(|p| p.parent_id == Some(root.id))
                    .collect();
                for k in kids {
                    print_project_row(k, 2);
                }
            }
        }
        ProjectCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let client: ProjectServiceClient = establish_client(server, &slug).await?;
            let p = resolve_project_target(&client, &target).await?;

            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&p).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }

            println!("{} [{}]\n", p.title, p.status);
            println!("  id:       {}", p.id);
            println!("  path:     {}", p.path);
            println!("  priority: {}", p.priority);
            if let Some(parent) = p.parent_id {
                println!("  parent:   {parent}");
            }
            if !p.tags.0.is_empty() {
                println!("  tags:     {}", p.tags.0.join(", "));
            }
            if !p.details.is_empty() {
                println!("\n{}", p.details);
            }
        }
        ProjectCmd::Create {
            title,
            path,
            parent,
            status,
            priority,
            project_type,
            tags,
            details,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_project_client(&url).await?;

            let parent_id = match parent {
                None => None,
                Some(s) => Some(resolve_project_target(&client, &s).await?.id),
            };
            let details = resolve_body(details)?;
            let new_project = project::ProjectInfo {
                id: uuid::Uuid::nil(),
                path: path.unwrap_or_default(),
                title,
                status: status.unwrap_or_else(|| "active".into()),
                priority: priority.unwrap_or_else(|| "normal".into()),
                project_type: project_type.unwrap_or_else(|| "general".into()),
                lead: String::new(),
                tags: project::model::Tags(tags),
                parent_id,
                same_as: None,
                target_date: None,
                progress_percent: -1,
                details,
                client_id: None,
                billable_default: false,
                currency: String::new(),
                default_rate_cents: 0,
                estimated_seconds: 0,
                agent_profile: String::new(),
                color: String::new(),
                image: String::new(),
                archived: false,
                states: None,
                date_created: None,
                date_modified: None,
            };
            let created = client
                .create(new_project)
                .await
                .map_err(|e| eyre::eyre!("create: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&created).map_err(|e| eyre::eyre!("json: {e}"))?
                );
            } else {
                println!("created {} ({})", created.title, created.path);
                println!("  id: {}", created.id);
            }
        }
        ProjectCmd::SetStatus {
            target,
            status,
            org,
            server,
            json,
        } => {
            mutate_project(target, org, server, json, |p| p.status = status).await?;
        }
        ProjectCmd::States {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_project_client(&url).await?;
            let p = resolve_project_target(&client, &target).await?;
            let custom = p.states.is_some();
            let registry = p.states.clone().unwrap_or_else(project::default_states);
            if json {
                crate::json_out::print_json(&serde_json::json!({
                    "project": p.id,
                    "custom": custom,
                    "states": registry,
                }))?;
                return Ok(());
            }
            let origin = if custom { "custom" } else { "default" };
            println!("{}  ({origin} registry)\n", p.title);
            for s in registry.ordered() {
                let default = if s.default { "  (default)" } else { "" };
                let color = if s.color.is_empty() {
                    String::new()
                } else {
                    format!("  {}", s.color)
                };
                println!("  {:<18} {:<10}{color}{default}", s.name, s.group.as_str());
            }
        }
        ProjectCmd::SetStates {
            target,
            from,
            add,
            default_state,
            clear,
            org,
            server,
            json,
        } => {
            if clear && (from.is_some() || !add.is_empty()) {
                return Err(eyre::eyre!("--clear can't be combined with --from/--add"));
            }
            if !clear && from.is_none() && add.is_empty() && default_state.is_none() {
                return Err(eyre::eyre!(
                    "nothing to do — pass --from <file|->, --add <name>:<group>, \
                     --default <name>, or --clear"
                ));
            }
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_project_client(&url).await?;
            let mut p = resolve_project_target(&client, &target).await?;

            let next = if clear {
                None
            } else {
                // Start from --from (whole-registry replace), else
                // the current registry, else the default set — so
                // `--add` extends rather than orphaning canonical
                // states.
                let mut cfg: project::StatesConfig = match &from {
                    Some(src) => {
                        let raw = if src == "-" {
                            use std::io::Read as _;
                            let mut s = String::new();
                            std::io::stdin()
                                .read_to_string(&mut s)
                                .map_err(|e| eyre::eyre!("stdin: {e}"))?;
                            s
                        } else {
                            std::fs::read_to_string(src)
                                .map_err(|e| eyre::eyre!("read {src}: {e}"))?
                        };
                        // serde_yaml parses JSON too (YAML superset).
                        serde_yaml::from_str(&raw)
                            .map_err(|e| eyre::eyre!("parse states config: {e}"))?
                    }
                    None => p.states.clone().unwrap_or_else(project::default_states),
                };
                for spec in &add {
                    let (name, group, color) = parse_state_spec(spec)?;
                    let order = cfg.0.iter().map(|s| s.order + 1).max().unwrap_or_default();
                    match cfg
                        .0
                        .iter_mut()
                        .find(|s| s.name.eq_ignore_ascii_case(&name))
                    {
                        Some(existing) => {
                            existing.group = group;
                            if let Some(c) = color {
                                existing.color = c;
                            }
                        }
                        None => cfg.0.push(project::StateDef {
                            name,
                            group,
                            color: color.unwrap_or_default(),
                            default: false,
                            order,
                        }),
                    }
                }
                if let Some(name) = &default_state {
                    if !cfg.0.iter().any(|s| s.name.eq_ignore_ascii_case(name)) {
                        return Err(eyre::eyre!(
                            "--default `{name}` is not in the registry — add it first"
                        ));
                    }
                    for s in &mut cfg.0 {
                        s.default = s.name.eq_ignore_ascii_case(name);
                    }
                }
                if cfg.0.is_empty() {
                    return Err(eyre::eyre!(
                        "registry would be empty — use --clear to drop it instead"
                    ));
                }
                Some(cfg)
            };
            p.states = next;
            let updated = client
                .update(p)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            let registry = updated
                .states
                .clone()
                .unwrap_or_else(project::default_states);
            if json {
                crate::json_out::print_json(&serde_json::json!({
                    "project": updated.id,
                    "custom": updated.states.is_some(),
                    "states": registry,
                }))?;
                return Ok(());
            }
            if updated.states.is_none() {
                println!(
                    "{}: custom registry cleared (default applies)",
                    updated.title
                );
            } else {
                println!("{}: registry updated\n", updated.title);
                for s in registry.ordered() {
                    let default = if s.default { "  (default)" } else { "" };
                    println!("  {:<18} {:<10}{default}", s.name, s.group.as_str());
                }
            }
        }
        ProjectCmd::SetTarget {
            target,
            date,
            org,
            server,
            json,
        } => {
            let parsed = if matches!(
                date.trim().to_ascii_lowercase().as_str(),
                "none" | "clear" | "null" | ""
            ) {
                None
            } else {
                Some(
                    date.parse::<chrono::NaiveDate>()
                        .map_err(|e| eyre::eyre!("target date `{date}` (want YYYY-MM-DD): {e}"))?,
                )
            };
            mutate_project(target, org, server, json, |p| p.target_date = parsed).await?;
        }
        ProjectCmd::Progress {
            target,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let pc = connect_project_client(&url).await?;
            let proj = resolve_project_target(&pc, &target).await?;
            // Count tasks whose project_id == this project.
            let tc = connect_task_client(&url).await?;
            let tasks = tc
                .list()
                .await
                .map_err(|e| eyre::eyre!("list tasks: {e:?}"))?;
            let mine: Vec<&task::TaskInfo> = tasks
                .iter()
                .filter(|t| t.project_id == Some(proj.id))
                .collect();
            let total = mine.len();
            let done = mine
                .iter()
                .filter(|t| matches!(task::Status::from_str(&t.status), Some(task::Status::Done)))
                .count();
            let pct: i16 = if total == 0 {
                -1
            } else {
                i16::try_from((done * 100) / total).unwrap_or(100)
            };
            // Persist the rollup.
            let mut p = proj.clone();
            p.progress_percent = pct;
            pc.update(p)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            let shown = if pct < 0 {
                "—".to_string()
            } else {
                format!("{pct}%")
            };
            println!("{}  {}", proj.title, shown);
            println!("  {done}/{total} tasks done");
            if let Some(d) = proj.target_date {
                println!("  target: {d}");
            }
        }
        ProjectCmd::SetPriority {
            target,
            priority,
            org,
            server,
            json,
        } => {
            mutate_project(target, org, server, json, |p| p.priority = priority).await?;
        }
        ProjectCmd::SetParent {
            target,
            parent,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org.clone())?;
            let url = resolve_org_vox_url(server.clone(), &slug);
            let client = connect_project_client(&url).await?;
            let new_parent = if matches!(parent.as_str(), "none" | "null" | "") {
                None
            } else {
                Some(resolve_project_target(&client, &parent).await?.id)
            };
            mutate_project(target, org, server, json, |p| p.parent_id = new_parent).await?;
        }
        ProjectCmd::Archive {
            target,
            org,
            server,
            json,
        } => {
            mutate_project(target, org, server, json, |p| p.archived = true).await?;
        }
        ProjectCmd::Unarchive {
            target,
            org,
            server,
            json,
        } => {
            mutate_project(target, org, server, json, |p| p.archived = false).await?;
        }
        ProjectCmd::Rename {
            target,
            new_path,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_project_client(&url).await?;
            let p = resolve_project_target(&client, &target).await?;
            let renamed = client
                .rename(p.id, new_path.clone())
                .await
                .map_err(|e| eyre::eyre!("rename: {e:?}"))?;
            if json {
                crate::json_out::print_json(&renamed)?;
            } else {
                println!("renamed → {}", renamed.path);
            }
        }
        ProjectCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_project_client(&url).await?;
            let p = resolve_project_target(&client, &target).await?;
            if !yes && !confirm(&format!("delete `{}` ({})?", p.title, p.path))? {
                println!("aborted");
                return Ok(());
            }
            client
                .delete(p.id)
                .await
                .map_err(|e| eyre::eyre!("delete: {e:?}"))?;
            println!("deleted {}", p.path);
        }
    }
    Ok(())
}

pub(crate) async fn connect_project_client(url: &str) -> eyre::Result<project::ProjectServiceClient> {
    establish_for_url(url).await
}

/// Resolve a project reference — uuid, vault path, title, or a
/// unique prefix of either (shared flexible resolver).
pub(crate) async fn resolve_project_target(
    client: &project::ProjectServiceClient,
    target: &str,
) -> eyre::Result<project::ProjectInfo> {
    crate::json_out::resolve_project_flexible(client, target).await
}

async fn mutate_project<F>(
    target: String,
    org: Option<String>,
    server: Option<String>,
    json: bool,
    apply: F,
) -> eyre::Result<()>
where
    F: FnOnce(&mut project::ProjectInfo),
{
    let slug = resolve_active_org(org)?;
    let url = resolve_org_vox_url(server, &slug);
    let client = connect_project_client(&url).await?;
    let mut p = resolve_project_target(&client, &target).await?;
    apply(&mut p);
    let updated = client
        .update(p)
        .await
        .map_err(|e| eyre::eyre!("update: {e:?}"))?;
    if json {
        crate::json_out::print_json(&updated)?;
    } else {
        println!("{}  [{}]  {}", updated.title, updated.status, updated.path);
    }
    Ok(())
}

/// Parse a `--add` state spec: `<name>:<group>[:<color>]`.
fn parse_state_spec(spec: &str) -> eyre::Result<(String, project::StateGroup, Option<String>)> {
    let mut parts = spec.splitn(3, ':');
    let name = parts.next().unwrap_or_default().trim();
    let group_s = parts.next().unwrap_or_default().trim();
    let color = parts.next().map(|c| c.trim().to_string());
    if name.is_empty() || group_s.is_empty() {
        return Err(eyre::eyre!(
            "bad state spec `{spec}` — want <name>:<group>[:<color>]"
        ));
    }
    let group = project::StateGroup::from_str(group_s).ok_or_else(|| {
        eyre::eyre!(
            "unknown group `{group_s}` — one of backlog / unstarted / started / \
             completed / cancelled"
        )
    })?;
    Ok((name.to_string(), group, color))
}

fn print_project_row(p: &project::ProjectInfo, indent: usize) {
    let pad = " ".repeat(indent);
    let tags = if p.tags.0.is_empty() {
        String::new()
    } else {
        format!("  [{}]", p.tags.0.join(", "))
    };
    println!(
        "{pad}{:<28}  {:<10}  {:<8}{tags}",
        p.title, p.status, p.priority
    );
}
