//! `task timer …` — billable time tracking.
//!
//! Moved verbatim out of `main.rs`; behaviour unchanged.

use clap::Subcommand;

use crate::establish_for_url;
use crate::resolve_org_vox_url;

#[derive(Subcommand)]
pub(crate) enum TimerCmd {
    /// Start the timer for the configured user. Fails if a
    /// session is already open.
    Start {
        /// Free-text description. Quoted to allow spaces.
        /// Optional when `--task` is given (defaults to the
        /// task's title).
        #[arg(required_unless_present = "task")]
        description: Option<String>,
        /// Task to track against — full UUID, unique id
        /// prefix, or vault-relative path. Validates the
        /// task exists and fills description (title),
        /// project (the task's project), and task-note
        /// (the task's path); explicit flags still win.
        #[arg(long)]
        task: Option<String>,
        /// Project the session is logged against — uuid,
        /// title, vault path, or a unique prefix of either.
        /// Empty = uncategorized.
        #[arg(long)]
        project: Option<String>,
        /// Vault-relative path to the task note this
        /// session is for.
        #[arg(long, default_value = "")]
        task_note: String,
        /// Tag names to attach to the session. Tags are
        /// auto-created in the calling user's org if they
        /// don't already exist. Pass `--tag focus --tag review`
        /// to attach two.
        #[arg(long = "tag")]
        tags: Vec<String>,
        /// Emit the started session as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Stop the current session. Snapshots `rate_cents` +
    /// `currency` via the rate cascade and writes the closed
    /// row.
    Stop {
        /// Emit the closed session as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Show the active session, if any.
    Active {
        /// Emit the session as JSON (plus derived
        /// `seconds_elapsed` and joined task / project
        /// titles where resolvable). `null` when idle.
        #[arg(long)]
        json: bool,
    },
    /// Atomic stop-then-start. Same args as `start`.
    Switch {
        #[arg(required_unless_present = "task")]
        description: Option<String>,
        /// Task to track against (id / prefix / path) —
        /// same semantics as `start --task`.
        #[arg(long)]
        task: Option<String>,
        /// Project — uuid, title, path, or unique prefix.
        #[arg(long)]
        project: Option<String>,
        #[arg(long, default_value = "")]
        task_note: String,
        #[arg(long = "tag")]
        tags: Vec<String>,
        /// Emit `{stopped, started}` sessions as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Retro-log a past session: `--from` / `--to` ISO 8601
    /// timestamps + description. Skips the active-timer
    /// invariant.
    Log {
        #[arg(required_unless_present = "task")]
        description: Option<String>,
        #[arg(long)]
        from: chrono::DateTime<chrono::Utc>,
        #[arg(long)]
        to: chrono::DateTime<chrono::Utc>,
        /// Task to log against (id / prefix / path) — same
        /// semantics as `start --task`.
        #[arg(long)]
        task: Option<String>,
        /// Project — uuid, title, path, or unique prefix.
        #[arg(long)]
        project: Option<String>,
        #[arg(long, default_value = "")]
        task_note: String,
        /// `true` / `false` to override the project default.
        /// Omit to inherit.
        #[arg(long)]
        billable: Option<bool>,
        #[arg(long = "tag")]
        tags: Vec<String>,
        /// Emit the logged session as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Set an org-level member hourly rate (cascade level 3) for a
    /// user. New sessions logged for that user snapshot this rate at
    /// close. Upserts. Use `--org` to target the org's timer DB.
    SetRate {
        /// The member's user id (uuid).
        #[arg(long)]
        user_id: uuid::Uuid,
        /// Hourly rate in cents (e.g. 3000 = $30/hr).
        #[arg(long)]
        cents: i64,
        #[arg(long, default_value = "USD")]
        currency: String,
        /// Emit the stored rate as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Edit an existing session. Only the flags you pass change; the
    /// billable rate is re-snapshotted from the cascade afterward
    /// (so reassigning `--user-id` or `--project` re-rates it).
    Edit {
        /// Session id (uuid).
        id: uuid::Uuid,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        from: Option<chrono::DateTime<chrono::Utc>>,
        #[arg(long)]
        to: Option<chrono::DateTime<chrono::Utc>>,
        /// Reassign to a project — uuid, title, path, or a
        /// unique prefix of either.
        #[arg(long)]
        project: Option<String>,
        /// Reassign to a different member.
        #[arg(long)]
        user_id: Option<uuid::Uuid>,
        #[arg(long)]
        billable: Option<bool>,
        #[arg(long)]
        task_note: Option<String>,
        /// Emit the updated session as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Delete a session by id. Permanent.
    Delete {
        /// Session id (uuid).
        id: uuid::Uuid,
        /// Emit `{"deleted": <id>}` as JSON.
        #[arg(long)]
        json: bool,
    },
    /// List sessions. Defaults to the last 7 days, all
    /// users (matching the `finance project` rollup —
    /// the per-org DB is already the scope).
    List {
        /// Only sessions on this project — uuid, title,
        /// path, or a unique prefix of either.
        #[arg(long)]
        project: Option<String>,
        /// Only sessions logged by this user id. Omit for
        /// all users in the org.
        #[arg(long)]
        user: Option<uuid::Uuid>,
        /// Inclusive since-date. Defaults to 7 days ago.
        #[arg(long)]
        since: Option<chrono::DateTime<chrono::Utc>>,
        /// Exclusive until-date. Defaults to "now".
        #[arg(long)]
        until: Option<chrono::DateTime<chrono::Utc>>,
        /// Filter open / closed sessions; omit for both.
        #[arg(long)]
        open: Option<bool>,
        /// Filter billable / non-billable; omit for both.
        #[arg(long)]
        billable: Option<bool>,
        /// Emit the sessions as a JSON array.
        #[arg(long)]
        json: bool,
    },
    /// Resolve the rate cascade for the configured user +
    /// project. Useful to preview "what will this session
    /// bill at" before stopping.
    Resolve {
        /// Project — uuid, title, path, or unique prefix.
        #[arg(long)]
        project: Option<String>,
        /// Emit the resolution as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Audit which user_ids appear on sessions, with name
    /// resolution from the org's `auth.sqlite`. Useful for
    /// spotting detached / mis-attributed ids before
    /// invoicing.
    Users {
        /// Emit the per-user aggregates as a JSON array.
        #[arg(long)]
        json: bool,
    },
    /// Bulk-swap every matching session's `user_id`.
    /// Optional filters narrow the swap to a project /
    /// date window — without them, ALL sessions for `from`
    /// in the org are moved.
    ReassignUser {
        /// Source user_id (current owner of the sessions).
        #[arg(long)]
        from: uuid::Uuid,
        /// Destination user_id (new owner).
        #[arg(long)]
        to: uuid::Uuid,
        /// Limit to one project — uuid, title, path, or a
        /// unique prefix of either.
        #[arg(long)]
        project: Option<String>,
        /// Inclusive lower bound on `start_time`.
        #[arg(long)]
        since: Option<chrono::DateTime<chrono::Utc>>,
        /// Exclusive upper bound on `start_time`.
        #[arg(long)]
        until: Option<chrono::DateTime<chrono::Utc>>,
        /// Limit to sessions whose description matches this
        /// substring (case-insensitive). Useful for
        /// untangling "video editing" vs "PNG tracking"
        /// rows that share a user_id.
        #[arg(long)]
        description_contains: Option<String>,
        /// Re-snapshot `rate_cents` + `currency` from the
        /// rate cascade for the *new* user. Off by default
        /// so already-billed amounts don't shift; pass when
        /// you're correcting a fresh mistake.
        #[arg(long, default_value_t = false)]
        rerate: bool,
        /// Show what would change without writing.
        #[arg(long, default_value_t = false)]
        dry_run: bool,
        /// Emit the match/update summary as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Tag CRUD + attach to existing sessions.
    #[command(subcommand)]
    Tag(TimerTagCmd),
}

#[derive(Subcommand)]
pub(crate) enum TimerTagCmd {
    /// List tags in the calling user's org.
    List {
        /// Emit the tags as a JSON array.
        #[arg(long)]
        json: bool,
    },
    /// Create a tag. Idempotent — no-op if a tag with that
    /// name already exists.
    Create {
        name: String,
        /// Hex `#RRGGBB` (UI hint). Empty = auto-pick.
        #[arg(long, default_value = "")]
        color: String,
        /// Emit the tag as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Delete a tag by name. Removes the join rows on every
    /// session via FK cascade.
    Rm {
        name: String,
        /// Emit the deleted tag as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Attach tags to an existing session.
    Attach {
        session_id: uuid::Uuid,
        #[arg(long = "tag", required = true)]
        tags: Vec<String>,
        /// Emit `{session_id, attached}` as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Detach tags from a session. `--tag <name>` removes
    /// that tag; `--all` removes every tag.
    Detach {
        session_id: uuid::Uuid,
        #[arg(long = "tag")]
        tags: Vec<String>,
        #[arg(long)]
        all: bool,
        /// Emit `{session_id, detached}` as JSON.
        #[arg(long)]
        json: bool,
    },
}

/// Deterministic local-owner user id for an org. MUST stay identical to
/// the web UI's `task_ui::chrome::owner_id` (`v5(org_id,
/// "task-local-owner")`) so the CLI and the `/timer` page resolve the
/// same user and therefore see the same sessions.
pub(crate) fn timer_owner_id(org_id: uuid::Uuid) -> uuid::Uuid {
    uuid::Uuid::new_v5(&org_id, b"task-local-owner")
}

pub(crate) async fn run_timer(cmd: TimerCmd, org_override: Option<&str>) -> eyre::Result<()> {
    use sea_orm::Database;
    use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
    use sea_orm_migration::MigratorTrait;
    use std::sync::Arc;
    use timer::entity::{TagColumn, TagEntity, WorkSessionTagColumn, WorkSessionTagEntity};
    use timer::store::{Store, VaultProjectDefaults};
    use timer_proto::service::{LogSessionRequest, StartTimerRequest, TimerService};

    // OrgRoot-driven path resolution. `TASK_TIMER_DB` /
    // `TASK_VAULT_ROOT` still win as hard overrides for
    // test fixtures. User/org ids come from the active
    // server entry in `session.json`; falls back to env vars
    // and finally dev nil-uuids for fresh setups.
    let ctx = crate::org_ctx::resolve_active(org_override)?;
    let db_url = std::env::var("TASK_TIMER_DB")
        .unwrap_or_else(|_| format!("sqlite://{}?mode=rwc", ctx.root.timer_db().display()));
    let vault_root = std::env::var("TASK_VAULT_ROOT")
        .map_or_else(|_| ctx.root.vault_dir(), std::path::PathBuf::from);
    // Unified identity. The org id is the org's *manifest* id (the
    // same value the web UI gets from `.well-known` → `OrgMeta.id`),
    // and the default user is the deterministic "local owner" derived
    // from it — matching `task_ui::chrome::owner_id`. This is what makes
    // CLI- and UI-logged sessions land in the same `(org_id, user_id)`
    // keyspace so both surfaces see the same data. `TASK_ORG_ID` /
    // `TASK_USER_ID` still override (e.g. logging a contractor's time
    // under a distinct user id).
    let org_id = std::env::var("TASK_ORG_ID")
        .ok()
        .and_then(|s| s.parse::<uuid::Uuid>().ok())
        .or_else(|| ctx.root.manifest().ok().map(|m| m.id))
        .unwrap_or_else(|| uuid::Uuid::parse_str("00000000-0000-0000-0000-00000000000a").unwrap());
    let user_id = std::env::var("TASK_USER_ID")
        .ok()
        .and_then(|s| s.parse::<uuid::Uuid>().ok())
        .unwrap_or_else(|| timer_owner_id(org_id));

    let conn = Database::connect(&db_url)
        .await
        .map_err(|e| eyre::eyre!("connect timer db `{db_url}`: {e}"))?;
    timer::Migrator::up(&conn, None)
        .await
        .map_err(|e| eyre::eyre!("timer migrations: {e}"))?;
    let defaults = Arc::new(VaultProjectDefaults {
        vault_root: vault_root.clone(),
    });
    let store = Store::new(conn, defaults);

    // Per-org vox URL for `--task` / `--project` reference
    // resolution. `establish_for_url` honors `TASK_EMBED` (in-process
    // backend) vs a running server; the URL is only dialed when a
    // flag actually needs resolving, so plain local timer use (raw
    // uuids / no flags) keeps working fully offline.
    let vox_url = resolve_org_vox_url(None, ctx.root.slug());
    // `--task <id|prefix|path>` → TaskInfo, used to default the
    // description / project / task-note on start | switch | log.
    let resolve_task_flag = |flag: Option<String>| {
        let vox_url = vox_url.clone();
        async move {
            match flag {
                None => Ok::<_, eyre::Report>(None),
                Some(t) => {
                    let tc: task::TaskServiceClient = establish_for_url(&vox_url).await?;
                    Ok(Some(crate::json_out::resolve_task_flexible(&tc, &t).await?))
                }
            }
        }
    };
    // `--project <uuid|title|path|prefix>` → (id, known-path).
    let resolve_project_flag = |flag: Option<String>| {
        let vox_url = vox_url.clone();
        async move {
            crate::json_out::resolve_project_arg(flag.as_deref(), || async {
                establish_for_url::<project::ProjectServiceClient>(&vox_url).await
            })
            .await
        }
    };

    match cmd {
        TimerCmd::Start {
            description,
            task,
            project,
            task_note,
            tags,
            json,
        } => {
            let task_info = resolve_task_flag(task).await?;
            let (mut project_id, resolved_path) = resolve_project_flag(project).await?;
            // --task fills the gaps; explicit flags win.
            if project_id.is_none() {
                project_id = task_info.as_ref().and_then(|t| t.project_id);
            }
            let description = description
                .or_else(|| task_info.as_ref().map(|t| t.title.clone()))
                .unwrap_or_default();
            let task_note = if task_note.is_empty() {
                task_info
                    .as_ref()
                    .map(|t| t.path.clone())
                    .unwrap_or_default()
            } else {
                task_note
            };
            let project_path =
                resolved_path.unwrap_or_else(|| project_path_for(&vault_root, project_id));
            let session = store
                .start_timer(StartTimerRequest {
                    user_id,
                    org_id,
                    project_id,
                    project_path,
                    task_note_path: task_note,
                    description,
                })
                .await
                .map_err(|e| eyre::eyre!("start: {e}"))?;
            attach_tags_by_name(store.conn(), org_id, session.id, &tags).await?;
            if json {
                crate::json_out::print_json(&crate::json_out::session_json(&session))?;
            } else {
                println!("Started {} at {}", session.id, session.start_time);
                println!("  description: {}", session.description);
                if !session.project_path.is_empty() {
                    println!("  project:     {}", session.project_path);
                }
                if !session.task_note_path.is_empty() {
                    println!("  task:        {}", session.task_note_path);
                }
                println!("  billable:    {}", session.billable);
                if !tags.is_empty() {
                    println!("  tags:        {}", tags.join(", "));
                }
            }
        }
        TimerCmd::Stop { json } => {
            let session = store
                .stop_timer(user_id)
                .await
                .map_err(|e| eyre::eyre!("stop: {e}"))?;
            if json {
                crate::json_out::print_json(&crate::json_out::session_json(&session))?;
            } else {
                let elapsed = session
                    .end_time
                    .unwrap_or_else(chrono::Utc::now)
                    .signed_duration_since(session.start_time);
                println!("Stopped {}", session.id);
                println!("  description: {}", session.description);
                println!("  elapsed:     {}", fmt_duration(elapsed));
                if session.billable {
                    println!(
                        "  billed:      {} {} (rate: {} {}/h)",
                        fmt_money(billed_cents(&session, elapsed)),
                        session.currency,
                        fmt_money(session.rate_cents),
                        session.currency,
                    );
                }
            }
        }
        TimerCmd::Active { json } => {
            match store
                .active_timer(user_id)
                .await
                .map_err(|e| eyre::eyre!("{e}"))?
            {
                Some(s) => {
                    if json {
                        // Joined titles are best-effort: vox being
                        // down shouldn't break `active --json` —
                        // the entity + derived seconds still print.
                        let task_title = if s.task_note_path.is_empty() {
                            None
                        } else {
                            match establish_for_url::<task::TaskServiceClient>(&vox_url).await {
                                Ok(tc) => tc
                                    .get_by_path(s.task_note_path.clone())
                                    .await
                                    .ok()
                                    .map(|t| t.title),
                                Err(_) => None,
                            }
                        };
                        let project_title = match s.project_id {
                            None => None,
                            Some(pid) => {
                                match establish_for_url::<project::ProjectServiceClient>(&vox_url)
                                    .await
                                {
                                    Ok(pc) => pc.get(pid).await.ok().map(|p| p.title),
                                    Err(_) => None,
                                }
                            }
                        };
                        crate::json_out::print_json(&crate::json_out::session_json_joined(
                            &s,
                            task_title,
                            project_title,
                        ))?;
                    } else {
                        let elapsed = chrono::Utc::now().signed_duration_since(s.start_time);
                        println!("Running for {} ({})", fmt_duration(elapsed), s.id);
                        if !s.description.is_empty() {
                            println!("  description: {}", s.description);
                        }
                        if !s.project_path.is_empty() {
                            println!("  project:     {}", s.project_path);
                        }
                        if !s.task_note_path.is_empty() {
                            println!("  task:        {}", s.task_note_path);
                        }
                    }
                }
                None => {
                    if json {
                        println!("null");
                    } else {
                        println!("No active timer.");
                    }
                }
            }
        }
        TimerCmd::Switch {
            description,
            task,
            project,
            task_note,
            tags,
            json,
        } => {
            let task_info = resolve_task_flag(task).await?;
            let (mut project_id, resolved_path) = resolve_project_flag(project).await?;
            if project_id.is_none() {
                project_id = task_info.as_ref().and_then(|t| t.project_id);
            }
            let description = description
                .or_else(|| task_info.as_ref().map(|t| t.title.clone()))
                .unwrap_or_default();
            let task_note = if task_note.is_empty() {
                task_info
                    .as_ref()
                    .map(|t| t.path.clone())
                    .unwrap_or_default()
            } else {
                task_note
            };
            let project_path =
                resolved_path.unwrap_or_else(|| project_path_for(&vault_root, project_id));
            let (closed, started) = store
                .switch_timer(StartTimerRequest {
                    user_id,
                    org_id,
                    project_id,
                    project_path,
                    task_note_path: task_note,
                    description,
                })
                .await
                .map_err(|e| eyre::eyre!("switch: {e}"))?;
            attach_tags_by_name(store.conn(), org_id, started.id, &tags).await?;
            if json {
                crate::json_out::print_json(&serde_json::json!({
                    "stopped": closed.as_ref().map(crate::json_out::session_json),
                    "started": crate::json_out::session_json(&started),
                }))?;
            } else {
                if let Some(prev) = closed {
                    let elapsed = prev
                        .end_time
                        .unwrap_or_else(chrono::Utc::now)
                        .signed_duration_since(prev.start_time);
                    println!("Stopped {} after {}", prev.id, fmt_duration(elapsed));
                }
                println!("Started {} at {}", started.id, started.start_time);
                if !tags.is_empty() {
                    println!("  tags: {}", tags.join(", "));
                }
            }
        }
        TimerCmd::Log {
            description,
            from,
            to,
            task,
            project,
            task_note,
            billable,
            tags,
            json,
        } => {
            let task_info = resolve_task_flag(task).await?;
            let (mut project_id, resolved_path) = resolve_project_flag(project).await?;
            if project_id.is_none() {
                project_id = task_info.as_ref().and_then(|t| t.project_id);
            }
            let description = description
                .or_else(|| task_info.as_ref().map(|t| t.title.clone()))
                .unwrap_or_default();
            let task_note = if task_note.is_empty() {
                task_info
                    .as_ref()
                    .map(|t| t.path.clone())
                    .unwrap_or_default()
            } else {
                task_note
            };
            let project_path =
                resolved_path.unwrap_or_else(|| project_path_for(&vault_root, project_id));
            let session = store
                .log_session(LogSessionRequest {
                    user_id,
                    org_id,
                    project_id,
                    project_path,
                    task_note_path: task_note,
                    description,
                    start_time: from,
                    end_time: to,
                    billable_override: billable,
                })
                .await
                .map_err(|e| eyre::eyre!("log: {e}"))?;
            attach_tags_by_name(store.conn(), org_id, session.id, &tags).await?;
            if json {
                crate::json_out::print_json(&crate::json_out::session_json(&session))?;
            } else {
                println!("Logged {} ({})", session.id, fmt_duration(to - from));
            }
        }
        TimerCmd::SetRate {
            user_id,
            cents,
            currency,
            json,
        } => {
            store
                .set_org_member_rate(org_id, user_id, cents, &currency)
                .await
                .map_err(|e| eyre::eyre!("set rate: {e}"))?;
            if json {
                crate::json_out::print_json(&serde_json::json!({
                    "org_id": org_id,
                    "user_id": user_id,
                    "hourly_cents": cents,
                    "currency": currency,
                }))?;
            } else {
                println!(
                    "Set org rate for {user_id}: {} {currency}/hr",
                    fmt_money(cents)
                );
            }
        }
        TimerCmd::Edit {
            id,
            description,
            from,
            to,
            project,
            user_id: edit_user,
            billable,
            task_note,
            json,
        } => {
            let (project_id, resolved_path) = resolve_project_flag(project).await?;
            // Reassigning the project also refreshes the cached
            // path (resolver-known path first, vault scan second).
            let project_path = project_id.map(|pid| {
                resolved_path.unwrap_or_else(|| project_path_for(&vault_root, Some(pid)))
            });
            let session = store
                .update_session(timer_proto::service::UpdateSessionRequest {
                    id,
                    user_id: edit_user,
                    project_id,
                    project_path,
                    task_note_path: task_note,
                    description,
                    start_time: from,
                    end_time: to,
                    billable,
                })
                .await
                .map_err(|e| eyre::eyre!("edit: {e}"))?;
            if json {
                crate::json_out::print_json(&crate::json_out::session_json(&session))?;
            } else {
                println!(
                    "Updated {} — \"{}\" [{}] {}/hr",
                    session.id,
                    session.description,
                    if session.billable {
                        "billable"
                    } else {
                        "non-billable"
                    },
                    fmt_money(session.rate_cents),
                );
            }
        }
        TimerCmd::Delete { id, json } => {
            store
                .delete_session(id)
                .await
                .map_err(|e| eyre::eyre!("delete: {e}"))?;
            if json {
                crate::json_out::print_json(&serde_json::json!({ "deleted": id }))?;
            } else {
                println!("Deleted {id}");
            }
        }
        TimerCmd::List {
            project,
            user,
            since,
            until,
            open,
            billable,
            json,
        } => {
            let (project_id, _) = resolve_project_flag(project).await?;
            // No default user filter: sessions land in this
            // DB from several surfaces (CLI, web UI) whose
            // identity derivations have drifted, and a
            // silent owner filter made `list` undercount vs
            // the finance rollup (which has always been
            // org-wide).
            let filter = timer_proto::WorkSessionFilter {
                user_id: user,
                project_id,
                since: Some(
                    since.unwrap_or_else(|| chrono::Utc::now() - chrono::Duration::days(7)),
                ),
                until,
                billable,
                open,
            };
            let rows = store
                .query_sessions(&filter)
                .await
                .map_err(|e| eyre::eyre!("list: {e}"))?;
            if json {
                let out: Vec<serde_json::Value> =
                    rows.iter().map(crate::json_out::session_json).collect();
                crate::json_out::print_json(&out)?;
                return Ok(());
            }
            if rows.is_empty() {
                println!("(no sessions)");
            }
            for s in rows {
                let end = s
                    .end_time
                    .map_or_else(|| "open".to_string(), |t| t.to_rfc3339());
                let elapsed = s
                    .end_time
                    .unwrap_or_else(chrono::Utc::now)
                    .signed_duration_since(s.start_time);
                println!(
                    "{}  {:>8}  {}  {} {}",
                    s.start_time.format("%Y-%m-%d %H:%M"),
                    fmt_duration(elapsed),
                    if s.billable { "billable" } else { "        " },
                    s.description,
                    if end == "open" {
                        "[OPEN]".to_string()
                    } else {
                        String::new()
                    },
                );
            }
        }
        TimerCmd::Users { json } => {
            // All sessions in scope; aggregate per user_id.
            let rows = store
                .query_sessions(&timer_proto::WorkSessionFilter::default())
                .await
                .map_err(|e| eyre::eyre!("list: {e}"))?;
            let mut agg: std::collections::BTreeMap<uuid::Uuid, (usize, i64, i64)> =
                std::collections::BTreeMap::new();
            for s in &rows {
                let e = agg.entry(s.user_id).or_default();
                e.0 += 1;
                let secs = s
                    .end_time
                    .unwrap_or(s.start_time)
                    .signed_duration_since(s.start_time)
                    .num_seconds()
                    .max(0);
                e.1 += secs;
                e.2 +=
                    i64::try_from(i128::from(secs) * i128::from(s.rate_cents) / 3600).unwrap_or(0);
            }
            // Resolve names from auth.sqlite — same lookup
            // the invoice path uses.
            let names = {
                use architect_auth::db::{AuthUserColumn, AuthUserEntity};
                use sea_orm::{ColumnTrait, Database, EntityTrait, QueryFilter};
                let mut map: std::collections::HashMap<uuid::Uuid, String> =
                    std::collections::HashMap::new();
                let auth_path = ctx.root.auth_db();
                if auth_path.exists() {
                    let url = format!("sqlite://{}?mode=ro", auth_path.display());
                    if let Ok(db) = Database::connect(&url).await {
                        let ids: Vec<uuid::Uuid> = agg.keys().copied().collect();
                        if let Ok(users) = AuthUserEntity::find()
                            .filter(AuthUserColumn::Id.is_in(ids))
                            .all(&db)
                            .await
                        {
                            for u in users {
                                let lbl = u
                                    .name
                                    .filter(|s| !s.is_empty())
                                    .or(u.email)
                                    .unwrap_or_default();
                                map.insert(u.id, lbl);
                            }
                        }
                    }
                }
                map
            };
            if json {
                let out: Vec<serde_json::Value> = agg
                    .iter()
                    .map(|(uid, (count, secs, cents))| {
                        serde_json::json!({
                            "user_id": uid,
                            "sessions": count,
                            "seconds": secs,
                            "cents": cents,
                            "name": names.get(uid),
                        })
                    })
                    .collect();
                crate::json_out::print_json(&out)?;
                return Ok(());
            }
            if agg.is_empty() {
                println!("(no sessions)");
            }
            println!(
                "{:<38}  {:>6}  {:>9}  {:>10}  name",
                "user_id", "count", "hours", "cents"
            );
            for (uid, (count, secs, cents)) in agg {
                let hours = secs as f64 / 3600.0;
                let name = names
                    .get(&uid)
                    .cloned()
                    .unwrap_or_else(|| "(not in auth_users)".into());
                println!("{uid:<38}  {count:>6}  {hours:>9.2}  {cents:>10}  {name}");
            }
        }
        TimerCmd::ReassignUser {
            from,
            to,
            project,
            since,
            until,
            description_contains,
            rerate,
            dry_run,
            json,
        } => {
            let (project_id, _) = resolve_project_flag(project).await?;
            let filter = timer_proto::WorkSessionFilter {
                user_id: Some(from),
                project_id,
                since,
                until,
                billable: None,
                open: None,
            };
            let rows = store
                .query_sessions(&filter)
                .await
                .map_err(|e| eyre::eyre!("list: {e}"))?;
            let needle = description_contains.map(|s| s.to_lowercase());
            let matched: Vec<_> = rows
                .into_iter()
                .filter(|s| {
                    needle
                        .as_ref()
                        .is_none_or(|n| s.description.to_lowercase().contains(n.as_str()))
                })
                .collect();
            if !json {
                println!(
                    "{} session(s) match (from={from}, to={to}, rerate={rerate}, dry_run={dry_run})",
                    matched.len()
                );
                for s in &matched {
                    println!(
                        "  {}  {}  {}",
                        s.start_time.format("%Y-%m-%d %H:%M"),
                        s.id,
                        s.description
                    );
                }
            }
            if dry_run || matched.is_empty() {
                if json {
                    crate::json_out::print_json(&serde_json::json!({
                        "from": from,
                        "to": to,
                        "rerate": rerate,
                        "dry_run": dry_run,
                        "matched": matched.len(),
                        "updated": 0,
                        "session_ids": matched.iter().map(|s| s.id).collect::<Vec<_>>(),
                    }))?;
                }
                return Ok(());
            }
            let mut updated = 0_usize;
            for s in &matched {
                if rerate {
                    // Goes through `update_session`, which
                    // re-snapshots `rate_cents` + `currency`
                    // from the cascade for the new user.
                    store
                        .update_session(timer_proto::service::UpdateSessionRequest {
                            id: s.id,
                            user_id: Some(to),
                            ..Default::default()
                        })
                        .await
                        .map_err(|e| eyre::eyre!("reassign {}: {e}", s.id))?;
                } else {
                    // Preserve the historical rate snapshot
                    // — only swap user_id. Direct SeaORM
                    // update bypasses cascade re-resolution.
                    use sea_orm::{ActiveModelTrait, EntityTrait, Set};
                    use timer::entity::{WorkSessionActive, WorkSessionEntity};
                    let row = WorkSessionEntity::find_by_id(s.id)
                        .one(store.conn())
                        .await?
                        .ok_or_else(|| eyre::eyre!("session {} disappeared", s.id))?;
                    let mut active: WorkSessionActive = row.into();
                    active.user_id = Set(to);
                    active.updated_at = Set(chrono::Utc::now());
                    active.update(store.conn()).await?;
                }
                updated += 1;
            }
            if json {
                crate::json_out::print_json(&serde_json::json!({
                    "from": from,
                    "to": to,
                    "rerate": rerate,
                    "dry_run": false,
                    "matched": matched.len(),
                    "updated": updated,
                    "session_ids": matched.iter().map(|s| s.id).collect::<Vec<_>>(),
                }))?;
            } else {
                println!("Updated {updated} session(s).");
            }
        }
        TimerCmd::Resolve { project, json } => {
            let (project_id, _) = resolve_project_flag(project).await?;
            let resolved = store
                .resolve_rate(user_id, project_id)
                .await
                .map_err(|e| eyre::eyre!("resolve: {e}"))?;
            if json {
                crate::json_out::print_json(&resolved)?;
            } else {
                println!(
                    "rate: {} {}/h  source: {:?}",
                    fmt_money(resolved.hourly_cents),
                    if resolved.currency.is_empty() {
                        "(none)".to_string()
                    } else {
                        resolved.currency
                    },
                    resolved.source,
                );
            }
        }
        TimerCmd::Tag(sub) => match sub {
            TimerTagCmd::List { json } => {
                let rows = TagEntity::find()
                    .filter(TagColumn::OrgId.eq(org_id))
                    .all(store.conn())
                    .await
                    .map_err(|e| eyre::eyre!("list tags: {e}"))?;
                if json {
                    let out: Vec<serde_json::Value> = rows
                        .into_iter()
                        .map(|t| crate::json_out::tag_json(&timer_proto::Tag::from(t)))
                        .collect();
                    crate::json_out::print_json(&out)?;
                    return Ok(());
                }
                if rows.is_empty() {
                    println!("(no tags)");
                }
                for t in rows {
                    let color = if t.color.is_empty() {
                        "(auto)"
                    } else {
                        t.color.as_str()
                    };
                    println!("{}  {}  {}", t.id, t.name, color);
                }
            }
            TimerTagCmd::Create { name, color, json } => {
                let tag = ensure_tag(store.conn(), org_id, &name, &color).await?;
                if json {
                    crate::json_out::print_json(&crate::json_out::tag_json(
                        &timer_proto::Tag::from(tag),
                    ))?;
                } else {
                    println!("{}  {}", tag.id, tag.name);
                }
            }
            TimerTagCmd::Rm { name, json } => {
                let existing = TagEntity::find()
                    .filter(TagColumn::OrgId.eq(org_id))
                    .filter(TagColumn::Name.eq(name.clone()))
                    .one(store.conn())
                    .await
                    .map_err(|e| eyre::eyre!("find tag: {e}"))?;
                let Some(tag) = existing else {
                    return Err(eyre::eyre!("no such tag: {name}"));
                };
                TagEntity::delete_by_id(tag.id)
                    .exec(store.conn())
                    .await
                    .map_err(|e| eyre::eyre!("delete tag: {e}"))?;
                if json {
                    crate::json_out::print_json(&serde_json::json!({
                        "deleted": crate::json_out::tag_json(&timer_proto::Tag::from(tag)),
                    }))?;
                } else {
                    println!("Deleted tag {} ({})", tag.name, tag.id);
                }
            }
            TimerTagCmd::Attach {
                session_id,
                tags,
                json,
            } => {
                attach_tags_by_name(store.conn(), org_id, session_id, &tags).await?;
                if json {
                    crate::json_out::print_json(&serde_json::json!({
                        "session_id": session_id,
                        "attached": tags,
                    }))?;
                } else {
                    println!("Attached {} to {session_id}", tags.join(", "));
                }
            }
            TimerTagCmd::Detach {
                session_id,
                tags,
                all,
                json,
            } => {
                if all {
                    WorkSessionTagEntity::delete_many()
                        .filter(WorkSessionTagColumn::WorkSessionId.eq(session_id))
                        .exec(store.conn())
                        .await
                        .map_err(|e| eyre::eyre!("detach all: {e}"))?;
                    if json {
                        crate::json_out::print_json(&serde_json::json!({
                            "session_id": session_id,
                            "detached": "all",
                        }))?;
                    } else {
                        println!("Detached all tags from {session_id}");
                    }
                } else if tags.is_empty() {
                    return Err(eyre::eyre!("pass --tag <name> or --all"));
                } else {
                    let tag_rows = TagEntity::find()
                        .filter(TagColumn::OrgId.eq(org_id))
                        .filter(TagColumn::Name.is_in(tags.clone()))
                        .all(store.conn())
                        .await
                        .map_err(|e| eyre::eyre!("lookup tags: {e}"))?;
                    let ids: Vec<uuid::Uuid> = tag_rows.iter().map(|t| t.id).collect();
                    if ids.is_empty() {
                        return Err(eyre::eyre!("no matching tags"));
                    }
                    WorkSessionTagEntity::delete_many()
                        .filter(WorkSessionTagColumn::WorkSessionId.eq(session_id))
                        .filter(WorkSessionTagColumn::TagId.is_in(ids))
                        .exec(store.conn())
                        .await
                        .map_err(|e| eyre::eyre!("detach: {e}"))?;
                    if json {
                        crate::json_out::print_json(&serde_json::json!({
                            "session_id": session_id,
                            "detached": tags,
                        }))?;
                    } else {
                        println!("Detached {} from {session_id}", tags.join(", "));
                    }
                }
            }
        },
    }
    Ok(())
}

/// Idempotent tag upsert by `(org_id, name)`. Returns the
/// existing or freshly inserted row.
async fn ensure_tag(
    conn: &sea_orm::DatabaseConnection,
    org_id: uuid::Uuid,
    name: &str,
    color: &str,
) -> eyre::Result<timer::entity::TagModel> {
    use sea_orm::{ActiveModelTrait, ColumnTrait, EntityTrait, QueryFilter, Set};
    if let Some(existing) = timer::entity::TagEntity::find()
        .filter(timer::entity::TagColumn::OrgId.eq(org_id))
        .filter(timer::entity::TagColumn::Name.eq(name.to_string()))
        .one(conn)
        .await
        .map_err(|e| eyre::eyre!("find tag: {e}"))?
    {
        return Ok(existing);
    }
    let now = chrono::Utc::now();
    let am = timer::entity::TagActive {
        id: Set(uuid::Uuid::new_v4()),
        org_id: Set(org_id),
        name: Set(name.to_string()),
        color: Set(color.to_string()),
        created_at: Set(now),
        updated_at: Set(now),
    };
    am.insert(conn)
        .await
        .map_err(|e| eyre::eyre!("insert tag: {e}"))
}

/// Ensure each tag in `names` exists in `org_id` and attach
/// it to `session_id`. Already-attached pairs are skipped
/// (uniqueness index guards the join).
async fn attach_tags_by_name(
    conn: &sea_orm::DatabaseConnection,
    org_id: uuid::Uuid,
    session_id: uuid::Uuid,
    names: &[String],
) -> eyre::Result<()> {
    use sea_orm::{ActiveModelTrait, ColumnTrait, EntityTrait, QueryFilter, Set};
    for name in names {
        let tag = ensure_tag(conn, org_id, name, "").await?;
        let already = timer::entity::WorkSessionTagEntity::find()
            .filter(timer::entity::WorkSessionTagColumn::WorkSessionId.eq(session_id))
            .filter(timer::entity::WorkSessionTagColumn::TagId.eq(tag.id))
            .one(conn)
            .await
            .map_err(|e| eyre::eyre!("check join: {e}"))?;
        if already.is_some() {
            continue;
        }
        let am = timer::entity::WorkSessionTagActive {
            id: Set(uuid::Uuid::new_v4()),
            work_session_id: Set(session_id),
            tag_id: Set(tag.id),
            created_at: Set(chrono::Utc::now()),
        };
        am.insert(conn)
            .await
            .map_err(|e| eyre::eyre!("insert join: {e}"))?;
    }
    Ok(())
}

/// Resolve the project markdown path from its frontmatter
/// id by scanning `Projects/**/*.md` recursively (projects
/// conventionally live in their own folder, e.g.
/// `Projects/<Name>/<Name>.md` — a flat scan misses them and
/// every session then stores an empty `project_path`).
/// `None` project_id → empty.
pub(crate) fn project_path_for(
    vault_root: &std::path::Path,
    project_id: Option<uuid::Uuid>,
) -> String {
    let Some(pid) = project_id else {
        return String::new();
    };
    let mut dirs = vec![vault_root.join("Projects")];
    while let Some(dir) = dirs.pop() {
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                dirs.push(path);
                continue;
            }
            if path.extension().and_then(|s| s.to_str()) != Some("md") {
                continue;
            }
            let Ok(raw) = std::fs::read_to_string(&path) else {
                continue;
            };
            let rel = path
                .strip_prefix(vault_root)
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_default();
            let basename = path.file_stem().and_then(|s| s.to_str()).unwrap_or("");
            let Ok(p) = project::parse_str(&rel, basename, &raw) else {
                continue;
            };
            if p.id == pid {
                return rel;
            }
        }
    }
    String::new()
}

fn fmt_duration(d: chrono::Duration) -> String {
    let secs = d.num_seconds().max(0);
    let h = secs / 3600;
    let m = (secs % 3600) / 60;
    let s = secs % 60;
    if h > 0 {
        format!("{h}h{m:02}m{s:02}s")
    } else if m > 0 {
        format!("{m}m{s:02}s")
    } else {
        format!("{s}s")
    }
}

fn fmt_money(cents: i64) -> String {
    let neg = cents < 0;
    let abs = cents.unsigned_abs();
    let dollars = abs / 100;
    let frac = abs % 100;
    format!("{}{dollars}.{frac:02}", if neg { "-" } else { "" })
}

fn billed_cents(s: &timer_proto::WorkSession, elapsed: chrono::Duration) -> i64 {
    let secs = elapsed.num_seconds().max(0);
    // rate_cents is per hour; convert seconds → hours via i128 to dodge overflow.
    let cents = (secs as i128) * (s.rate_cents as i128) / 3600_i128;
    cents.try_into().unwrap_or(i64::MAX)
}
