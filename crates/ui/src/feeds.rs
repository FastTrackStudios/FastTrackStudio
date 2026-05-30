//! Multi-org data feeds.
//!
//! Each fetcher takes the resolved list of org slugs (from
//! [`crate::orgs::selected_slugs`]) and fans out **concurrently** —
//! establishing each org's service client and concatenating the rows.
//! "All" mode passes every hosted slug; single-org mode passes one.
//! Per-org failures are tolerated in multi-org mode (a down/empty org
//! doesn't blank the whole view); an error only surfaces if *nothing*
//! came back.

#[cfg(target_arch = "wasm32")]
use project::ProjectInfo;
#[cfg(target_arch = "wasm32")]
use task::TaskInfo as DbTask;

/// Active projects across the selected orgs (concurrent fan-out).
#[cfg(target_arch = "wasm32")]
pub async fn fetch_projects(slugs: &[String]) -> Result<Vec<ProjectInfo>, String> {
    let futs = slugs.iter().cloned().map(|slug| async move {
        match crate::vox_clients::establish_for::<project::ProjectServiceClient>(&slug).await {
            Ok(client) => client
                .list()
                .await
                .map_err(|e| format!("{slug}: list: {e:?}")),
            Err(e) => Err(format!("{slug}: {e}")),
        }
    });
    collect(futures_util::future::join_all(futs).await)
}

/// Tasks across the selected orgs (concurrent fan-out).
#[cfg(target_arch = "wasm32")]
pub async fn fetch_tasks(slugs: &[String]) -> Result<Vec<DbTask>, String> {
    Ok(fetch_tasks_tagged(slugs)
        .await?
        .into_iter()
        .map(|(_, t)| t)
        .collect())
}

/// Tasks across the selected orgs, each paired with the slug of the org
/// it came from — so mutations can be routed back to the right org's
/// `TaskService` when viewing "All".
#[cfg(target_arch = "wasm32")]
pub async fn fetch_tasks_tagged(slugs: &[String]) -> Result<Vec<(String, DbTask)>, String> {
    let futs = slugs.iter().cloned().map(|slug| async move {
        match crate::vox_clients::establish_for::<task::TaskServiceClient>(&slug).await {
            Ok(client) => client
                .list()
                .await
                .map(|rows| {
                    rows.into_iter()
                        .map(|t| (slug.clone(), t))
                        .collect::<Vec<_>>()
                })
                .map_err(|e| format!("{slug}: list: {e:?}")),
            Err(e) => Err(format!("{slug}: {e}")),
        }
    });
    collect(futures_util::future::join_all(futs).await)
}

/// Fetch one org's day-plan templates (drives the calendar schedule
/// overlay), in the order the backend lists them.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_day_templates(slug: &str) -> Result<Vec<scheduling_proto::DayTemplate>, String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::DayTemplatesClient>(slug).await?;
    client
        .list_day_templates()
        .await
        .map_err(|e| format!("{slug}: day templates: {e:?}"))
}

/// The saved per-date plan for `date` (ISO `YYYY-MM-DD`), or `None`
/// when the date hasn't been edited (caller materializes a default).
#[cfg(target_arch = "wasm32")]
pub async fn fetch_day_plan(
    slug: &str,
    date: &str,
) -> Result<Option<scheduling_proto::DayPlan>, String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::DayPlansClient>(slug).await?;
    client
        .get_day_plan(date.to_string())
        .await
        .map_err(|e| format!("{slug}: day plan {date}: {e:?}"))
}

/// Save (replacing) a per-date plan.
#[cfg(target_arch = "wasm32")]
pub async fn save_day_plan(slug: &str, plan: scheduling_proto::DayPlan) -> Result<(), String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::DayPlansClient>(slug).await?;
    client
        .upsert_day_plan(plan)
        .await
        .map_err(|e| format!("{slug}: save day plan: {e:?}"))
}

/// Delete a per-date plan, reverting that date to the template.
#[cfg(target_arch = "wasm32")]
pub async fn delete_day_plan(slug: &str, date: &str) -> Result<(), String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::DayPlansClient>(slug).await?;
    client
        .delete_day_plan(date.to_string())
        .await
        .map_err(|e| format!("{slug}: delete day plan {date}: {e:?}"))
}

// ── Timer ─────────────────────────────────────────────────────────

/// The currently-running session for `user_id` in this org, if any.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_active_timer(
    slug: &str,
    user_id: uuid::Uuid,
) -> Result<Option<timer_proto::WorkSession>, String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    client
        .active_timer(user_id)
        .await
        .map_err(|e| format!("{slug}: active timer: {e:?}"))
}

/// Recent sessions for `user_id`, newest first.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_recent_sessions(
    slug: &str,
    user_id: uuid::Uuid,
) -> Result<Vec<timer_proto::WorkSession>, String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    let filter = timer_proto::WorkSessionFilter {
        user_id: Some(user_id),
        ..Default::default()
    };
    let mut sessions = client
        .list_sessions(filter)
        .await
        .map_err(|e| format!("{slug}: list sessions: {e:?}"))?;
    sessions.sort_by(|a, b| b.start_time.cmp(&a.start_time));
    Ok(sessions)
}

/// Start a timer; returns the new open session.
#[cfg(target_arch = "wasm32")]
pub async fn start_timer(
    slug: &str,
    req: timer_proto::StartTimerRequest,
) -> Result<timer_proto::WorkSession, String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    client
        .start_timer(req)
        .await
        .map_err(|e| format!("{slug}: start timer: {e:?}"))
}

/// Stop `user_id`'s running timer; returns the closed session.
#[cfg(target_arch = "wasm32")]
pub async fn stop_timer(
    slug: &str,
    user_id: uuid::Uuid,
) -> Result<timer_proto::WorkSession, String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    client
        .stop_timer(user_id)
        .await
        .map_err(|e| format!("{slug}: stop timer: {e:?}"))
}

/// Fetch one org's vault markdown as `WikiFile`s for the knowledge
/// graph: pull the manifest, then read every `.md` file concurrently
/// over the one socket. Pure graph-building happens caller-side.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_wiki_files(slug: &str) -> Result<Vec<view_knowledge_graph::WikiFile>, String> {
    use view_knowledge_graph::WikiFile;

    let client = crate::vox_clients::establish_for::<vault_proto::VaultSyncClient>(slug).await?;
    let manifest = client
        .manifest("default".to_owned())
        .await
        .map_err(|e| format!("manifest: {e:?}"))?;
    let md_paths: Vec<String> = manifest
        .files
        .into_iter()
        .map(|f| f.path)
        .filter(|p| p.ends_with(".md"))
        .collect();

    let futs = md_paths.into_iter().map(|path| {
        let c = client.clone();
        async move {
            let bytes = c.get_file("default".to_owned(), path.clone()).await.ok()?;
            let name = std::path::Path::new(&path)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or(&path)
                .to_string();
            Some(WikiFile {
                name,
                path,
                content: String::from_utf8_lossy(&bytes.0).into_owned(),
            })
        }
    });
    Ok(futures_util::future::join_all(futs)
        .await
        .into_iter()
        .flatten()
        .collect())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_wiki_files(_slug: &str) -> Result<Vec<view_knowledge_graph::WikiFile>, String> {
    Err("native client not wired yet".to_owned())
}

/// Locate a single project by id across the selected orgs, returning it
/// together with the slug of the org that owns it. Used by the project
/// detail page so it works regardless of which org is in view.
#[cfg(target_arch = "wasm32")]
pub async fn find_project(id: &str, slugs: &[String]) -> Result<(ProjectInfo, String), String> {
    let uuid = uuid::Uuid::parse_str(id).map_err(|_| "invalid project id".to_owned())?;
    let mut last_err = None;
    for slug in slugs {
        match crate::vox_clients::establish_for::<project::ProjectServiceClient>(slug).await {
            Ok(client) => match client.get(uuid).await {
                Ok(p) => return Ok((p, slug.clone())),
                Err(e) => last_err = Some(format!("{slug}: {e:?}")),
            },
            Err(e) => last_err = Some(format!("{slug}: {e}")),
        }
    }
    Err(last_err.unwrap_or_else(|| "project not found in any hosted org".to_owned()))
}

/// Flatten per-org results: concat the successes; surface an error only
/// if every org failed *and* nothing came back.
#[cfg(target_arch = "wasm32")]
fn collect<T>(results: Vec<Result<Vec<T>, String>>) -> Result<Vec<T>, String> {
    let mut out = Vec::new();
    let mut last_err = None;
    for r in results {
        match r {
            Ok(rows) => out.extend(rows),
            Err(e) => last_err = Some(e),
        }
    }
    if out.is_empty() {
        if let Some(e) = last_err {
            return Err(e);
        }
    }
    Ok(out)
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_projects(_slugs: &[String]) -> Result<Vec<project::ProjectInfo>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_tasks(_slugs: &[String]) -> Result<Vec<task::TaskInfo>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_tasks_tagged(
    _slugs: &[String],
) -> Result<Vec<(String, task::TaskInfo)>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_day_templates(
    _slug: &str,
) -> Result<Vec<scheduling_proto::DayTemplate>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_day_plan(
    _slug: &str,
    _date: &str,
) -> Result<Option<scheduling_proto::DayPlan>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn save_day_plan(_slug: &str, _plan: scheduling_proto::DayPlan) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn delete_day_plan(_slug: &str, _date: &str) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_active_timer(
    _slug: &str,
    _user_id: uuid::Uuid,
) -> Result<Option<timer_proto::WorkSession>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_recent_sessions(
    _slug: &str,
    _user_id: uuid::Uuid,
) -> Result<Vec<timer_proto::WorkSession>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn start_timer(
    _slug: &str,
    _req: timer_proto::StartTimerRequest,
) -> Result<timer_proto::WorkSession, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn stop_timer(
    _slug: &str,
    _user_id: uuid::Uuid,
) -> Result<timer_proto::WorkSession, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn find_project(
    _id: &str,
    _slugs: &[String],
) -> Result<(project::ProjectInfo, String), String> {
    Err("native client not wired yet".to_owned())
}
