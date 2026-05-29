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
            Ok(client) => client.list().await.map_err(|e| format!("{slug}: list: {e:?}")),
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
pub async fn find_project(
    _id: &str,
    _slugs: &[String],
) -> Result<(project::ProjectInfo, String), String> {
    Err("native client not wired yet".to_owned())
}
