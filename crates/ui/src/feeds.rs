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
    let futs = slugs.iter().cloned().map(|slug| async move {
        match crate::vox_clients::establish_for::<task::TaskServiceClient>(&slug).await {
            Ok(client) => client.list().await.map_err(|e| format!("{slug}: list: {e:?}")),
            Err(e) => Err(format!("{slug}: {e}")),
        }
    });
    collect(futures_util::future::join_all(futs).await)
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
