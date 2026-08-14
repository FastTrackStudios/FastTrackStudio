//! The Files placement layer's home in the server (issue #262).
//!
//! The Storage Location registry is **deployment-scoped**, not per org:
//! one physical volume serves many orgs, and an org reaches it only
//! through a Storage grant. So there is exactly one [`StorageCore`] per
//! server process — built here, held as a field on [`crate::AppState`]
//! next to the data root it belongs to, and handed to both
//! `build_org_state` and `server_layer_router`.
//!
//! It used to be a process-global `OnceLock`, which was wrong three ways
//! (PR #284 review): a second `AppState` with a different data root
//! silently reused the first deployment's registry, two concurrent first
//! callers each ran the pre-`get_or_init` side effects (including
//! registry writes, which are last-writer-wins), and every caller
//! resolved `DataRoot::from_env` independently — so a vault-root-only
//! test server touched `$HOME/.task`. One owner, one construction, one
//! failure policy: **fatal**, at `AppState` construction, where the data
//! root is already resolved and ensured.
//!
//! On construction the server enrolls itself as a Storage agent (the
//! first of the three hostings) speaking for its own volume under
//! `<data_root>/files-volumes/`. It enrolls **pending**: an operator
//! approves it — and issues grants — through the `StorageAdminService`
//! on the server lane, the same way any other agent is admitted. Nothing
//! is placeable until they do, which is the point.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use files_storage::core::{in_server_announcement, registry_dir, server_volume};
use files_storage::{InServerAgent, StorageCore, StorageError};
use serde::{Deserialize, Serialize};

/// The volume the server speaks for, under the data root.
fn volume_root(data_root: &Path) -> PathBuf {
    data_root.join("files-volumes").join("primary")
}

/// Extra volumes this server speaks for, from `TASK_STORAGE_VOLUMES`:
/// `key=/abs/path` pairs, comma-separated.
///
///     TASK_STORAGE_VOLUMES="media=/mnt/storage/Task"
///
/// The in-server agent announces only `primary`, under the data root —
/// which on a cluster deployment is the server's own PVC. Media that
/// was never going to fit there (a NAS mount, an external volume) is
/// therefore unannounceable, and since a Storage Location can only be
/// admitted from an ANNOUNCED volume, a File Root on that media could
/// not be granted at all. This is how such a mount enters the registry.
///
/// A path that does not exist is SKIPPED with a warning rather than
/// failing the boot: the mount may simply not be present on this node
/// yet, and refusing to start a server because a media volume is
/// missing would take the whole instance down for a storage detail.
///
/// Malformed entries are likewise warned about and skipped — an
/// operator typo should cost one volume, not the deployment.
fn extra_volumes() -> Vec<(String, PathBuf)> {
    let Ok(raw) = std::env::var("TASK_STORAGE_VOLUMES") else {
        return Vec::new();
    };
    raw.split(',')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .filter_map(|entry| {
            let Some((key, path)) = entry.split_once('=') else {
                tracing::warn!(
                    entry,
                    "TASK_STORAGE_VOLUMES: expected key=/abs/path — skipped"
                );
                return None;
            };
            let (key, path) = (key.trim(), Path::new(path.trim()));
            if key.is_empty() || !path.is_absolute() {
                tracing::warn!(
                    entry,
                    "TASK_STORAGE_VOLUMES: needs a key and an absolute path — skipped"
                );
                return None;
            }
            if !path.is_dir() {
                tracing::warn!(
                    key,
                    path = %path.display(),
                    "TASK_STORAGE_VOLUMES: not a directory on this node — skipped"
                );
                return None;
            }
            Some((key.to_owned(), path.to_path_buf()))
        })
        .collect()
}

/// The in-server agent's persisted identity: a stable id **and** the
/// enrollment secret it must present to re-announce. Both live beside
/// the registry so a restart comes back as the same agent, keeping its
/// approval, rather than arriving as a stranger.
#[derive(Debug, Serialize, Deserialize)]
struct AgentIdentity {
    id: uuid::Uuid,
    token: String,
}

/// Build the deployment's storage coordinator and enroll the in-server
/// agent. Called once per `AppState`.
pub fn open(data_root: &Path) -> eyre::Result<Arc<StorageCore>> {
    let core = StorageCore::open(registry_dir(data_root))
        .map_err(|e| eyre::eyre!("storage registry: {e}"))?;

    let volume = volume_root(data_root);
    std::fs::create_dir_all(&volume)?;

    // The in-server hosting: an ordinary agent that happens to live in
    // this process, so its directives are carried out inline.
    let identity = load_identity(data_root)?;
    let agent_id = identity
        .as_ref()
        .map(|i| i.id)
        .unwrap_or_else(uuid::Uuid::new_v4);
    core.register_local_agent(Arc::new(InServerAgent::new(agent_id)));

    let mut volumes = vec![server_volume("primary", "Server primary", &volume)];
    for (key, path) in extra_volumes() {
        tracing::info!(key, path = %path.display(), "announcing extra storage volume");
        volumes.push(server_volume(key.clone(), key, &path));
    }

    let enrollment = core
        .announce(in_server_announcement(
            agent_id,
            "task-server",
            identity.as_ref().map(|i| i.token.clone()),
            volumes,
        ))
        .map_err(|e| match e {
            // A stored token the coordinator rejects means the registry
            // and the identity file disagree — refuse rather than fork
            // the volume under a second agent id.
            StorageError::Unauthorized(m) => eyre::eyre!(
                "in-server storage agent {agent_id} failed to re-enroll ({m}); \
                 `storage.json` and `in-server-agent.json` disagree"
            ),
            other => eyre::eyre!("announce in-server storage agent: {other}"),
        })?;

    if let Some(token) = enrollment.token {
        // First enrollment: persist the secret we were just handed. It
        // is never transmitted again.
        store_identity(
            data_root,
            &AgentIdentity {
                id: agent_id,
                token,
            },
        )?;
    }

    tracing::info!(
        agent = %agent_id,
        status = ?enrollment.agent.status,
        volume = %volume.display(),
        "files: in-server storage agent enrolled"
    );
    Ok(core)
}

fn identity_path(data_root: &Path) -> PathBuf {
    registry_dir(data_root).join("in-server-agent.json")
}

/// Read the persisted identity. **Absent is fine** (first boot);
/// present-but-unreadable is fatal.
///
/// Silently minting a new id on any read failure — a truncated file from
/// a crash mid-write, a stray byte, a transient `EACCES` — breaks the
/// stable-identity invariant in the worst way: the server re-announces
/// as a brand-new pending agent, and on approval the operator gets a
/// *second* location for the same physical volume while every existing
/// grant and placement still points at the first (PR #284 review).
fn load_identity(data_root: &Path) -> eyre::Result<Option<AgentIdentity>> {
    let path = identity_path(data_root);
    match std::fs::read(&path) {
        Ok(bytes) => serde_json::from_slice(&bytes).map(Some).map_err(|e| {
            eyre::eyre!(
                "{}: in-server storage agent identity is unreadable ({e}). Refusing to mint a \
                 new one — that would fork the volume under a second agent. Restore the file, \
                 or delete it AND the agent's entry in storage.json to re-enroll.",
                path.display()
            )
        }),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(eyre::eyre!(
            "{}: cannot read the in-server storage agent identity: {e}",
            path.display()
        )),
    }
}

/// Write the identity atomically (tmp + rename), so a crash mid-write
/// leaves the previous file intact rather than a truncated one.
fn store_identity(data_root: &Path, identity: &AgentIdentity) -> eyre::Result<()> {
    let path = identity_path(data_root);
    if let Some(dir) = path.parent() {
        std::fs::create_dir_all(dir)?;
    }
    let tmp = path.with_extension("json.tmp");
    std::fs::write(&tmp, serde_json::to_vec_pretty(identity)?)?;
    std::fs::rename(&tmp, &path)?;
    Ok(())
}

/// The operator-lane authorization the `StorageAdminService` runs on
/// `/server/vox`: a session token validated against the home org, the
/// same check `OrgManagementImpl::create_org` performs for the same
/// reason — that lane has no permission gate in front of it, so a
/// service mounted there authorizes its own callers or it authorizes
/// nobody.
pub struct HomeOrgOperator {
    state: crate::AppState,
}

impl HomeOrgOperator {
    #[must_use]
    pub fn new(state: crate::AppState) -> Self {
        Self { state }
    }
}

impl files_storage::OperatorAuth for HomeOrgOperator {
    fn authorize<'a>(&'a self, session_token: &'a str) -> files_storage::AuthorizeFuture<'a> {
        Box::pin(async move {
            let Some(home_slug) = self.state.home_slug() else {
                return Err(StorageError::Unauthorized(
                    "server has no home org — cannot validate an operator session".into(),
                ));
            };
            if session_token.is_empty() {
                return Err(StorageError::Unauthorized(
                    "missing session token (storage administration is an operator action)".into(),
                ));
            }
            let home = self.state.org(&home_slug).ok_or_else(|| {
                StorageError::Unauthorized(format!("home org `{home_slug}` not in live dispatcher"))
            })?;
            home.auth
                .auth
                .current_session(architect_auth::commands::CurrentSession {
                    token: session_token.to_string(),
                })
                .await
                .map_err(|e| StorageError::Unauthorized(format!("invalid session token: {e}")))?;
            Ok(())
        })
    }
}

/// This org's view of the deployment's Storage Locations, as the Files
/// backend's confinement boundary (issue #262).
///
/// A File Root may live under `<org>/files` — always — or under any
/// location the org holds a live-tree grant on. Without the second half
/// media on a NAS is unregisterable: the boundary check refuses a path
/// outside the org directory, which is on the server's own disk and was
/// never going to hold a 236 GiB video project.
///
/// Deliberately holds the registry rather than a resolved list. Grants
/// are issued at runtime, and a boundary snapshotted at boot would mean
/// a new Storage Location only takes effect after a restart — the kind
/// of staleness that gets diagnosed as "the mount is broken".
pub struct GrantedBoundaries {
    core: Arc<StorageCore>,
    org: String,
}

impl GrantedBoundaries {
    #[must_use]
    pub fn new(core: Arc<StorageCore>, org: String) -> Self {
        Self { core, org }
    }
}

impl files::LocationBoundaries for GrantedBoundaries {
    fn permitted(&self) -> Vec<PathBuf> {
        self.core.live_tree_boundaries(&self.org)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Review finding 9: a persisted identity that cannot be read must
    /// stop the server, not silently mint a new one.
    ///
    /// The silent path forks the volume: the server re-announces as a
    /// brand-new pending agent, and approving it registers a SECOND
    /// location for the same directory while every existing grant and
    /// placement still names the first — whose agent will never speak
    /// again.
    #[test]
    fn a_corrupt_agent_identity_refuses_to_start_rather_than_forking_the_volume() {
        let dir = tempfile::tempdir().expect("data root");
        let root = dir.path();

        // First boot enrolls and persists id + secret.
        let core = open(root).expect("first boot");
        let first = core.list_agents();
        assert_eq!(first.len(), 1, "the in-server agent enrolled");
        drop(core);

        // Second boot re-announces as the SAME agent.
        let core = open(root).expect("second boot");
        assert_eq!(
            core.list_agents().len(),
            1,
            "a restart re-announces rather than enrolling a second agent"
        );
        assert_eq!(core.list_agents()[0].id, first[0].id, "same identity");
        drop(core);

        // Now truncate the identity file, as a crash mid-write would.
        std::fs::write(identity_path(root), b"{\"id\": \"tru").expect("truncate");
        let err = open(root).expect_err("a corrupt identity must not boot");
        let message = format!("{err:#}");
        assert!(
            message.contains("unreadable"),
            "the error should name the problem: {message}"
        );

        // And nothing was invented in the registry while failing.
        let core = StorageCore::open(registry_dir(root)).expect("registry still opens");
        assert_eq!(
            core.list_agents().len(),
            1,
            "a refused boot must not have enrolled a second agent"
        );
    }

    /// A missing identity file is the ordinary first-boot case, not an
    /// error — the distinction the silent-mint path erased.
    #[test]
    fn a_missing_agent_identity_is_first_boot() {
        let dir = tempfile::tempdir().expect("data root");
        assert!(load_identity(dir.path()).expect("absent is fine").is_none());
        open(dir.path()).expect("first boot enrolls");
        assert!(load_identity(dir.path()).expect("readable").is_some());
    }
}
