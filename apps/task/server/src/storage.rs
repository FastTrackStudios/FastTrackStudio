//! The Files placement layer's home in the server (issue #262).
//!
//! The Storage Location registry is **deployment-scoped**, not per org:
//! one physical volume serves many orgs, and an org reaches it only
//! through a Storage grant. So there is exactly one [`StorageCore`] per
//! server process, shared by every org lane and by the operator and agent
//! lanes on the server router — hence the `OnceLock` rather than a field
//! on each `OrgAppState`, which would give every org its own in-memory
//! view of one on-disk registry.
//!
//! On first initialization the server announces itself as a Storage agent
//! (the first of the three hostings) speaking for its own volume under
//! `<data_root>/files-volumes/`. It announces **pending**: an operator
//! approves it — and issues grants — through the `StorageAdminService` on
//! the server lane, the same way any other agent is admitted. Nothing is
//! placeable until they do, which is the point.

use std::path::Path;
use std::sync::{Arc, OnceLock};

use files_storage::core::{in_server_announcement, registry_dir, server_volume};
use files_storage::{InServerAgent, StorageCore};

static CORE: OnceLock<Arc<StorageCore>> = OnceLock::new();

/// The volume the server speaks for, under the data root.
fn volume_root(data_root: &Path) -> std::path::PathBuf {
    data_root.join("files-volumes").join("primary")
}

/// The process's storage coordinator, initialized on first use.
pub fn core(data_root: &Path) -> eyre::Result<Arc<StorageCore>> {
    if let Some(core) = CORE.get() {
        return Ok(core.clone());
    }
    let core = StorageCore::open(registry_dir(data_root))
        .map_err(|e| eyre::eyre!("storage registry: {e}"))?;

    // The in-server hosting: an ordinary agent that happens to live in
    // this process, so its directives are carried out inline.
    let agent_id = agent_id(data_root)?;
    let volume = volume_root(data_root);
    std::fs::create_dir_all(&volume)?;
    core.register_local_agent(Arc::new(InServerAgent::new(agent_id)));
    let announced = core
        .announce(in_server_announcement(
            agent_id,
            "task-server",
            vec![server_volume("primary", "Server primary", &volume)],
        ))
        .map_err(|e| eyre::eyre!("announce in-server storage agent: {e}"))?;
    tracing::info!(
        agent = %agent_id,
        status = ?announced.status,
        volume = %volume.display(),
        "files: in-server storage agent announced"
    );

    Ok(CORE.get_or_init(|| core).clone())
}

/// The in-server agent's stable id, persisted beside the registry so a
/// restart re-announces as the *same* agent (and keeps its approval)
/// rather than arriving as a stranger the operator must approve again.
fn agent_id(data_root: &Path) -> eyre::Result<uuid::Uuid> {
    let dir = registry_dir(data_root);
    std::fs::create_dir_all(&dir)?;
    let path = dir.join("in-server-agent-id");
    if let Ok(existing) = std::fs::read_to_string(&path)
        && let Ok(id) = existing.trim().parse()
    {
        return Ok(id);
    }
    let id = uuid::Uuid::new_v4();
    std::fs::write(&path, id.to_string())?;
    Ok(id)
}
