//! The deployment-scoped registry: agents, locations, grants, placements
//! and outstanding directives, persisted as one JSON document
//! (`<dir>/storage.json`) — the same plain-file idiom `files`' own root
//! registry uses, and for the same reason: this is a small, low-churn
//! index that has to survive a restart, not a database.
//!
//! Deployment-scoped is the load-bearing word (glossary "Storage
//! Location"): ONE of these serves every org in the deployment, which is
//! exactly why an org's reach into it is mediated by grants rather than
//! by having its own registry.

use std::path::{Path, PathBuf};
use std::sync::Mutex;

use files_storage_proto::{
    AgentDirective, AgentInfo, RootPlacement, StorageGrantInfo, StorageLocationInfo,
};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::error::{Error, Result};

/// A directive handed to an agent that has not reported back yet. The
/// wire directive says nothing about *where* the work lands (the agent
/// does not need to know); the coordinator keeps that here so an
/// incoming outcome can be applied to the right placement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Outstanding {
    pub directive: AgentDirective,
    /// The location the directive's result belongs to — the live tree's
    /// for hosting/measuring, the replica's for replication.
    pub location_id: Uuid,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct State {
    #[serde(default)]
    pub agents: Vec<AgentInfo>,
    #[serde(default)]
    pub locations: Vec<StorageLocationInfo>,
    #[serde(default)]
    pub grants: Vec<StorageGrantInfo>,
    #[serde(default)]
    pub placements: Vec<RootPlacement>,
    #[serde(default)]
    pub outstanding: Vec<Outstanding>,
}

impl State {
    pub fn agent(&self, id: Uuid) -> Option<&AgentInfo> {
        self.agents.iter().find(|a| a.id == id)
    }

    pub fn agent_mut(&mut self, id: Uuid) -> Option<&mut AgentInfo> {
        self.agents.iter_mut().find(|a| a.id == id)
    }

    pub fn location(&self, id: Uuid) -> Option<&StorageLocationInfo> {
        self.locations.iter().find(|l| l.id == id)
    }

    pub fn location_for_volume(&self, agent_id: Uuid, key: &str) -> Option<&StorageLocationInfo> {
        self.locations
            .iter()
            .find(|l| l.agent_id == agent_id && l.volume_key == key)
    }

    /// The grant admitting `org` onto `location` — the single gate every
    /// placement passes through.
    pub fn grant(&self, org: &str, location_id: Uuid) -> Option<&StorageGrantInfo> {
        self.grants
            .iter()
            .find(|g| g.org == org && g.location_id == location_id)
    }

    pub fn placement(&self, org: &str, root_id: Uuid) -> Option<&RootPlacement> {
        self.placements
            .iter()
            .find(|p| p.org == org && p.root_id == root_id)
    }

    pub fn placement_mut(&mut self, root_id: Uuid) -> Option<&mut RootPlacement> {
        self.placements.iter_mut().find(|p| p.root_id == root_id)
    }
}

/// The registry file plus the in-memory state it holds.
#[derive(Debug)]
pub struct Registry {
    path: PathBuf,
    state: Mutex<State>,
}

impl Registry {
    pub fn open(dir: &Path) -> Result<Self> {
        std::fs::create_dir_all(dir)?;
        let path = dir.join("storage.json");
        let state = if path.exists() {
            serde_json::from_slice(&std::fs::read(&path)?)?
        } else {
            State::default()
        };
        Ok(Self {
            path,
            state: Mutex::new(state),
        })
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, State> {
        self.state.lock().expect("storage registry lock poisoned")
    }

    /// Read-only access.
    pub fn read<T>(&self, f: impl FnOnce(&State) -> T) -> T {
        f(&self.lock())
    }

    /// Mutate under the lock and persist — the whole document is
    /// rewritten, which is fine at registry scale and keeps the file
    /// always internally consistent. A failed `f` persists nothing.
    pub fn write<T>(&self, f: impl FnOnce(&mut State) -> Result<T>) -> Result<T> {
        let mut state = self.lock();
        let out = f(&mut state)?;
        let bytes = serde_json::to_vec_pretty(&*state)?;
        let tmp = self.path.with_extension("json.tmp");
        std::fs::write(&tmp, bytes)?;
        std::fs::rename(&tmp, &self.path)?;
        Ok(out)
    }

    pub fn require_location(&self, id: Uuid) -> Result<StorageLocationInfo> {
        self.read(|s| s.location(id).cloned())
            .ok_or_else(|| Error::NotFound(format!("storage location {id}")))
    }
}
