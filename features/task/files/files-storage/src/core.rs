//! The coordinator: one deployment-scoped object holding the registry,
//! the org-lane event hubs, the agent-directive hub, and every rule that
//! governs placement. The three RPC lanes ([`crate::admin`],
//! [`crate::org`], [`crate::agent_lane`]) are thin shells over this.
//!
//! The rules, all of them enforced here rather than at any lane's edge:
//!
//! - An org reaches a location **only** through a grant. No grant is
//!   indistinguishable from no location — an org lane never learns that a
//!   location it wasn't admitted to exists.
//! - A grant's **capability subset** gates each axis: `LiveTrees` to host
//!   a live tree, `Blobs` to hold replicas. A blob-only location can
//!   never hold a live tree, whoever asks.
//! - A grant's **path prefix** is the org's subtree, enforced textually
//!   before anything is created and by canonicalization afterwards (see
//!   [`crate::paths`]).
//! - A grant's **logical-byte quota** is checked before every placement
//!   that could add bytes. Usage is derived from placements, never a
//!   counter that can drift.
//! - An agent's volumes become locations only once the operator
//!   **approves** it. The coordinator never becomes the data path: it
//!   issues directives, and agents move bytes.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use chrono::Utc;
use files_storage_proto::{
    AgentAnnouncement, AgentDirective, AgentHosting, AgentInfo, AgentStatus, AnnouncedVolume,
    BlobReplica, CapabilityClass, DirectiveKind, DirectiveOutcome, GrantSpec, GrantUsage,
    LiveTreeBinding, LocationHealth, PlacementStatus, RootPlacement, StorageEvent,
    StorageGrantInfo, StorageLocationInfo, VolumeHealth,
};
use uuid::Uuid;

use crate::agent::LocalAgent;
use crate::error::{Error, Result};
use crate::paths;
use crate::state::{Outstanding, Registry, State};

/// Sub-directory of a blob-capable location's granted prefix that holds
/// replicas, one chunk store per root. Kept out of the way of any live
/// tree the same grant may also host.
const REPLICA_DIR: &str = "blobs";

pub struct StorageCore {
    registry: Registry,
    /// Agents living in this process (the in-server hosting, and in tests
    /// a fake). A directive for one of these is executed inline; anything
    /// else waits on the wire protocol.
    local_agents: Mutex<HashMap<Uuid, Arc<dyn LocalAgent>>>,
    /// One hub per org — an org's subscribers must never see another
    /// org's grants or placements, and `#[subscribe]` hands out a
    /// `&PubSub`, so the per-org backend holds a clone of its own hub.
    org_hubs: Mutex<HashMap<String, architect::PubSub<StorageEvent>>>,
    /// The agent lane's single directive hub. Directives carry their
    /// `agent_id` and agents filter client-side (root CLAUDE.md's
    /// `#[subscribe]` idiom).
    directives: architect::PubSub<AgentDirective>,
}

impl std::fmt::Debug for StorageCore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StorageCore").finish_non_exhaustive()
    }
}

impl StorageCore {
    /// Open (or create) the deployment's registry under `dir`.
    pub fn open(dir: impl AsRef<Path>) -> Result<Arc<Self>> {
        Ok(Arc::new(Self {
            registry: Registry::open(dir.as_ref())?,
            local_agents: Mutex::new(HashMap::new()),
            org_hubs: Mutex::new(HashMap::new()),
            directives: architect::PubSub::sliding(256),
        }))
    }

    /// Attach an in-process agent. Its directives are executed inline, so
    /// a placement onto one of its volumes is `Hosted` by the time
    /// `place_root` returns.
    pub fn register_local_agent(&self, agent: Arc<dyn LocalAgent>) {
        self.local_agents
            .lock()
            .expect("local agent map poisoned")
            .insert(agent.id(), agent);
    }

    fn local_agent(&self, id: Uuid) -> Option<Arc<dyn LocalAgent>> {
        self.local_agents
            .lock()
            .expect("local agent map poisoned")
            .get(&id)
            .cloned()
    }

    pub fn directives_hub(&self) -> &architect::PubSub<AgentDirective> {
        &self.directives
    }

    /// This org's event hub, created on first use. Cloned hubs share one
    /// subscriber list.
    pub fn org_hub(&self, org: &str) -> architect::PubSub<StorageEvent> {
        self.org_hubs
            .lock()
            .expect("org hub map poisoned")
            .entry(org.to_string())
            .or_insert_with(|| architect::PubSub::sliding(256))
            .clone()
    }

    fn publish(&self, org: &str, event: StorageEvent) {
        self.org_hub(org).publish(event);
    }

    // ── Agent lane ──────────────────────────────────────────────────

    /// Announce (or re-announce) an agent. New agents land `Pending`;
    /// re-announcing updates the volume list and `last_seen` but never
    /// resets an approval — an agent restarting must not need
    /// re-approval, and must not be able to grant itself one either.
    pub fn announce(&self, announcement: AgentAnnouncement) -> Result<AgentInfo> {
        let AgentAnnouncement {
            agent_id,
            hosting,
            label,
            volumes,
        } = announcement;
        for volume in &volumes {
            if volume.key.trim().is_empty() {
                return Err(Error::BadRequest("announced volume has no key".into()));
            }
            if volume.capabilities.is_empty() {
                return Err(Error::BadRequest(format!(
                    "volume {} announces no capability classes",
                    volume.key
                )));
            }
        }
        self.registry.write(|state| {
            let now = Utc::now();
            if let Some(existing) = state.agent_mut(agent_id) {
                existing.hosting = hosting;
                existing.label = label;
                existing.volumes = volumes;
                existing.last_seen = now;
                return Ok(existing.clone());
            }
            let info = AgentInfo {
                id: agent_id,
                hosting,
                label,
                status: AgentStatus::Pending,
                volumes,
                last_seen: now,
            };
            state.agents.push(info.clone());
            Ok(info)
        })
    }

    /// Heartbeat: liveness plus per-volume health, propagated onto the
    /// volumes' registered locations.
    pub fn heartbeat(&self, agent_id: Uuid, volumes: Vec<VolumeHealth>) -> Result<AgentInfo> {
        let (info, changed) = self.registry.write(|state| {
            let Some(agent) = state.agent_mut(agent_id) else {
                return Err(Error::NotFound(format!("agent {agent_id}")));
            };
            agent.last_seen = Utc::now();
            let info = agent.clone();
            let mut changed = Vec::new();
            for report in &volumes {
                if let Some(location) = state
                    .locations
                    .iter_mut()
                    .find(|l| l.agent_id == agent_id && l.volume_key == report.volume_key)
                {
                    if location.health != report.health {
                        location.health = report.health;
                        changed.push(location.clone());
                    }
                }
            }
            Ok((info, changed))
        })?;
        for location in changed {
            for org in self.orgs_granted_on(location.id) {
                self.publish(&org, StorageEvent::LocationChanged(location.clone()));
            }
        }
        Ok(info)
    }

    pub fn list_agents(&self) -> Vec<AgentInfo> {
        self.registry.read(|s| s.agents.clone())
    }

    pub fn pending_directives(&self, agent_id: Uuid) -> Vec<AgentDirective> {
        self.registry.read(|s| {
            s.outstanding
                .iter()
                .filter(|o| o.directive.agent_id == agent_id)
                .map(|o| o.directive.clone())
                .collect()
        })
    }

    // ── Operator lane ───────────────────────────────────────────────

    /// Approve (or reject) an agent. Approval is what turns announced
    /// volumes into registered Storage Locations — the step that keeps a
    /// rogue agent out of the data path.
    pub fn approve_agent(&self, agent_id: Uuid, approved: bool) -> Result<AgentInfo> {
        let (info, registered, offlined) = self.registry.write(|state| {
            let Some(agent) = state.agent_mut(agent_id) else {
                return Err(Error::NotFound(format!("agent {agent_id}")));
            };
            agent.status = if approved {
                AgentStatus::Approved
            } else {
                AgentStatus::Rejected
            };
            let info = agent.clone();

            let mut registered = Vec::new();
            let mut offlined = Vec::new();
            if approved {
                for volume in &info.volumes {
                    if state.location_for_volume(agent_id, &volume.key).is_none() {
                        let location = new_location(agent_id, volume);
                        state.locations.push(location.clone());
                        registered.push(location);
                    }
                }
            } else {
                // A revoked approval is not a delete: placements and
                // their data stay, the locations simply go offline.
                for location in state
                    .locations
                    .iter_mut()
                    .filter(|l| l.agent_id == agent_id)
                {
                    location.health = LocationHealth::Offline;
                    offlined.push(location.clone());
                }
            }
            Ok((info, registered, offlined))
        })?;
        for location in registered.into_iter().chain(offlined) {
            for org in self.orgs_granted_on(location.id) {
                self.publish(&org, StorageEvent::LocationChanged(location.clone()));
            }
        }
        Ok(info)
    }

    /// Admit one announced volume of an already-approved agent.
    pub fn register_location(
        &self,
        agent_id: Uuid,
        volume_key: &str,
    ) -> Result<StorageLocationInfo> {
        self.registry.write(|state| {
            let Some(agent) = state.agent(agent_id) else {
                return Err(Error::NotFound(format!("agent {agent_id}")));
            };
            if agent.status != AgentStatus::Approved {
                return Err(Error::AgentNotApproved(format!(
                    "agent {agent_id} is {:?}",
                    agent.status
                )));
            }
            let Some(volume) = agent.volumes.iter().find(|v| v.key == volume_key).cloned() else {
                return Err(Error::NotFound(format!(
                    "agent {agent_id} announced no volume {volume_key}"
                )));
            };
            if let Some(existing) = state.location_for_volume(agent_id, volume_key) {
                return Err(Error::AlreadyExists(format!(
                    "volume {volume_key} is already location {}",
                    existing.id
                )));
            }
            let location = new_location(agent_id, &volume);
            state.locations.push(location.clone());
            Ok(location)
        })
    }

    pub fn list_locations(&self) -> Vec<StorageLocationInfo> {
        self.registry.read(|s| s.locations.clone())
    }

    /// Admit an org onto a location. Re-issuing for the same (org,
    /// location) replaces the terms and keeps the grant's id.
    pub fn issue_grant(&self, spec: GrantSpec) -> Result<StorageGrantInfo> {
        if spec.org.trim().is_empty() {
            return Err(Error::BadRequest("grant has no org".into()));
        }
        if spec.capabilities.is_empty() {
            return Err(Error::BadRequest(
                "a grant with no capability class admits nothing".into(),
            ));
        }
        // The prefix is a path the org's whole subtree hangs off; it must
        // itself be a safe relative path, or "the org's own subtree"
        // means nothing.
        paths::safe_relative(&spec.path_prefix)?;

        let grant = self.registry.write(|state| {
            let Some(location) = state.location(spec.location_id).cloned() else {
                return Err(Error::NotFound(format!(
                    "storage location {}",
                    spec.location_id
                )));
            };
            if let Some(extra) = spec
                .capabilities
                .iter()
                .find(|c| !location.capabilities.contains(c))
            {
                return Err(Error::CapabilityDenied(format!(
                    "location {} does not offer {extra:?}",
                    location.id
                )));
            }
            let now = Utc::now();
            if let Some(existing) = state
                .grants
                .iter_mut()
                .find(|g| g.org == spec.org && g.location_id == spec.location_id)
            {
                existing.capabilities = spec.capabilities;
                existing.quota_bytes = spec.quota_bytes;
                existing.path_prefix = spec.path_prefix;
                existing.granted_at = now;
                return Ok(existing.clone());
            }
            let grant = StorageGrantInfo {
                id: Uuid::new_v4(),
                org: spec.org,
                location_id: spec.location_id,
                capabilities: spec.capabilities,
                quota_bytes: spec.quota_bytes,
                used_bytes: 0,
                path_prefix: spec.path_prefix,
                granted_at: now,
            };
            state.grants.push(grant.clone());
            Ok(grant)
        })?;
        let grant = self.with_usage(grant);
        self.publish(&grant.org, StorageEvent::GrantIssued(grant.clone()));
        Ok(grant)
    }

    /// Withdraw an org's admission. Data already placed is left exactly
    /// where it is — revoking is an admission change, not a delete.
    pub fn revoke_grant(&self, grant_id: Uuid) -> Result<()> {
        let org = self.registry.write(|state| {
            let Some(index) = state.grants.iter().position(|g| g.id == grant_id) else {
                return Err(Error::NotFound(format!("storage grant {grant_id}")));
            };
            Ok(state.grants.remove(index).org)
        })?;
        self.publish(&org, StorageEvent::GrantRevoked(grant_id));
        Ok(())
    }

    pub fn list_grants(&self, org: Option<&str>) -> Vec<StorageGrantInfo> {
        self.registry
            .read(|s| {
                s.grants
                    .iter()
                    .filter(|g| org.is_none_or(|o| g.org == o))
                    .cloned()
                    .collect::<Vec<_>>()
            })
            .into_iter()
            .map(|g| self.with_usage(g))
            .collect()
    }

    // ── Org lane ────────────────────────────────────────────────────

    /// Locations this org holds a grant on — the only ones it can see.
    pub fn locations_for(&self, org: &str) -> Vec<StorageLocationInfo> {
        self.registry.read(|state| {
            state
                .locations
                .iter()
                .filter(|l| state.grant(org, l.id).is_some())
                .cloned()
                .collect()
        })
    }

    pub fn placement(&self, org: &str, root_id: Uuid) -> Result<RootPlacement> {
        self.registry
            .read(|s| s.placement(org, root_id).cloned())
            .ok_or_else(|| Error::NotFound(format!("placement for root {root_id}")))
    }

    pub fn list_placements(&self, org: &str) -> Vec<RootPlacement> {
        self.registry.read(|s| {
            s.placements
                .iter()
                .filter(|p| p.org == org)
                .cloned()
                .collect()
        })
    }

    pub fn usage(&self, org: &str, location_id: Uuid) -> Result<GrantUsage> {
        let grant = self.require_grant(org, location_id)?;
        let (used_bytes, placements) = self.registry.read(|s| usage_of(s, org, location_id));
        Ok(GrantUsage {
            location_id,
            quota_bytes: grant.quota_bytes,
            used_bytes,
            placements,
        })
    }

    /// Bind a root's live tree to a location and have that location's
    /// agent host it.
    pub fn place_root(
        &self,
        org: &str,
        root_id: Uuid,
        location_id: Uuid,
        relative_path: &str,
    ) -> Result<RootPlacement> {
        let grant = self.require_grant(org, location_id)?;
        self.require_capability(&grant, CapabilityClass::LiveTrees, "host a live tree")?;
        let location = self.require_online(location_id)?;
        self.require_headroom(org, &grant)?;

        let relative = paths::safe_relative(relative_path)?;
        let prefix_dir =
            Path::new(&location.root_path).join(paths::safe_relative(&grant.path_prefix)?);
        let target = prefix_dir.join(&relative);
        let absolute_path = paths::to_utf8(&target)?;
        let relative_path = paths::to_utf8(&relative)?;

        let directive_id = Uuid::new_v4();
        let directive = AgentDirective {
            id: directive_id,
            agent_id: location.agent_id,
            kind: DirectiveKind::HostLiveTree {
                root_id,
                org: org.to_string(),
                absolute_path: absolute_path.clone(),
            },
        };

        self.registry.write(|state| {
            if let Some(existing) = state.placement(org, root_id) {
                if existing.live_tree.is_some() {
                    return Err(Error::AlreadyExists(format!(
                        "root {root_id} already has a live tree"
                    )));
                }
            }
            // A root's live tree sits wholly on one location, and two
            // roots never share a tree (glossary "File Root": roots never
            // overlap on disk).
            if let Some(clash) = state.placements.iter().find(|p| {
                p.root_id != root_id
                    && p.live_tree
                        .as_ref()
                        .is_some_and(|lt| lt.absolute_path == absolute_path)
            }) {
                return Err(Error::AlreadyExists(format!(
                    "{absolute_path} is already root {}'s live tree",
                    clash.root_id
                )));
            }
            let binding = LiveTreeBinding {
                location_id,
                relative_path: relative_path.clone(),
                absolute_path: absolute_path.clone(),
                repo_initialized: false,
            };
            match state.placement_mut(root_id) {
                Some(existing) => existing.live_tree = Some(binding),
                None => state.placements.push(RootPlacement {
                    root_id,
                    org: org.to_string(),
                    status: PlacementStatus::Pending,
                    live_tree: Some(binding),
                    logical_bytes: 0,
                    replicas: Vec::new(),
                    failure: None,
                }),
            }
            state.outstanding.push(Outstanding {
                directive: directive.clone(),
                location_id,
            });
            Ok(())
        })?;

        self.dispatch(directive)?;

        // Symlink escape is only resolvable once the tree exists, and
        // only by whoever can see the filesystem. For a local agent that
        // is us, right here; a remote agent runs the same check on
        // receipt (it holds the volume, we hold the grant).
        if self.local_agent(location.agent_id).is_some()
            && let Err(escape) = paths::confine(&target, &prefix_dir)
        {
            self.fail_placement(org, root_id, &escape.to_string())?;
            return Err(escape);
        }
        self.finish(org, root_id)
    }

    /// Replicate a root's version-store blobs onto a second location —
    /// the axis independent of the live tree.
    pub fn add_blob_replica(
        &self,
        org: &str,
        root_id: Uuid,
        location_id: Uuid,
    ) -> Result<RootPlacement> {
        let grant = self.require_grant(org, location_id)?;
        self.require_capability(&grant, CapabilityClass::Blobs, "hold blob replicas")?;
        let location = self.require_online(location_id)?;

        let placement = self.placement(org, root_id)?;
        let Some(live_tree) = placement.live_tree.clone() else {
            return Err(Error::BadRequest(format!(
                "root {root_id} has no live tree to replicate from"
            )));
        };
        if live_tree.location_id == location_id {
            return Err(Error::BadRequest(
                "a root's blob replica must live on a different location than its live tree".into(),
            ));
        }
        // The replica costs the destination grant the root's logical
        // bytes; refuse before moving any of them.
        let (used, _) = self.registry.read(|s| usage_of(s, org, location_id));
        let projected = used.saturating_add(placement.logical_bytes);
        if projected > grant.quota_bytes {
            return Err(Error::QuotaExceeded(format!(
                "replicating root {root_id} needs {} logical bytes on location {location_id}; \
                 {used} of {} used",
                placement.logical_bytes, grant.quota_bytes
            )));
        }

        let dest = Path::new(&location.root_path)
            .join(paths::safe_relative(&grant.path_prefix)?)
            .join(REPLICA_DIR)
            .join(root_id.to_string());
        let dest_path = paths::to_utf8(&dest)?;

        let directive = AgentDirective {
            id: Uuid::new_v4(),
            agent_id: location.agent_id,
            kind: DirectiveKind::ReplicateBlobs {
                root_id,
                org: org.to_string(),
                source_path: live_tree.absolute_path.clone(),
                dest_path: dest_path.clone(),
            },
        };

        self.registry.write(|state| {
            let Some(placement) = state.placement_mut(root_id) else {
                return Err(Error::NotFound(format!("placement for root {root_id}")));
            };
            if !placement
                .replicas
                .iter()
                .any(|r| r.location_id == location_id)
            {
                placement.replicas.push(BlobReplica {
                    location_id,
                    absolute_path: dest_path.clone(),
                    files_present: 0,
                    logical_bytes: 0,
                    synced_at: None,
                });
            }
            state.outstanding.push(Outstanding {
                directive: directive.clone(),
                location_id,
            });
            Ok(())
        })?;

        self.dispatch(directive)?;
        self.finish(org, root_id)
    }

    /// Re-measure a root's logical bytes from its authoritative repo.
    pub fn refresh_usage(&self, org: &str, root_id: Uuid) -> Result<RootPlacement> {
        let placement = self.placement(org, root_id)?;
        let Some(live_tree) = placement.live_tree.clone() else {
            return Err(Error::BadRequest(format!(
                "root {root_id} has no live tree to measure"
            )));
        };
        let location = self.registry.require_location(live_tree.location_id)?;
        let directive = AgentDirective {
            id: Uuid::new_v4(),
            agent_id: location.agent_id,
            kind: DirectiveKind::MeasureLiveTree {
                root_id,
                org: org.to_string(),
                live_tree_path: live_tree.absolute_path.clone(),
            },
        };
        self.registry.write(|state| {
            state.outstanding.push(Outstanding {
                directive: directive.clone(),
                location_id: live_tree.location_id,
            });
            Ok(())
        })?;
        self.dispatch(directive)?;
        self.finish(org, root_id)
    }

    // ── Directive plumbing ──────────────────────────────────────────

    /// Publish a directive to the agent lane and, when its agent lives in
    /// this process, run it inline and apply the outcome. The publish
    /// happens either way: it is how a remote agent (and any observer)
    /// learns of the work.
    fn dispatch(&self, directive: AgentDirective) -> Result<()> {
        self.directives.publish(directive.clone());
        if let Some(agent) = self.local_agent(directive.agent_id) {
            let outcome = agent.execute(&directive);
            self.complete_directive(directive.agent_id, directive.id, outcome)?;
        }
        Ok(())
    }

    /// Apply a finished directive's outcome — the one place a placement
    /// moves forward, whether the agent was local or remote.
    pub fn complete_directive(
        &self,
        agent_id: Uuid,
        directive_id: Uuid,
        outcome: DirectiveOutcome,
    ) -> Result<()> {
        let placement = self.registry.write(|state| {
            let Some(index) = state
                .outstanding
                .iter()
                .position(|o| o.directive.id == directive_id)
            else {
                return Err(Error::NotFound(format!("directive {directive_id}")));
            };
            if state.outstanding[index].directive.agent_id != agent_id {
                return Err(Error::BadRequest(format!(
                    "directive {directive_id} belongs to another agent"
                )));
            }
            let outstanding = state.outstanding.remove(index);
            let (root_id, org) = match &outstanding.directive.kind {
                DirectiveKind::HostLiveTree { root_id, org, .. }
                | DirectiveKind::ReplicateBlobs { root_id, org, .. }
                | DirectiveKind::MeasureLiveTree { root_id, org, .. } => (*root_id, org.clone()),
            };
            let location_id = outstanding.location_id;
            let Some(placement) = state.placement_mut(root_id) else {
                return Err(Error::NotFound(format!("placement for root {root_id}")));
            };
            match outcome {
                DirectiveOutcome::Hosted { repo_initialized } => {
                    if let Some(live_tree) = placement.live_tree.as_mut() {
                        live_tree.repo_initialized = repo_initialized;
                    }
                    placement.status = PlacementStatus::Hosted;
                    placement.failure = None;
                }
                DirectiveOutcome::Measured { logical_bytes, .. } => {
                    placement.logical_bytes = logical_bytes;
                }
                DirectiveOutcome::Replicated {
                    files_present,
                    logical_bytes,
                } => {
                    if let Some(replica) = placement
                        .replicas
                        .iter_mut()
                        .find(|r| r.location_id == location_id)
                    {
                        replica.files_present = files_present;
                        replica.logical_bytes = logical_bytes;
                        replica.synced_at = Some(Utc::now());
                    }
                }
                DirectiveOutcome::Failed { reason } => {
                    placement.status = PlacementStatus::Failed;
                    placement.failure = Some(reason);
                }
            }
            Ok((org, placement.clone()))
        })?;
        let (org, placement) = placement;
        self.publish(&org, StorageEvent::PlacementChanged(placement));
        Ok(())
    }

    /// Read a placement back and announce it — every mutating org-lane
    /// call ends here so subscribers see exactly what the caller got.
    fn finish(&self, org: &str, root_id: Uuid) -> Result<RootPlacement> {
        let placement = self.placement(org, root_id)?;
        self.publish(org, StorageEvent::PlacementChanged(placement.clone()));
        Ok(placement)
    }

    fn fail_placement(&self, _org: &str, root_id: Uuid, reason: &str) -> Result<()> {
        self.registry.write(|state| {
            if let Some(placement) = state.placement_mut(root_id) {
                placement.status = PlacementStatus::Failed;
                placement.failure = Some(reason.to_string());
            }
            Ok(())
        })
    }

    // ── Rule helpers ────────────────────────────────────────────────

    /// The grant admitting `org` onto `location_id`. A location the org
    /// holds no grant on is reported as ungranted whether or not it
    /// exists — an org lane never learns the deployment's registry.
    fn require_grant(&self, org: &str, location_id: Uuid) -> Result<StorageGrantInfo> {
        self.registry
            .read(|s| s.grant(org, location_id).cloned())
            .map(|g| self.with_usage(g))
            .ok_or_else(|| {
                Error::NotGranted(format!(
                    "org {org} holds no grant on location {location_id}"
                ))
            })
    }

    fn require_capability(
        &self,
        grant: &StorageGrantInfo,
        class: CapabilityClass,
        verb: &str,
    ) -> Result<()> {
        if grant.capabilities.contains(&class) {
            return Ok(());
        }
        Err(Error::CapabilityDenied(format!(
            "grant {} does not carry {class:?}, so it may not {verb}",
            grant.id
        )))
    }

    fn require_online(&self, location_id: Uuid) -> Result<StorageLocationInfo> {
        let location = self.registry.require_location(location_id)?;
        if location.health == LocationHealth::Online {
            return Ok(location);
        }
        Err(Error::BadRequest(format!(
            "location {location_id} is {:?}; placement waits for it to come back",
            location.health
        )))
    }

    /// A grant with no headroom left takes no new placements. Bytes
    /// already placed are never retro-actively evicted — the quota gates
    /// growth (issue #230: "placement never bypasses the grant/quota
    /// check").
    fn require_headroom(&self, org: &str, grant: &StorageGrantInfo) -> Result<()> {
        let (used, _) = self.registry.read(|s| usage_of(s, org, grant.location_id));
        if used >= grant.quota_bytes {
            return Err(Error::QuotaExceeded(format!(
                "org {org} has used {used} of {} logical bytes on location {}",
                grant.quota_bytes, grant.location_id
            )));
        }
        Ok(())
    }

    /// Fill a grant's derived `used_bytes`. Usage is always computed from
    /// placements rather than stored, so a counter can never drift from
    /// what is actually placed.
    fn with_usage(&self, mut grant: StorageGrantInfo) -> StorageGrantInfo {
        let (used, _) = self
            .registry
            .read(|s| usage_of(s, &grant.org, grant.location_id));
        grant.used_bytes = used;
        grant
    }

    fn orgs_granted_on(&self, location_id: Uuid) -> Vec<String> {
        self.registry.read(|s| {
            s.grants
                .iter()
                .filter(|g| g.location_id == location_id)
                .map(|g| g.org.clone())
                .collect()
        })
    }
}

/// Logical bytes an org has on one location, plus how many placements
/// (live trees + replicas) make them up.
fn usage_of(state: &State, org: &str, location_id: Uuid) -> (u64, u32) {
    let mut used = 0u64;
    let mut count = 0u32;
    for placement in state.placements.iter().filter(|p| p.org == org) {
        if placement
            .live_tree
            .as_ref()
            .is_some_and(|lt| lt.location_id == location_id)
        {
            used = used.saturating_add(placement.logical_bytes);
            count += 1;
        }
        for replica in placement
            .replicas
            .iter()
            .filter(|r| r.location_id == location_id)
        {
            used = used.saturating_add(replica.logical_bytes);
            count += 1;
        }
    }
    (used, count)
}

fn new_location(agent_id: Uuid, volume: &AnnouncedVolume) -> StorageLocationInfo {
    StorageLocationInfo {
        id: Uuid::new_v4(),
        name: volume.name.clone(),
        kind: volume.kind,
        agent_id,
        volume_key: volume.key.clone(),
        root_path: volume.root_path.clone(),
        capabilities: volume.capabilities.clone(),
        capacity_bytes: volume.capacity_bytes,
        health: LocationHealth::Online,
        registered_at: Utc::now(),
    }
}

/// The in-server agent's own announcement — the server speaking for its
/// own volumes. Approval still runs (the operator decides what the
/// deployment offers), but nothing about it is remote.
#[must_use]
pub fn in_server_announcement(
    agent_id: Uuid,
    label: impl Into<String>,
    volumes: Vec<AnnouncedVolume>,
) -> AgentAnnouncement {
    AgentAnnouncement {
        agent_id,
        hosting: AgentHosting::InServer,
        label: label.into(),
        volumes,
    }
}

/// Convenience for the common in-server volume: a POSIX directory that
/// can do both capability classes.
#[must_use]
pub fn server_volume(
    key: impl Into<String>,
    name: impl Into<String>,
    root_path: &Path,
) -> AnnouncedVolume {
    AnnouncedVolume {
        key: key.into(),
        name: name.into(),
        kind: files_storage_proto::LocationKind::ServerVolume,
        root_path: root_path.to_string_lossy().into_owned(),
        capabilities: vec![CapabilityClass::LiveTrees, CapabilityClass::Blobs],
        capacity_bytes: None,
    }
}

/// Where the deployment's registry lives under a data root.
#[must_use]
pub fn registry_dir(data_root: &Path) -> PathBuf {
    data_root.join("storage")
}
