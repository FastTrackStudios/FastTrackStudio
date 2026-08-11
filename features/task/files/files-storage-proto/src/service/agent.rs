//! The storage-agent protocol (glossary "Storage agent"): one protocol,
//! three hostings. An agent announces its volumes, heartbeats their
//! health, subscribes to the directive stream, and reports each
//! directive's outcome. The in-server hosting speaks this same protocol
//! in-process; a desktop or standalone agent speaks it over vox.
//!
//! The coordinator plans and the agent transfers — the coordinator is
//! never itself the data path (issue #230), which is why directives carry
//! paths rather than bytes.

use uuid::Uuid;

use crate::error::StorageError;
use crate::model::{AgentAnnouncement, AgentDirective, AgentInfo, DirectiveOutcome, VolumeHealth};

#[architect::rpc]
pub trait StorageAgentService {
    /// Announce (or re-announce) this agent and its volumes. A new agent
    /// lands [`crate::model::AgentStatus::Pending`] — the operator
    /// approves before any of its volumes becomes a location.
    /// Re-announcing never resets an existing approval.
    async fn announce(&self, announcement: AgentAnnouncement) -> Result<AgentInfo, StorageError>;

    /// Report liveness plus per-volume health. `Offline` /
    /// `ExpectedOffline` propagate to the volume's registered location
    /// (a removable drive being unplugged is health, not error).
    async fn heartbeat(
        &self,
        agent_id: Uuid,
        volumes: Vec<VolumeHealth>,
    ) -> Result<AgentInfo, StorageError>;

    /// Directives still outstanding for `agent_id` — the catch-up read an
    /// agent does on connect, before folding in the live stream.
    async fn pending_directives(&self, agent_id: Uuid)
    -> Result<Vec<AgentDirective>, StorageError>;

    /// Report a directive finished. This is what flips a placement from
    /// [`crate::model::PlacementStatus::Pending`] to `Hosted`, or records
    /// a replica's contents.
    async fn complete_directive(
        &self,
        agent_id: Uuid,
        directive_id: Uuid,
        outcome: DirectiveOutcome,
    ) -> Result<(), StorageError>;

    /// Every directive the coordinator issues. Directives carry their
    /// `agent_id`; a subscriber keeps its own and ignores the rest —
    /// the monorepo's `#[subscribe]`-stream idiom (root CLAUDE.md),
    /// not a per-agent channel.
    #[subscribe]
    fn directives(&self) -> AgentDirective;
}
