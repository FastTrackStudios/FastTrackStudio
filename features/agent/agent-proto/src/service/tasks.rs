//! Agent-task queues. Modeled on hermes-webui's kanban bridge
//! but renamed visualization-neutral — see `tasks.rs` for the
//! domain types. Server-side dispatcher exposes this trait over
//! vox; clients consume it through the generated RPC client.

use crate::error::AgentError;
use crate::tasks::{
    AgentTask, AgentTaskComment, AgentTaskLink, AgentTaskLinkKind, AgentTaskStatus, Queue,
    QueueFilter, QueueSnapshot,
};

#[architect::rpc]
pub trait AgentTaskQueue {
    // Queues ----------------------------------------------------
    fn list_queues(&self) -> Result<Vec<Queue>, AgentError>;
    fn upsert_queue(&self, queue: Queue) -> Result<Queue, AgentError>;
    fn remove_queue(&self, queue_id: &str) -> Result<(), AgentError>;
    fn read_queue(&self, queue_id: &str, filter: QueueFilter) -> Result<QueueSnapshot, AgentError>;

    // Agent tasks -----------------------------------------------
    fn upsert_agent_task(&self, task: AgentTask) -> Result<AgentTask, AgentError>;
    fn remove_agent_task(&self, agent_task_id: &str) -> Result<(), AgentError>;
    fn read_agent_task(&self, agent_task_id: &str) -> Result<AgentTask, AgentError>;
    /// Atomic claim. Flips status to `Running` if the task is
    /// free; returns `AgentError::Conflict` if already claimed.
    /// The only sanctioned path into `Running`.
    fn claim_agent_task(&self, agent_task_id: &str, handle: &str) -> Result<AgentTask, AgentError>;
    /// Set a non-`Running` status. Returns `AgentError::Invalid`
    /// if `new_status == Running` (use `claim_agent_task`).
    fn set_agent_task_status(
        &self,
        agent_task_id: &str,
        new_status: AgentTaskStatus,
    ) -> Result<AgentTask, AgentError>;
    fn link_agent_task_to_session(
        &self,
        agent_task_id: &str,
        session_id: &str,
    ) -> Result<AgentTask, AgentError>;
    /// Mark an agent task done with a result blob. Convenience
    /// over `set_agent_task_status` + a separate result write.
    fn complete_agent_task(
        &self,
        agent_task_id: &str,
        result_blob: &str,
    ) -> Result<AgentTask, AgentError>;

    // Links + comments -----------------------------------------
    fn add_agent_task_link(
        &self,
        from_task: &str,
        to_task: &str,
        kind: AgentTaskLinkKind,
    ) -> Result<AgentTaskLink, AgentError>;
    fn remove_agent_task_link(
        &self,
        from_task: &str,
        to_task: &str,
        kind: AgentTaskLinkKind,
    ) -> Result<(), AgentError>;
    fn list_agent_task_links(&self, queue_id: &str) -> Result<Vec<AgentTaskLink>, AgentError>;

    fn add_agent_task_comment(
        &self,
        agent_task_id: &str,
        author: &str,
        body: &str,
    ) -> Result<AgentTaskComment, AgentError>;
    fn list_agent_task_comments(
        &self,
        agent_task_id: &str,
    ) -> Result<Vec<AgentTaskComment>, AgentError>;
}
