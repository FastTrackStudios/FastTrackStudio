//! `AgentService` — the canonical sync trait. Decorated with
//! `#[architect::rpc]`; the vox feature emits an async
//! client + dispatcher for remote callers.
//!
//! Coverage:
//!
//! - Backend + profile + project management.
//! - Session lifecycle (create / list / read / archive / pin).
//! - Turn dispatch + cancellation.
//! - Approval + question resolution.
//! - Tool + reasoning + attachment read.
//! - Kanban board CRUD.
//! - Live event subscription.
//!
//! Backend authors implement as much as makes sense for
//! their shape: external-monitor backends typically reject
//! `dispatch_turn` with `AgentError::Unsupported` and rely on
//! the wrapped CLI for the actual turn. The trait stays
//! uniform; the difference is at the implementation layer.

use crate::{
    approval::{Approval, ApprovalDecision},
    attachment::{Attachment, AttachmentRef},
    backend::{AgentBackend, BackendHealth, BackendKind},
    error::AgentError,
    event::AgentEvent,
    kanban::{Board, BoardFilter, BoardView, Card, CardComment, CardLink},
    message::Message,
    profile::Profile,
    project::Project,
    question::{QuestionAnswer, QuestionRequest},
    reasoning::ReasoningBlock,
    session::Session,
    tool::ToolCall,
};
use chrono::{DateTime, Utc};
use vox::Tx;

/// Turn-dispatch arguments. Wrapped in a struct because the
/// architect macro's `Facet` requirement caps tuple args at
/// 4-arity.
#[derive(Debug, Clone, PartialEq, Eq, facet::Facet)]
#[repr(C)]
pub struct DispatchTurn {
    pub session_id: String,
    /// User message text (markdown / plain).
    pub text: String,
    /// Optional attachments. Referenced by id; bytes
    /// must already be in the attachment store.
    pub attachments: Vec<AttachmentRef>,
    /// Optional override profile id. Empty = use the
    /// session's locked profile.
    pub profile_override_id: String,
    /// Optional override personality id. Empty = the
    /// profile's active personality.
    pub personality_override_id: String,
    /// Optional model override (provider-namespaced).
    /// Empty = profile default.
    pub model_override: String,
}

#[derive(Debug, Clone, PartialEq, Eq, facet::Facet)]
#[repr(C)]
pub struct DispatchAck {
    pub session_id: String,
    pub stream_id: String,
    pub turn_id: u64,
    pub started_at: DateTime<Utc>,
    /// Effective model used (resolved from overrides +
    /// profile + backend defaults).
    pub effective_model: String,
    pub effective_backend_id: String,
    pub effective_profile_id: String,
}

#[derive(Debug, Clone, PartialEq, Eq, facet::Facet)]
#[repr(C)]
pub struct CreateSession {
    pub project_id: String,
    pub profile_id: String,
    /// Optional title. Empty ⇒ backend auto-generates.
    pub title: String,
    /// Override workspace path (defaults to project.path).
    pub workspace_path: String,
    /// Optional sub-agent nickname.
    pub subagent_nickname: String,
}

#[derive(Debug, Clone, PartialEq, Eq, facet::Facet)]
#[repr(C)]
pub struct SessionFilter {
    pub project_id: String,
    pub backend_id: String,
    pub profile_id: String,
    pub include_archived: bool,
    /// Filter on `Session::pinned`.
    pub only_pinned: bool,
    /// Result cap. `0` = backend default.
    pub limit: u32,
    /// Opaque pagination cursor.
    pub cursor: String,
}

#[derive(Debug, Clone, PartialEq, facet::Facet)]
#[repr(C)]
pub struct SessionPage {
    pub sessions: Vec<Session>,
    /// Cursor for the next page. Empty ⇒ exhausted.
    pub next_cursor: String,
    pub has_more: bool,
}

#[architect::rpc]
pub trait AgentService {
    // ───────────────────────── Backends ─────────────────────────

    /// Register a new backend (or update one matching `id`).
    fn upsert_backend(&self, backend: AgentBackend) -> Result<AgentBackend, AgentError>;

    /// Remove a backend. Sessions referencing it are kept;
    /// they become orphans until reassigned.
    fn remove_backend(&self, backend_id: &str) -> Result<(), AgentError>;

    /// List registered backends.
    fn list_backends(&self) -> Result<Vec<AgentBackend>, AgentError>;

    /// Probe a backend; returns latency + version.
    fn backend_health(&self, backend_id: &str) -> Result<BackendHealth, AgentError>;

    /// Filter backends by [`BackendKind`].
    fn backends_by_kind(&self, kind: BackendKind) -> Result<Vec<AgentBackend>, AgentError>;

    // ───────────────────────── Profiles ─────────────────────────

    fn upsert_profile(&self, profile: Profile) -> Result<Profile, AgentError>;
    fn remove_profile(&self, profile_id: &str) -> Result<(), AgentError>;
    fn list_profiles(&self) -> Result<Vec<Profile>, AgentError>;
    fn read_profile(&self, profile_id: &str) -> Result<Profile, AgentError>;

    // ───────────────────────── Projects ─────────────────────────

    fn upsert_project(&self, project: Project) -> Result<Project, AgentError>;
    fn remove_project(&self, project_id: &str) -> Result<(), AgentError>;
    fn list_projects(&self) -> Result<Vec<Project>, AgentError>;
    fn read_project(&self, project_id: &str) -> Result<Project, AgentError>;

    // ───────────────────────── Sessions ─────────────────────────

    fn create_session(&self, args: CreateSession) -> Result<Session, AgentError>;
    fn read_session(&self, session_id: &str) -> Result<Session, AgentError>;
    fn list_sessions(&self, filter: SessionFilter) -> Result<SessionPage, AgentError>;
    fn rename_session(&self, session_id: &str, title: &str) -> Result<Session, AgentError>;
    fn pin_session(&self, session_id: &str, pinned: bool) -> Result<Session, AgentError>;
    fn archive_session(&self, session_id: &str, archived: bool) -> Result<Session, AgentError>;
    fn delete_session(&self, session_id: &str) -> Result<(), AgentError>;
    /// Persist a composer draft. Auto-called as the user
    /// types. Returns the updated session.
    fn save_composer_draft(
        &self,
        session_id: &str,
        text: &str,
        attachments: Vec<AttachmentRef>,
    ) -> Result<Session, AgentError>;

    /// Import an external-CLI session log (Codex, Claude
    /// CLI) into Task. Backend reads the log, materializes
    /// a [`Session`] + its messages, and returns the new
    /// session. Idempotent: re-importing the same log
    /// returns the existing session id.
    fn import_external_session(
        &self,
        backend_id: &str,
        log_path: &str,
        project_id: &str,
    ) -> Result<Session, AgentError>;

    // ───────────────────────── Turn dispatch ─────────────────────────

    /// Kick off a new turn. Returns immediately; events
    /// flow over the session's subscription channel.
    fn dispatch_turn(&self, args: DispatchTurn) -> Result<DispatchAck, AgentError>;

    /// Cancel the in-flight turn (if any).
    fn cancel_turn(&self, session_id: &str) -> Result<(), AgentError>;

    /// Resume a session that crashed mid-turn (e.g. the
    /// backend died). The run journal lets the backend
    /// replay events the client missed; this method just
    /// kicks off the replay.
    fn resume_session(&self, session_id: &str) -> Result<DispatchAck, AgentError>;

    // ───────────────────────── Messages + tools ─────────────────────────

    fn list_messages(
        &self,
        session_id: &str,
        limit: u32,
        before_cursor: &str,
    ) -> Result<Vec<Message>, AgentError>;
    fn read_message(&self, message_id: &str) -> Result<Message, AgentError>;
    /// Append a free-form note to the session (system-role
    /// message). Useful for curator annotations that
    /// shouldn't go through the agent.
    fn append_note(&self, session_id: &str, text: &str) -> Result<Message, AgentError>;

    fn list_tool_calls(&self, session_id: &str) -> Result<Vec<ToolCall>, AgentError>;
    fn read_tool_call(&self, tool_call_id: &str) -> Result<ToolCall, AgentError>;

    fn read_reasoning(&self, message_id: &str) -> Result<Option<ReasoningBlock>, AgentError>;

    // ───────────────────────── Attachments ─────────────────────────

    fn upload_attachment(
        &self,
        name: &str,
        mime: &str,
        bytes: Vec<u8>,
    ) -> Result<AttachmentRef, AgentError>;
    fn read_attachment(&self, attachment_id: &str) -> Result<Attachment, AgentError>;
    fn list_attachments(&self, session_id: &str) -> Result<Vec<AttachmentRef>, AgentError>;

    // ───────────────────────── Approvals + questions ─────────────────────────

    fn list_pending_approvals(&self, session_id: &str) -> Result<Vec<Approval>, AgentError>;
    fn resolve_approval(
        &self,
        approval_id: &str,
        decision: ApprovalDecision,
    ) -> Result<Approval, AgentError>;

    fn list_pending_questions(&self, session_id: &str) -> Result<Vec<QuestionRequest>, AgentError>;
    fn answer_question(
        &self,
        request_id: &str,
        answers: Vec<QuestionAnswer>,
    ) -> Result<QuestionRequest, AgentError>;

    // ───────────────────────── Kanban ─────────────────────────

    fn list_boards(&self) -> Result<Vec<Board>, AgentError>;
    fn upsert_board(&self, board: Board) -> Result<Board, AgentError>;
    fn remove_board(&self, board_id: &str) -> Result<(), AgentError>;
    fn read_board(&self, board_id: &str, filter: BoardFilter) -> Result<BoardView, AgentError>;

    fn upsert_card(&self, card: Card) -> Result<Card, AgentError>;
    fn remove_card(&self, card_id: &str) -> Result<(), AgentError>;
    fn read_card(&self, card_id: &str) -> Result<Card, AgentError>;
    /// Claim a card — sets assignee + moves to the
    /// `running` column. Idempotent for the same handle.
    fn claim_card(&self, card_id: &str, handle: &str) -> Result<Card, AgentError>;
    /// Move a card to a different column.
    fn move_card(&self, card_id: &str, new_column: &str) -> Result<Card, AgentError>;
    /// Link a card to an agent session.
    fn link_card_to_session(&self, card_id: &str, session_id: &str) -> Result<Card, AgentError>;

    fn add_card_link(&self, parent_id: &str, child_id: &str) -> Result<CardLink, AgentError>;
    fn remove_card_link(&self, parent_id: &str, child_id: &str) -> Result<(), AgentError>;
    fn list_card_links(&self, board_id: &str) -> Result<Vec<CardLink>, AgentError>;

    fn add_card_comment(
        &self,
        card_id: &str,
        author: &str,
        text: &str,
    ) -> Result<CardComment, AgentError>;
    fn list_card_comments(&self, card_id: &str) -> Result<Vec<CardComment>, AgentError>;

    // ───────────────────────── Live events ─────────────────────────

    /// Subscribe to live events scoped to one session.
    /// The server keeps sending until the caller drops `tx`.
    /// On broadcast lag the server sends
    /// [`AgentEvent::Resync`] and continues — clients
    /// re-pull state in response.
    async fn subscribe_session(&self, session_id: String, tx: Tx<AgentEvent>);

    /// Subscribe to board-scoped events (card changed,
    /// claimed, archived). Same lag semantics as
    /// `subscribe_session`.
    async fn subscribe_board(&self, board_id: String, tx: Tx<AgentEvent>);

    /// Cross-session firehose — list-changed events,
    /// session created / archived / pinned. Used by
    /// sidebar UIs that need to stay live.
    async fn subscribe_global(&self, tx: Tx<AgentEvent>);
}
