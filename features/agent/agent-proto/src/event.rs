//! Streaming event union. Backends emit these from
//! [`crate::service::Agents::subscribe_session`] (and
//! the board / global subscriptions). Mirrors Hermes's SSE
//! event types verbatim where it matters; CodexMonitor's
//! `AppServerEvent` payloads translate into the same union.
//!
//! The variants are intentionally fine-grained — granular
//! events make it easy for UIs to render partial state
//! without re-pulling the whole session.

use chrono::{DateTime, Utc};
use facet::Facet;

use crate::{
    approval::Approval, kanban::Card, message::Message, question::QuestionRequest, tool::ToolCall,
};

#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(C)]
pub enum AgentEvent {
    /// Backend accepted a new turn and started a worker.
    /// Carries the stream id clients can use to subscribe.
    TurnStarted {
        session_id: String,
        stream_id: String,
        at: DateTime<Utc>,
    },
    /// One whole message landed (typically the user
    /// message at turn start).
    MessageWritten { message: Message },
    /// Streaming partial token — text body grew.
    /// `content_delta` is appended to the message's
    /// current text-content block.
    MessageDelta {
        session_id: String,
        message_id: String,
        content_delta: String,
    },
    /// Reasoning content streamed in.
    ReasoningDelta {
        session_id: String,
        message_id: String,
        delta: String,
    },
    /// A tool call was initiated.
    ToolStarted { tool_call: ToolCall },
    /// Tool produced an intermediate update (output line,
    /// progress percent).
    ToolProgress {
        tool_call_id: String,
        preview: String,
        /// 0.0–1.0; `-1.0` for indeterminate.
        progress: f32,
    },
    /// Tool finished (with success or error).
    ToolFinished { tool_call: ToolCall },
    /// Backend wants user approval for something.
    ApprovalRequested { approval: Approval },
    /// User answered an approval. Mirrored back on the
    /// stream for any other subscribers.
    ApprovalResolved { approval: Approval },
    /// Backend wants the user to answer a structured
    /// question.
    QuestionAsked { request: QuestionRequest },
    /// Question was answered.
    QuestionResolved { request: QuestionRequest },
    /// Context compression started — older messages are
    /// being summarized into the anchor.
    CompressionStarted { session_id: String, engine: String },
    /// Compression done; new anchor visible at
    /// `anchor_idx`.
    CompressionFinished { session_id: String, anchor_idx: u32 },
    /// Usage / metering tick. Emitted periodically while
    /// a turn runs.
    Metering {
        session_id: String,
        input_tokens: u64,
        output_tokens: u64,
        estimated_cost_usd: f32,
    },
    /// Backend status hint (`"falling back to model X"`,
    /// `"queued behind 3 other turns"`).
    Warning {
        session_id: String,
        kind: String,
        message: String,
    },
    /// Turn finished cleanly.
    TurnFinished {
        session_id: String,
        message_id: String,
        at: DateTime<Utc>,
    },
    /// Turn errored.
    TurnErrored {
        session_id: String,
        kind: String,
        message: String,
        at: DateTime<Utc>,
    },
    /// User cancelled mid-flight.
    TurnCancelled {
        session_id: String,
        at: DateTime<Utc>,
    },
    /// A kanban card changed (created, moved, claimed,
    /// archived). Emitted on board-scoped subscriptions.
    CardChanged { card: Card },
    /// Generic resync signal — subscribers re-pull state.
    Resync,
}
