//! `agent-proto` — wire contract for the agent feature.
//!
//! Task's port of the LLM-agent integration patterns from
//! [`hermes-webui`](https://github.com/nesquena/hermes-webui)
//! (in-process Hermes) and
//! [`CodexMonitor`](https://github.com/Dimillian/CodexMonitor)
//! (external Codex CLI monitor). The trait deliberately
//! abstracts over both shapes so a single Task UI can chat
//! with whatever backends are configured.
//!
//! ## Two backend shapes
//!
//! - **In-process** — the backend embeds an agent runtime
//!   (e.g. Hermes Rust SDK, an OpenAI-API client) and answers
//!   `dispatch_turn` synchronously by spinning up a worker
//!   that emits [`event::AgentEvent`]s.
//! - **External-monitor** — the backend watches an external
//!   CLI's on-disk session logs (`~/.codex/`, `~/.claude/`,
//!   etc.) and translates them into the same
//!   [`event::AgentEvent`] stream. `dispatch_turn` may be
//!   read-only or shell out to the CLI's IPC channel.
//!
//! ## Modules
//!
//! - [`backend`] — `AgentBackend`, `BackendKind`, configuration.
//! - [`profile`] — named agent identities (model, system
//!   prompt, MCP servers, toolsets). Mirrors Hermes's
//!   `~/.hermes/profiles/<name>/`.
//! - [`project`] — workspace roots; sessions belong to projects.
//! - [`session`] — one conversation; carries messages, tool
//!   calls, compression state, source tagging, worktree
//!   metadata.
//! - [`message`] — turn in a session; role + content blocks
//!   (multimodal: text, image, tool_use, tool_result).
//! - [`tool`] — tool call shape (independent of message so
//!   external monitors can stream tool events distinctly).
//! - [`reasoning`] — extended-thinking blocks.
//! - [`attachment`] — file / image attachments on a message.
//! - [`approval`] — agent-initiated permission requests.
//! - [`question`] — structured multi-choice questions (matches
//!   CodexMonitor's `RequestUserInputParams`).
//! - [`kanban`] — boards, cards, columns, comments, links.
//!   Cards optionally link to sessions.
//! - [`event`] — streaming `AgentEvent` union.
//! - [`error`] — `AgentError`.
//! - [`paths`] — disk layout for backends.
//!
//! ## The trait
//!
//! [`service::AgentService`] is decorated with
//! `#[architect::rpc]`. The default `vox` feature emits an
//! async client + dispatcher for remote callers; in-process
//! callers use the sync trait directly.

pub mod approval;
pub mod attachment;
pub mod backend;
pub mod error;
pub mod event;
pub mod kanban;
pub mod message;
pub mod paths;
pub mod profile;
pub mod project;
pub mod question;
pub mod reasoning;
pub mod service;
pub mod session;
pub mod tool;

pub use error::AgentError;
pub use event::AgentEvent;
pub use service::{AgentService, AgentServiceRpc};

#[cfg(feature = "vox")]
pub use service::{
    AgentServiceClient, AgentServiceRpcDispatcher as Dispatcher, Service,
    agent_service_rpc_service_descriptor as descriptor, layer, serve,
};
