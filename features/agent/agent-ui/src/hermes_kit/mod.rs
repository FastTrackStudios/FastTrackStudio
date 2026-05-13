//! Hermes-agent + git integration UI components.
//!
//! Companion to the existing `AgentRunDashboard` family. These
//! components target the broader integration workflow: dispatching
//! runs from tasks, inspecting in-flight runs, browsing every run on
//! a board, configuring credentials, and debugging webhook deliveries.
//!
//! All components are dumb: they take data + emit events. The
//! `crates/task-ui` route layer owns the CRDT repos and wires
//! callbacks back into Loro writes.

pub mod common;
pub mod dispatch;
pub mod integrations_settings;
pub mod run_board;
pub mod run_panel;
pub mod webhook_log;

pub use common::{
    format_cost_cents, format_relative_time, format_tokens, log_level_class, run_status_variant,
};
pub use dispatch::{AgentDispatchDialog, DispatchIntent, IntegrationInfo, TaskBrief};
pub use integrations_settings::{
    ConfiguredIntegration, GitRepoDraft, HermesConfigDraft, IntegrationConfigStatus,
    IntegrationSettings,
};
pub use run_board::AgentRunBoard;
pub use run_panel::AgentRunPanel;
pub use webhook_log::{WebhookEventLog, WebhookFilter, WebhookInboxRow};
