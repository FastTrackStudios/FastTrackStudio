//! Hermes-agent integration plugin.
//!
//! Server-only. Implements [`agent_proto::integration::AgentIntegration`]
//! against the Hermes dashboard's HTTP + WebSocket API. Also exposes a
//! deterministic [`MockIntegration`] for demos and integration tests
//! that need to exercise the plugin trait without a live Hermes
//! instance.
//!
//! ## Wire protocol assumptions
//!
//! Hermes' "kanban" plugin exposes the following endpoints (taken
//! from the integration research spec):
//!
//! - `POST   /api/plugins/kanban/tasks`           — create
//! - `POST   /api/plugins/kanban/dispatch`        — nudge dispatcher
//! - `GET    /api/plugins/kanban/tasks/{id}`      — fetch task + events
//! - `PATCH  /api/plugins/kanban/tasks/{id}`      — mutate status/etc
//! - `POST   /api/plugins/kanban/tasks/{id}/reclaim`
//! - `GET    /api/plugins/kanban/tasks/{id}/log?tail=`
//! - `WS     /api/plugins/kanban/events?token=&since=&board=`
//!
//! Auth is dual-mode: both `Authorization: Bearer …` and
//! `Cookie: session_token=…` are accepted server-side. We send both
//! on every request for cross-version compatibility.

pub mod client;
pub mod config;
pub mod hermes;
pub mod hermes_chat;
pub mod mock;
pub mod mock_chat;

pub use config::HermesConfig;
pub use hermes::HermesIntegration;
pub use hermes_chat::HermesChatModel;
pub use mock::MockIntegration;
pub use mock_chat::MockChatModel;
