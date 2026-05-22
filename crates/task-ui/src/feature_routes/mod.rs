//! Per-feature route components.
//!
//! The previous shape ("each module wires its feature's
//! `RepoLoro` against a shared `CrdtDoc` synced over WebSocket")
//! went away when the Loro entity layer was ripped. Routes now
//! drive plain components against the file-backed vault +
//! per-feature local state.

pub mod project;
pub mod servers;
pub mod view_gantt;
