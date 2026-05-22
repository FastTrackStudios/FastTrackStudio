//! Per-feature route components. Each module wires its feature's
//! `RepoLoro` against a shared `CrdtDoc` synced over WebSocket and
//! drives the dumb components from the matching `<name>-ui` crate.

pub mod federated_tasks;
pub mod project;
pub mod servers;
pub mod view_gantt;
pub mod view_kanban;
