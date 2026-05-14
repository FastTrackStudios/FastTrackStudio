//! Per-feature route components. Each module wires its feature's
//! `RepoLoro` against a shared `CrdtDoc` synced over WebSocket and
//! drives the dumb components from the matching `<name>-ui` crate.

pub mod knowledge;
pub mod project;
