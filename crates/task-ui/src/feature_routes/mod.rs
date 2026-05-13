//! Per-feature route components. Each module wires its feature's
//! `RepoLoro` against a shared `CrdtDoc` synced over WebSocket and
//! drives the dumb components from the matching `<name>-ui` crate.

pub mod agent;
pub mod agent_chat;
pub mod calendar;
pub mod chat;
pub mod conference;
pub mod cookbook;
pub mod email;
pub mod finance;
pub mod fitness;
pub mod inventory;
pub mod invoice;
pub mod location;
pub mod person;
pub mod project;
pub mod threads;
pub mod timer;
