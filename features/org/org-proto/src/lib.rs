//! Federated org-root resolver.
//!
//! An [`OrgRoot`] is one self-contained federated node on
//! disk: `<data_root>/orgs/<slug>/`. Inside it sit the org's
//! `org.toml` manifest, its `auth.sqlite` / `timer.sqlite` /
//! `finance.sqlite` databases, the vault, and any other
//! org-scoped state. See `plans/federated-task-platform.md`
//! for the full federation model.
//!
//! This crate owns the **layout** — path resolvers, manifest
//! read/write, directory discovery — and nothing else. No
//! database, no server, no CLI. Downstream crates (server,
//! CLI) use [`OrgRoot`] to find where their files belong.

pub mod manifest;
pub mod root;

pub use manifest::{OrgManifest, ParseError};
pub use root::{DataRoot, OrgRoot, RootError, default_client_vault_root};
