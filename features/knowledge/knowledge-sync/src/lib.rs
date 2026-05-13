//! Read-only Obsidian-vault importer. Walks a directory, parses
//! markdown / canvas / base files via [`knowledge_proto::obsidian`]
//! helpers, and produces in-memory entities suitable for inserting
//! into the Loro repos.
//!
//! Write-back to disk is FUTURE — v1 ships read-only so we don't risk
//! corrupting user files. Logseq vaults are detected via
//! `logseq/config.edn` or `id::` block properties and get their
//! `((uuid))` block refs translated through
//! [`knowledge_proto::obsidian::translate_logseq_block_refs`].

#![cfg(not(target_arch = "wasm32"))]

pub mod import;
pub mod scan;

pub use import::{ImportError, ImportOptions, ImportResult, import_vault};
pub use scan::{ScannedFile, ScannedFileKind, scan_vault};
