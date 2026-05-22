//! On-disk Maildir + SQLite FTS5 index. Source of truth for
//! cached mail. Scaffold for phase 1.
//!
//! Layout (per account):
//! ```text
//! <root>/<account-id>/
//!   index.db               SQLite (envelopes, threads, FTS5)
//!   INBOX/{cur,new,tmp}/   Maildir per folder
//!   Sent/{cur,new,tmp}/
//!   ...
//! ```
//!
//! The index is disposable — it can be rebuilt by walking the
//! maildir. The maildir is canonical. Same shape as Task's
//! markdown-first + index-cache philosophy.

#![cfg(not(target_arch = "wasm32"))]

pub mod schema;
