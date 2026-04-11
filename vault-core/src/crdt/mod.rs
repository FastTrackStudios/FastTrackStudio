//! CRDT layer — conflict-free replicated data types for real-time collaboration.
//!
//! Each `.md` file is represented as two CRDT documents:
//!
//! - **Metadata (Automerge)** — YAML frontmatter fields. Each field is
//!   independently mergeable. Two people can change `status` and `assignee`
//!   concurrently and both edits are preserved.
//!
//! - **Body (Yrs)** — Markdown content after `---`. Handles concurrent
//!   text insertions/deletions. Two people can check off different subtasks
//!   simultaneously.
//!
//! The CRDT state is the real-time in-memory representation. It persists
//! to `.md` files (debounced) and syncs over WebSocket.

pub mod metadata;
pub mod body;
pub mod document;

pub use document::CrdtDocument;
