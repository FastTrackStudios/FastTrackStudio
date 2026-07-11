//! `store-proto` — general app-state persistence contract.
//!
//! Two capability sub-traits, each `#[architect::rpc]`-decorated
//! (so each gets its own auto-emitted async client / dispatcher /
//! descriptor). Same shape as `wiki-proto::service::*` — no
//! umbrella trait; callers express what they need via bounds:
//!
//! ```ignore
//! fn sync_loop<S: KvStore + LogStore>(store: &S) { /* ... */ }
//! fn cache_get<S: KvStore>(store: &S, ns: &str, k: &str) { /* ... */ }
//! ```
//!
//! Backends mix + match:
//! - `MemStore` (in this crate) — impls both, for tests + the
//!   default demo route. Wasm-clean.
//! - `store-json` — JSON-on-disk, native. Impls both.
//! - `store-sqlite` — `SQLite`, native. Impls both with indexed
//!   audit-log queries.
//!
//! Features that need app state hold a `Box<dyn KvStore>` (and
//! optionally a `Box<dyn LogStore>`) and choose the backend at
//! mount time. The proto is feature-agnostic — `scheduling`,
//! `email`, `wiki`, any of them, reach for the same trait pair.

pub mod error;
pub mod kv_store;
pub mod log_store;
pub mod mem;
pub mod types;

pub use error::StoreError;
pub use kv_store::KvStore;
pub use log_store::LogStore;
pub use mem::MemStore;
pub use types::{LogEntry, Namespace};
