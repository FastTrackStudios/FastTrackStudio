//! Architecture Playground — feature-isolated vertical slices
//!
//! ```text
//!   auth/       — authentication (better-auth → service → control → UI)
//!   items/      — item CRUD (rusqlite → repo → service → control → UI)
//! ```

pub mod auth;
pub mod context;
pub mod items;
pub mod ui_state;
