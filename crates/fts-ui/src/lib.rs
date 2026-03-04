//! FastTrack Studio design system.
//!
//! Single opinionated UI dependency for all FTS apps. Wraps and re-exports
//! lumen-blocks components, adds layout/typography primitives, and provides
//! the canonical FTS theme tokens.
//!
//! # Quick start
//! ```rust,ignore
//! use fts_ui::prelude::*;
//! ```

pub mod components;
pub mod layout;
pub mod theme;
pub mod typography;

pub mod prelude;

// Re-export underlying crates for escape-hatch access.
pub use lumen_blocks;
pub use lucide_dioxus;
