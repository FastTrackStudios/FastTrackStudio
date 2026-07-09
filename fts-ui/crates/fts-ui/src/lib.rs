//! FastTrack Studio design system — facade crate.
//!
//! `fts-ui` itself contains no components. It re-exports the capability crates
//! under `features/`, each behind a Cargo feature. The default `core` feature
//! is re-exported flat so existing imports keep working:
//!
//! ```rust,ignore
//! use fts_ui::prelude::*;        // core (standard) components, layout, theme
//! ```
//!
//! # Features
//! * `core` (default) — the standard shadcn-style design system ([`fts_ui_core`]).
//!   Re-exported flat at the crate root.
//! * `router` — forwards `fts-ui-core`'s router feature.

// Core is the always-on baseline: re-export it flat so `fts_ui::prelude`,
// `fts_ui::components`, `fts_ui::theme`, … resolve exactly as before the split.
#[cfg(feature = "core")]
#[doc(inline)]
pub use fts_ui_core::*;
