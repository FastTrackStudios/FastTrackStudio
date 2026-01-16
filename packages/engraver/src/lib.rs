//! # Engraver
//!
//! A WGPU-based music notation renderer and editor.
//!
//! This crate provides:
//! - Score data model for music notation
//! - Layout engine for music engraving
//! - Scene graph for efficient rendering and hit testing
//! - WGPU/Vello renderer for GPU-accelerated vector graphics
//! - Interaction layer for editing operations
//! - SMuFL font support
//!
//! ## Architecture
//!
//! ```text
//! ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌─────────────┐
//! │  Model   │→ │  Layout  │→ │  Scene   │→ │  Renderer   │
//! │ (Score)  │  │ (Engine) │  │  (Graph) │  │   (Vello)   │
//! └──────────┘  └──────────┘  └──────────┘  └─────────────┘
//! ```

// region:    --- Modules

pub mod derive_aliases;
pub mod error;
pub mod export;
pub mod fonts;
#[cfg(feature = "keyflow-import")]
pub mod import;
pub mod interaction;
pub mod layout;
pub mod model;
pub mod notation;
pub mod renderer;
pub mod scene;
pub mod style;
pub mod ui;

// endregion: --- Modules

// region:    --- Re-exports

// Error types
pub use error::{Error, Result};

// Model types
pub use model::{Measure, MusicElement, Part, Score, Voice};

// Style types
pub use style::{MStyle, Sid, StyleValue};

// endregion: --- Re-exports
