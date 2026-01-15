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

pub mod export;
pub mod fonts;
pub mod import;
pub mod interaction;
pub mod layout;
pub mod model;
pub mod notation;
pub mod renderer;
pub mod scene;
pub mod style;
pub mod ui;

// Re-export main types
pub use model::{Measure, MusicElement, Part, Score, Voice};
pub use style::{MStyle, Sid, StyleValue};
