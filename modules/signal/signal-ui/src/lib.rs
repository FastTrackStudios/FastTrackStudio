//! Signal2 UI crate.
//!
//! ## `components` -- domain-agnostic presentation
//!
//! Pure Dioxus building blocks (entity editor, star ratings, scene tiles,
//! morph slider, etc.) that take all data via props and have zero knowledge
//! of signal domain types.
//!
//! ## `views` -- domain-aware smart components
//!
//! Components that use [`signal::SignalController`] and signal domain types
//! to fetch data, manage state, and compose the dumb `components` into
//! full editor/browser views.

pub mod components;
pub mod views;

// Convenience re-exports
pub use views::SignalSlider;
