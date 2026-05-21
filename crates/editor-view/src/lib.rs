//! Dioxus view layer for the editor. Renders an
//! [`editor_state::EditorState`] into a contenteditable element
//! and translates user input into transactions.
//!
//! v1 scope: plain-text editing. Decorations come next.

pub use editor_state;

mod bridge;
mod editor;
pub mod slash;
pub mod tile;

pub use editor::{DecorationSource, Editor};
