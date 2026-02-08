//! Universal input processing with trie-based key dispatch and modal editing.
//!
//! This crate provides the core input processing pipeline: key chord matching,
//! modal editing with a mode stack, multi-key sequences via a trie, and
//! operator+motion composition.

#![deny(unsafe_code)]

pub mod command;
pub mod event;
pub mod key;
pub mod mode;

pub use command::{InputArgs, InputCommand};
pub use event::{InputEvent, KeyEvent, MouseButton, MouseAction, MouseEvent, ScrollEvent};
pub use key::KeyChord;
pub use mode::{ModeDefinition, ModeId, ModeStack};
