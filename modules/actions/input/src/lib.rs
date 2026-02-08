//! Universal input processing with trie-based key dispatch and modal editing.
//!
//! This crate provides the core input processing pipeline: key chord matching,
//! modal editing with a mode stack, multi-key sequences via a trie, and
//! operator+motion composition.

#![deny(unsafe_code)]

pub mod command;
pub mod context;
pub mod event;
pub mod key;
pub mod mode;
pub mod processor;
pub mod trie;

pub use command::{ActionId, InputArgs, InputCommand};
pub use context::ActionContext;
pub use event::{InputEvent, KeyEvent, MouseAction, MouseButton, MouseEvent, ScrollEvent};
pub use key::{KeyChord, KeyCode, Modifiers};
pub use mode::{ModeDefinition, ModeId, ModeStack};
pub use processor::InputProcessor;
pub use trie::{KeyTrie, TrieLookup};
