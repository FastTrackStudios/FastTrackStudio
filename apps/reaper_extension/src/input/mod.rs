//! FTS-Input: Key Sequence System
//!
//! Provides a vim-like key sequence system for REAPER, similar to reaper-keys.
//! Supports:
//! - Key sequence building (e.g., "tL" = play and loop next measure)
//! - Action composition (timeline_operator + timeline_motion)
//! - Multiple modes (normal, visual_timeline, visual_track)
//! - Context awareness (main, midi, global)
//! - Macro recording and playback

pub mod state;
pub mod bindings;
pub mod event_parser;
pub mod matcher;
pub mod executor;
pub mod handler;
pub mod actions;
pub mod wheel_hook;
pub mod mouse_util;
pub mod mouse_context;
pub mod midi_utils;
pub mod utils;
pub mod reaper_windows;

pub use handler::InputHandler;
pub use state::{CommandState, Mode, Context};
