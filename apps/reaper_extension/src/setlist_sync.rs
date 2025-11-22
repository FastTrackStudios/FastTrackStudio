//! Shared state for setlist synchronization

use std::sync::atomic::AtomicBool;

/// Flag to force an immediate setlist update on the next timer tick
/// Set to true when a new client connects to ensure they get the setlist immediately
pub static FORCE_SETLIST_UPDATE: AtomicBool = AtomicBool::new(false);

