//! Input service trait.
//!
//! Raw input interception and streaming. The host captures
//! keyboard/mouse events from REAPER's TranslateAccel hook and
//! streams them to subscribing extensions. Extensions upload a
//! `KeyFilter` to control which keys are eaten vs passed through.
//!
//! Domain-agnostic — the host has no knowledge of keybindings, modal
//! editing, or command resolution.

use super::{InputEvent, KeyFilter};
use vox::{Tx, service};

#[service]
pub trait InputService {
    /// Subscribe to input events. Multiple subscribers supported.
    async fn subscribe_input(&self, tx: Tx<InputEvent>);

    /// Upload a key filter configuration. Replaces the previous one.
    async fn set_key_filter(&self, filter: KeyFilter);

    async fn get_key_filter(&self) -> KeyFilter;

    /// When disabled, TranslateAccel passes all keys through to REAPER.
    async fn set_enabled(&self, enabled: bool);
    async fn is_enabled(&self) -> bool;

    /// Execute a REAPER action by command name or numeric ID.
    ///
    /// Supports named commands (`"FTS_SIGNAL_NEXT_SONG"`), named with
    /// prefix (`"_SWS_ABOUT"`), or numeric IDs (`"40044"`).
    async fn execute_action(&self, action_id: String);
}
