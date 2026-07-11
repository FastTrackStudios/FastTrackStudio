//! Modal editing modes for the vim engine.

use keybindings::{EmptyKeyState, Mode, ModeKeys};

use crate::action::VimAction;
use crate::key::DioxusKey;

/// The set of editing modes the vim engine recognizes.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum VimMode {
    /// Default. Keys are interpreted as commands; the buffer is
    /// not directly editable.
    #[default]
    Normal,

    /// Free-form text entry — a `<textarea>` is the focus and
    /// receives raw keystrokes.
    Insert,

    /// Selection extension. Motions extend the selection.
    Visual,
    /// Line-wise visual mode (`V`). Same anchor mechanism as
    /// `Visual` but selection is treated by the host as
    /// whole-block (or whole-line within a block) rather than
    /// per-character.
    VisualLine,
    /// Ex-style command line (`:`). The engine accumulates keys
    /// into [`VimEngine::command_buffer`] until Enter (submit)
    /// or Esc (cancel). Hosts render the buffer as a status-line
    /// echo so the user sees what they're typing.
    Command,
    /// Search input (`/`). Same lifecycle as Command but the
    /// buffer is held in [`VimEngine::search_buffer`] and survives
    /// submit so `n` / `N` can replay against it.
    Search,
}

impl Mode<VimAction, EmptyKeyState> for VimMode {
    fn show(&self, _: &EmptyKeyState) -> Option<String> {
        Some(
            match self {
                VimMode::Normal => "NORMAL",
                VimMode::Insert => "INSERT",
                VimMode::Visual => "VISUAL",
                VimMode::VisualLine => "V-LINE",
                VimMode::Command => "COMMAND",
                VimMode::Search => "SEARCH",
            }
            .to_string(),
        )
    }
}

impl ModeKeys<DioxusKey, VimAction, EmptyKeyState> for VimMode {
    /// Default behavior when no binding matched the current key.
    ///
    /// In Insert mode we don't intercept anything — the host
    /// editor receives the keystroke directly via Dioxus's normal
    /// event flow. In Normal/Visual we silently swallow unknown
    /// keys (no accidental text insertion).
    fn unmapped(
        &self,
        _key: &DioxusKey,
        _ctx: &mut EmptyKeyState,
    ) -> (Vec<VimAction>, Option<Self>) {
        (vec![], None)
    }
}

/// Convenience tuple type expected by `ModalMachine`.
pub type VimStep = (Option<VimAction>, Option<VimMode>);
