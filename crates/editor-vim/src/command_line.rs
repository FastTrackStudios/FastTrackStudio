//! `:` command-line mode. Stub for v1 — the dispatcher routes to
//! here when `mode == Command` but the body is empty. A real
//! impl would buffer typed chars in `VimState` and execute on
//! `<Enter>`.
//!
//! vim ref: codemirror-vim/src/vim.js (`exCommandDispatcher`)

use editor_state::{EditorState, KeySpec, TransactionSpec};

use crate::state::{Mode, VimState};

pub(crate) fn dispatch(
    _state: &EditorState,
    vim: &mut VimState,
    key: &KeySpec,
) -> Option<TransactionSpec> {
    if key.key == "Escape" {
        vim.mode = Mode::Normal;
        vim.clear_pending();
    }
    None
}
