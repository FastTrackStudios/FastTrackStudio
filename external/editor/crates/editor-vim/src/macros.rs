//! `.` repeat + (eventually) `q`/`@` macros. v1 only handles
//! operator-motion replay; insert-text replay is a follow-up.
//!
//! vim ref: codemirror-vim/src/vim.js (`vim.lastEditInputState`)
//! vim ref: zed/crates/vim/src/normal/repeat.rs

use editor_state::{EditorState, TransactionSpec};

use crate::motions;
use crate::operators;
use crate::state::{LastChange, VimState};
use crate::text_objects;

pub(crate) fn replay_last(state: &EditorState, vim: &mut VimState) -> Option<TransactionSpec> {
    let last = vim.last_change.clone()?;
    let caret = state.selection.primary().head;
    match last {
        LastChange::OperatorMotion { operator, motion, count } => {
            let to = motions::apply(state, motion, count);
            let (lo, hi) = if caret <= to { (caret, to) } else { (to, caret) };
            Some(operators::apply_range(state, vim, operator, lo, hi))
        }
        LastChange::OperatorTextObject { operator, object, around, .. } => {
            let r = text_objects::apply(state, object, around, caret);
            Some(operators::apply_range(state, vim, operator, r.start, r.end))
        }
        LastChange::Insert(_) => None, // v1: TODO
    }
}
