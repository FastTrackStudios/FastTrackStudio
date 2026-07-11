//! Insert-mode entry — `i`, `a`, `I`, `A` (and `o`/`O` are handled
//! in `blocks.rs` since they create new blocks).

use crate::action::{InsertEntry, VimAction};
use crate::engine::VimMachine;
use crate::mode::VimMode;

use super::{bind, ch};

pub fn register(m: &mut VimMachine) {
    bind(
        m,
        VimMode::Normal,
        &[ch('i')],
        (
            Some(VimAction::EnterInsert(InsertEntry::BeforeCursor)),
            Some(VimMode::Insert),
        ),
    );
    bind(
        m,
        VimMode::Normal,
        &[ch('a')],
        (
            Some(VimAction::EnterInsert(InsertEntry::AfterCursor)),
            Some(VimMode::Insert),
        ),
    );
    bind(
        m,
        VimMode::Normal,
        &[ch('I')],
        (
            Some(VimAction::EnterInsert(InsertEntry::LineStart)),
            Some(VimMode::Insert),
        ),
    );
    bind(
        m,
        VimMode::Normal,
        &[ch('A')],
        (
            Some(VimAction::EnterInsert(InsertEntry::LineEnd)),
            Some(VimMode::Insert),
        ),
    );
}
