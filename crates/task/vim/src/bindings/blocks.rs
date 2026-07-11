//! Block-scope operations — operate on whole blocks rather than
//! characters. Mirrors plugin commands like `nextNewBlock`,
//! `deleteCurrentBlock`, `indent`/`outdent`.

use crate::action::{BlockOp, VimAction};
use crate::engine::VimMachine;
use crate::mode::VimMode;

use super::{bind, ch};

pub fn register(m: &mut VimMachine) {
    // o / O — new block + enter insert.
    bind(
        m,
        VimMode::Normal,
        &[ch('o')],
        (
            Some(VimAction::Block(BlockOp::NewBelow)),
            Some(VimMode::Insert),
        ),
    );
    bind(
        m,
        VimMode::Normal,
        &[ch('O')],
        (
            Some(VimAction::Block(BlockOp::NewAbove)),
            Some(VimMode::Insert),
        ),
    );

    // x — delete char at cursor (kept here with the other
    // single-key edits even though it's not a block op proper).
    bind(
        m,
        VimMode::Normal,
        &[ch('x')],
        (Some(VimAction::DeleteChar), None),
    );

    // <Space> — cycle TODO state on the current list_item block.
    // Logseq convention: focus a row + space to toggle.
    bind(
        m,
        VimMode::Normal,
        &[ch(' ')],
        (Some(VimAction::CycleTodo), None),
    );

    // dd — delete current block.
    bind(
        m,
        VimMode::Normal,
        &[ch('d'), ch('d')],
        (Some(VimAction::Block(BlockOp::DeleteCurrent)), None),
    );

    // cc — change current block (clear + enter insert).
    bind(
        m,
        VimMode::Normal,
        &[ch('c'), ch('c')],
        (
            Some(VimAction::Block(BlockOp::ChangeCurrent)),
            Some(VimMode::Insert),
        ),
    );

    // J — join next block into the current one.
    bind(
        m,
        VimMode::Normal,
        &[ch('J')],
        (Some(VimAction::Block(BlockOp::JoinNext)), None),
    );

    // >> / << — indent / outdent. These are unusual: vim expects
    // `>` to take a motion, but in an outliner the natural single-
    // press is just "indent the current block". We bind both `>>`
    // and a single `Tab` later via the host editor.
    bind(
        m,
        VimMode::Normal,
        &[ch('>'), ch('>')],
        (Some(VimAction::Block(BlockOp::IndentRight)), None),
    );
    bind(
        m,
        VimMode::Normal,
        &[ch('<'), ch('<')],
        (Some(VimAction::Block(BlockOp::IndentLeft)), None),
    );
}
