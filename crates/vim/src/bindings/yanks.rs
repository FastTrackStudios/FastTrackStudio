//! Yank / paste — `yy`, `p`, `P`. Block-scope only for v1
//! (Logseq-style: yank the whole block, paste a new block).
//! Linewise / character-wise selection paste comes with Visual
//! mode coverage.

use crate::action::{BlockOp, VimAction};
use crate::engine::VimMachine;
use crate::mode::VimMode;

use super::{bind, ch};

pub fn register(m: &mut VimMachine) {
    // yy — yank current block.
    bind(
        m,
        VimMode::Normal,
        &[ch('y'), ch('y')],
        (Some(VimAction::Block(BlockOp::YankCurrent)), None),
    );

    // p — paste register as new block after current.
    bind(
        m,
        VimMode::Normal,
        &[ch('p')],
        (Some(VimAction::Block(BlockOp::PasteAfter)), None),
    );

    // P — paste register as new block before current.
    bind(
        m,
        VimMode::Normal,
        &[ch('P')],
        (Some(VimAction::Block(BlockOp::PasteBefore)), None),
    );
}
