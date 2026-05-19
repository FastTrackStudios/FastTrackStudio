//! Mode transitions — Insert→Normal, Visual entry/exit.

use crate::action::VimAction;
use crate::engine::VimMachine;
use crate::key::DioxusKey;
use crate::mode::VimMode;

use super::{bind, ch, k};

pub fn register(m: &mut VimMachine) {
    // v — enter Visual.
    bind(
        m,
        VimMode::Normal,
        &[ch('v')],
        (Some(VimAction::EnterVisual), Some(VimMode::Visual)),
    );

    // V — enter VisualLine. Reuses the same EnterVisual action
    // since the host's selection-anchor logic doesn't care about
    // line vs char granularity at the data level — only the
    // renderer differs.
    bind(
        m,
        VimMode::Normal,
        &[ch('V')],
        (Some(VimAction::EnterVisual), Some(VimMode::VisualLine)),
    );

    // : — enter Command mode. The mode change rides on this
    // binding; the engine seeds the empty buffer when it sees `:`
    // in Normal (see `engine::feed`).
    bind(
        m,
        VimMode::Normal,
        &[ch(':')],
        (None, Some(VimMode::Command)),
    );

    // / — enter Search mode. Same buffer-seeding pattern as `:`.
    bind(
        m,
        VimMode::Normal,
        &[ch('/')],
        (None, Some(VimMode::Search)),
    );

    // d / x in VisualLine — delete the selected blocks and exit to
    // Normal. Engine emits the action; host applies it to its
    // anchor→cursor range.
    bind(
        m,
        VimMode::VisualLine,
        &[ch('d')],
        (Some(VimAction::DeleteSelection), Some(VimMode::Normal)),
    );
    bind(
        m,
        VimMode::VisualLine,
        &[ch('x')],
        (Some(VimAction::DeleteSelection), Some(VimMode::Normal)),
    );
    // y in VisualLine — yank the selected blocks into the host's
    // register and exit to Normal.
    bind(
        m,
        VimMode::VisualLine,
        &[ch('y')],
        (Some(VimAction::YankSelection), Some(VimMode::Normal)),
    );

    // Esc — Insert/Visual/VisualLine → Normal.
    bind(
        m,
        VimMode::Insert,
        &[k(DioxusKey::Escape)],
        (Some(VimAction::EnterNormal), Some(VimMode::Normal)),
    );
    bind(
        m,
        VimMode::Visual,
        &[k(DioxusKey::Escape)],
        (Some(VimAction::EnterNormal), Some(VimMode::Normal)),
    );
    bind(
        m,
        VimMode::VisualLine,
        &[k(DioxusKey::Escape)],
        (Some(VimAction::EnterNormal), Some(VimMode::Normal)),
    );
}
