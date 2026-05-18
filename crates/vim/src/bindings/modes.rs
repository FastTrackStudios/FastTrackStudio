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
