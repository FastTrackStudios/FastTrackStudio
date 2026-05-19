//! Cursor / block motions — Normal, Visual, VisualLine.
//!
//! Vocabulary mirrors the reference plugin's `up.ts` / `down.ts` /
//! `wordForward.ts` etc.

use crate::action::{Motion, VimAction};
use crate::engine::VimMachine;
use crate::mode::VimMode;

use super::{bind, ch};

pub fn register(m: &mut VimMachine) {
    for mode in [VimMode::Normal, VimMode::Visual, VimMode::VisualLine] {
        // hjkl — char/line motions.
        bind(
            m,
            mode,
            &[ch('h')],
            (Some(VimAction::Move(Motion::CharLeft)), None),
        );
        bind(
            m,
            mode,
            &[ch('l')],
            (Some(VimAction::Move(Motion::CharRight)), None),
        );
        bind(
            m,
            mode,
            &[ch('j')],
            (Some(VimAction::Move(Motion::LineDown)), None),
        );
        bind(
            m,
            mode,
            &[ch('k')],
            (Some(VimAction::Move(Motion::LineUp)), None),
        );

        // 0 / $ — line endpoints.
        bind(
            m,
            mode,
            &[ch('0')],
            (Some(VimAction::Move(Motion::LineStart)), None),
        );
        bind(
            m,
            mode,
            &[ch('$')],
            (Some(VimAction::Move(Motion::LineEnd)), None),
        );

        // w / b / e — word motions.
        bind(
            m,
            mode,
            &[ch('w')],
            (Some(VimAction::Move(Motion::WordForward)), None),
        );
        bind(
            m,
            mode,
            &[ch('b')],
            (Some(VimAction::Move(Motion::WordBackward)), None),
        );
        bind(
            m,
            mode,
            &[ch('e')],
            (Some(VimAction::Move(Motion::WordEnd)), None),
        );

        // W / B / E — WORD-class motions (whitespace-delimited).
        bind(
            m,
            mode,
            &[ch('W')],
            (Some(VimAction::Move(Motion::BigWordForward)), None),
        );
        bind(
            m,
            mode,
            &[ch('B')],
            (Some(VimAction::Move(Motion::BigWordBackward)), None),
        );
        bind(
            m,
            mode,
            &[ch('E')],
            (Some(VimAction::Move(Motion::BigWordEnd)), None),
        );

        // gg / G — document endpoints.
        bind(
            m,
            mode,
            &[ch('g'), ch('g')],
            (Some(VimAction::Move(Motion::DocStart)), None),
        );
        bind(
            m,
            mode,
            &[ch('G')],
            (Some(VimAction::Move(Motion::DocEnd)), None),
        );

        // n / N — repeat last `/` search forward / backward. Host
        // applies it against `engine.search_buffer()`; engine just
        // announces the direction via the Motion enum.
        bind(
            m,
            mode,
            &[ch('n')],
            (Some(VimAction::Move(Motion::SearchNext)), None),
        );
        bind(
            m,
            mode,
            &[ch('N')],
            (Some(VimAction::Move(Motion::SearchPrev)), None),
        );
    }
}
