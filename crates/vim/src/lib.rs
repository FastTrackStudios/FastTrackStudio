//! First-class vim editing layer for the Task app.
//!
//! Built on top of [`keybindings`](https://docs.rs/keybindings)
//! and [`editor-types`](https://docs.rs/editor-types) (both
//! Apache-2.0, both wasm-compatible). We own the binding table,
//! the key adapter, and the action vocabulary so the same engine
//! can drive in-block editing today and app-wide navigation
//! tomorrow.
//!
//! ## Quick start
//!
//! ```no_run
//! use vim::{VimEngine, DioxusKey, VimMode, VimAction};
//!
//! let mut engine = VimEngine::default();
//! assert_eq!(engine.mode(), VimMode::Normal);
//!
//! // `i` switches to insert mode and emits an entry action.
//! let actions = engine.feed(DioxusKey::Char('i'));
//! assert_eq!(engine.mode(), VimMode::Insert);
//! assert!(matches!(actions.as_slice(), [VimAction::EnterInsert(_)]));
//!
//! // Escape returns to normal.
//! engine.feed(DioxusKey::Escape);
//! assert_eq!(engine.mode(), VimMode::Normal);
//! ```

pub mod action;
pub mod bindings;
pub mod cursor;
pub mod engine;
pub mod key;
pub mod mode;

pub use action::{BlockOp, InsertEntry, Motion, VimAction, VimCommand};
pub use cursor::{Cursor, CursorState, DocView, apply_motion};
pub use engine::{DefaultBindings, VimEngine, VimMachine};
pub use key::DioxusKey;
pub use mode::{VimMode, VimStep};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn starts_in_normal() {
        let engine = VimEngine::default();
        assert_eq!(engine.mode(), VimMode::Normal);
    }

    #[test]
    fn i_enters_insert_then_esc_returns() {
        let mut e = VimEngine::default();
        let acts = e.feed(DioxusKey::Char('i'));
        assert_eq!(e.mode(), VimMode::Insert);
        assert!(
            acts.iter()
                .any(|a| matches!(a, VimAction::EnterInsert(InsertEntry::BeforeCursor))),
            "expected EnterInsert(BeforeCursor), got {acts:?}"
        );

        let acts = e.feed(DioxusKey::Escape);
        assert_eq!(e.mode(), VimMode::Normal);
        assert!(
            acts.iter().any(|a| matches!(a, VimAction::EnterNormal)),
            "expected EnterNormal, got {acts:?}"
        );
    }

    #[test]
    fn hjkl_emits_motions() {
        let mut e = VimEngine::default();
        let h = e.feed(DioxusKey::Char('h'));
        assert!(
            h.iter()
                .any(|a| matches!(a, VimAction::Move(Motion::CharLeft)))
        );
        let j = e.feed(DioxusKey::Char('j'));
        assert!(
            j.iter()
                .any(|a| matches!(a, VimAction::Move(Motion::LineDown)))
        );
    }

    #[test]
    fn gg_is_a_two_key_sequence() {
        let mut e = VimEngine::default();
        // First `g` is pending — no action.
        let first = e.feed(DioxusKey::Char('g'));
        assert!(
            !first
                .iter()
                .any(|a| matches!(a, VimAction::Move(Motion::DocStart))),
            "first `g` should not fire DocStart yet"
        );
        // Second `g` completes.
        let second = e.feed(DioxusKey::Char('g'));
        assert!(
            second
                .iter()
                .any(|a| matches!(a, VimAction::Move(Motion::DocStart))),
            "expected DocStart after `gg`, got {second:?}"
        );
    }

    #[test]
    fn dd_deletes_block() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('d'));
        let acts = e.feed(DioxusKey::Char('d'));
        assert!(
            acts.iter()
                .any(|a| matches!(a, VimAction::Block(BlockOp::DeleteCurrent))),
            "expected Block(DeleteCurrent), got {acts:?}"
        );
    }

    #[test]
    fn count_buffer_repeats_motion() {
        let mut e = VimEngine::default();
        // `3j` should emit three Move(LineDown).
        let a = e.feed(DioxusKey::Char('3'));
        assert!(a.is_empty(), "digit alone shouldn't fire an action");
        assert_eq!(e.pending_count(), Some(3));
        let a = e.feed(DioxusKey::Char('j'));
        assert_eq!(a.len(), 3);
        assert!(
            a.iter()
                .all(|x| matches!(x, VimAction::Move(Motion::LineDown)))
        );
        assert_eq!(e.pending_count(), None, "count clears after action");
    }

    #[test]
    fn multi_digit_count() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('1'));
        e.feed(DioxusKey::Char('2'));
        assert_eq!(e.pending_count(), Some(12));
        let a = e.feed(DioxusKey::Char('k'));
        assert_eq!(a.len(), 12);
    }

    #[test]
    fn bare_zero_is_line_start_not_count() {
        let mut e = VimEngine::default();
        let a = e.feed(DioxusKey::Char('0'));
        assert!(
            a.iter()
                .any(|x| matches!(x, VimAction::Move(Motion::LineStart))),
            "bare 0 should be LineStart, got {a:?}"
        );
    }

    #[test]
    fn zero_extends_existing_count() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('1'));
        e.feed(DioxusKey::Char('0'));
        assert_eq!(e.pending_count(), Some(10));
        let a = e.feed(DioxusKey::Char('j'));
        assert_eq!(a.len(), 10);
    }

    #[test]
    fn escape_clears_pending_count() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('5'));
        assert_eq!(e.pending_count(), Some(5));
        e.feed(DioxusKey::Escape);
        assert_eq!(e.pending_count(), None);
        // Subsequent j is one motion, not five.
        let a = e.feed(DioxusKey::Char('j'));
        assert_eq!(a.len(), 1);
    }

    #[test]
    fn count_does_not_multiply_mode_entries() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('3'));
        let a = e.feed(DioxusKey::Char('i'));
        assert_eq!(a.len(), 1, "EnterInsert should emit once even with count 3");
        assert_eq!(e.mode(), VimMode::Insert);
    }

    #[test]
    fn count_multiplies_dd() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('2'));
        e.feed(DioxusKey::Char('d'));
        let a = e.feed(DioxusKey::Char('d'));
        assert_eq!(a.len(), 2, "2dd should delete two blocks, got {a:?}");
        assert!(
            a.iter()
                .all(|x| matches!(x, VimAction::Block(BlockOp::DeleteCurrent)))
        );
    }

    #[test]
    fn o_creates_block_below_and_enters_insert() {
        let mut e = VimEngine::default();
        let acts = e.feed(DioxusKey::Char('o'));
        assert_eq!(e.mode(), VimMode::Insert);
        assert!(
            acts.iter()
                .any(|a| matches!(a, VimAction::Block(BlockOp::NewBelow))),
            "expected Block(NewBelow), got {acts:?}"
        );
    }

    #[test]
    fn cc_changes_block_and_enters_insert() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('c'));
        let acts = e.feed(DioxusKey::Char('c'));
        assert_eq!(e.mode(), VimMode::Insert);
        assert!(
            acts.iter()
                .any(|a| matches!(a, VimAction::Block(BlockOp::ChangeCurrent))),
            "expected Block(ChangeCurrent), got {acts:?}"
        );
    }

    #[test]
    fn capital_j_emits_join_next() {
        let mut e = VimEngine::default();
        let acts = e.feed(DioxusKey::Char('J'));
        assert_eq!(e.mode(), VimMode::Normal);
        assert!(
            acts.iter()
                .any(|a| matches!(a, VimAction::Block(BlockOp::JoinNext))),
            "expected Block(JoinNext), got {acts:?}"
        );
    }

    #[test]
    fn f_then_char_emits_find_forward() {
        let mut e = VimEngine::default();
        let pending = e.feed(DioxusKey::Char('f'));
        assert!(pending.is_empty(), "f waits for next char, got {pending:?}");
        let acts = e.feed(DioxusKey::Char('x'));
        assert!(
            acts.iter().any(|a| matches!(
                a,
                VimAction::Move(Motion::FindChar {
                    ch: 'x',
                    direction: 1,
                    till: false
                })
            )),
            "expected Move(FindChar fx), got {acts:?}"
        );
    }

    #[test]
    fn capital_t_emits_find_back_till() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('T'));
        let acts = e.feed(DioxusKey::Char('z'));
        assert!(acts.iter().any(|a| matches!(
            a,
            VimAction::Move(Motion::FindChar {
                ch: 'z',
                direction: -1,
                till: true
            })
        )));
    }

    #[test]
    fn semicolon_repeats_last_find() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('f'));
        e.feed(DioxusKey::Char('q'));
        let acts = e.feed(DioxusKey::Char(';'));
        assert!(acts.iter().any(|a| matches!(
            a,
            VimAction::Move(Motion::FindChar {
                ch: 'q',
                direction: 1,
                till: false
            })
        )));
    }

    #[test]
    fn comma_reverses_last_find() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('f'));
        e.feed(DioxusKey::Char('q'));
        let acts = e.feed(DioxusKey::Char(','));
        assert!(acts.iter().any(|a| matches!(
            a,
            VimAction::Move(Motion::FindChar {
                ch: 'q',
                direction: -1,
                till: false
            })
        )));
    }

    #[test]
    fn m_then_char_emits_set_mark() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('m'));
        let acts = e.feed(DioxusKey::Char('a'));
        assert!(acts.iter().any(|a| matches!(a, VimAction::SetMark('a'))));
    }

    #[test]
    fn apostrophe_then_char_emits_jump_to_mark() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('\''));
        let acts = e.feed(DioxusKey::Char('a'));
        assert!(acts.iter().any(|a| matches!(a, VimAction::JumpToMark('a'))));
    }

    #[test]
    fn capital_v_enters_visual_line() {
        let mut e = VimEngine::default();
        let acts = e.feed(DioxusKey::Char('V'));
        assert_eq!(e.mode(), VimMode::VisualLine);
        assert!(acts.iter().any(|a| matches!(a, VimAction::EnterVisual)));
    }

    #[test]
    fn lowercase_v_still_enters_visual() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('v'));
        assert_eq!(e.mode(), VimMode::Visual);
    }

    #[test]
    fn motion_in_visual_line_stays_in_visual_line() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('V'));
        assert_eq!(e.mode(), VimMode::VisualLine);
        let acts = e.feed(DioxusKey::Char('j'));
        assert_eq!(e.mode(), VimMode::VisualLine);
        assert!(
            acts.iter()
                .any(|a| matches!(a, VimAction::Move(Motion::LineDown))),
            "expected LineDown, got {acts:?}"
        );
    }

    #[test]
    fn count_prefix_works_in_visual_line() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('V'));
        e.feed(DioxusKey::Char('3'));
        let acts = e.feed(DioxusKey::Char('j'));
        assert_eq!(
            acts.iter()
                .filter(|a| matches!(a, VimAction::Move(Motion::LineDown)))
                .count(),
            3,
            "3j in V-LINE should emit 3 LineDowns, got {acts:?}"
        );
    }

    #[test]
    fn escape_from_visual_line_to_normal() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('V'));
        e.feed(DioxusKey::Escape);
        assert_eq!(e.mode(), VimMode::Normal);
    }

    #[test]
    fn colon_enters_command_mode_with_empty_buffer() {
        let mut e = VimEngine::default();
        let acts = e.feed(DioxusKey::Char(':'));
        assert_eq!(e.mode(), VimMode::Command);
        assert_eq!(e.command_buffer(), Some(""));
        // Mode-only transitions emit `NoOp` (or nothing) — neither
        // should produce a meaningful action.
        assert!(
            acts.iter().all(|a| matches!(a, VimAction::NoOp)),
            "expected only NoOps; got {acts:?}"
        );
    }

    #[test]
    fn command_buffer_accumulates_chars() {
        let mut e = VimEngine::default();
        for c in ":wq".chars() {
            e.feed(DioxusKey::Char(c));
        }
        assert_eq!(e.mode(), VimMode::Command);
        assert_eq!(e.command_buffer(), Some("wq"));
    }

    #[test]
    fn enter_submits_known_command_and_returns_to_normal() {
        let mut e = VimEngine::default();
        for c in ":wq".chars() {
            e.feed(DioxusKey::Char(c));
        }
        let acts = e.feed(DioxusKey::Enter);
        assert_eq!(e.mode(), VimMode::Normal);
        assert_eq!(e.command_buffer(), None);
        assert!(
            acts.iter()
                .any(|a| matches!(a, VimAction::SubmitCommand(VimCommand::SaveQuit))),
            "expected SaveQuit, got {acts:?}"
        );
    }

    #[test]
    fn enter_drops_unknown_command_silently() {
        let mut e = VimEngine::default();
        for c in ":xyzzy".chars() {
            e.feed(DioxusKey::Char(c));
        }
        let acts = e.feed(DioxusKey::Enter);
        assert_eq!(e.mode(), VimMode::Normal);
        assert!(acts.is_empty(), "unknown command, got {acts:?}");
    }

    #[test]
    fn escape_cancels_command_mode() {
        let mut e = VimEngine::default();
        for c in ":wq".chars() {
            e.feed(DioxusKey::Char(c));
        }
        e.feed(DioxusKey::Escape);
        assert_eq!(e.mode(), VimMode::Normal);
        assert_eq!(e.command_buffer(), None);
    }

    #[test]
    fn backspace_pops_then_exits() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char(':'));
        e.feed(DioxusKey::Char('w'));
        e.feed(DioxusKey::Backspace);
        assert_eq!(e.command_buffer(), Some(""));
        // Empty + Backspace → exit Command.
        e.feed(DioxusKey::Backspace);
        assert_eq!(e.mode(), VimMode::Normal);
    }

    #[test]
    fn command_aliases_parse() {
        for (input, expected) in [
            ("w", VimCommand::Save),
            ("write", VimCommand::Save),
            ("q", VimCommand::Quit),
            ("quit", VimCommand::Quit),
            ("wq", VimCommand::SaveQuit),
            ("x", VimCommand::SaveQuit),
            ("help", VimCommand::Help),
            ("h", VimCommand::Help),
            ("noh", VimCommand::NoHighlight),
        ] {
            assert_eq!(VimCommand::parse(input), Some(expected), "parse {input}");
        }
        assert_eq!(VimCommand::parse("garbage"), None);
    }

    #[test]
    fn slash_enters_search_mode() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('/'));
        assert_eq!(e.mode(), VimMode::Search);
        assert_eq!(e.search_buffer(), Some(""));
    }

    #[test]
    fn search_buffer_survives_submit_for_n_replay() {
        let mut e = VimEngine::default();
        for c in "/foo".chars() {
            e.feed(DioxusKey::Char(c));
        }
        let acts = e.feed(DioxusKey::Enter);
        assert_eq!(e.mode(), VimMode::Normal);
        assert_eq!(
            e.search_buffer(),
            Some("foo"),
            "buffer should survive submit so n/N can replay"
        );
        assert!(
            acts.iter().any(|a| matches!(a, VimAction::SubmitSearch)),
            "expected SubmitSearch, got {acts:?}"
        );
    }

    #[test]
    fn n_and_big_n_emit_search_motions() {
        let mut e = VimEngine::default();
        let n_acts = e.feed(DioxusKey::Char('n'));
        let big_n_acts = e.feed(DioxusKey::Char('N'));
        assert!(
            n_acts
                .iter()
                .any(|a| matches!(a, VimAction::Move(Motion::SearchNext))),
            "expected SearchNext, got {n_acts:?}"
        );
        assert!(
            big_n_acts
                .iter()
                .any(|a| matches!(a, VimAction::Move(Motion::SearchPrev))),
            "expected SearchPrev, got {big_n_acts:?}"
        );
    }

    #[test]
    fn empty_search_submit_is_noop() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('/'));
        let acts = e.feed(DioxusKey::Enter);
        assert_eq!(e.mode(), VimMode::Normal);
        assert!(acts.is_empty(), "empty search shouldn't emit; got {acts:?}");
        assert_eq!(e.search_buffer(), None);
    }

    #[test]
    fn escape_cancels_search_and_wipes_buffer() {
        let mut e = VimEngine::default();
        for c in "/foo".chars() {
            e.feed(DioxusKey::Char(c));
        }
        e.feed(DioxusKey::Escape);
        assert_eq!(e.mode(), VimMode::Normal);
        assert_eq!(e.search_buffer(), None);
    }

    #[test]
    fn d_in_visual_line_emits_delete_selection_and_exits() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('V'));
        let acts = e.feed(DioxusKey::Char('d'));
        assert_eq!(e.mode(), VimMode::Normal);
        assert!(
            acts.iter().any(|a| matches!(a, VimAction::DeleteSelection)),
            "expected DeleteSelection, got {acts:?}"
        );
    }

    #[test]
    fn y_in_visual_line_emits_yank_selection_and_exits() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('V'));
        let acts = e.feed(DioxusKey::Char('y'));
        assert_eq!(e.mode(), VimMode::Normal);
        assert!(
            acts.iter().any(|a| matches!(a, VimAction::YankSelection)),
            "expected YankSelection, got {acts:?}"
        );
    }

    #[test]
    fn x_in_visual_line_is_alias_for_d() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('V'));
        let acts = e.feed(DioxusKey::Char('x'));
        assert_eq!(e.mode(), VimMode::Normal);
        assert!(
            acts.iter().any(|a| matches!(a, VimAction::DeleteSelection)),
            "expected DeleteSelection via x, got {acts:?}"
        );
    }

    #[test]
    fn escape_clears_pending_operator() {
        let mut e = VimEngine::default();
        e.feed(DioxusKey::Char('f'));
        e.feed(DioxusKey::Escape);
        // Next char should NOT complete the cancelled find.
        let acts = e.feed(DioxusKey::Char('x'));
        assert!(
            !acts
                .iter()
                .any(|a| matches!(a, VimAction::Move(Motion::FindChar { .. }))),
            "escape should cancel pending f, got {acts:?}"
        );
    }
}
