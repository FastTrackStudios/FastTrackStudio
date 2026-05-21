//! [`VimState`] + the central dispatcher.
//!
//! vim ref: zed/crates/vim/src/state.rs:45 (`Mode`, `Operator`)
//! vim ref: codemirror-vim/src/vim.js (vim_api / commandDispatcher)

use editor_state::{
    Changes, EditorState, KeySpec, Range, Selection, TransactionSpec,
};

use crate::motions::{self, Motion};
use crate::operators::{self, Operator};
use crate::registers::{RegisterKey, Registers};
use crate::text_objects::{self, TextObject};

/// Current vim mode. Operator-pending is modeled as a flag
/// inside `Normal`, not its own variant — `pending_operator`
/// being `Some` is what makes us "operator-pending".
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Mode {
    #[default]
    Normal,
    Insert,
    VisualChar,
    VisualLine,
    VisualBlock,
    Replace,
    Command,
}

/// Sticky pending state between key presses. Each non-`None`
/// field represents a partially-typed command.
#[derive(Clone, Debug, Default)]
pub struct VimState {
    pub mode: Mode,
    pub pending_count: Option<usize>,
    pub pending_operator: Option<Operator>,
    pub pending_register: Option<RegisterKey>,
    /// Set when the previous key was one of `f F t T r` and the
    /// next key is a literal char, not a command.
    pub pending_motion_input: Option<MotionInput>,
    /// Anchor offset for visual mode. `None` outside visual.
    pub visual_anchor: Option<usize>,
    pub registers: Registers,
    pub last_change: Option<LastChange>,
}

/// Which pending command is waiting on a character.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MotionInput {
    FindForward,        // `f`
    FindBackward,       // `F`
    TillForward,        // `t`
    TillBackward,       // `T`
    Replace,            // `r`
    Register,           // `"` — the next char names a register
}

/// Recorded last change for `.` repeat. We store *intent* rather
/// than keystrokes: an operator + motion (or text object) + count,
/// or an insert payload.
///
/// vim ref: codemirror-vim/src/vim.js#L4500 (`vim.lastEditInputState`)
#[derive(Clone, Debug)]
pub enum LastChange {
    OperatorMotion {
        operator: Operator,
        motion: Motion,
        count: usize,
    },
    OperatorTextObject {
        operator: Operator,
        object: TextObject,
        around: bool,
        count: usize,
    },
    /// Insert-mode text that was committed by `<Esc>`. Replay
    /// inserts this at the caret. Not wired through `apply`
    /// yet — see `macros.rs`.
    Insert(String),
}

impl VimState {
    pub fn new() -> Self {
        Self::default()
    }

    /// Reset all the transient pending fields. Call after a
    /// command commits or is aborted.
    pub fn clear_pending(&mut self) {
        self.pending_count = None;
        self.pending_operator = None;
        self.pending_register = None;
        self.pending_motion_input = None;
    }

    pub fn is_visual(&self) -> bool {
        matches!(
            self.mode,
            Mode::VisualChar | Mode::VisualLine | Mode::VisualBlock
        )
    }

    /// True when the host should let raw character keystrokes
    /// fall through to the contenteditable / text-input path.
    /// Used by `editor-view`'s onkeydown to gate
    /// `preventDefault` between non-Insert and Insert modes.
    pub fn is_inserting(&self) -> bool {
        matches!(self.mode, Mode::Insert | Mode::Replace)
    }

    /// Hook for the editor's history system. Returns `None`
    /// today; the dispatcher emits `request_undo` / `request_redo`
    /// via metadata on the returned [`TransactionSpec`].
    pub fn request_undo(&self) -> Option<TransactionSpec> {
        Some(
            TransactionSpec::new()
                .annotate("vim", "undo")
                .user_event("undo"),
        )
    }

    pub fn request_redo(&self) -> Option<TransactionSpec> {
        Some(
            TransactionSpec::new()
                .annotate("vim", "redo")
                .user_event("redo"),
        )
    }
}

/// Dispatch a single key. See module docs in `lib.rs` for the
/// state machine outline.
pub(crate) fn dispatch(
    state: &EditorState,
    vim: &mut VimState,
    key: &KeySpec,
) -> Option<TransactionSpec> {
    // Pending-input first: `f<x>`, `r<x>`, `"<x>` consume the
    // next keystroke verbatim. Doing this *before* mode dispatch
    // keeps motion-with-arg logic centralized.
    if let Some(input) = vim.pending_motion_input.take() {
        return finish_pending_input(state, vim, key, input);
    }

    match vim.mode {
        Mode::Normal => dispatch_normal(state, vim, key),
        Mode::Insert => dispatch_insert(state, vim, key),
        Mode::VisualChar | Mode::VisualLine | Mode::VisualBlock => {
            dispatch_visual(state, vim, key)
        }
        Mode::Replace => dispatch_replace(state, vim, key),
        Mode::Command => crate::command_line::dispatch(state, vim, key),
    }
}

// --- Normal mode -----------------------------------------------

fn dispatch_normal(
    state: &EditorState,
    vim: &mut VimState,
    key: &KeySpec,
) -> Option<TransactionSpec> {
    // `<Esc>` always clears pending state.
    if key.key == "Escape" {
        vim.clear_pending();
        return None;
    }

    // No modifiers? Treat the key string as the raw vim "char".
    // We deliberately ignore `shift` for letter case — pressing
    // capital `A` arrives as key="A" without shift mattering for
    // dispatch (the lexical case carries the info).
    if key.ctrl || key.alt || key.meta {
        return dispatch_normal_modified(state, vim, key);
    }

    let ch = match single_char(key) {
        Some(c) => c,
        None => return None,
    };

    // Count accumulation: `0` only starts a count if there's
    // already one in flight — otherwise `0` is the line-start
    // motion. vim ref: codemirror-vim/src/vim.js (numberRegex)
    if ch.is_ascii_digit() && !(ch == '0' && vim.pending_count.is_none()) {
        let d = ch.to_digit(10).unwrap() as usize;
        vim.pending_count = Some(vim.pending_count.unwrap_or(0) * 10 + d);
        return None;
    }

    // Register prefix.
    if ch == '"' {
        vim.pending_motion_input = Some(MotionInput::Register);
        return None;
    }

    // Operator-pending: if `pending_operator` is `Some`, this
    // keystroke must produce a motion or be the doubled-op
    // shorthand (`dd`/`cc`/`yy`).
    if let Some(op) = vim.pending_operator.clone() {
        return finish_operator(state, vim, op, ch);
    }

    // Try a single-char command (mode change, paste, undo, etc.)
    // before falling through to motion.
    if let Some(spec) = single_char_normal_command(state, vim, ch) {
        return Some(spec);
    }

    // Try an operator key.
    if let Some(op) = Operator::from_char(ch) {
        vim.pending_operator = Some(op);
        return None;
    }

    // Try a motion. Motions that need a char (`f F t T`) set the
    // pending-input flag and return.
    if let Some(motion) = Motion::from_char(ch) {
        if motion_needs_input(motion).is_some() {
            vim.pending_motion_input = motion_needs_input(motion);
            // Stash the half-built motion on the operator slot is
            // wrong — instead we re-derive it in finish_pending_input.
            // For now we just remember the count and let the
            // pending-input branch rebuild.
            return None;
        }
        let count = vim.pending_count.take().unwrap_or(1);
        let new_pos = motions::apply(state, motion, count);
        vim.clear_pending();
        return Some(TransactionSpec::new().selection(Selection::caret(new_pos)));
    }

    None
}

fn dispatch_normal_modified(
    _state: &EditorState,
    vim: &mut VimState,
    key: &KeySpec,
) -> Option<TransactionSpec> {
    // Ctrl-r = redo. Ctrl-v in normal = enter visual block.
    if key.ctrl && key.key == "r" {
        vim.clear_pending();
        return vim.request_redo();
    }
    if key.ctrl && key.key == "v" {
        vim.mode = Mode::VisualBlock;
        vim.visual_anchor = None; // set on first dispatch_visual
        return None;
    }
    None
}

fn single_char_normal_command(
    state: &EditorState,
    vim: &mut VimState,
    ch: char,
) -> Option<TransactionSpec> {
    match ch {
        'i' => {
            vim.mode = Mode::Insert;
            vim.clear_pending();
            Some(TransactionSpec::new())
        }
        'I' => {
            // First non-blank on current line, then insert.
            let pos = motions::line_first_nonblank(state, caret(state));
            vim.mode = Mode::Insert;
            vim.clear_pending();
            Some(TransactionSpec::new().selection(Selection::caret(pos)))
        }
        'a' => {
            let pos = (caret(state) + 1).min(state.doc.len());
            vim.mode = Mode::Insert;
            vim.clear_pending();
            Some(TransactionSpec::new().selection(Selection::caret(pos)))
        }
        'A' => {
            let pos = motions::line_end(state, caret(state));
            vim.mode = Mode::Insert;
            vim.clear_pending();
            Some(TransactionSpec::new().selection(Selection::caret(pos)))
        }
        'o' => {
            let end = motions::line_end(state, caret(state));
            vim.mode = Mode::Insert;
            vim.clear_pending();
            Some(
                TransactionSpec::new()
                    .changes(Changes::insert(end, "\n"))
                    .selection(Selection::caret(end + 1)),
            )
        }
        'O' => {
            let start = motions::line_start(state, caret(state));
            vim.mode = Mode::Insert;
            vim.clear_pending();
            Some(
                TransactionSpec::new()
                    .changes(Changes::insert(start, "\n"))
                    .selection(Selection::caret(start)),
            )
        }
        's' => {
            let from = caret(state);
            let to = (from + 1).min(state.doc.len());
            vim.mode = Mode::Insert;
            vim.clear_pending();
            Some(
                TransactionSpec::new()
                    .changes(Changes::delete(from..to))
                    .selection(Selection::caret(from)),
            )
        }
        'S' => {
            let from = motions::line_start(state, caret(state));
            let to = motions::line_end(state, caret(state));
            vim.mode = Mode::Insert;
            vim.clear_pending();
            Some(
                TransactionSpec::new()
                    .changes(Changes::delete(from..to))
                    .selection(Selection::caret(from)),
            )
        }
        'r' => {
            vim.pending_motion_input = Some(MotionInput::Replace);
            None
        }
        'R' => {
            vim.mode = Mode::Replace;
            vim.clear_pending();
            Some(TransactionSpec::new())
        }
        'v' => {
            vim.mode = Mode::VisualChar;
            vim.visual_anchor = Some(caret(state));
            vim.clear_pending();
            None
        }
        'V' => {
            vim.mode = Mode::VisualLine;
            vim.visual_anchor = Some(caret(state));
            vim.clear_pending();
            None
        }
        ':' => {
            vim.mode = Mode::Command;
            vim.clear_pending();
            None
        }
        'u' => {
            vim.clear_pending();
            vim.request_undo()
        }
        'p' => Some(paste(state, vim, /*before=*/ false)),
        'P' => Some(paste(state, vim, /*before=*/ true)),
        'x' => {
            // Delete char under caret.
            let from = caret(state);
            let to = (from + 1).min(state.doc.len());
            if from == to {
                return None;
            }
            let text = state.doc.slice(from..to);
            vim.registers.write_unnamed(&text);
            vim.clear_pending();
            Some(
                TransactionSpec::new()
                    .changes(Changes::delete(from..to))
                    .selection(Selection::caret(from)),
            )
        }
        'J' => Some(join_lines(state, vim)),
        '~' => Some(toggle_case_char(state, vim)),
        '.' => crate::macros::replay_last(state, vim),
        _ => None,
    }
}

fn finish_operator(
    state: &EditorState,
    vim: &mut VimState,
    op: Operator,
    ch: char,
) -> Option<TransactionSpec> {
    // Doubled-op shorthand: `dd`, `cc`, `yy` act on the current
    // line.
    if Operator::from_char(ch) == Some(op.clone()) {
        let count = vim.pending_count.take().unwrap_or(1);
        let from = motions::line_start(state, caret(state));
        let to = motions::line_end_n(state, caret(state), count);
        // Include the trailing newline if there is one (vim
        // semantics: `dd` is linewise).
        let to_inclusive = (to + 1).min(state.doc.len());
        return Some(operators::apply_linewise(
            state, vim, op, from, to_inclusive,
        ));
    }

    // Text object: `iw`, `aw`, `i"`, ...
    if ch == 'i' || ch == 'a' {
        vim.pending_motion_input = Some(if ch == 'a' {
            MotionInput::TillForward // dummy — text-object path
        } else {
            MotionInput::TillBackward
        });
        // Hack: we re-derive operator + around-flag from these
        // sentinel values in `finish_pending_input`. Cleaner
        // would be a `TextObjectPending { around: bool }` variant.
        // Track for v1.1.
        return None;
    }

    // Otherwise we expect a motion.
    if let Some(motion) = Motion::from_char(ch) {
        if let Some(needed) = motion_needs_input(motion) {
            // Stash op + register on vim, mark waiting for char.
            // Stays pending: vim.pending_operator already set.
            vim.pending_motion_input = Some(needed);
            return None;
        }
        let count = vim.pending_count.take().unwrap_or(1);
        let from = caret(state);
        let to = motions::apply(state, motion, count);
        let (lo, hi) = if from <= to { (from, to) } else { (to, from) };
        vim.last_change = Some(LastChange::OperatorMotion {
            operator: op.clone(),
            motion,
            count,
        });
        let spec = operators::apply_range(state, vim, op, lo, hi);
        return Some(spec);
    }

    None
}

fn finish_pending_input(
    state: &EditorState,
    vim: &mut VimState,
    key: &KeySpec,
    input: MotionInput,
) -> Option<TransactionSpec> {
    let ch = single_char(key)?;
    match input {
        MotionInput::Register => {
            vim.pending_register = RegisterKey::from_char(ch);
            None
        }
        MotionInput::Replace => {
            let from = caret(state);
            let to = (from + 1).min(state.doc.len());
            if from == to {
                return None;
            }
            vim.clear_pending();
            Some(
                TransactionSpec::new()
                    .changes(Changes::replace(from..to, ch.to_string()))
                    .selection(Selection::caret(from)),
            )
        }
        MotionInput::FindForward
        | MotionInput::FindBackward
        | MotionInput::TillForward
        | MotionInput::TillBackward => {
            // If we got here via the text-object hack inside
            // `finish_operator`, `pending_operator` is `Some` and
            // `ch` is the object key (`w`/`"`/`{`/...).
            if let Some(op) = vim.pending_operator.clone() {
                let around = matches!(input, MotionInput::TillForward);
                if let Some(obj) = TextObject::from_char(ch) {
                    let count = vim.pending_count.take().unwrap_or(1);
                    let range = text_objects::apply(state, obj, around, caret(state));
                    vim.last_change = Some(LastChange::OperatorTextObject {
                        operator: op.clone(),
                        object: obj,
                        around,
                        count,
                    });
                    let spec = operators::apply_range(state, vim, op, range.start, range.end);
                    return Some(spec);
                }
            }
            // Plain f/F/t/T motion case — apply directly.
            let count = vim.pending_count.take().unwrap_or(1);
            let target = motions::find_char(state, caret(state), ch, input, count)?;
            if let Some(op) = vim.pending_operator.clone() {
                let from = caret(state);
                let (lo, hi) = if from <= target {
                    (from, target + 1)
                } else {
                    (target, from)
                };
                let spec = operators::apply_range(state, vim, op, lo, hi);
                return Some(spec);
            }
            Some(TransactionSpec::new().selection(Selection::caret(target)))
        }
    }
}

fn motion_needs_input(motion: Motion) -> Option<MotionInput> {
    match motion {
        Motion::FindForward => Some(MotionInput::FindForward),
        Motion::FindBackward => Some(MotionInput::FindBackward),
        Motion::TillForward => Some(MotionInput::TillForward),
        Motion::TillBackward => Some(MotionInput::TillBackward),
        _ => None,
    }
}

// --- Insert mode -----------------------------------------------

fn dispatch_insert(
    _state: &EditorState,
    vim: &mut VimState,
    key: &KeySpec,
) -> Option<TransactionSpec> {
    if key.key == "Escape" {
        vim.mode = Mode::Normal;
        vim.clear_pending();
        return Some(TransactionSpec::new());
    }
    // All other keys fall through to the host's text-input path.
    None
}

// --- Replace mode ----------------------------------------------

fn dispatch_replace(
    state: &EditorState,
    vim: &mut VimState,
    key: &KeySpec,
) -> Option<TransactionSpec> {
    if key.key == "Escape" {
        vim.mode = Mode::Normal;
        return Some(TransactionSpec::new());
    }
    let ch = single_char(key)?;
    let from = caret(state);
    let to = (from + 1).min(state.doc.len());
    Some(
        TransactionSpec::new()
            .changes(Changes::replace(from..to, ch.to_string()))
            .selection(Selection::caret(to)),
    )
}

// --- Visual mode -----------------------------------------------

fn dispatch_visual(
    state: &EditorState,
    vim: &mut VimState,
    key: &KeySpec,
) -> Option<TransactionSpec> {
    if key.key == "Escape" {
        vim.mode = Mode::Normal;
        let pos = caret(state);
        vim.visual_anchor = None;
        vim.clear_pending();
        return Some(TransactionSpec::new().selection(Selection::caret(pos)));
    }
    let ch = single_char(key)?;
    // Operator on the current visual range.
    if let Some(op) = Operator::from_char(ch) {
        let r = state.selection.primary();
        let (lo, hi) = (r.from(), r.to());
        vim.mode = Mode::Normal;
        vim.visual_anchor = None;
        let spec = operators::apply_range(state, vim, op, lo, hi);
        return Some(spec);
    }
    if ch == 'y' {
        // (already handled by Operator::from_char path above)
    }
    // Motion in visual: extend `head` to motion target, keep anchor.
    if let Some(motion) = Motion::from_char(ch) {
        if motion_needs_input(motion).is_some() {
            vim.pending_motion_input = motion_needs_input(motion);
            return None;
        }
        let count = vim.pending_count.take().unwrap_or(1);
        let new_head = motions::apply(state, motion, count);
        let anchor = vim.visual_anchor.unwrap_or(caret(state));
        return Some(TransactionSpec::new().selection(Selection::single(Range::new(anchor, new_head))));
    }
    None
}

// --- helpers ----------------------------------------------------

fn caret(state: &EditorState) -> usize {
    state.selection.primary().head
}

pub(crate) fn single_char(key: &KeySpec) -> Option<char> {
    let mut chars = key.key.chars();
    let c = chars.next()?;
    if chars.next().is_some() {
        return None;
    }
    Some(c)
}

fn paste(state: &EditorState, vim: &mut VimState, before: bool) -> TransactionSpec {
    let key = vim.pending_register.take();
    let text = vim.registers.read(key).unwrap_or_default();
    vim.clear_pending();
    if text.is_empty() {
        return TransactionSpec::new();
    }
    let linewise = text.ends_with('\n');
    let (pos, new_caret) = if linewise {
        // `p` inserts below current line, `P` inserts above.
        if before {
            let start = motions::line_start(state, caret(state));
            (start, start)
        } else {
            let end = motions::line_end(state, caret(state));
            let insert_at = if end < state.doc.len() { end + 1 } else { end };
            // If we're inserting at EOF without a trailing newline,
            // prepend a '\n' to keep the payload on its own line.
            if end == state.doc.len() {
                let payload = format!("\n{}", text.trim_end_matches('\n'));
                let new_caret = end + 1;
                return TransactionSpec::new()
                    .changes(Changes::insert(end, payload))
                    .selection(Selection::caret(new_caret));
            }
            (insert_at, insert_at)
        }
    } else if before {
        let p = caret(state);
        (p, p + text.len() - 1)
    } else {
        let p = (caret(state) + 1).min(state.doc.len());
        (p, p + text.len() - 1)
    };
    TransactionSpec::new()
        .changes(Changes::insert(pos, text))
        .selection(Selection::caret(new_caret))
}

fn join_lines(state: &EditorState, vim: &mut VimState) -> TransactionSpec {
    vim.clear_pending();
    let line_end = motions::line_end(state, caret(state));
    if line_end >= state.doc.len() {
        return TransactionSpec::new();
    }
    // Replace `\n` + any leading whitespace on the next line with
    // a single space. Trailing-whitespace handling matches vim.
    let mut end = line_end + 1;
    let doc_str = state.doc.to_string();
    let bytes = doc_str.as_bytes();
    while end < bytes.len() && (bytes[end] == b' ' || bytes[end] == b'\t') {
        end += 1;
    }
    TransactionSpec::new()
        .changes(Changes::replace(line_end..end, " "))
        .selection(Selection::caret(line_end))
}

fn toggle_case_char(state: &EditorState, vim: &mut VimState) -> TransactionSpec {
    vim.clear_pending();
    let from = caret(state);
    let to = (from + 1).min(state.doc.len());
    if from == to {
        return TransactionSpec::new();
    }
    let ch = state.doc.slice(from..to);
    let flipped: String = ch
        .chars()
        .map(|c| {
            if c.is_ascii_uppercase() {
                c.to_ascii_lowercase()
            } else if c.is_ascii_lowercase() {
                c.to_ascii_uppercase()
            } else {
                c
            }
        })
        .collect();
    TransactionSpec::new()
        .changes(Changes::replace(from..to, flipped))
        .selection(Selection::caret(to))
}
