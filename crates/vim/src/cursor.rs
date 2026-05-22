//! Document cursor model — `(block_id, offset)` pairs that
//! survive across block boundaries.
//!
//! Designed for multi-cursor work eventually: state is a
//! `Vec<Cursor>` with a primary index. v1 always carries
//! exactly one cursor; the multi-cursor surface is reserved.
//!
//! All motion logic is pure: `apply_motion(cursor, motion,
//! &doc_view) -> Cursor`. The host plugs in a `DocView` impl
//! that knows how to look up block content + walk siblings;
//! the engine doesn't bind to any storage layer.

use uuid::Uuid;

use crate::action::Motion;

/// Single cursor location. `offset` is a byte offset into the
/// owning block's `content` string (UTF-8-aware via `floor_char_boundary`).
///
/// `preferred_col` is vim's "sticky column" — when set, vertical
/// motions (`j` / `k`) try to restore this column on each line
/// instead of using the current (clamped) column. Set when a
/// vertical motion runs over a short line; cleared by any
/// horizontal motion. Logged in **byte** units to stay consistent
/// with `offset`; for monospace text this matches visual columns
/// 1:1.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Cursor {
    pub block_id: Uuid,
    pub offset: usize,
    pub preferred_col: Option<usize>,
}

impl Cursor {
    /// Convenience for the common case (no sticky column).
    #[must_use]
    pub fn new(block_id: Uuid, offset: usize) -> Self {
        Self {
            block_id,
            offset,
            preferred_col: None,
        }
    }
}

/// Multi-cursor state. v1 invariant: `cursors.len() == 1` and
/// `primary == 0`. Surface kept plural so future multi-cursor
/// work doesn't have to refactor every consumer.
///
/// `anchor` is the selection start in Visual mode. `None` outside
/// Visual mode (Normal/Insert have no selection). Set when the
/// host emits `EnterVisual`, cleared on `EnterNormal`. While set,
/// motions move the primary; the anchor stays put — that's what
/// makes the selection grow.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CursorState {
    pub cursors: Vec<Cursor>,
    pub primary: usize,
    pub anchor: Option<Cursor>,
}

impl CursorState {
    #[must_use]
    pub fn single(c: Cursor) -> Self {
        Self {
            cursors: vec![c],
            primary: 0,
            anchor: None,
        }
    }

    /// The cursor the user is "on" — always present.
    #[must_use]
    pub fn primary(&self) -> Cursor {
        self.cursors[self.primary.min(self.cursors.len() - 1)]
    }

    pub fn set_primary(&mut self, c: Cursor) {
        let i = self.primary.min(self.cursors.len() - 1);
        self.cursors[i] = c;
    }

    /// Pin the anchor at the current primary (entering Visual).
    pub fn begin_selection(&mut self) {
        self.anchor = Some(self.primary());
    }

    /// Drop the anchor (exiting Visual).
    pub fn clear_selection(&mut self) {
        self.anchor = None;
    }
}

/// Host-supplied view of the document so the motion engine can
/// stay storage-agnostic. Implement once at the route level
/// (knowledge-ui's `PageBody`) over your snapshot.
pub trait DocView {
    /// Block content for `id`. `None` if the block is unknown.
    fn block_content(&self, id: Uuid) -> Option<String>;
    /// The block immediately *before* `id` in visible reading
    /// order (DFS over the outliner). `None` if `id` is the
    /// first block on the page.
    fn prev_visible(&self, id: Uuid) -> Option<Uuid>;
    /// The block immediately *after* `id` in visible reading
    /// order. `None` if `id` is the last block on the page.
    fn next_visible(&self, id: Uuid) -> Option<Uuid>;
}

// ── Helpers ───────────────────────────────────────────────────────────

/// Round a byte offset down to the nearest UTF-8 char boundary.
/// `String::is_char_boundary` is `O(1)`; loop is bounded by the
/// max UTF-8 codepoint width (4 bytes).
fn snap_to_boundary(s: &str, mut byte: usize) -> usize {
    if byte > s.len() {
        byte = s.len();
    }
    while byte > 0 && !s.is_char_boundary(byte) {
        byte -= 1;
    }
    byte
}

/// `(line_index, column_byte_offset_within_line)` for `offset`
/// in `content`. Column is byte-offset, not visual column —
/// good enough for monospace + matches how textarea selection
/// reports.
#[must_use]
pub fn line_col(content: &str, offset: usize) -> (usize, usize) {
    let off = snap_to_boundary(content, offset);
    let head = &content[..off];
    let line = head.bytes().filter(|b| *b == b'\n').count();
    let col = head.rfind('\n').map_or(off, |i| off - i - 1);
    (line, col)
}

/// Inverse: given a target `(line, col)` and content, return
/// the byte offset on that line clamped to its end.
#[must_use]
pub fn offset_at(content: &str, line: usize, col: usize) -> usize {
    let mut start = 0usize;
    let mut current_line = 0usize;
    for (i, byte) in content.bytes().enumerate() {
        if current_line == line {
            break;
        }
        if byte == b'\n' {
            current_line += 1;
            start = i + 1;
        }
    }
    if current_line < line {
        // Past the last line — clamp to end.
        return content.len();
    }
    let line_end = content[start..]
        .find('\n')
        .map_or(content.len(), |i| start + i);
    let line_slice = &content[start..line_end];
    let target = start + col.min(line_slice.len());
    snap_to_boundary(content, target)
}

fn line_count(content: &str) -> usize {
    content.bytes().filter(|b| *b == b'\n').count() + 1
}

fn line_at(content: &str, line: usize) -> &str {
    let mut start = 0usize;
    let mut current = 0usize;
    for (i, byte) in content.bytes().enumerate() {
        if current == line {
            break;
        }
        if byte == b'\n' {
            current += 1;
            start = i + 1;
        }
    }
    if current < line {
        return "";
    }
    let end = content[start..]
        .find('\n')
        .map_or(content.len(), |i| start + i);
    &content[start..end]
}

// ── Motion compute ────────────────────────────────────────────────────

/// Compute a new cursor from a motion. Pure — `view` provides
/// the read-side of the document model; the function never
/// mutates anything.
pub fn apply_motion<V: DocView>(cursor: Cursor, motion: Motion, view: &V) -> Cursor {
    let content = view.block_content(cursor.block_id).unwrap_or_default();
    match motion {
        Motion::CharLeft => move_char_left(cursor, &content, view),
        Motion::CharRight => move_char_right(cursor, &content, view),
        Motion::LineDown => move_line_down(cursor, &content, view),
        Motion::LineUp => move_line_up(cursor, &content, view),
        Motion::LineStart => move_line_start(cursor, &content),
        Motion::LineEnd => move_line_end(cursor, &content),
        Motion::WordForward => move_word_forward(cursor, &content),
        Motion::WordBackward => move_word_backward(cursor, &content),
        Motion::WordEnd => move_word_end(cursor, &content),
        // BIG/word distinction stubbed to char-class for v1 —
        // good enough behavior for paragraphs of text; refine
        // once we add full whitespace-only segmentation.
        Motion::BigWordForward => move_word_forward(cursor, &content),
        Motion::BigWordBackward => move_word_backward(cursor, &content),
        Motion::BigWordEnd => move_word_end(cursor, &content),
        Motion::DocStart | Motion::DocEnd => {
            // Doc-scope motions need full-page knowledge; the
            // host handles them by walking visible blocks itself.
            // Here we no-op (caller filters before calling).
            cursor
        }
        Motion::FindChar {
            ch,
            direction,
            till,
        } => find_char(cursor, &content, ch, direction, till),
        // SearchNext / SearchPrev resolve against the host's stored
        // last query — the cursor module has no doc-scope context,
        // so we no-op here and let the host handle them.
        Motion::SearchNext | Motion::SearchPrev => cursor,
    }
}

/// `f`/`F`/`t`/`T` core. Searches the current line for `ch`
/// (forward when `direction == 1`, backward when `-1`) and
/// returns the new cursor offset. `till = true` stops one char
/// shy of the target (`t`/`T` behavior). No-op when the char
/// isn't found on this line.
fn find_char(c: Cursor, content: &str, ch: char, direction: i8, till: bool) -> Cursor {
    let off = c.offset.min(content.len());
    // Bound search to the current line.
    let line_start = content[..off].rfind('\n').map_or(0, |i| i + 1);
    let line_end = content[off..].find('\n').map_or(content.len(), |i| off + i);
    let new_off = if direction >= 0 {
        let search_from = off.saturating_add(1).min(line_end);
        content[search_from..line_end]
            .char_indices()
            .find(|(_, cc)| *cc == ch)
            .map(|(i, _)| search_from + i)
            .map(|i| if till { i.saturating_sub(1) } else { i })
    } else {
        let head = &content[line_start..off];
        head.char_indices()
            .rev()
            .find(|(_, cc)| *cc == ch)
            .map(|(i, _)| line_start + i)
            .map(|i| if till { i + ch.len_utf8() } else { i })
    };
    match new_off {
        Some(o) => Cursor {
            offset: o,
            preferred_col: None,
            ..c
        },
        None => c,
    }
}

fn move_char_left<V: DocView>(c: Cursor, content: &str, view: &V) -> Cursor {
    if c.offset == 0 {
        if let Some(prev) = view.prev_visible(c.block_id) {
            let prev_content = view.block_content(prev).unwrap_or_default();
            return Cursor {
                block_id: prev,
                offset: prev_content.len(),
                preferred_col: None,
            };
        }
        return Cursor {
            preferred_col: None,
            ..c
        };
    }
    let mut new = c.offset;
    new -= 1;
    new = snap_to_boundary(content, new);
    Cursor {
        offset: new,
        preferred_col: None,
        ..c
    }
}

fn move_char_right<V: DocView>(c: Cursor, content: &str, view: &V) -> Cursor {
    if c.offset >= content.len() {
        if let Some(next) = view.next_visible(c.block_id) {
            return Cursor {
                block_id: next,
                offset: 0,
                preferred_col: None,
            };
        }
        return Cursor {
            preferred_col: None,
            ..c
        };
    }
    let mut new = c.offset + 1;
    new = snap_to_boundary(content, new);
    Cursor {
        offset: new,
        preferred_col: None,
        ..c
    }
}

fn move_line_down<V: DocView>(c: Cursor, content: &str, view: &V) -> Cursor {
    // Sticky column: use the remembered preferred col when set,
    // else seed it from where the cursor currently sits. The
    // preference rides along with the cursor across vertical
    // motions until a horizontal motion clears it.
    let (_line, current_col) = line_col(content, c.offset);
    let target_col = c.preferred_col.unwrap_or(current_col);
    let preferred = Some(target_col);
    let total = line_count(content);
    let line = line_col(content, c.offset).0;
    if line + 1 < total {
        let new = offset_at(content, line + 1, target_col);
        return Cursor {
            offset: new,
            preferred_col: preferred,
            ..c
        };
    }
    // At last line of this block — drop into next block at
    // matching column on its first line.
    if let Some(next) = view.next_visible(c.block_id) {
        let next_content = view.block_content(next).unwrap_or_default();
        let new = offset_at(&next_content, 0, target_col);
        return Cursor {
            block_id: next,
            offset: new,
            preferred_col: preferred,
        };
    }
    Cursor {
        preferred_col: preferred,
        ..c
    }
}

fn move_line_up<V: DocView>(c: Cursor, content: &str, view: &V) -> Cursor {
    let (line, current_col) = line_col(content, c.offset);
    let target_col = c.preferred_col.unwrap_or(current_col);
    let preferred = Some(target_col);
    if line > 0 {
        let new = offset_at(content, line - 1, target_col);
        return Cursor {
            offset: new,
            preferred_col: preferred,
            ..c
        };
    }
    if let Some(prev) = view.prev_visible(c.block_id) {
        let prev_content = view.block_content(prev).unwrap_or_default();
        let last_line = line_count(&prev_content) - 1;
        let new = offset_at(&prev_content, last_line, target_col);
        return Cursor {
            block_id: prev,
            offset: new,
            preferred_col: preferred,
        };
    }
    Cursor {
        preferred_col: preferred,
        ..c
    }
}

fn move_line_start(c: Cursor, content: &str) -> Cursor {
    let (line, _) = line_col(content, c.offset);
    let new = offset_at(content, line, 0);
    Cursor {
        offset: new,
        preferred_col: None,
        ..c
    }
}

fn move_line_end(c: Cursor, content: &str) -> Cursor {
    let (line, _) = line_col(content, c.offset);
    let line_text = line_at(content, line);
    let new = offset_at(content, line, line_text.len());
    Cursor {
        offset: new,
        preferred_col: None,
        ..c
    }
}

fn is_word_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn move_word_forward(c: Cursor, content: &str) -> Cursor {
    let bytes = content.as_bytes();
    let mut i = c.offset.min(content.len());
    let chars: Vec<(usize, char)> = content[i..]
        .char_indices()
        .map(|(o, ch)| (i + o, ch))
        .collect();
    let _ = bytes;
    // Skip current word, then skip whitespace, land on first
    // char of next word.
    let mut iter = chars.into_iter().peekable();
    if let Some(&(_, first)) = iter.peek() {
        let in_word = is_word_char(first);
        while let Some(&(_, ch)) = iter.peek() {
            if is_word_char(ch) != in_word {
                break;
            }
            iter.next();
        }
    }
    while let Some(&(_, ch)) = iter.peek() {
        if !ch.is_whitespace() {
            break;
        }
        iter.next();
    }
    if let Some((pos, _)) = iter.peek() {
        i = *pos;
    } else {
        i = content.len();
    }
    Cursor {
        offset: i,
        preferred_col: None,
        ..c
    }
}

fn move_word_backward(c: Cursor, content: &str) -> Cursor {
    let mut i = c.offset.min(content.len());
    if i == 0 {
        return c;
    }
    // Walk back past whitespace, then back past the current word.
    let mut prev_chars: Vec<(usize, char)> = content[..i].char_indices().collect();
    while let Some((_, ch)) = prev_chars.last() {
        if !ch.is_whitespace() {
            break;
        }
        prev_chars.pop();
    }
    let in_word = prev_chars.last().is_none_or(|(_, ch)| is_word_char(*ch));
    while let Some((_, ch)) = prev_chars.last() {
        if is_word_char(*ch) != in_word {
            break;
        }
        prev_chars.pop();
    }
    i = prev_chars.last().map_or(0, |(pos, _)| *pos);
    // We landed on the char BEFORE the boundary; bump to the
    // first char of the word we walked over.
    if !prev_chars.is_empty() {
        i = prev_chars
            .last()
            .and_then(|(p, _)| {
                let after = &content[*p..];
                after.char_indices().nth(1).map(|(o, _)| *p + o)
            })
            .unwrap_or(i);
    }
    Cursor {
        offset: i,
        preferred_col: None,
        ..c
    }
}

fn move_word_end(c: Cursor, content: &str) -> Cursor {
    let mut iter = content[c.offset..]
        .char_indices()
        .map(|(o, ch)| (c.offset + o, ch))
        .peekable();
    // Skip whitespace, then walk to end of current word.
    while let Some(&(_, ch)) = iter.peek() {
        if !ch.is_whitespace() {
            break;
        }
        iter.next();
    }
    let in_word = iter.peek().map(|(_, ch)| is_word_char(*ch));
    let Some(in_word) = in_word else {
        return Cursor {
            offset: content.len(),
            ..c
        };
    };
    let mut last_pos = c.offset;
    while let Some(&(pos, ch)) = iter.peek() {
        if is_word_char(ch) != in_word {
            break;
        }
        last_pos = pos + ch.len_utf8();
        iter.next();
    }
    Cursor {
        offset: last_pos,
        ..c
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    struct Doc {
        blocks: HashMap<Uuid, String>,
        order: Vec<Uuid>,
    }
    impl DocView for Doc {
        fn block_content(&self, id: Uuid) -> Option<String> {
            self.blocks.get(&id).cloned()
        }
        fn prev_visible(&self, id: Uuid) -> Option<Uuid> {
            let i = self.order.iter().position(|x| *x == id)?;
            if i == 0 {
                None
            } else {
                Some(self.order[i - 1])
            }
        }
        fn next_visible(&self, id: Uuid) -> Option<Uuid> {
            let i = self.order.iter().position(|x| *x == id)?;
            self.order.get(i + 1).copied()
        }
    }

    fn make_doc(rows: &[&str]) -> (Doc, Vec<Uuid>) {
        let mut blocks = HashMap::new();
        let mut order = Vec::new();
        for s in rows {
            let id = Uuid::new_v4();
            blocks.insert(id, s.to_string());
            order.push(id);
        }
        (
            Doc {
                blocks,
                order: order.clone(),
            },
            order,
        )
    }

    #[test]
    fn line_col_basics() {
        let s = "abc\ndef\nghi";
        assert_eq!(line_col(s, 0), (0, 0));
        assert_eq!(line_col(s, 3), (0, 3));
        assert_eq!(line_col(s, 4), (1, 0));
        assert_eq!(line_col(s, 6), (1, 2));
    }

    #[test]
    fn h_within_block() {
        let (doc, ids) = make_doc(&["hello"]);
        let c = Cursor::new(ids[0], 3);
        let out = apply_motion(c, Motion::CharLeft, &doc);
        assert_eq!(out.offset, 2);
    }

    #[test]
    fn h_at_offset_zero_jumps_to_prev_block_end() {
        let (doc, ids) = make_doc(&["abc", "def"]);
        let c = Cursor::new(ids[1], 0);
        let out = apply_motion(c, Motion::CharLeft, &doc);
        assert_eq!(out.block_id, ids[0]);
        assert_eq!(out.offset, 3, "should land at end of prev block");
    }

    #[test]
    fn l_at_end_jumps_to_next_block_start() {
        let (doc, ids) = make_doc(&["abc", "def"]);
        let c = Cursor::new(ids[0], 3);
        let out = apply_motion(c, Motion::CharRight, &doc);
        assert_eq!(out.block_id, ids[1]);
        assert_eq!(out.offset, 0);
    }

    #[test]
    fn j_within_multiline_block() {
        let (doc, ids) = make_doc(&["line one\nline two\nline three"]);
        let c = Cursor::new(ids[0], 5); // mid-"line one"
        let out = apply_motion(c, Motion::LineDown, &doc);
        assert_eq!(out.block_id, ids[0]);
        // Same column on next line.
        assert_eq!(out.offset, "line one\n".len() + 5);
    }

    #[test]
    fn j_at_last_line_jumps_to_next_block() {
        let (doc, ids) = make_doc(&["solo", "next"]);
        let c = Cursor::new(ids[0], 2);
        let out = apply_motion(c, Motion::LineDown, &doc);
        assert_eq!(out.block_id, ids[1]);
        assert_eq!(out.offset, 2, "preserves column");
    }

    #[test]
    fn k_within_multiline_block() {
        let (doc, ids) = make_doc(&["a\nbb\nccc"]);
        let c = Cursor::new(ids[0], 6); // mid-"ccc"
        let out = apply_motion(c, Motion::LineUp, &doc);
        // From line 2 col 1 → line 1 col 1.
        assert_eq!(out.offset, 3);
    }

    #[test]
    fn sticky_column_restores_after_short_line() {
        // line 0: "longer text" (col 5 = 'r')
        // line 1: "x"            (col 0 only — clamps)
        // line 2: "another long" (col 5 should be restored)
        let (doc, ids) = make_doc(&["longer text\nx\nanother long"]);
        let c = Cursor::new(ids[0], 5); // col 5 on line 0
        // j → line 1, clamps to col 1 (end of "x")
        let down1 = apply_motion(c, Motion::LineDown, &doc);
        assert_eq!(down1.offset, "longer text\n".len() + 1);
        assert_eq!(down1.preferred_col, Some(5));
        // j again → line 2, sticky col restores to 5.
        let down2 = apply_motion(down1, Motion::LineDown, &doc);
        assert_eq!(down2.offset, "longer text\nx\n".len() + 5);
        assert_eq!(down2.preferred_col, Some(5));
    }

    #[test]
    fn horizontal_motion_clears_sticky_column() {
        let (doc, ids) = make_doc(&["abcdef\nx\nabcdef"]);
        let c = Cursor::new(ids[0], 5);
        let down = apply_motion(c, Motion::LineDown, &doc);
        assert_eq!(down.preferred_col, Some(5));
        // h clears the preference; subsequent j uses the new
        // (clamped) col, not the old preference.
        let left = apply_motion(down, Motion::CharLeft, &doc);
        assert_eq!(left.preferred_col, None);
        let down2 = apply_motion(left, Motion::LineDown, &doc);
        assert_eq!(down2.offset, "abcdef\nx\n".len(), "col 0, not 5");
    }

    #[test]
    fn dollar_to_line_end() {
        let (doc, ids) = make_doc(&["abc\ndefgh"]);
        let c = Cursor::new(ids[0], 5);
        let out = apply_motion(c, Motion::LineEnd, &doc);
        assert_eq!(out.offset, 9);
    }

    #[test]
    fn zero_to_line_start() {
        let (doc, ids) = make_doc(&["abc\ndefgh"]);
        let c = Cursor::new(ids[0], 7);
        let out = apply_motion(c, Motion::LineStart, &doc);
        assert_eq!(out.offset, 4);
    }

    #[test]
    fn w_skips_to_next_word() {
        let (doc, ids) = make_doc(&["hello world foo"]);
        let c = Cursor::new(ids[0], 0);
        let out = apply_motion(c, Motion::WordForward, &doc);
        assert_eq!(out.offset, 6);
    }

    #[test]
    fn b_walks_to_word_start() {
        let (doc, ids) = make_doc(&["hello world"]);
        let c = Cursor::new(ids[0], 9); // mid-"world"
        let out = apply_motion(c, Motion::WordBackward, &doc);
        assert_eq!(out.offset, 6, "start of 'world'");
    }

    #[test]
    fn cursor_state_primary_roundtrip() {
        let id = Uuid::new_v4();
        let mut s = CursorState::single(Cursor::new(id, 0));
        assert_eq!(s.primary().offset, 0);
        s.set_primary(Cursor::new(id, 5));
        assert_eq!(s.primary().offset, 5);
    }

    #[test]
    fn selection_anchor_lifecycle() {
        let id = Uuid::new_v4();
        let mut s = CursorState::single(Cursor::new(id, 3));
        assert!(s.anchor.is_none());
        s.begin_selection();
        assert_eq!(s.anchor.map(|c| c.offset), Some(3));
        // Move primary; anchor stays at 3.
        s.set_primary(Cursor::new(id, 7));
        assert_eq!(s.primary().offset, 7);
        assert_eq!(s.anchor.map(|c| c.offset), Some(3));
        s.clear_selection();
        assert!(s.anchor.is_none());
    }
}
