//! Built-in commands. These are intentionally tiny —
//! `fn(&EditorState) -> Option<TransactionSpec>` — so they're
//! testable in isolation and composable into any keymap.
//!
//! Mirrors `@codemirror/commands`. We add commands here as we
//! find we want them in the default keymap.

use crate::change::Changes;
use crate::selection::{Range, Selection};
use crate::state::EditorState;
use crate::transaction::TransactionSpec;

/// Select the entire document. Bound by convention to `Mod-a`.
pub fn select_all(state: &EditorState) -> Option<TransactionSpec> {
    Some(TransactionSpec::new().selection(Selection::single(Range::new(0, state.doc.len()))))
}

/// Insert a newline at the caret. If there's a non-empty
/// selection, replace it with `"\n"`. Bound by convention to
/// `Enter`.
pub fn insert_newline(state: &EditorState) -> Option<TransactionSpec> {
    let p = state.selection.primary();
    let (from, to) = (p.from(), p.to());
    Some(TransactionSpec::new().changes(Changes::replace(from..to, "\n")))
}

/// One unit of indentation. CM6 uses a configurable
/// `indentUnit` facet that defaults to two spaces; this is a
/// plain const for now and can be promoted to config later.
pub const INDENT_UNIT: &str = "  ";

/// Enter — but if the caret is on a list / task item, continue
/// the list on the next line. On an *empty* list item (marker
/// + whitespace only), instead remove the marker, exiting the
/// list. Ports CM6's `insertNewlineContinueMarkup`
/// (`lang-markdown/src/commands.ts:98`).
///
/// Falls back to a plain `\n` insert when the line isn't a list
/// item.
pub fn enter_continue_list(state: &EditorState) -> Option<TransactionSpec> {
    let p = state.selection.primary();
    let (from, to) = (p.from(), p.to());
    if from != to {
        // Non-empty selection — defer to plain newline insert.
        return insert_newline(state);
    }
    let doc = state.doc.to_string();
    let (line_from, line_to) = line_bounds(&doc, from);
    let line = &doc[line_from..line_to];
    let cont = match parse_list_continuation(line) {
        Some(c) => c,
        None => return insert_newline(state),
    };
    // Empty item: marker + (optional task box) + whitespace,
    // nothing after. Strip the marker, exit the list.
    let content_starts_at = line_from + cont.marker_end;
    if content_starts_at >= from {
        // Caret is on the marker itself or right after it; line
        // has no real content yet. Delete the marker.
        let changes = Changes::delete(line_from..line_from + cont.marker_end);
        return Some(
            TransactionSpec::new()
                .changes(changes)
                .selection(Selection::caret(line_from)),
        );
    }
    // Build the continuation marker. Tasks always start
    // unchecked on the next line.
    let mut marker = String::new();
    marker.push_str(&cont.indent);
    marker.push_str(&cont.bq_prefix);
    match cont.kind {
        ListKind::Bullet(c) => {
            marker.push(c);
            marker.push_str(&cont.after);
        }
        ListKind::Ordered(n) => {
            marker.push_str(&(n + 1).to_string());
            marker.push('.');
            marker.push_str(&cont.after);
        }
        ListKind::Blockquote => {
            // bq_prefix already contains the `>` chain (with
            // trailing space).
        }
    }
    if cont.task {
        marker.push_str("[ ] ");
    }
    let insert = format!("\n{marker}");
    let caret = from + insert.len();
    let mut all_changes: Vec<crate::change::Change> = vec![crate::change::Change {
        from,
        to,
        inserted: insert.clone(),
    }];
    // Ordered lists: bump each consecutive following item's
    // number by 1 so the inserted `(n+1).` doesn't duplicate the
    // existing one. Mirrors CM6's `renumberList`
    // (`lang-markdown/src/commands.ts:66`).
    if let ListKind::Ordered(n) = cont.kind {
        // The newly inserted item has number `n + 1`. Pass that
        // as the starting expected value so the renumber walk
        // matches the displaced old-`n+1` item first.
        all_changes.extend(renumber_following_ordered(&doc, line_to, n + 1));
    }
    Some(
        TransactionSpec::new()
            .changes(Changes::from_sorted(all_changes))
            .selection(Selection::caret(caret)),
    )
}

/// Walk lines starting at `after_pos` (must be at the start of
/// the line right after the one Enter was pressed on, i.e.
/// `line_to` of the current line — *before* the `\n`). For each
/// consecutive ordered-list item whose number equals the one we'd
/// expect from the unbroken sequence, emit a Change that bumps it
/// by `+1`. Stops on sequence break or non-list line.
fn renumber_following_ordered(
    doc: &str,
    after_line_to: usize,
    inserted_number: u32,
) -> Vec<crate::change::Change> {
    let mut out = Vec::new();
    let bytes = doc.as_bytes();
    // Skip the trailing `\n` of the current line.
    let mut i = if after_line_to < bytes.len() && bytes[after_line_to] == b'\n' {
        after_line_to + 1
    } else {
        return out;
    };
    let mut expected_old = inserted_number;
    while i < bytes.len() {
        let mut line_end = i;
        while line_end < bytes.len() && bytes[line_end] != b'\n' {
            line_end += 1;
        }
        let line = &doc[i..line_end];
        // Find leading whitespace + digits + `.`.
        let leading = line.bytes().take_while(|&b| b == b' ').count();
        let digit_start = i + leading;
        let mut digit_end = digit_start;
        while digit_end < line_end && bytes[digit_end].is_ascii_digit() {
            digit_end += 1;
        }
        if digit_end == digit_start || bytes.get(digit_end) != Some(&b'.') {
            break;
        }
        let n: u32 = match doc[digit_start..digit_end].parse() {
            Ok(v) => v,
            Err(_) => break,
        };
        if n != expected_old {
            break;
        }
        out.push(crate::change::Change {
            from: digit_start,
            to: digit_end,
            inserted: (n + 1).to_string(),
        });
        expected_old = n + 1;
        if line_end >= bytes.len() {
            break;
        }
        i = line_end + 1;
    }
    out
}

/// Indent the line(s) intersecting the primary selection by one
/// [`INDENT_UNIT`]. Ports CM6's `indentMore`
/// (`commands/src/commands.ts:906`).
pub fn indent_more(state: &EditorState) -> Option<TransactionSpec> {
    let doc = state.doc.to_string();
    let lines = selected_line_starts(state, &doc);
    if lines.is_empty() {
        return None;
    }
    let mut changes = Vec::with_capacity(lines.len());
    for &line_from in &lines {
        changes.push(crate::change::Change {
            from: line_from,
            to: line_from,
            inserted: INDENT_UNIT.to_string(),
        });
    }
    Some(TransactionSpec::new().changes(Changes::from_sorted(changes)))
}

/// Outdent — remove up to [`INDENT_UNIT`] worth of leading
/// whitespace from each selected line. Ports CM6's `indentLess`
/// (`commands/src/commands.ts:916`).
pub fn indent_less(state: &EditorState) -> Option<TransactionSpec> {
    let doc = state.doc.to_string();
    let lines = selected_line_starts(state, &doc);
    if lines.is_empty() {
        return None;
    }
    let unit = INDENT_UNIT.len();
    let mut changes = Vec::new();
    for &line_from in &lines {
        let bytes = doc.as_bytes();
        let mut leading = 0;
        while leading < unit
            && bytes.get(line_from + leading) == Some(&b' ')
        {
            leading += 1;
        }
        if leading > 0 {
            changes.push(crate::change::Change {
                from: line_from,
                to: line_from + leading,
                inserted: String::new(),
            });
        }
    }
    if changes.is_empty() {
        return None;
    }
    Some(TransactionSpec::new().changes(Changes::from_sorted(changes)))
}

// ── helpers ─────────────────────────────────────────────────

fn line_bounds(doc: &str, pos: usize) -> (usize, usize) {
    let bytes = doc.as_bytes();
    let mut start = pos.min(bytes.len());
    while start > 0 && bytes[start - 1] != b'\n' {
        start -= 1;
    }
    let mut end = pos.min(bytes.len());
    while end < bytes.len() && bytes[end] != b'\n' {
        end += 1;
    }
    (start, end)
}

fn selected_line_starts(state: &EditorState, doc: &str) -> Vec<usize> {
    let p = state.selection.primary();
    let (from, to) = (p.from(), p.to());
    let (first_line, _) = line_bounds(doc, from);
    let (last_line, _) = if to > from {
        // If selection ends exactly on a newline, don't include
        // the next line.
        let probe = if to > 0 && doc.as_bytes()[to - 1] == b'\n' {
            to - 1
        } else {
            to
        };
        line_bounds(doc, probe)
    } else {
        line_bounds(doc, from)
    };
    let mut out = Vec::new();
    let bytes = doc.as_bytes();
    let mut i = first_line;
    while i <= last_line {
        out.push(i);
        while i < bytes.len() && bytes[i] != b'\n' {
            i += 1;
        }
        if i >= bytes.len() {
            break;
        }
        i += 1;
    }
    out
}

#[derive(Debug, Clone, Copy)]
enum ListKind {
    Bullet(char),
    Ordered(u32),
    /// Blockquote line with no inner list marker. Carries the
    /// blockquote-nesting depth (`>` for 1, `> >` for 2, etc.)
    /// so Enter reproduces the same depth on the next line.
    Blockquote,
}

struct ListContinuation {
    /// Verbatim prefix to repeat on Enter (indentation, any `>`
    /// chain, list marker / task box, trailing space). For
    /// ordered lists the `(n+1).` substitution is done after
    /// reconstruction in [`enter_continue_list`]; this string
    /// keeps the *original* marker bytes.
    indent: String,
    /// Combined `>` / `> >` / `> > >` blockquote prefix the
    /// new line should start with — empty if the line wasn't a
    /// blockquote.
    bq_prefix: String,
    kind: ListKind,
    after: String,
    task: bool,
    marker_end: usize,
}

fn parse_list_continuation(line: &str) -> Option<ListContinuation> {
    let bytes = line.as_bytes();
    let leading = bytes.iter().take_while(|&&c| c == b' ').count();

    // Consume any leading `>` / `> ` chain (with optional space
    // after each `>`) — supports nested blockquotes and the
    // common `> - foo` "list inside a blockquote" pattern.
    let mut i = leading;
    let mut bq_prefix = String::new();
    while bytes.get(i) == Some(&b'>') {
        bq_prefix.push('>');
        i += 1;
        if bytes.get(i) == Some(&b' ') {
            bq_prefix.push(' ');
            i += 1;
        }
    }
    let after_indent_pos = i;

    // After any `>` chain, look for a list marker. If none,
    // the line is a plain blockquote (or plain text if no `>`
    // either, in which case we bail).
    let inner_bytes = &bytes[after_indent_pos..];
    let (kind, after_marker) = match inner_bytes.first() {
        Some(c @ (b'-' | b'*' | b'+')) => (ListKind::Bullet(*c as char), 1),
        Some(c) if c.is_ascii_digit() => {
            let n_end = inner_bytes
                .iter()
                .take_while(|&&x| x.is_ascii_digit())
                .count();
            if inner_bytes.get(n_end) != Some(&b'.') {
                if bq_prefix.is_empty() {
                    return None;
                }
                // Pure blockquote — no inner list.
                return Some(ListContinuation {
                    indent: " ".repeat(leading),
                    bq_prefix,
                    kind: ListKind::Blockquote,
                    after: String::new(),
                    task: false,
                    marker_end: after_indent_pos,
                });
            }
            let n: u32 = std::str::from_utf8(&inner_bytes[..n_end])
                .ok()?
                .parse()
                .ok()?;
            (ListKind::Ordered(n), n_end + 1)
        }
        _ => {
            if bq_prefix.is_empty() {
                return None;
            }
            // Pure blockquote.
            return Some(ListContinuation {
                indent: " ".repeat(leading),
                bq_prefix,
                kind: ListKind::Blockquote,
                after: String::new(),
                task: false,
                marker_end: after_indent_pos,
            });
        }
    };
    let inner_start = after_indent_pos;
    let after_marker_abs = inner_start + after_marker;

    // Whitespace after the list marker.
    let ws_count = bytes[after_marker_abs..]
        .iter()
        .take_while(|&&x| x == b' ')
        .count();
    if ws_count == 0 && bytes.len() > after_marker_abs {
        // A bare `-foo` is not a list — bail (unless we're
        // already committed to a blockquote with valid markers
        // — but then we'd have returned earlier).
        if bq_prefix.is_empty() {
            return None;
        }
        return Some(ListContinuation {
            indent: " ".repeat(leading),
            bq_prefix,
            kind: ListKind::Blockquote,
            after: String::new(),
            task: false,
            marker_end: after_indent_pos,
        });
    }
    let after = " ".repeat(ws_count.max(1));
    let mut marker_end = after_marker_abs + ws_count;

    // Optional task box `[ ]` / `[x]`.
    let task = bytes
        .get(marker_end..marker_end + 3)
        .map(|sl| sl.len() == 3 && sl[0] == b'[' && sl[2] == b']' && matches!(sl[1], b' ' | b'x' | b'X'))
        .unwrap_or(false);
    if task {
        marker_end += 3;
        if bytes.get(marker_end) == Some(&b' ') {
            marker_end += 1;
        }
    }
    Some(ListContinuation {
        indent: " ".repeat(leading),
        bq_prefix,
        kind,
        after,
        task,
        marker_end,
    })
}

/// Delete the character before the caret. With a non-empty
/// selection, deletes the selection. Bound by convention to
/// `Backspace`.
pub fn delete_backward(state: &EditorState) -> Option<TransactionSpec> {
    let p = state.selection.primary();
    let (from, to) = (p.from(), p.to());
    if from != to {
        return Some(TransactionSpec::new().changes(Changes::delete(from..to)));
    }
    if from == 0 {
        return None;
    }
    // For now we step one byte. A future commit will step by
    // grapheme cluster so we don't split multi-byte chars.
    Some(TransactionSpec::new().changes(Changes::delete(from - 1..from)))
}

/// Toggle bold markdown markers (`**…**`) at the caret /
/// around the current selection. Behavior:
///
/// - **Empty caret, doc[caret..] starts with `**`**: caret is
///   sitting just before a closing marker (typical "I'm done
///   typing bold content" case). Skip past it — no doc change,
///   just move the caret +2.
/// - **Empty caret elsewhere**: insert `****` and park the
///   caret between the markers, so subsequent typing goes
///   inside the bold span.
/// - **Non-empty selection**: wrap the selection with `**…**`,
///   keeping the wrapped range selected.
///
/// Bound by convention to `Mod-b`.
pub fn toggle_bold(state: &EditorState) -> Option<TransactionSpec> {
    toggle_marker(state, "**")
}

/// Same as [`toggle_bold`] but with single `*…*` for italic.
/// Bound to `Mod-i`.
pub fn toggle_italic(state: &EditorState) -> Option<TransactionSpec> {
    toggle_marker(state, "*")
}

fn toggle_marker(state: &EditorState, marker: &str) -> Option<TransactionSpec> {
    let sel = state.selection.primary();
    let from = sel.from();
    let to = sel.to();
    let doc = state.doc.to_string();
    let m = marker;
    let mlen = m.len();

    if from == to {
        // Empty caret. If the next bytes are the marker, skip
        // past it — closes an open span the user just filled.
        if doc.get(from..).map_or(false, |s| s.starts_with(m)) {
            return Some(TransactionSpec::new().selection(Selection::caret(from + mlen)));
        }
        // Open a new span: insert "marker + marker" with caret
        // in the middle.
        let pair = format!("{m}{m}");
        return Some(
            TransactionSpec::new()
                .changes(Changes::insert(from, pair))
                .selection(Selection::caret(from + mlen)),
        );
    }
    // Wrap the selection.
    let selected = doc.get(from..to).unwrap_or("");
    let wrapped = format!("{m}{selected}{m}");
    let new_to = from + wrapped.len();
    Some(
        TransactionSpec::new()
            .changes(Changes::replace(from..to, wrapped))
            .selection(Selection::single(Range::new(from, new_to))),
    )
}

/// Delete the character after the caret. With a non-empty
/// selection, deletes the selection. Bound by convention to
/// `Delete`.
pub fn delete_forward(state: &EditorState) -> Option<TransactionSpec> {
    let p = state.selection.primary();
    let (from, to) = (p.from(), p.to());
    if from != to {
        return Some(TransactionSpec::new().changes(Changes::delete(from..to)));
    }
    if to >= state.doc.len() {
        return None;
    }
    Some(TransactionSpec::new().changes(Changes::delete(to..to + 1)))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn at(text: &str, caret: usize) -> EditorState {
        let mut s = EditorState::new(text);
        s.selection = Selection::caret(caret);
        s
    }

    #[test]
    fn enter_continues_bullet_list() {
        let s = at("- foo", 5);
        let next = s.update(enter_continue_list(&s).unwrap());
        assert_eq!(next.doc.to_string(), "- foo\n- ");
        assert_eq!(next.selection.primary().head, 8);
    }

    #[test]
    fn enter_continues_ordered_list_increments() {
        let s = at("1. foo", 6);
        let next = s.update(enter_continue_list(&s).unwrap());
        assert_eq!(next.doc.to_string(), "1. foo\n2. ");
        assert_eq!(next.selection.primary().head, 10);
    }

    #[test]
    fn enter_continues_blockquote() {
        let s = at("> quoted", 8);
        let next = s.update(enter_continue_list(&s).unwrap());
        assert_eq!(next.doc.to_string(), "> quoted\n> ");
        assert_eq!(next.selection.primary().head, 11);
    }

    #[test]
    fn enter_continues_nested_blockquote() {
        let s = at("> > deep", 8);
        let next = s.update(enter_continue_list(&s).unwrap());
        assert_eq!(next.doc.to_string(), "> > deep\n> > ");
    }

    #[test]
    fn enter_on_empty_blockquote_exits() {
        let s = at("> ", 2);
        let next = s.update(enter_continue_list(&s).unwrap());
        assert_eq!(next.doc.to_string(), "");
    }

    #[test]
    fn enter_continues_list_inside_blockquote() {
        let s = at("> - item", 8);
        let next = s.update(enter_continue_list(&s).unwrap());
        assert_eq!(next.doc.to_string(), "> - item\n> - ");
    }

    #[test]
    fn enter_renumbers_subsequent_ordered_items() {
        let s = at("1. one\n2. two\n3. three", 6);
        let next = s.update(enter_continue_list(&s).unwrap());
        assert_eq!(next.doc.to_string(), "1. one\n2. \n3. two\n4. three");
        // Caret right after the new `2. ` marker.
        assert_eq!(next.selection.primary().head, 10);
    }

    #[test]
    fn enter_renumber_stops_at_sequence_break() {
        let s = at("1. one\n2. two\n5. five", 6);
        let next = s.update(enter_continue_list(&s).unwrap());
        // Only the first following item gets bumped; the `5.`
        // stays untouched because the sequence already broke.
        assert_eq!(next.doc.to_string(), "1. one\n2. \n3. two\n5. five");
    }

    #[test]
    fn enter_on_empty_list_item_exits_list() {
        let s = at("- ", 2);
        let next = s.update(enter_continue_list(&s).unwrap());
        assert_eq!(next.doc.to_string(), "");
        assert_eq!(next.selection.primary().head, 0);
    }

    #[test]
    fn enter_continues_task_unchecked_after_checked() {
        let s = at("- [x] done", 10);
        let next = s.update(enter_continue_list(&s).unwrap());
        assert_eq!(next.doc.to_string(), "- [x] done\n- [ ] ");
        assert_eq!(next.selection.primary().head, 17);
    }

    #[test]
    fn enter_outside_list_falls_back_to_newline() {
        let s = at("plain", 5);
        let next = s.update(enter_continue_list(&s).unwrap());
        assert_eq!(next.doc.to_string(), "plain\n");
    }

    #[test]
    fn indent_more_inserts_two_spaces() {
        let s = at("foo", 1);
        let next = s.update(indent_more(&s).unwrap());
        assert_eq!(next.doc.to_string(), "  foo");
    }

    #[test]
    fn indent_less_removes_leading_pair() {
        let s = at("  foo", 2);
        let next = s.update(indent_less(&s).unwrap());
        assert_eq!(next.doc.to_string(), "foo");
    }

    #[test]
    fn indent_less_at_zero_is_noop() {
        let s = at("foo", 0);
        assert!(indent_less(&s).is_none());
    }

    #[test]
    fn indent_more_across_selection_indents_each_line() {
        let mut s = EditorState::new("a\nb\nc");
        s.selection = Selection::single(Range::new(0, 5));
        let next = s.update(indent_more(&s).unwrap());
        assert_eq!(next.doc.to_string(), "  a\n  b\n  c");
    }

    #[test]
    fn select_all_covers_doc() {
        let s = EditorState::new("hello");
        let spec = select_all(&s).unwrap();
        let next = s.update(spec);
        let p = next.selection.primary();
        assert_eq!(p.from(), 0);
        assert_eq!(p.to(), 5);
    }

    #[test]
    fn delete_backward_at_pos_5() {
        let mut s = EditorState::new("hello");
        s.selection = Selection::caret(5);
        let next = s.update(delete_backward(&s).unwrap());
        assert_eq!(next.doc.to_string(), "hell");
        assert_eq!(next.selection.primary().head, 4);
    }

    #[test]
    fn delete_backward_at_start_is_noop() {
        let mut s = EditorState::new("hello");
        s.selection = Selection::caret(0);
        assert!(delete_backward(&s).is_none());
    }

    #[test]
    fn delete_backward_with_selection_deletes_range() {
        let mut s = EditorState::new("hello");
        s.selection = Selection::single(Range::new(1, 4));
        let next = s.update(delete_backward(&s).unwrap());
        assert_eq!(next.doc.to_string(), "ho");
    }

    #[test]
    fn toggle_bold_with_empty_caret_inserts_pair() {
        let mut s = EditorState::new("Testing ");
        s.selection = Selection::caret(8);
        let next = s.update(toggle_bold(&s).unwrap());
        assert_eq!(next.doc.to_string(), "Testing ****");
        // Caret parked between the markers.
        assert_eq!(next.selection.primary().head, 10);
        assert_eq!(next.selection.primary().anchor, 10);
    }

    #[test]
    fn toggle_bold_skips_past_closing_marker() {
        // "Testing **bold**" with caret at 14 (just after
        // "bold", before closing "**"). Pressing toggle_bold
        // should move caret to 16 without changing doc.
        let mut s = EditorState::new("Testing **bold**");
        s.selection = Selection::caret(14);
        let next = s.update(toggle_bold(&s).unwrap());
        assert_eq!(next.doc.to_string(), "Testing **bold**"); // unchanged
        assert_eq!(next.selection.primary().head, 16);
    }

    #[test]
    fn toggle_bold_wraps_selection() {
        let mut s = EditorState::new("Make this bold");
        s.selection = Selection::single(Range::new(5, 9)); // "this"
        let next = s.update(toggle_bold(&s).unwrap());
        assert_eq!(next.doc.to_string(), "Make **this** bold");
        let p = next.selection.primary();
        assert_eq!(p.from(), 5);
        assert_eq!(p.to(), 13); // covers **this**
    }

    #[test]
    fn toggle_italic_uses_single_marker() {
        let mut s = EditorState::new("foo");
        s.selection = Selection::caret(3);
        let next = s.update(toggle_italic(&s).unwrap());
        assert_eq!(next.doc.to_string(), "foo**");
        assert_eq!(next.selection.primary().head, 4);
    }
}
