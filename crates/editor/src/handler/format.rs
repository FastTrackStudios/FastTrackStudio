//! Inline-format keyboard shortcuts. Ports Logseq's
//! `frontend.handler.editor/format-text!` algorithm (see
//! `~/Development/research/logseq/src/main/frontend/handler/editor.cljs`
//! around line 140).
//!
//! Behavior, line-by-line from the original:
//!
//! 1. Read `selection-start` and `selection-end` from the textarea.
//! 2. The chars immediately *before* `selection-start` (length =
//!    `count pattern`) and immediately *after* `selection-end`
//!    are compared to `pattern`. If both match, the selection is
//!    already wrapped and the action toggles the wrapping off
//!    (`already-wrapped?` branch).
//! 3. Otherwise the action wraps `pattern + selection + pattern`
//!    around the current selection (which may be empty).
//! 4. Cursor:
//!    - already-wrapped → keep the selection but shifted back by
//!      `pattern-count` on both ends,
//!    - non-empty selection → move the cursor past the closer
//!      (`selection-end + pattern-count`),
//!    - empty selection (the Cmd-B-with-no-selection case) →
//!      select the empty range `[start + pattern-count,
//!      end + pattern-count]` which lands the caret between the
//!      two inserted markers.
//!
//! The shortcut keys themselves are wired in
//! `components::EditableBlock::on_keydown`.

/// Result of applying a format shortcut. `selection` is the new
/// `(start, end)` for the textarea; equal values = single caret.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FormatResult {
    pub new_value: String,
    pub selection: (usize, usize),
}

/// Pure implementation of Logseq's `format-text!`. Wraps (or
/// unwraps) the current selection with `pattern`.
///
/// Caller pulls `value`, `selection_start`, `selection_end` from
/// the live textarea; the result is fed back to
/// `update_block_content` + a `setSelectionRange` on the next
/// render frame.
pub fn format_text(
    value: &str,
    selection_start: usize,
    selection_end: usize,
    pattern: &str,
) -> FormatResult {
    let n = value.len();
    let s = selection_start.min(n);
    let e = selection_end.min(n).max(s);
    let pc = pattern.len();

    // Char-prefix immediately before the selection (length `pc`).
    let prefix_start = s.saturating_sub(pc);
    let pattern_prefix = &value[prefix_start..s];
    // Char-suffix immediately after the selection (length `pc`).
    let suffix_end = (e + pc).min(n);
    let pattern_suffix = &value[e..suffix_end];

    let already_wrapped = pc > 0 && pattern_prefix == pattern && pattern_suffix == pattern;

    let (prefix, postfix, inner) = if already_wrapped {
        let prefix = &value[..s - pc];
        let postfix = &value[e + pc..];
        let inner = value[s..e].to_string();
        (prefix.to_string(), postfix.to_string(), inner)
    } else {
        let prefix = &value[..s];
        let postfix = &value[e..];
        let selection = &value[s..e];
        let inner = format!("{pattern}{selection}{pattern}");
        (prefix.to_string(), postfix.to_string(), inner)
    };
    let new_value = format!("{prefix}{inner}{postfix}");

    // Cursor / selection math — matches Logseq's three branches.
    let selection_was_empty = s == e;
    let new_selection = if already_wrapped {
        // Strip pattern → selection shifts back by `pc` on both ends.
        (s - pc, e - pc)
    } else if selection_was_empty {
        // Empty selection wrapped → caret lands between the
        // markers. Logseq returns the empty range
        // (start + pc, end + pc) which equals a single caret at
        // start + pc when start == end.
        (s + pc, e + pc)
    } else {
        // Non-empty selection wrapped → Logseq parks the caret
        // at `selection-end + pattern-count` which, after the
        // opening pattern shifts everything by `pc`, lands the
        // caret *just before* the closing pattern in the new
        // string. That way continued typing stays inside the
        // formatting (bold/italic/etc) rather than escaping it.
        (e + pc, e + pc)
    };

    FormatResult {
        new_value,
        selection: new_selection,
    }
}

/// Convenience for the link shortcut. Logseq's `html-link-format!`
/// inserts `[label](url)` and parks the caret on the url half
/// when there's a selected label.
pub fn format_link(value: &str, sel_start: usize, sel_end: usize) -> FormatResult {
    let n = value.len();
    let s = sel_start.min(n);
    let e = sel_end.min(n).max(s);
    let selection = &value[s..e];
    if s == e {
        // No selection: insert `[]()` with caret between the
        // brackets so the user types the label first.
        let new_value = format!("{}[]()", value);
        // Splice properly at caret:
        let mut nv = String::with_capacity(value.len() + 4);
        nv.push_str(&value[..s]);
        nv.push_str("[]()");
        nv.push_str(&value[s..]);
        let _ = new_value; // unused; nv is the real one.
        return FormatResult {
            new_value: nv,
            selection: (s + 1, s + 1),
        };
    }
    // With a selection: wrap as `[selection](url)` with caret
    // parked between the `()` so the user can paste the URL.
    let mut nv = String::with_capacity(value.len() + 4);
    nv.push_str(&value[..s]);
    nv.push('[');
    nv.push_str(selection);
    nv.push_str("](");
    let url_caret = nv.len();
    nv.push(')');
    nv.push_str(&value[e..]);
    FormatResult {
        new_value: nv,
        selection: (url_caret, url_caret),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bold_empty_selection_lands_caret_between_markers() {
        let r = format_text("", 0, 0, "**");
        assert_eq!(r.new_value, "****");
        assert_eq!(r.selection, (2, 2));
    }

    #[test]
    fn bold_with_selection_parks_inside_closer() {
        // "hello" with "ell" selected (1..4). After bold the
        // new string is "h**ell**o" — caret lands at position 6
        // (right at the start of the closing `**`) so continued
        // typing stays inside the bold span.
        let r = format_text("hello", 1, 4, "**");
        assert_eq!(r.new_value, "h**ell**o");
        assert_eq!(r.selection, (6, 6));
    }

    #[test]
    fn bold_already_wrapped_unwraps() {
        // Selection is "ell" with `**` on both sides.
        let r = format_text("h**ell**o", 3, 6, "**");
        assert_eq!(r.new_value, "hello");
        // Selection shifts back by 2 on both ends → (1, 4).
        assert_eq!(r.selection, (1, 4));
    }

    #[test]
    fn italic_pattern_works() {
        let r = format_text("", 0, 0, "*");
        assert_eq!(r.new_value, "**");
        assert_eq!(r.selection, (1, 1));
    }

    #[test]
    fn code_pattern_works() {
        let r = format_text("hello", 0, 5, "`");
        assert_eq!(r.new_value, "`hello`");
        // Caret lands just before the closing backtick → 6.
        assert_eq!(r.selection, (6, 6));
    }

    #[test]
    fn link_no_selection_caret_in_label() {
        let r = format_link("see ", 4, 4);
        assert_eq!(r.new_value, "see []()");
        // Caret between the `[` and `]`.
        assert_eq!(r.selection, (5, 5));
    }

    #[test]
    fn link_with_selection_caret_in_url() {
        let r = format_link("see Rust here", 4, 8);
        assert_eq!(r.new_value, "see [Rust]() here");
        // Caret between `(` and `)` → after `[Rust](` = position 11.
        assert_eq!(r.selection, (11, 11));
    }
}
