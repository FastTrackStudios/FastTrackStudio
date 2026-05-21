//! Markdown live-preview decoration source.
//!
//! Scans the doc for `**…**` (bold), `*…*` (italic), and
//! `` `…` `` (inline code) spans and emits decorations:
//!
//! - The body of the span gets a `MarkDecoration` with the
//!   corresponding class (`md-bold`, `md-italic`, `md-code`).
//! - The opening + closing markers are `Replace`d **only when
//!   the primary cursor is outside the span**. While the cursor
//!   is on the span, markers stay visible so the user sees
//!   the raw markdown and can edit it directly.
//!
//! This is exactly Obsidian's "Live Preview" mode in spirit —
//! it's a renderer trick, not a document-model change.
//!
//! The parser is single-pass and intentionally tiny. Not a real
//! CommonMark implementation; just enough to demo the
//! decoration pipeline. A future commit can swap in a proper
//! markdown parser (pulldown-cmark or a port of CM6's
//! lang-markdown) without touching the decoration shape.

use crate::decoration::{Decoration, DecoratedRange};
use crate::selection::Range;
use crate::state::EditorState;

/// The full live-preview decoration source. Suitable to register
/// as `editor_view::DecorationSource`.
pub fn live_preview(state: &EditorState) -> Vec<DecoratedRange> {
    let text = state.doc.to_string();
    let primary = state.selection.primary();
    let mut out = Vec::new();

    // Per-step timing for the perf trace. The cost in this fn is
    // dominated by `emit_fence_tokens` (tree-sitter) on docs
    // with code fences; the rest is O(doc-length) byte walking.
    let t_blocks = now_ms_native();
    let fenced_ranges = scan_blocks(&text, primary, &mut out);
    let blocks_ms = now_ms_native() - t_blocks;

    let t_inline = now_ms_native();
    let inline_decs_before = out.len();
    for span in find_spans(&text) {
        if in_fenced_code(&fenced_ranges, span.outer.start) {
            continue;
        }
        if !span.body.is_empty() {
            let href = match span.class {
                "md-link" => Some(text[span.body.end + 2..span.outer.end - 1].to_string()),
                "md-wikilink" => Some(text[span.body.clone()].to_string()),
                _ => None,
            };
            if let Some(h) = href {
                out.push(Decoration::mark_with_attrs(
                    span.body.clone(),
                    span.class,
                    vec![("data-href".into(), h)],
                ));
                if !cursor_touches(primary, span.outer.clone()) {
                    // Caret elsewhere: treat the link as one
                    // atomic unit. Clicks anywhere inside snap
                    // to the nearer edge so the user never lands
                    // in the hidden marker bytes (`](url)` etc.).
                    out.push(Decoration::atomic(span.outer.clone()));
                }
            } else {
                out.push(Decoration::mark(span.body.clone(), span.class));
            }
        }
        if !cursor_touches(primary, span.outer.clone()) {
            if span.body.start > span.outer.start {
                out.push(Decoration::replace(span.outer.start..span.body.start));
            }
            if span.outer.end > span.body.end {
                out.push(Decoration::replace(span.body.end..span.outer.end));
            }
        }
    }
    let inline_ms = now_ms_native() - t_inline;
    let inline_decs = out.len() - inline_decs_before;
    tracing::debug!(
        doc_len = text.len(),
        block_decs = inline_decs_before,
        inline_decs,
        fence_count = fenced_ranges.len(),
        blocks_ms = %format!("{:.2}", blocks_ms),
        inline_ms = %format!("{:.2}", inline_ms),
        "md.live_preview"
    );
    out
}

/// Wall-clock milliseconds. wasm-safe alias around the
/// view-layer `now_ms`; mirrored here so editor-state stays
/// free of dioxus deps.
fn now_ms_native() -> f64 {
    #[cfg(not(target_arch = "wasm32"))]
    {
        use std::sync::OnceLock;
        static START: OnceLock<std::time::Instant> = OnceLock::new();
        let s = START.get_or_init(std::time::Instant::now);
        s.elapsed().as_secs_f64() * 1000.0
    }
    #[cfg(target_arch = "wasm32")]
    {
        web_sys::window()
            .and_then(|w| w.performance())
            .map(|p| p.now())
            .unwrap_or(0.0)
    }
}

fn in_fenced_code(ranges: &[std::ops::Range<usize>], pos: usize) -> bool {
    ranges.iter().any(|r| pos >= r.start && pos < r.end)
}

struct Span {
    /// Includes the opening + closing markers.
    outer: std::ops::Range<usize>,
    /// Just the inner content.
    body: std::ops::Range<usize>,
    /// CSS class to apply to the body. Static for now; later
    /// callers may want to inject their own class names.
    class: &'static str,
}

/// Did the primary selection touch any byte in `range`? A caret
/// *adjacent* to the span counts as touching — so cursors at
/// either edge keep the markers visible (matches Obsidian).
fn cursor_touches(primary: Range, range: std::ops::Range<usize>) -> bool {
    let (sel_from, sel_to) = (primary.from(), primary.to());
    sel_to >= range.start && sel_from <= range.end
}

/// Block-level scanner. Walks the doc line by line, recognizing
/// headings, blockquotes, lists, task lists, HRs and fenced code
/// blocks. Pushes the right `Decoration`s onto `out` and returns
/// the byte ranges occupied by fenced-code *content* so the
/// caller can skip inline parsing inside them.
fn scan_blocks(
    text: &str,
    primary: Range,
    out: &mut Vec<DecoratedRange>,
) -> Vec<std::ops::Range<usize>> {
    let mut fenced_ranges = Vec::new();
    // Fence-tracking state:
    //   - `Some((open_line_end, marker_char, marker_len))` while
    //     we're inside a fence; `open_line_end` is the byte AFTER
    //     the opening fence's `\n` (or end of doc if the fence is
    //     the last thing).
    let mut fence: Option<(usize, u8, usize)> = None;

    for (line_from, line_to) in line_ranges(text) {
        let line = &text[line_from..line_to];

        // ── Inside a fence ─────────────────────────────────
        if let Some((_, mc, mlen)) = fence {
            if is_closing_fence(line, mc, mlen) {
                out.push(Decoration::line(line_from, "md-code-block"));
                out.push(Decoration::replace(line_from..line_to));
                // The closing fence line itself counts as part of
                // the protected range so the inline scanner skips
                // its backticks.
                if let Some((open_end, _, _)) = fence.take() {
                    fenced_ranges.push(open_end..line_to);
                }
                continue;
            }
            out.push(Decoration::line(line_from, "md-code-block"));
            continue;
        }

        // ── Starting a fence ───────────────────────────────
        let trimmed = line.trim_start();
        if let Some((mc, mlen, info_start)) = opens_fence(trimmed) {
            out.push(Decoration::line(line_from, "md-code-block"));
            out.push(Decoration::replace(line_from..line_to));
            // The opener line is part of the protected fence range
            // — without this the inline scanner finds the `\`\``
            // pair inside the triple-backtick opener and emits a
            // stray inline-code mark.
            let info = trimmed[info_start..].trim();
            let content_start = if line_to < text.len() { line_to + 1 } else { line_to };
            // The current fence's protected range starts at the
            // opener line. If we close cleanly we'll rewrite it
            // above; if we hit EOF without a close, the EOF
            // fallback at the bottom of this function uses
            // `open_line_from` instead of `content_start`.
            fence = Some((line_from, mc, mlen));
            // Header widget at the line's start position — shows
            // the language label + a copy button. The button
            // carries `data-copy-from`/`data-copy-to` so the JS
            // bridge can grab the body text out of the doc.
            let body_end_estimate = find_fence_close(text, content_start, mc, mlen);
            let header_html = format!(
                r#"<span class="md-code-header"><span class="md-code-lang">{lang}</span><button class="md-code-copy" data-copy-from="{from}" data-copy-to="{to}" title="Copy">⧉</button></span>"#,
                lang = if info.is_empty() { "plain" } else { info },
                from = content_start,
                to = body_end_estimate,
            );
            out.push(Decoration::widget(line_from, header_html));
            if let Some(lang) = editor_syntax::Lang::from_fence_tag(info) {
                emit_fence_tokens(text, content_start, mc, mlen, lang, out);
            }
            continue;
        }

        // ── Headings ───────────────────────────────────────
        if let Some((level, marker_end)) = parse_heading(line) {
            let abs_marker_end = line_from + marker_end;
            let class = HEADING_CLASS[level - 1];
            out.push(Decoration::line(line_from, class));
            // Marker stays visible (muted) any time the caret
            // is anywhere on the line — typing inside the
            // heading body shouldn't make the marker disappear.
            if cursor_touches(primary, line_from..line_to) {
                out.push(Decoration::mark(
                    line_from..abs_marker_end,
                    "md-heading-marker",
                ));
            } else {
                out.push(Decoration::replace(line_from..abs_marker_end));
            }
            continue;
        }

        // ── HR ─────────────────────────────────────────────
        if is_hr(line) {
            // Two states:
            //   `md-hr-active` while the caret is on the line —
            //     just dim the `---` text, leave it on one row
            //     so the user can edit it.
            //   `md-hr` otherwise — hide the text bytes and let
            //     CSS render a single-row horizontal rule via
            //     the line's own border-top.
            if cursor_touches(primary, line_from..line_to) {
                out.push(Decoration::line(line_from, "md-hr-active"));
            } else {
                out.push(Decoration::line(line_from, "md-hr"));
                out.push(Decoration::replace(line_from..line_to));
            }
            continue;
        }

        // ── Task list ──────────────────────────────────────
        if let Some((prefix_end, checked)) = parse_task_marker(line) {
            let abs_prefix_end = line_from + prefix_end;
            out.push(Decoration::line(line_from, "md-task"));
            // Always hide the `- [ ]` / `- [x]` source and show
            // the clickable checkbox widget — Obsidian behavior.
            // Revealing the source on cursor-touch makes the
            // checkbox flicker away every time the user edits
            // the line text, which is exactly the wrong UX.
            let html = if checked {
                format!(
                    r#"<span class="md-task-checkbox checked" data-task-pos="{p}">✓</span>"#,
                    p = line_from
                )
            } else {
                format!(
                    r#"<span class="md-task-checkbox" data-task-pos="{p}"></span>"#,
                    p = line_from
                )
            };
            out.push(Decoration::replace(line_from..abs_prefix_end));
            out.push(Decoration::widget(line_from, html));
            continue;
        }

        // ── Blockquote ─────────────────────────────────────
        if let Some(marker_end) = parse_blockquote(line) {
            let abs_marker_end = line_from + marker_end;
            out.push(Decoration::line(line_from, "md-blockquote"));
            // Marker is "off" when the caret is anywhere on
            // this line; "on" otherwise. "Off" = transparent
            // (md-quote-marker) so the vertical bar overlays
            // the glyph and the content column doesn't shift.
            // "On" = leave bytes visible at normal styling so
            // the user can edit the `>` directly.
            if !cursor_touches(primary, line_from..line_to) {
                out.push(Decoration::mark(
                    line_from..abs_marker_end,
                    "md-quote-marker",
                ));
            }
            continue;
        }

        // ── List (unordered or ordered) ────────────────────
        if let Some(marker_end) = parse_list_marker(line) {
            let abs_marker_end = line_from + marker_end;
            out.push(Decoration::line(line_from, "md-list-item"));
            // Always render the bullet/number widget. The marker
            // source bytes stay in the doc but stay hidden — the
            // bullet is what users care about, and revealing the
            // raw `- ` on cursor-touch made the list bullet flick
            // away every keystroke.
            let kind_byte = line.trim_start().as_bytes()[0];
            let widget_html = if kind_byte.is_ascii_digit() {
                let leading = line.len() - line.trim_start().len();
                let num_end = marker_end - 2 - leading;
                let num = &line.trim_start()[..num_end];
                format!(r#"<span class="md-list-marker">{num}.&nbsp;</span>"#)
            } else {
                r#"<span class="md-list-marker">-&nbsp;</span>"#.into()
            };
            out.push(Decoration::replace(line_from..abs_marker_end));
            out.push(Decoration::widget(line_from, widget_html));
            continue;
        }
    }

    // EOF with unclosed fence — close it implicitly at doc end so
    // the inline parser still skips that range.
    if let Some((open_end, _, _)) = fence {
        fenced_ranges.push(open_end..text.len());
    }

    fenced_ranges
}

const HEADING_CLASS: [&str; 6] = ["md-h1", "md-h2", "md-h3", "md-h4", "md-h5", "md-h6"];

/// Iterate `(line_from, line_to)` byte ranges, exclusive of the
/// trailing `\n`. The last line (no trailing `\n`) is included.
fn line_ranges(text: &str) -> Vec<(usize, usize)> {
    let mut out = Vec::new();
    let bytes = text.as_bytes();
    let mut start = 0;
    for (i, &b) in bytes.iter().enumerate() {
        if b == b'\n' {
            out.push((start, i));
            start = i + 1;
        }
    }
    if start <= bytes.len() {
        out.push((start, bytes.len()));
    }
    out
}

fn parse_heading(line: &str) -> Option<(usize, usize)> {
    let b = line.as_bytes();
    let mut level = 0;
    while level < 6 && b.get(level) == Some(&b'#') {
        level += 1;
    }
    if level == 0 {
        return None;
    }
    // Must be followed by a space (`# foo`) — otherwise it's a
    // tag (`#foo`) or just hashes.
    if b.get(level) != Some(&b' ') {
        return None;
    }
    Some((level, level + 1))
}

fn parse_blockquote(line: &str) -> Option<usize> {
    let b = line.as_bytes();
    if b.first() != Some(&b'>') {
        return None;
    }
    // `> ` is the standard form; bare `>` is also legal.
    if b.get(1) == Some(&b' ') { Some(2) } else { Some(1) }
}

fn is_hr(line: &str) -> bool {
    let t = line.trim();
    if t.len() < 3 {
        return false;
    }
    let c = t.as_bytes()[0];
    if c != b'-' && c != b'*' && c != b'_' {
        return false;
    }
    t.bytes().all(|x| x == c)
}

fn parse_task_marker(line: &str) -> Option<(usize, bool)> {
    let b = line.as_bytes();
    // `- [ ]` / `- [x]` / `* [ ]` ... — minimum 6 bytes including
    // the trailing space.
    if b.len() < 5 {
        return None;
    }
    let bullet = b[0];
    if (bullet != b'-' && bullet != b'*' && bullet != b'+') || b[1] != b' ' || b[2] != b'[' {
        return None;
    }
    let inner = b[3];
    if b[4] != b']' {
        return None;
    }
    let checked = matches!(inner, b'x' | b'X');
    if !checked && inner != b' ' {
        return None;
    }
    // Consume trailing space after the bracket if present.
    let end = if b.get(5) == Some(&b' ') { 6 } else { 5 };
    Some((end, checked))
}

fn parse_list_marker(line: &str) -> Option<usize> {
    let b = line.as_bytes();
    let leading = b.iter().take_while(|&&c| c == b' ').count();
    let after = &b[leading..];
    // Unordered: `- ` / `* ` / `+ `.
    if let Some(&c) = after.first() {
        if (c == b'-' || c == b'*' || c == b'+') && after.get(1) == Some(&b' ') {
            return Some(leading + 2);
        }
    }
    // Ordered: `1. ` / `12. `.
    let digit_count = after.iter().take_while(|&&x| x.is_ascii_digit()).count();
    if digit_count > 0
        && after.get(digit_count) == Some(&b'.')
        && after.get(digit_count + 1) == Some(&b' ')
    {
        return Some(leading + digit_count + 2);
    }
    None
}

/// Does this trimmed line *open* a fence? Returns
/// `(marker_char, marker_len, info_string_start_offset)`.
fn opens_fence(trimmed: &str) -> Option<(u8, usize, usize)> {
    let b = trimmed.as_bytes();
    if b.len() < 3 {
        return None;
    }
    let c = b[0];
    if c != b'`' && c != b'~' {
        return None;
    }
    let run = b.iter().take_while(|&&x| x == c).count();
    if run < 3 {
        return None;
    }
    Some((c, run, run))
}

/// Find the byte offset of the closing fence's `\n` (or doc end)
/// when walking forward from `content_start`. Used by both the
/// syntax-highlighter and the lang/copy header widget.
fn find_fence_close(
    text: &str,
    content_start: usize,
    marker_char: u8,
    marker_len: usize,
) -> usize {
    let bytes = text.as_bytes();
    let mut i = content_start;
    while i < bytes.len() {
        let line_from = i;
        while i < bytes.len() && bytes[i] != b'\n' {
            i += 1;
        }
        let line = &text[line_from..i];
        if is_closing_fence(line, marker_char, marker_len) {
            return line_from;
        }
        if i < bytes.len() {
            i += 1;
        }
    }
    text.len()
}

/// Slice the fenced-body bytes between `content_start` and the
/// matching closing fence (or doc end), run the syntax highlighter,
/// and emit one `Mark` decoration per token.
///
/// Memoized by `(lang, body)` — typing OUTSIDE a fence shouldn't
/// re-parse the fence with tree-sitter on every keystroke. The
/// cache is bounded so it doesn't grow forever; entries evict
/// LRU-style when the bound is hit.
fn emit_fence_tokens(
    text: &str,
    content_start: usize,
    marker_char: u8,
    marker_len: usize,
    lang: editor_syntax::Lang,
    out: &mut Vec<DecoratedRange>,
) {
    let t_find = now_ms_native();
    let end = find_fence_close(text, content_start, marker_char, marker_len);
    let find_ms = now_ms_native() - t_find;
    if end <= content_start {
        return;
    }
    let body = &text[content_start..end];
    let t_tok = now_ms_native();
    let cached = with_fence_cache(|cache| cache.get(lang, body));
    let was_cached = cached.is_some();
    let tokens = match cached {
        Some(toks) => toks,
        None => {
            let toks = editor_syntax::highlight(lang, body);
            with_fence_cache(|cache| cache.put(lang, body.to_string(), toks.clone()));
            toks
        }
    };
    let tok_ms = now_ms_native() - t_tok;
    tracing::trace!(
        body_len = body.len(),
        token_count = tokens.len(),
        cache_hit = was_cached,
        find_ms = %format!("{:.2}", find_ms),
        tokenize_ms = %format!("{:.2}", tok_ms),
        "md.fence_tokens"
    );
    for tok in tokens {
        let abs_from = content_start + tok.start;
        let abs_to = content_start + tok.end;
        let class = format!("md-tok-{}", tok.tag);
        out.push(Decoration::mark(abs_from..abs_to, class));
    }
}

/// Bounded LRU-ish cache of `(lang, body) -> tokens`. Sized for
/// the common case of a handful of fences in a doc. Tree-sitter
/// parses are the per-keystroke cost we want to avoid.
struct FenceCache {
    entries: Vec<(editor_syntax::Lang, String, Vec<editor_syntax::Token>)>,
    cap: usize,
}

impl FenceCache {
    fn new(cap: usize) -> Self {
        Self {
            entries: Vec::with_capacity(cap),
            cap,
        }
    }
    fn get(&mut self, lang: editor_syntax::Lang, body: &str) -> Option<Vec<editor_syntax::Token>> {
        let idx = self.entries.iter().position(|(l, b, _)| *l == lang && b == body)?;
        // Move to back so this entry is "freshest".
        let hit = self.entries.remove(idx);
        let toks = hit.2.clone();
        self.entries.push(hit);
        Some(toks)
    }
    fn put(&mut self, lang: editor_syntax::Lang, body: String, toks: Vec<editor_syntax::Token>) {
        if self.entries.len() >= self.cap {
            self.entries.remove(0);
        }
        self.entries.push((lang, body, toks));
    }
}

fn with_fence_cache<R>(f: impl FnOnce(&mut FenceCache) -> R) -> R {
    thread_local! {
        static CACHE: std::cell::RefCell<FenceCache> =
            std::cell::RefCell::new(FenceCache::new(16));
    }
    CACHE.with(|c| f(&mut c.borrow_mut()))
}

fn is_closing_fence(line: &str, marker_char: u8, marker_len: usize) -> bool {
    let trimmed = line.trim();
    if trimmed.len() < marker_len {
        return false;
    }
    let b = trimmed.as_bytes();
    let run = b.iter().take_while(|&&x| x == marker_char).count();
    run >= marker_len && b[run..].iter().all(|&x| x == b' ')
}

/// Single-pass scanner. Walks bytes, recognizing the inline
/// markdown flavors supported by live-preview. Doesn't cross
/// newlines (a stray `*` on one line shouldn't pair with one on
/// the next). Top-level only — no nesting yet (e.g. `**a~~b~~c**`
/// gets the bold but not the strike inside).
fn find_spans(text: &str) -> Vec<Span> {
    let mut out = Vec::new();
    let b = text.as_bytes();
    let mut i = 0;
    while i < b.len() {
        if b[i] == b'\n' {
            i += 1;
            continue;
        }
        // ***bold-italic***  — must precede `**` so the triple
        // marker isn't consumed as nested bold + italic.
        if i + 6 <= b.len() && &b[i..i + 3] == b"***" {
            if let Some(end) = find_close(b, i + 3, b"***") {
                out.push(Span {
                    outer: i..end + 3,
                    body: i + 3..end,
                    class: "md-bold-italic",
                });
                i = end + 3;
                continue;
            }
        }
        // **bold**
        if i + 4 <= b.len() && &b[i..i + 2] == b"**" {
            if let Some(end) = find_close(b, i + 2, b"**") {
                out.push(Span {
                    outer: i..end + 2,
                    body: i + 2..end,
                    class: "md-bold",
                });
                i = end + 2;
                continue;
            }
        }
        // ~~strikethrough~~
        if i + 4 <= b.len() && &b[i..i + 2] == b"~~" {
            if let Some(end) = find_close(b, i + 2, b"~~") {
                out.push(Span {
                    outer: i..end + 2,
                    body: i + 2..end,
                    class: "md-strike",
                });
                i = end + 2;
                continue;
            }
        }
        // ==highlight==
        if i + 4 <= b.len() && &b[i..i + 2] == b"==" {
            if let Some(end) = find_close(b, i + 2, b"==") {
                out.push(Span {
                    outer: i..end + 2,
                    body: i + 2..end,
                    class: "md-highlight",
                });
                i = end + 2;
                continue;
            }
        }
        // `inline code`
        if b[i] == b'`' {
            if let Some(end) = find_close(b, i + 1, b"`") {
                out.push(Span {
                    outer: i..end + 1,
                    body: i + 1..end,
                    class: "md-code",
                });
                i = end + 1;
                continue;
            }
        }
        // *italic* — must not be `**` (handled above) and must
        // contain at least one char.
        if b[i] == b'*' {
            if let Some(end) = find_close(b, i + 1, b"*") {
                if end > i + 1 && b[end + 1..].first() != Some(&b'*') {
                    out.push(Span {
                        outer: i..end + 1,
                        body: i + 1..end,
                        class: "md-italic",
                    });
                    i = end + 1;
                    continue;
                }
            }
        }
        // [[wikilink]]  — keep before `[link]` so the `[[`
        // isn't misread as the start of a regular link.
        if i + 4 <= b.len() && &b[i..i + 2] == b"[[" {
            if let Some(end) = find_close(b, i + 2, b"]]") {
                out.push(Span {
                    outer: i..end + 2,
                    body: i + 2..end,
                    class: "md-wikilink",
                });
                i = end + 2;
                continue;
            }
        }
        // [^footnote-ref]
        if i + 4 <= b.len() && &b[i..i + 2] == b"[^" {
            if let Some(end) = find_close(b, i + 2, b"]") {
                out.push(Span {
                    outer: i..end + 1,
                    body: i + 2..end,
                    class: "md-footnote",
                });
                i = end + 1;
                continue;
            }
        }
        // [text](url) — find `]` then verify `(...)` follows.
        if b[i] == b'[' {
            if let Some(close_text) = find_close(b, i + 1, b"]") {
                if b.get(close_text + 1) == Some(&b'(') {
                    if let Some(close_paren) = find_close(b, close_text + 2, b")") {
                        out.push(Span {
                            outer: i..close_paren + 1,
                            body: i + 1..close_text,
                            class: "md-link",
                        });
                        i = close_paren + 1;
                        continue;
                    }
                }
            }
        }
        // #tag  — `#` at doc start or after non-word char,
        // followed by tag chars (alnum / `-` / `_` / `/`). The
        // body equals the outer (no markers to hide) so the
        // mark just colors the whole `#tag` string.
        if b[i] == b'#' && tag_boundary_before(b, i) {
            let start = i;
            let mut j = i + 1;
            while j < b.len() && is_tag_char(b[j]) {
                j += 1;
            }
            // Need at least one tag char after `#`.
            if j > i + 1 {
                out.push(Span {
                    outer: start..j,
                    body: start..j,
                    class: "md-tag",
                });
                i = j;
                continue;
            }
        }
        i += 1;
    }
    out
}

fn tag_boundary_before(b: &[u8], i: usize) -> bool {
    if i == 0 {
        // A `#` at the very start of the doc is a heading if
        // followed by a space; otherwise treat it as a tag.
        return b.get(1) != Some(&b' ');
    }
    let prev = b[i - 1];
    // `#` immediately after a newline followed by a space is a
    // heading marker, not a tag.
    if prev == b'\n' && b.get(i + 1) == Some(&b' ') {
        return false;
    }
    !prev.is_ascii_alphanumeric() && prev != b'_' && prev != b'/'
}

fn is_tag_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_' || c == b'-' || c == b'/'
}

/// Find the next occurrence of `needle` in `b` starting at
/// `from`, returning the start byte offset. Stops at newlines
/// (a span can't cross a line boundary).
fn find_close(b: &[u8], from: usize, needle: &[u8]) -> Option<usize> {
    let mut i = from;
    while i + needle.len() <= b.len() {
        if b[i] == b'\n' {
            return None;
        }
        if &b[i..i + needle.len()] == needle {
            return Some(i);
        }
        i += 1;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::doc::Doc;
    use crate::selection::{Range, Selection};

    fn state(text: &str, caret: usize) -> EditorState {
        EditorState {
            doc: Doc::from_str(text),
            selection: Selection::caret(caret),
        }
    }

    #[test]
    fn bold_with_caret_outside_hides_markers() {
        // "**hi**" at offset 0..6. Body is 2..4 ("hi").
        // Caret at 7 (past the span) — markers should be hidden.
        let s = state("**hi** there", 7);
        let decs = live_preview(&s);
        // Expect: mark(2..4 bold), replace(0..2), replace(4..6).
        assert!(decs.iter().any(|d| d.from == 0 && d.to == 2));
        assert!(decs.iter().any(|d| d.from == 4 && d.to == 6));
        assert!(decs.iter().any(|d| d.from == 2 && d.to == 4));
    }

    #[test]
    fn bold_with_caret_inside_keeps_markers() {
        // Caret at 3 — inside "hi". Markers should NOT be hidden.
        let s = state("**hi** there", 3);
        let decs = live_preview(&s);
        let replace_count = decs
            .iter()
            .filter(|d| matches!(d.kind, crate::decoration::DecorationKind::Replace))
            .count();
        assert_eq!(replace_count, 0, "caret inside span should keep markers");
        // But the body mark is still there.
        assert!(decs.iter().any(|d| d.from == 2 && d.to == 4));
    }

    #[test]
    fn caret_adjacent_to_span_counts_as_touching() {
        // Caret right after the closing `**` — adjacent.
        let s = state("**hi**", 6);
        let decs = live_preview(&s);
        let replace_count = decs
            .iter()
            .filter(|d| matches!(d.kind, crate::decoration::DecorationKind::Replace))
            .count();
        assert_eq!(replace_count, 0);
    }

    #[test]
    fn italic_recognized() {
        let s = state("hello *world*", 0);
        let decs = live_preview(&s);
        assert!(decs.iter().any(|d| matches!(
            &d.kind,
            crate::decoration::DecorationKind::Mark { class, .. } if class == "md-italic"
        )));
    }

    #[test]
    fn inline_code_recognized() {
        let s = state("see `let x = 1`", 0);
        let decs = live_preview(&s);
        assert!(decs.iter().any(|d| matches!(
            &d.kind,
            crate::decoration::DecorationKind::Mark { class, .. } if class == "md-code"
        )));
    }

    #[test]
    fn span_does_not_cross_newline() {
        let s = state("**a\nb**", 0);
        let decs = live_preview(&s);
        // No span — the opening `**` doesn't pair across the \n.
        let marks: Vec<_> = decs
            .iter()
            .filter(|d| matches!(d.kind, crate::decoration::DecorationKind::Mark { .. }))
            .collect();
        assert!(marks.is_empty());
    }

    fn mark_classes(decs: &[DecoratedRange]) -> Vec<&str> {
        decs.iter()
            .filter_map(|d| match &d.kind {
                crate::decoration::DecorationKind::Mark { class, .. } => Some(class.as_str()),
                _ => None,
            })
            .collect()
    }

    #[test]
    fn bold_italic_triple_recognized() {
        let s = state("***hi***", 0);
        assert!(mark_classes(&live_preview(&s)).contains(&"md-bold-italic"));
    }

    #[test]
    fn strikethrough_recognized() {
        let s = state("~~gone~~", 0);
        assert!(mark_classes(&live_preview(&s)).contains(&"md-strike"));
    }

    #[test]
    fn highlight_recognized() {
        let s = state("==pop==", 0);
        assert!(mark_classes(&live_preview(&s)).contains(&"md-highlight"));
    }

    #[test]
    fn link_recognized() {
        let s = state("[text](https://x)", 0);
        let decs = live_preview(&s);
        assert!(mark_classes(&decs).contains(&"md-link"));
        // Body is just "text" (offsets 1..5).
        assert!(decs.iter().any(|d| d.from == 1 && d.to == 5));
    }

    #[test]
    fn wikilink_recognized() {
        let s = state("[[Page Name]]", 0);
        let decs = live_preview(&s);
        assert!(mark_classes(&decs).contains(&"md-wikilink"));
        assert!(decs.iter().any(|d| d.from == 2 && d.to == 11));
    }

    #[test]
    fn footnote_recognized() {
        let s = state("see[^1] here", 0);
        let decs = live_preview(&s);
        assert!(mark_classes(&decs).contains(&"md-footnote"));
    }

    #[test]
    fn tag_recognized() {
        let s = state("a #todo b", 0);
        assert!(mark_classes(&live_preview(&s)).contains(&"md-tag"));
    }

    #[test]
    fn tag_requires_word_boundary() {
        let s = state("foo#bar", 0);
        let decs = live_preview(&s);
        assert!(!mark_classes(&decs).contains(&"md-tag"));
    }

    fn has_line_class(decs: &[DecoratedRange], pos: usize, class: &str) -> bool {
        decs.iter().any(|d| {
            d.from == pos
                && d.to == pos
                && matches!(&d.kind,
                    crate::decoration::DecorationKind::Line { class: c } if c == class)
        })
    }

    #[test]
    fn heading_emits_line_class_and_hides_marker() {
        let s = state("# Title", 100);
        let decs = live_preview(&s);
        assert!(has_line_class(&decs, 0, "md-h1"));
        // Marker `# ` (2 bytes) replaced when caret elsewhere.
        assert!(decs.iter().any(|d| {
            d.from == 0
                && d.to == 2
                && matches!(d.kind, crate::decoration::DecorationKind::Replace)
        }));
    }

    #[test]
    fn heading_levels_2_through_6() {
        for level in 2..=6 {
            let prefix = "#".repeat(level);
            let s = state(&format!("{prefix} h"), 100);
            let class = format!("md-h{level}");
            assert!(has_line_class(&live_preview(&s), 0, &class));
        }
    }

    #[test]
    fn heading_with_caret_shows_marker() {
        let s = state("# Title", 0);
        let decs = live_preview(&s);
        // Line class still applied …
        assert!(has_line_class(&decs, 0, "md-h1"));
        // … but no Replace on the `# `.
        let replace_on_marker = decs.iter().any(|d| {
            d.from == 0
                && d.to == 2
                && matches!(d.kind, crate::decoration::DecorationKind::Replace)
        });
        assert!(!replace_on_marker);
    }

    #[test]
    fn blockquote_recognized() {
        let s = state("> quoted", 100);
        let decs = live_preview(&s);
        assert!(has_line_class(&decs, 0, "md-blockquote"));
    }

    #[test]
    fn hr_recognized() {
        let s = state("---", 100);
        let decs = live_preview(&s);
        assert!(has_line_class(&decs, 0, "md-hr"));
    }

    #[test]
    fn hr_active_class_when_caret_on_line() {
        let s = state("---", 1);
        let decs = live_preview(&s);
        assert!(has_line_class(&decs, 0, "md-hr-active"));
        // And the `---` source isn't replaced — user can edit.
        let has_replace = decs.iter().any(|d| {
            d.from == 0 && d.to == 3
                && matches!(d.kind, crate::decoration::DecorationKind::Replace)
        });
        assert!(!has_replace);
    }

    #[test]
    fn list_bullet_recognized() {
        let s = state("- item", 100);
        let decs = live_preview(&s);
        assert!(has_line_class(&decs, 0, "md-list-item"));
    }

    #[test]
    fn list_ordered_recognized() {
        let s = state("1. first", 100);
        let decs = live_preview(&s);
        assert!(has_line_class(&decs, 0, "md-list-item"));
    }

    #[test]
    fn task_unchecked_recognized() {
        let s = state("- [ ] todo", 100);
        let decs = live_preview(&s);
        assert!(has_line_class(&decs, 0, "md-task"));
        // Widget emitted for the checkbox.
        let widget = decs.iter().any(|d| {
            matches!(&d.kind, crate::decoration::DecorationKind::Widget { html }
                if html.contains("md-task-checkbox") && !html.contains("checked"))
        });
        assert!(widget);
    }

    #[test]
    fn task_checked_recognized() {
        let s = state("- [x] done", 100);
        let decs = live_preview(&s);
        let widget = decs.iter().any(|d| {
            matches!(&d.kind, crate::decoration::DecorationKind::Widget { html }
                if html.contains("checked"))
        });
        assert!(widget);
    }

    #[test]
    fn code_fence_with_lang_emits_syntax_tokens() {
        let s = state("```rust\nfn main() {}\n```", 999);
        let decs = live_preview(&s);
        let has_token = decs.iter().any(|d| matches!(&d.kind,
            crate::decoration::DecorationKind::Mark { class, .. }
                if class.starts_with("md-tok-")));
        assert!(has_token, "expected at least one md-tok-* mark");
    }

    #[test]
    fn code_fence_spans_multiple_lines() {
        let s = state("```rust\nfn main() {}\n```", 999);
        let decs = live_preview(&s);
        // Every line gets md-code-block.
        assert!(has_line_class(&decs, 0, "md-code-block")); // open
        assert!(has_line_class(&decs, 8, "md-code-block")); // body
        // Closing fence at byte 21 (`...{}\n` ends at 20).
        let close_line_start = "```rust\nfn main() {}\n".len();
        assert!(has_line_class(&decs, close_line_start, "md-code-block"));
    }

    #[test]
    fn inline_inside_fence_is_skipped() {
        let s = state("```\n**bold**\n```", 999);
        let decs = live_preview(&s);
        // No bold mark should exist for the `**bold**` inside fence.
        let has_bold = decs.iter().any(|d| {
            matches!(&d.kind, crate::decoration::DecorationKind::Mark { class, .. }
                if class == "md-bold")
        });
        assert!(!has_bold);
    }

    #[test]
    fn tag_at_line_start_when_no_heading() {
        let s = state("#foo bar", 100);
        assert!(mark_classes(&live_preview(&s)).contains(&"md-tag"));
    }

    #[test]
    fn tag_has_no_hidden_markers() {
        let s = state("#todo", 100);
        let decs = live_preview(&s);
        let has_replace = decs
            .iter()
            .any(|d| matches!(d.kind, crate::decoration::DecorationKind::Replace));
        assert!(!has_replace, "tag should have no markers to hide");
    }
}
