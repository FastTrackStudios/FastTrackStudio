//! HTML renderer for the *editing* surface — keeps syntax markers
//! visible but styles them so users see live formatting while
//! typing. Mirrors Logseq's "atomic editor" behavior where
//! `[[Page]]` stays as the literal bracketed text but the inner
//! name is colored like a link.
//!
//! The emitted markup is *flat* — every styled run is one `<span>`
//! with a semantic class — so caret-offset restoration after a
//! re-render is just `textContent.length` arithmetic, no tree walk.
//!
//! Classes used by the renderer:
//!
//! | class            | meaning                          |
//! |------------------|----------------------------------|
//! | `ce-bracket`     | dim `[[` / `]]` / `((` / `))`    |
//! | `ce-wikilink`    | inner page name of `[[…]]`       |
//! | `ce-blockref`    | inner uuid of `((…))`            |
//! | `ce-marker`      | `**` / `*` / `~~` / `==` markers |
//! | `ce-bold`        | bold body                        |
//! | `ce-italic`      | italic body                      |
//! | `ce-strike`      | strikethrough body               |
//! | `ce-highlight`   | highlight body                   |
//! | `ce-code`        | inline code body (incl. backticks)|
//! | `ce-tag`         | `#tag` chip                      |
//! | `ce-macro`       | `{{…}}` macro span               |
//! | `ce-heading-mark`| leading `#` chars at line start  |

/// Encode reserved HTML characters so user content can't break out
/// of the surrounding `<span>`. Keeps the four characters that
/// matter for innerHTML and leaves the rest verbatim.
pub fn escape_html(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '&' => out.push_str("&amp;"),
            '"' => out.push_str("&quot;"),
            _ => out.push(c),
        }
    }
    out
}

/// Render `source` as HTML with semantic spans for every inline
/// token recognized by the editor. Unrecognized characters are
/// emitted as escaped text. Empty input yields an empty string.
///
/// The renderer is intentionally less complete than `parser::parse`
/// — it only styles tokens that *change appearance* while editing.
/// Things like `{{query …}}` or media embeds render as a single
/// dim `ce-macro` span so the user can still see the boundaries.
pub fn render_edit_html(source: &str) -> String {
    if source.is_empty() {
        return String::new();
    }
    let mut out = String::with_capacity(source.len() + 32);
    let bytes = source.as_bytes();
    let mut i = 0usize;
    let mut at_line_start = true;
    while i < source.len() {
        // ` # `…`###### ` heading marker at line start.
        if at_line_start && bytes[i] == b'#' {
            let mut j = i;
            while j < source.len() && bytes[j] == b'#' {
                j += 1;
            }
            let level = j - i;
            if level <= 6 && j < source.len() && bytes[j] == b' ' {
                out.push_str("<span class=\"ce-heading-mark\">");
                out.push_str(&"#".repeat(level));
                out.push_str(" </span>");
                i = j + 1;
                at_line_start = false;
                continue;
            }
        }
        // [[Page]] / [[Page|alias]]
        if source[i..].starts_with("[[") {
            if let Some(end) = source[i + 2..].find("]]") {
                let body = &source[i + 2..i + 2 + end];
                out.push_str("<span class=\"ce-bracket\">[[</span>");
                out.push_str("<span class=\"ce-wikilink\">");
                out.push_str(&escape_html(body));
                out.push_str("</span>");
                out.push_str("<span class=\"ce-bracket\">]]</span>");
                i += 2 + end + 2;
                at_line_start = false;
                continue;
            }
        }
        // ((uuid))
        if source[i..].starts_with("((") {
            if let Some(end) = source[i + 2..].find("))") {
                let body = &source[i + 2..i + 2 + end];
                out.push_str("<span class=\"ce-bracket\">((</span>");
                out.push_str("<span class=\"ce-blockref\">");
                out.push_str(&escape_html(body));
                out.push_str("</span>");
                out.push_str("<span class=\"ce-bracket\">))</span>");
                i += 2 + end + 2;
                at_line_start = false;
                continue;
            }
        }
        // {{ … }} — any macro form. Video-timestamp gets a
        // dedicated style so it's recognizable in edit mode.
        if source[i..].starts_with("{{") {
            if let Some(end) = source[i + 2..].find("}}") {
                let body = &source[i + 2..i + 2 + end];
                let trimmed = body.trim();
                let is_ts = trimmed.starts_with("video-timestamp ")
                    || trimmed.starts_with("youtube-timestamp ");
                let class = if is_ts { "ce-timestamp" } else { "ce-macro" };
                out.push_str("<span class=\"");
                out.push_str(class);
                out.push_str("\">{{");
                out.push_str(&escape_html(body));
                out.push_str("}}</span>");
                i += 2 + end + 2;
                at_line_start = false;
                continue;
            }
        }
        // **bold**
        if source[i..].starts_with("**") {
            if let Some(end) = source[i + 2..].find("**") {
                let body = &source[i + 2..i + 2 + end];
                if !body.is_empty() && !body.contains('\n') {
                    out.push_str("<span class=\"ce-marker\">**</span>");
                    out.push_str("<span class=\"ce-bold\">");
                    out.push_str(&escape_html(body));
                    out.push_str("</span>");
                    out.push_str("<span class=\"ce-marker\">**</span>");
                    i += 2 + end + 2;
                    at_line_start = false;
                    continue;
                }
            }
        }
        // ~~strike~~
        if source[i..].starts_with("~~") {
            if let Some(end) = source[i + 2..].find("~~") {
                let body = &source[i + 2..i + 2 + end];
                if !body.is_empty() && !body.contains('\n') {
                    out.push_str("<span class=\"ce-marker\">~~</span>");
                    out.push_str("<span class=\"ce-strike\">");
                    out.push_str(&escape_html(body));
                    out.push_str("</span>");
                    out.push_str("<span class=\"ce-marker\">~~</span>");
                    i += 2 + end + 2;
                    at_line_start = false;
                    continue;
                }
            }
        }
        // ==highlight==
        if source[i..].starts_with("==") {
            if let Some(end) = source[i + 2..].find("==") {
                let body = &source[i + 2..i + 2 + end];
                if !body.is_empty() && !body.contains('\n') {
                    out.push_str("<span class=\"ce-marker\">==</span>");
                    out.push_str("<span class=\"ce-highlight\">");
                    out.push_str(&escape_html(body));
                    out.push_str("</span>");
                    out.push_str("<span class=\"ce-marker\">==</span>");
                    i += 2 + end + 2;
                    at_line_start = false;
                    continue;
                }
            }
        }
        // *italic* — single-char marker. Must not be preceded by
        // another `*` (already handled by the `**` arm above when
        // the bold matcher fires first).
        if bytes[i] == b'*' && i + 1 < source.len() && bytes[i + 1] != b'*' {
            if let Some(end_rel) = source[i + 1..].find('*') {
                let body = &source[i + 1..i + 1 + end_rel];
                if !body.is_empty() && !body.contains('\n') && !body.contains('*') {
                    out.push_str("<span class=\"ce-marker\">*</span>");
                    out.push_str("<span class=\"ce-italic\">");
                    out.push_str(&escape_html(body));
                    out.push_str("</span>");
                    out.push_str("<span class=\"ce-marker\">*</span>");
                    i += 1 + end_rel + 1;
                    at_line_start = false;
                    continue;
                }
            }
        }
        // `code`
        if bytes[i] == b'`' {
            if let Some(end_rel) = source[i + 1..].find('`') {
                let body = &source[i + 1..i + 1 + end_rel];
                if !body.is_empty() && !body.contains('\n') {
                    out.push_str("<span class=\"ce-code\">`");
                    out.push_str(&escape_html(body));
                    out.push_str("`</span>");
                    i += 1 + end_rel + 1;
                    at_line_start = false;
                    continue;
                }
            }
        }
        // #tag — at word boundary.
        if bytes[i] == b'#' && (i == 0 || (bytes[i - 1] as char).is_whitespace()) {
            let mut j = i + 1;
            while j < source.len() {
                let c = bytes[j] as char;
                if c.is_alphanumeric() || c == '-' || c == '_' || c == '/' {
                    j += 1;
                } else {
                    break;
                }
            }
            if j > i + 1 {
                out.push_str("<span class=\"ce-tag\">");
                out.push_str(&escape_html(&source[i..j]));
                out.push_str("</span>");
                i = j;
                at_line_start = false;
                continue;
            }
        }
        let c = source[i..].chars().next().expect("non-empty");
        if c == '\n' {
            // Keep newlines as real newlines so `white-space: pre-wrap`
            // preserves block-internal line breaks.
            out.push('\n');
            at_line_start = true;
        } else {
            out.push_str(&escape_html(&c.to_string()));
            at_line_start = false;
        }
        i += c.len_utf8();
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_renders_empty() {
        assert_eq!(render_edit_html(""), "");
    }

    #[test]
    fn plain_text_escapes_html() {
        assert_eq!(render_edit_html("a < b & c"), "a &lt; b &amp; c");
    }

    #[test]
    fn wikilink_keeps_brackets_visible() {
        let html = render_edit_html("see [[Foo]] here");
        assert!(html.contains("ce-bracket\">[["));
        assert!(html.contains("ce-wikilink\">Foo"));
        assert!(html.contains("ce-bracket\">]]"));
    }

    #[test]
    fn bold_keeps_markers_visible() {
        let html = render_edit_html("hi **there** you");
        assert!(html.contains("ce-marker\">**"));
        assert!(html.contains("ce-bold\">there"));
    }

    #[test]
    fn italic_does_not_swallow_bold() {
        // `**bold**` should match as bold, not as two `*…*` italics.
        let html = render_edit_html("**bold**");
        assert!(html.contains("ce-bold\">bold"));
        assert!(!html.contains("ce-italic"));
    }

    #[test]
    fn hashtag_at_word_boundary() {
        let html = render_edit_html("see #rust today");
        assert!(html.contains("ce-tag\">#rust"));
        // `c#sharp` shouldn't match.
        let html2 = render_edit_html("c#sharp");
        assert!(!html2.contains("ce-tag"));
    }

    #[test]
    fn heading_marker_at_line_start() {
        let html = render_edit_html("## My header");
        assert!(html.contains("ce-heading-mark\">## "));
    }

    #[test]
    fn unclosed_marker_left_as_text() {
        // `**bold` without closing should not render styled.
        let html = render_edit_html("**bold");
        assert!(!html.contains("ce-bold"));
        assert!(html.contains("**bold"));
    }
}
