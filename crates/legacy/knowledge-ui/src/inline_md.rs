//! Inline markdown parser for view-mode block rendering.
//!
//! Produces a flat `Vec<Inline>` from a single block's content.
//! Covers the inline forms users see most: `[[wikilink]]` /
//! `[[wikilink|alias]]`, `#nested/tag`, `` `code` ``, `**bold**`,
//! `*italic*`. Block-level kinds (heading, code, blockquote,
//! list_item) are handled by the renderer's outer chrome.
//!
//! `[[wikilink]]` and `#tag` detection delegate to the existing
//! regex constants in `knowledge_proto::obsidian` so the view
//! mirrors the same grammar that `extract_refs` uses for
//! persistence.
//!
//! Bold / italic / code spans are scanned by hand — we deliberately
//! don't pull in a full CommonMark parser. The scope is one block,
//! one pass, no dependencies beyond `regex` (already in workspace).

use std::sync::OnceLock;

use knowledge_proto::obsidian::{BLOCK_REF_REGEX, EMBED_REGEX, LINK_REGEX, TAG_REGEX};
use regex::Regex;
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Inline {
    Text(String),
    /// `[[Target]]` or `[[Target|Alias]]`. `heading` / `block_id`
    /// captures from the wikilink grammar are flattened into the
    /// target for now (view-mode display only — persistence keeps
    /// them split via `LinkRef`).
    Link {
        target: String,
        alias: Option<String>,
    },
    /// `((block-uuid))` — Logseq-style direct block reference.
    /// View-mode renders as a chip showing the referenced
    /// block's first line.
    BlockRef {
        target_block_id: Uuid,
    },
    /// `![[Page]]` — page embed. View-mode renders the
    /// embedded page's blocks inside a card.
    Embed {
        target: String,
        alias: Option<String>,
    },
    /// `#nested/tag` — caller renders as a chip.
    Tag(String),
    /// `` `code` `` — inline code span.
    Code(String),
    Bold(Vec<Inline>),
    Italic(Vec<Inline>),
    /// `~~text~~` — strikethrough. GFM convention.
    Strikethrough(Vec<Inline>),
    /// `==text==` — highlight. Obsidian convention; renders with
    /// a `<mark>` element.
    Highlight(Vec<Inline>),
    /// `[label](https://…)` — external Markdown link. Distinct
    /// from `[[wikilink]]`. Renders as `<a target=_blank rel=noopener>`.
    ExternalLink {
        label: String,
        url: String,
    },
    /// `![alt](url)` — Markdown inline image. Renders as `<img>`.
    /// Distinct from `![[Page]]` page-embed, which is parsed by
    /// the obsidian-span pass before we get here.
    Image {
        alt: String,
        url: String,
    },
    /// `[^id]` — footnote reference. Renders as a superscript
    /// link; the definition lives in a separate block of the
    /// shape `[^id]: …`.
    FootnoteRef(String),
}

fn link_re() -> &'static Regex {
    static R: OnceLock<Regex> = OnceLock::new();
    R.get_or_init(|| Regex::new(LINK_REGEX).expect("LINK_REGEX compiles"))
}

fn tag_re() -> &'static Regex {
    static R: OnceLock<Regex> = OnceLock::new();
    R.get_or_init(|| Regex::new(TAG_REGEX).expect("TAG_REGEX compiles"))
}

fn block_ref_re() -> &'static Regex {
    static R: OnceLock<Regex> = OnceLock::new();
    R.get_or_init(|| Regex::new(BLOCK_REF_REGEX).expect("BLOCK_REF_REGEX compiles"))
}

fn embed_re() -> &'static Regex {
    static R: OnceLock<Regex> = OnceLock::new();
    R.get_or_init(|| Regex::new(EMBED_REGEX).expect("EMBED_REGEX compiles"))
}

/// Pre-found wikilink / tag spans used by [`parse_inline`] to
/// short-circuit the manual scanner without re-implementing the
/// obsidian grammar.
#[derive(Clone, Debug)]
struct Span {
    start: usize,
    end: usize,
    inline: Inline,
}

fn collect_obsidian_spans(s: &str) -> Vec<Span> {
    let mut out = Vec::new();
    // Embeds first so `![[X]]` claims the span before the link
    // regex sees `[[X]]` inside it.
    for caps in embed_re().captures_iter(s) {
        let m = caps.get(0).unwrap();
        let target = caps
            .get(1)
            .map(|c| c.as_str().to_string())
            .unwrap_or_default();
        let alias = caps.get(4).map(|c| c.as_str().to_string());
        out.push(Span {
            start: m.start(),
            end: m.end(),
            inline: Inline::Embed { target, alias },
        });
    }
    for caps in link_re().captures_iter(s) {
        let m = caps.get(0).unwrap();
        // Skip if this `[[...]]` is inside an embed `![[...]]`
        // span we already captured (collect dedupe will catch
        // it but cleaner to filter early).
        if m.start() > 0 && s.as_bytes()[m.start() - 1] == b'!' {
            continue;
        }
        let target = caps
            .get(1)
            .map(|c| c.as_str().to_string())
            .unwrap_or_default();
        let alias = caps.get(4).map(|c| c.as_str().to_string());
        out.push(Span {
            start: m.start(),
            end: m.end(),
            inline: Inline::Link { target, alias },
        });
    }
    for caps in block_ref_re().captures_iter(s) {
        let m = caps.get(0).unwrap();
        let id_str = caps.get(1).map(|c| c.as_str()).unwrap_or("");
        if let Ok(id) = Uuid::parse_str(id_str) {
            out.push(Span {
                start: m.start(),
                end: m.end(),
                inline: Inline::BlockRef {
                    target_block_id: id,
                },
            });
        }
    }
    for caps in tag_re().captures_iter(s) {
        // TAG_REGEX matches a leading whitespace char before `#`;
        // we want the span to cover only the `#tag` itself.
        let m = caps.get(0).unwrap();
        let inner = caps.get(1).unwrap();
        // `#` byte = inner.start - 1
        let hash_start = inner.start().saturating_sub(1);
        out.push(Span {
            start: hash_start,
            end: m.end(),
            inline: Inline::Tag(inner.as_str().to_string()),
        });
    }
    out.sort_by_key(|s| s.start);
    // Drop overlapping spans (a `[[#tag]]` shouldn't double-emit).
    let mut deduped: Vec<Span> = Vec::with_capacity(out.len());
    for s in out {
        match deduped.last() {
            Some(last) if s.start < last.end => continue,
            _ => deduped.push(s),
        }
    }
    deduped
}

pub fn parse_inline(s: &str) -> Vec<Inline> {
    let spans = collect_obsidian_spans(s);
    let mut out = Vec::new();
    let mut buf = String::new();
    let mut i = 0usize;
    let bytes = s.as_bytes();
    let mut span_iter = spans.into_iter().peekable();

    while i < s.len() {
        // If a wikilink/tag span starts here, emit it and skip.
        if let Some(span) = span_iter.peek() {
            if span.start == i {
                flush(&mut buf, &mut out);
                let span = span_iter.next().unwrap();
                out.push(span.inline);
                i = span.end;
                continue;
            }
            // Skip stale spans (shouldn't happen if we track i,
            // but be defensive).
            if span.end <= i {
                span_iter.next();
                continue;
            }
        }
        // `code`
        if bytes[i] == b'`' {
            if let Some(end) = s[i + 1..].find('`') {
                let code = &s[i + 1..i + 1 + end];
                if !code.contains('\n') && !code.is_empty() {
                    flush(&mut buf, &mut out);
                    out.push(Inline::Code(code.to_string()));
                    i += 1 + end + 1;
                    continue;
                }
            }
        }
        // **bold**
        if s[i..].starts_with("**") {
            if let Some(end) = find_close(&s[i + 2..], "**") {
                let inner = parse_inline(&s[i + 2..i + 2 + end]);
                flush(&mut buf, &mut out);
                out.push(Inline::Bold(inner));
                i += 2 + end + 2;
                continue;
            }
        }
        // ~~strikethrough~~
        if s[i..].starts_with("~~") {
            if let Some(end) = find_close(&s[i + 2..], "~~") {
                let inner = parse_inline(&s[i + 2..i + 2 + end]);
                flush(&mut buf, &mut out);
                out.push(Inline::Strikethrough(inner));
                i += 2 + end + 2;
                continue;
            }
        }
        // ==highlight== (Obsidian)
        if s[i..].starts_with("==") {
            if let Some(end) = find_close(&s[i + 2..], "==") {
                let inner = parse_inline(&s[i + 2..i + 2 + end]);
                flush(&mut buf, &mut out);
                out.push(Inline::Highlight(inner));
                i += 2 + end + 2;
                continue;
            }
        }
        // [^id] — footnote reference. Must run BEFORE the
        // `[label](url)` rule since both start with `[`.
        if bytes[i] == b'['
            && bytes.get(i + 1).copied() == Some(b'^')
            && bytes.get(i + 2).copied() != Some(b']')
        {
            if let Some(end) = s[i + 2..].find(']') {
                let id = &s[i + 2..i + 2 + end];
                let after = i + 2 + end + 1;
                // Skip if this is actually a definition line
                // start (`[^id]:` …) — those are block-level
                // and parsed differently. We still emit the
                // ref inside the definition body though, so
                // only skip when the colon directly follows AND
                // we're at the very beginning of the string.
                let is_definition_start = i == 0 && bytes.get(after).copied() == Some(b':');
                if !is_definition_start && !id.is_empty() && !id.contains(char::is_whitespace) {
                    flush(&mut buf, &mut out);
                    out.push(Inline::FootnoteRef(id.to_string()));
                    i = after;
                    continue;
                }
            }
        }
        // ![alt](url) — markdown inline image. Has to run before
        // the `[label](url)` rule because of the leading `!`.
        // (`![[Page]]` page-embeds are already consumed by the
        // obsidian-span pass at the top of the loop.)
        if bytes[i] == b'!' && bytes.get(i + 1).copied() == Some(b'[') {
            if let Some(alt_end) = s[i + 2..].find(']') {
                let after_alt = i + 2 + alt_end + 1;
                if bytes.get(after_alt).copied() == Some(b'(') {
                    if let Some(url_end) = s[after_alt + 1..].find(')') {
                        let alt = &s[i + 2..i + 2 + alt_end];
                        let url = &s[after_alt + 1..after_alt + 1 + url_end];
                        if !alt.contains('\n')
                            && !url.contains('\n')
                            && !url.contains(' ')
                            && !url.is_empty()
                        {
                            flush(&mut buf, &mut out);
                            out.push(Inline::Image {
                                alt: alt.to_string(),
                                url: url.to_string(),
                            });
                            i = after_alt + 1 + url_end + 1;
                            continue;
                        }
                    }
                }
            }
        }
        // [label](url) — external markdown link. Must not greedily
        // swallow `[[wikilinks]]` (the wikilink span handler runs
        // first via `collect_obsidian_spans`, so `[[` is already
        // consumed before we get here).
        if bytes[i] == b'[' && bytes.get(i + 1).copied() != Some(b'[') {
            if let Some(label_end) = s[i + 1..].find(']') {
                let after_label = i + 1 + label_end + 1;
                if bytes.get(after_label).copied() == Some(b'(') {
                    if let Some(url_end) = s[after_label + 1..].find(')') {
                        let label = &s[i + 1..i + 1 + label_end];
                        let url = &s[after_label + 1..after_label + 1 + url_end];
                        // Cheap URL sanity check — must look like a
                        // scheme://… or relative path with no spaces.
                        // Reject anything starting with `[` to avoid
                        // confusing it with footnote refs.
                        if !label.contains('\n')
                            && !url.contains('\n')
                            && !url.contains(' ')
                            && !url.is_empty()
                            && !label.starts_with('^')
                        {
                            flush(&mut buf, &mut out);
                            out.push(Inline::ExternalLink {
                                label: label.to_string(),
                                url: url.to_string(),
                            });
                            i = after_label + 1 + url_end + 1;
                            continue;
                        }
                    }
                }
            }
        }
        // *italic* — but not part of `**`.
        if bytes[i] == b'*' && bytes.get(i + 1).copied() != Some(b'*') {
            if let Some(end) = find_close(&s[i + 1..], "*") {
                let inner = parse_inline(&s[i + 1..i + 1 + end]);
                flush(&mut buf, &mut out);
                out.push(Inline::Italic(inner));
                i += 1 + end + 1;
                continue;
            }
        }
        // Default: copy one char.
        let c = s[i..].chars().next().expect("non-empty");
        buf.push(c);
        i += c.len_utf8();
    }
    flush(&mut buf, &mut out);
    out
}

fn flush(buf: &mut String, out: &mut Vec<Inline>) {
    if !buf.is_empty() {
        out.push(Inline::Text(std::mem::take(buf)));
    }
}

/// Find the offset of `needle` in `s`, skipping occurrences that
/// appear inside another inline span (`code` or `[[link]]`). Keeps
/// `*foo `bar*baz` ` from breaking emphasis matching.
fn find_close(s: &str, needle: &str) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut i = 0;
    while i + needle.len() <= s.len() {
        if bytes[i] == b'`' {
            if let Some(end) = s[i + 1..].find('`') {
                i += 1 + end + 1;
                continue;
            }
        }
        if s[i..].starts_with("[[") {
            if let Some(end) = s[i + 2..].find("]]") {
                i += 2 + end + 2;
                continue;
            }
        }
        if s[i..].starts_with(needle) {
            return Some(i);
        }
        let c = s[i..].chars().next().expect("non-empty");
        i += c.len_utf8();
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain_text() {
        assert_eq!(
            parse_inline("hello world"),
            vec![Inline::Text("hello world".into())]
        );
    }

    #[test]
    fn wikilink_and_alias() {
        let out = parse_inline("see [[Page]] and [[Other|alias]]");
        assert_eq!(
            out,
            vec![
                Inline::Text("see ".into()),
                Inline::Link {
                    target: "Page".into(),
                    alias: None
                },
                Inline::Text(" and ".into()),
                Inline::Link {
                    target: "Other".into(),
                    alias: Some("alias".into())
                },
            ]
        );
    }

    #[test]
    fn tag_with_slash() {
        // TAG_REGEX is unicode-aware and requires whitespace before `#`.
        assert_eq!(
            parse_inline("hi #area/work end"),
            vec![
                Inline::Text("hi ".into()),
                Inline::Tag("area/work".into()),
                Inline::Text(" end".into()),
            ]
        );
    }

    #[test]
    fn bold_italic_code() {
        let out = parse_inline("**b** and *i* and `c`");
        assert_eq!(
            out,
            vec![
                Inline::Bold(vec![Inline::Text("b".into())]),
                Inline::Text(" and ".into()),
                Inline::Italic(vec![Inline::Text("i".into())]),
                Inline::Text(" and ".into()),
                Inline::Code("c".into()),
            ]
        );
    }

    #[test]
    fn strikethrough_and_highlight() {
        let out = parse_inline("~~old~~ and ==hot==");
        assert_eq!(
            out,
            vec![
                Inline::Strikethrough(vec![Inline::Text("old".into())]),
                Inline::Text(" and ".into()),
                Inline::Highlight(vec![Inline::Text("hot".into())]),
            ]
        );
    }

    #[test]
    fn external_link_distinct_from_wikilink() {
        let out = parse_inline("see [docs](https://example.com) and [[InternalPage]]");
        let kinds: Vec<&str> = out
            .iter()
            .filter_map(|i| match i {
                Inline::ExternalLink { url, .. } => Some(url.as_str()),
                Inline::Link { target, .. } => Some(target.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(kinds, vec!["https://example.com", "InternalPage"]);
    }

    #[test]
    fn external_link_label_can_contain_emphasis_in_render_but_keeps_raw_here() {
        // Parser captures the label as raw text; renderer doesn't
        // re-parse emphasis inside the label for v1.
        let out = parse_inline("[**bold**](https://x.io)");
        match &out[0] {
            Inline::ExternalLink { label, url } => {
                assert_eq!(label, "**bold**");
                assert_eq!(url, "https://x.io");
            }
            other => panic!("expected ExternalLink, got {other:?}"),
        }
    }

    #[test]
    fn footnote_ref_parses() {
        let out = parse_inline("see [^1] for details");
        assert!(
            out.iter()
                .any(|i| matches!(i, Inline::FootnoteRef(s) if s == "1")),
            "got: {out:?}"
        );
    }

    #[test]
    fn footnote_ref_distinct_from_external_link() {
        let out = parse_inline("see [^1] and [text](url)");
        let refs: Vec<&str> = out
            .iter()
            .filter_map(|i| match i {
                Inline::FootnoteRef(s) => Some(s.as_str()),
                _ => None,
            })
            .collect();
        let ext: Vec<&str> = out
            .iter()
            .filter_map(|i| match i {
                Inline::ExternalLink { url, .. } => Some(url.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(refs, vec!["1"]);
        assert_eq!(ext, vec!["url"]);
    }

    #[test]
    fn footnote_def_start_not_emitted_as_inline_ref() {
        // `[^1]:` at the very start of a string is the definition;
        // the parser should leave the `[^1]:` for the block-level
        // handler. (The whole def block lives in `outliner::
        // parse_footnote_def` which short-circuits at BlockView.)
        let out = parse_inline("[^1]: body");
        // Should NOT have a FootnoteRef in this case; the `[^1]`
        // is text-like inside the def.
        let has_ref = out.iter().any(|i| matches!(i, Inline::FootnoteRef(_)));
        assert!(!has_ref, "got: {out:?}");
    }

    #[test]
    fn inline_image_parses() {
        let out = parse_inline("see ![cat](https://cdn/cat.png) ok");
        match &out[1] {
            Inline::Image { alt, url } => {
                assert_eq!(alt, "cat");
                assert_eq!(url, "https://cdn/cat.png");
            }
            other => panic!("expected Image, got {other:?}"),
        }
    }

    #[test]
    fn page_embed_not_confused_with_image() {
        // `![[Page]]` is still an Embed, not an Image.
        let out = parse_inline("![[Page A]]");
        assert!(
            out.iter().any(|i| matches!(i, Inline::Embed { .. })),
            "expected Embed, got {out:?}"
        );
        assert!(
            !out.iter().any(|i| matches!(i, Inline::Image { .. })),
            "should not be Image, got {out:?}"
        );
    }

    #[test]
    fn external_link_rejects_url_with_space() {
        // Spaces in URLs are unusual; treat as plain text instead.
        let out = parse_inline("[a](b c)");
        assert!(
            !out.iter().any(|i| matches!(i, Inline::ExternalLink { .. })),
            "got: {out:?}"
        );
    }

    #[test]
    fn embed_parses_distinct_from_link() {
        let out = parse_inline("see ![[Page A]] and [[Page B]]");
        let kinds: Vec<_> = out
            .iter()
            .filter_map(|i| match i {
                Inline::Embed { target, .. } => Some(("embed", target.as_str())),
                Inline::Link { target, .. } => Some(("link", target.as_str())),
                _ => None,
            })
            .collect();
        assert_eq!(kinds, vec![("embed", "Page A"), ("link", "Page B")]);
    }

    #[test]
    fn embed_does_not_get_double_emitted_as_link() {
        // The `[[Page A]]` inside `![[Page A]]` must NOT also
        // produce an Inline::Link.
        let out = parse_inline("![[Page A]]");
        let link_count = out
            .iter()
            .filter(|i| matches!(i, Inline::Link { .. }))
            .count();
        assert_eq!(link_count, 0, "got: {out:?}");
    }

    #[test]
    fn embed_with_alias() {
        let out = parse_inline("![[Page|Display]]");
        match &out[0] {
            Inline::Embed { target, alias } => {
                assert_eq!(target, "Page");
                assert_eq!(alias.as_deref(), Some("Display"));
            }
            other => panic!("expected Embed, got {other:?}"),
        }
    }

    #[test]
    fn block_ref_parses_to_uuid() {
        let id = "550e8400-e29b-41d4-a716-446655440000";
        let out = parse_inline(&format!("see (({id})) end"));
        let parsed = out
            .iter()
            .find_map(|i| match i {
                Inline::BlockRef { target_block_id } => Some(*target_block_id),
                _ => None,
            })
            .expect("block ref present");
        assert_eq!(parsed, Uuid::parse_str(id).unwrap());
    }

    #[test]
    fn link_inside_bold_does_not_double_match() {
        let out = parse_inline("**[[Page]]**");
        assert_eq!(
            out,
            vec![Inline::Bold(vec![Inline::Link {
                target: "Page".into(),
                alias: None
            }])]
        );
    }
}
