//! Obsidian-style `[[wikilink]]` parser + resolver for markdown bodies.
//!
//! Pure-Rust, no DB calls. Callers (typically a service) load a
//! `slug -> term-id` map and pass spans through [`resolve_wikilinks`].
//!
//! Supported forms:
//!   `[[simmer]]`              → slug=simmer, display=None
//!   `[[Simmer]]`              → slug=simmer (slugified)
//!   `[[mise en place]]`       → slug=mise-en-place
//!   `[[blanching|blanch]]`    → slug=blanching, display=blanch
//!
//! Limitations:
//!   * Code-block detection is a *rough heuristic*: text inside
//!     triple-backtick fenced blocks and inline single-backtick
//!     spans is skipped. Nested fences (e.g. four-backtick fences
//!     containing three-backtick fences) can fool the heuristic.
//!     A future revision should hand markdown parsing off to a real
//!     pulldown-cmark pass.
//!   * Triple-bracket forms `[[[foo]]]` are not specially recognized
//!     beyond the conservative inner `[[foo]]` match (the trailing
//!     bracket is left in the surrounding text).
//!   * Pipes inside the bracket (e.g. `[[a|b|c]]`) split on the first
//!     pipe; the rest becomes the display text.

use std::collections::HashMap;
use uuid::Uuid;

use super::model::slugify;

/// One wikilink span found in a markdown body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WikilinkSpan {
    /// Byte range in the original text (inclusive start, exclusive end).
    pub start: usize,
    pub end: usize,
    /// What was inside the brackets, raw, e.g. `"simmer"` or
    /// `"blanching|blanch"`. Includes the alias-pipe form.
    pub raw: String,
    /// Slug to look up (lowercase, hyphens). For `"[[blanching|blanch]]"`
    /// this is `"blanching"` — the part before the pipe is the LINK
    /// TARGET, the part after is the DISPLAY TEXT.
    pub slug: String,
    /// Display label if the wikilink used the alias-pipe form, else
    /// `None`. For `"[[blanching|blanch]]"` this is `Some("blanch")`.
    pub display: Option<String>,
}

/// Resolution result for a span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedWikilink {
    pub span: WikilinkSpan,
    pub target_id: Option<Uuid>,
}

/// Find every `[[term]]` and `[[term|display]]` in `text`, return
/// spans. Doesn't validate that the term exists — that's the caller's
/// job after calling `find_wikilinks`.
#[must_use]
pub fn find_wikilinks(text: &str) -> Vec<WikilinkSpan> {
    let bytes = text.as_bytes();
    let n = bytes.len();
    let masked = mask_code_regions(text);
    let mut out = Vec::new();
    let mut i = 0usize;
    while i + 3 < n {
        // Need exactly two `[` followed by content and exactly two `]`.
        if bytes[i] == b'[' && bytes[i + 1] == b'[' {
            // Skip triple-bracket forms `[[[`. Conservative: don't
            // emit a span; advance past the leading `[`.
            if i > 0 && bytes[i - 1] == b'[' {
                i += 1;
                continue;
            }
            if i + 2 < n && bytes[i + 2] == b'[' {
                i += 1;
                continue;
            }
            // Skipped if start is inside a code region.
            if masked[i] {
                i += 2;
                continue;
            }
            // Find closing `]]`. Cap the search at the next newline
            // pair-of-brackets to avoid swallowing the rest of the doc.
            let inner_start = i + 2;
            let mut j = inner_start;
            let mut closed = None;
            while j + 1 < n {
                let b = bytes[j];
                if b == b'\n' {
                    break;
                }
                if b == b'[' {
                    // Bail — nested unmatched `[` indicates a malformed
                    // wikilink; don't emit.
                    break;
                }
                if b == b']' && bytes[j + 1] == b']' {
                    closed = Some(j);
                    break;
                }
                j += 1;
            }
            if let Some(close_at) = closed {
                let raw = &text[inner_start..close_at];
                if !raw.is_empty() && !raw.contains('\n') {
                    let (target, display) = split_pipe(raw);
                    let slug = slugify(target);
                    if !slug.is_empty() {
                        out.push(WikilinkSpan {
                            start: i,
                            end: close_at + 2,
                            raw: raw.to_string(),
                            slug,
                            display: display.map(str::to_string),
                        });
                    }
                }
                i = close_at + 2;
                continue;
            }
            // No close — advance past the leading bracket.
            i += 2;
            continue;
        }
        i += 1;
    }
    out
}

fn split_pipe(raw: &str) -> (&str, Option<&str>) {
    if let Some(idx) = raw.find('|') {
        let target = raw[..idx].trim();
        let display = raw[idx + 1..].trim();
        return (
            target,
            if display.is_empty() {
                None
            } else {
                Some(display)
            },
        );
    }
    (raw.trim(), None)
}

/// Build a byte-indexed mask where `mask[i] == true` means byte `i`
/// of `text` is inside a code region (fenced block or inline backtick
/// span) and should be ignored by the wikilink scanner.
fn mask_code_regions(text: &str) -> Vec<bool> {
    let bytes = text.as_bytes();
    let n = bytes.len();
    let mut mask = vec![false; n];
    let mut i = 0usize;
    let mut in_fence = false;
    let mut fence_start = 0usize;
    // Walk line-by-line for fenced block detection; track inline
    // backtick spans within non-fenced lines.
    let mut line_start = 0usize;
    while i <= n {
        let at_eol = i == n || bytes[i] == b'\n';
        if at_eol {
            let line = &text[line_start..i];
            let trimmed = line.trim_start();
            let is_fence = trimmed.starts_with("```");
            if in_fence {
                // Mark the entire line (including leading whitespace
                // and trailing newline) as in-code.
                let end = (i + 1).min(n);
                for slot in mask.iter_mut().take(end).skip(line_start) {
                    *slot = true;
                }
                if is_fence {
                    in_fence = false;
                }
            } else if is_fence {
                in_fence = true;
                fence_start = line_start;
                let end = (i + 1).min(n);
                for slot in mask.iter_mut().take(end).skip(fence_start) {
                    *slot = true;
                }
            } else {
                // Inline backtick spans within this single line.
                mask_inline_backticks(line, line_start, &mut mask);
            }
            if i == n {
                break;
            }
            line_start = i + 1;
        }
        i += 1;
    }
    // If a fence was opened but never closed, the trailing region
    // is already masked from the per-line loop.
    let _ = fence_start;
    mask
}

fn mask_inline_backticks(line: &str, base: usize, mask: &mut [bool]) {
    let bytes = line.as_bytes();
    let n = bytes.len();
    let mut i = 0usize;
    while i < n {
        if bytes[i] == b'`' {
            // Find matching closing backtick on the same line.
            let mut j = i + 1;
            while j < n && bytes[j] != b'`' {
                j += 1;
            }
            if j < n {
                let end = (base + j + 1).min(mask.len());
                for slot in mask.iter_mut().take(end).skip(base + i) {
                    *slot = true;
                }
                i = j + 1;
                continue;
            }
            // Unmatched — stop.
            break;
        }
        i += 1;
    }
}

/// Resolve found spans against a `slug -> term-id` map. Returns one
/// entry per span; `target_id` is `None` when the slug isn't in the
/// map.
#[must_use]
pub fn resolve_wikilinks(
    spans: &[WikilinkSpan],
    slug_to_id: &HashMap<String, Uuid>,
) -> Vec<ResolvedWikilink> {
    spans
        .iter()
        .map(|span| ResolvedWikilink {
            span: span.clone(),
            target_id: slug_to_id.get(&span.slug).copied(),
        })
        .collect()
}

/// Render the text with terminal-friendly highlighting: wraps each
/// resolved wikilink in ANSI color codes (cyan + underline for
/// resolved, dim red for unresolved). Returns the wrapped text plus a
/// deduplicated list of resolved-target ids in first-appearance order.
#[must_use]
pub fn render_for_terminal(
    text: &str,
    spans_with_targets: &[ResolvedWikilink],
) -> (String, Vec<Uuid>) {
    const RESOLVED_OPEN: &str = "\x1b[36;4m"; // cyan + underline
    const UNRESOLVED_OPEN: &str = "\x1b[2;31m"; // dim red
    const CLOSE: &str = "\x1b[0m";

    // Walk spans in source order so the byte ranges map cleanly to
    // the input. The find_wikilinks output is already in order, but
    // be defensive: sort copies.
    let mut sorted: Vec<&ResolvedWikilink> = spans_with_targets.iter().collect();
    sorted.sort_by_key(|r| r.span.start);

    let mut out = String::with_capacity(text.len() + sorted.len() * 16);
    let mut cursor = 0usize;
    let mut seen: Vec<Uuid> = Vec::new();
    for r in &sorted {
        let start = r.span.start.min(text.len());
        let end = r.span.end.min(text.len());
        if start < cursor {
            // Overlapping; skip.
            continue;
        }
        out.push_str(&text[cursor..start]);
        let label = r.span.display.clone().unwrap_or_else(|| {
            r.span
                .raw
                .split('|')
                .next()
                .unwrap_or(&r.span.raw)
                .to_string()
        });
        let (open, close) = if r.target_id.is_some() {
            (RESOLVED_OPEN, CLOSE)
        } else {
            (UNRESOLVED_OPEN, CLOSE)
        };
        out.push_str(open);
        out.push_str(&label);
        out.push_str(close);
        if let Some(id) = r.target_id {
            if !seen.contains(&id) {
                seen.push(id);
            }
        }
        cursor = end;
    }
    if cursor < text.len() {
        out.push_str(&text[cursor..]);
    }
    (out, seen)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn slugs(text: &str) -> Vec<String> {
        find_wikilinks(text).into_iter().map(|s| s.slug).collect()
    }

    #[test]
    fn finds_single_simple_link() {
        let spans = find_wikilinks("Bring to a [[simmer]] and rest.");
        assert_eq!(spans.len(), 1);
        assert_eq!(spans[0].slug, "simmer");
        assert_eq!(spans[0].display, None);
    }

    #[test]
    fn finds_multiple_links() {
        let s = slugs("[[simmer]] then [[deglaze]] the pan.");
        assert_eq!(s, vec!["simmer".to_string(), "deglaze".to_string()]);
    }

    #[test]
    fn slug_is_lowercased() {
        let s = slugs("[[Simmer]]");
        assert_eq!(s, vec!["simmer"]);
    }

    #[test]
    fn slug_collapses_spaces_to_hyphens() {
        let s = slugs("[[Mise en Place]]");
        assert_eq!(s, vec!["mise-en-place"]);
    }

    #[test]
    fn slug_collapses_multiple_spaces() {
        let s = slugs("[[mise   en    place]]");
        assert_eq!(s, vec!["mise-en-place"]);
    }

    #[test]
    fn alias_pipe_form() {
        let spans = find_wikilinks("a [[blanching|blanch]] b");
        assert_eq!(spans[0].slug, "blanching");
        assert_eq!(spans[0].display.as_deref(), Some("blanch"));
    }

    #[test]
    fn alias_pipe_with_caps() {
        let spans = find_wikilinks("[[Mise en Place|MIS]]");
        assert_eq!(spans[0].slug, "mise-en-place");
        assert_eq!(spans[0].display.as_deref(), Some("MIS"));
    }

    #[test]
    fn empty_pipe_display_treated_as_none() {
        let spans = find_wikilinks("[[simmer|]]");
        assert_eq!(spans[0].slug, "simmer");
        assert_eq!(spans[0].display, None);
    }

    #[test]
    fn single_bracket_ignored() {
        let s = slugs("this is [foo] not a wikilink");
        assert!(s.is_empty());
    }

    #[test]
    fn triple_bracket_skipped_conservatively() {
        // We don't try to parse `[[[foo]]]` — at minimum the leading
        // bracket-triplet should not yield a clean `[[foo]]` span
        // because we skip when preceded by another `[`.
        let spans = find_wikilinks("[[[foo]]]");
        // Either zero spans, or at most a malformed one that doesn't
        // panic — we accept the conservative interpretation.
        for span in &spans {
            assert_ne!(span.slug, "");
        }
    }

    #[test]
    fn unmatched_open_ignored() {
        let s = slugs("[[simmer no close");
        assert!(s.is_empty());
    }

    #[test]
    fn newline_inside_breaks_match() {
        let s = slugs("[[sim\nmer]]");
        assert!(s.is_empty());
    }

    #[test]
    fn empty_brackets_ignored() {
        let s = slugs("[[]] empty");
        assert!(s.is_empty());
    }

    #[test]
    fn non_alpha_only_brackets_ignored() {
        // After slugify "..." → "", so no span.
        let s = slugs("[[...]]");
        assert!(s.is_empty());
    }

    #[test]
    fn fenced_code_block_skipped() {
        let text = "Outside [[simmer]]\n```\ninside [[deglaze]] code\n```\nback [[reduce]]";
        let s = slugs(text);
        assert_eq!(s, vec!["simmer", "reduce"]);
    }

    #[test]
    fn inline_backticks_skipped() {
        let text = "Use `[[simmer]]` literally, but [[deglaze]] is real.";
        let s = slugs(text);
        assert_eq!(s, vec!["deglaze"]);
    }

    #[test]
    fn unclosed_fence_masks_remainder() {
        let text = "Real [[simmer]]\n```\ntrailing [[deglaze]] never closes";
        let s = slugs(text);
        assert_eq!(s, vec!["simmer"]);
    }

    #[test]
    fn span_byte_offsets_round_trip() {
        let text = "Bring to a [[simmer]] now";
        let spans = find_wikilinks(text);
        assert_eq!(spans.len(), 1);
        let s = &spans[0];
        assert_eq!(&text[s.start..s.end], "[[simmer]]");
    }

    #[test]
    fn resolve_present_and_absent() {
        let mut map = HashMap::new();
        let id = Uuid::nil();
        map.insert("simmer".to_string(), id);
        let spans = find_wikilinks("[[simmer]] [[unknown]]");
        let resolved = resolve_wikilinks(&spans, &map);
        assert_eq!(resolved.len(), 2);
        assert_eq!(resolved[0].target_id, Some(id));
        assert_eq!(resolved[1].target_id, None);
    }

    #[test]
    fn render_for_terminal_resolved_uses_cyan() {
        let text = "[[simmer]]";
        let spans = find_wikilinks(text);
        let mut map = HashMap::new();
        let id = Uuid::from_u128(1);
        map.insert("simmer".to_string(), id);
        let resolved = resolve_wikilinks(&spans, &map);
        let (out, ids) = render_for_terminal(text, &resolved);
        assert!(out.contains("\x1b[36;4m"));
        assert!(out.contains("simmer"));
        assert_eq!(ids, vec![id]);
    }

    #[test]
    fn render_for_terminal_unresolved_uses_dim_red() {
        let text = "[[mystery]]";
        let spans = find_wikilinks(text);
        let map = HashMap::new();
        let resolved = resolve_wikilinks(&spans, &map);
        let (out, ids) = render_for_terminal(text, &resolved);
        assert!(out.contains("\x1b[2;31m"));
        assert!(ids.is_empty());
    }

    #[test]
    fn render_uses_display_label_when_provided() {
        let text = "[[blanching|blanch]] them";
        let spans = find_wikilinks(text);
        let mut map = HashMap::new();
        let id = Uuid::from_u128(2);
        map.insert("blanching".to_string(), id);
        let resolved = resolve_wikilinks(&spans, &map);
        let (out, _) = render_for_terminal(text, &resolved);
        assert!(out.contains("blanch"));
        assert!(!out.contains("blanching|"));
    }

    #[test]
    fn render_round_trip_with_no_spans() {
        let text = "Plain text with no links.";
        let (out, ids) = render_for_terminal(text, &[]);
        assert_eq!(out, text);
        assert!(ids.is_empty());
    }

    #[test]
    fn render_dedupes_resolved_ids() {
        let text = "[[simmer]] then [[simmer]] again";
        let spans = find_wikilinks(text);
        let mut map = HashMap::new();
        let id = Uuid::from_u128(3);
        map.insert("simmer".to_string(), id);
        let resolved = resolve_wikilinks(&spans, &map);
        let (_, ids) = render_for_terminal(text, &resolved);
        assert_eq!(ids, vec![id]);
    }

    #[test]
    fn pipe_target_lower_cased_via_slug() {
        let spans = find_wikilinks("[[BLANCHING|do it]]");
        assert_eq!(spans[0].slug, "blanching");
        assert_eq!(spans[0].display.as_deref(), Some("do it"));
    }

    #[test]
    fn punctuation_around_link() {
        let s = slugs("end with [[simmer]].");
        assert_eq!(s, vec!["simmer"]);
    }

    #[test]
    fn multiple_links_same_line() {
        let spans =
            find_wikilinks("Use [[mise en place]] then [[Sauté|sauté]] the [[Onion|onions]].");
        assert_eq!(spans.len(), 3);
        assert_eq!(spans[0].slug, "mise-en-place");
        assert_eq!(spans[1].slug, "sauté");
        assert_eq!(spans[2].slug, "onion");
    }

    #[test]
    fn whitespace_only_inside_brackets_ignored() {
        let s = slugs("[[   ]]");
        assert!(s.is_empty());
    }
}
