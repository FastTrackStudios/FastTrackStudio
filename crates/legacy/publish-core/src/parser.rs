//! Inline-markup parser for the publisher.
//!
//! Mirrors a subset of `knowledge_ui::inline_md` but emits a
//! `Node` enum that's safe to render via Dioxus components.
//! Wikilinks and block refs are *resolved at parse time* against
//! `WikiResolver` / `BlockRefResolver` lookups so the renderer
//! doesn't need context — the AST carries the resolved URL +
//! snippet directly.

use crate::components::{
    BlockRefResolver, NamespaceResolver, PageEmbedResolver, PagePropertyResolver, QueryHit,
    QueryResolver, TemplateResolver, WikiResolver,
};
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Text(String),
    Bold(Vec<Node>),
    Italic(Vec<Node>),
    Strikethrough(Vec<Node>),
    Highlight(Vec<Node>),
    Code(String),
    Wikilink {
        slug: String,
        label: String,
        broken: bool,
    },
    ExternalLink {
        label: String,
        url: String,
    },
    /// `((<uuid>))` — points at a specific block by stable id.
    /// Resolved at parse time: `page_slug` is the destination
    /// page slug (`""` if broken), `snippet` is a short preview of
    /// the target block's content. `broken: true` when the UUID
    /// is parseable but not present in the vault.
    BlockRef {
        target_id: Uuid,
        page_slug: String,
        snippet: String,
        broken: bool,
    },
    /// `![[Page]]` — embed the target page inline. Resolved at
    /// parse time: `slug` is the destination page slug,
    /// `contents` is the ordered raw block-content strings (will
    /// be re-parsed with an empty PageEmbedResolver at render
    /// time to bound recursion). `broken: true` when no page
    /// matches the basename.
    PageEmbed {
        slug: String,
        label: String,
        contents: Vec<String>,
        broken: bool,
    },
    /// `{{query <expr>}}` — embedded live query. Evaluated at
    /// parse time against a [`QueryResolver`] and rendered as a
    /// bullet list of matching pages. v1 supports the form
    /// `{{query #tag}}`; unknown forms emit `broken: true` and
    /// render as an inert pill.
    Query {
        expr: String,
        results: Vec<QueryHit>,
        broken: bool,
    },
    /// `{{namespace foo/bar}}` — list every page whose basename
    /// starts with `foo/bar/`. Resolved at parse time against a
    /// [`NamespaceResolver`].
    Namespace {
        prefix: String,
        results: Vec<QueryHit>,
    },
    /// `![alt](url)` — inline markdown image. Distinct from
    /// `![[Page]]` page-embed, which is matched first.
    Image {
        alt: String,
        url: String,
    },
    /// `[^id]` — superscript footnote reference. The definition
    /// `[^id]: body` block is treated as a normal paragraph by
    /// this inline parser; rendering renders the definitions
    /// separately at block level.
    FootnoteRef(String),
    /// `{{video <url>}}` / `{{youtube <id-or-url>}}` /
    /// `{{tweet <id-or-url>}}` — media embed macros. `kind`
    /// selects the renderer (HTML `<video>`, YouTube iframe,
    /// Twitter blockquote stub); `target` is the URL or
    /// extracted id.
    Media {
        kind: MediaKind,
        target: String,
    },
    /// `$expr$` — inline LaTeX. The renderer wraps it in a
    /// `<span class="math math-inline">` and emits the raw source
    /// as text content; a host-provided MathJax/KaTeX script then
    /// upgrades the spans client-side.
    MathInline(String),
    /// `$$expr$$` — block LaTeX. Same idea, rendered as a `<div>`.
    MathBlock(String),
    /// `{{template <name>}}` — expand the template block's contents
    /// inline. Resolved at parse time against a [`TemplateResolver`];
    /// `broken: true` when no template matches.
    Template {
        name: String,
        contents: Vec<String>,
        broken: bool,
    },
    /// `#tag` — Logseq-style inline tag. The lowercased name is
    /// the canonical key; the renderer wraps it in a chip + clicking
    /// it (via a host-provided navigator) opens the tag page.
    Hashtag(String),
    /// `{{pdf <url>}}` — open the referenced PDF in the dedicated
    /// PDF reader pane. The renderer emits a chip with a
    /// `data-pdf-url` attribute that a host-installed click
    /// delegate uses to navigate. Mirrors Logseq's PDF UX where
    /// every reference to an asset PDF becomes a clickable chip.
    PdfMacro(String),
    /// `{{video-timestamp <secs>}}` or `{{youtube-timestamp <secs>}}`
    /// — clickable timestamp chip. The renderer wires it to seek
    /// the nearest video / youtube player in the surrounding block
    /// when clicked. Stored as raw seconds so HH:MM:SS rendering
    /// is locale-independent.
    VideoTimestamp(i32),
}

/// Closed set of media-embed kinds recognized by the parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MediaKind {
    Video,
    Youtube,
    Tweet,
}

pub fn parse(
    s: &str,
    resolver: &WikiResolver,
    blocks: &BlockRefResolver,
    page_embeds: &PageEmbedResolver,
    queries: &QueryResolver,
    namespaces: &NamespaceResolver,
    properties: &PagePropertyResolver,
    templates: &TemplateResolver,
) -> Vec<Node> {
    let mut out = Vec::new();
    let mut buf = String::new();
    let bytes = s.as_bytes();
    let mut i = 0usize;
    while i < s.len() {
        // $$expr$$ — block math. Match before single-$ inline math
        // so the longer delimiter wins. Body must be non-empty.
        if s[i..].starts_with("$$") {
            if let Some(end) = s[i + 2..].find("$$") {
                let body = &s[i + 2..i + 2 + end];
                if !body.trim().is_empty() {
                    flush(&mut buf, &mut out);
                    out.push(Node::MathBlock(body.to_string()));
                    i += 2 + end + 2;
                    continue;
                }
            }
        }
        // $expr$ — inline math. Body must be non-empty and the
        // closing `$` must not be preceded by a space (to avoid
        // accidentally matching prose with two currency markers).
        if bytes[i] == b'$' && i + 1 < s.len() && bytes[i + 1] != b'$' {
            if let Some(end_rel) = s[i + 1..].find('$') {
                let body = &s[i + 1..i + 1 + end_rel];
                if !body.is_empty() && !body.starts_with(' ') && !body.ends_with(' ') {
                    flush(&mut buf, &mut out);
                    out.push(Node::MathInline(body.to_string()));
                    i += 1 + end_rel + 1;
                    continue;
                }
            }
        }
        // {{namespace <prefix>}} — list pages under a namespace.
        if s[i..].starts_with("{{namespace") {
            if let Some(end) = s[i + 11..].find("}}") {
                let raw = s[i + 11..i + 11 + end].trim();
                let results = namespaces
                    .0
                    .get(&raw.to_lowercase())
                    .cloned()
                    .unwrap_or_default();
                flush(&mut buf, &mut out);
                out.push(Node::Namespace {
                    prefix: raw.to_string(),
                    results,
                });
                i += 11 + end + 2;
                continue;
            }
        }
        // {{template <name>}} — expand the named template block's
        // contents inline. `broken: true` when no template matches.
        // {{video-timestamp <s>}} or {{youtube-timestamp <s>}} —
        // clickable seek chip. Seconds may be a bare integer or
        // `HH:MM:SS` / `MM:SS`; we normalize to seconds at parse
        // time so the renderer doesn't need to.
        // {{pdf <url>}} — open the referenced PDF in the reader.
        if s[i..].starts_with("{{pdf") {
            if let Some(end) = s[i + 5..].find("}}") {
                let url = s[i + 5..i + 5 + end].trim();
                if !url.is_empty() {
                    flush(&mut buf, &mut out);
                    out.push(Node::PdfMacro(url.to_string()));
                    i += 5 + end + 2;
                    continue;
                }
            }
        }
        let ts_advance = (|| -> Option<(i32, usize)> {
            for prefix in ["{{video-timestamp", "{{youtube-timestamp"] {
                if s[i..].starts_with(prefix) {
                    let end = s[i + prefix.len()..].find("}}")?;
                    let raw = s[i + prefix.len()..i + prefix.len() + end].trim();
                    let secs = parse_timestamp_seconds(raw)?;
                    return Some((secs, prefix.len() + end + 2));
                }
            }
            None
        })();
        if let Some((secs, advance)) = ts_advance {
            flush(&mut buf, &mut out);
            out.push(Node::VideoTimestamp(secs));
            i += advance;
            continue;
        }
        if s[i..].starts_with("{{template") {
            if let Some(end) = s[i + 10..].find("}}") {
                let name = s[i + 10..i + 10 + end].trim();
                let key = name.to_lowercase();
                let contents = templates.0.get(&key).cloned();
                flush(&mut buf, &mut out);
                out.push(Node::Template {
                    name: name.to_string(),
                    broken: contents.is_none(),
                    contents: contents.unwrap_or_default(),
                });
                i += 10 + end + 2;
                continue;
            }
        }
        // {{embed [[Page]]}} or {{embed ((uuid))}} — Logseq's
        // embed macro. Lower into the corresponding PageEmbed or
        // BlockRef so the downstream renderer doesn't need new
        // node variants. Recognized payloads:
        //   {{embed [[Page]]}}     → PageEmbed
        //   {{embed ((uuid))}}     → BlockRef
        if s[i..].starts_with("{{embed") {
            if let Some(end) = s[i + 7..].find("}}") {
                let raw = s[i + 7..i + 7 + end].trim();
                // Page-embed form.
                if let Some(inner) = raw.strip_prefix("[[").and_then(|r| r.strip_suffix("]]")) {
                    let (target, alias) = match inner.split_once('|') {
                        Some((t, a)) => (t.trim(), a.trim()),
                        None => (inner.trim(), inner.trim()),
                    };
                    let key = target.to_lowercase();
                    let slug = resolver.0.get(&key).cloned();
                    let contents = page_embeds.0.get(&key).cloned().unwrap_or_default();
                    flush(&mut buf, &mut out);
                    out.push(Node::PageEmbed {
                        broken: slug.is_none(),
                        slug: slug.unwrap_or_default(),
                        label: alias.to_string(),
                        contents,
                    });
                    i += 7 + end + 2;
                    continue;
                }
                // Block-embed form.
                if let Some(inner) = raw.strip_prefix("((").and_then(|r| r.strip_suffix("))")) {
                    if let Ok(id) = Uuid::parse_str(inner.trim()) {
                        flush(&mut buf, &mut out);
                        let target = blocks.0.get(&id);
                        out.push(Node::BlockRef {
                            target_id: id,
                            page_slug: target.map(|t| t.page_slug.clone()).unwrap_or_default(),
                            snippet: target
                                .map(|t| t.snippet.clone())
                                .unwrap_or_else(|| short_uuid(&id)),
                            broken: target.is_none(),
                        });
                        i += 7 + end + 2;
                        continue;
                    }
                }
                // Unrecognized embed payload — fall through and
                // let text accumulate.
            }
        }
        // {{video <url>}} / {{youtube <id-or-url>}} /
        // {{tweet <id-or-url>}} — media embed macros. Helper
        // returns Some(advance) on match.
        let media_advance = (|| -> Option<usize> {
            for (prefix, kind) in [
                ("{{video", MediaKind::Video),
                ("{{youtube", MediaKind::Youtube),
                ("{{tweet", MediaKind::Tweet),
            ] {
                if s[i..].starts_with(prefix) {
                    let after = i + prefix.len();
                    if let Some(end) = s[after..].find("}}") {
                        let target = s[after..after + end].trim().to_string();
                        if !target.is_empty() {
                            flush(&mut buf, &mut out);
                            out.push(Node::Media { kind, target });
                            return Some(after + end + 2);
                        }
                    }
                }
            }
            None
        })();
        if let Some(next_i) = media_advance {
            i = next_i;
            continue;
        }
        // {{query <expr>}} — embedded live query. Match before
        // emphasis / brackets so the literal sigil wins.
        if s[i..].starts_with("{{query") {
            // Need at least `{{query ` followed by `}}` somewhere.
            if let Some(end) = s[i + 7..].find("}}") {
                let raw = s[i + 7..i + 7 + end].trim();
                let (results, broken) = eval_query(raw, queries, properties);
                flush(&mut buf, &mut out);
                out.push(Node::Query {
                    expr: raw.to_string(),
                    results,
                    broken,
                });
                i += 7 + end + 2;
                continue;
            }
        }
        // ![[Page]] — page embed, OR ![[image.png]] — image
        // wikilink. Match BEFORE wikilink so the bang prefix wins;
        // mirrors Obsidian's `![[...]]` semantics. When the target
        // ends in a known image extension, emit an `Image` with the
        // file basename as the url (the AssetBaseResolver rewrites
        // it at render time).
        if s[i..].starts_with("![[") {
            if let Some(end) = s[i + 3..].find("]]") {
                let body = &s[i + 3..i + 3 + end];
                let (target, alias) = match body.split_once('|') {
                    Some((t, a)) => (t.trim(), a.trim()),
                    None => (body.trim(), body.trim()),
                };
                if is_image_target(target) {
                    flush(&mut buf, &mut out);
                    out.push(Node::Image {
                        alt: alias.to_string(),
                        url: format!("assets/{target}"),
                    });
                    i += 3 + end + 2;
                    continue;
                }
                let key = target.to_lowercase();
                let slug = resolver.0.get(&key).cloned();
                let contents = page_embeds.0.get(&key).cloned().unwrap_or_default();
                flush(&mut buf, &mut out);
                out.push(Node::PageEmbed {
                    broken: slug.is_none(),
                    slug: slug.unwrap_or_default(),
                    label: alias.to_string(),
                    contents,
                });
                i += 3 + end + 2;
                continue;
            }
        }
        // ((<uuid>)) — block reference. Match before wikilink so
        // `((` is preferred over a hypothetical bracket sequence.
        // UUID v4 canonical form is 36 chars (8-4-4-4-12 with
        // hyphens); we accept any parseable UUID inside `((…))`.
        if s[i..].starts_with("((") {
            if let Some(end) = s[i + 2..].find("))") {
                let body = s[i + 2..i + 2 + end].trim();
                if let Ok(id) = Uuid::parse_str(body) {
                    flush(&mut buf, &mut out);
                    let target = blocks.0.get(&id);
                    out.push(Node::BlockRef {
                        target_id: id,
                        page_slug: target.map(|t| t.page_slug.clone()).unwrap_or_default(),
                        snippet: target
                            .map(|t| t.snippet.clone())
                            .unwrap_or_else(|| short_uuid(&id)),
                        broken: target.is_none(),
                    });
                    i += 2 + end + 2;
                    continue;
                }
            }
        }
        // ![alt](url) — inline markdown image. Matches AFTER
        // `![[Page]]` so the page-embed form wins; before `[
        // label](url)` so the bang prefix isn't confused with a
        // text-emoji.
        if bytes[i] == b'!' && bytes.get(i + 1).copied() == Some(b'[') {
            // Skip if this is actually `![[...`.
            if bytes.get(i + 2).copied() != Some(b'[') {
                if let Some(alt_end) = s[i + 2..].find(']') {
                    let after = i + 2 + alt_end + 1;
                    if bytes.get(after).copied() == Some(b'(') {
                        if let Some(url_end) = s[after + 1..].find(')') {
                            let alt = &s[i + 2..i + 2 + alt_end];
                            let url = &s[after + 1..after + 1 + url_end];
                            if !url.is_empty() && !url.contains(' ') {
                                flush(&mut buf, &mut out);
                                out.push(Node::Image {
                                    alt: alt.to_string(),
                                    url: url.to_string(),
                                });
                                i = after + 1 + url_end + 1;
                                continue;
                            }
                        }
                    }
                }
            }
        }
        // [^id] — footnote reference. Matches before plain `[a](b)`
        // so the caret prefix isn't consumed as a label.
        if bytes[i] == b'[' && bytes.get(i + 1).copied() == Some(b'^') {
            if let Some(end) = s[i + 2..].find(']') {
                let id = &s[i + 2..i + 2 + end];
                // Validate the id contains only sane chars.
                if !id.is_empty()
                    && id
                        .chars()
                        .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
                {
                    // Make sure this isn't actually the start of
                    // a `[^id]: body` definition — those are
                    // block-level, parsed by the renderer
                    // separately. Heuristic: leave the inline
                    // ref to be parsed; definitions only appear
                    // at start of content with a `:` after `]`.
                    let after = i + 2 + end + 1;
                    let is_definition = i == 0 && bytes.get(after).copied() == Some(b':');
                    if !is_definition {
                        flush(&mut buf, &mut out);
                        out.push(Node::FootnoteRef(id.to_string()));
                        i = after;
                        continue;
                    }
                }
            }
        }
        // [[wikilink]] / [[Page|Alias]]
        if s[i..].starts_with("[[") {
            if let Some(end) = s[i + 2..].find("]]") {
                let body = &s[i + 2..i + 2 + end];
                let (target, alias) = match body.split_once('|') {
                    Some((t, a)) => (t.trim(), a.trim()),
                    None => (body.trim(), body.trim()),
                };
                let slug = resolver.0.get(&target.to_lowercase()).cloned();
                flush(&mut buf, &mut out);
                out.push(Node::Wikilink {
                    broken: slug.is_none(),
                    slug: slug.unwrap_or_default(),
                    label: alias.to_string(),
                });
                i += 2 + end + 2;
                continue;
            }
        }
        // [label](url) — external. Must come before the bare `*`/`**`
        // checks so `[a](b)` isn't mistaken for emphasis.
        if bytes[i] == b'[' && bytes.get(i + 1).copied() != Some(b'[') {
            if let Some(label_end) = s[i + 1..].find(']') {
                let after = i + 1 + label_end + 1;
                if bytes.get(after).copied() == Some(b'(') {
                    if let Some(url_end) = s[after + 1..].find(')') {
                        let label = &s[i + 1..i + 1 + label_end];
                        let url = &s[after + 1..after + 1 + url_end];
                        if !url.is_empty() && !url.contains(' ') {
                            flush(&mut buf, &mut out);
                            out.push(Node::ExternalLink {
                                label: label.to_string(),
                                url: url.to_string(),
                            });
                            i = after + 1 + url_end + 1;
                            continue;
                        }
                    }
                }
            }
        }
        // **bold**
        if s[i..].starts_with("**") {
            if let Some(end) = s[i + 2..].find("**") {
                let inner = parse(
                    &s[i + 2..i + 2 + end],
                    resolver,
                    blocks,
                    page_embeds,
                    queries,
                    namespaces,
                    properties,
                    templates,
                );
                flush(&mut buf, &mut out);
                out.push(Node::Bold(inner));
                i += 2 + end + 2;
                continue;
            }
        }
        // ~~strike~~
        if s[i..].starts_with("~~") {
            if let Some(end) = s[i + 2..].find("~~") {
                let inner = parse(
                    &s[i + 2..i + 2 + end],
                    resolver,
                    blocks,
                    page_embeds,
                    queries,
                    namespaces,
                    properties,
                    templates,
                );
                flush(&mut buf, &mut out);
                out.push(Node::Strikethrough(inner));
                i += 2 + end + 2;
                continue;
            }
        }
        // ==highlight==
        if s[i..].starts_with("==") {
            if let Some(end) = s[i + 2..].find("==") {
                let inner = parse(
                    &s[i + 2..i + 2 + end],
                    resolver,
                    blocks,
                    page_embeds,
                    queries,
                    namespaces,
                    properties,
                    templates,
                );
                flush(&mut buf, &mut out);
                out.push(Node::Highlight(inner));
                i += 2 + end + 2;
                continue;
            }
        }
        // *italic* (not part of `**`)
        if bytes[i] == b'*' && bytes.get(i + 1).copied() != Some(b'*') {
            if let Some(end) = s[i + 1..].find('*') {
                let inner = parse(
                    &s[i + 1..i + 1 + end],
                    resolver,
                    blocks,
                    page_embeds,
                    queries,
                    namespaces,
                    properties,
                    templates,
                );
                flush(&mut buf, &mut out);
                out.push(Node::Italic(inner));
                i += 1 + end + 1;
                continue;
            }
        }
        // `code`
        if bytes[i] == b'`' {
            if let Some(end) = s[i + 1..].find('`') {
                let inner = &s[i + 1..i + 1 + end];
                if !inner.is_empty() {
                    flush(&mut buf, &mut out);
                    out.push(Node::Code(inner.to_string()));
                    i += 1 + end + 1;
                    continue;
                }
            }
        }
        // #tag — inline hashtag. Requires `#` at start of input or
        // after whitespace (so `c#sharp` isn't a tag), and at least
        // one tag-safe char. Stops at whitespace or punctuation.
        if bytes[i] == b'#' && (i == 0 || (bytes[i - 1] as char).is_whitespace()) {
            let mut j = i + 1;
            while j < s.len() {
                let c = bytes[j] as char;
                if c.is_alphanumeric() || c == '-' || c == '_' || c == '/' {
                    j += 1;
                } else {
                    break;
                }
            }
            if j > i + 1 {
                let tag = &s[i + 1..j];
                flush(&mut buf, &mut out);
                out.push(Node::Hashtag(tag.to_string()));
                i = j;
                continue;
            }
        }
        let c = s[i..].chars().next().expect("non-empty");
        buf.push(c);
        i += c.len_utf8();
    }
    flush(&mut buf, &mut out);
    out
}

fn flush(buf: &mut String, out: &mut Vec<Node>) {
    if !buf.is_empty() {
        out.push(Node::Text(std::mem::take(buf)));
    }
}

/// 8-char prefix of a UUID — used as the visible label for a
/// broken block reference where we have no snippet to show.
/// Parse `42`, `1:23`, or `1:23:45` into a count of seconds. Returns
/// None on garbage input. Whitespace is trimmed by the caller.
pub fn parse_timestamp_seconds(s: &str) -> Option<i32> {
    if !s.contains(':') {
        return s.parse::<i32>().ok().filter(|n| *n >= 0);
    }
    let parts: Vec<&str> = s.split(':').collect();
    let nums: Vec<i32> = parts
        .iter()
        .map(|p| p.parse::<i32>().ok())
        .collect::<Option<Vec<_>>>()?;
    if nums.iter().any(|n| *n < 0) {
        return None;
    }
    match nums.as_slice() {
        [m, sec] => Some(m * 60 + sec),
        [h, m, sec] => Some(h * 3600 + m * 60 + sec),
        _ => None,
    }
}

/// Format a seconds count back to HH:MM:SS (drops hours when 0).
/// Used by the renderer for the chip label.
pub fn format_timestamp(seconds: i32) -> String {
    let h = seconds / 3600;
    let m = (seconds % 3600) / 60;
    let s = seconds % 60;
    if h > 0 {
        format!("{h}:{m:02}:{s:02}")
    } else {
        format!("{m}:{s:02}")
    }
}

/// True when a `![[…]]` target's extension marks it as an image.
fn is_image_target(target: &str) -> bool {
    let lower = target.to_lowercase();
    for ext in [
        ".png", ".jpg", ".jpeg", ".gif", ".webp", ".svg", ".bmp", ".avif",
    ] {
        if lower.ends_with(ext) {
            return true;
        }
    }
    false
}

fn short_uuid(id: &Uuid) -> String {
    id.simple().to_string().chars().take(8).collect()
}

/// Evaluate a `{{query <expr>}}` body against a resolver.
///
/// Grammar:
/// - `#<tag>`                — pages tagged `tag`
/// - `(and <expr> <expr>…)`  — set intersection
/// - `(or  <expr> <expr>…)`  — set union
/// - `(not <expr>)`          — complement against the resolver
///                             universe (pages with at least one tag)
///
/// Returns `(results, broken)`. `broken = true` only when the
/// expression is structurally unrecognizable; `false` covers
/// "known shape but no matches."
pub fn eval_query(
    expr: &str,
    queries: &QueryResolver,
    properties: &PagePropertyResolver,
) -> (Vec<QueryHit>, bool) {
    match parse_query_expr(expr.trim()) {
        Some(qe) => (eval_parsed(&qe, queries, properties), false),
        None => (Vec::new(), true),
    }
}

/// AST for the query mini-language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QueryExpr {
    Tag(String),
    And(Vec<QueryExpr>),
    Or(Vec<QueryExpr>),
    Not(Box<QueryExpr>),
    /// `(property <key> <value>)` — pages whose frontmatter has
    /// `<key>:: <value>`. Both are lowercased for matching.
    PageProperty(String, String),
}

/// Top-level parser entry. Returns `None` on malformed input.
pub fn parse_query_expr(s: &str) -> Option<QueryExpr> {
    let s = s.trim();
    if s.starts_with('#') {
        let tag = s[1..].trim();
        if tag.is_empty() {
            return None;
        }
        return Some(QueryExpr::Tag(tag.to_lowercase()));
    }
    if s.starts_with('(') && s.ends_with(')') {
        let inner = &s[1..s.len() - 1];
        let mut tokens = tokenize_sexp(inner)?;
        if tokens.is_empty() {
            return None;
        }
        let head = tokens.remove(0);
        match head.as_str() {
            "and" => {
                let args: Option<Vec<QueryExpr>> =
                    tokens.iter().map(|t| parse_query_expr(t)).collect();
                args.map(QueryExpr::And)
            }
            "or" => {
                let args: Option<Vec<QueryExpr>> =
                    tokens.iter().map(|t| parse_query_expr(t)).collect();
                args.map(QueryExpr::Or)
            }
            "not" => {
                if tokens.len() != 1 {
                    return None;
                }
                parse_query_expr(&tokens[0]).map(|e| QueryExpr::Not(Box::new(e)))
            }
            "property" => {
                if tokens.len() != 2 {
                    return None;
                }
                Some(QueryExpr::PageProperty(
                    tokens[0].to_lowercase(),
                    tokens[1].to_lowercase(),
                ))
            }
            _ => None,
        }
    } else {
        None
    }
}

/// Tokenize the body of an s-expression into top-level tokens,
/// respecting nested `()` groups. Whitespace separates tokens
/// outside of parens.
fn tokenize_sexp(s: &str) -> Option<Vec<String>> {
    let mut out: Vec<String> = Vec::new();
    let mut buf = String::new();
    let mut depth = 0i32;
    for c in s.chars() {
        match c {
            '(' => {
                depth += 1;
                buf.push(c);
            }
            ')' => {
                depth -= 1;
                if depth < 0 {
                    return None;
                }
                buf.push(c);
            }
            c if c.is_whitespace() && depth == 0 => {
                if !buf.is_empty() {
                    out.push(std::mem::take(&mut buf));
                }
            }
            _ => buf.push(c),
        }
    }
    if depth != 0 {
        return None;
    }
    if !buf.is_empty() {
        out.push(buf);
    }
    Some(out)
}

fn eval_parsed(
    expr: &QueryExpr,
    queries: &QueryResolver,
    properties: &PagePropertyResolver,
) -> Vec<QueryHit> {
    match expr {
        QueryExpr::Tag(t) => queries.0.get(t).cloned().unwrap_or_default(),
        QueryExpr::PageProperty(k, v) => properties
            .0
            .get(k)
            .and_then(|m| m.get(v))
            .cloned()
            .unwrap_or_default(),
        QueryExpr::And(children) => {
            if children.is_empty() {
                return Vec::new();
            }
            let mut acc = eval_parsed(&children[0], queries, properties);
            for c in &children[1..] {
                let set = eval_parsed(c, queries, properties);
                acc.retain(|h| set.iter().any(|s| s.slug == h.slug));
            }
            dedupe_sort(acc)
        }
        QueryExpr::Or(children) => {
            let mut acc: Vec<QueryHit> = Vec::new();
            for c in children {
                acc.extend(eval_parsed(c, queries, properties));
            }
            dedupe_sort(acc)
        }
        QueryExpr::Not(inner) => {
            let exclude = eval_parsed(inner, queries, properties);
            // Universe = every page hit across both resolvers
            // (tags + properties). Conservative; in practice the
            // tag index already covers "every page" since we
            // index pages by their tags.
            let mut universe: Vec<QueryHit> = Vec::new();
            for hits in queries.0.values() {
                universe.extend(hits.iter().cloned());
            }
            for kmap in properties.0.values() {
                for hits in kmap.values() {
                    universe.extend(hits.iter().cloned());
                }
            }
            let universe = dedupe_sort(universe);
            universe
                .into_iter()
                .filter(|u| !exclude.iter().any(|x| x.slug == u.slug))
                .collect()
        }
    }
}

fn dedupe_sort(mut v: Vec<QueryHit>) -> Vec<QueryHit> {
    v.sort_by(|a, b| a.slug.cmp(&b.slug));
    v.dedup_by(|a, b| a.slug == b.slug);
    v
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::BlockRefTarget;
    use std::collections::HashMap;
    use std::sync::Arc;

    fn book() -> WikiResolver {
        let mut m = HashMap::new();
        m.insert("foo".into(), "foo".into());
        WikiResolver(Arc::new(m))
    }

    fn empty_blocks() -> BlockRefResolver {
        BlockRefResolver(Arc::new(HashMap::new()))
    }

    fn one_block(id: Uuid, page_slug: &str, snippet: &str) -> BlockRefResolver {
        let mut m = HashMap::new();
        m.insert(
            id,
            BlockRefTarget {
                page_slug: page_slug.into(),
                snippet: snippet.into(),
                content: snippet.into(),
            },
        );
        BlockRefResolver(Arc::new(m))
    }

    #[test]
    fn wikilink_resolves() {
        let n = parse(
            "see [[Foo]]",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        assert!(matches!(
            n.last().unwrap(),
            Node::Wikilink { broken: false, .. }
        ));
    }

    #[test]
    fn wikilink_broken() {
        let n = parse(
            "see [[Bar]]",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        assert!(matches!(
            n.last().unwrap(),
            Node::Wikilink { broken: true, .. }
        ));
    }

    #[test]
    fn external_link() {
        let n = parse(
            "[GitHub](https://github.com)",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        assert!(matches!(
            n.first().unwrap(),
            Node::ExternalLink { url, .. } if url == "https://github.com"
        ));
    }

    #[test]
    fn nested_emphasis() {
        let n = parse(
            "**bold *and italic* end**",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let bold = match n.first().unwrap() {
            Node::Bold(c) => c,
            _ => panic!(),
        };
        assert!(bold.iter().any(|x| matches!(x, Node::Italic(_))));
    }

    #[test]
    fn block_ref_resolves() {
        let id = Uuid::new_v4();
        let resolver = one_block(id, "intro", "intro snippet");
        let n = parse(
            &format!("see (({id})) end"),
            &book(),
            &resolver,
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let r = n.iter().find_map(|x| match x {
            Node::BlockRef {
                target_id,
                page_slug,
                snippet,
                broken,
            } => Some((*target_id, page_slug.clone(), snippet.clone(), *broken)),
            _ => None,
        });
        assert_eq!(
            r,
            Some((id, "intro".to_string(), "intro snippet".to_string(), false))
        );
    }

    #[test]
    fn block_ref_broken_uuid_present_but_unknown() {
        let id = Uuid::new_v4();
        let n = parse(
            &format!("see (({id})) end"),
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let r = n.iter().find_map(|x| match x {
            Node::BlockRef { broken, .. } => Some(*broken),
            _ => None,
        });
        assert_eq!(r, Some(true));
    }

    #[test]
    fn page_embed_resolves_and_carries_contents() {
        let mut embeds = HashMap::new();
        embeds.insert(
            "foo".to_string(),
            vec!["line one".to_string(), "line two".to_string()],
        );
        let er = PageEmbedResolver(Arc::new(embeds));
        let n = parse(
            "![[Foo]] outside",
            &book(),
            &empty_blocks(),
            &er,
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let pe = n.iter().find_map(|x| match x {
            Node::PageEmbed {
                slug,
                contents,
                broken,
                ..
            } => Some((slug.clone(), contents.clone(), *broken)),
            _ => None,
        });
        assert_eq!(
            pe,
            Some((
                "foo".to_string(),
                vec!["line one".to_string(), "line two".to_string()],
                false
            ))
        );
    }

    #[test]
    fn query_tag_resolves_to_pages() {
        let mut q = HashMap::new();
        q.insert(
            "todo".to_string(),
            vec![
                QueryHit {
                    slug: "a".into(),
                    title: "A".into(),
                },
                QueryHit {
                    slug: "b".into(),
                    title: "B".into(),
                },
            ],
        );
        let qr = QueryResolver(Arc::new(q));
        let n = parse(
            "{{query #todo}}",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &qr,
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let r = n.iter().find_map(|x| match x {
            Node::Query {
                results, broken, ..
            } => Some((results.len(), *broken)),
            _ => None,
        });
        assert_eq!(r, Some((2, false)));
    }

    #[test]
    fn query_unknown_form_is_broken() {
        let n = parse(
            "{{query (xor #x #y)}}",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let broken = n
            .iter()
            .any(|x| matches!(x, Node::Query { broken: true, .. }));
        assert!(broken, "xor isn't a supported operator; should be broken");
    }

    #[test]
    fn page_embed_unknown_target_is_broken() {
        let n = parse(
            "see ![[Nope]]",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let broken = n
            .iter()
            .any(|x| matches!(x, Node::PageEmbed { broken: true, .. }));
        assert!(broken, "expected broken PageEmbed; got {n:?}");
    }

    #[test]
    fn block_ref_not_a_uuid_is_left_alone() {
        // `((not-a-uuid))` should not become a BlockRef — it should
        // fall through to text since the body isn't parseable.
        let n = parse(
            "see ((not-a-uuid)) end",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        assert!(!n.iter().any(|x| matches!(x, Node::BlockRef { .. })));
        let txt: String = n
            .iter()
            .filter_map(|x| match x {
                Node::Text(s) => Some(s.clone()),
                _ => None,
            })
            .collect::<Vec<_>>()
            .join("");
        assert!(txt.contains("((not-a-uuid))"));
    }

    #[test]
    fn parses_image() {
        let n = parse(
            "before ![alt text](https://example.com/x.png) after",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let img = n.iter().find(|x| matches!(x, Node::Image { .. }));
        assert!(img.is_some());
        match img.unwrap() {
            Node::Image { alt, url } => {
                assert_eq!(alt, "alt text");
                assert_eq!(url, "https://example.com/x.png");
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn hashtag_at_start() {
        let n = parse(
            "#rust is fun",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let h = n.iter().find_map(|x| match x {
            Node::Hashtag(t) => Some(t.clone()),
            _ => None,
        });
        assert_eq!(h, Some("rust".to_string()));
    }

    #[test]
    fn hashtag_in_word_is_text() {
        let n = parse(
            "c#sharp",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        assert!(!n.iter().any(|x| matches!(x, Node::Hashtag(_))));
    }

    #[test]
    fn parse_timestamp_seconds_forms() {
        assert_eq!(parse_timestamp_seconds("42"), Some(42));
        assert_eq!(parse_timestamp_seconds("1:30"), Some(90));
        assert_eq!(parse_timestamp_seconds("1:01:01"), Some(3661));
        assert_eq!(parse_timestamp_seconds(""), None);
        assert_eq!(parse_timestamp_seconds("garbage"), None);
    }

    #[test]
    fn video_timestamp_macro_parses() {
        let n = parse(
            "watch {{video-timestamp 1:30}} now",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let secs = n.iter().find_map(|x| match x {
            Node::VideoTimestamp(s) => Some(*s),
            _ => None,
        });
        assert_eq!(secs, Some(90));
    }

    #[test]
    fn template_macro_resolves() {
        let mut t = std::collections::HashMap::new();
        t.insert(
            "daily".to_string(),
            vec!["- Reflection".to_string(), "- Wins".to_string()],
        );
        let tr = TemplateResolver(std::sync::Arc::new(t));
        let n = parse(
            "{{template daily}}",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &tr,
        );
        let tmpl = n
            .iter()
            .find(|x| matches!(x, Node::Template { broken: false, .. }));
        assert!(tmpl.is_some(), "expected resolved Template; got {n:?}");
    }

    #[test]
    fn template_macro_broken() {
        let n = parse(
            "{{template missing}}",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        assert!(
            n.iter()
                .any(|x| matches!(x, Node::Template { broken: true, .. }))
        );
    }

    #[test]
    fn inline_math_round_trip() {
        let n = parse(
            "result is $a + b$ today",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let m = n.iter().find_map(|x| match x {
            Node::MathInline(s) => Some(s.clone()),
            _ => None,
        });
        assert_eq!(m, Some("a + b".to_string()));
    }

    #[test]
    fn block_math_round_trip() {
        let n = parse(
            "$$\\int_0^1 x dx$$",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let m = n.iter().find_map(|x| match x {
            Node::MathBlock(s) => Some(s.clone()),
            _ => None,
        });
        assert_eq!(m, Some("\\int_0^1 x dx".to_string()));
    }

    #[test]
    fn lone_dollar_is_left_as_text() {
        let n = parse(
            "costs $5 today",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        assert!(!n.iter().any(|x| matches!(x, Node::MathInline(_))));
    }

    #[test]
    fn image_wikilink_emits_image_node() {
        let n = parse(
            "see ![[diagram.png]]",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let img = n.iter().find(|x| matches!(x, Node::Image { .. }));
        assert!(img.is_some(), "expected Image; got {n:?}");
        match img.unwrap() {
            Node::Image { url, .. } => assert_eq!(url, "assets/diagram.png"),
            _ => unreachable!(),
        }
    }

    #[test]
    fn image_does_not_swallow_page_embed() {
        // `![[Page]]` is the page-embed form, not an image.
        let n = parse(
            "see ![[Foo]]",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        assert!(n.iter().any(|x| matches!(x, Node::PageEmbed { .. })));
        assert!(!n.iter().any(|x| matches!(x, Node::Image { .. })));
    }

    #[test]
    fn parses_footnote_ref() {
        let n = parse(
            "see[^1] for details",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let fr = n.iter().find_map(|x| match x {
            Node::FootnoteRef(id) => Some(id.clone()),
            _ => None,
        });
        assert_eq!(fr, Some("1".to_string()));
    }

    #[test]
    fn footnote_definition_not_treated_as_inline_ref() {
        // `[^id]:` at start of content is a definition line; the
        // inline parser shouldn't emit a FootnoteRef for it.
        let n = parse(
            "[^id]: this is a footnote definition",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        assert!(!n.iter().any(|x| matches!(x, Node::FootnoteRef(_))));
    }

    #[test]
    fn embed_macro_page_form() {
        let mut embeds = std::collections::HashMap::new();
        embeds.insert(
            "foo".to_string(),
            vec!["line one".to_string(), "line two".to_string()],
        );
        let er = PageEmbedResolver(std::sync::Arc::new(embeds));
        let n = parse(
            "{{embed [[Foo]]}}",
            &book(),
            &empty_blocks(),
            &er,
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let pe = n.iter().find(|x| matches!(x, Node::PageEmbed { .. }));
        assert!(pe.is_some(), "expected PageEmbed, got {n:?}");
    }

    #[test]
    fn query_dsl_and_intersection() {
        let mut q = HashMap::new();
        q.insert("a".to_string(), vec![hit("p1"), hit("p2"), hit("p3")]);
        q.insert("b".to_string(), vec![hit("p2"), hit("p3"), hit("p4")]);
        let qr = QueryResolver(Arc::new(q));
        let (results, broken) = eval_query("(and #a #b)", &qr, &PagePropertyResolver::default());
        assert!(!broken);
        let slugs: Vec<&str> = results.iter().map(|h| h.slug.as_str()).collect();
        assert_eq!(slugs, vec!["p2", "p3"]);
    }

    #[test]
    fn query_dsl_or_union() {
        let mut q = HashMap::new();
        q.insert("a".to_string(), vec![hit("p1"), hit("p2")]);
        q.insert("b".to_string(), vec![hit("p2"), hit("p3")]);
        let qr = QueryResolver(Arc::new(q));
        let (results, broken) = eval_query("(or #a #b)", &qr, &PagePropertyResolver::default());
        assert!(!broken);
        let slugs: Vec<&str> = results.iter().map(|h| h.slug.as_str()).collect();
        assert_eq!(slugs, vec!["p1", "p2", "p3"]);
    }

    #[test]
    fn query_dsl_not_complement() {
        let mut q = HashMap::new();
        q.insert("a".to_string(), vec![hit("p1"), hit("p2")]);
        q.insert("b".to_string(), vec![hit("p2"), hit("p3")]);
        let qr = QueryResolver(Arc::new(q));
        // Universe = {p1, p2, p3}; not #a → {p3}
        let (results, broken) = eval_query("(not #a)", &qr, &PagePropertyResolver::default());
        assert!(!broken);
        let slugs: Vec<&str> = results.iter().map(|h| h.slug.as_str()).collect();
        assert_eq!(slugs, vec!["p3"]);
    }

    #[test]
    fn query_dsl_nested_and_or() {
        let mut q = HashMap::new();
        q.insert("a".to_string(), vec![hit("p1"), hit("p2")]);
        q.insert("b".to_string(), vec![hit("p3"), hit("p4")]);
        q.insert("c".to_string(), vec![hit("p2"), hit("p3")]);
        let qr = QueryResolver(Arc::new(q));
        // (and (or #a #b) #c) → pages tagged c AND (a OR b)
        // c={p2,p3}; (a OR b)={p1,p2,p3,p4}; intersection={p2,p3}
        let (results, broken) =
            eval_query("(and (or #a #b) #c)", &qr, &PagePropertyResolver::default());
        assert!(!broken);
        let slugs: Vec<&str> = results.iter().map(|h| h.slug.as_str()).collect();
        assert_eq!(slugs, vec!["p2", "p3"]);
    }

    #[test]
    fn query_dsl_property_filter() {
        let qr = QueryResolver(Arc::new(HashMap::new()));
        let mut by_key: HashMap<String, HashMap<String, Vec<QueryHit>>> = HashMap::new();
        let mut by_val: HashMap<String, Vec<QueryHit>> = HashMap::new();
        by_val.insert("active".to_string(), vec![hit("p1"), hit("p2")]);
        by_val.insert("done".to_string(), vec![hit("p3")]);
        by_key.insert("status".to_string(), by_val);
        let pr = PagePropertyResolver(Arc::new(by_key));
        let (results, broken) = eval_query("(property status active)", &qr, &pr);
        assert!(!broken);
        let slugs: Vec<&str> = results.iter().map(|h| h.slug.as_str()).collect();
        assert_eq!(slugs, vec!["p1", "p2"]);
    }

    #[test]
    fn query_dsl_malformed_marks_broken() {
        let qr = QueryResolver(Arc::new(HashMap::new()));
        let (_, broken) = eval_query("(unknown #x)", &qr, &PagePropertyResolver::default());
        assert!(broken);
        let (_, broken) = eval_query("(and #x", &qr, &PagePropertyResolver::default());
        assert!(broken);
        let (_, broken) = eval_query("just text", &qr, &PagePropertyResolver::default());
        assert!(broken);
    }

    fn hit(slug: &str) -> QueryHit {
        QueryHit {
            slug: slug.into(),
            title: slug.into(),
        }
    }

    #[test]
    fn media_macro_video() {
        let n = parse(
            "{{video https://example.com/clip.mp4}}",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let media = n.iter().find_map(|x| match x {
            Node::Media { kind, target } => Some((*kind, target.clone())),
            _ => None,
        });
        assert_eq!(
            media,
            Some((MediaKind::Video, "https://example.com/clip.mp4".into()))
        );
    }

    #[test]
    fn media_macro_youtube_and_tweet() {
        let n = parse(
            "{{youtube dQw4w9WgXcQ}} {{tweet 1234567890}}",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let kinds: Vec<MediaKind> = n
            .iter()
            .filter_map(|x| match x {
                Node::Media { kind, .. } => Some(*kind),
                _ => None,
            })
            .collect();
        assert_eq!(kinds, vec![MediaKind::Youtube, MediaKind::Tweet]);
    }

    #[test]
    fn extract_youtube_id_from_watch_url() {
        assert_eq!(
            crate::components::extract_youtube_id("https://www.youtube.com/watch?v=dQw4w9WgXcQ"),
            "dQw4w9WgXcQ"
        );
    }

    #[test]
    fn extract_youtube_id_from_short_url() {
        assert_eq!(
            crate::components::extract_youtube_id("https://youtu.be/dQw4w9WgXcQ"),
            "dQw4w9WgXcQ"
        );
    }

    #[test]
    fn extract_youtube_id_passthrough_bare_id() {
        assert_eq!(
            crate::components::extract_youtube_id("dQw4w9WgXcQ"),
            "dQw4w9WgXcQ"
        );
    }

    #[test]
    fn embed_macro_block_form() {
        let id = Uuid::new_v4();
        let resolver = one_block(id, "p", "snippet");
        let n = parse(
            &format!("{{{{embed (({id}))}}}}"),
            &book(),
            &resolver,
            &PageEmbedResolver::default(),
            &QueryResolver::default(),
            &NamespaceResolver::default(),
            &PagePropertyResolver::default(),
            &TemplateResolver::default(),
        );
        let br = n.iter().find(|x| matches!(x, Node::BlockRef { .. }));
        assert!(
            br.is_some(),
            "expected BlockRef from embed macro, got {n:?}"
        );
    }
}
