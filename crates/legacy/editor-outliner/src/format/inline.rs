//! Inline markdown parser + renderer. Mirrors the Logseq
//! `frontend.components.block/inline` function's role: take raw
//! block content, walk it to an AST, hand it to a Dioxus
//! component that emits the right tags.
//!
//! Supported tokens (the same subset Logseq treats as "inline"
//! during render):
//!
//! - `**bold**`, `*italic*`, `_italic_`, `~~strike~~`,
//!   `==highlight==`, `` `code` ``
//! - `[[Page name]]` and `[[Page|alias]]` wikilinks
//! - `((uuid))` block references
//! - `#tag` and `#[[Multi word tag]]`
//! - `[label](url)` external links
//! - `![alt](url)` images and `![[image.png]]` image-wikilinks
//! - `$expr$` inline math (rendered as styled text — a KaTeX
//!   loader can upgrade it later)
//!
//! Anything that doesn't match is emitted as plain text. The
//! parser is forgiving — an unclosed `**` is just a literal
//! pair of asterisks.

use dioxus::prelude::*;
use uuid::Uuid;

use crate::state::AppState;

/// One inline node in the parsed tree. Mirrors the variants
/// Logseq's `inline` dispatch handles.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Inline {
    Text(String),
    Bold(Vec<Inline>),
    Italic(Vec<Inline>),
    Strike(Vec<Inline>),
    Highlight(Vec<Inline>),
    Code(String),
    Wikilink {
        target: String,
        alias: Option<String>,
    },
    BlockRef(Uuid),
    Tag(String),
    ExternalLink {
        label: String,
        url: String,
    },
    Image {
        alt: String,
        url: String,
    },
    Math(String),
}

/// Parse a single line of block content. Multi-line blocks are
/// parsed line-by-line by the caller — newlines become `<br>` at
/// the renderer.
pub fn parse(input: &str) -> Vec<Inline> {
    let mut out = Vec::new();
    let mut buf = String::new();
    let bytes = input.as_bytes();
    let mut i = 0usize;
    let flush = |buf: &mut String, out: &mut Vec<Inline>| {
        if !buf.is_empty() {
            out.push(Inline::Text(std::mem::take(buf)));
        }
    };
    while i < input.len() {
        // **bold**
        if input[i..].starts_with("**") {
            if let Some(end) = input[i + 2..].find("**") {
                let body = &input[i + 2..i + 2 + end];
                if !body.is_empty() && !body.contains('\n') {
                    flush(&mut buf, &mut out);
                    out.push(Inline::Bold(parse(body)));
                    i += 2 + end + 2;
                    continue;
                }
            }
        }
        // ~~strike~~
        if input[i..].starts_with("~~") {
            if let Some(end) = input[i + 2..].find("~~") {
                let body = &input[i + 2..i + 2 + end];
                if !body.is_empty() && !body.contains('\n') {
                    flush(&mut buf, &mut out);
                    out.push(Inline::Strike(parse(body)));
                    i += 2 + end + 2;
                    continue;
                }
            }
        }
        // ==highlight==
        if input[i..].starts_with("==") {
            if let Some(end) = input[i + 2..].find("==") {
                let body = &input[i + 2..i + 2 + end];
                if !body.is_empty() && !body.contains('\n') {
                    flush(&mut buf, &mut out);
                    out.push(Inline::Highlight(parse(body)));
                    i += 2 + end + 2;
                    continue;
                }
            }
        }
        // [[Page]] or [[Page|alias]]
        if input[i..].starts_with("[[") {
            if let Some(end) = input[i + 2..].find("]]") {
                let body = &input[i + 2..i + 2 + end];
                let (target, alias) = match body.split_once('|') {
                    Some((t, a)) => (t.trim().to_string(), Some(a.trim().to_string())),
                    None => (body.trim().to_string(), None),
                };
                if is_image_target(&target) {
                    flush(&mut buf, &mut out);
                    out.push(Inline::Image {
                        alt: alias.unwrap_or_else(|| target.clone()),
                        url: format!("assets/{target}"),
                    });
                } else {
                    flush(&mut buf, &mut out);
                    out.push(Inline::Wikilink { target, alias });
                }
                i += 2 + end + 2;
                continue;
            }
        }
        // ((uuid))
        if input[i..].starts_with("((") {
            if let Some(end) = input[i + 2..].find("))") {
                let body = input[i + 2..i + 2 + end].trim();
                if let Ok(id) = Uuid::parse_str(body) {
                    flush(&mut buf, &mut out);
                    out.push(Inline::BlockRef(id));
                    i += 2 + end + 2;
                    continue;
                }
            }
        }
        // ![[diagram.png]] — image-wikilink. Check this BEFORE
        // `![alt](url)` so the bang prefix doesn't get peeled off
        // as a literal `!` followed by a wikilink.
        if input[i..].starts_with("![[") {
            if let Some(end) = input[i + 3..].find("]]") {
                let body = &input[i + 3..i + 3 + end];
                let (target, alias) = match body.split_once('|') {
                    Some((t, a)) => (t.trim().to_string(), Some(a.trim().to_string())),
                    None => (body.trim().to_string(), None),
                };
                flush(&mut buf, &mut out);
                if is_image_target(&target) {
                    out.push(Inline::Image {
                        alt: alias.unwrap_or_else(|| target.clone()),
                        url: format!("assets/{target}"),
                    });
                } else {
                    // Page-embed form — render as a styled link to
                    // the page; full embed UI follows later.
                    out.push(Inline::Wikilink { target, alias });
                }
                i += 3 + end + 2;
                continue;
            }
        }
        // ![alt](url) image
        if input[i..].starts_with("![") {
            if let Some(close_alt) = input[i + 2..].find(']') {
                let after_alt = i + 2 + close_alt + 1;
                if after_alt < input.len() && bytes[after_alt] == b'(' {
                    if let Some(close_url) = input[after_alt + 1..].find(')') {
                        let alt = input[i + 2..i + 2 + close_alt].to_string();
                        let url = input[after_alt + 1..after_alt + 1 + close_url].to_string();
                        flush(&mut buf, &mut out);
                        out.push(Inline::Image { alt, url });
                        i = after_alt + 1 + close_url + 1;
                        continue;
                    }
                }
            }
        }
        // [label](url) external link — only after we've ruled out
        // ![alt](url) and [[wikilink]].
        if bytes[i] == b'[' && !input[i..].starts_with("[[") {
            if let Some(close_label) = input[i + 1..].find(']') {
                let after_label = i + 1 + close_label + 1;
                if after_label < input.len() && bytes[after_label] == b'(' {
                    if let Some(close_url) = input[after_label + 1..].find(')') {
                        let label = input[i + 1..i + 1 + close_label].to_string();
                        let url = input[after_label + 1..after_label + 1 + close_url].to_string();
                        flush(&mut buf, &mut out);
                        out.push(Inline::ExternalLink { label, url });
                        i = after_label + 1 + close_url + 1;
                        continue;
                    }
                }
            }
        }
        // `code` — backtick-delimited inline code.
        if bytes[i] == b'`' {
            if let Some(end) = input[i + 1..].find('`') {
                let body = &input[i + 1..i + 1 + end];
                if !body.is_empty() {
                    flush(&mut buf, &mut out);
                    out.push(Inline::Code(body.to_string()));
                    i += 1 + end + 1;
                    continue;
                }
            }
        }
        // *italic* — single-star. Must not be the `**` we already
        // checked for; require the next byte to not be `*`.
        if bytes[i] == b'*' && i + 1 < input.len() && bytes[i + 1] != b'*' {
            if let Some(end) = input[i + 1..].find('*') {
                let body = &input[i + 1..i + 1 + end];
                if !body.is_empty() && !body.contains('\n') && !body.contains('*') {
                    flush(&mut buf, &mut out);
                    out.push(Inline::Italic(parse(body)));
                    i += 1 + end + 1;
                    continue;
                }
            }
        }
        // _italic_ — underscore form. Same rules as `*…*`.
        if bytes[i] == b'_' && (i == 0 || !is_word_byte(bytes[i - 1])) {
            if let Some(end) = input[i + 1..].find('_') {
                let body = &input[i + 1..i + 1 + end];
                if !body.is_empty() && !body.contains('\n') && !body.contains('_') {
                    flush(&mut buf, &mut out);
                    out.push(Inline::Italic(parse(body)));
                    i += 1 + end + 1;
                    continue;
                }
            }
        }
        // $expr$ inline math. Same shape as italic but with `$`.
        if bytes[i] == b'$'
            && i + 1 < input.len()
            && bytes[i + 1] != b'$'
            && (i == 0 || (bytes[i - 1] as char).is_whitespace())
        {
            if let Some(end) = input[i + 1..].find('$') {
                let body = &input[i + 1..i + 1 + end];
                if !body.is_empty() && !body.starts_with(' ') && !body.ends_with(' ') {
                    flush(&mut buf, &mut out);
                    out.push(Inline::Math(body.to_string()));
                    i += 1 + end + 1;
                    continue;
                }
            }
        }
        // #tag — word-boundary `#`. `#[[Multi word]]` form is
        // also accepted.
        if bytes[i] == b'#' && (i == 0 || (bytes[i - 1] as char).is_whitespace()) {
            if input[i + 1..].starts_with("[[") {
                if let Some(end) = input[i + 3..].find("]]") {
                    let tag = input[i + 3..i + 3 + end].to_string();
                    flush(&mut buf, &mut out);
                    out.push(Inline::Tag(tag));
                    i += 3 + end + 2;
                    continue;
                }
            }
            let mut j = i + 1;
            while j < input.len() {
                let c = bytes[j] as char;
                if c.is_alphanumeric() || c == '-' || c == '_' || c == '/' {
                    j += 1;
                } else {
                    break;
                }
            }
            if j > i + 1 {
                let tag = input[i + 1..j].to_string();
                flush(&mut buf, &mut out);
                out.push(Inline::Tag(tag));
                i = j;
                continue;
            }
        }
        let c = input[i..].chars().next().expect("non-empty");
        buf.push(c);
        i += c.len_utf8();
    }
    flush(&mut buf, &mut out);
    out
}

fn is_word_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn is_image_target(target: &str) -> bool {
    let lower = target.to_lowercase();
    [
        ".png", ".jpg", ".jpeg", ".gif", ".webp", ".svg", ".avif", ".bmp",
    ]
    .iter()
    .any(|ext| lower.ends_with(ext))
}

/// Render a parsed inline tree as Dioxus elements. Mirrors
/// Logseq's `(inline config item)` dispatch — one match arm per
/// variant.
#[component]
pub fn Inlines(nodes: Vec<Inline>) -> Element {
    rsx! {
        for (i, n) in nodes.into_iter().enumerate() {
            InlineNode { key: "{i}", node: n }
        }
    }
}

#[component]
pub fn InlineNode(node: Inline) -> Element {
    match node {
        Inline::Text(s) => render_text_with_breaks(&s),
        Inline::Bold(children) => rsx! { strong { Inlines { nodes: children } } },
        Inline::Italic(children) => rsx! { em { Inlines { nodes: children } } },
        Inline::Strike(children) => rsx! { s { Inlines { nodes: children } } },
        Inline::Highlight(children) => rsx! { mark { Inlines { nodes: children } } },
        Inline::Code(s) => rsx! { code { class: "inline-code", "{s}" } },
        Inline::Wikilink { target, alias } => {
            let label = alias.clone().unwrap_or_else(|| target.clone());
            let target_for_click = target.clone();
            let on_click = move |_: Event<MouseData>| {
                if let Some(state) = try_consume_context::<AppState>() {
                    let v = state.vault.read();
                    if let Some(p) = v.page_by_basename(&target_for_click) {
                        let pid = p.id;
                        drop(v);
                        state.active_page.clone().set(Some(pid));
                    }
                }
            };
            rsx! {
                span { class: "wikilink",
                    span { class: "wikilink-bracket", "[[" }
                    a { class: "wikilink-label",
                        href: "#",
                        onclick: move |e: Event<MouseData>| {
                            e.prevent_default();
                            on_click(e);
                        },
                        "{label}"
                    }
                    span { class: "wikilink-bracket", "]]" }
                }
            }
        }
        Inline::BlockRef(id) => rsx! {
            span { class: "blockref", "(({id}))" }
        },
        Inline::Tag(name) => rsx! {
            span { class: "tag", "#{name}" }
        },
        Inline::ExternalLink { label, url } => rsx! {
            a { class: "ext-link", href: "{url}", target: "_blank", rel: "noopener", "{label}" }
        },
        Inline::Image { alt, url } => rsx! {
            img { class: "inline-image", src: "{url}", alt: "{alt}", loading: "lazy" }
        },
        Inline::Math(src) => rsx! {
            span { class: "math math-inline", "${src}$" }
        },
    }
}

/// Plain text — but preserve embedded `\n` as `<br>` so blocks
/// with explicit newlines render with line breaks instead of one
/// long wrapped line.
fn render_text_with_breaks(s: &str) -> Element {
    if !s.contains('\n') {
        return rsx! { "{s}" };
    }
    let parts: Vec<&str> = s.split('\n').collect();
    rsx! {
        for (i, p) in parts.iter().enumerate() {
            { if i > 0 { rsx! { br {} } } else { rsx! {} } }
            { rsx! { "{p}" } }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain_text() {
        assert_eq!(parse("hello"), vec![Inline::Text("hello".into())]);
    }

    #[test]
    fn bold_inline() {
        let n = parse("a **b** c");
        assert!(matches!(n[0], Inline::Text(_)));
        assert!(matches!(n[1], Inline::Bold(_)));
    }

    #[test]
    fn bold_then_italic_neighbors() {
        let n = parse("**bold** and *italic*");
        assert!(n.iter().any(|x| matches!(x, Inline::Bold(_))));
        assert!(n.iter().any(|x| matches!(x, Inline::Italic(_))));
    }

    #[test]
    fn wikilink_with_alias() {
        let n = parse("[[Page|Alias]]");
        assert_eq!(
            n[0],
            Inline::Wikilink {
                target: "Page".into(),
                alias: Some("Alias".into()),
            }
        );
    }

    #[test]
    fn image_wikilink() {
        let n = parse("![[diagram.png]]");
        match &n[0] {
            Inline::Image { url, .. } => assert_eq!(url, "assets/diagram.png"),
            other => panic!("expected Image, got {other:?}"),
        }
    }

    #[test]
    fn hashtag_word_boundary() {
        let n = parse("a #tag b");
        assert!(n.iter().any(|x| matches!(x, Inline::Tag(t) if t == "tag")));
        let n = parse("c#sharp");
        assert!(!n.iter().any(|x| matches!(x, Inline::Tag(_))));
    }

    #[test]
    fn multi_word_tag() {
        let n = parse("#[[Big Idea]]");
        assert_eq!(n[0], Inline::Tag("Big Idea".into()));
    }

    #[test]
    fn external_link() {
        let n = parse("[Rust](https://rust-lang.org)");
        assert_eq!(
            n[0],
            Inline::ExternalLink {
                label: "Rust".into(),
                url: "https://rust-lang.org".into(),
            }
        );
    }

    #[test]
    fn code_span() {
        let n = parse("hit `Esc` to exit");
        assert!(n.iter().any(|x| matches!(x, Inline::Code(s) if s == "Esc")));
    }

    #[test]
    fn block_ref() {
        let n = parse("see ((11111111-1111-1111-1111-111111111111))");
        assert!(n.iter().any(|x| matches!(x, Inline::BlockRef(_))));
    }

    #[test]
    fn unclosed_marker_is_text() {
        let n = parse("**unclosed");
        assert_eq!(n[0], Inline::Text("**unclosed".into()));
    }
}
