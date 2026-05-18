//! Inline-markup parser for the publisher.
//!
//! Mirrors a subset of `knowledge_ui::inline_md` but emits a
//! `Node` enum that's safe to render via Dioxus components.
//! Wikilinks are *resolved at parse time* against a `WikiResolver`
//! lookup so the renderer doesn't need context — the AST carries
//! the resolved URL directly.

use crate::components::WikiResolver;

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
}

pub fn parse(s: &str, resolver: &WikiResolver) -> Vec<Node> {
    let mut out = Vec::new();
    let mut buf = String::new();
    let bytes = s.as_bytes();
    let mut i = 0usize;
    while i < s.len() {
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
                let inner = parse(&s[i + 2..i + 2 + end], resolver);
                flush(&mut buf, &mut out);
                out.push(Node::Bold(inner));
                i += 2 + end + 2;
                continue;
            }
        }
        // ~~strike~~
        if s[i..].starts_with("~~") {
            if let Some(end) = s[i + 2..].find("~~") {
                let inner = parse(&s[i + 2..i + 2 + end], resolver);
                flush(&mut buf, &mut out);
                out.push(Node::Strikethrough(inner));
                i += 2 + end + 2;
                continue;
            }
        }
        // ==highlight==
        if s[i..].starts_with("==") {
            if let Some(end) = s[i + 2..].find("==") {
                let inner = parse(&s[i + 2..i + 2 + end], resolver);
                flush(&mut buf, &mut out);
                out.push(Node::Highlight(inner));
                i += 2 + end + 2;
                continue;
            }
        }
        // *italic* (not part of `**`)
        if bytes[i] == b'*' && bytes.get(i + 1).copied() != Some(b'*') {
            if let Some(end) = s[i + 1..].find('*') {
                let inner = parse(&s[i + 1..i + 1 + end], resolver);
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use std::sync::Arc;

    fn book() -> WikiResolver {
        let mut m = HashMap::new();
        m.insert("foo".into(), "foo".into());
        WikiResolver(Arc::new(m))
    }

    #[test]
    fn wikilink_resolves() {
        let n = parse("see [[Foo]]", &book());
        assert!(matches!(
            n.last().unwrap(),
            Node::Wikilink { broken: false, .. }
        ));
    }

    #[test]
    fn wikilink_broken() {
        let n = parse("see [[Bar]]", &book());
        assert!(matches!(
            n.last().unwrap(),
            Node::Wikilink { broken: true, .. }
        ));
    }

    #[test]
    fn external_link() {
        let n = parse("[GitHub](https://github.com)", &book());
        assert!(matches!(
            n.first().unwrap(),
            Node::ExternalLink { url, .. } if url == "https://github.com"
        ));
    }

    #[test]
    fn nested_emphasis() {
        let n = parse("**bold *and italic* end**", &book());
        let bold = match n.first().unwrap() {
            Node::Bold(c) => c,
            _ => panic!(),
        };
        assert!(bold.iter().any(|x| matches!(x, Node::Italic(_))));
    }
}
