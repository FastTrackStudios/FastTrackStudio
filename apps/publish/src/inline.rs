//! Inline-markup parser for the publisher.
//!
//! Mirrors a subset of `knowledge_ui::inline_md` but emits a
//! `Node` enum that's safe to render via Dioxus components.
//! Wikilinks and block refs are *resolved at parse time* against
//! `WikiResolver` / `BlockRefResolver` lookups so the renderer
//! doesn't need context — the AST carries the resolved URL +
//! snippet directly.

use crate::components::{BlockRefResolver, PageEmbedResolver, WikiResolver};
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
}

pub fn parse(
    s: &str,
    resolver: &WikiResolver,
    blocks: &BlockRefResolver,
    page_embeds: &PageEmbedResolver,
) -> Vec<Node> {
    let mut out = Vec::new();
    let mut buf = String::new();
    let bytes = s.as_bytes();
    let mut i = 0usize;
    while i < s.len() {
        // ![[Page]] — page embed. Match BEFORE wikilink so the
        // bang prefix wins; mirrors Obsidian's `![[...]]` semantics.
        if s[i..].starts_with("![[") {
            if let Some(end) = s[i + 3..].find("]]") {
                let body = &s[i + 3..i + 3 + end];
                let (target, alias) = match body.split_once('|') {
                    Some((t, a)) => (t.trim(), a.trim()),
                    None => (body.trim(), body.trim()),
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
                let inner = parse(&s[i + 2..i + 2 + end], resolver, blocks, page_embeds);
                flush(&mut buf, &mut out);
                out.push(Node::Bold(inner));
                i += 2 + end + 2;
                continue;
            }
        }
        // ~~strike~~
        if s[i..].starts_with("~~") {
            if let Some(end) = s[i + 2..].find("~~") {
                let inner = parse(&s[i + 2..i + 2 + end], resolver, blocks, page_embeds);
                flush(&mut buf, &mut out);
                out.push(Node::Strikethrough(inner));
                i += 2 + end + 2;
                continue;
            }
        }
        // ==highlight==
        if s[i..].starts_with("==") {
            if let Some(end) = s[i + 2..].find("==") {
                let inner = parse(&s[i + 2..i + 2 + end], resolver, blocks, page_embeds);
                flush(&mut buf, &mut out);
                out.push(Node::Highlight(inner));
                i += 2 + end + 2;
                continue;
            }
        }
        // *italic* (not part of `**`)
        if bytes[i] == b'*' && bytes.get(i + 1).copied() != Some(b'*') {
            if let Some(end) = s[i + 1..].find('*') {
                let inner = parse(&s[i + 1..i + 1 + end], resolver, blocks, page_embeds);
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

/// 8-char prefix of a UUID — used as the visible label for a
/// broken block reference where we have no snippet to show.
fn short_uuid(id: &Uuid) -> String {
    id.simple().to_string().chars().take(8).collect()
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
        let n = parse("![[Foo]] outside", &book(), &empty_blocks(), &er);
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
    fn page_embed_unknown_target_is_broken() {
        let n = parse(
            "see ![[Nope]]",
            &book(),
            &empty_blocks(),
            &PageEmbedResolver::default(),
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
}
