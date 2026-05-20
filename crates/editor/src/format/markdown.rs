//! Logseq-flavored markdown reader + writer.
//!
//! Read side: tokenize bullet lines, fold by indentation, lift
//! `id::` + `key:: value` continuation lines into block properties.
//! Write side: emit bullets with 2-space indent per level + the
//! `id::` line that lets us round-trip stable identities.

use chrono::Utc;
use indexmap::IndexMap;
use uuid::Uuid;

use crate::db::{Block, Page};

/// Parse one page from raw markdown. Returns the page header
/// (id + frontmatter) plus every block in document order.
pub fn parse_page(
    raw: &str,
    basename: &str,
    relpath: &str,
    is_journal: bool,
) -> (Page, Vec<Block>) {
    let now = Utc::now();
    let page_id = Uuid::new_v4();
    let (frontmatter_json, aliases, body) = peel_frontmatter(raw);

    let journal_day = if is_journal {
        Some(normalize_journal_day(basename))
    } else {
        None
    };

    let display_basename = if is_journal {
        journal_day.clone().unwrap_or_else(|| basename.to_string())
    } else {
        basename.replace('_', "/")
    };

    let page = Page {
        id: page_id,
        basename: display_basename,
        relpath: relpath.to_string(),
        aliases,
        frontmatter_json,
        is_journal,
        journal_day,
        created_at: now,
        updated_at: now,
    };

    let blocks = parse_blocks(&body, page_id, now);
    (page, blocks)
}

/// Strip an optional `---\n…\n---` YAML-ish frontmatter header.
/// We only extract `aliases:` (list or scalar) for now; everything
/// else gets stashed in a JSON blob so it round-trips on save.
fn peel_frontmatter(raw: &str) -> (String, Vec<String>, String) {
    if !raw.trim_start().starts_with("---") {
        return ("{}".into(), Vec::new(), raw.to_string());
    }
    let body = raw.trim_start();
    let after = &body[3..];
    let Some(end) = after.find("\n---") else {
        return ("{}".into(), Vec::new(), raw.to_string());
    };
    let block = &after[..end];
    let rest = &after[end + 4..];
    let mut map: IndexMap<String, serde_json::Value> = IndexMap::new();
    let mut aliases: Vec<String> = Vec::new();
    for line in block.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let Some((k, v)) = line.split_once(':') else {
            continue;
        };
        let k = k.trim();
        let v = v.trim();
        if k.eq_ignore_ascii_case("aliases") {
            aliases.extend(
                v.trim_matches(|c| c == '[' || c == ']')
                    .split(',')
                    .filter_map(|s| {
                        let t = s.trim().trim_matches('"').trim_matches('\'');
                        (!t.is_empty()).then(|| t.to_string())
                    }),
            );
            map.insert(k.to_string(), serde_json::Value::String(v.to_string()));
        } else {
            map.insert(k.to_string(), serde_json::Value::String(v.to_string()));
        }
    }
    let fm = serde_json::to_string(&map).unwrap_or_else(|_| "{}".into());
    (fm, aliases, rest.trim_start_matches('\n').to_string())
}

/// Parse bullet-list blocks. Indentation = parent chain. `id::`
/// lines on a block restore the stable uuid (so external edits
/// don't churn ids). `key:: value` lines become block properties.
fn parse_blocks(body: &str, page_id: Uuid, now: chrono::DateTime<chrono::Utc>) -> Vec<Block> {
    // Each "raw" entry is a (indent_level, list of content lines,
    // property lines, optional explicit id) tuple. We fold them
    // by indent into a tree-by-stack.
    #[derive(Default)]
    struct RawBlock {
        indent: usize,
        content_lines: Vec<String>,
        props: IndexMap<String, serde_json::Value>,
        id: Option<Uuid>,
    }
    let mut raws: Vec<RawBlock> = Vec::new();
    for line in body.lines() {
        // A bullet starts a new block.
        let trimmed_start = line.trim_start();
        if let Some(rest) = trimmed_start.strip_prefix("- ") {
            let indent = line.len() - trimmed_start.len();
            raws.push(RawBlock {
                indent: indent / 2,
                content_lines: vec![rest.to_string()],
                ..Default::default()
            });
        } else if let Some(last) = raws.last_mut() {
            // Continuation line: either a property or extra text.
            let stripped = trimmed_start;
            if let Some((k, v)) = split_property(stripped) {
                if k == "id" {
                    if let Ok(id) = Uuid::parse_str(&v) {
                        last.id = Some(id);
                        continue;
                    }
                }
                last.props.insert(k, serde_json::Value::String(v));
            } else if !stripped.is_empty() {
                last.content_lines.push(stripped.to_string());
            }
        }
    }

    // Build the tree by walking with an indent stack.
    let mut stack: Vec<(usize, Uuid)> = Vec::new();
    let mut blocks: Vec<Block> = Vec::with_capacity(raws.len());
    for (i, raw) in raws.into_iter().enumerate() {
        // Pop until we find a parent whose indent is less than ours.
        while stack.last().map(|(d, _)| *d >= raw.indent).unwrap_or(false) {
            stack.pop();
        }
        let parent_id = stack.last().map(|(_, id)| *id);
        let id = raw.id.unwrap_or_else(Uuid::new_v4);
        let content = raw.content_lines.join("\n");
        let props_json = serde_json::to_string(&raw.props).unwrap_or_else(|_| "{}".into());
        blocks.push(Block {
            id,
            page_id,
            parent_id,
            sort_key: sort_key_for_index(i),
            content,
            properties_json: props_json,
            collapsed: false,
            created_at: now,
            updated_at: now,
        });
        stack.push((raw.indent, id));
    }
    blocks
}

fn split_property(line: &str) -> Option<(String, String)> {
    // `key:: value` form. Reject anything with whitespace in the
    // key so URLs (`https://…`) aren't misread.
    let idx = line.find("::")?;
    let key = line[..idx].trim();
    if key.is_empty() || key.chars().any(char::is_whitespace) {
        return None;
    }
    let val = line[idx + 2..].trim();
    Some((key.to_string(), val.to_string()))
}

fn normalize_journal_day(s: &str) -> String {
    // Logseq writes `2026_05_19` on disk for the file name. Read
    // it back as `2026-05-19`.
    s.replace('_', "-")
}

/// Produce a sort_key for a freshly-inserted block at position
/// `i`. Two chars per index keeps lexorank-friendly ordering.
fn sort_key_for_index(i: usize) -> String {
    let mut s = String::with_capacity(2);
    let alpha = b"abcdefghijklmnopqrstuvwxyz";
    let n = alpha.len();
    let hi = i / n;
    let lo = i % n;
    s.push(alpha[hi.min(n - 1)] as char);
    s.push(alpha[lo] as char);
    s
}

/// Render a page's blocks back to disk as Logseq markdown.
/// Optional frontmatter, then bullet tree with 2-space indent.
pub fn serialize_page(page: &Page, blocks: &[Block]) -> String {
    let mut out = String::new();

    // Frontmatter — only when the original had a non-empty JSON.
    let fm: serde_json::Value =
        serde_json::from_str(&page.frontmatter_json).unwrap_or(serde_json::Value::Null);
    if let Some(obj) = fm.as_object() {
        if !obj.is_empty() {
            out.push_str("---\n");
            for (k, v) in obj {
                let value = v
                    .as_str()
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| v.to_string());
                out.push_str(&format!("{k}: {value}\n"));
            }
            out.push_str("---\n\n");
        }
    }

    // Sort blocks by parent + sort_key.
    let mut roots: Vec<&Block> = blocks.iter().filter(|b| b.parent_id.is_none()).collect();
    roots.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    for r in roots {
        write_block_tree(&mut out, blocks, r, 0);
    }
    out
}

fn write_block_tree(out: &mut String, all: &[Block], b: &Block, depth: usize) {
    let indent = "  ".repeat(depth);
    // First content line gets the `- ` bullet; additional lines
    // get the same indent but no bullet (Logseq's continuation
    // form).
    let mut content_lines = b.content.split('\n');
    let first = content_lines.next().unwrap_or("");
    out.push_str(&indent);
    out.push_str("- ");
    out.push_str(first);
    out.push('\n');
    for extra in content_lines {
        out.push_str(&indent);
        out.push_str("  ");
        out.push_str(extra);
        out.push('\n');
    }
    // id:: line.
    out.push_str(&indent);
    out.push_str("  id:: ");
    out.push_str(&b.id.to_string());
    out.push('\n');
    // Block properties (skip "id" — already emitted).
    if let Ok(serde_json::Value::Object(props)) =
        serde_json::from_str::<serde_json::Value>(&b.properties_json)
    {
        for (k, v) in props {
            if k == "id" {
                continue;
            }
            let value = v
                .as_str()
                .map(|s| s.to_string())
                .unwrap_or_else(|| v.to_string());
            out.push_str(&indent);
            out.push_str("  ");
            out.push_str(&k);
            out.push_str(":: ");
            out.push_str(&value);
            out.push('\n');
        }
    }
    // Children.
    let mut kids: Vec<&Block> = all.iter().filter(|c| c.parent_id == Some(b.id)).collect();
    kids.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    for k in kids {
        write_block_tree(out, all, k, depth + 1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_simple_page() {
        let raw = "- Hello\n  id:: 11111111-1111-1111-1111-111111111111\n  - Child\n    id:: 22222222-2222-2222-2222-222222222222\n";
        let (page, blocks) = parse_page(raw, "Home", "pages/Home.md", false);
        assert_eq!(page.basename, "Home");
        assert_eq!(blocks.len(), 2);
        assert_eq!(blocks[0].content, "Hello");
        assert_eq!(blocks[1].content, "Child");
        assert_eq!(blocks[1].parent_id, Some(blocks[0].id));
        let out = serialize_page(&page, &blocks);
        assert!(out.contains("- Hello"));
        assert!(out.contains("id:: 11111111"));
        assert!(out.contains("  - Child"));
    }

    #[test]
    fn preserves_block_properties() {
        let raw =
            "- TODO Buy milk\n  id:: 11111111-1111-1111-1111-111111111111\n  priority:: high\n";
        let (_, blocks) = parse_page(raw, "Tasks", "pages/Tasks.md", false);
        assert_eq!(blocks.len(), 1);
        let v: serde_json::Value = serde_json::from_str(&blocks[0].properties_json).unwrap();
        assert_eq!(v["priority"], "high");
    }

    #[test]
    fn empty_input_yields_empty_blocks() {
        let (_, blocks) = parse_page("", "X", "pages/X.md", false);
        assert!(blocks.is_empty());
    }

    #[test]
    fn journal_basename_normalizes() {
        let (page, _) = parse_page("- hi\n", "2026_05_20", "journals/2026_05_20.md", true);
        assert_eq!(page.basename, "2026-05-20");
        assert!(page.is_journal);
    }
}
