//! Tiny query DSL for `{{query EXPR}}` blocks.
//!
//! v1 covers the most-used Logseq query forms — tag lookups,
//! page backlinks, and property equality. Each query type maps
//! cleanly onto one of our materialized indices
//! (`BlockRefEdge` / `BlockPropEdge`) so evaluation is O(edges)
//! rather than scanning every block.
//!
//! Out of scope for v1: boolean combinators (`(and …)`,
//! `(or …)`), date filters (`(between :due ...)`),
//! Datalog-style queries.

use knowledge_proto::{BlockPropEdge, BlockRefEdge};
use uuid::Uuid;

/// Parsed `{{query …}}` body.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Query {
    /// `{{query}}` (empty body) → list everything.
    All,
    /// `#tag` or `#nested/tag`.
    Tag(String),
    /// `[[Page Name]]` — backlinks to the page.
    Link(String),
    /// `key:value` — block has this property at this value.
    Property { key: String, value: String },
}

/// Parse the body of a `{{query …}}` construct. Returns `None`
/// if the body doesn't fit any supported form.
pub fn parse_query(body: &str) -> Option<Query> {
    let trimmed = body.trim();
    if trimmed.is_empty() {
        return Some(Query::All);
    }
    if let Some(rest) = trimmed.strip_prefix('#') {
        if rest.is_empty() {
            return None;
        }
        return Some(Query::Tag(rest.to_string()));
    }
    if trimmed.starts_with("[[") && trimmed.ends_with("]]") && trimmed.len() > 4 {
        let inner = &trimmed[2..trimmed.len() - 2];
        return Some(Query::Link(inner.to_string()));
    }
    if let Some((k, v)) = trimmed.split_once(':') {
        let key = k.trim();
        let value = v.trim();
        if key.is_empty() || value.is_empty() {
            return None;
        }
        return Some(Query::Property {
            key: key.to_lowercase(),
            value: value.to_string(),
        });
    }
    None
}

/// Detect a block-level `{{query …}}` whose ENTIRE content is
/// the query (no surrounding prose). Returns the parsed query.
/// Inline queries (queries embedded inside a block) follow in
/// a later pass.
pub fn parse_block_level_query(content: &str) -> Option<Query> {
    let trimmed = content.trim();
    let body = trimmed
        .strip_prefix("{{query ")
        .or_else(|| trimmed.strip_prefix("{{query"))
        .and_then(|s| s.strip_suffix("}}"))?;
    parse_query(body)
}

/// Result hit: a block that matches the query.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QueryHit {
    pub block_id: Uuid,
    pub page_id: Uuid,
}

/// Resolve a query against the materialized indices, returning
/// the matching block ids paired with their containing pages.
/// `block_refs` and `block_props` are the snapshot copies from
/// `KnowledgeSnapshot`.
pub fn evaluate(
    query: &Query,
    block_refs: &[BlockRefEdge],
    block_props: &[BlockPropEdge],
    all_block_ids: impl IntoIterator<Item = (Uuid, Uuid)>,
) -> Vec<QueryHit> {
    let mut out: Vec<QueryHit> = Vec::new();
    let mut seen: std::collections::HashSet<Uuid> = std::collections::HashSet::new();
    let push = |id: Uuid,
                page: Uuid,
                out: &mut Vec<QueryHit>,
                seen: &mut std::collections::HashSet<Uuid>| {
        if seen.insert(id) {
            out.push(QueryHit {
                block_id: id,
                page_id: page,
            });
        }
    };
    match query {
        Query::All => {
            for (bid, pid) in all_block_ids {
                push(bid, pid, &mut out, &mut seen);
            }
        }
        Query::Tag(name) => {
            let needle = name.to_lowercase();
            for e in block_refs {
                if e.target_kind == "tag" && e.target_str.to_lowercase() == needle {
                    push(e.source_block_id, e.source_page_id, &mut out, &mut seen);
                }
            }
        }
        Query::Link(name) => {
            let needle = name.to_lowercase();
            for e in block_refs {
                if e.target_kind == "page" && e.target_str.to_lowercase() == needle {
                    push(e.source_block_id, e.source_page_id, &mut out, &mut seen);
                }
            }
        }
        Query::Property { key, value } => {
            let want_value_json = serde_json::Value::String(value.clone()).to_string();
            for e in block_props {
                if e.key != *key {
                    continue;
                }
                // Match either the JSON-encoded form (`"todo"`)
                // or the raw value (handles non-string values
                // like numbers / bools written directly).
                if e.value_json == want_value_json || e.value_json == *value {
                    push(e.block_id, e.page_id, &mut out, &mut seen);
                }
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty_is_all() {
        assert_eq!(parse_query(""), Some(Query::All));
        assert_eq!(parse_query("   "), Some(Query::All));
    }

    #[test]
    fn parse_tag() {
        assert_eq!(
            parse_query("#area/work"),
            Some(Query::Tag("area/work".into()))
        );
        assert_eq!(parse_query("#"), None);
    }

    #[test]
    fn parse_page_link() {
        assert_eq!(
            parse_query("[[Project A]]"),
            Some(Query::Link("Project A".into()))
        );
    }

    #[test]
    fn parse_property() {
        assert_eq!(
            parse_query("status:todo"),
            Some(Query::Property {
                key: "status".into(),
                value: "todo".into()
            })
        );
        // Case-insensitive key, case-preserving value.
        assert_eq!(
            parse_query("Priority: High"),
            Some(Query::Property {
                key: "priority".into(),
                value: "High".into()
            })
        );
    }

    #[test]
    fn parse_malformed_returns_none() {
        assert_eq!(parse_query("just text"), None);
        assert_eq!(parse_query(":only-value"), None);
        assert_eq!(parse_query("only-key:"), None);
    }

    #[test]
    fn parse_block_level_extracts_body() {
        assert_eq!(
            parse_block_level_query("{{query #foo}}"),
            Some(Query::Tag("foo".into()))
        );
        assert_eq!(
            parse_block_level_query("  {{query [[X]]}}  "),
            Some(Query::Link("X".into()))
        );
        assert_eq!(parse_block_level_query("{{query}}"), Some(Query::All));
        // Surrounding prose disqualifies it as a block-level query.
        assert_eq!(parse_block_level_query("hello {{query #x}} bye"), None);
    }
}
