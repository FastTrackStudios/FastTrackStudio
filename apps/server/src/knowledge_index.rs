//! Phase 5b — server-side Knowledge indexes.
//!
//! Three correlated in-memory indexes maintained over the org vault:
//!
//! - **`FrontmatterIndex`** — `(field, value) -> {page_id}`. Lets
//!   Bases queries (Phase 6) and the access check (Phase 4 ACL
//!   resolver, when wired) find pages by frontmatter facets.
//! - **`BacklinkIndex`** — `target_basename -> {referrer_page_id}`.
//!   Powers the Obsidian-style "linked mentions" surface.
//! - **`MemoryBasenameIndex`** (Phase 4) — `basename -> auth_user_id`.
//!   Updated as a side effect of the frontmatter scan whenever a
//!   page carries `auth_user_id`.
//!
//! The indexer hooks the org-vault `LoroDoc`'s
//! `subscribe_local_update` callback and rebuilds on every commit.
//! Rebuild is O(pages + blocks) — fine for the vertical-slice
//! scale; an incremental version is a Phase 10 optimization.
//!
//! Indexes are server-local + non-persistent — they reconstruct
//! from the doc on boot.

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

use architect::{Filter, Page as PageWindow, Sort};
use knowledge_crdt::{BlockRepoLoro, PageRepoLoro};
use knowledge_proto::property_schema::{PropertySchemaRegistry, PropertyType};
use knowledge_proto::{BlockRepo, PageRepo};
use serde_json::Value as JsonValue;
use uuid::Uuid;

use crate::basename_index::MemoryBasenameIndex;

/// `(field, value)` → set of page ids. Field names are taken
/// verbatim from frontmatter top-level keys; values are stringified
/// (numbers / bools become their Display form).
#[derive(Debug, Default, Clone)]
pub struct FrontmatterIndex {
    inner: Arc<RwLock<HashMap<(String, String), HashSet<Uuid>>>>,
}

impl FrontmatterIndex {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn pages_with(&self, field: &str, value: &str) -> Vec<Uuid> {
        let inner = self.inner.read().unwrap();
        inner
            .get(&(field.to_string(), value.to_string()))
            .map(|s| s.iter().copied().collect())
            .unwrap_or_default()
    }

    pub fn fields(&self) -> Vec<(String, String)> {
        self.inner.read().unwrap().keys().cloned().collect()
    }

    fn replace_with(&self, rebuilt: HashMap<(String, String), HashSet<Uuid>>) {
        *self.inner.write().unwrap() = rebuilt;
    }
}

/// Wiki-link target basename → set of referrer page ids.
#[derive(Debug, Default, Clone)]
pub struct BacklinkIndex {
    inner: Arc<RwLock<HashMap<String, HashSet<Uuid>>>>,
}

impl BacklinkIndex {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn referrers_of(&self, basename: &str) -> Vec<Uuid> {
        self.inner
            .read()
            .unwrap()
            .get(basename)
            .map(|s| s.iter().copied().collect())
            .unwrap_or_default()
    }

    pub fn len(&self) -> usize {
        self.inner.read().unwrap().len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.read().unwrap().is_empty()
    }

    fn replace_with(&self, rebuilt: HashMap<String, HashSet<Uuid>>) {
        *self.inner.write().unwrap() = rebuilt;
    }
}

/// Driver: holds repo handles + the three indexes; rebuilds them
/// from scratch on each `rebuild()` call.
#[derive(Clone)]
pub struct KnowledgeIndexer {
    pub frontmatter: FrontmatterIndex,
    pub backlinks: BacklinkIndex,
    pub basenames: MemoryBasenameIndex,
    pub schemas: Arc<PropertySchemaRegistry>,
    pages: PageRepoLoro,
    blocks: BlockRepoLoro,
}

impl KnowledgeIndexer {
    pub fn new(pages: PageRepoLoro, blocks: BlockRepoLoro, basenames: MemoryBasenameIndex) -> Self {
        Self::with_schemas(
            pages,
            blocks,
            basenames,
            Arc::new(PropertySchemaRegistry::with_builtins()),
        )
    }

    pub fn with_schemas(
        pages: PageRepoLoro,
        blocks: BlockRepoLoro,
        basenames: MemoryBasenameIndex,
        schemas: Arc<PropertySchemaRegistry>,
    ) -> Self {
        Self {
            frontmatter: FrontmatterIndex::new(),
            backlinks: BacklinkIndex::new(),
            basenames,
            schemas,
            pages,
            blocks,
        }
    }

    /// Rescan every page + block, rebuild all three indexes from
    /// scratch. Backlink edges + frontmatter facets are derived;
    /// the basename → user_id mapping is updated as a side effect
    /// of seeing `auth_user_id` in a page's frontmatter.
    pub async fn rebuild(&self) -> Result<(), architect::RepoError> {
        // Pull everything. Vertical-slice sizing — list all in one
        // shot. Production lift later.
        let pages = self
            .pages
            .list(big_page(), None::<Sort>, None::<Filter>)
            .await?;
        let blocks = self
            .blocks
            .list(big_page(), None::<Sort>, None::<Filter>)
            .await?;

        let mut basename_to_page: HashMap<String, Uuid> = HashMap::new();
        let mut frontmatter: HashMap<(String, String), HashSet<Uuid>> = HashMap::new();
        let mut basename_users: HashMap<String, Uuid> = HashMap::new();

        for page in &pages.items {
            basename_to_page.insert(page.basename.clone(), page.id);
            let Ok(JsonValue::Object(map)) =
                serde_json::from_str::<JsonValue>(&page.frontmatter_json)
            else {
                continue;
            };
            // What kind is this page? Drives schema-aware
            // decomposition. Fallback: no kind = treat every value
            // as a scalar (legacy behavior).
            let page_kind = map.get("kind").and_then(|v| v.as_str()).unwrap_or("");
            let schema = if page_kind.is_empty() {
                None
            } else {
                self.schemas.get(page_kind)
            };
            for (k, v) in &map {
                // Find the declared property type, if any.
                let declared = schema
                    .as_ref()
                    .and_then(|s| s.properties.iter().find(|p| &p.key == k))
                    .map(|p| p.ty.clone());
                index_value(&mut frontmatter, page.id, k, v, declared.as_ref());

                // Side-channel: harvest `auth_user_id` for the
                // basename index.
                if k == "auth_user_id" {
                    if let Some(s) = v.as_str() {
                        if let Ok(uid) = Uuid::parse_str(s) {
                            basename_users.insert(page.basename.clone(), uid);
                        }
                    }
                }
            }
        }

        let mut backlinks: HashMap<String, HashSet<Uuid>> = HashMap::new();
        for block in &blocks.items {
            for target in extract_wikilink_targets(&block.content) {
                backlinks.entry(target).or_default().insert(block.page_id);
            }
        }

        self.frontmatter.replace_with(frontmatter);
        self.backlinks.replace_with(backlinks);
        // Drive the basename index. Overwrites existing entries that
        // share a name; users removed from a page lose their entry.
        // Cheap because the set is small in practice.
        self.basenames.clear();
        for (basename, user_id) in basename_users {
            self.basenames.upsert(basename, user_id);
        }
        Ok(())
    }
}

fn big_page() -> PageWindow {
    PageWindow {
        index: 0,
        size: u32::MAX,
    }
}

fn json_value_to_index_string(v: &JsonValue) -> Option<String> {
    match v {
        JsonValue::String(s) => Some(s.clone()),
        JsonValue::Number(n) => Some(n.to_string()),
        JsonValue::Bool(b) => Some(b.to_string()),
        JsonValue::Null => None,
        // Arrays + objects: skip for the legacy index path. The
        // schema-aware decomposition in `index_value` handles these.
        _ => None,
    }
}

/// Schema-aware indexing — Phase 6.5a upgrade.
///
/// - For `Tags` / `Aliases` / `LinkList` types (lists of strings),
///   index each element under `(key, element)`.
/// - For `Struct` / `StructList` types, index nested fields under
///   `(key.field, value)` paths (one level deep — deeper nesting
///   stays opaque until Phase 6.6).
/// - For scalar types, fall back to the legacy stringification.
/// - When `declared` is `None`, mirror legacy behavior for scalars
///   AND fall through to list-element decomposition so untyped
///   pages still index their array values.
fn index_value(
    frontmatter: &mut HashMap<(String, String), HashSet<Uuid>>,
    page_id: Uuid,
    key: &str,
    value: &JsonValue,
    declared: Option<&PropertyType>,
) {
    use PropertyType::*;
    let list_like = matches!(
        declared,
        Some(Tags) | Some(Aliases) | Some(LinkList) | Some(Multitext)
    );
    let struct_like = matches!(declared, Some(Struct { .. }) | Some(StructList { .. }));
    match value {
        // Scalars first — typed or untyped, same logic.
        JsonValue::String(_) | JsonValue::Number(_) | JsonValue::Bool(_) => {
            if let Some(s) = json_value_to_index_string(value) {
                frontmatter
                    .entry((key.to_string(), s))
                    .or_default()
                    .insert(page_id);
            }
        }
        JsonValue::Null => {}
        JsonValue::Array(items) => {
            if list_like || declared.is_none() {
                // Decompose: one entry per element. Skip non-scalar
                // elements (deeper nesting is opaque).
                for el in items {
                    if let Some(s) = json_value_to_index_string(el) {
                        frontmatter
                            .entry((key.to_string(), s))
                            .or_default()
                            .insert(page_id);
                    } else if struct_like {
                        if let JsonValue::Object(map) = el {
                            index_struct_fields(frontmatter, page_id, key, map);
                        }
                    }
                }
            }
        }
        JsonValue::Object(map) => {
            if struct_like || declared.is_none() {
                index_struct_fields(frontmatter, page_id, key, map);
            }
        }
    }
}

fn index_struct_fields(
    frontmatter: &mut HashMap<(String, String), HashSet<Uuid>>,
    page_id: Uuid,
    parent_key: &str,
    map: &serde_json::Map<String, JsonValue>,
) {
    for (child_key, child_value) in map {
        if let Some(s) = json_value_to_index_string(child_value) {
            let path = format!("{parent_key}.{child_key}");
            frontmatter.entry((path, s)).or_default().insert(page_id);
        }
    }
}

/// Extract `[[Target]]` basenames out of a content string.
/// Handles `[[Target]]`, `[[Target|alias]]` (target only), and
/// `[[Target#heading]]` (target only). Ignores empty targets.
fn extract_wikilink_targets(content: &str) -> Vec<String> {
    let mut out = Vec::new();
    let bytes = content.as_bytes();
    let mut i = 0;
    while i + 1 < bytes.len() {
        if bytes[i] == b'[' && bytes[i + 1] == b'[' {
            let start = i + 2;
            let mut j = start;
            while j + 1 < bytes.len() && !(bytes[j] == b']' && bytes[j + 1] == b']') {
                j += 1;
            }
            if j + 1 < bytes.len() {
                let inner = &content[start..j];
                let target = inner
                    .split(|c| c == '|' || c == '#')
                    .next()
                    .unwrap_or("")
                    .trim();
                if !target.is_empty() {
                    out.push(target.to_string());
                }
                i = j + 2;
                continue;
            }
        }
        i += 1;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wikilink_extraction() {
        assert_eq!(extract_wikilink_targets("plain text"), Vec::<String>::new());
        assert_eq!(extract_wikilink_targets("see [[Cody]]"), vec!["Cody"]);
        assert_eq!(
            extract_wikilink_targets("two [[A]] and [[B]]"),
            vec!["A", "B"]
        );
        assert_eq!(
            extract_wikilink_targets("[[Target|display alias]]"),
            vec!["Target"]
        );
        assert_eq!(
            extract_wikilink_targets("[[Target#heading]] and [[Other]]"),
            vec!["Target", "Other"]
        );
        // Unclosed: no panic, no result.
        assert_eq!(
            extract_wikilink_targets("[[NotClosed"),
            Vec::<String>::new()
        );
    }

    #[test]
    fn json_value_index_strings() {
        assert_eq!(
            json_value_to_index_string(&JsonValue::String("x".into())),
            Some("x".into())
        );
        assert_eq!(
            json_value_to_index_string(&JsonValue::Bool(true)),
            Some("true".into())
        );
        assert_eq!(
            json_value_to_index_string(&serde_json::json!(42)),
            Some("42".into())
        );
        assert_eq!(json_value_to_index_string(&JsonValue::Null), None);
        assert_eq!(json_value_to_index_string(&serde_json::json!(["a"])), None,);
    }
}
