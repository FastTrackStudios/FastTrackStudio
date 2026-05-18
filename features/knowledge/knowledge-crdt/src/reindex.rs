//! Materialized-view maintenance for `BlockRefEdge` and
//! `BlockPropEdge`.
//!
//! `Block.refs_json` and `Block.properties_json` are the source of
//! truth — these helpers re-project them into edge entities every
//! time a block is created/updated/deleted, so queries like
//! "what blocks reference page X?" are O(edges) lookups instead of
//! O(blocks × refs-per-block) content scans.
//!
//! Synchronous-on-write for v1 — predictable, easy to reason
//! about. Subscription-driven async reindex can replace this if
//! the per-write latency ever shows up in profiles.

use std::collections::{HashMap, HashSet};

use architect::{Page as PageWindow, RepoError};
use crdt::CrdtDoc;
use knowledge_proto::{
    Block, BlockPropEdge, BlockPropEdgeCreate, BlockPropEdgeRepo, BlockRefEdge, BlockRefEdgeCreate,
    BlockRefEdgeRepo, BlockRepo, PagePropEdge, PagePropEdgeCreate, PagePropEdgeRepo, PageRepo, Ref,
    property_schema::{KindSchema, PropertySchemaRegistry, PropertyType},
};
use uuid::Uuid;

use crate::{
    BlockPropEdgeRepoLoro, BlockRefEdgeRepoLoro, BlockRepoLoro, PagePropEdgeRepoLoro, PageRepoLoro,
};

const BIG: u32 = 100_000;

/// Re-project a single block's refs + properties into edge
/// entities. Idempotent — re-running on an unchanged block
/// produces the same edge set.
///
/// Call this after every `Block` create or update.
pub async fn reindex_block(doc: &CrdtDoc, block_id: Uuid) -> Result<(), RepoError> {
    let block_repo = BlockRepoLoro::new(doc);
    let block = block_repo.get(block_id).await?;
    let registry = PropertySchemaRegistry::with_builtins();
    let schema = lookup_page_schema(doc, block.page_id, &registry).await;
    reindex_refs(doc, &block).await?;
    reindex_block_props(doc, &block, schema.as_ref()).await?;
    Ok(())
}

/// Re-project a single page's frontmatter into typed
/// `PagePropEdge` rows. Idempotent. Call after every `Page`
/// create or update.
pub async fn reindex_page(doc: &CrdtDoc, page_id: Uuid) -> Result<(), RepoError> {
    let page_repo = PageRepoLoro::new(doc);
    let page = page_repo.get(page_id).await?;
    let registry = PropertySchemaRegistry::with_builtins();
    let kind = page_kind_from_frontmatter(&page.frontmatter_json);
    let schema = kind.as_deref().and_then(|k| registry.get(k));

    let repo = PagePropEdgeRepoLoro::new(doc);
    let existing = list_page_prop_edges(&repo, page_id).await?;
    let parsed: HashMap<String, serde_json::Value> =
        serde_json::from_str(&page.frontmatter_json).unwrap_or_default();

    let desired: HashSet<(String, String, String)> = parsed
        .into_iter()
        .map(|(k, v)| {
            let key_lower = k.to_lowercase();
            let value_json = serde_json::to_string(&v).unwrap_or_else(|_| "null".into());
            let value_type = resolve_value_type(schema.as_ref(), &key_lower);
            (key_lower, value_json, value_type)
        })
        .collect();
    let existing_map: HashMap<(String, String, String), PagePropEdge> = existing
        .into_iter()
        .map(|e| {
            (
                (e.key.clone(), e.value_json.clone(), e.value_type.clone()),
                e,
            )
        })
        .collect();

    for (key, value_json, value_type) in &desired {
        if !existing_map.contains_key(&(key.clone(), value_json.clone(), value_type.clone())) {
            repo.create(PagePropEdgeCreate {
                vault_id: page.vault_id,
                page_id: page.id,
                key: key.clone(),
                value_json: value_json.clone(),
                value_type: value_type.clone(),
            })
            .await?;
        }
    }
    for (k, edge) in existing_map {
        if !desired.contains(&k) {
            repo.delete(edge.id).await?;
        }
    }
    Ok(())
}

async fn lookup_page_schema(
    doc: &CrdtDoc,
    page_id: Uuid,
    registry: &PropertySchemaRegistry,
) -> Option<KindSchema> {
    let page_repo = PageRepoLoro::new(doc);
    let page = page_repo.get(page_id).await.ok()?;
    let kind = page_kind_from_frontmatter(&page.frontmatter_json)?;
    registry.get(&kind)
}

fn page_kind_from_frontmatter(json: &str) -> Option<String> {
    let v: serde_json::Value = serde_json::from_str(json).ok()?;
    v.get("kind")
        .and_then(|x| x.as_str())
        .map(|s| s.to_string())
}

/// Resolve the canonical value-type tag for a key against an
/// optional schema. Returns the empty string when the property
/// isn't declared (free-form user property).
fn resolve_value_type(schema: Option<&KindSchema>, key_lower: &str) -> String {
    let Some(s) = schema else {
        return String::new();
    };
    let Some(def) = s
        .properties
        .iter()
        .find(|d| d.key.to_lowercase() == key_lower)
    else {
        return String::new();
    };
    property_type_tag(&def.ty).into()
}

fn property_type_tag(t: &PropertyType) -> &'static str {
    match t {
        PropertyType::Text => "text",
        PropertyType::Multitext => "multitext",
        PropertyType::Number => "number",
        PropertyType::Checkbox => "checkbox",
        PropertyType::Date => "date",
        PropertyType::Datetime => "datetime",
        PropertyType::Tags => "tags",
        PropertyType::Aliases => "aliases",
        PropertyType::Link => "link",
        PropertyType::LinkList => "link_list",
        PropertyType::EnumWithMetadata { .. } => "enum_with_metadata",
        PropertyType::Struct { .. } => "struct",
        PropertyType::StructList { .. } => "struct_list",
        PropertyType::Computed { .. } => "computed",
        PropertyType::LexoRank => "lexorank",
        PropertyType::Json => "json",
    }
}

async fn list_page_prop_edges(
    repo: &PagePropEdgeRepoLoro,
    page_id: Uuid,
) -> Result<Vec<PagePropEdge>, RepoError> {
    let list = repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await?;
    Ok(list
        .items
        .into_iter()
        .filter(|e| e.page_id == page_id)
        .collect())
}

/// Cascade-delete every page-prop edge for a deleted page.
pub async fn cascade_delete_page_edges(doc: &CrdtDoc, page_id: Uuid) -> Result<(), RepoError> {
    let repo = PagePropEdgeRepoLoro::new(doc);
    for edge in list_page_prop_edges(&repo, page_id).await? {
        repo.delete(edge.id).await?;
    }
    Ok(())
}

// ── Tier 3: page-rename cascade + ref resolution ─────────────────────

/// When a page is renamed, every existing `BlockRefEdge` that
/// targeted the OLD basename should:
/// 1. Update its `target_str` to the NEW basename (preserves the
///    case the user typed).
/// 2. Re-resolve its `target_uuid` to point at the renamed page.
///
/// Idempotent — re-running with the same arguments is a no-op
/// after the first call.
pub async fn cascade_rename_page_refs(
    doc: &CrdtDoc,
    page_id: Uuid,
    old_basename: &str,
    new_basename: &str,
) -> Result<(), RepoError> {
    if old_basename.eq_ignore_ascii_case(new_basename) {
        return Ok(());
    }
    let repo = BlockRefEdgeRepoLoro::new(doc);
    let lower_old = old_basename.to_lowercase();
    let list = repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await?;
    for edge in list.items {
        if edge.target_kind != "page" || edge.target_str.to_lowercase() != lower_old {
            continue;
        }
        repo.update(
            edge.id,
            knowledge_proto::BlockRefEdgeUpdate {
                target_str: Some(new_basename.to_string()),
                target_uuid: Some(Some(page_id)),
                ..Default::default()
            },
        )
        .await?;
    }
    Ok(())
}

/// When a brand-new page is created (or imported), look for any
/// previously-unresolved `BlockRefEdge`s whose `target_str`
/// matches the page's basename **or any of its aliases** and
/// stamp their `target_uuid`. "Click `[[NewPage]]` in a block,
/// then create NewPage" — the old block-link auto-resolves
/// without needing a re-edit.
pub async fn resolve_page_refs(
    doc: &CrdtDoc,
    page_id: Uuid,
    basename: &str,
    aliases: &[String],
) -> Result<usize, RepoError> {
    let repo = BlockRefEdgeRepoLoro::new(doc);
    let mut targets: HashSet<String> = HashSet::new();
    targets.insert(basename.to_lowercase());
    for a in aliases {
        targets.insert(a.to_lowercase());
    }
    let list = repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await?;
    let mut count = 0usize;
    for edge in list.items {
        if edge.target_kind != "page"
            || edge.target_uuid.is_some()
            || !targets.contains(&edge.target_str.to_lowercase())
        {
            continue;
        }
        repo.update(
            edge.id,
            knowledge_proto::BlockRefEdgeUpdate {
                target_uuid: Some(Some(page_id)),
                ..Default::default()
            },
        )
        .await?;
        count += 1;
    }
    Ok(count)
}

/// When a page is deleted, every `BlockRefEdge` that pointed at
/// its UUID becomes a broken link — clear the resolved
/// `target_uuid` so the UI marker shows. (We deliberately keep
/// the edge so the source block's prose is preserved; users may
/// recreate the page later, in which case `resolve_page_refs`
/// will reattach it.)
pub async fn unresolve_page_refs(doc: &CrdtDoc, page_id: Uuid) -> Result<(), RepoError> {
    let repo = BlockRefEdgeRepoLoro::new(doc);
    let list = repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await?;
    for edge in list.items {
        if edge.target_kind != "page" || edge.target_uuid != Some(page_id) {
            continue;
        }
        repo.update(
            edge.id,
            knowledge_proto::BlockRefEdgeUpdate {
                target_uuid: Some(None),
                ..Default::default()
            },
        )
        .await?;
    }
    Ok(())
}

/// Every `BlockRefEdge` of kind `"page"` whose `target_uuid` is
/// `None`. Drives the broken-link UI marker + a future
/// "Broken links" panel.
pub async fn find_broken_links(doc: &CrdtDoc) -> Result<Vec<BlockRefEdge>, RepoError> {
    let repo = BlockRefEdgeRepoLoro::new(doc);
    let list = repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await?;
    Ok(list
        .items
        .into_iter()
        .filter(|e| e.target_kind == "page" && e.target_uuid.is_none())
        .collect())
}

/// Cascade-delete every edge whose source/owner is the deleted
/// block. Call this **after** the block itself has been removed.
pub async fn cascade_delete_block_edges(doc: &CrdtDoc, block_id: Uuid) -> Result<(), RepoError> {
    let ref_repo = BlockRefEdgeRepoLoro::new(doc);
    for edge in list_ref_edges_for_block(&ref_repo, block_id).await? {
        ref_repo.delete(edge.id).await?;
    }
    let prop_repo = BlockPropEdgeRepoLoro::new(doc);
    for edge in list_prop_edges_for_block(&prop_repo, block_id).await? {
        prop_repo.delete(edge.id).await?;
    }
    Ok(())
}

// ── Refs ──────────────────────────────────────────────────────────────

async fn reindex_refs(doc: &CrdtDoc, block: &Block) -> Result<(), RepoError> {
    let repo = BlockRefEdgeRepoLoro::new(doc);
    let existing = list_ref_edges_for_block(&repo, block.id).await?;
    let parsed_refs: Vec<Ref> = serde_json::from_str(&block.refs_json).unwrap_or_default();
    let desired = project_refs(block, &parsed_refs, doc).await;

    let existing_keys: HashMap<RefEdgeKey, BlockRefEdge> = existing
        .into_iter()
        .map(|e| (RefEdgeKey::from_edge(&e), e))
        .collect();
    let mut desired_keys: HashSet<RefEdgeKey> = HashSet::new();

    for create in desired {
        let key = RefEdgeKey {
            target_kind: create.target_kind.clone(),
            target_str: create.target_str.clone(),
            target_uuid: create.target_uuid,
            alias: create.alias.clone(),
        };
        desired_keys.insert(key.clone());
        if !existing_keys.contains_key(&key) {
            repo.create(create).await?;
        }
    }

    // Drop edges that disappeared from the new content.
    for (key, edge) in existing_keys {
        if !desired_keys.contains(&key) {
            repo.delete(edge.id).await?;
        }
    }
    Ok(())
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct RefEdgeKey {
    target_kind: String,
    target_str: String,
    target_uuid: Option<Uuid>,
    alias: Option<String>,
}

impl RefEdgeKey {
    fn from_edge(e: &BlockRefEdge) -> Self {
        Self {
            target_kind: e.target_kind.clone(),
            target_str: e.target_str.clone(),
            target_uuid: e.target_uuid,
            alias: e.alias.clone(),
        }
    }
}

/// Translate the parsed `Ref`s into create-payloads, resolving
/// page-name → page-id when possible.
async fn project_refs(block: &Block, refs: &[Ref], doc: &CrdtDoc) -> Vec<BlockRefEdgeCreate> {
    // Pre-load the page basename → id map for resolution.
    // Aliases are also indexed so `[[Old Name]]` resolves to a
    // page that has `aliases: [Old Name]` in its frontmatter.
    let page_repo = PageRepoLoro::new(doc);
    let mut basename_index: HashMap<String, Uuid> = HashMap::new();
    if let Ok(list) = page_repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await
    {
        for p in list.items {
            basename_index.insert(p.basename.to_lowercase(), p.id);
            for alias in &p.aliases {
                basename_index.entry(alias.to_lowercase()).or_insert(p.id);
            }
        }
    }

    let mut out = Vec::with_capacity(refs.len());
    for r in refs {
        let create = match r {
            Ref::Link(link) => {
                let target = link.target_linkpath.clone();
                let resolved = basename_index.get(&target.to_lowercase()).copied();
                BlockRefEdgeCreate {
                    vault_id: block.vault_id,
                    source_block_id: block.id,
                    source_page_id: block.page_id,
                    target_kind: "page".into(),
                    target_str: target,
                    target_uuid: resolved,
                    alias: link.alias.clone(),
                }
            }
            Ref::Embed(embed) => {
                let target = embed.target_linkpath.clone();
                let resolved = basename_index.get(&target.to_lowercase()).copied();
                BlockRefEdgeCreate {
                    vault_id: block.vault_id,
                    source_block_id: block.id,
                    source_page_id: block.page_id,
                    target_kind: "embed".into(),
                    target_str: target,
                    target_uuid: resolved,
                    alias: embed.alias.clone(),
                }
            }
            Ref::Tag(tag) => BlockRefEdgeCreate {
                vault_id: block.vault_id,
                source_block_id: block.id,
                source_page_id: block.page_id,
                target_kind: "tag".into(),
                target_str: tag.path.join("/"),
                target_uuid: None,
                alias: None,
            },
            Ref::Entity(ent) => BlockRefEdgeCreate {
                vault_id: block.vault_id,
                source_block_id: block.id,
                source_page_id: block.page_id,
                target_kind: format!("entity:{}", ent.kind),
                target_str: ent.id.to_string(),
                target_uuid: Some(ent.id),
                alias: ent.display.clone(),
            },
            Ref::BlockRef(br) => BlockRefEdgeCreate {
                vault_id: block.vault_id,
                source_block_id: block.id,
                source_page_id: block.page_id,
                target_kind: "block".into(),
                target_str: br.target_block_id.to_string(),
                target_uuid: Some(br.target_block_id),
                alias: br.alias.clone(),
            },
        };
        out.push(create);
    }
    out
}

async fn list_ref_edges_for_block(
    repo: &BlockRefEdgeRepoLoro,
    source_block_id: Uuid,
) -> Result<Vec<BlockRefEdge>, RepoError> {
    let list = repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await?;
    Ok(list
        .items
        .into_iter()
        .filter(|e| e.source_block_id == source_block_id)
        .collect())
}

// ── Props ─────────────────────────────────────────────────────────────

async fn reindex_block_props(
    doc: &CrdtDoc,
    block: &Block,
    schema: Option<&KindSchema>,
) -> Result<(), RepoError> {
    let repo = BlockPropEdgeRepoLoro::new(doc);
    let existing = list_prop_edges_for_block(&repo, block.id).await?;
    let parsed: HashMap<String, serde_json::Value> =
        serde_json::from_str(&block.properties_json).unwrap_or_default();

    // Desired set keyed by (key, value_json, value_type). Distinct
    // values for the same key still produce distinct edges.
    let desired: HashSet<(String, String, String)> = parsed
        .into_iter()
        .map(|(k, v)| {
            let key_lower = k.to_lowercase();
            let value_json = serde_json::to_string(&v).unwrap_or_else(|_| "null".into());
            let value_type = resolve_value_type(schema, &key_lower);
            (key_lower, value_json, value_type)
        })
        .collect();
    let existing_map: HashMap<(String, String, String), BlockPropEdge> = existing
        .into_iter()
        .map(|e| {
            (
                (e.key.clone(), e.value_json.clone(), e.value_type.clone()),
                e,
            )
        })
        .collect();

    for (key, value_json, value_type) in &desired {
        if !existing_map.contains_key(&(key.clone(), value_json.clone(), value_type.clone())) {
            repo.create(BlockPropEdgeCreate {
                vault_id: block.vault_id,
                block_id: block.id,
                page_id: block.page_id,
                key: key.clone(),
                value_json: value_json.clone(),
                value_type: value_type.clone(),
            })
            .await?;
        }
    }
    for (k, edge) in existing_map {
        if !desired.contains(&k) {
            repo.delete(edge.id).await?;
        }
    }
    Ok(())
}

async fn list_prop_edges_for_block(
    repo: &BlockPropEdgeRepoLoro,
    block_id: Uuid,
) -> Result<Vec<BlockPropEdge>, RepoError> {
    let list = repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await?;
    Ok(list
        .items
        .into_iter()
        .filter(|e| e.block_id == block_id)
        .collect())
}

// ── Public query helpers ──────────────────────────────────────────────

/// Find every block whose content references a page basename
/// (case-insensitive). Constant-time per block — scans the
/// `BlockRefEdge` index, not the underlying content.
pub async fn find_backlinks(
    doc: &CrdtDoc,
    target_basename: &str,
) -> Result<Vec<BlockRefEdge>, RepoError> {
    let repo = BlockRefEdgeRepoLoro::new(doc);
    let lower = target_basename.to_lowercase();
    let list = repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await?;
    Ok(list
        .items
        .into_iter()
        .filter(|e| e.target_kind == "page" && e.target_str.to_lowercase() == lower)
        .collect())
}

/// Find every block whose `properties_json` contains the given
/// `key` (any value). Pass `Some(value_json)` to require an
/// exact-string match against the serialized JSON value.
pub async fn find_blocks_with_prop(
    doc: &CrdtDoc,
    key: &str,
    value_json: Option<&str>,
) -> Result<Vec<BlockPropEdge>, RepoError> {
    let repo = BlockPropEdgeRepoLoro::new(doc);
    let key_lower = key.to_lowercase();
    let list = repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await?;
    Ok(list
        .items
        .into_iter()
        .filter(|e| e.key == key_lower)
        .filter(|e| match value_json {
            Some(v) => e.value_json == v,
            None => true,
        })
        .collect())
}

/// One-shot bootstrap: re-project every page + block in the doc
/// into edges. Useful after loading a doc that was created by
/// another client (e.g. server-seeded), since our reindex hooks
/// only fire on local writes through the wrapped repos.
///
/// Idempotent — safe to call repeatedly. Bounded by O(pages +
/// blocks); each reprojection is O(refs/props per row).
pub async fn reindex_all(doc: &CrdtDoc) -> Result<(), RepoError> {
    let page_repo = PageRepoLoro::new(doc);
    let block_repo = BlockRepoLoro::new(doc);
    let pages = page_repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await?;
    for p in &pages.items {
        if let Err(e) = reindex_page(doc, p.id).await {
            tracing::warn!(?e, %p.id, "bootstrap page reindex failed");
        }
    }
    let blocks = block_repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await?;
    for b in &blocks.items {
        if let Err(e) = reindex_block(doc, b.id).await {
            tracing::warn!(?e, %b.id, "bootstrap block reindex failed");
        }
    }
    Ok(())
}

/// Find every page whose frontmatter contains the given `key`
/// (any value if `value_json` is None, exact string match
/// otherwise). Tier 2 — answers "all pages where status:
/// in_progress" / "all kind: project pages" in O(edges).
pub async fn find_pages_with_prop(
    doc: &CrdtDoc,
    key: &str,
    value_json: Option<&str>,
) -> Result<Vec<PagePropEdge>, RepoError> {
    let repo = PagePropEdgeRepoLoro::new(doc);
    let key_lower = key.to_lowercase();
    let list = repo
        .list(
            PageWindow {
                index: 0,
                size: BIG,
            },
            None,
            None,
        )
        .await?;
    Ok(list
        .items
        .into_iter()
        .filter(|e| e.key == key_lower)
        .filter(|e| match value_json {
            Some(v) => e.value_json == v,
            None => true,
        })
        .collect())
}
