//! Hand-written `BlockRepoLoro` — kept off the `entity_crdt!`
//! macro because `Block.content` is a `LoroText` child container
//! (the macro currently doesn't support `text` field types) and
//! the editor's fast path (`apply_text_ops`) needs direct access
//! to the underlying `LoroText` for character-level merge.
//!
//! Migration target: extend `entity_crdt!` with a `text` field
//! kind, then collapse this into a one-liner like the other
//! knowledge entities.

use architect::{Page as PageWindow, RepoError, SortOrder};
use chrono::Utc;
use crdt::codec::{
    apply_text_diff, read_bool, read_dt, read_i64, read_opt_str, read_opt_uuid, read_str,
    read_text, read_text_with_migration, read_uuid, text_child, write_bool, write_dt, write_i64,
    write_opt_str, write_opt_uuid, write_str, write_uuid,
};
use crdt::{CrdtDoc, EntityCrdt, LoroRepo, codec::TextOp};
use knowledge_proto::{Block, BlockCreate, BlockList, BlockRepo, BlockUpdate};
use loro::{Container, LoroMap, LoroText, ValueOrContainer};
use uuid::Uuid;

pub struct BlockEntity;

#[derive(Clone)]
pub struct BlockRepoLoro {
    inner: LoroRepo<BlockEntity>,
    /// Cached `CrdtDoc` handle so post-write reindexing can spin
    /// up sibling repos (BlockRefEdge, BlockPropEdge, Page) over
    /// the same doc without re-plumbing every call site.
    crdt_doc: CrdtDoc,
}

impl BlockRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self {
            inner: doc.repo(),
            crdt_doc: doc.clone(),
        }
    }
    pub fn inner(&self) -> &LoroRepo<BlockEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
    pub fn crdt_doc(&self) -> &CrdtDoc {
        &self.crdt_doc
    }

    /// Return the live `LoroText` handle for a block's `content`
    /// container, or `None` if the block isn't in the doc yet. Used
    /// by the awareness layer to mint stable text cursors.
    pub fn text_handle(&self, block_id: Uuid) -> Option<LoroText> {
        let root = self.inner.doc().get_map(<BlockEntity as EntityCrdt>::ROOT);
        let sub = match root.get(&block_id.to_string())? {
            ValueOrContainer::Container(Container::Map(m)) => m,
            _ => return None,
        };
        match sub.get("content")? {
            ValueOrContainer::Container(Container::Text(t)) => Some(t),
            _ => None,
        }
    }

    /// Editor fast path. Apply character-level edits to the block's
    /// `content` LoroText. Skips `updated_at` bumping (the editor
    /// flushes a real `update` on blur / structural change).
    pub async fn apply_text_ops(&self, id: Uuid, ops: Vec<TextOp>) -> Result<(), RepoError> {
        self.inner.apply_text_ops(id, "content", &ops).await
    }
}

fn write_opt_i32(m: &LoroMap, k: &str, v: Option<i32>) -> Result<(), RepoError> {
    match v {
        Some(x) => write_i64(m, k, x as i64),
        None => write_i64(m, k, 0),
    }
}
fn read_opt_i32(m: &LoroMap, k: &str) -> Result<Option<i32>, RepoError> {
    // Sentinel 0 = absent. Heading levels are always >0 when set, so
    // this is unambiguous for the one Option<i32> field in Block.
    let v = read_i64(m, k)?;
    Ok(if v == 0 { None } else { Some(v as i32) })
}

impl EntityCrdt for BlockEntity {
    type Wire = Block;
    type Create = BlockCreate;
    type Update = BlockUpdate;
    type List = BlockList;

    const ROOT: &'static str = "knowledge_blocks";

    fn id(w: &Block) -> Uuid {
        w.id
    }

    fn from_create(input: BlockCreate) -> Block {
        let now = Utc::now();
        Block {
            id: Uuid::new_v4(),
            vault_id: input.vault_id,
            page_id: input.page_id,
            parent_block_id: input.parent_block_id,
            sort_key: input.sort_key,
            kind: input.kind,
            content: input.content,
            heading_level: input.heading_level,
            list_ordered: input.list_ordered,
            list_task: input.list_task,
            code_lang: input.code_lang,
            callout_kind: input.callout_kind,
            callout_foldable: input.callout_foldable,
            properties_json: input.properties_json,
            obsidian_block_id: input.obsidian_block_id,
            collapsed: input.collapsed,
            refs_json: input.refs_json,
            canvas_node_json: input.canvas_node_json,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Block) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "vault_id", e.vault_id)?;
        write_uuid(m, "page_id", e.page_id)?;
        write_opt_uuid(m, "parent_block_id", e.parent_block_id)?;
        write_str(m, "sort_key", &e.sort_key)?;
        write_str(m, "kind", &e.kind)?;
        // Seed the content `LoroText` container so subsequent edits
        // attach to the same container id.
        let _ = text_child(m, "content")?;
        if !e.content.is_empty() {
            apply_text_diff(m, "content", "", &e.content)?;
        }
        write_opt_i32(m, "heading_level", e.heading_level)?;
        write_bool(m, "list_ordered", e.list_ordered)?;
        write_opt_str(m, "list_task", e.list_task.as_deref())?;
        write_opt_str(m, "code_lang", e.code_lang.as_deref())?;
        write_opt_str(m, "callout_kind", e.callout_kind.as_deref())?;
        write_bool(m, "callout_foldable", e.callout_foldable)?;
        write_str(m, "properties_json", &e.properties_json)?;
        write_opt_str(m, "obsidian_block_id", e.obsidian_block_id.as_deref())?;
        write_bool(m, "collapsed", e.collapsed)?;
        write_str(m, "refs_json", &e.refs_json)?;
        write_opt_str(m, "canvas_node_json", e.canvas_node_json.as_deref())?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Block, RepoError> {
        Ok(Block {
            id: read_uuid(m, "id")?,
            vault_id: read_uuid(m, "vault_id")?,
            page_id: read_uuid(m, "page_id")?,
            parent_block_id: read_opt_uuid(m, "parent_block_id")?,
            sort_key: read_str(m, "sort_key")?,
            kind: read_str(m, "kind")?,
            content: read_text_with_migration(m, "content")?,
            heading_level: read_opt_i32(m, "heading_level")?,
            list_ordered: read_bool(m, "list_ordered")?,
            list_task: read_opt_str(m, "list_task")?,
            code_lang: read_opt_str(m, "code_lang")?,
            callout_kind: read_opt_str(m, "callout_kind")?,
            callout_foldable: read_bool(m, "callout_foldable")?,
            properties_json: read_str(m, "properties_json")?,
            obsidian_block_id: read_opt_str(m, "obsidian_block_id")?,
            collapsed: read_bool(m, "collapsed")?,
            refs_json: read_str(m, "refs_json")?,
            // Pre-A.5 snapshots may lack canvas_node_json.
            canvas_node_json: read_opt_str(m, "canvas_node_json").unwrap_or(None),
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: BlockUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.vault_id {
            write_uuid(m, "vault_id", v)?;
        }
        if let Some(v) = u.page_id {
            write_uuid(m, "page_id", v)?;
        }
        if let Some(v) = u.parent_block_id {
            write_opt_uuid(m, "parent_block_id", v)?;
        }
        if let Some(v) = u.sort_key {
            write_str(m, "sort_key", &v)?;
        }
        if let Some(v) = u.kind {
            write_str(m, "kind", &v)?;
        }
        if let Some(v) = u.content {
            // Compat: programmatic full-string sets diff against
            // the current LoroText so concurrent peer edits still
            // merge. Keystrokes use `apply_text_ops` instead.
            let old = read_text(m, "content")?;
            apply_text_diff(m, "content", &old, &v)?;
        }
        if let Some(v) = u.heading_level {
            write_opt_i32(m, "heading_level", v)?;
        }
        if let Some(v) = u.list_ordered {
            write_bool(m, "list_ordered", v)?;
        }
        if let Some(v) = u.list_task {
            write_opt_str(m, "list_task", v.as_deref())?;
        }
        if let Some(v) = u.code_lang {
            write_opt_str(m, "code_lang", v.as_deref())?;
        }
        if let Some(v) = u.callout_kind {
            write_opt_str(m, "callout_kind", v.as_deref())?;
        }
        if let Some(v) = u.callout_foldable {
            write_bool(m, "callout_foldable", v)?;
        }
        if let Some(v) = u.properties_json {
            write_str(m, "properties_json", &v)?;
        }
        if let Some(v) = u.obsidian_block_id {
            write_opt_str(m, "obsidian_block_id", v.as_deref())?;
        }
        if let Some(v) = u.collapsed {
            write_bool(m, "collapsed", v)?;
        }
        if let Some(v) = u.refs_json {
            write_str(m, "refs_json", &v)?;
        }
        if let Some(v) = u.canvas_node_json {
            write_opt_str(m, "canvas_node_json", v.as_deref())?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Block], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "sort_key" => items.sort_by(|a, b| a.sort_key.cmp(&b.sort_key)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<Block>, total: u32, page: PageWindow) -> BlockList {
        BlockList { items, total, page }
    }
}

impl BlockRepo for BlockRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Block, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<BlockList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: BlockCreate) -> Result<Block, RepoError> {
        let block = self.inner.create(input).await?;
        // Best-effort reindex — if the index update fails the
        // block still got created (source of truth is intact). The
        // index can be rebuilt later via `reindex_block`.
        if let Err(e) = crate::reindex::reindex_block(&self.crdt_doc, block.id).await {
            tracing::warn!(?e, %block.id, "block ref/prop reindex failed (post-create)");
        }
        Ok(block)
    }
    async fn update(&self, id: Uuid, input: BlockUpdate) -> Result<Block, RepoError> {
        let block = self.inner.update(id, input).await?;
        if let Err(e) = crate::reindex::reindex_block(&self.crdt_doc, block.id).await {
            tracing::warn!(?e, %block.id, "block ref/prop reindex failed (post-update)");
        }
        Ok(block)
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await?;
        if let Err(e) = crate::reindex::cascade_delete_block_edges(&self.crdt_doc, id).await {
            tracing::warn!(?e, %id, "edge cascade-delete failed");
        }
        Ok(())
    }
}
