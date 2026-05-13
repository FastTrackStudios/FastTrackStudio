//! Loro-backed source-of-truth for the knowledge feature.
//!
//! Six entities, six `EntityCrdt` impls, six `*RepoLoro` newtypes.
//! Roots are `knowledge_*` prefixed to keep them out of the way of
//! any other feature's root names.
//!
//! `Block.content` is stored as a plain Loro string (LWW per write).
//! FUTURE: upgrade `Block.content` to `LoroText` for character-level
//! CRDT merges. Currently full-string LWW per write, which is fine
//! for the single-block-at-a-time edit pattern the live-preview
//! editor uses; concurrent in-block edits will need the upgrade.

use architect::{Page as PageWindow, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_i64, read_opt_str, read_opt_uuid, read_str, read_string_list,
    read_uuid, write_bool, write_dt, write_i64, write_opt_str, write_opt_string_list, write_str,
    write_string_list, write_uuid,
};
use knowledge_proto::{
    Base, BaseCreate, BaseList, BaseRepo, BaseUpdate, Block, BlockCreate, BlockList, BlockRepo,
    BlockUpdate, Folder, FolderCreate, FolderList, FolderRepo, FolderUpdate, KnowledgeTag,
    KnowledgeTagCreate, KnowledgeTagList, KnowledgeTagRepo, KnowledgeTagUpdate, Page, PageCreate,
    PageList, PageRepo, PageUpdate, Vault, VaultCreate, VaultList, VaultRepo, VaultUpdate,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

// Helper: codec for `Option<Uuid>` write where the Update wrapper is
// `Option<Option<Uuid>>` and we want "Some(None)" to clear the value.
use crdt::codec::write_opt_uuid;

// Internal helper: handle `Option<i32>` via `write_i64`. Architect
// emits Update fields as `Option<Option<T>>`; the outer `Option`
// means "field is being touched", and we collapse to i64.
fn write_opt_i32(m: &LoroMap, k: &str, v: Option<i32>) -> Result<(), RepoError> {
    match v {
        Some(x) => write_i64(m, k, x as i64),
        None => write_i64(m, k, 0),
    }
}
fn read_opt_i32(m: &LoroMap, k: &str) -> Result<Option<i32>, RepoError> {
    // We use sentinel 0 for "absent" since architect's `Option<i32>`
    // path doesn't have a built-in codec helper; -1 would collide
    // with valid heading levels. For Phase A this works because the
    // only `Option<i32>` field is `Block.heading_level`, which is
    // always >0 when set. The decoder treats 0 as `None`.
    let v = read_i64(m, k)?;
    Ok(if v == 0 { None } else { Some(v as i32) })
}

// ── Vault ─────────────────────────────────────────────────────────────

pub struct VaultEntity;

#[derive(Clone)]
pub struct VaultRepoLoro {
    inner: LoroRepo<VaultEntity>,
}

impl VaultRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<VaultEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for VaultEntity {
    type Wire = Vault;
    type Create = VaultCreate;
    type Update = VaultUpdate;
    type List = VaultList;

    const ROOT: &'static str = "knowledge_vaults";

    fn id(w: &Vault) -> Uuid {
        w.id
    }

    fn from_create(input: VaultCreate) -> Vault {
        let now = Utc::now();
        Vault {
            id: Uuid::new_v4(),
            name: input.name,
            root_path: input.root_path,
            use_markdown_links: input.use_markdown_links,
            new_link_format: input.new_link_format,
            attachment_folder_path: input.attachment_folder_path,
            default_view_mode: input.default_view_mode,
            config_json: input.config_json,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Vault) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "root_path", e.root_path.as_deref())?;
        write_bool(m, "use_markdown_links", e.use_markdown_links)?;
        write_str(m, "new_link_format", &e.new_link_format)?;
        write_str(m, "attachment_folder_path", &e.attachment_folder_path)?;
        write_str(m, "default_view_mode", &e.default_view_mode)?;
        write_str(m, "config_json", &e.config_json)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Vault, RepoError> {
        Ok(Vault {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            root_path: read_opt_str(m, "root_path")?,
            use_markdown_links: read_bool(m, "use_markdown_links")?,
            new_link_format: read_str(m, "new_link_format")?,
            attachment_folder_path: read_str(m, "attachment_folder_path")?,
            default_view_mode: read_str(m, "default_view_mode")?,
            config_json: read_str(m, "config_json")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: VaultUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.root_path {
            write_opt_str(m, "root_path", v.as_deref())?;
        }
        if let Some(v) = u.use_markdown_links {
            write_bool(m, "use_markdown_links", v)?;
        }
        if let Some(v) = u.new_link_format {
            write_str(m, "new_link_format", &v)?;
        }
        if let Some(v) = u.attachment_folder_path {
            write_str(m, "attachment_folder_path", &v)?;
        }
        if let Some(v) = u.default_view_mode {
            write_str(m, "default_view_mode", &v)?;
        }
        if let Some(v) = u.config_json {
            write_str(m, "config_json", &v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Vault], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
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

    fn build_list(items: Vec<Vault>, total: u32, page: PageWindow) -> VaultList {
        VaultList { items, total, page }
    }
}

impl VaultRepo for VaultRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Vault, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<VaultList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: VaultCreate) -> Result<Vault, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: VaultUpdate) -> Result<Vault, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Folder ────────────────────────────────────────────────────────────

pub struct FolderEntity;

#[derive(Clone)]
pub struct FolderRepoLoro {
    inner: LoroRepo<FolderEntity>,
}

impl FolderRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<FolderEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for FolderEntity {
    type Wire = Folder;
    type Create = FolderCreate;
    type Update = FolderUpdate;
    type List = FolderList;

    const ROOT: &'static str = "knowledge_folders";

    fn id(w: &Folder) -> Uuid {
        w.id
    }

    fn from_create(input: FolderCreate) -> Folder {
        let now = Utc::now();
        Folder {
            id: Uuid::new_v4(),
            vault_id: input.vault_id,
            path: input.path,
            parent_id: input.parent_id,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Folder) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "vault_id", e.vault_id)?;
        write_str(m, "path", &e.path)?;
        write_opt_uuid(m, "parent_id", e.parent_id)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Folder, RepoError> {
        Ok(Folder {
            id: read_uuid(m, "id")?,
            vault_id: read_uuid(m, "vault_id")?,
            path: read_str(m, "path")?,
            parent_id: read_opt_uuid(m, "parent_id")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: FolderUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.vault_id {
            write_uuid(m, "vault_id", v)?;
        }
        if let Some(v) = u.path {
            write_str(m, "path", &v)?;
        }
        if let Some(v) = u.parent_id {
            write_opt_uuid(m, "parent_id", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Folder], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "path" => items.sort_by(|a, b| a.path.cmp(&b.path)),
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

    fn build_list(items: Vec<Folder>, total: u32, page: PageWindow) -> FolderList {
        FolderList { items, total, page }
    }
}

impl FolderRepo for FolderRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Folder, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<FolderList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: FolderCreate) -> Result<Folder, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: FolderUpdate) -> Result<Folder, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Page ──────────────────────────────────────────────────────────────

pub struct PageEntity;

#[derive(Clone)]
pub struct PageRepoLoro {
    inner: LoroRepo<PageEntity>,
}

impl PageRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<PageEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for PageEntity {
    type Wire = Page;
    type Create = PageCreate;
    type Update = PageUpdate;
    type List = PageList;

    const ROOT: &'static str = "knowledge_pages";

    fn id(w: &Page) -> Uuid {
        w.id
    }

    fn from_create(input: PageCreate) -> Page {
        let now = Utc::now();
        Page {
            id: Uuid::new_v4(),
            vault_id: input.vault_id,
            folder_id: input.folder_id,
            path: input.path,
            basename: input.basename,
            ext: input.ext,
            aliases: input.aliases,
            frontmatter_json: input.frontmatter_json,
            stat_ctime: input.stat_ctime,
            stat_mtime: input.stat_mtime,
            stat_size: input.stat_size,
            is_journal: input.is_journal,
            journal_day: input.journal_day,
            shadow_for_kind: input.shadow_for_kind,
            shadow_for_id: input.shadow_for_id,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Page) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "vault_id", e.vault_id)?;
        write_opt_uuid(m, "folder_id", e.folder_id)?;
        write_str(m, "path", &e.path)?;
        write_str(m, "basename", &e.basename)?;
        write_str(m, "ext", &e.ext)?;
        write_string_list(m, "aliases", &e.aliases)?;
        write_str(m, "frontmatter_json", &e.frontmatter_json)?;
        write_dt(m, "stat_ctime", e.stat_ctime)?;
        write_dt(m, "stat_mtime", e.stat_mtime)?;
        write_i64(m, "stat_size", e.stat_size)?;
        write_bool(m, "is_journal", e.is_journal)?;
        write_opt_str(m, "journal_day", e.journal_day.as_deref())?;
        write_opt_str(m, "shadow_for_kind", e.shadow_for_kind.as_deref())?;
        write_opt_uuid(m, "shadow_for_id", e.shadow_for_id)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Page, RepoError> {
        Ok(Page {
            id: read_uuid(m, "id")?,
            vault_id: read_uuid(m, "vault_id")?,
            folder_id: read_opt_uuid(m, "folder_id")?,
            path: read_str(m, "path")?,
            basename: read_str(m, "basename")?,
            ext: read_str(m, "ext")?,
            aliases: read_string_list(m, "aliases")?,
            frontmatter_json: read_str(m, "frontmatter_json")?,
            stat_ctime: read_dt(m, "stat_ctime")?,
            stat_mtime: read_dt(m, "stat_mtime")?,
            stat_size: read_i64(m, "stat_size")?,
            is_journal: read_bool(m, "is_journal")?,
            journal_day: read_opt_str(m, "journal_day")?,
            shadow_for_kind: read_opt_str(m, "shadow_for_kind")?,
            shadow_for_id: read_opt_uuid(m, "shadow_for_id")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: PageUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.vault_id {
            write_uuid(m, "vault_id", v)?;
        }
        if let Some(v) = u.folder_id {
            write_opt_uuid(m, "folder_id", v)?;
        }
        if let Some(v) = u.path {
            write_str(m, "path", &v)?;
        }
        if let Some(v) = u.basename {
            write_str(m, "basename", &v)?;
        }
        if let Some(v) = u.ext {
            write_str(m, "ext", &v)?;
        }
        if let Some(v) = u.aliases {
            write_opt_string_list(m, "aliases", Some(&v))?;
        }
        if let Some(v) = u.frontmatter_json {
            write_str(m, "frontmatter_json", &v)?;
        }
        if let Some(v) = u.stat_ctime {
            write_dt(m, "stat_ctime", v)?;
        }
        if let Some(v) = u.stat_mtime {
            write_dt(m, "stat_mtime", v)?;
        }
        if let Some(v) = u.stat_size {
            write_i64(m, "stat_size", v)?;
        }
        if let Some(v) = u.is_journal {
            write_bool(m, "is_journal", v)?;
        }
        if let Some(v) = u.journal_day {
            write_opt_str(m, "journal_day", v.as_deref())?;
        }
        if let Some(v) = u.shadow_for_kind {
            write_opt_str(m, "shadow_for_kind", v.as_deref())?;
        }
        if let Some(v) = u.shadow_for_id {
            write_opt_uuid(m, "shadow_for_id", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Page], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "basename" => items.sort_by(|a, b| a.basename.cmp(&b.basename)),
            "path" => items.sort_by(|a, b| a.path.cmp(&b.path)),
            "stat_mtime" => items.sort_by(|a, b| a.stat_mtime.cmp(&b.stat_mtime)),
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

    fn build_list(items: Vec<Page>, total: u32, page: PageWindow) -> PageList {
        PageList { items, total, page }
    }
}

impl PageRepo for PageRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Page, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<PageList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: PageCreate) -> Result<Page, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: PageUpdate) -> Result<Page, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Block ─────────────────────────────────────────────────────────────

pub struct BlockEntity;

#[derive(Clone)]
pub struct BlockRepoLoro {
    inner: LoroRepo<BlockEntity>,
}

impl BlockRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<BlockEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
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
        // FUTURE: upgrade this to LoroText for character-level merge.
        write_str(m, "content", &e.content)?;
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
            content: read_str(m, "content")?,
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
            // Fallback to None for pre-A.5 snapshots so old data decodes.
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
            write_str(m, "content", &v)?;
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
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: BlockUpdate) -> Result<Block, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── KnowledgeTag ──────────────────────────────────────────────────────

pub struct KnowledgeTagEntity;

#[derive(Clone)]
pub struct KnowledgeTagRepoLoro {
    inner: LoroRepo<KnowledgeTagEntity>,
}

impl KnowledgeTagRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<KnowledgeTagEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for KnowledgeTagEntity {
    type Wire = KnowledgeTag;
    type Create = KnowledgeTagCreate;
    type Update = KnowledgeTagUpdate;
    type List = KnowledgeTagList;

    const ROOT: &'static str = "knowledge_tags";

    fn id(w: &KnowledgeTag) -> Uuid {
        w.id
    }

    fn from_create(input: KnowledgeTagCreate) -> KnowledgeTag {
        let now = Utc::now();
        KnowledgeTag {
            id: Uuid::new_v4(),
            vault_id: input.vault_id,
            tag: input.tag,
            color: input.color,
            description: input.description,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &KnowledgeTag) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "vault_id", e.vault_id)?;
        write_str(m, "tag", &e.tag)?;
        write_opt_str(m, "color", e.color.as_deref())?;
        write_opt_str(m, "description", e.description.as_deref())?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<KnowledgeTag, RepoError> {
        Ok(KnowledgeTag {
            id: read_uuid(m, "id")?,
            vault_id: read_uuid(m, "vault_id")?,
            tag: read_str(m, "tag")?,
            color: read_opt_str(m, "color")?,
            description: read_opt_str(m, "description")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: KnowledgeTagUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.vault_id {
            write_uuid(m, "vault_id", v)?;
        }
        if let Some(v) = u.tag {
            write_str(m, "tag", &v)?;
        }
        if let Some(v) = u.color {
            write_opt_str(m, "color", v.as_deref())?;
        }
        if let Some(v) = u.description {
            write_opt_str(m, "description", v.as_deref())?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [KnowledgeTag],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "tag" => items.sort_by(|a, b| a.tag.cmp(&b.tag)),
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

    fn build_list(items: Vec<KnowledgeTag>, total: u32, page: PageWindow) -> KnowledgeTagList {
        KnowledgeTagList { items, total, page }
    }
}

impl KnowledgeTagRepo for KnowledgeTagRepoLoro {
    async fn get(&self, id: Uuid) -> Result<KnowledgeTag, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<KnowledgeTagList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: KnowledgeTagCreate) -> Result<KnowledgeTag, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: KnowledgeTagUpdate) -> Result<KnowledgeTag, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Base ──────────────────────────────────────────────────────────────

pub struct BaseEntity;

#[derive(Clone)]
pub struct BaseRepoLoro {
    inner: LoroRepo<BaseEntity>,
}

impl BaseRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<BaseEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for BaseEntity {
    type Wire = Base;
    type Create = BaseCreate;
    type Update = BaseUpdate;
    type List = BaseList;

    const ROOT: &'static str = "knowledge_bases";

    fn id(w: &Base) -> Uuid {
        w.id
    }

    fn from_create(input: BaseCreate) -> Base {
        let now = Utc::now();
        Base {
            id: Uuid::new_v4(),
            vault_id: input.vault_id,
            page_id: input.page_id,
            name: input.name,
            definition_yaml: input.definition_yaml,
            parsed_filter_json: input.parsed_filter_json,
            parsed_views_json: input.parsed_views_json,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Base) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "vault_id", e.vault_id)?;
        write_opt_uuid(m, "page_id", e.page_id)?;
        write_str(m, "name", &e.name)?;
        write_str(m, "definition_yaml", &e.definition_yaml)?;
        write_str(m, "parsed_filter_json", &e.parsed_filter_json)?;
        write_str(m, "parsed_views_json", &e.parsed_views_json)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Base, RepoError> {
        Ok(Base {
            id: read_uuid(m, "id")?,
            vault_id: read_uuid(m, "vault_id")?,
            page_id: read_opt_uuid(m, "page_id")?,
            name: read_str(m, "name")?,
            definition_yaml: read_str(m, "definition_yaml")?,
            parsed_filter_json: read_str(m, "parsed_filter_json")?,
            parsed_views_json: read_str(m, "parsed_views_json")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: BaseUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.vault_id {
            write_uuid(m, "vault_id", v)?;
        }
        if let Some(v) = u.page_id {
            write_opt_uuid(m, "page_id", v)?;
        }
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.definition_yaml {
            write_str(m, "definition_yaml", &v)?;
        }
        if let Some(v) = u.parsed_filter_json {
            write_str(m, "parsed_filter_json", &v)?;
        }
        if let Some(v) = u.parsed_views_json {
            write_str(m, "parsed_views_json", &v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Base], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
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

    fn build_list(items: Vec<Base>, total: u32, page: PageWindow) -> BaseList {
        BaseList { items, total, page }
    }
}

impl BaseRepo for BaseRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Base, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<BaseList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: BaseCreate) -> Result<Base, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: BaseUpdate) -> Result<Base, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
