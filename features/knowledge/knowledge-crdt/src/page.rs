//! Loro-backed `PageRepo`. Emitted by `entity_crdt!`.

use crdt::entity_crdt;
use knowledge_proto::{Page, PageCreate, PageList, PageRepo, PageUpdate};

entity_crdt! {
    pub Page,
    root = "knowledge_pages",
    fields {
        id: uuid (pk),
        vault_id: uuid,
        folder_id: opt_uuid,
        path: str,
        basename: str (sortable),
        ext: str,
        aliases: string_list,
        frontmatter_json: str,
        stat_ctime: dt,
        stat_mtime: dt,
        stat_size: i64,
        is_journal: bool,
        journal_day: opt_str,
        shadow_for_kind: opt_str,
        shadow_for_id: opt_uuid,
    },
    audit_timestamps,
}
