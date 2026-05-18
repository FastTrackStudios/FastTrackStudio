//! Loro-backed `PagePropEdgeRepo` — Tier 2 materialized index
//! over `Page.frontmatter_json`. See
//! `plans/logseq-data-model-alignment.md`.

use crdt::entity_crdt;
use knowledge_proto::{
    PagePropEdge, PagePropEdgeCreate, PagePropEdgeList, PagePropEdgeRepo, PagePropEdgeUpdate,
};

entity_crdt! {
    pub PagePropEdge,
    root = "knowledge_page_props",
    fields {
        id: uuid (pk),
        vault_id: uuid,
        page_id: uuid,
        key: str (sortable),
        value_json: str,
        value_type: str,
    },
    audit_timestamps,
}
