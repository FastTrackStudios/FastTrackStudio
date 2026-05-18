//! Loro-backed `BlockRefEdgeRepo` — materialized index over
//! `Block.refs_json`. See `plans/logseq-data-model-alignment.md`.

use crdt::entity_crdt;
use knowledge_proto::{
    BlockRefEdge, BlockRefEdgeCreate, BlockRefEdgeList, BlockRefEdgeRepo, BlockRefEdgeUpdate,
};

entity_crdt! {
    pub BlockRefEdge,
    root = "knowledge_block_refs",
    fields {
        id: uuid (pk),
        vault_id: uuid,
        source_block_id: uuid,
        source_page_id: uuid,
        target_kind: str,
        target_str: str (sortable),
        target_uuid: opt_uuid,
        alias: opt_str,
    },
    audit_timestamps,
}
