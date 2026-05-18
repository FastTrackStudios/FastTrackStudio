//! Loro-backed `BlockPropEdgeRepo` — materialized index over
//! `Block.properties_json`. See
//! `plans/logseq-data-model-alignment.md`.

use crdt::entity_crdt;
use knowledge_proto::{
    BlockPropEdge, BlockPropEdgeCreate, BlockPropEdgeList, BlockPropEdgeRepo, BlockPropEdgeUpdate,
};

entity_crdt! {
    pub BlockPropEdge,
    root = "knowledge_block_props",
    fields {
        id: uuid (pk),
        vault_id: uuid,
        block_id: uuid,
        page_id: uuid,
        key: str (sortable),
        value_json: str,
        value_type: str,
    },
    audit_timestamps,
}
