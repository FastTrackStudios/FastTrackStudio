//! Loro-backed `KnowledgeTagRepo`. Emitted by `entity_crdt!`.

use crdt::entity_crdt;
use knowledge_proto::{
    KnowledgeTag, KnowledgeTagCreate, KnowledgeTagList, KnowledgeTagRepo, KnowledgeTagUpdate,
};

entity_crdt! {
    pub KnowledgeTag,
    root = "knowledge_tags",
    fields {
        id: uuid (pk),
        vault_id: uuid,
        tag: str (sortable),
        color: opt_str,
        description: opt_str,
    },
    audit_timestamps,
}
