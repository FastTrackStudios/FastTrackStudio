//! Loro-backed `BaseRepo`. Emitted by `entity_crdt!`.

use crdt::entity_crdt;
use knowledge_proto::{Base, BaseCreate, BaseList, BaseRepo, BaseUpdate};

entity_crdt! {
    pub Base,
    root = "knowledge_bases",
    fields {
        id: uuid (pk),
        vault_id: uuid,
        page_id: opt_uuid,
        name: str (sortable),
        definition_yaml: str,
        parsed_filter_json: str,
        parsed_views_json: str,
    },
    audit_timestamps,
}
