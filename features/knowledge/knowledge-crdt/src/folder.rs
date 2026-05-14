//! Loro-backed `FolderRepo`. Emitted by `entity_crdt!`.

use crdt::entity_crdt;
use knowledge_proto::{Folder, FolderCreate, FolderList, FolderRepo, FolderUpdate};

entity_crdt! {
    pub Folder,
    root = "knowledge_folders",
    fields {
        id: uuid (pk),
        vault_id: uuid,
        path: str (sortable),
        parent_id: opt_uuid,
    },
    audit_timestamps,
}
