//! Loro-backed `ProjectRepo` impl. The `entity_crdt!` macro emits
//! the [`ProjectEntity`] marker, the [`EntityCrdt`] impl, the
//! [`ProjectRepoLoro`] newtype, and the `ProjectRepo` forwarder.

use crdt::entity_crdt;
use project_proto::{Project, ProjectCreate, ProjectList, ProjectRepo, ProjectUpdate};

entity_crdt! {
    pub Project,
    root = "projects",
    fields {
        id: uuid (pk),
        name: str (sortable),
    },
}
