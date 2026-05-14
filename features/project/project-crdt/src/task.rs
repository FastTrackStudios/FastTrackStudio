//! Loro-backed `TaskRepo` impl. The `entity_crdt!` macro emits
//! the [`TaskEntity`] marker, the [`EntityCrdt`] impl, the
//! [`TaskRepoLoro`] newtype, and the `TaskRepo` forwarder.

use crdt::entity_crdt;
use project_proto::{Task, TaskCreate, TaskList, TaskRepo, TaskUpdate};

entity_crdt! {
    pub Task,
    root = "tasks",
    fields {
        id: uuid (pk),
        project_id: uuid,
        title: str (sortable),
        done: bool (sortable),
    },
}
