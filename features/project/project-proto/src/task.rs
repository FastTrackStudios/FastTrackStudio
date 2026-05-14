//! `Task` wire entity. Bound to a `Project` by `project_id`. `done`
//! is the only state field for now — the slice exists to verify
//! sync, not to model rich task management.

use architect::Entity;
use facet::Facet;
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(fake::Dummy))]
#[derive(Entity, Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "tasks", repo)]
pub struct Task {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub project_id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "fake::TaskTitle"))]
    pub title: String,

    #[architect(filterable, sortable)]
    pub done: bool,
}

#[cfg(feature = "fake")]
mod fake {
    pub use ::fake::Dummy;
    use ::fake::rand::{Rng, seq::IndexedRandom};

    /// `#[dummy(faker = "TaskTitle")]` source — picks a "verb noun"
    /// pair from fixed pools.
    pub struct TaskTitle;

    impl Dummy<TaskTitle> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &TaskTitle, rng: &mut R) -> Self {
            const VERBS: &[&str] = &[
                "Investigate",
                "Refactor",
                "Document",
                "Deploy",
                "Test",
                "Migrate",
                "Review",
            ];
            const NOUNS: &[&str] = &[
                "login flow",
                "search results page",
                "settings panel",
                "API rate limiter",
                "background job queue",
                "notification system",
                "user permissions",
            ];
            let verb = *VERBS.choose(rng).unwrap();
            let noun = *NOUNS.choose(rng).unwrap();
            format!("{verb} {noun}")
        }
    }
}
