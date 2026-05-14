//! `Project` wire entity. The container that holds tasks.

use architect::Entity;
use facet::Facet;
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(fake::Dummy))]
#[derive(Entity, Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "projects", repo)]
pub struct Project {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "fake::ProjectName"))]
    pub name: String,
}

#[cfg(feature = "fake")]
mod fake {
    pub use ::fake::Dummy;
    use ::fake::rand::{Rng, seq::IndexedRandom};

    /// `#[dummy(faker = "ProjectName")]` source — picks from a
    /// fixed pool. Pure presentation; no runtime dep.
    pub struct ProjectName;

    impl Dummy<ProjectName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ProjectName, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "Q4 Launch",
                "Mobile App v2",
                "Customer Onboarding",
                "Analytics Pipeline",
                "Documentation Refresh",
                "Holiday Campaign",
            ];
            (*POOL.choose(rng).unwrap()).to_string()
        }
    }
}
