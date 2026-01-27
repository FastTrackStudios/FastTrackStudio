//! Block service context for dependency injection

use fts::rig::{LocalBlockClient, MockBlockService};

crate::define_service_context! {
    /// Block service context for dependency injection
    name: Block,
    client_type: LocalBlockClient<MockBlockService>,
}
