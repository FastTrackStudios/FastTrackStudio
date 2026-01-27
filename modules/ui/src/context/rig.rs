//! Rig service context for dependency injection

use fts::rig::{LocalRigClient, MockRig};

crate::define_service_context! {
    /// Rig service context for dependency injection
    name: Rig,
    client_type: LocalRigClient<MockRig>,
}
