//! Rig service context for dependency injection

use rig_control::{LocalRigControlClient, MockRigControlService};

crate::define_service_context! {
    /// Rig service context for dependency injection
    name: Rig,
    client_type: LocalRigControlClient<MockRigControlService>,
}
