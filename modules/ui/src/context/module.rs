//! Module service context for dependency injection

use fts::rig::{LocalModuleClient, MockModuleService};

crate::define_service_context! {
    /// Module service context for dependency injection
    name: Module,
    client_type: LocalModuleClient<MockModuleService>,
}
