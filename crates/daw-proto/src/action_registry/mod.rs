//! Action registry — types + events + service traits.

mod event;
mod service;
mod types;

pub use event::ActionEvent;
pub use service::{
    ActionRegistry, ActionRegistryService, ActionRegistryServiceClient,
    ActionRegistryServiceDispatcher, action_registry_service_service_descriptor,
};
pub use types::{
    ActionExecutionResult, ActionInfo, ActionListFilter, ActionListRequest, ActionListResponse,
    ActionOrigin, ActionSection,
};
