//! Action registration — types + events + service trait.

mod event;
mod service;
mod types;

pub use event::ActionEvent;
pub use service::{ActionRegistration, ActionRegistrationRpc};
pub use types::{
    ActionExecutionResult, ActionInfo, ActionListFilter, ActionListRequest, ActionListResponse,
    ActionOrigin, ActionSection,
};

#[cfg(feature = "vox")]
pub use service::{ActionRegistrationClient, Dispatcher, descriptor, serve};
