//! UI service — dialogs and refresh control.

mod service;
mod types;

pub use service::{UiService, UiServiceClient, UiServiceDispatcher, ui_service_service_descriptor};
pub use types::UserInputResult;
