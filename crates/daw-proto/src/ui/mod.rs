//! UI — dialogs and refresh control.

mod service;
mod types;

pub use service::{UiDialogs, UiDialogsRpc};
pub use types::UserInputResult;

#[cfg(feature = "vox")]
pub use service::{Dispatcher, Service, UiDialogsClient, descriptor, layer, serve};
