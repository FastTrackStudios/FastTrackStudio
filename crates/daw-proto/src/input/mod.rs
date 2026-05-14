//! Input — keyboard/mouse event interception and streaming.

mod service;
mod types;

pub use service::{
    InputService, InputServiceClient, InputServiceDispatcher, input_service_service_descriptor,
};
pub use types::{
    InputContext, InputEvent, KeyCode, KeyEvent, KeyFilter, KeyModifiers, KeyMsgKind, KeyPattern,
};
