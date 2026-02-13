//! Shared UI state/context for playground app.

use crate::example::auth::control::{AuthControl, Authenticated};
use crate::example::auth::live::AuthServiceLive;
use crate::example::items::{ControlApi, ItemInfo};
use dioxus::prelude::*;
use std::sync::Arc;

#[derive(Clone)]
pub struct AppUiContext {
    pub auth_service: Arc<AuthServiceLive>,
    pub item_control: ControlApi,
    pub auth_state: Signal<Option<AuthControl<Authenticated>>>,
    pub login_error: Signal<Option<String>>,
    pub items: Signal<Vec<ItemInfo>>,
}

impl AppUiContext {
    pub fn new(auth_service: Arc<AuthServiceLive>, item_control: ControlApi) -> Self {
        Self {
            auth_service,
            item_control,
            auth_state: Signal::new(None),
            login_error: Signal::new(None),
            items: Signal::new(Vec::new()),
        }
    }
}
