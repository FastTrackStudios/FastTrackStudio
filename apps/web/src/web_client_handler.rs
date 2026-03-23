//! Web client handler — receives pushed events from the desktop app.

use session::{SetlistEvent, WebClientService};

#[derive(Clone)]
pub struct WebClientHandler;

impl WebClientService for WebClientHandler {
    async fn push_event(&self, event: SetlistEvent) {
        session_ui::apply_setlist_event(&event);
    }
}
