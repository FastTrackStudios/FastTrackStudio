//! Page-level actions. Mirrors `frontend.handler.page`.

use dioxus::prelude::*;
use uuid::Uuid;

use crate::db::new_page;
use crate::state::AppState;

/// Create a new top-level page and switch to it.
pub fn create_page(state: AppState, basename: String) -> Uuid {
    let page = new_page(basename);
    let id = page.id;
    let mut v = state.vault;
    v.write().pages.push(page);
    state.active_page.clone().set(Some(id));
    id
}

/// Switch the active page to the named id. No-op when the id
/// doesn't resolve.
pub fn open_page(state: AppState, page_id: Uuid) {
    if state.vault.read().page_by_id(page_id).is_some() {
        state.active_page.clone().set(Some(page_id));
    }
}
