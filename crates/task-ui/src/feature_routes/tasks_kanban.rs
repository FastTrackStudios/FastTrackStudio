//! Phase 6 demo route — wraps `knowledge_ui::TasksKanbanLive`.

use dioxus::prelude::*;
use knowledge_ui::TasksKanbanLive;

#[component]
pub fn TasksKanbanView() -> Element {
    rsx! {
        TasksKanbanLive {
            vox_url: crate::vox_session::vox_url(),
        }
    }
}
