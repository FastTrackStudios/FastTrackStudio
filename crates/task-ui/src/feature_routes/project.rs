//! Project route — thin wrapper that hands the configured vox URL to
//! the project-ui live component. All fetch + render logic lives in
//! `project-ui` (`features/project/project-ui/src/live.rs`).

use dioxus::prelude::*;
use project_ui::TasksByProjectLive;

#[component]
pub fn ProjectView() -> Element {
    rsx! {
        TasksByProjectLive {
            vox_url: crate::vox_session::vox_url(),
        }
    }
}
