//! Knowledge route — thin wrapper that hands the configured vox URL
//! to the knowledge-ui live component.

use dioxus::prelude::*;
use knowledge_ui::KnowledgeLive;

#[component]
pub fn KnowledgeView() -> Element {
    rsx! {
        KnowledgeLive {
            vox_url: crate::vox_session::vox_url(),
        }
    }
}
