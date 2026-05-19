//! Knowledge route — thin wrapper that hands the configured vox URL
//! to the knowledge-ui live component.

use dioxus::prelude::*;
use knowledge_ui::LogseqShell;

#[component]
pub fn KnowledgeView() -> Element {
    rsx! { LogseqShell {} }
}
