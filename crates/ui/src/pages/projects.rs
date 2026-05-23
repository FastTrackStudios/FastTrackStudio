//! Project route — stub.
//!
//! The previous implementation lived in `project-ui`'s
//! `TasksByProjectLive` component, which mined task rows out of
//! knowledge-page entities. With the `knowledge` feature deleted
//! the original wiring broke; the crate is parked under
//! `crates/legacy/project-ui/`.
//!
//! Rebuild this against `project_crdt::TaskRepoLoro` +
//! `project_proto::TaskRepoClient` (project-feature-native, no
//! knowledge dep) when we're ready to bring the route back.

use dioxus::prelude::*;
use fts_ui::prelude::*;

#[component]
pub fn ProjectsView() -> Element {
    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-4 p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Projects" }
            Text {
                variant: TextVariant::Muted,
                "Coming soon — rebuilding against `project_crdt::TaskRepoLoro` after the knowledge-feature rip."
            }
        }
    }
}
