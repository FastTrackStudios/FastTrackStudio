//! Project feature route — minimal placeholder during the
//! vertical-slice reset. Live Loro wiring is rebuilt in Phase D
//! (vox transport).

use dioxus::prelude::*;
use fts_ui::prelude::*;

#[component]
pub fn ProjectView() -> Element {
    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-4 p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Projects" }
            Text { variant: TextVariant::Muted,
                "Vertical-slice reset in progress. Project + task views will be wired through vox in Phase D."
            }
        }
    }
}
