use dioxus::prelude::*;
use fts_ui::prelude::*;

#[component]
pub fn HomeView() -> Element {
    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-4 p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Home" }
            Text { variant: TextVariant::Muted,
                "Local-first command center. Use the sidebar to jump into Projects, Tasks, Vault, Schedule, or Wiki."
            }
        }
    }
}
