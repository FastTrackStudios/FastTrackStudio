use dioxus::prelude::*;
use fts_ui::prelude::*;

#[component]
pub fn TasksView() -> Element {
    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-4 p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Tasks" }
            Text { variant: TextVariant::Muted,
                "Stub. Wires to `features/task` once its `task-ui` crate lands."
            }
        }
    }
}
