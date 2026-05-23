use dioxus::prelude::*;
use fts_ui::prelude::*;

#[component]
pub fn WikiView() -> Element {
    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-4 p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Wiki" }
            Text { variant: TextVariant::Muted,
                "Stub. Wires to `features/wiki` crates (wiki-extract / wiki-graph / wiki-live / wiki-search) once a `wiki-ui` crate lands."
            }
        }
    }
}
