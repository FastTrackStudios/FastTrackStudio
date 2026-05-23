use dioxus::prelude::*;
use fts_ui::prelude::*;

#[component]
pub fn SettingsView() -> Element {
    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-2 p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Settings" }
            Text { variant: TextVariant::Muted, "Not implemented yet." }
        }
    }
}
