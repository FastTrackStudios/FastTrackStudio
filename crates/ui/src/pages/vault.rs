use dioxus::prelude::*;
use fts_ui::prelude::*;

#[component]
pub fn VaultView() -> Element {
    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-4 p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Vault" }
            Text { variant: TextVariant::Muted,
                "Stub. Wires to `features/vault` + `vault-live` once a `vault-ui` crate lands."
            }
        }
    }
}
