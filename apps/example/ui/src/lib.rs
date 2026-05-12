//! Runtime UI shell — composes feature-ui crates into the pages that
//! `apps/web` and `apps/desktop` mount.
//!
//! For the example, there's one feature (`example`). A real project
//! pulls in `timeline_ui::*`, `mixing_ui::*`, etc. here; the shell
//! handles routing, layout, and the WebSocket client lifecycle, then
//! delegates rendering to the feature components.

use dioxus::prelude::*;
use example_ui::{ExampleCreateForm, ExampleList};

#[component]
pub fn App() -> Element {
    // Placeholder data — wire to `ExampleRepoClient` on mount in the
    // next pass. The components are intentionally agnostic to where
    // the data came from, so we can render them with literals here
    // and stream live data in later without changing the JSX.
    let items = use_signal(Vec::new);

    rsx! {
        div { class: "app-shell",
            header { h1 { "architect" } }
            main {
                section { class: "panel",
                    h2 { "Examples" }
                    ExampleList { items: items() }
                }
                section { class: "panel",
                    h2 { "Add example" }
                    ExampleCreateForm {
                        on_submit: move |(_name, _description): (String, String)| {
                            // TODO: call ExampleRepoClient::create here.
                        }
                    }
                }
            }
        }
    }
}
