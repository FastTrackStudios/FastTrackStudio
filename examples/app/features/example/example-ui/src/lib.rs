//! Example feature UI — components scoped to the `example` feature.
//!
//! Renders + drives the wire types from `example_proto` against the
//! auto-generated `ExampleRepoClient`. Backend-agnostic: works the same
//! whether the server's pulling from sqlite or in-memory storage.
//!
//! `apps/ui` composes these components into the runtime shell that
//! `apps/web` and `apps/desktop` mount.

use dioxus::prelude::*;
use example::Example;

/// Renders a list of examples. Caller fetches the data and hands it in;
/// keeps this component testable in isolation without an open socket.
#[component]
pub fn ExampleList(items: Vec<Example>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "example-list empty",
                p { "No examples yet." }
            }
        };
    }
    rsx! {
        ul { class: "example-list",
            for ex in items {
                ExampleRow { key: "{ex.id}", example: ex }
            }
        }
    }
}

/// Single-row presentation. Composable into list views, search results,
/// detail panes, etc.
#[component]
pub fn ExampleRow(example: Example) -> Element {
    rsx! {
        li { class: "example-row",
            span { class: "example-row__name", "{example.name}" }
            span { class: "example-row__description", "{example.description}" }
        }
    }
}

/// Minimal create form. Emits the input pair via `on_submit`; the
/// caller decides whether to call `ExampleRepoClient::create` or stub
/// it for tests/storybook.
#[component]
pub fn ExampleCreateForm(on_submit: EventHandler<(String, String)>) -> Element {
    let mut name = use_signal(String::new);
    let mut description = use_signal(String::new);
    rsx! {
        form {
            class: "example-form",
            onsubmit: move |evt| {
                evt.prevent_default();
                on_submit.call((name(), description()));
                name.set(String::new());
                description.set(String::new());
            },
            input {
                placeholder: "name",
                value: "{name}",
                oninput: move |evt| name.set(evt.value()),
            }
            input {
                placeholder: "description",
                value: "{description}",
                oninput: move |evt| description.set(evt.value()),
            }
            button { r#type: "submit", "Add" }
        }
    }
}

/// Edit form, pre-filled from an existing row. Emits the edited pair via
/// `on_submit`; the caller calls `ExampleRepoClient::update`. Mounted
/// fresh per detail route, so seeding the signals from the props once is
/// the intended behaviour.
#[component]
pub fn ExampleEditForm(
    initial_name: String,
    initial_description: String,
    on_submit: EventHandler<(String, String)>,
) -> Element {
    let mut name = use_signal(|| initial_name.clone());
    let mut description = use_signal(|| initial_description.clone());
    rsx! {
        form {
            class: "example-form",
            onsubmit: move |evt| {
                evt.prevent_default();
                on_submit.call((name(), description()));
            },
            input {
                placeholder: "name",
                value: "{name}",
                oninput: move |evt| name.set(evt.value()),
            }
            input {
                placeholder: "description",
                value: "{description}",
                oninput: move |evt| description.set(evt.value()),
            }
            button { r#type: "submit", "Save" }
        }
    }
}

/// Search box. Emits the query string via `on_search` on submit; emits
/// an empty string when cleared so the caller can fall back to the full
/// list. Backend-agnostic: the caller routes the query to
/// `ExampleServiceClient::search`.
#[component]
pub fn SearchBar(on_search: EventHandler<String>) -> Element {
    let mut query = use_signal(String::new);
    rsx! {
        form {
            class: "search-bar",
            onsubmit: move |evt| {
                evt.prevent_default();
                on_search.call(query());
            },
            input {
                r#type: "search",
                placeholder: "search name or description…",
                value: "{query}",
                oninput: move |evt| {
                    let v = evt.value();
                    query.set(v.clone());
                    if v.is_empty() {
                        on_search.call(String::new());
                    }
                },
            }
            button { r#type: "submit", "Search" }
        }
    }
}
