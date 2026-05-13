//! Cookbook feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! v1 scope: `Recipe` only. Cookbook / ingredients / steps / meal plans come later.

use cookbook_proto::{Recipe, RecipeCreate};
use dioxus::prelude::*;
use uuid::Uuid;

#[component]
pub fn RecipeList(items: Vec<Recipe>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No recipes yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for recipe in items.iter().cloned() {
                RecipeRow {
                    key: "{recipe.id}",
                    recipe: recipe.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn RecipeRow(recipe: Recipe, on_delete: EventHandler<Uuid>) -> Element {
    let id = recipe.id;
    let cuisine = recipe.cuisine.clone().unwrap_or_else(|| "recipe".into());
    let total = recipe
        .total_time_minutes
        .map(|m| format!("{}m", m))
        .unwrap_or_else(|| "—".into());
    let meta = format!("{} · {}", cuisine, total);
    let servings = recipe.servings;
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium text-slate-100", "{recipe.name}" }
                span { class: "text-xs text-slate-500", "{meta}" }
                if let Some(s) = servings {
                    span { class: "text-xs text-slate-500", "{s} servings" }
                }
            }
            button {
                class: "text-xs text-slate-500 hover:text-rose-400",
                onclick: move |_| on_delete.call(id),
                "Delete"
            }
        }
    }
}

#[component]
pub fn RecipeCreateForm(on_submit: EventHandler<RecipeCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut summary = use_signal(String::new);
    let mut servings = use_signal(String::new);
    let mut cuisine = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-col gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let n = name.read().clone();
                if n.trim().is_empty() {
                    return;
                }
                let servings_val: Option<u32> = {
                    let s = servings.read().clone();
                    let s = s.trim();
                    if s.is_empty() {
                        None
                    } else {
                        match s.parse() {
                            Ok(v) => Some(v),
                            Err(_) => return,
                        }
                    }
                };
                let payload = RecipeCreate {
                    cookbook_id: None,
                    name: n,
                    summary: trim_to_option(summary.read().clone()),
                    servings: servings_val,
                    prep_time_minutes: None,
                    cook_time_minutes: None,
                    total_time_minutes: None,
                    cuisine: trim_to_option(cuisine.read().clone()),
                    source_url: None,
                    image_url: None,
                    tags: Vec::new(),
                };
                on_submit.call(payload);
                name.set(String::new());
                summary.set(String::new());
                servings.set(String::new());
                cuisine.set(String::new());
            },
            div { class: "flex flex-wrap gap-2",
                input {
                    class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                    placeholder: "Name (required)",
                    value: "{name}",
                    oninput: move |evt| name.set(evt.value()),
                }
                input {
                    class: "w-28 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                    placeholder: "Servings",
                    r#type: "number",
                    value: "{servings}",
                    oninput: move |evt| servings.set(evt.value()),
                }
                input {
                    class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                    placeholder: "Cuisine",
                    value: "{cuisine}",
                    oninput: move |evt| cuisine.set(evt.value()),
                }
            }
            textarea {
                class: "w-full rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Summary",
                rows: 2,
                value: "{summary}",
                oninput: move |evt| summary.set(evt.value()),
            }
            div {
                button {
                    r#type: "submit",
                    class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                    "Add recipe"
                }
            }
        }
    }
}

fn trim_to_option(s: String) -> Option<String> {
    let t = s.trim();
    if t.is_empty() {
        None
    } else {
        Some(t.to_string())
    }
}
