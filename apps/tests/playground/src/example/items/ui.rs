//! Items UI — CRUD components for authenticated users

use super::control::ItemName;
use super::proto::{ItemEvent, ItemInfo};
use crate::example::ui_state::AppUiContext;
use dioxus::prelude::*;

pub fn start_subscription(app: AppUiContext) {
    let Some(auth) = app.auth_state.read().as_ref().cloned() else {
        let mut items = app.items;
        items.set(Vec::new());
        return;
    };

    let token = auth.session().clone();
    let token_key = token.token.clone();
    let ctl = app.item_control.clone();
    let items = app.items;
    let auth_state = app.auth_state;

    spawn(async move {
        if let Ok(mut rx) = ctl.subscribe(&token).await {
            loop {
                match rx.recv().await {
                    Ok(Some(ItemEvent::ListChanged { items: next })) => {
                        let current_token = auth_state
                            .read()
                            .as_ref()
                            .map(|a| a.session().token.clone());
                        if current_token.as_deref() != Some(token_key.as_str()) {
                            break;
                        }
                        let mut items = items;
                        items.set(next);
                    }
                    Ok(None) => break,
                    Err(_) => continue,
                }
            }
        }
    });
}

pub fn clear_items(app: AppUiContext) {
    let mut items = app.items;
    items.set(Vec::new());
}

#[component]
pub fn AuthenticatedApp() -> Element {
    let app = use_context::<AppUiContext>();
    let auth = app.auth_state.read();
    let Some(ctl) = auth.as_ref() else {
        return rsx! {};
    };
    let user_name = ctl.user_name().to_string();
    let user_email = ctl.user_email().to_string();

    rsx! {
        div { class: "max-w-2xl mx-auto px-4 py-8",
            div { class: "flex items-center justify-between mb-6",
                div {
                    h1 { class: "text-2xl font-bold text-zinc-100", "Items" }
                    p { class: "text-sm text-zinc-500",
                        "Signed in as "
                        span { class: "text-zinc-300 font-medium", "{user_name}" }
                        span { class: "text-zinc-600 ml-1", "({user_email})" }
                    }
                }
                crate::example::auth::ui::SignOutButton {}
            }

            CreateForm {}
            ItemList {}

            div { class: "mt-8 pt-4 border-t border-zinc-800",
                p { class: "text-xs text-zinc-600 font-mono",
                    "AuthControl<Authenticated> -- items feature unlocked"
                }
                p { class: "text-xs text-zinc-700 mt-1",
                    "Auth: better-auth-rs | Items: SeaORM + ItemRepo | Direct trait calls with empty Context"
                }
            }
        }
    }
}

#[component]
fn CreateForm() -> Element {
    let app = use_context::<AppUiContext>();
    let mut input = use_signal(String::new);

    let mut submit = move || {
        let raw_name = ItemName::new(input.read().clone());
        let Ok(name) = raw_name.validate() else {
            return;
        };
        input.set(String::new());

        let Some(auth) = app.auth_state.read().as_ref().cloned() else {
            return;
        };
        let token = auth.session().clone();
        let ctl = app.item_control.clone();
        tracing::debug!("Creating item: {}", name.as_str());
        spawn(async move {
            ctl.create(&token, name).await;
        });
    };

    rsx! {
        div { class: "flex gap-2 mb-6",
            input {
                class: "flex-1 px-3 py-2 bg-zinc-900 border border-zinc-700 rounded-md text-zinc-100 placeholder-zinc-500 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent",
                r#type: "text",
                placeholder: "Add an item...",
                value: "{input}",
                oninput: move |e| input.set(e.value()),
            }
            button {
                class: "px-4 py-2 bg-blue-600 hover:bg-blue-500 text-white font-medium rounded-md transition-colors",
                onclick: move |_| submit(),
                "Add"
            }
        }
    }
}

#[component]
fn ItemList() -> Element {
    let app = use_context::<AppUiContext>();
    let items = app.items.read();

    if items.is_empty() {
        return rsx! {
            div { class: "flex flex-col items-center justify-center py-16 text-zinc-600",
                p { class: "text-lg mb-1", "No items yet" }
                p { class: "text-sm", "Add one above to get started." }
            }
        };
    }

    rsx! {
        div { class: "space-y-1",
            for item in items.iter() {
                ItemRow { key: "{item.id}", item: item.clone() }
            }
        }
        div { class: "mt-4 text-right text-xs text-zinc-600",
            "{items.len()} items -- {items.iter().filter(|i| i.done).count()} done"
        }
    }
}

#[component]
fn ItemRow(item: ItemInfo) -> Element {
    let app = use_context::<AppUiContext>();
    let id = item.id.clone();
    let id_del = item.id.clone();

    let app_toggle = app.clone();
    let on_toggle = move |_| {
        let id = id.clone();
        let Some(auth) = app_toggle.auth_state.read().as_ref().cloned() else {
            return;
        };
        let token = auth.session().clone();
        let ctl = app_toggle.item_control.clone();
        spawn(async move { ctl.toggle(&token, &id).await });
    };

    let app_delete = app.clone();
    let on_delete = move |_| {
        let id = id_del.clone();
        let Some(auth) = app_delete.auth_state.read().as_ref().cloned() else {
            return;
        };
        let token = auth.session().clone();
        let ctl = app_delete.item_control.clone();
        spawn(async move { ctl.delete(&token, &id).await });
    };

    rsx! {
        div { class: "group flex items-center gap-3 px-3 py-2.5 rounded-md hover:bg-zinc-900 transition-colors",
            input {
                r#type: "checkbox",
                checked: item.done,
                onchange: on_toggle,
                class: "w-4 h-4 rounded border-zinc-600 bg-zinc-800 text-blue-500 focus:ring-blue-500 focus:ring-offset-0 cursor-pointer",
            }
            span {
                class: if item.done {
                    "flex-1 text-zinc-500 line-through"
                } else {
                    "flex-1 text-zinc-200"
                },
                "{item.name}"
            }
            button {
                onclick: on_delete,
                class: "opacity-0 group-hover:opacity-100 px-2 py-1 text-zinc-600 hover:text-red-400 transition-all text-sm",
                "delete"
            }
        }
    }
}
