//! Chat feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! Scoped to the `Message` entity for v1; `Channel` and `ChannelMember`
//! come later. The component split mirrors the asset-ui template:
//!
//! - [`MessageList`]       — full collection view, dispatches `on_delete`
//! - [`MessageRow`]        — single-row presentation (composable)
//! - [`MessageCreateForm`] — minimal new-message form, emits the create payload

use chat_proto::{Message, MessageCreate};
use dioxus::prelude::*;
use uuid::Uuid;

#[component]
pub fn MessageList(items: Vec<Message>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No messages yet. Send one below."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for message in items.iter().cloned() {
                MessageRow {
                    key: "{message.id}",
                    message: message.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn MessageRow(message: Message, on_delete: EventHandler<Uuid>) -> Element {
    let id = message.id;
    let edited = message.edited_at.is_some();
    let deleted = message.deleted;
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col gap-1 min-w-0",
                div { class: "flex items-baseline gap-2",
                    span { class: "text-sm font-semibold text-slate-100", "{message.author}" }
                    if edited {
                        span { class: "text-xs text-slate-500", "edited" }
                    }
                }
                if deleted {
                    span { class: "text-sm italic text-slate-500", "(deleted)" }
                } else {
                    span { class: "text-sm text-slate-300", "{message.body}" }
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
pub fn MessageCreateForm(on_submit: EventHandler<MessageCreate>) -> Element {
    let mut author = use_signal(String::new);
    let mut body = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-col gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let a = author.read().clone();
                let b = body.read().clone();
                if a.trim().is_empty() || b.trim().is_empty() {
                    return;
                }
                let payload = MessageCreate {
                    channel_id: Uuid::new_v4(),
                    author: a,
                    body: b,
                    reply_to: None,
                    edited_at: None,
                    deleted: false,
                    mentions: Vec::new(),
                    attachment_ids: Vec::new(),
                };
                on_submit.call(payload);
                author.set(String::new());
                body.set(String::new());
            },
            input {
                class: "w-full rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Author (required)",
                value: "{author}",
                oninput: move |evt| author.set(evt.value()),
            }
            textarea {
                class: "min-h-20 w-full rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Body (required)",
                value: "{body}",
                oninput: move |evt| body.set(evt.value()),
            }
            div {
                button {
                    r#type: "submit",
                    class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                    "Send message"
                }
            }
        }
    }
}
