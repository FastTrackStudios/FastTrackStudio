//! Threads feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! Scoped to the `Comment` entity for v1; `Reaction` and `Attachment`
//! come later. The component split mirrors the asset-ui template:
//!
//! - [`CommentList`]       — full collection view, dispatches `on_delete`
//! - [`CommentRow`]        — single-row presentation (composable)
//! - [`CommentCreateForm`] — minimal new-comment form, emits the create payload

use dioxus::prelude::*;
use threads_proto::{Comment, CommentCreate};
use uuid::Uuid;

#[component]
pub fn CommentList(items: Vec<Comment>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No comments yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for comment in items.iter().cloned() {
                CommentRow {
                    key: "{comment.id}",
                    comment: comment.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn CommentRow(comment: Comment, on_delete: EventHandler<Uuid>) -> Element {
    let id = comment.id;
    let body_preview = truncate(&comment.body, 80);
    let resolved_prefix = if comment.resolved { "✓ " } else { "" };
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col gap-1 min-w-0",
                div { class: "flex items-baseline gap-2",
                    span { class: "text-sm font-semibold text-slate-100",
                        "{resolved_prefix}{comment.author}"
                    }
                    span { class: "text-xs text-slate-500", "{comment.entity_type}" }
                }
                span { class: "text-sm text-slate-300 truncate", "{body_preview}" }
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
pub fn CommentCreateForm(on_submit: EventHandler<CommentCreate>) -> Element {
    let mut author = use_signal(String::new);
    let mut body = use_signal(String::new);
    let mut entity_type = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-col gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let a = author.read().clone();
                let b = body.read().clone();
                let et = entity_type.read().clone();
                if a.trim().is_empty() || b.trim().is_empty() || et.trim().is_empty() {
                    return;
                }
                let payload = CommentCreate {
                    entity_id: Uuid::new_v4(),
                    entity_type: et,
                    author: a,
                    body: b,
                    time_start_ms: None,
                    time_end_ms: None,
                    reply_to: None,
                    resolved: false,
                    resolved_by: None,
                    mentions: Vec::new(),
                    tags: Vec::new(),
                };
                on_submit.call(payload);
                author.set(String::new());
                body.set(String::new());
                entity_type.set(String::new());
            },
            div { class: "flex flex-wrap gap-2",
                input {
                    class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                    placeholder: "Author (required)",
                    value: "{author}",
                    oninput: move |evt| author.set(evt.value()),
                }
                input {
                    class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                    placeholder: "task",
                    value: "{entity_type}",
                    oninput: move |evt| entity_type.set(evt.value()),
                }
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
                    "Add comment"
                }
            }
        }
    }
}

fn truncate(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        s.to_string()
    } else {
        let mut out: String = s.chars().take(max).collect();
        out.push('…');
        out
    }
}
