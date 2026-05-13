//! Email feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! The component split mirrors the email domain:
//!
//! - [`EmailList`]  — full collection view, dispatches `on_delete`
//! - [`EmailRow`]   — single-row presentation (composable into other lists)
//! - [`EmailCreateForm`] — minimal new-email form, emits the create payload

use dioxus::prelude::*;
use email_proto::{Email, EmailCreate};
use uuid::Uuid;

#[component]
pub fn EmailList(items: Vec<Email>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No emails yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for email in items.iter().cloned() {
                EmailRow {
                    key: "{email.id}",
                    email: email.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn EmailRow(email: Email, on_delete: EventHandler<Uuid>) -> Element {
    let id = email.id;
    let folder = email
        .folder
        .clone()
        .unwrap_or_else(|| "inbox".into());
    let subject_class = if email.read {
        "text-sm font-bold text-slate-300"
    } else {
        "text-sm font-bold text-slate-100"
    };
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "{subject_class}", "{email.subject}" }
                span { class: "text-xs text-slate-500",
                    "{email.from_addr} · {folder}"
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
pub fn EmailCreateForm(on_submit: EventHandler<EmailCreate>) -> Element {
    let mut subject = use_signal(String::new);
    let mut from_addr = use_signal(String::new);
    let mut body = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let s = subject.read().clone();
                let f = from_addr.read().clone();
                if s.trim().is_empty() || f.trim().is_empty() {
                    return;
                }
                let payload = EmailCreate {
                    message_id: format!("<{}@local>", Uuid::new_v4()),
                    subject: s,
                    from_addr: f,
                    to_addrs: Vec::new(),
                    cc_addrs: Vec::new(),
                    bcc_addrs: Vec::new(),
                    body: trim_to_option(body.read().clone()),
                    received_at: chrono::Utc::now(),
                    read: false,
                    starred: false,
                    folder: Some("inbox".into()),
                    thread_id: None,
                    tags: Vec::new(),
                };
                on_submit.call(payload);
                subject.set(String::new());
                from_addr.set(String::new());
                body.set(String::new());
            },
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Subject (required)",
                value: "{subject}",
                oninput: move |evt| subject.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "From (required)",
                value: "{from_addr}",
                oninput: move |evt| from_addr.set(evt.value()),
            }
            textarea {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Body",
                value: "{body}",
                oninput: move |evt| body.set(evt.value()),
            }
            button {
                r#type: "submit",
                class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                "Add email"
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
