//! Email feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! The component split mirrors the email domain:
//!
//! - [`EmailList`]  — full collection view, dispatches `on_delete`
//! - [`EmailRow`]   — single-row presentation (composable into other lists)
//! - [`EmailCreateForm`] — minimal new-email form, emits the create payload

use dioxus::prelude::*;
use email_proto::{Email, EmailCreate};
use fts_ui::lucide_dioxus::{Inbox, Mail, MailOpen, Plus, Star, Trash2};
use fts_ui::prelude::*;
use std::collections::BTreeMap;
use uuid::Uuid;

#[component]
pub fn EmailList(items: Vec<Email>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No emails yet. Add one above.",
                icon: rsx! { Mail { size: 32 } },
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
    let folder = email.folder.clone().unwrap_or_else(|| "inbox".into());
    let read = email.read;
    let meta = format!("{} · {}", email.from_addr, folder);
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{email.subject}" }
                ItemDescription { "{meta}" }
            }
            ItemActions { class: "gap-2",
                if !read {
                    Badge { variant: BadgeVariant::Default, "Unread" }
                }
                if email.starred {
                    Badge { variant: BadgeVariant::Secondary, "Starred" }
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_delete.call(id),
                    Trash2 { size: 14 }
                }
            }
        }
    }
}

#[component]
pub fn EmailDashboard(
    items: Vec<Email>,
    status: String,
    on_create: EventHandler<EmailCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let total = items.len();
    let unread = items.iter().filter(|e| !e.read).count();
    let starred = items.iter().filter(|e| e.starred).count();
    let mut by_folder: BTreeMap<String, usize> = BTreeMap::new();
    for e in &items {
        let f = e.folder.clone().unwrap_or_else(|| "inbox".into());
        *by_folder.entry(f).or_insert(0) += 1;
    }
    let folder_count = by_folder.len();
    let mut tab = use_signal(|| "inbox".to_string());

    let filtered: Vec<Email> = match tab.read().as_str() {
        "unread" => items.iter().filter(|e| !e.read).cloned().collect(),
        "starred" => items.iter().filter(|e| e.starred).cloned().collect(),
        "all" => items.clone(),
        other => items
            .iter()
            .filter(|e| e.folder.as_deref().unwrap_or("inbox") == other)
            .cloned()
            .collect(),
    };

    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-6 p-6 lg:p-10",
            SectionHeader {
                label: "Email".to_string(),
                trailing: rsx! {
                    StatusBadge { variant: StatusBadgeVariant::Neutral, label: status.clone() }
                },
            }
            HStack { class: "items-center gap-3",
                div { class: "rounded-md bg-sky-500/10 p-2 text-sky-600",
                    Inbox { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Inbox" }
                    Text { variant: TextVariant::Muted,
                        "Triage messages across folders — unread, starred, and everything else."
                    }
                }
            }
            div { class: "grid gap-3 sm:grid-cols-2 lg:grid-cols-4",
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Total" }
                        Mail { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{total}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "across {folder_count} folders" }
                    }
                }
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Unread" }
                        MailOpen { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{unread}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "awaiting reply" }
                    }
                }
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Starred" }
                        Star { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{starred}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "flagged for follow-up" }
                    }
                }
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Folders" }
                        Inbox { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{folder_count}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "in use" }
                    }
                }
            }

            EmailCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            HStack { class: "items-center justify-between",
                Heading { level: HeadingLevel::H3, "Messages" }
                SegmentedControl {
                    value: tab.read().clone(),
                    on_change: move |v: String| tab.set(v),
                    options: vec![
                        ("inbox".to_string(), "Inbox".to_string()),
                        ("unread".to_string(), "Unread".to_string()),
                        ("starred".to_string(), "Starred".to_string()),
                        ("all".to_string(), "All".to_string()),
                    ],
                }
            }
            EmailList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}

#[component]
pub fn EmailCreateForm(on_submit: EventHandler<EmailCreate>) -> Element {
    let mut subject = use_signal(String::new);
    let mut from_addr = use_signal(String::new);
    let mut body = use_signal(String::new);

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Add an email" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: subject,
                        placeholder: "Subject (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: from_addr,
                        placeholder: "From (required)",
                        class: "flex-1 min-w-40",
                    }
                }
                Textarea {
                    value: body,
                    placeholder: "Body",
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
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
                        Plus { size: 14 }
                        " Add email"
                    }
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
