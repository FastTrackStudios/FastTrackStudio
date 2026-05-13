//! Chat feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! Scoped to the `Message` entity for v1; `Channel` and `ChannelMember`
//! come later. The component split mirrors the timer-ui template:
//!
//! - [`MessageList`]       — full collection view, dispatches `on_delete`
//! - [`MessageRow`]        — single-row presentation (composable)
//! - [`MessageCreateForm`] — minimal new-message form, emits the create payload

pub mod ai;
pub use ai::*;

use chat_proto::{Message, MessageCreate};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    AtSign, Hash, MessageSquare, MessagesSquare, Paperclip, Send, Trash2, Users,
};
use fts_ui::prelude::*;
use std::collections::BTreeSet;
use uuid::Uuid;

#[component]
pub fn MessageList(items: Vec<Message>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No messages yet. Send one below.",
                icon: rsx! { MessageSquare { size: 32 } },
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
    let author = message.author.clone();
    let body = message.body.clone();
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{author}" }
                if deleted {
                    ItemDescription { class: "italic", "(deleted)" }
                } else {
                    ItemDescription { "{body}" }
                }
            }
            ItemActions { class: "gap-2",
                if edited {
                    Badge { variant: BadgeVariant::Secondary, "edited" }
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
pub fn MessageDashboard(
    items: Vec<Message>,
    status: String,
    on_create: EventHandler<MessageCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let total = items.len();
    let authors: BTreeSet<String> = items.iter().map(|m| m.author.clone()).collect();
    let mentions_count: usize = items.iter().map(|m| m.mentions.len()).sum();
    let attachments_count: usize = items.iter().map(|m| m.attachment_ids.len()).sum();
    let edited_count = items.iter().filter(|m| m.edited_at.is_some()).count();
    let active_count = items.iter().filter(|m| !m.deleted).count();
    let mut tab = use_signal(|| "all".to_string());

    let filtered: Vec<Message> = match tab.read().as_str() {
        "mentions" => items
            .iter()
            .filter(|m| !m.mentions.is_empty())
            .cloned()
            .collect(),
        "attached" => items
            .iter()
            .filter(|m| !m.attachment_ids.is_empty())
            .cloned()
            .collect(),
        "active" => items.iter().filter(|m| !m.deleted).cloned().collect(),
        _ => items.clone(),
    };

    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-6 p-6 lg:p-10",
            SectionHeader {
                label: "Chat".to_string(),
                trailing: rsx! {
                    StatusBadge { variant: StatusBadgeVariant::Neutral, label: status.clone() }
                },
            }
            HStack { class: "items-center gap-3",
                div { class: "rounded-md bg-primary/10 p-2 text-primary",
                    MessagesSquare { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Conversations" }
                    Text { variant: TextVariant::Muted,
                        "Multiplayer chat — track mentions, attachments, and recent activity."
                    }
                }
            }
            div { class: "grid gap-3 sm:grid-cols-2 lg:grid-cols-4",
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Messages" }
                        Hash { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{total}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "{active_count} active · {edited_count} edited" }
                    }
                }
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Participants" }
                        Users { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{authors.len()}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "unique authors" }
                    }
                }
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Mentions" }
                        AtSign { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{mentions_count}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "across all messages" }
                    }
                }
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Attachments" }
                        Paperclip { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{attachments_count}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "files shared" }
                    }
                }
            }

            MessageCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            HStack { class: "items-center justify-between",
                Heading { level: HeadingLevel::H3, "Timeline" }
                SegmentedControl {
                    value: tab.read().clone(),
                    on_change: move |v: String| tab.set(v),
                    options: vec![
                        ("all".to_string(), "All".to_string()),
                        ("active".to_string(), "Active".to_string()),
                        ("mentions".to_string(), "Mentions".to_string()),
                        ("attached".to_string(), "Attachments".to_string()),
                    ],
                }
            }
            MessageList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}

#[component]
pub fn MessageCreateForm(on_submit: EventHandler<MessageCreate>) -> Element {
    let mut author = use_signal(String::new);
    let mut body = use_signal(String::new);

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Send a message" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: author,
                        placeholder: "Author (required)",
                        class: "flex-1 min-w-40",
                    }
                }
                Textarea {
                    value: body,
                    placeholder: "Body (required)",
                    class: "min-h-20",
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let a = author.read().clone();
                            let b = body.read().clone();
                            if a.trim().is_empty() || b.trim().is_empty() {
                                return;
                            }
                            let payload = MessageCreate {
                                channel_id: Some(Uuid::new_v4()),
                                author: a,
                                body: b,
                                reply_to: None,
                                edited_at: None,
                                deleted: false,
                                mentions: Vec::new(),
                                attachment_ids: Vec::new(),
                                role: None,
                                model: None,
                                reasoning: None,
                                tool_calls_json: None,
                                finish_reason: None,
                                tokens_input: None,
                                tokens_output: None,
                                cost_cents: None,
                                streaming: false,
                                agent_conversation_id: None,
                            };
                            on_submit.call(payload);
                            author.set(String::new());
                            body.set(String::new());
                        },
                        Send { size: 14 }
                        " Send message"
                    }
                }
            }
        }
    }
}
