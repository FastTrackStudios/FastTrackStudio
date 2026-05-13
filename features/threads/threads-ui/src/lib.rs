//! Threads feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! Scoped to the `Comment` entity for v1; `Reaction` and `Attachment`
//! come later. The component split mirrors the timer-ui template:
//!
//! - [`CommentList`]       — full collection view, dispatches `on_delete`
//! - [`CommentRow`]        — single-row presentation (composable)
//! - [`CommentCreateForm`] — minimal new-comment form, emits the create payload

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{CheckCheck, MessageSquare, Plus, Send, Trash2};
use fts_ui::prelude::*;
use std::collections::BTreeMap;
use threads_proto::{Comment, CommentCreate};
use uuid::Uuid;

#[component]
pub fn CommentList(items: Vec<Comment>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No comments yet. Add one above.",
                icon: rsx! { MessageSquare { size: 32 } },
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
    let title = format!("{} · {}", comment.author, comment.entity_type);
    let resolved = comment.resolved;
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{title}" }
                ItemDescription { "{body_preview}" }
            }
            ItemActions { class: "gap-2",
                if resolved {
                    StatusBadge { variant: StatusBadgeVariant::Success, label: "Resolved" }
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
pub fn CommentCreateForm(on_submit: EventHandler<CommentCreate>) -> Element {
    let mut author = use_signal(String::new);
    let mut body = use_signal(String::new);
    let mut entity_type = use_signal(String::new);

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Add a comment" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: author,
                        placeholder: "Author (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: entity_type,
                        placeholder: "task",
                        class: "flex-1 min-w-40",
                    }
                }
                Input {
                    value: body,
                    placeholder: "Body (required)",
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
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
                        Plus { size: 14 }
                        " Add comment"
                    }
                }
            }
        }
    }
}

/// Purpose-built threads dashboard. Page header + message-volume stats +
/// entity-type filter + create form + list.
#[component]
pub fn ThreadDashboard(
    items: Vec<Comment>,
    status: String,
    on_create: EventHandler<CommentCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let mut type_filter = use_signal(|| "all".to_string());

    let count = items.len();
    let resolved = items.iter().filter(|c| c.resolved).count();
    let open = count.saturating_sub(resolved);
    let authors: std::collections::BTreeSet<String> =
        items.iter().map(|c| c.author.clone()).collect();
    let mut by_type: BTreeMap<String, usize> = BTreeMap::new();
    for c in &items {
        *by_type.entry(c.entity_type.clone()).or_insert(0) += 1;
    }

    let types: Vec<String> = by_type.keys().cloned().collect();
    let current = type_filter.read().clone();
    let filtered: Vec<Comment> = if current == "all" {
        items.clone()
    } else {
        items
            .iter()
            .filter(|c| c.entity_type == current)
            .cloned()
            .collect()
    };

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Threads",
                trailing: rsx! {
                    HStack { class: "gap-2 items-center",
                        StatusDot {
                            color: StatusDotColor::Success,
                            size: StatusDotSize::Small,
                        }
                        Text { variant: TextVariant::Muted, "{status}" }
                    }
                },
            }

            HStack { class: "gap-3 items-start",
                div { class: "rounded-md bg-rose-500/10 p-2 text-rose-500",
                    MessageSquare { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Threads dashboard" }
                    Text { variant: TextVariant::Muted,
                        "Comments and feedback threaded across tasks, projects, and assets."
                    }
                }
            }

            div { class: "grid grid-cols-1 sm:grid-cols-4 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Messages" }
                            MessageSquare { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{count}" }
                        Text { variant: TextVariant::Muted, "total" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Open" }
                            Send { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{open}" }
                        Text { variant: TextVariant::Muted, "needs attention" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Resolved" }
                            CheckCheck { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{resolved}" }
                        Text { variant: TextVariant::Muted, "closed" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Voices" }
                            MessageSquare { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{authors.len()}" }
                        Text { variant: TextVariant::Muted, "distinct authors" }
                    }
                }
            }

            if types.len() > 1 {
                HStack { class: "gap-2 flex-wrap items-center",
                    Text { variant: TextVariant::Muted, "Entity:" }
                    Button {
                        variant: if current == "all" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                        size: ButtonSize::Small,
                        on_click: move |_| type_filter.set("all".into()),
                        "All"
                    }
                    for t in types.iter().cloned() {
                        Button {
                            key: "{t}",
                            variant: if current == t { ButtonVariant::Primary } else { ButtonVariant::Outline },
                            size: ButtonSize::Small,
                            on_click: {
                                let t = t.clone();
                                move |_| type_filter.set(t.clone())
                            },
                            "{t}"
                        }
                    }
                }
            }

            CommentCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            SectionHeader {
                label: "Conversation",
                trailing: rsx! {
                    Badge { variant: BadgeVariant::Secondary, "{filtered.len()}" }
                },
            }
            CommentList { items: filtered, on_delete: move |id| on_delete.call(id) }
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
