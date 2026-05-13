//! `ConversationSidebar` — left-column list of `AgentConversation`s
//! grouped by recency. Rendered as a plain `<aside>` so it composes
//! cleanly inside the app's outer Sidebar shell (one Sidebar per
//! application — don't nest fts-ui Sidebars).

use agent_proto::AgentConversation;
use chrono::{DateTime, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{MessageSquarePlus, Search};
use fts_ui::prelude::*;
use std::collections::HashMap;
use uuid::Uuid;

use super::common::group_conversations_by_day;

#[derive(Props, Clone, PartialEq)]
pub struct ConversationSidebarProps {
    pub conversations: Vec<AgentConversation>,
    #[props(default)]
    pub active: Option<Uuid>,
    #[props(default)]
    pub last_message_by_conv: HashMap<Uuid, String>,
    pub now: DateTime<Utc>,
    pub on_select: EventHandler<Uuid>,
    pub on_new: EventHandler<()>,
    pub on_archive: EventHandler<Uuid>,
    pub on_delete: EventHandler<Uuid>,
    pub on_rename: EventHandler<(Uuid, String)>,
}

#[component]
pub fn ConversationSidebar(props: ConversationSidebarProps) -> Element {
    let query = use_signal(String::new);

    // Apply search filter; group what remains.
    let q = query.read().to_lowercase();
    let filtered: Vec<AgentConversation> = if q.is_empty() {
        props.conversations.clone()
    } else {
        props
            .conversations
            .iter()
            .filter(|c| c.title.to_lowercase().contains(&q))
            .cloned()
            .collect()
    };
    let groups = group_conversations_by_day(&filtered, props.now);

    rsx! {
        aside { class: "w-72 border-r border-border bg-sidebar/40 flex flex-col h-full",
            // Header
            div { class: "p-3 border-b border-border flex flex-col gap-2",
                Button {
                    variant: ButtonVariant::Outline,
                    on_click: move |_| props.on_new.call(()),
                    MessageSquarePlus { size: 14 }
                    " New chat"
                }
                div { class: "flex items-center gap-2 rounded-md border border-input bg-input/30 px-2",
                    Search { size: 14 }
                    Input {
                        value: query,
                        placeholder: "Search…".to_string(),
                        class: "border-0 bg-transparent shadow-none focus-visible:ring-0".to_string(),
                    }
                }
            }

            // Body
            div { class: "flex-1 overflow-y-auto",
                if filtered.is_empty() {
                    div { class: "p-6 text-center text-xs text-muted-foreground",
                        if props.conversations.is_empty() {
                            "No conversations yet."
                        } else {
                            "No matches."
                        }
                    }
                }
                for (group, convs) in groups.into_iter() {
                    div { key: "{group.as_str()}", class: "flex flex-col py-2",
                        div { class: "px-3 pb-1 text-[10px] font-medium uppercase tracking-wider text-muted-foreground",
                            "{group.as_str()}"
                        }
                        for conv in convs.into_iter() {
                            ConversationRow {
                                key: "{conv.id}",
                                conv: conv.clone(),
                                active: props.active == Some(conv.id),
                                preview: props.last_message_by_conv.get(&conv.id).cloned(),
                                on_select: props.on_select,
                                on_archive: props.on_archive,
                                on_delete: props.on_delete,
                                on_rename: props.on_rename,
                            }
                        }
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct ConversationRowProps {
    conv: AgentConversation,
    active: bool,
    preview: Option<String>,
    on_select: EventHandler<Uuid>,
    on_archive: EventHandler<Uuid>,
    on_delete: EventHandler<Uuid>,
    on_rename: EventHandler<(Uuid, String)>,
}

#[component]
fn ConversationRow(props: ConversationRowProps) -> Element {
    let id = props.conv.id;
    let title = props.conv.title.clone();
    let preview = props.preview.clone().unwrap_or_default();
    let active_class = if props.active {
        "bg-accent"
    } else {
        "hover:bg-accent/50"
    };

    rsx! {
        ContextMenu {
            ContextMenuTrigger {
                div {
                    class: "px-3 py-2 cursor-pointer {active_class}",
                    onclick: move |_| props.on_select.call(id),
                    div { class: "text-sm truncate font-medium", "{title}" }
                    if !preview.is_empty() {
                        div { class: "text-xs text-muted-foreground truncate", "{preview}" }
                    }
                }
            }
            ContextMenuContent {
                ContextMenuItem {
                    value: "rename".to_string(),
                    index: 0usize,
                    on_select: {
                        let title = title.clone();
                        move |_v: String| props.on_rename.call((id, title.clone()))
                    },
                    "Rename"
                }
                ContextMenuItem {
                    value: "archive".to_string(),
                    index: 1usize,
                    on_select: move |_v: String| props.on_archive.call(id),
                    "Archive"
                }
                ContextMenuSeparator {}
                ContextMenuItem {
                    value: "delete".to_string(),
                    index: 2usize,
                    destructive: true,
                    on_select: move |_v: String| props.on_delete.call(id),
                    "Delete"
                }
            }
        }
    }
}
