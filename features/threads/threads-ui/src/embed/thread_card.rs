//! One thread = a root comment + its reply tree.
//!
//! Dumb. The caller provides the full comment list filtered to one
//! `(entity_kind, entity_id, optional anchor)` slice; this component
//! splits roots from replies and lays them out. Actions render as a
//! flat row of small ghost buttons (no Popover) — the fts-ui surface
//! we're working with doesn't ship an `IconButton` primitive yet, so
//! we keep the menu shape simple and rely on small Ghost buttons.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    CircleCheck, CornerDownRight, MessageSquare, Send, Smile, Trash2,
};
use fts_ui::prelude::*;
use threads_proto::{Anchor, Comment};
use uuid::Uuid;

use super::anchor_chip::AnchorChip;

#[derive(Props, Clone, PartialEq)]
pub struct ThreadCardProps {
    pub root: Comment,
    pub all_comments: Vec<Comment>,
    #[props(default)]
    pub mention_pool: Vec<String>,
    pub on_reply: EventHandler<(Uuid, String)>,
    pub on_resolve: EventHandler<Uuid>,
    pub on_reopen: EventHandler<Uuid>,
    pub on_promote_to_task: EventHandler<Uuid>,
    pub on_react: EventHandler<(Uuid, String)>,
    pub on_delete: EventHandler<Uuid>,
    pub on_edit: EventHandler<(Uuid, String)>,
}

#[component]
pub fn ThreadCard(props: ThreadCardProps) -> Element {
    let root = props.root.clone();
    let kind_variant = kind_badge_variant(&root.kind);
    let kind_label_str = kind_label(&root);
    let anchor = root
        .anchor_json
        .as_deref()
        .and_then(|s| serde_json::from_str::<Anchor>(s).ok())
        .unwrap_or(Anchor::Entity);

    let root_id = root.id;
    let is_action = root.kind == "action";
    let resolved = root.resolved;

    rsx! {
        Card {
            CardHeader {
                div { class: "flex items-start justify-between gap-2 w-full",
                    div { class: "flex items-center gap-2 flex-wrap",
                        span { class: "font-medium text-sm", "{root.author}" }
                        AnchorChip { anchor: anchor }
                        StatusBadge { variant: kind_variant, label: kind_label_str }
                        if resolved {
                            StatusBadge {
                                variant: StatusBadgeVariant::Success,
                                label: "Resolved".to_string(),
                                icon: rsx! { CircleCheck { size: 12 } },
                            }
                        }
                    }
                }
            }
            CardContent {
                p { class: "text-sm whitespace-pre-wrap", "{root.body}" }
                ReplyTree {
                    parent_id: root_id,
                    all_comments: props.all_comments.clone(),
                }
            }
            CardFooter {
                div { class: "flex items-center justify-between gap-2 w-full flex-wrap",
                    div { class: "flex gap-1",
                        Button {
                            variant: ButtonVariant::Ghost,
                            size: ButtonSize::Small,
                            on_click: move |_| props.on_react.call((root_id, "+1".to_string())),
                            Smile { size: 14 }
                        }
                        if resolved {
                            Button {
                                variant: ButtonVariant::Ghost,
                                size: ButtonSize::Small,
                                on_click: move |_| props.on_reopen.call(root_id),
                                "Reopen"
                            }
                        } else {
                            Button {
                                variant: ButtonVariant::Ghost,
                                size: ButtonSize::Small,
                                on_click: move |_| props.on_resolve.call(root_id),
                                CircleCheck { size: 14 }
                                " Resolve"
                            }
                        }
                        if is_action {
                            Button {
                                variant: ButtonVariant::Ghost,
                                size: ButtonSize::Small,
                                on_click: move |_| props.on_promote_to_task.call(root_id),
                                "Promote to task"
                            }
                        }
                        Button {
                            variant: ButtonVariant::Ghost,
                            size: ButtonSize::Small,
                            on_click: move |_| props.on_delete.call(root_id),
                            Trash2 { size: 14 }
                        }
                    }
                    InlineReplyComposer {
                        parent_id: root_id,
                        on_reply: move |args| props.on_reply.call(args),
                    }
                }
            }
        }
    }
}

fn kind_label(c: &Comment) -> String {
    if c.kind == "action" {
        match c.action_status.as_deref() {
            Some(s) => format!("action: {s}"),
            None => "action".into(),
        }
    } else {
        c.kind.clone()
    }
}

fn kind_badge_variant(kind: &str) -> StatusBadgeVariant {
    // FUTURE: per plan v1 limits — markdown body, edit history, threaded
    // reactions, and mention notifications all live downstream of this kind
    // mapping. For now we hold to fts-ui's four-variant palette.
    match kind {
        "action" => StatusBadgeVariant::Warning,
        "decision" | "praise" => StatusBadgeVariant::Success,
        _ => StatusBadgeVariant::Neutral,
    }
}

#[derive(Props, Clone, PartialEq)]
struct ReplyTreeProps {
    parent_id: Uuid,
    all_comments: Vec<Comment>,
}

#[component]
fn ReplyTree(props: ReplyTreeProps) -> Element {
    let children: Vec<Comment> = props
        .all_comments
        .iter()
        .filter(|c| c.reply_to == Some(props.parent_id) && !c.deleted)
        .cloned()
        .collect();
    if children.is_empty() {
        return rsx! {};
    }
    rsx! {
        div { class: "flex flex-col gap-2 mt-2 pl-3 border-l border-border",
            for child in children.iter().cloned() {
                div { class: "flex flex-col gap-1",
                    div { class: "flex items-center gap-2 text-xs text-muted-foreground",
                        CornerDownRight { size: 12 }
                        span { class: "font-medium", "{child.author}" }
                    }
                    p { class: "text-sm whitespace-pre-wrap", "{child.body}" }
                    ReplyTree {
                        parent_id: child.id,
                        all_comments: props.all_comments.clone(),
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct InlineReplyComposerProps {
    parent_id: Uuid,
    on_reply: EventHandler<(Uuid, String)>,
}

#[component]
fn InlineReplyComposer(props: InlineReplyComposerProps) -> Element {
    let mut open = use_signal(|| false);
    let mut draft = use_signal(String::new);
    let parent_id = props.parent_id;

    rsx! {
        div { class: "flex flex-col gap-1",
            if *open.read() {
                div { class: "flex gap-1 items-end",
                    Textarea {
                        placeholder: "Reply\u{2026}",
                        value: draft,
                    }
                    Button {
                        size: ButtonSize::Small,
                        on_click: move |_| {
                            let b = draft.read().trim().to_string();
                            if b.is_empty() { return; }
                            props.on_reply.call((parent_id, b));
                            draft.set(String::new());
                            open.set(false);
                        },
                        Send { size: 14 }
                    }
                }
            } else {
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| open.set(true),
                    MessageSquare { size: 14 }
                    " Reply"
                }
            }
        }
    }
}
