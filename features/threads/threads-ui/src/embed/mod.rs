//! Universal `ThreadEmbed` — the component every feature mounts when it
//! wants threads on something. Three render modes via `EmbedMode`:
//! `Sidebar` (default, vertical thread list), `Inline` (compact pinned
//! comment bubble for embedding alongside a block), `Margin` (right-rail
//! document-style annotations).
//!
//! Wholly dumb — caller filters the comments list and owns the repo
//! writes. The component emits events, never calls a repo directly.

pub mod anchor_chip;
pub mod composer;
pub mod thread_card;

pub use anchor_chip::{AnchorChip, breadcrumb_label};
pub use composer::{ComposerSubmit, ThreadComposer};
pub use thread_card::ThreadCard;

use dioxus::prelude::*;
use threads_proto::{Anchor, Comment, CommentCreate};
use uuid::Uuid;

/// Visual layout of the embed.
#[derive(Clone, Copy, PartialEq, Default)]
pub enum EmbedMode {
    /// Vertical list of threads with composer at bottom. Default.
    #[default]
    Sidebar,
    /// Compact pinned-comment bubble — for inline placement alongside a block.
    Inline,
    /// Right-rail document-style annotations (Google Docs style).
    Margin,
}

#[derive(Props, Clone, PartialEq)]
pub struct ThreadEmbedProps {
    pub entity_kind: String,
    pub entity_id: Uuid,
    /// Optional anchor scoping the embed to a specific anchor (e.g. a
    /// `TextQuoteSelector` shown next to a paragraph). `None` = whole-entity.
    #[props(default)]
    pub anchor: Option<Anchor>,
    /// The caller's pre-filtered comment set for this `(entity_kind, entity_id)`
    /// (and optionally `anchor`). The component does no further filtering.
    pub comments: Vec<Comment>,
    /// Mention pool for the composer's autocomplete. Caller supplies the
    /// usernames currently relevant to this surface.
    #[props(default)]
    pub mention_pool: Vec<String>,
    #[props(default)]
    pub mode: EmbedMode,

    pub on_create: EventHandler<CommentCreate>,
    pub on_reply: EventHandler<(Uuid, String)>,
    pub on_resolve: EventHandler<Uuid>,
    pub on_reopen: EventHandler<Uuid>,
    pub on_promote_to_task: EventHandler<Uuid>,
    pub on_react: EventHandler<(Uuid, String)>,
    pub on_delete: EventHandler<Uuid>,
    pub on_edit: EventHandler<(Uuid, String)>,
}

#[component]
pub fn ThreadEmbed(props: ThreadEmbedProps) -> Element {
    let roots: Vec<Comment> = props
        .comments
        .iter()
        .filter(|c| c.reply_to.is_none() && !c.deleted)
        .cloned()
        .collect();

    let container_class = match props.mode {
        EmbedMode::Sidebar => {
            "flex flex-col gap-3 p-3 border-l border-border bg-background h-full overflow-y-auto"
        }
        EmbedMode::Inline => "flex flex-col gap-2 p-2 border rounded-md bg-muted/30",
        EmbedMode::Margin => "flex flex-col gap-2 p-2 text-sm",
    };

    rsx! {
        div { class: "{container_class}",
            for root in roots.iter().cloned() {
                ThreadCard {
                    key: "{root.id}",
                    root: root.clone(),
                    all_comments: props.comments.clone(),
                    mention_pool: props.mention_pool.clone(),
                    on_reply: move |args| props.on_reply.call(args),
                    on_resolve: move |id| props.on_resolve.call(id),
                    on_reopen: move |id| props.on_reopen.call(id),
                    on_promote_to_task: move |id| props.on_promote_to_task.call(id),
                    on_react: move |args| props.on_react.call(args),
                    on_delete: move |id| props.on_delete.call(id),
                    on_edit: move |args| props.on_edit.call(args),
                }
            }
            ThreadComposer {
                mention_pool: props.mention_pool.clone(),
                on_submit: move |submit: ComposerSubmit| {
                    let payload = CommentCreate {
                        entity_id: props.entity_id,
                        entity_type: props.entity_kind.clone(),
                        author: submit.author,
                        body: submit.body,
                        time_start_ms: None,
                        time_end_ms: None,
                        reply_to: None,
                        resolved: false,
                        resolved_by: None,
                        mentions: submit.mentions,
                        tags: vec![],
                        anchor_json: props
                            .anchor
                            .as_ref()
                            .and_then(|a| serde_json::to_string(a).ok()),
                    };
                    props.on_create.call(payload);
                },
                on_promote_to_task: move |_| {},
            }
        }
    }
}
