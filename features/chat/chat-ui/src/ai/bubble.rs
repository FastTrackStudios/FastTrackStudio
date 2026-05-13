//! `MessageBubble` — role-aware renderer for a single AI-chat message.
//!
//! Dispatches on `message.role`:
//! - user (or `None`)   → right-aligned bubble, no avatar.
//! - assistant          → left-aligned card + avatar; reasoning + tool
//!   calls stack below the body; muted metadata row at the bottom.
//! - system             → centered Secondary badge with truncation.
//! - tool               → renders only the tool card; bubble itself is
//!   suppressed because tool rows usually embed inside their parent
//!   assistant message at the page level.

use chat_proto::Message;
use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use uuid::Uuid;

use super::actions::{MessageAction, MessageActions};
use super::common::{format_cost, format_token_count, relative_time};
use super::markdown::Markdown;
use super::reasoning::ReasoningPanel;
use super::streaming::StreamingIndicator;
use super::tool_call::{ToolCallCard, parse_tool_calls};

const SYSTEM_TRUNCATE: usize = 140;

#[derive(Props, Clone, PartialEq)]
pub struct MessageBubbleProps {
    pub message: Message,
    #[props(default)]
    pub author_avatar_url: Option<String>,
    #[props(default = true)]
    pub show_timestamp: bool,
    #[props(default = true)]
    pub show_actions: bool,
    pub on_action: EventHandler<(Uuid, MessageAction)>,
}

#[component]
pub fn MessageBubble(props: MessageBubbleProps) -> Element {
    let role = props.message.role.clone().unwrap_or_else(|| "user".into());
    match role.as_str() {
        "assistant" => render_assistant(&props),
        "system" => render_system(&props),
        "tool" => render_tool_only(&props),
        _ => render_user(&props),
    }
}

fn render_user(props: &MessageBubbleProps) -> Element {
    let body = props.message.body.clone();
    rsx! {
        div { class: "group/message flex justify-end my-2",
            div { class: "flex flex-col items-end gap-1 max-w-[80%]",
                div { class: "rounded-2xl bg-primary/10 px-4 py-2 text-sm whitespace-pre-wrap break-words",
                    "{body}"
                }
                if props.show_actions {
                    div { class: "opacity-0 group-hover/message:opacity-100 transition-opacity",
                        MessageActions {
                            message_id: props.message.id,
                            role: "user",
                            body: props.message.body.clone(),
                            on_action: props.on_action,
                        }
                    }
                }
            }
        }
    }
}

fn render_assistant(props: &MessageBubbleProps) -> Element {
    let m = &props.message;
    let body = m.body.clone();
    let streaming = m.streaming;
    let reasoning = m.reasoning.clone();
    let tool_calls = parse_tool_calls(m.tool_calls_json.as_deref());
    let model = m.model.clone();
    let in_tokens = m.tokens_input;
    let out_tokens = m.tokens_output;
    let cost = m.cost_cents;
    let created = m.created_at;
    let now = Utc::now();
    let avatar_url = props.author_avatar_url.clone();

    rsx! {
        div { class: "group/message flex gap-3 my-3",
            Avatar { size: AvatarSize::Small,
                if let Some(url) = avatar_url {
                    AvatarImage { src: url, alt: "Assistant" }
                }
                AvatarFallback { "A" }
            }
            div { class: "flex flex-col gap-1 min-w-0 flex-1",
                Card { class: "border-border/60",
                    CardContent { class: "py-3 px-4",
                        div { class: "text-sm",
                            Markdown { source: body, streaming }
                            if streaming {
                                StreamingIndicator {}
                            }
                        }
                    }
                }
                if let Some(r) = reasoning {
                    ReasoningPanel { text: r, streaming, duration_ms: None }
                }
                for call in tool_calls.into_iter() {
                    ToolCallCard { key: "{call.id}", call }
                }
                div { class: "flex items-center gap-2 flex-wrap text-xs text-muted-foreground",
                    if let Some(m) = model {
                        span { class: "font-mono", "{m}" }
                    }
                    if let (Some(i), Some(o)) = (in_tokens, out_tokens) {
                        span { "·" }
                        span { "{format_token_count(i)} in · {format_token_count(o)} out" }
                    } else if let Some(o) = out_tokens {
                        span { "·" }
                        span { "{format_token_count(o)} tokens" }
                    }
                    if let Some(c) = cost {
                        span { "·" }
                        span { "{format_cost(c)}" }
                    }
                    if props.show_timestamp {
                        span { "·" }
                        span { "{relative_time(created, now)}" }
                    }
                    Spacer {}
                    if props.show_actions {
                        div { class: "opacity-0 group-hover/message:opacity-100 transition-opacity",
                            MessageActions {
                                message_id: m_id(props),
                                role: "assistant",
                                body: props.message.body.clone(),
                                on_action: props.on_action,
                            }
                        }
                    }
                }
            }
        }
    }
}

fn m_id(props: &MessageBubbleProps) -> Uuid {
    props.message.id
}

fn render_system(props: &MessageBubbleProps) -> Element {
    let body = props.message.body.clone();
    let needs_truncate = body.chars().count() > SYSTEM_TRUNCATE;
    let mut expanded = use_signal(|| false);

    let shown: String = if needs_truncate && !*expanded.read() {
        let head: String = body.chars().take(SYSTEM_TRUNCATE).collect();
        format!("{head}…")
    } else {
        body
    };

    rsx! {
        div { class: "flex justify-center my-2",
            div { class: "max-w-[80%]",
                Badge { variant: BadgeVariant::Secondary,
                    span { class: "whitespace-pre-wrap text-xs",
                        "system · {shown}"
                        if needs_truncate {
                            " "
                            button {
                                r#type: "button",
                                class: "underline hover:no-underline",
                                onclick: move |_| {
                                    let cur = *expanded.read();
                                    expanded.set(!cur);
                                },
                                if *expanded.read() { "show less" } else { "show more" }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn render_tool_only(props: &MessageBubbleProps) -> Element {
    // Defensive: the page-level layout usually folds tool rows into
    // their parent assistant message via `tool_calls_json`. When a
    // standalone tool Message lands, render whatever tool calls we can
    // parse off its body / json blob.
    let calls = parse_tool_calls(props.message.tool_calls_json.as_deref());
    if calls.is_empty() {
        return rsx! {
            div { class: "my-2 text-xs text-muted-foreground italic",
                "tool: {props.message.body}"
            }
        };
    }
    rsx! {
        div { class: "my-2 flex flex-col gap-2",
            for call in calls.into_iter() {
                ToolCallCard { key: "{call.id}", call }
            }
        }
    }
}
