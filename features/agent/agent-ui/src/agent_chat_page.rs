//! `AgentChatPage` — page-level AI chat layout.
//!
//! Stitches together the dumb `chat-ui::ai::*` components against the
//! `AgentConversation` + `Message` data model. Caller-supplied
//! `EventHandler`s let the route module dispatch real writes.

use agent_proto::{AgentConversation, AgentConversationUpdate, ModelInfo};
use chat_proto::Message;
use chat_ui::{
    ConversationSidebar, MessageAction, MessageBubble, MessageComposer, ModelPicker, SendIntent,
};
use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ArrowDown, GitBranch, MessageCircle, Settings as SettingsIcon};
use fts_ui::prelude::*;
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Props, Clone, PartialEq)]
pub struct AgentChatPageProps {
    pub conversations: Vec<AgentConversation>,
    #[props(default)]
    pub active_conversation: Option<Uuid>,
    pub messages: Vec<Message>,
    pub available_models: Vec<ModelInfo>,
    pub available_tools: Vec<String>,
    #[props(default)]
    pub streaming_message_id: Option<Uuid>,
    pub on_select_conversation: EventHandler<Uuid>,
    pub on_new_conversation: EventHandler<()>,
    pub on_send: EventHandler<SendIntent>,
    pub on_stop: EventHandler<Uuid>,
    pub on_update_conversation: EventHandler<(Uuid, AgentConversationUpdate)>,
    pub on_message_action: EventHandler<(Uuid, MessageAction)>,
    pub on_archive_conversation: EventHandler<Uuid>,
    pub on_delete_conversation: EventHandler<Uuid>,
    pub on_rename_conversation: EventHandler<(Uuid, String)>,
}

#[component]
pub fn AgentChatPage(props: AgentChatPageProps) -> Element {
    let active_id = props.active_conversation;
    let active_conv: Option<AgentConversation> =
        active_id.and_then(|id| props.conversations.iter().find(|c| c.id == id).cloned());

    // Composer-local overrides default to the conversation's defaults.
    let default_model = active_conv
        .as_ref()
        .map(|c| c.default_model.clone())
        .or_else(|| props.available_models.first().map(|m| m.id.clone()))
        .unwrap_or_default();
    let default_tools = active_conv
        .as_ref()
        .map(|c| c.tool_set.clone())
        .unwrap_or_default();

    let draft = use_signal(String::new);
    let mut model_override = use_signal::<Option<String>>(|| None);
    let mut reasoning_enabled = use_signal(|| false);
    let mut tools_override = use_signal::<Option<Vec<String>>>(|| None);
    let mut settings_open = use_signal(|| false);
    let pinned_to_bottom = use_signal(|| true);

    let model = model_override
        .read()
        .clone()
        .unwrap_or_else(|| default_model.clone());
    let tools = tools_override
        .read()
        .clone()
        .unwrap_or_else(|| default_tools.clone());

    let now = Utc::now();
    let last_message_by_conv: HashMap<Uuid, String> = {
        let mut map: HashMap<Uuid, String> = HashMap::new();
        for m in props.messages.iter() {
            if let Some(cid) = m.agent_conversation_id {
                let body: String = m.body.chars().take(60).collect();
                map.insert(cid, body);
            }
        }
        map
    };

    let on_send = props.on_send;
    let on_send_handler = move |intent: SendIntent| {
        on_send.call(intent);
    };

    rsx! {
        div { class: "flex h-[calc(100vh-4rem)] w-full bg-background",
            // ── Sidebar ────────────────────────────────────────────
            ConversationSidebar {
                conversations: props.conversations.clone(),
                active: props.active_conversation,
                last_message_by_conv,
                now,
                on_select: props.on_select_conversation,
                on_new: props.on_new_conversation,
                on_archive: props.on_archive_conversation,
                on_delete: props.on_delete_conversation,
                on_rename: props.on_rename_conversation,
            }

            // ── Main column ───────────────────────────────────────
            div { class: "flex flex-1 flex-col min-w-0",
                if active_conv.is_some() {
                    {render_active(
                        active_conv.clone().unwrap(),
                        props.messages.clone(),
                        props.available_models.clone(),
                        props.available_tools.clone(),
                        props.streaming_message_id,
                        model.clone(),
                        tools.clone(),
                        *reasoning_enabled.read(),
                        draft,
                        settings_open,
                        pinned_to_bottom,
                        move |m: String| model_override.set(Some(m)),
                        move |v: bool| reasoning_enabled.set(v),
                        move |ts: Vec<String>| tools_override.set(Some(ts)),
                        on_send_handler.clone(),
                        props.on_stop,
                        props.on_message_action,
                        props.on_update_conversation,
                    )}
                } else {
                    {render_empty(props.on_new_conversation)}
                }
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn render_active(
    conv: AgentConversation,
    all_messages: Vec<Message>,
    available_models: Vec<ModelInfo>,
    available_tools: Vec<String>,
    streaming_message_id: Option<Uuid>,
    model: String,
    tools: Vec<String>,
    reasoning_enabled: bool,
    draft: Signal<String>,
    mut settings_open: Signal<bool>,
    mut pinned_to_bottom: Signal<bool>,
    on_change_model: impl FnMut(String) + Clone + 'static,
    on_toggle_reasoning: impl FnMut(bool) + Clone + 'static,
    on_change_tools: impl FnMut(Vec<String>) + Clone + 'static,
    on_send: impl FnMut(SendIntent) + Clone + 'static,
    on_stop: EventHandler<Uuid>,
    on_message_action: EventHandler<(Uuid, MessageAction)>,
    on_update_conversation: EventHandler<(Uuid, AgentConversationUpdate)>,
) -> Element {
    let conv_id = conv.id;
    let title = conv.title.clone();
    let messages: Vec<Message> = all_messages
        .into_iter()
        .filter(|m| m.agent_conversation_id == Some(conv.id))
        .collect();
    let streaming = streaming_message_id.is_some();
    let streaming_id = streaming_message_id;

    rsx! {
        // Header
        div { class: "sticky top-0 z-10 flex items-center gap-2 border-b border-border bg-background/95 px-4 py-3 backdrop-blur",
            div { class: "flex items-center gap-2 min-w-0",
                MessageCircle { size: 18 }
                Heading { level: HeadingLevel::H3, "{title}" }
            }
            Spacer {}
            Button {
                variant: ButtonVariant::Ghost,
                size: ButtonSize::Small,
                on_click: move |_| {},
                GitBranch { size: 14 }
                " Branch"
            }
            Button {
                variant: ButtonVariant::Ghost,
                size: ButtonSize::Small,
                on_click: move |_| settings_open.set(true),
                SettingsIcon { size: 14 }
                " Settings"
            }
        }

        // Scroll area
        div {
            class: "flex-1 overflow-y-auto px-4 py-4",
            onscroll: move |_| {
                // The dioxus web event lacks direct scroll metrics
                // without an interop call. v1 keeps things simple:
                // any user scroll unpins. The "Jump to latest" button
                // re-pins.
                if *pinned_to_bottom.read() {
                    pinned_to_bottom.set(false);
                }
            },
            div { class: "mx-auto max-w-3xl flex flex-col",
                for m in messages.iter().cloned() {
                    MessageBubble {
                        key: "{m.id}",
                        message: m,
                        on_action: on_message_action,
                    }
                }
            }
        }

        // Jump-to-latest reveal
        if !*pinned_to_bottom.read() {
            div { class: "pointer-events-none absolute bottom-32 right-8 z-20",
                div { class: "pointer-events-auto",
                    Button {
                        variant: ButtonVariant::Outline,
                        on_click: move |_| pinned_to_bottom.set(true),
                        ArrowDown { size: 14 }
                        " Jump to latest"
                    }
                }
            }
        }

        // Composer (sticky bottom)
        div { class: "sticky bottom-0 border-t border-border bg-background/95 px-4 py-3 backdrop-blur",
            div { class: "mx-auto max-w-3xl",
                {
                    let mut on_change_model = on_change_model.clone();
                    let mut on_toggle_reasoning = on_toggle_reasoning.clone();
                    let mut on_change_tools = on_change_tools.clone();
                    let mut on_send = on_send.clone();
                    let stop_id = streaming_id;
                    rsx! {
                        MessageComposer {
                            draft,
                            streaming,
                            model: model.clone(),
                            available_models: available_models.clone(),
                            tool_set: tools.clone(),
                            available_tools: available_tools.clone(),
                            reasoning_enabled,
                            on_send: move |i: SendIntent| on_send(i),
                            on_stop: move |_| {
                                if let Some(id) = stop_id {
                                    on_stop.call(id);
                                }
                            },
                            on_change_model: move |m: String| on_change_model(m),
                            on_toggle_reasoning: move |v: bool| on_toggle_reasoning(v),
                            on_change_tools: move |ts: Vec<String>| on_change_tools(ts),
                        }
                    }
                }
            }
        }

        // ── Settings sheet ────────────────────────────────────────
        {render_settings_sheet(conv.clone(), settings_open, available_models.clone(), available_tools.clone(), move |patch| {
            on_update_conversation.call((conv_id, patch));
        })}
    }
}

fn render_settings_sheet(
    conv: AgentConversation,
    mut settings_open: Signal<bool>,
    available_models: Vec<ModelInfo>,
    available_tools: Vec<String>,
    mut on_save: impl FnMut(AgentConversationUpdate) + 'static,
) -> Element {
    let title = use_signal(|| conv.title.clone());
    let system_prompt = use_signal(|| conv.system_prompt.clone().unwrap_or_default());
    let mut model = use_signal(|| conv.default_model.clone());
    let temperature_milli_init = conv.temperature_milli as f64;
    let temperature = use_signal(|| temperature_milli_init);
    let max_tokens = use_signal(|| conv.max_tokens.map(|v| v.to_string()).unwrap_or_default());
    let initial_tools = conv.tool_set.clone();
    let mut tool_set = use_signal(|| initial_tools.clone());

    rsx! {
        Sheet {
            open: *settings_open.read(),
            side: SheetSide::Right,
            on_close: move |_| settings_open.set(false),
            class: "w-full sm:max-w-md",
            div { class: "flex flex-col gap-4 overflow-y-auto",
                Heading { level: HeadingLevel::H3, "Conversation settings" }
                div { class: "flex flex-col gap-1",
                    Label { class: "text-xs uppercase tracking-widest text-muted-foreground", "Title" }
                    Input { value: title }
                }
                div { class: "flex flex-col gap-1",
                    Label { class: "text-xs uppercase tracking-widest text-muted-foreground", "System prompt" }
                    Textarea { value: system_prompt, rows: 5u32 }
                }
                div { class: "flex flex-col gap-1",
                    Label { class: "text-xs uppercase tracking-widest text-muted-foreground", "Default model" }
                    ModelPicker {
                        value: model.read().clone(),
                        models: available_models,
                        on_change: move |m: String| model.set(m),
                    }
                }
                div { class: "flex flex-col gap-1",
                    Label { class: "text-xs uppercase tracking-widest text-muted-foreground",
                        "Temperature · {format_temp(*temperature.read() as i32)}"
                    }
                    Slider {
                        value: temperature,
                        min: 0.0,
                        max: 2000.0,
                        step: 50.0,
                    }
                }
                div { class: "flex flex-col gap-1",
                    Label { class: "text-xs uppercase tracking-widest text-muted-foreground", "Max tokens" }
                    Input {
                        value: max_tokens,
                        placeholder: "(unbounded)",
                    }
                }
                div { class: "flex flex-col gap-1",
                    Label { class: "text-xs uppercase tracking-widest text-muted-foreground", "Tools" }
                    div { class: "flex flex-col gap-1",
                        for tool in available_tools.iter().cloned() {
                            {
                                let is_on = tool_set.read().iter().any(|t| t == &tool);
                                let label = tool.clone();
                                rsx! {
                                    label {
                                        key: "{tool}",
                                        class: "flex items-center gap-2 text-sm",
                                        input {
                                            r#type: "checkbox",
                                            checked: is_on,
                                            onchange: move |_| {
                                                let mut next = tool_set.read().clone();
                                                if is_on {
                                                    next.retain(|t| t != &label);
                                                } else {
                                                    next.push(label.clone());
                                                }
                                                tool_set.set(next);
                                            },
                                        }
                                        "{tool}"
                                    }
                                }
                            }
                        }
                    }
                }
                div { class: "flex items-center justify-end gap-2 pt-3",
                    Button {
                        variant: ButtonVariant::Ghost,
                        on_click: move |_| settings_open.set(false),
                        "Cancel"
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        on_click: move |_| {
                            let mx = max_tokens.read().parse::<i32>().ok();
                            let patch = AgentConversationUpdate {
                                title: Some(title.read().clone()),
                                system_prompt: Some(Some(system_prompt.read().clone()).filter(|s: &String| !s.is_empty())),
                                default_model: Some(model.read().clone()),
                                temperature_milli: Some(*temperature.read() as i32),
                                max_tokens: Some(mx),
                                tool_set: Some(tool_set.read().clone()),
                                agent_run_id: None,
                                project_id: None,
                                parent_conversation_id: None,
                                branch_from_message_id: None,
                                archived: None,
                            };
                            on_save(patch);
                            settings_open.set(false);
                        },
                        "Save"
                    }
                }
            }
        }
    }
}

fn format_temp(milli: i32) -> String {
    let v = (milli as f32) / 1000.0;
    format!("{v:.2}")
}

fn render_empty(on_new_conversation: EventHandler<()>) -> Element {
    rsx! {
        div { class: "flex flex-1 items-center justify-center",
            VStack { class: "items-center gap-3",
                MessageCircle { size: 36 }
                Heading { level: HeadingLevel::H3, "No conversation selected" }
                Text { variant: TextVariant::Muted, "Pick a conversation on the left or start a new one." }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| on_new_conversation.call(()),
                    "+ New chat"
                }
            }
        }
    }
}
