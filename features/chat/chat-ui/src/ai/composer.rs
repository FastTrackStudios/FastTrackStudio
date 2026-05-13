//! `MessageComposer` — bottom-of-the-chat input.
//!
//! Top chip row (model + reasoning toggle + tools popover), textarea,
//! send/stop button. Stateless beyond the draft signal the caller
//! passes in.

use agent_proto::ModelInfo;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{CircleStop, Paperclip, SendHorizontal, Sparkles, Wrench};
use fts_ui::prelude::*;

use super::model_picker::ModelPicker;

#[derive(Clone, Debug, PartialEq)]
pub struct SendIntent {
    pub body: String,
    pub model: String,
    pub reasoning: bool,
    pub tool_set: Vec<String>,
}

#[derive(Props, Clone, PartialEq)]
pub struct MessageComposerProps {
    pub draft: Signal<String>,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default = false)]
    pub streaming: bool,
    pub model: String,
    pub available_models: Vec<ModelInfo>,
    pub tool_set: Vec<String>,
    pub available_tools: Vec<String>,
    #[props(default = false)]
    pub reasoning_enabled: bool,
    pub on_send: EventHandler<SendIntent>,
    pub on_stop: EventHandler<()>,
    pub on_change_model: EventHandler<String>,
    pub on_toggle_reasoning: EventHandler<bool>,
    pub on_change_tools: EventHandler<Vec<String>>,
}

#[component]
pub fn MessageComposer(props: MessageComposerProps) -> Element {
    let mut draft = props.draft;
    let model = props.model.clone();
    let tool_set = props.tool_set.clone();
    let available_tools = props.available_tools.clone();
    let reasoning_enabled = props.reasoning_enabled;
    let streaming = props.streaming;
    let disabled = props.disabled;
    let available_models = props.available_models.clone();

    // Auto-grow: 1..=8 rows based on '\n' count, clamped.
    let row_count = use_memo(move || {
        let s = draft.read();
        let lines = s.matches('\n').count() + 1;
        lines.clamp(1, 8) as u32
    });

    // Tool popover open state.
    let tool_popover_open = use_signal(|| false);

    let send_intent = {
        let model = model.clone();
        let tool_set = tool_set.clone();
        move || -> Option<SendIntent> {
            let body = draft.read().trim().to_string();
            if body.is_empty() {
                return None;
            }
            // Slash-command stub: log and clear; do NOT dispatch on_send.
            if let Some(rest) = body.strip_prefix('/') {
                tracing::info!(command = %rest, "slash-command stub");
                draft.set(String::new());
                return None;
            }
            Some(SendIntent {
                body,
                model: model.clone(),
                reasoning: reasoning_enabled,
                tool_set: tool_set.clone(),
            })
        }
    };

    let trigger_send = {
        let on_send = props.on_send;
        let mut send_intent = send_intent.clone();
        move || {
            if let Some(intent) = send_intent() {
                on_send.call(intent);
                draft.set(String::new());
            }
        }
    };

    let on_send_click = {
        let mut trigger_send = trigger_send.clone();
        move |_| trigger_send()
    };

    let on_keydown = {
        let mut trigger_send = trigger_send.clone();
        move |e: KeyboardEvent| {
            let key = e.data().key();
            let mods = e.data().modifiers();
            if (key == Key::Enter)
                && (mods.contains(Modifiers::META) || mods.contains(Modifiers::CONTROL))
            {
                e.prevent_default();
                trigger_send();
            }
        }
    };

    let tools_count = tool_set.len();
    let tool_set_for_render = tool_set.clone();
    let available_tools_for_render = available_tools.clone();
    let on_change_tools = props.on_change_tools;

    rsx! {
        Card { class: "border-border/60",
            div { class: "flex flex-col gap-2 p-3",
                // Top chip row.
                HStack { class: "items-center gap-2 flex-wrap",
                    ModelPicker {
                        value: model.clone(),
                        models: available_models,
                        on_change: move |m: String| props.on_change_model.call(m),
                    }
                    Toggle {
                        pressed: reasoning_enabled,
                        on_toggle: move |v: bool| props.on_toggle_reasoning.call(v),
                        Sparkles { size: 14 }
                        "Thinking"
                    }
                    Popover {
                        open: Some(*tool_popover_open.read()),
                        on_open_change: {
                            let mut tool_popover_open = tool_popover_open;
                            move |o: bool| tool_popover_open.set(o)
                        },
                        PopoverTrigger {
                            div { class: "inline-flex items-center gap-1 rounded-full border border-border px-3 py-1 text-sm cursor-pointer hover:bg-accent",
                                Wrench { size: 14 }
                                "Tools"
                                if tools_count > 0 {
                                    Badge { variant: BadgeVariant::Secondary, "{tools_count}" }
                                }
                            }
                        }
                        PopoverContent { class: "w-56 p-2",
                            VStack { class: "gap-1",
                                if available_tools_for_render.is_empty() {
                                    Text { variant: TextVariant::Muted, class: "text-xs p-2",
                                        "No tools available."
                                    }
                                }
                                for tool in available_tools_for_render.into_iter() {
                                    {
                                        let is_on = tool_set_for_render.iter().any(|t| t == &tool);
                                        let label = tool.clone();
                                        let current_set = tool_set_for_render.clone();
                                        rsx! {
                                            label {
                                                class: "flex items-center gap-2 p-1 text-sm cursor-pointer hover:bg-accent rounded",
                                                input {
                                                    r#type: "checkbox",
                                                    checked: is_on,
                                                    onchange: move |_| {
                                                        let mut next = current_set.clone();
                                                        if is_on {
                                                            next.retain(|t| t != &label);
                                                        } else {
                                                            next.push(label.clone());
                                                        }
                                                        on_change_tools.call(next);
                                                    },
                                                }
                                                "{tool}"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Textarea + send/stop.
                HStack { class: "items-end gap-2",
                    Button {
                        variant: ButtonVariant::Ghost,
                        size: ButtonSize::Small,
                        on_click: move |_| {
                            // Attachments not wired in v1.
                            tracing::info!("attach button (stub)");
                        },
                        Paperclip { size: 16 }
                    }
                    div { class: "flex-1",
                        Textarea {
                            value: draft,
                            placeholder: "Send a message…  (⌘/Ctrl-Enter to send)",
                            rows: *row_count.read(),
                            disabled,
                            on_change: use_callback(move |_e: FormEvent| {}),
                        }
                    }
                    if streaming {
                        Button {
                            variant: ButtonVariant::Destructive,
                            on_click: move |_| props.on_stop.call(()),
                            CircleStop { size: 16 }
                        }
                    } else {
                        Button {
                            variant: ButtonVariant::Primary,
                            disabled,
                            on_click: on_send_click,
                            SendHorizontal { size: 16 }
                        }
                    }
                }
                // Hidden key-capture surface — textarea doesn't expose
                // keydown directly through TextareaProps, so we mount
                // a sibling div with `tabindex=-1` and let the browser
                // bubble. This is a v1 compromise; native key handlers
                // can land once fts-ui Textarea grows an on_keydown.
                div {
                    tabindex: -1i64,
                    onkeydown: on_keydown,
                    class: "sr-only",
                }
            }
        }
    }
}
