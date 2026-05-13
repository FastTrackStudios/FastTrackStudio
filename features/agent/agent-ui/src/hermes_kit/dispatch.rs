//! `AgentDispatchDialog` — modal to launch an agent run from a task.
//!
//! Dumb component: the caller passes the integration registry's
//! capabilities + the task brief, the dialog emits a single
//! `DispatchIntent` on confirm.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Bot;
use fts_ui::prelude::*;
use uuid::Uuid;

/// Small projection of a `Task` for the dialog header + prompt
/// prefill. We don't pull `project-proto::Task` here to keep the
/// crate graph one-directional (agent-ui → fts-ui only).
#[derive(Clone, PartialEq)]
pub struct TaskBrief {
    pub id: Uuid,
    pub title: String,
    pub description: Option<String>,
    pub tags: Vec<String>,
    pub branch_name: Option<String>,
}

#[derive(Clone, PartialEq)]
pub struct IntegrationInfo {
    pub name: String,
    pub label: String,
    pub agent_kinds: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DispatchIntent {
    pub task_id: Uuid,
    pub integration: String,
    pub agent_kind: String,
    pub prompt: String,
}

fn default_prompt(task: &TaskBrief) -> String {
    let desc = task.description.clone().unwrap_or_default();
    let tags = if task.tags.is_empty() {
        "—".to_string()
    } else {
        task.tags.join(", ")
    };
    format!("# {}\n\n{}\n\nAcceptance:\n- {}", task.title, desc, tags)
}

#[component]
pub fn AgentDispatchDialog(
    open: bool,
    task: Option<TaskBrief>,
    available_integrations: Vec<IntegrationInfo>,
    on_dispatch: EventHandler<DispatchIntent>,
    on_close: EventHandler<()>,
) -> Element {
    // Field-local signals. Reset each open by keying on task.id +
    // available_integrations.len() through a use_effect.
    let initial_integration = available_integrations
        .first()
        .map(|i| i.name.clone())
        .unwrap_or_default();
    let initial_kind = available_integrations
        .first()
        .and_then(|i| i.agent_kinds.first().cloned())
        .unwrap_or_default();
    let initial_prompt = task.as_ref().map(default_prompt).unwrap_or_default();

    let mut integration = use_signal(|| initial_integration.clone());
    let mut agent_kind = use_signal(|| initial_kind.clone());
    let prompt = use_signal(|| initial_prompt.clone());

    // When the task changes (or the integration set), refresh defaults.
    let task_key = task.as_ref().map(|t| t.id);
    let integrations_for_effect = available_integrations.clone();
    use_effect(move || {
        let _ = task_key;
        let next_integration = integrations_for_effect
            .first()
            .map(|i| i.name.clone())
            .unwrap_or_default();
        let next_kind = integrations_for_effect
            .first()
            .and_then(|i| i.agent_kinds.first().cloned())
            .unwrap_or_default();
        integration.set(next_integration);
        agent_kind.set(next_kind);
    });

    // When integration changes, reset agent_kind to its first kind.
    let integrations_for_kind_effect = available_integrations.clone();
    use_effect(move || {
        let current = integration.read().clone();
        let kinds = integrations_for_kind_effect
            .iter()
            .find(|i| i.name == current)
            .map(|i| i.agent_kinds.clone())
            .unwrap_or_default();
        if !kinds.iter().any(|k| k == &*agent_kind.read()) {
            agent_kind.set(kinds.first().cloned().unwrap_or_default());
        }
    });

    let task_for_render = task.clone();
    let integrations_for_render = available_integrations.clone();

    rsx! {
        Dialog {
            open,
            on_open_change: move |o: bool| if !o { on_close.call(()) },
            DialogHeader {
                DialogTitle {
                    HStack { class: "items-center gap-2",
                        Bot { size: 16 }
                        span { "Run with agent" }
                    }
                }
                DialogDescription {
                    if let Some(t) = task_for_render.as_ref() {
                        "Dispatch ‘{t.title}’ to an external integration."
                    } else {
                        "Pick an integration to dispatch this task."
                    }
                }
            }
            div { class: "flex flex-col gap-3 px-1 py-2",
                div { class: "flex flex-col gap-1",
                    Label { "Integration" }
                    {
                        let opts = integrations_for_render.clone();
                        rsx! {
                            NativeSelect {
                                value: integration,
                                for opt in opts.into_iter() {
                                    NativeSelectOption {
                                        key: "{opt.name}",
                                        value: opt.name.clone(),
                                        "{opt.label}"
                                    }
                                }
                            }
                        }
                    }
                }
                div { class: "flex flex-col gap-1",
                    Label { "Agent kind" }
                    {
                        let cur = integration.read().clone();
                        let kinds: Vec<String> = integrations_for_render
                            .iter()
                            .find(|i| i.name == cur)
                            .map(|i| i.agent_kinds.clone())
                            .unwrap_or_default();
                        rsx! {
                            NativeSelect {
                                value: agent_kind,
                                for k in kinds.into_iter() {
                                    NativeSelectOption {
                                        key: "{k}",
                                        value: k.clone(),
                                        "{k}"
                                    }
                                }
                            }
                        }
                    }
                }
                div { class: "flex flex-col gap-1",
                    Label { "Prompt" }
                    Textarea {
                        value: prompt,
                        rows: 8u32,
                        placeholder: "Tell the agent what to do…".to_string(),
                    }
                }
            }
            DialogFooter {
                Button {
                    variant: ButtonVariant::Outline,
                    size: ButtonSize::Small,
                    on_click: move |_| on_close.call(()),
                    "Cancel"
                }
                Button {
                    variant: ButtonVariant::Primary,
                    size: ButtonSize::Small,
                    on_click: {
                        let task_id_opt = task.as_ref().map(|t| t.id);
                        move |_| {
                            let Some(tid) = task_id_opt else { return; };
                            let i = integration.read().clone();
                            let k = agent_kind.read().clone();
                            let p = prompt.read().clone();
                            if i.is_empty() || k.is_empty() {
                                return;
                            }
                            on_dispatch.call(DispatchIntent {
                                task_id: tid,
                                integration: i,
                                agent_kind: k,
                                prompt: p,
                            });
                        }
                    },
                    Bot { size: 14 }
                    " Launch"
                }
            }
        }
    }
}
