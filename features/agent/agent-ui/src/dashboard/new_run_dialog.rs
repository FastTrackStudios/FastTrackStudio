//! `NewRunDialog` — modal for starting a new agent run.
//! Spec ref: P4C in plans/agent-p4-dashboard.goal.md.

use agent_proto::AgentRunCreate;
use dioxus::prelude::*;
use fts_ui::prelude::*;

/// Lightweight payload the dialog emits. The route layer adapts it to
/// the full `AgentRunCreate` shape (filling in all the optional
/// fields with `None` / zero defaults).
#[derive(Clone, Debug, PartialEq)]
pub struct NewRunDraft {
    pub prompt: String,
    pub kind: String,
    pub worktree_path: Option<String>,
}

impl NewRunDraft {
    pub fn into_create(self) -> AgentRunCreate {
        AgentRunCreate {
            name: self
                .prompt
                .lines()
                .next()
                .unwrap_or("agent run")
                .chars()
                .take(80)
                .collect(),
            kind: self.kind,
            prompt: self.prompt,
            status: "queued".into(),
            task_id: None,
            started_at: None,
            completed_at: None,
            result: None,
            error_message: None,
            tokens_used: None,
            cost_cents: None,
            tags: Vec::new(),
            integration: None,
            external_id: None,
            external_url: None,
            log_cursor: None,
            parent_run_id: None,
            worktree_path: self.worktree_path,
            git_repo_connection_id: None,
            spawned_from_message_id: None,
            input_tokens: None,
            output_tokens: None,
            cache_read_tokens: None,
            cache_creation_tokens: None,
            cost_cents_estimate: None,
            tool_call_count: 0,
            assistant_message_count: 0,
            max_tokens: None,
            max_tool_calls: None,
            max_wall_seconds: None,
        }
    }
}

const KNOWN_KINDS: &[&str] = &[
    "claude-code",
    "codex",
    "hermes",
    "hermes-agent",
    "gemini-cli",
    "aider",
    "cursor-agent",
    "mock",
];

#[component]
pub fn NewRunDialog(on_submit: EventHandler<NewRunDraft>, on_dismiss: EventHandler<()>) -> Element {
    let mut prompt = use_signal(String::new);
    let mut kind = use_signal(|| "hermes".to_string());
    let mut worktree = use_signal(String::new);

    let prompt_value = prompt.read().clone();
    let kind_value = kind.read().clone();
    let worktree_value = worktree.read().clone();

    let disabled = prompt_value.trim().is_empty() || kind_value.trim().is_empty();
    let error = if prompt_value.trim().is_empty() {
        Some("Prompt is required")
    } else if kind_value.trim().is_empty() {
        Some("Kind is required")
    } else {
        None
    };

    let on_submit_click = {
        let prompt_value = prompt_value.clone();
        let kind_value = kind_value.clone();
        let worktree_value = worktree_value.clone();
        move |_| {
            if disabled {
                return;
            }
            let wt = if worktree_value.trim().is_empty() {
                None
            } else {
                Some(worktree_value.clone())
            };
            on_submit.call(NewRunDraft {
                prompt: prompt_value.clone(),
                kind: kind_value.clone(),
                worktree_path: wt,
            });
        }
    };

    rsx! {
        // Backdrop + centered modal panel. Theme tokens only.
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center bg-background/70 backdrop-blur-sm",
            onclick: move |_| on_dismiss.call(()),
            div {
                class: "relative w-full max-w-lg rounded-lg border border-border bg-card p-6 shadow-lg",
                onclick: move |e: MouseEvent| e.stop_propagation(),
                VStack { class: "gap-4",
                    Heading { level: HeadingLevel::H2, "Start a new agent run" }

                    VStack { class: "gap-2",
                        Text { variant: TextVariant::Muted, "Prompt" }
                        textarea {
                            class: "min-h-[160px] w-full rounded-md border border-border bg-background p-3 text-sm",
                            placeholder: "What should the agent do?",
                            value: "{prompt_value}",
                            oninput: move |e| prompt.set(e.value()),
                        }
                    }

                    VStack { class: "gap-2",
                        Text { variant: TextVariant::Muted, "Kind" }
                        select {
                            class: "rounded-md border border-border bg-background p-2 text-sm",
                            value: "{kind_value}",
                            onchange: move |e| kind.set(e.value()),
                            for k in KNOWN_KINDS.iter() {
                                option {
                                    key: "{k}",
                                    value: "{k}",
                                    "{k}"
                                }
                            }
                        }
                    }

                    VStack { class: "gap-2",
                        Text { variant: TextVariant::Muted, "Worktree path (optional)" }
                        input {
                            class: "rounded-md border border-border bg-background p-2 text-sm",
                            placeholder: "/home/you/Development/project",
                            value: "{worktree_value}",
                            oninput: move |e| worktree.set(e.value()),
                        }
                    }

                    if let Some(msg) = error {
                        Text { variant: TextVariant::Muted, "{msg}" }
                    }

                    HStack { class: "gap-2 justify-end",
                        Button {
                            variant: ButtonVariant::Outline,
                            on_click: move |_| on_dismiss.call(()),
                            "Cancel"
                        }
                        Button {
                            disabled,
                            on_click: on_submit_click,
                            "Start run"
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn draft_first_line_becomes_name() {
        let d = NewRunDraft {
            prompt: "Refactor auth\nDetails below…".into(),
            kind: "hermes".into(),
            worktree_path: None,
        };
        let c = d.into_create();
        assert_eq!(c.name, "Refactor auth");
        assert_eq!(c.kind, "hermes");
    }

    #[test]
    fn draft_name_capped_at_80_chars() {
        let d = NewRunDraft {
            prompt: "x".repeat(200),
            kind: "mock".into(),
            worktree_path: None,
        };
        let c = d.into_create();
        assert_eq!(c.name.chars().count(), 80);
    }

    #[test]
    fn draft_worktree_passes_through() {
        let d = NewRunDraft {
            prompt: "go".into(),
            kind: "hermes".into(),
            worktree_path: Some("/tmp/x".into()),
        };
        let c = d.into_create();
        assert_eq!(c.worktree_path.as_deref(), Some("/tmp/x"));
    }
}
