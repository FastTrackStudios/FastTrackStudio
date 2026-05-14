//! `RunDetailView` — three-pane operator view for one agent run.
//! Spec ref: P4B in plans/agent-p4-dashboard.goal.md +
//! `agent.dashboard.split-detail`.

use agent_proto::{AgentLogLine, AgentRun, ConversationMessage, ToolCall};
use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ChevronDown, ChevronRight};
use fts_ui::prelude::*;
use uuid::Uuid;

use super::{InlineDiff, fmt_cost_cents, fmt_elapsed_seconds, status_to_badge_variant};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RunDetailPane {
    Logs,
    ToolCalls,
    Conversation,
}

impl RunDetailPane {
    pub fn label(self) -> &'static str {
        match self {
            Self::Logs => "Logs",
            Self::ToolCalls => "Tool calls",
            Self::Conversation => "Conversation",
        }
    }
}

#[component]
pub fn RunDetailView(
    run: AgentRun,
    logs: Vec<AgentLogLine>,
    tool_calls: Vec<ToolCall>,
    conversation: Vec<ConversationMessage>,
    on_cancel: EventHandler<Uuid>,
    on_back: EventHandler<()>,
) -> Element {
    let mut pane = use_signal(|| RunDetailPane::Logs);
    let mut log_level: Signal<Option<String>> = use_signal(|| None);

    let run_id = run.id;
    let status = run.status.clone();
    let kind = run.kind.clone();
    let name = run.name.clone();
    let prompt = run.prompt.clone();
    let variant = status_to_badge_variant(&run.status);
    let elapsed = match run.started_at {
        Some(start) => {
            let end = run.completed_at.unwrap_or_else(Utc::now);
            fmt_elapsed_seconds(Some((end - start).num_seconds().max(0)))
        }
        None => "—".into(),
    };
    let cost = fmt_cost_cents(run.cost_cents_estimate);
    let is_terminal = matches!(
        status.as_str(),
        "completed" | "failed" | "cancelled" | "timed-out"
    );

    let logs_filtered: Vec<AgentLogLine> = match log_level.read().clone() {
        Some(lvl) => logs.iter().filter(|l| l.level == lvl).cloned().collect(),
        None => logs.clone(),
    };

    rsx! {
        VStack { class: "gap-4 p-4 md:p-6",
            HStack { class: "items-center justify-between",
                HStack { class: "gap-3 items-center",
                    Button {
                        variant: ButtonVariant::Outline,
                        on_click: move |_| on_back.call(()),
                        "← Back"
                    }
                    StatusBadge { variant, label: status.clone() }
                    Heading { level: HeadingLevel::H1, "{name}" }
                }
                if !is_terminal {
                    Button {
                        variant: ButtonVariant::Outline,
                        on_click: move |_| on_cancel.call(run_id),
                        "Cancel run"
                    }
                }
            }

            HStack { class: "gap-4 text-sm text-muted-foreground flex-wrap",
                span { "kind: " span { class: "font-mono text-foreground", "{kind}" } }
                span { "elapsed: " span { class: "font-mono text-foreground", "{elapsed}" } }
                span { "cost: " span { class: "font-mono text-foreground", "{cost}" } }
            }

            details {
                class: "rounded-md border border-border bg-card p-3",
                summary { class: "cursor-pointer text-sm text-muted-foreground", "Prompt" }
                pre { class: "mt-2 whitespace-pre-wrap text-xs font-mono", "{prompt}" }
            }

            // Pane tabs.
            HStack { class: "gap-1 border-b border-border",
                for p in [RunDetailPane::Logs, RunDetailPane::ToolCalls, RunDetailPane::Conversation] {
                    PaneTab { pane, this: p }
                }
            }

            match *pane.read() {
                RunDetailPane::Logs => rsx! {
                    LogsPane {
                        logs: logs_filtered,
                        log_level,
                    }
                },
                RunDetailPane::ToolCalls => rsx! {
                    ToolCallsPane { tool_calls: tool_calls.clone() }
                },
                RunDetailPane::Conversation => rsx! {
                    ConversationPane { messages: conversation.clone() }
                },
            }
        }
    }
}

#[component]
fn PaneTab(pane: Signal<RunDetailPane>, this: RunDetailPane) -> Element {
    let active = *pane.read() == this;
    let class = if active {
        "px-3 py-2 text-sm font-medium border-b-2 border-primary text-foreground"
    } else {
        "px-3 py-2 text-sm text-muted-foreground hover:text-foreground"
    };
    rsx! {
        button {
            class,
            onclick: move |_| pane.set(this),
            "{this.label()}"
        }
    }
}

#[component]
fn LogsPane(logs: Vec<AgentLogLine>, log_level: Signal<Option<String>>) -> Element {
    let level = log_level.read().clone();
    rsx! {
        VStack { class: "gap-2",
            HStack { class: "gap-2 items-center",
                Text { variant: TextVariant::Muted, "Level filter:" }
                select {
                    class: "rounded-md border border-border bg-background p-1 text-xs",
                    value: level.clone().unwrap_or_else(|| "all".into()),
                    onchange: move |e| {
                        let v = e.value();
                        log_level.set(if v == "all" { None } else { Some(v) });
                    },
                    option { value: "all", "all" }
                    option { value: "debug", "debug" }
                    option { value: "info", "info" }
                    option { value: "warn", "warn" }
                    option { value: "error", "error" }
                    option { value: "tool", "tool" }
                    option { value: "assistant", "assistant" }
                }
            }
            div {
                class: "max-h-[60vh] overflow-y-auto rounded-md border border-border bg-card font-mono text-xs",
                if logs.is_empty() {
                    div { class: "p-4 text-muted-foreground", "No log lines yet." }
                } else {
                    for line in logs.iter() {
                        LogLineRow { key: "{line.id}", line: line.clone() }
                    }
                }
            }
        }
    }
}

#[component]
fn LogLineRow(line: AgentLogLine) -> Element {
    let level_class = match line.level.as_str() {
        "error" => "select-none text-rose-500",
        "warn" => "select-none text-amber-500",
        "tool" => "select-none text-violet-500",
        "assistant" => "select-none text-sky-500",
        _ => "select-none text-muted-foreground",
    };
    let time = line.at.format("%H:%M:%S%.3f").to_string();
    let text = line.text.clone();
    let level = line.level.clone();
    let source = line.source.clone();
    rsx! {
        div { class: "flex gap-2 px-2 py-0.5 border-b border-border/40 last:border-b-0",
            span { class: "text-muted-foreground/70 select-none", "{time}" }
            span { class: level_class, "{level}" }
            span { class: "text-muted-foreground/70", "{source}" }
            span { class: "whitespace-pre-wrap break-all", "{text}" }
        }
    }
}

#[component]
fn ToolCallsPane(tool_calls: Vec<ToolCall>) -> Element {
    let mut expanded: Signal<Option<Uuid>> = use_signal(|| None);
    if tool_calls.is_empty() {
        return rsx! {
            EmptyState {
                message: "No tool calls recorded for this run.",
                icon: rsx! { ChevronRight { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "rounded-md border border-border bg-card overflow-hidden",
            for tc in tool_calls.iter().cloned() {
                ToolCallRow {
                    key: "{tc.id}",
                    tool_call: tc.clone(),
                    expanded,
                }
            }
        }
    }
}

#[component]
fn ToolCallRow(tool_call: ToolCall, expanded: Signal<Option<Uuid>>) -> Element {
    let id = tool_call.id;
    let is_open = *expanded.read() == Some(id);
    let status_variant = match tool_call.status.as_str() {
        "ok" => StatusBadgeVariant::Success,
        "error" | "denied" => StatusBadgeVariant::Danger,
        "running" | "pending" => StatusBadgeVariant::Warning,
        _ => StatusBadgeVariant::Neutral,
    };
    let name = tool_call.name.clone();
    let status = tool_call.status.clone();
    let args = tool_call.args_json.clone();
    let result = tool_call.result_json.clone();
    let is_file_edit = matches!(tool_call.name.as_str(), "Edit" | "Write" | "NotebookEdit");
    let (diff_before, diff_after) = if is_file_edit {
        parse_diff_args(&args)
    } else {
        (None, None)
    };

    rsx! {
        div { class: "border-b border-border last:border-b-0",
            div {
                class: "flex items-center gap-2 px-3 py-2 cursor-pointer hover:bg-muted/30",
                onclick: move |_| {
                    expanded.set(if is_open { None } else { Some(id) });
                },
                {if is_open { rsx! { ChevronDown { size: 14 } } } else { rsx! { ChevronRight { size: 14 } } }}
                span { class: "font-medium text-sm", "{name}" }
                StatusBadge { variant: status_variant, label: status }
                span { class: "flex-1" }
            }
            if is_open {
                div { class: "px-3 pb-3 space-y-2 text-xs",
                    if let (Some(b), Some(a)) = (&diff_before, &diff_after) {
                        VStack { class: "gap-1",
                            Text { variant: TextVariant::Muted, "Diff" }
                            InlineDiff { before: b.clone(), after: a.clone() }
                        }
                    }
                    VStack { class: "gap-1",
                        Text { variant: TextVariant::Muted, "args_json" }
                        pre { class: "rounded-md border border-border bg-muted/20 p-2 font-mono whitespace-pre-wrap break-all", "{args}" }
                    }
                    if let Some(r) = result {
                        VStack { class: "gap-1",
                            Text { variant: TextVariant::Muted, "result_json" }
                            pre { class: "rounded-md border border-border bg-muted/20 p-2 font-mono whitespace-pre-wrap break-all", "{r}" }
                        }
                    }
                }
            }
        }
    }
}

/// Tiny JSON probe to pull `before` + `after` out of args_json without
/// a serde_json dep. Looks for `"before":"…"` and `"after":"…"`
/// substrings, decoding standard JSON string escapes. Returns
/// `(None, None)` if either side is missing or malformed — the diff
/// pane just falls back to showing the raw args.
fn parse_diff_args(args_json: &str) -> (Option<String>, Option<String>) {
    let before = extract_json_string(args_json, "\"before\"");
    let after = extract_json_string(args_json, "\"after\"");
    (before, after)
}

fn extract_json_string(s: &str, key: &str) -> Option<String> {
    let start = s.find(key)?;
    let after_key = &s[start + key.len()..];
    let colon = after_key.find(':')?;
    let after_colon = after_key[colon + 1..].trim_start();
    if !after_colon.starts_with('"') {
        return None;
    }
    let body = &after_colon[1..];
    // Walk, handling escapes, until the next unescaped quote.
    let mut out = String::new();
    let mut chars = body.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next()? {
                'n' => out.push('\n'),
                't' => out.push('\t'),
                'r' => out.push('\r'),
                '"' => out.push('"'),
                '\\' => out.push('\\'),
                other => out.push(other),
            }
        } else if c == '"' {
            return Some(out);
        } else {
            out.push(c);
        }
    }
    None
}

#[component]
fn ConversationPane(messages: Vec<ConversationMessage>) -> Element {
    if messages.is_empty() {
        return rsx! {
            EmptyState {
                message: "This run was not spawned from a conversation.",
                icon: rsx! { ChevronRight { size: 32 } },
            }
        };
    }
    rsx! {
        VStack { class: "gap-3",
            for msg in messages.iter().cloned() {
                MessageBubble {
                    key: "{msg.id}",
                    message: msg.clone(),
                }
            }
        }
    }
}

#[component]
fn MessageBubble(message: ConversationMessage) -> Element {
    let (align, bg) = match message.role.as_str() {
        "user" => ("self-end", "bg-primary/15"),
        "assistant" => ("self-start", "bg-muted/40"),
        "tool" => ("self-start", "bg-violet-500/10"),
        _ => ("self-start", "bg-muted/20"),
    };
    let class = format!("{align} max-w-3xl rounded-lg border border-border px-3 py-2 text-sm {bg}");
    let role = message.role.clone();
    let body = message.body.clone();
    rsx! {
        div { class,
            div { class: "text-[10px] uppercase tracking-wide text-muted-foreground mb-1", "{role}" }
            div { class: "whitespace-pre-wrap break-words", "{body}" }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_json_basic() {
        let s = r#"{"path":"a.rs","before":"old text","after":"new text"}"#;
        let (b, a) = parse_diff_args(s);
        assert_eq!(b.as_deref(), Some("old text"));
        assert_eq!(a.as_deref(), Some("new text"));
    }

    #[test]
    fn extract_json_with_escapes() {
        let s = r#"{"before":"line1\nline2","after":"line1\nline2 ✓"}"#;
        let (b, a) = parse_diff_args(s);
        assert_eq!(b.as_deref(), Some("line1\nline2"));
        assert!(a.unwrap().contains('✓'));
    }

    #[test]
    fn extract_missing_returns_none() {
        let s = r#"{"path":"a.rs"}"#;
        let (b, a) = parse_diff_args(s);
        assert!(b.is_none());
        assert!(a.is_none());
    }
}
