//! Agent feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! - [`AgentRunList`]  — full collection view, dispatches `on_delete`
//! - [`AgentRunRow`]   — single-row presentation
//! - [`AgentRunCreateForm`] — minimal new-run form, emits the create payload

use agent_proto::{AgentRun, AgentRunCreate};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Activity, Bot, CircleCheck, Coins, Plus, Trash2, Zap};
use fts_ui::prelude::*;
use uuid::Uuid;

/// Purpose-built dashboard for agent runs — header, summary stats,
/// status filter tabs, create form, and the list.
#[component]
pub fn AgentRunDashboard(
    items: Vec<AgentRun>,
    status: String,
    on_create: EventHandler<AgentRunCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let mut filter = use_signal(|| "all".to_string());

    let total = items.len();
    let running = items.iter().filter(|r| r.status == "running").count();
    let completed = items
        .iter()
        .filter(|r| r.status == "completed" || r.status == "success")
        .count();
    let failed = items
        .iter()
        .filter(|r| r.status == "failed" || r.status == "error")
        .count();
    let tokens: u64 = items
        .iter()
        .filter_map(|r| r.tokens_used)
        .map(u64::from)
        .sum();
    let cost_cents: u64 = items
        .iter()
        .filter_map(|r| r.cost_cents)
        .map(u64::from)
        .sum();
    let cost_usd = (cost_cents as f64) / 100.0;

    let f = filter.read().clone();
    let filtered: Vec<AgentRun> = items
        .iter()
        .filter(|r| match f.as_str() {
            "running" => r.status == "running",
            "completed" => r.status == "completed" || r.status == "success",
            "failed" => r.status == "failed" || r.status == "error",
            _ => true,
        })
        .cloned()
        .collect();

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Agent runs".to_string(),
                trailing: rsx! {
                    StatusBadge {
                        variant: StatusBadgeVariant::Neutral,
                        label: status.clone(),
                    }
                },
            }
            HStack { class: "gap-2 items-center",
                Bot { size: 22 }
                Heading { level: HeadingLevel::H2, "AI agent runs" }
            }
            Text { variant: TextVariant::Muted, "Track prompt invocations, token spend, and run lifecycles across your workspace." }

            div { class: "grid grid-cols-2 md:grid-cols-4 gap-3",
                Card { class: "border-l-4 border-l-violet-500",
                    CardHeader {
                        CardDescription { "Total runs" }
                        CardTitle { class: "text-2xl", "{total}" }
                    }
                    CardContent { class: "flex items-center gap-2 text-violet-500",
                        Activity { size: 16 }
                        Text { variant: TextVariant::Muted, "lifetime" }
                    }
                }
                Card { class: "border-l-4 border-l-amber-500",
                    CardHeader {
                        CardDescription { "Running now" }
                        CardTitle { class: "text-2xl", "{running}" }
                    }
                    CardContent { class: "flex items-center gap-2 text-amber-500",
                        Zap { size: 16 }
                        Text { variant: TextVariant::Muted, "in flight" }
                    }
                }
                Card { class: "border-l-4 border-l-emerald-500",
                    CardHeader {
                        CardDescription { "Completed" }
                        CardTitle { class: "text-2xl", "{completed}" }
                    }
                    CardContent { class: "flex items-center gap-2 text-emerald-500",
                        CircleCheck { size: 16 }
                        Text { variant: TextVariant::Muted, "{failed} failed" }
                    }
                }
                Card { class: "border-l-4 border-l-sky-500",
                    CardHeader {
                        CardDescription { "Spend" }
                        CardTitle { class: "text-2xl", "${cost_usd:.2}" }
                    }
                    CardContent { class: "flex items-center gap-2 text-sky-500",
                        Coins { size: 16 }
                        Text { variant: TextVariant::Muted, "{tokens} tokens" }
                    }
                }
            }

            Divider {}

            AgentRunCreateForm { on_submit: move |p| on_create.call(p) }

            HStack { class: "items-center justify-between",
                Heading { level: HeadingLevel::H3, "Runs" }
                SegmentedControl {
                    value: filter.read().clone(),
                    on_change: move |v: String| filter.set(v),
                    options: vec![
                        ("all".to_string(), "All".to_string()),
                        ("running".to_string(), "Running".to_string()),
                        ("completed".to_string(), "Completed".to_string()),
                        ("failed".to_string(), "Failed".to_string()),
                    ],
                }
            }
            AgentRunList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}

#[component]
pub fn AgentRunList(items: Vec<AgentRun>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No agent runs yet. Add one above.",
                icon: rsx! { Bot { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for run in items.iter().cloned() {
                AgentRunRow {
                    key: "{run.id}",
                    run: run.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn AgentRunRow(run: AgentRun, on_delete: EventHandler<Uuid>) -> Element {
    let id = run.id;
    let status_variant = match run.status.as_str() {
        "completed" | "success" => StatusBadgeVariant::Success,
        "failed" | "error" => StatusBadgeVariant::Danger,
        "running" => StatusBadgeVariant::Warning,
        _ => StatusBadgeVariant::Neutral,
    };
    let kind = run.kind.clone();
    let status = run.status.clone();
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{run.name}" }
                ItemDescription { "{kind}" }
            }
            ItemActions { class: "gap-2",
                StatusBadge { variant: status_variant, label: status }
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
pub fn AgentRunCreateForm(on_submit: EventHandler<AgentRunCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut kind = use_signal(String::new);
    let mut prompt = use_signal(String::new);
    rsx! {
        Card {
            CardHeader {
                CardTitle { "New agent run" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: name,
                        placeholder: "Name (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: kind,
                        placeholder: "Kind (required)",
                        class: "flex-1 min-w-40",
                    }
                }
                Input {
                    value: prompt,
                    placeholder: "Prompt (required)",
                    class: "w-full",
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let n = name.read().clone();
                            let k = kind.read().clone();
                            let p = prompt.read().clone();
                            if n.trim().is_empty() || k.trim().is_empty() || p.trim().is_empty() {
                                return;
                            }
                            let payload = AgentRunCreate {
                                name: n,
                                kind: k,
                                prompt: p,
                                status: "queued".into(),
                                task_id: None,
                                started_at: None,
                                completed_at: None,
                                result: None,
                                error_message: None,
                                tokens_used: None,
                                cost_cents: None,
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            name.set(String::new());
                            kind.set(String::new());
                            prompt.set(String::new());
                        },
                        Plus { size: 14 }
                        " Add run"
                    }
                }
            }
        }
    }
}
