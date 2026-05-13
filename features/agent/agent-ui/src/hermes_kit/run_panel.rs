//! `AgentRunPanel` — Activity-tab embed showing the currently-bound
//! agent run + its log feed.
//!
//! Live-tailing model: the parent owns the log signal and re-renders
//! whenever new lines land in the CRDT doc. There's no SSE channel
//! here — this is a dumb component.

use agent_proto::{AgentLogLine, AgentRun};
use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Bot, ExternalLink, X as XIcon};
use fts_ui::prelude::*;
use uuid::Uuid;

use super::common::{
    format_cost_cents, format_relative_time, format_tokens, log_level_class, run_status_variant,
};

#[component]
pub fn AgentRunPanel(
    run: Option<AgentRun>,
    log_lines: Vec<AgentLogLine>,
    on_cancel: EventHandler<Uuid>,
    on_open_external: EventHandler<String>,
    on_dispatch_new: EventHandler<()>,
) -> Element {
    let Some(run) = run else {
        return rsx! {
            VStack { class: "gap-3 items-center py-6",
                EmptyState {
                    message: "No agent run for this task yet.",
                    icon: rsx! { Bot { size: 32 } },
                }
                Button {
                    variant: ButtonVariant::Primary,
                    size: ButtonSize::Small,
                    on_click: move |_| on_dispatch_new.call(()),
                    "+ Dispatch agent"
                }
            }
        };
    };

    // ── Header derivatives ─────────────────────────────────────────────
    let run_id = run.id;
    let status_for_badge = run.status.clone();
    let status_variant = run_status_variant(&status_for_badge);
    let can_cancel = matches!(status_for_badge.as_str(), "queued" | "running" | "blocked");
    let external_url = run.external_url.clone();
    let integration_label = run.integration.clone().unwrap_or_else(|| "agent".into());
    let name = run.name.clone();

    // Elapsed: live-tick once a second while the run is in flight.
    // We use a signal seeded with `Utc::now()` and bump it on a 1s
    // interval. `gloo_timers` isn't in the workspace, so we lean on
    // a simple `use_future` with `gloo_timers`-equivalent semantics
    // via dioxus's own scheduler. For the dumb-component world,
    // re-rendering on log changes is sufficient — full
    // gloo_timers integration is FUTURE.
    let now_signal = use_signal(Utc::now);
    let now = *now_signal.read();
    let elapsed = run
        .started_at
        .map(|s| {
            let secs = now.signed_duration_since(s).num_seconds().max(0);
            let m = secs / 60;
            let s = secs % 60;
            if m >= 60 {
                let h = m / 60;
                let rm = m % 60;
                format!("{h}h {rm}m")
            } else {
                format!("{m}m {s}s")
            }
        })
        .unwrap_or_else(|| "—".into());

    let tokens = run
        .tokens_used
        .map(format_tokens)
        .unwrap_or_else(|| "0 tokens".into());
    let cost = run
        .cost_cents
        .map(format_cost_cents)
        .unwrap_or_else(|| "$0.00".into());

    // ── Logs: take the last 200, oldest first ──────────────────────────
    let mut sorted = log_lines;
    sorted.sort_by_key(|l| l.at);
    let take_from = sorted.len().saturating_sub(200);
    let lines: Vec<AgentLogLine> = sorted.into_iter().skip(take_from).collect();
    let _lines_len = lines.len();

    rsx! {
        VStack { class: "gap-3",
            // Header row
            HStack { class: "items-center justify-between gap-2 flex-wrap",
                HStack { class: "items-center gap-2 flex-wrap",
                    StatusBadge { variant: status_variant, label: status_for_badge.clone() }
                    span { class: "text-sm font-medium text-foreground", "{name}" }
                    span { class: "text-xs text-muted-foreground", "· {elapsed}" }
                    span { class: "text-xs text-muted-foreground", "· {tokens}" }
                    span { class: "text-xs text-muted-foreground", "· {cost}" }
                }
                HStack { class: "items-center gap-2",
                    if let Some(url) = external_url.clone() {
                        Button {
                            variant: ButtonVariant::Outline,
                            size: ButtonSize::Small,
                            on_click: move |_| on_open_external.call(url.clone()),
                            ExternalLink { size: 12 }
                            " Open in {integration_label}"
                        }
                    }
                    if can_cancel {
                        Button {
                            variant: ButtonVariant::Destructive,
                            size: ButtonSize::Small,
                            on_click: move |_| on_cancel.call(run_id),
                            XIcon { size: 12 }
                            " Cancel"
                        }
                    }
                }
            }

            Divider {}

            // Log feed
            ScrollArea { class: "max-h-[28rem] w-full",
                div { class: "flex flex-col gap-0.5 font-mono text-[11px] leading-relaxed",
                    if lines.is_empty() {
                        div { class: "text-xs text-muted-foreground", "Waiting for log output…" }
                    } else {
                        for line in lines.into_iter() {
                            {
                                let ts = line.at.format("%H:%M:%S").to_string();
                                let level_cls = log_level_class(&line.level);
                                let src = line.source.clone();
                                let txt = line.text.clone();
                                rsx! {
                                    div { key: "{line.id}", class: "{level_cls} whitespace-pre-wrap",
                                        span { class: "text-muted-foreground", "[{ts} {src}] " }
                                        span { "{txt}" }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Footer relative-time hint when the run is finished.
            if let Some(completed) = run.completed_at {
                Text {
                    variant: TextVariant::Muted,
                    "Finished {format_relative_time(completed, now)}."
                }
            }
        }
    }
}
