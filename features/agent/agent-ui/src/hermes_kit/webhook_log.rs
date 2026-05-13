//! `WebhookEventLog` — debug surface for inbound webhook deliveries.
//!
//! Server-only intent: production routes won't expose this. The
//! component is fully dumb — caller hands in a `Vec<WebhookInboxRow>`
//! and a `WebhookFilter` selection, the component emits change events
//! up.

use std::collections::HashMap;

use chrono::{DateTime, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ChevronDown, ChevronRight};
use fts_ui::prelude::*;
use uuid::Uuid;

use super::common::format_relative_time;

#[derive(Clone, PartialEq, Default)]
pub struct WebhookFilter {
    pub integration: Option<String>,
    pub signature_only: bool,
    pub failed_only: bool,
}

#[derive(Clone, PartialEq)]
pub struct WebhookInboxRow {
    pub id: Uuid,
    pub integration: String,
    pub received_at: DateTime<Utc>,
    pub headers: HashMap<String, String>,
    pub body_preview: String,
    pub signature_ok: bool,
    pub processed_at: Option<DateTime<Utc>>,
    pub error: Option<String>,
}

#[component]
pub fn WebhookEventLog(
    events: Vec<WebhookInboxRow>,
    filter: WebhookFilter,
    on_filter_change: EventHandler<WebhookFilter>,
) -> Element {
    let mut expanded = use_signal::<Option<Uuid>>(|| None);

    let now = Utc::now();
    let integration_signal = use_signal(|| filter.integration.clone().unwrap_or_default());
    let signature_only = filter.signature_only;
    let failed_only = filter.failed_only;

    let filtered: Vec<WebhookInboxRow> = events
        .into_iter()
        .filter(|row| {
            if let Some(int) = &filter.integration {
                if &row.integration != int {
                    return false;
                }
            }
            if signature_only && !row.signature_ok {
                return false;
            }
            if failed_only && row.error.is_none() {
                return false;
            }
            true
        })
        .collect();

    rsx! {
        VStack { class: "gap-4",
            SectionHeader { label: "Webhook deliveries".to_string() }
            Text { variant: TextVariant::Muted,
                "Inbound webhook payloads from configured integrations. Server-only debug view."
            }

            // Filter row
            HStack { class: "items-center gap-3 flex-wrap",
                div { class: "flex flex-col gap-1",
                    Label { "Integration" }
                    {
                        let on_filter = on_filter_change;
                        let cur = filter.clone();
                        rsx! {
                            NativeSelect {
                                value: integration_signal,
                                on_change: move |_| {
                                    let v = integration_signal.read().clone();
                                    let next = WebhookFilter {
                                        integration: if v.is_empty() { None } else { Some(v) },
                                        signature_only: cur.signature_only,
                                        failed_only: cur.failed_only,
                                    };
                                    on_filter.call(next);
                                },
                                NativeSelectOption { value: "".to_string(), "All" }
                                NativeSelectOption { value: "github".to_string(), "github" }
                                NativeSelectOption { value: "hermes".to_string(), "hermes" }
                            }
                        }
                    }
                }
                Button {
                    variant: if signature_only { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: {
                        let cur = filter.clone();
                        move |_| on_filter_change.call(WebhookFilter {
                            integration: cur.integration.clone(),
                            signature_only: !cur.signature_only,
                            failed_only: cur.failed_only,
                        })
                    },
                    "Signature OK only"
                }
                Button {
                    variant: if failed_only { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: {
                        let cur = filter.clone();
                        move |_| on_filter_change.call(WebhookFilter {
                            integration: cur.integration.clone(),
                            signature_only: cur.signature_only,
                            failed_only: !cur.failed_only,
                        })
                    },
                    "Failed only"
                }
            }

            // Rows
            div { class: "flex flex-col gap-1",
                if filtered.is_empty() {
                    EmptyState { message: "No webhook events match the filter yet." }
                } else {
                    for row in filtered.into_iter() {
                        {
                            let row_id = row.id;
                            let is_open = *expanded.read() == Some(row_id);
                            let when = format_relative_time(row.received_at, now);
                            let integration = row.integration.clone();
                            let body = row.body_preview.clone();
                            let headers = row.headers.clone();
                            let error = row.error.clone();
                            let sig_variant = if row.signature_ok {
                                StatusBadgeVariant::Success
                            } else {
                                StatusBadgeVariant::Danger
                            };
                            let proc_variant = if row.error.is_some() {
                                StatusBadgeVariant::Danger
                            } else if row.processed_at.is_some() {
                                StatusBadgeVariant::Success
                            } else {
                                StatusBadgeVariant::Neutral
                            };
                            let proc_label = if let Some(_e) = &row.error {
                                "errored".to_string()
                            } else if row.processed_at.is_some() {
                                "processed".to_string()
                            } else {
                                "pending".to_string()
                            };
                            rsx! {
                                div { key: "{row_id}",
                                    class: "rounded-md border border-border bg-card",
                                    button {
                                        r#type: "button",
                                        class: "w-full flex items-center gap-2 px-3 py-2 text-left hover:bg-accent/30",
                                        onclick: move |_| {
                                            if is_open { expanded.set(None) } else { expanded.set(Some(row_id)) }
                                        },
                                        if is_open {
                                            ChevronDown { size: 12 }
                                        } else {
                                            ChevronRight { size: 12 }
                                        }
                                        span { class: "text-xs text-muted-foreground w-20", "{when}" }
                                        Badge { variant: BadgeVariant::Secondary, "{integration}" }
                                        StatusBadge {
                                            variant: sig_variant,
                                            label: if row.signature_ok { "sig ok".to_string() } else { "sig fail".to_string() },
                                        }
                                        StatusBadge { variant: proc_variant, label: proc_label }
                                    }
                                    if is_open {
                                        div { class: "border-t border-border px-3 py-2 flex flex-col gap-2",
                                            div {
                                                Label { "Headers" }
                                                div { class: "rounded bg-muted/40 p-2 font-mono text-[11px] whitespace-pre-wrap",
                                                    {
                                                        let mut sorted: Vec<_> = headers.iter().collect();
                                                        sorted.sort_by(|a, b| a.0.cmp(b.0));
                                                        let joined = sorted
                                                            .iter()
                                                            .map(|(k, v)| format!("{k}: {v}"))
                                                            .collect::<Vec<_>>()
                                                            .join("\n");
                                                        rsx! { "{joined}" }
                                                    }
                                                }
                                            }
                                            div {
                                                Label { "Body preview" }
                                                div { class: "rounded bg-muted/40 p-2 font-mono text-[11px] whitespace-pre-wrap",
                                                    "{body}"
                                                }
                                            }
                                            if let Some(err) = error {
                                                div { class: "text-xs text-rose-400", "Error: {err}" }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
