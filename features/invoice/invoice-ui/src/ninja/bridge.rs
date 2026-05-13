//! `TimeEntriesToInvoiceDialog` — bridges the timer into invoicing.
//! Picks a client + date range, lists their unbilled time entries,
//! previews the proposed `InvoiceLine`s and emits a `BillingProposal`.

use std::collections::{BTreeMap, HashMap};

use chrono::NaiveDate;
use dioxus::prelude::*;
use fts_ui::components::ComboboxItemData;
use fts_ui::prelude::*;
use invoice_proto::Client;
use project_proto::Project;
use timer_proto::TimeEntry;
use uuid::Uuid;

use super::common::{format_money, format_qty_thousandths};

#[derive(Clone, Debug, PartialEq)]
pub struct ProposedLine {
    pub description: String,
    pub project_id: Option<Uuid>,
    pub quantity_thousandths: i64,
    pub unit_price_cents: i64,
    pub source_entry_ids: Vec<Uuid>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BillingProposal {
    pub client_id: Uuid,
    pub lines: Vec<ProposedLine>,
    pub source_entry_ids: Vec<Uuid>,
    pub currency: String,
}

#[derive(Props, Clone, PartialEq)]
pub struct TimeEntriesToInvoiceDialogProps {
    pub open: bool,
    #[props(default)]
    pub initial_client_id: Option<Uuid>,
    pub clients: Vec<Client>,
    pub unbilled_entries: Vec<TimeEntry>,
    pub projects_by_id: HashMap<Uuid, Project>,
    pub rate_by_project: HashMap<Uuid, i64>,
    pub on_confirm: EventHandler<BillingProposal>,
    pub on_close: EventHandler<()>,
}

#[component]
pub fn TimeEntriesToInvoiceDialog(props: TimeEntriesToInvoiceDialogProps) -> Element {
    if !props.open {
        return rsx! {};
    }

    let client_value = use_signal(|| {
        props
            .initial_client_id
            .map(|id| id.to_string())
            .unwrap_or_default()
    });
    let from_str = use_signal(String::new);
    let to_str = use_signal(String::new);

    // Per-entry selection (default: all in scope checked).
    let unbilled = props.unbilled_entries.clone();
    let initial_selected: Vec<Uuid> = unbilled.iter().map(|e| e.id).collect();
    let selected = use_signal(|| initial_selected);

    let client_items: Vec<ComboboxItemData> = props
        .clients
        .iter()
        .map(|c| ComboboxItemData::new(c.id.to_string(), c.name.clone()))
        .collect();

    let parse_date = |s: &str| -> Option<NaiveDate> {
        if s.trim().is_empty() {
            None
        } else {
            NaiveDate::parse_from_str(s, "%Y-%m-%d").ok()
        }
    };

    let current_client_id: Option<Uuid> = Uuid::parse_str(&client_value.read()).ok();
    let from_d = parse_date(&from_str.read());
    let to_d = parse_date(&to_str.read());

    let in_scope: Vec<TimeEntry> = unbilled
        .iter()
        .filter(|e| {
            if let Some(cid) = current_client_id {
                if e.client_id != Some(cid) {
                    return false;
                }
            }
            let d = e.start_time.date_naive();
            if let Some(from) = from_d {
                if d < from {
                    return false;
                }
            }
            if let Some(to) = to_d {
                if d > to {
                    return false;
                }
            }
            true
        })
        .cloned()
        .collect();

    let selected_entries: Vec<TimeEntry> = in_scope
        .iter()
        .filter(|e| selected.read().contains(&e.id))
        .cloned()
        .collect();

    let client =
        current_client_id.and_then(|id| props.clients.iter().find(|c| c.id == id).cloned());
    let client_default_rate = client.as_ref().and_then(|c| c.default_rate_cents);
    let currency = client
        .as_ref()
        .map(|c| c.currency.clone())
        .unwrap_or_else(|| "USD".into());

    let proposed_lines = propose_lines_from_entries(
        &selected_entries,
        &props.projects_by_id,
        &props.rate_by_project,
        client_default_rate,
    );

    let preview_total_cents: i64 = proposed_lines
        .iter()
        .map(|l| l.quantity_thousandths.saturating_mul(l.unit_price_cents) / 1000)
        .sum();

    let on_confirm = props.on_confirm;
    let on_close = props.on_close;
    let proposed_for_submit = proposed_lines.clone();
    let currency_for_submit = currency.clone();

    let do_confirm = use_callback(move |_: ()| {
        let Some(cid) = current_client_id else {
            return;
        };
        if proposed_for_submit.is_empty() {
            return;
        }
        let source_entry_ids: Vec<Uuid> = proposed_for_submit
            .iter()
            .flat_map(|l| l.source_entry_ids.clone())
            .collect();
        let payload = BillingProposal {
            client_id: cid,
            lines: proposed_for_submit.clone(),
            source_entry_ids,
            currency: currency_for_submit.clone(),
        };
        on_confirm.call(payload);
        on_close.call(());
    });

    rsx! {
        Dialog {
            open: true,
            is_modal: true,
            on_open_change: move |open: bool| {
                if !open {
                    on_close.call(());
                }
            },
            DialogHeader {
                DialogTitle { "Bill time entries" }
                DialogDescription { "Group unbilled time into a draft invoice." }
            }

            div { class: "flex flex-col gap-3",
                div { class: "grid grid-cols-3 gap-3",
                    div { class: "flex flex-col gap-1",
                        Label { "Client" }
                        Combobox {
                            value: client_value,
                            ComboboxTrigger { placeholder: "Select client".to_string() }
                            ComboboxContent { items: client_items }
                        }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "From" }
                        input {
                            r#type: "date",
                            class: "h-9 rounded border bg-background px-2 text-sm",
                            value: "{from_str}",
                            oninput: move |e| from_str.clone().set(e.value()),
                        }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "To" }
                        input {
                            r#type: "date",
                            class: "h-9 rounded border bg-background px-2 text-sm",
                            value: "{to_str}",
                            oninput: move |e| to_str.clone().set(e.value()),
                        }
                    }
                }

                Divider {}

                // Entry list
                div { class: "flex flex-col gap-1 max-h-64 overflow-auto",
                    div { class: "grid items-center gap-2 px-1 py-1 text-xs text-muted-foreground border-b",
                        style: "grid-template-columns: 24px 100px 1fr 110px 90px;",
                        span {}
                        span { "Date" }
                        span { "Description" }
                        span { "Project" }
                        span { class: "text-right", "Hours" }
                    }
                    if in_scope.is_empty() {
                        div { class: "py-6 text-center text-sm text-muted-foreground",
                            "No unbilled time matches the current filter."
                        }
                    }
                    for e in in_scope.iter().cloned() {
                        {
                            let id = e.id;
                            let selected_now = selected.read().contains(&id);
                            let checked_signal = use_signal(|| selected_now);
                            let desc = e.description.clone().unwrap_or_default();
                            let date = e.start_time.format("%Y-%m-%d").to_string();
                            let project_name = e
                                .project_id
                                .and_then(|pid| props.projects_by_id.get(&pid))
                                .map(|p| p.name.clone())
                                .unwrap_or_else(|| "—".into());
                            let secs = e
                                .end_time
                                .map(|end| (end - e.start_time).num_seconds())
                                .unwrap_or(0)
                                .max(0);
                            let hours_thou = secs * 1000 / 3600;
                            rsx! {
                                div { key: "{id}",
                                    class: "grid items-center gap-2 px-1 py-1 text-sm border-b",
                                    style: "grid-template-columns: 24px 100px 1fr 110px 90px;",
                                    Checkbox {
                                        checked: checked_signal,
                                        on_change: {
                                            let mut selected = selected;
                                            move |on: bool| {
                                                let mut next: Vec<Uuid> = selected.read().clone();
                                                if on {
                                                    if !next.contains(&id) {
                                                        next.push(id);
                                                    }
                                                } else {
                                                    next.retain(|x| *x != id);
                                                }
                                                selected.set(next);
                                            }
                                        },
                                    }
                                    span { class: "text-xs text-muted-foreground tabular-nums", "{date}" }
                                    span { class: "truncate", "{desc}" }
                                    span { class: "text-xs text-muted-foreground truncate", "{project_name}" }
                                    span { class: "text-right tabular-nums font-mono text-xs",
                                        "{format_qty_thousandths(hours_thou)}"
                                    }
                                }
                            }
                        }
                    }
                }

                Divider {}

                // Preview proposed lines
                div { class: "flex flex-col gap-1",
                    div { class: "text-xs text-muted-foreground uppercase tracking-wide",
                        "Will create {proposed_lines.len()} line(s)"
                    }
                    for line in proposed_lines.iter() {
                        div { key: "{line.description}-{line.project_id:?}",
                            class: "grid items-center gap-2 px-1 py-1 text-sm border-b",
                            style: "grid-template-columns: 1fr 80px 110px 110px;",
                            span { class: "truncate", "{line.description}" }
                            span { class: "text-right tabular-nums text-xs",
                                "{format_qty_thousandths(line.quantity_thousandths)}"
                            }
                            span { class: "text-right tabular-nums text-xs",
                                "{format_money(line.unit_price_cents, &currency)}"
                            }
                            span { class: "text-right tabular-nums text-xs",
                                "{format_money(line.quantity_thousandths.saturating_mul(line.unit_price_cents) / 1000, &currency)}"
                            }
                        }
                    }
                    div { class: "flex justify-end pt-1 text-sm font-medium",
                        "Total: {format_money(preview_total_cents, &currency)}"
                    }
                }
            }

            DialogFooter {
                Button {
                    variant: ButtonVariant::Ghost,
                    on_click: move |_| on_close.call(()),
                    "Cancel"
                }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| do_confirm.call(()),
                    "Create draft invoice"
                }
            }
        }
    }
}

/// Group entries by `(project_id, description)`, sum hours, and pick a
/// rate (`rate_by_project[pid]` if present, else `client_default_rate`,
/// else any per-entry `billable_rate_cents`, else 0).
pub fn propose_lines_from_entries(
    entries: &[TimeEntry],
    projects_by_id: &HashMap<Uuid, Project>,
    rate_by_project: &HashMap<Uuid, i64>,
    client_default_rate: Option<i64>,
) -> Vec<ProposedLine> {
    // BTreeMap so output ordering is stable for snapshot tests / UI.
    let mut groups: BTreeMap<(Option<Uuid>, String), ProposedLine> = BTreeMap::new();
    for e in entries {
        let secs = e
            .end_time
            .map(|end| (end - e.start_time).num_seconds().max(0))
            .unwrap_or(0);
        let hours_thou = secs * 1000 / 3600;
        if hours_thou == 0 {
            continue;
        }
        let description = e.description.clone().unwrap_or_else(|| {
            e.project_id
                .and_then(|pid| projects_by_id.get(&pid))
                .map(|p| p.name.clone())
                .unwrap_or_else(|| "Time entry".into())
        });
        let unit_price = e
            .project_id
            .and_then(|pid| rate_by_project.get(&pid).copied())
            .or(client_default_rate)
            .or(e.billable_rate_cents.map(|v| v as i64))
            .unwrap_or(0);

        let key = (e.project_id, description.clone());
        let entry = groups.entry(key).or_insert(ProposedLine {
            description,
            project_id: e.project_id,
            quantity_thousandths: 0,
            unit_price_cents: unit_price,
            source_entry_ids: Vec::new(),
        });
        entry.quantity_thousandths += hours_thou;
        entry.source_entry_ids.push(e.id);
    }
    groups.into_values().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::{Duration, Utc};

    fn entry(client_id: Option<Uuid>, project_id: Option<Uuid>, hours: i64) -> TimeEntry {
        let start = Utc::now();
        TimeEntry {
            id: Uuid::new_v4(),
            task_id: None,
            project_id,
            client_id,
            user: None,
            start_time: start,
            end_time: Some(start + Duration::hours(hours)),
            description: Some("Work".into()),
            billable: true,
            manual: false,
            billable_rate_cents: None,
            tags: vec![],
            invoiced_at: None,
            created_at: start,
            updated_at: start,
        }
    }

    #[test]
    fn groups_by_project_description() {
        let pid = Uuid::new_v4();
        let cid = Uuid::new_v4();
        let entries = vec![
            entry(Some(cid), Some(pid), 2),
            entry(Some(cid), Some(pid), 3),
            entry(Some(cid), None, 1),
        ];
        let projects = HashMap::new();
        let mut rates: HashMap<Uuid, i64> = HashMap::new();
        rates.insert(pid, 15_000);
        let lines = propose_lines_from_entries(&entries, &projects, &rates, Some(10_000));
        assert_eq!(lines.len(), 2);
        let project_line = lines.iter().find(|l| l.project_id == Some(pid)).unwrap();
        assert_eq!(project_line.quantity_thousandths, 5_000);
        assert_eq!(project_line.unit_price_cents, 15_000);
        let other = lines.iter().find(|l| l.project_id.is_none()).unwrap();
        assert_eq!(other.unit_price_cents, 10_000);
    }
}
