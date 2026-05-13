//! `ManualTimeEntryDialog` — modal for after-the-fact entries.

use chrono::{Local, NaiveDate, TimeZone, Utc};
use dioxus::prelude::*;
use fts_ui::components::ComboboxItemData;
use fts_ui::prelude::*;
use invoice_proto::Client;
use project_proto::Project;
use timer_proto::{TimeEntry, TimeEntryCreate};
use uuid::Uuid;

#[derive(Props, Clone, PartialEq)]
pub struct ManualTimeEntryDialogProps {
    pub open: bool,
    pub projects: Vec<Project>,
    pub clients: Vec<Client>,
    pub initial: Option<TimeEntry>,
    pub on_submit: EventHandler<TimeEntryCreate>,
    pub on_close: EventHandler<()>,
}

#[component]
pub fn ManualTimeEntryDialog(props: ManualTimeEntryDialogProps) -> Element {
    if !props.open {
        return rsx! {};
    }

    let initial = props.initial.clone();
    let today = initial
        .as_ref()
        .map(|e| e.start_time.date_naive())
        .unwrap_or_else(|| Utc::now().date_naive());

    let mut date_str = use_signal(|| today.format("%Y-%m-%d").to_string());
    let mut start_str = use_signal(|| {
        initial
            .as_ref()
            .map(|e| {
                Local
                    .from_utc_datetime(&e.start_time.naive_utc())
                    .format("%H:%M")
                    .to_string()
            })
            .unwrap_or_else(|| "09:00".to_string())
    });
    let mut end_str = use_signal(|| {
        initial
            .as_ref()
            .and_then(|e| e.end_time)
            .map(|t| {
                Local
                    .from_utc_datetime(&t.naive_utc())
                    .format("%H:%M")
                    .to_string()
            })
            .unwrap_or_else(|| "10:00".to_string())
    });
    let description = use_signal(|| {
        initial
            .as_ref()
            .and_then(|e| e.description.clone())
            .unwrap_or_default()
    });
    let billable = use_signal(|| initial.as_ref().map(|e| e.billable).unwrap_or(false));

    let initial_proj = initial.as_ref().and_then(|e| e.project_id);
    let project_value = use_signal(|| initial_proj.map(|id| id.to_string()).unwrap_or_default());

    let project_items: Vec<ComboboxItemData> = props
        .projects
        .iter()
        .map(|p| ComboboxItemData::new(p.id.to_string(), p.name.clone()))
        .collect();

    let projects_for_submit = props.projects.clone();
    let on_submit = props.on_submit;
    let on_close = props.on_close;
    let do_submit = use_callback(move |_: ()| {
        let parse_date =
            |s: &str| -> Option<NaiveDate> { NaiveDate::parse_from_str(s, "%Y-%m-%d").ok() };
        let parse_time = |s: &str| -> Option<(u32, u32)> {
            let (h, m) = s.split_once(':')?;
            Some((h.trim().parse().ok()?, m.trim().parse().ok()?))
        };
        let Some(d) = parse_date(&date_str.read()) else {
            return;
        };
        let Some((sh, sm)) = parse_time(&start_str.read()) else {
            return;
        };
        let (eh, em) = parse_time(&end_str.read()).unwrap_or((sh, sm));
        let Some(start_naive) = d.and_hms_opt(sh, sm, 0) else {
            return;
        };
        let Some(end_naive) = d.and_hms_opt(eh, em, 0) else {
            return;
        };
        let Some(start_local) = Local.from_local_datetime(&start_naive).single() else {
            return;
        };
        let Some(end_local) = Local.from_local_datetime(&end_naive).single() else {
            return;
        };
        let start_utc = start_local.with_timezone(&Utc);
        let end_utc = end_local.with_timezone(&Utc);

        let project_id: Option<Uuid> = {
            let v = project_value.read().clone();
            if v.is_empty() {
                None
            } else {
                Uuid::parse_str(&v).ok()
            }
        };
        let client_id = project_id
            .and_then(|pid| projects_for_submit.iter().find(|p| p.id == pid))
            .and_then(|_| None::<Uuid>);

        let desc_text = description.read().clone();
        let payload = TimeEntryCreate {
            task_id: None,
            project_id,
            client_id,
            user: None,
            start_time: start_utc,
            end_time: Some(end_utc),
            description: if desc_text.trim().is_empty() {
                None
            } else {
                Some(desc_text)
            },
            billable: *billable.read(),
            manual: true,
            billable_rate_cents: None,
            tags: Vec::new(),
            invoiced_at: None,
        };
        on_submit.call(payload);
        on_close.call(());
    });

    let on_cancel = props.on_close;
    let on_overlay = props.on_close;

    rsx! {
        Dialog {
            open: true,
            is_modal: true,
            on_open_change: move |open: bool| {
                if !open {
                    on_overlay.call(());
                }
            },
            DialogHeader {
                DialogTitle { "Manual time entry" }
                DialogDescription { "Log a window of time after the fact." }
            }

            div { class: "flex flex-col gap-3",
                onkeydown: move |e: KeyboardEvent| {
                    if e.key() == Key::Enter && (e.modifiers().meta() || e.modifiers().ctrl()) {
                        do_submit.call(());
                    }
                },

                Label { "Date" }
                input {
                    r#type: "date",
                    class: "h-9 rounded border bg-background px-2 text-sm",
                    value: "{date_str}",
                    oninput: move |e| date_str.set(e.value()),
                }

                div { class: "grid grid-cols-2 gap-3",
                    div { class: "flex flex-col gap-1",
                        Label { "Start" }
                        input {
                            r#type: "time",
                            class: "h-9 rounded border bg-background px-2 text-sm",
                            value: "{start_str}",
                            oninput: move |e| start_str.set(e.value()),
                        }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "End" }
                        input {
                            r#type: "time",
                            class: "h-9 rounded border bg-background px-2 text-sm",
                            value: "{end_str}",
                            oninput: move |e| end_str.set(e.value()),
                        }
                    }
                }

                Label { "Project" }
                Combobox {
                    value: project_value,
                    ComboboxTrigger { placeholder: "Select project" }
                    ComboboxContent {
                        items: project_items,
                        empty: rsx! {
                            div { class: "py-4 text-center text-xs text-muted-foreground",
                                "No projects."
                            }
                        }
                    }
                }

                Label { "Description" }
                Input {
                    value: description,
                    placeholder: "What did you work on?",
                }

                HStack { class: "items-center gap-2",
                    Switch { checked: billable }
                    Text { variant: TextVariant::Muted, "Billable" }
                }
            }

            DialogFooter {
                Button {
                    variant: ButtonVariant::Ghost,
                    on_click: move |_| on_cancel.call(()),
                    "Cancel"
                }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| do_submit.call(()),
                    "Save entry"
                }
            }
        }
    }
}
