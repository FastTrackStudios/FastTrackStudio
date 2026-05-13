//! Create/edit dialog for a calendar event.
//!
//! `EventDialog` is fully controlled — caller owns `open` + supplies a
//! `Mode::Create { start, end }` or `Mode::Edit(event)`. Emits one of
//! `on_save_create`, `on_save_update`, or `on_delete`.

use calendar_proto::{CalendarEvent, CalendarEventCreate, CalendarEventUpdate};
use chrono::{DateTime, Duration, Local, NaiveDateTime, TimeZone, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Trash2, X};
use fts_ui::prelude::*;
use uuid::Uuid;

/// Dialog mode — Create with default times, or Edit on an existing event.
#[derive(Clone, PartialEq)]
pub enum EventDialogMode {
    Create {
        start: DateTime<Utc>,
        end: DateTime<Utc>,
        all_day: bool,
    },
    Edit(CalendarEvent),
}

#[component]
pub fn EventDialog(
    open: bool,
    mode: EventDialogMode,
    editable: bool,
    on_close: EventHandler<()>,
    on_save_create: EventHandler<CalendarEventCreate>,
    on_save_update: EventHandler<(Uuid, CalendarEventUpdate)>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    // Seed state from mode.
    let (
        init_title,
        init_desc,
        init_start,
        init_end,
        init_all_day,
        init_loc,
        init_status,
        init_tags,
        edit_id,
    ) = match &mode {
        EventDialogMode::Create {
            start,
            end,
            all_day,
        } => (
            String::new(),
            String::new(),
            *start,
            *end,
            *all_day,
            String::new(),
            "confirmed".to_string(),
            String::new(),
            None,
        ),
        EventDialogMode::Edit(e) => (
            e.title.clone(),
            e.description.clone().unwrap_or_default(),
            e.start_at,
            e.end_at,
            e.all_day,
            e.location_text.clone().unwrap_or_default(),
            e.status.clone(),
            e.tags.join(", "),
            Some(e.id),
        ),
    };

    let mut title = use_signal(|| init_title.clone());
    let mut description = use_signal(|| init_desc.clone());
    let mut start_str = use_signal(|| fmt_for_input(init_start));
    let mut end_str = use_signal(|| fmt_for_input(init_end));
    let mut all_day = use_signal(|| init_all_day);
    let mut location = use_signal(|| init_loc.clone());
    let mut status_sig = use_signal(|| init_status.clone());
    let mut tags_sig = use_signal(|| init_tags.clone());

    // Re-seed when `mode` changes (open with a different event).
    use_effect(use_reactive!(|(mode,)| {
        match mode {
            EventDialogMode::Create {
                start,
                end,
                all_day: ad,
            } => {
                title.set(String::new());
                description.set(String::new());
                start_str.set(fmt_for_input(start));
                end_str.set(fmt_for_input(end));
                all_day.set(ad);
                location.set(String::new());
                status_sig.set("confirmed".into());
                tags_sig.set(String::new());
            }
            EventDialogMode::Edit(e) => {
                title.set(e.title.clone());
                description.set(e.description.clone().unwrap_or_default());
                start_str.set(fmt_for_input(e.start_at));
                end_str.set(fmt_for_input(e.end_at));
                all_day.set(e.all_day);
                location.set(e.location_text.clone().unwrap_or_default());
                status_sig.set(e.status.clone());
                tags_sig.set(e.tags.join(", "));
            }
        }
    }));

    if !open {
        return rsx! { Fragment {} };
    }

    let is_edit = edit_id.is_some();
    let header_text = if is_edit { "Edit event" } else { "New event" };

    let save_handler = move |_| {
        let parsed_start = parse_from_input(&start_str.read()).unwrap_or(init_start);
        let parsed_end = parse_from_input(&end_str.read()).unwrap_or(init_end);
        let loc_opt = trim_opt(location.read().clone());
        let desc_opt = trim_opt(description.read().clone());
        let tags_vec: Vec<String> = tags_sig
            .read()
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect();
        let status_v = status_sig.read().clone();
        let t = title.read().trim().to_string();
        if t.is_empty() {
            return;
        }
        if let Some(id) = edit_id {
            on_save_update.call((
                id,
                CalendarEventUpdate {
                    title: Some(t),
                    description: Some(desc_opt),
                    start_at: Some(parsed_start),
                    end_at: Some(parsed_end),
                    all_day: Some(*all_day.read()),
                    location_id: None,
                    location_text: Some(loc_opt),
                    rrule: None,
                    organizer: None,
                    attendees: None,
                    calendar_id: None,
                    status: Some(status_v),
                    tags: Some(tags_vec),
                },
            ));
        } else {
            on_save_create.call(CalendarEventCreate {
                title: t,
                description: desc_opt,
                start_at: parsed_start,
                end_at: parsed_end,
                all_day: *all_day.read(),
                location_id: None,
                location_text: loc_opt,
                rrule: None,
                organizer: None,
                attendees: Vec::new(),
                calendar_id: None,
                status: status_v,
                tags: tags_vec,
            });
        }
        on_close.call(());
    };

    rsx! {
        Dialog {
            open: open,
            on_close: move |_| on_close.call(()),
            class: "max-w-lg",
            DialogHeader {
                HStack { class: "items-center justify-between",
                    DialogTitle { "{header_text}" }
                    button {
                        class: "rounded-md p-1 hover:bg-muted",
                        onclick: move |_| on_close.call(()),
                        X { size: 16 }
                    }
                }
                DialogDescription {
                    if is_edit { "Update fields and save." } else { "Fill the form to schedule a new event." }
                }
            }
            div { class: "flex flex-col gap-3",
                Label { "Title" }
                Input { value: title, placeholder: "Event title", disabled: !editable }

                Label { "Description" }
                Textarea { value: description, placeholder: "Optional description", rows: 2u32, disabled: !editable }

                div { class: "grid grid-cols-2 gap-3",
                    div { class: "flex flex-col gap-1",
                        Label { "Start" }
                        Input { value: start_str, disabled: !editable }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "End" }
                        Input { value: end_str, disabled: !editable }
                    }
                }

                div { class: "flex items-center gap-3",
                    Switch { checked: all_day, disabled: !editable }
                    span { class: "text-sm", "All day" }
                }

                div { class: "grid grid-cols-2 gap-3",
                    div { class: "flex flex-col gap-1",
                        Label { "Location" }
                        Input { value: location, placeholder: "e.g. Studio A", disabled: !editable }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "Status" }
                        SegmentedControl {
                            size: SegmentedControlSize::Small,
                            value: status_sig.read().clone(),
                            on_change: move |v: String| status_sig.set(v),
                            options: vec![
                                ("confirmed".into(), "Confirmed".into()),
                                ("tentative".into(), "Tentative".into()),
                                ("cancelled".into(), "Cancelled".into()),
                            ],
                        }
                    }
                }

                Label { "Tags (comma-separated)" }
                Input { value: tags_sig, placeholder: "music, studio", disabled: !editable }
            }

            DialogFooter {
                if is_edit && editable {
                    Button {
                        variant: ButtonVariant::Outline,
                        on_click: move |_| {
                            if let Some(id) = edit_id {
                                on_delete.call(id);
                                on_close.call(());
                            }
                        },
                        Trash2 { size: 14 }
                        " Delete"
                    }
                }
                Button {
                    variant: ButtonVariant::Outline,
                    on_click: move |_| on_close.call(()),
                    "Cancel"
                }
                if editable {
                    Button {
                        variant: ButtonVariant::Primary,
                        on_click: save_handler,
                        if is_edit { "Save" } else { "Create" }
                    }
                }
            }
        }
    }
}

fn fmt_for_input(dt: DateTime<Utc>) -> String {
    dt.with_timezone(&Local)
        .format("%Y-%m-%dT%H:%M")
        .to_string()
}

fn parse_from_input(s: &str) -> Option<DateTime<Utc>> {
    NaiveDateTime::parse_from_str(s.trim(), "%Y-%m-%dT%H:%M")
        .ok()
        .and_then(|ndt| Local.from_local_datetime(&ndt).single())
        .map(|local| local.with_timezone(&Utc))
}

fn trim_opt(s: String) -> Option<String> {
    let t = s.trim();
    if t.is_empty() {
        None
    } else {
        Some(t.to_string())
    }
}

// Suppress unused-import warning for chrono::Duration on wasm with feature gates.
#[allow(dead_code)]
const _DURATION_PROBE: Option<Duration> = None;
