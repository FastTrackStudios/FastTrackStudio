//! `/schedule` — calendar with the editable per-day plan.
//!
//! Each visible date gets a resolved plan row — the saved
//! `scheduling_proto::DayPlan` if the user has edited that date,
//! otherwise materialized from the matching `weekday` / `weekend`
//! template — loaded through the shared day-plan store
//! ([`stores::use_dayplan_list`]). The plan's blocks render on the
//! calendar as clickable guides; clicking one opens an editor to
//! move / relabel it, and every edit goes through the named
//! [`stores::DayPlanMutations`] (optimistic write + reconcile-or-
//! rollback, failures in the Notifications tray).
//!
//! Real calendar events are still in-memory (separate follow-up). Drag-
//! to-move on the grid and drag-a-task-onto-a-block are planned polish
//! (see `plans/day-by-day-scheduling.md`).

use std::collections::HashMap;

use chrono::{Datelike, NaiveDate, Utc};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use scheduling_proto::{BlockAssignment, BlockCategory, CalEvent};
use view_calendar::{
    BlockEdit, Calendar, CalendarEvent, CalendarMutation, CalendarState, ColorTag, EventId,
    TASK_DROP_MIME, TemplateBlock, ViewMode, apply,
};

use crate::orgs::{OrgMeta, OrgSelection};
use crate::stores::{self, DayPlanRow};

/// An assignment as the editor passes it back: `(kind, title, ref_id)`
/// — `kind` is `"label"` / `"task"` / `"project"`.
type Assign = (String, String, Option<String>);

/// `(id, title)` options for the assignment pickers.
type PickList = Vec<(String, String)>;

#[component]
pub fn ScheduleView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // The org we read/write plans for (first selected, or home).
    let slug = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    let templates = use_resource(move || async move {
        match slug() {
            Some(s) => crate::feeds::fetch_day_templates(&s).await,
            None => Ok(Vec::new()),
        }
    });

    // Tasks + projects to assign into blocks (the pickers).
    let pickers = use_resource(move || async move {
        let Some(s) = slug() else {
            return (PickList::new(), PickList::new());
        };
        let slugs = [s];
        let tasks = crate::feeds::fetch_tasks_tagged(&slugs)
            .await
            .unwrap_or_default()
            .into_iter()
            .map(|(_, t)| (t.id.to_string(), t.title))
            .collect::<PickList>();
        let projects = crate::feeds::fetch_projects(&slugs)
            .await
            .unwrap_or_default()
            .into_iter()
            .map(|p| (p.id.to_string(), p.title))
            .collect::<PickList>();
        (tasks, projects)
    });

    // Visible date range + the per-date plans we've loaded/materialized
    // for it. Seeded to the current week so blocks materialize on first
    // paint; the calendar's `on_range` then keeps it in step as the user
    // navigates (don't wait on that callback for the initial render).
    let mut range = use_signal(|| {
        let today = chrono::Local::now().date_naive();
        let monday =
            today - chrono::Duration::days(i64::from(today.weekday().num_days_from_monday()));
        Some((monday, monday + chrono::Duration::days(6)))
    });
    // The templates as the day-plan store hook wants them: `None`
    // while the fetch is still resolving (keeps the list `Loading`).
    let tpl_list = use_memo(move || match &*templates.read() {
        Some(Ok(t)) => Some(t.clone()),
        _ => None,
    });
    // Every visible date's resolved plan (saved blocks + soft template
    // fallback) as one shared-store list; all writes go through
    // [`stores::DayPlanMutations`] (optimistic, rollback-on-failure).
    let plans_result = stores::use_dayplan_list(range(), tpl_list());
    let plan_muts = stores::use_dayplan_mutations();
    // Which (date, block_id) is being edited, if any.
    let mut editing = use_signal(|| None::<(NaiveDate, String)>);
    // Planned meals — previewed inside Meal-category blocks
    // ("what's for dinner on that date").
    let meals = use_resource(move || async move {
        match slug() {
            Some(s) => crate::feeds::fetch_meal_plans(&s).await.unwrap_or_default(),
            None => Vec::new(),
        }
    });
    // Real events — loaded from + persisted to the CalendarEvents
    // service.
    let mut state = use_signal(CalendarState::default);
    let loaded_events = use_resource(move || async move {
        match slug() {
            Some(s) => crate::feeds::list_events(&s).await,
            None => Ok(Vec::new()),
        }
    });
    use_effect(move || {
        if let Some(Ok(evs)) = &*loaded_events.read() {
            let mut st = CalendarState::default();
            for e in evs {
                if let Some(ce) = from_proto(e) {
                    st.events.insert(ce.id, ce);
                }
            }
            state.set(st);
        }
    });

    // Apply a mutation locally, then persist the affected event.
    let mut on_event = move |mu: CalendarMutation| {
        apply(&mut state.write(), &mu);
        let Some(slug) = slug() else { return };
        match &mu {
            CalendarMutation::Remove { id } => {
                let id = id.to_string();
                spawn(async move {
                    let _ = crate::feeds::delete_event(&slug, &id).await;
                });
            }
            _ => {
                if let Some(id) = affected_id(&mu) {
                    if let Some(ev) = state.peek().events.get(&id).cloned() {
                        let ce = to_proto(&ev);
                        spawn(async move {
                            let _ = crate::feeds::upsert_event(&slug, ce).await;
                        });
                    }
                }
            }
        }
    };

    let events = state.read().events.values().cloned().collect::<Vec<_>>();
    let meal_lookup = build_meal_lookup(meals().as_deref().unwrap_or(&[]));
    // Stale-while-revalidate: navigating the range keeps the last rows
    // rendered while the refetch is in flight.
    let plan_rows: Vec<DayPlanRow> = plans_result
        .value()
        .map(|rows| rows.iter().map(|(_, r)| r.clone()).collect())
        .unwrap_or_default();
    let plans_err = plans_result.error().cloned();
    let template_blocks = build_blocks(&plan_rows, &meal_lookup);

    // The block currently under edit, resolved to its values.
    let editor = editing().and_then(|(date, id)| {
        let row = plan_rows.iter().find(|r| r.date == date)?;
        let b = row.plan.blocks.iter().find(|b| b.id.0 == id)?;
        let assignment = b
            .assignment
            .as_ref()
            .map(|a| (a.kind.clone(), a.title.clone(), a.ref_id.clone()));
        Some((
            date,
            id,
            b.label.clone(),
            b.start.minutes_since_midnight,
            b.end.minutes_since_midnight,
            assignment,
        ))
    });
    let (tasks, projects) = pickers().unwrap_or_default();

    // The named store mutations behind each edit surface — optimistic
    // store patch + write-through, rollback + Notifications on failure.
    let mut save_block = move |(date, id): (NaiveDate, String),
                               label: String,
                               s: u16,
                               e: u16,
                               assign: Option<Assign>| {
        let Some(slug) = slug() else { return };
        plan_muts.save_block(slug, date, id, label, (s, e), assign.map(to_assignment));
        editing.set(None);
    };

    // Set just a block's assignment (used by drag-drop), then persist.
    let assign_block = move |date: NaiveDate, id: String, assign: Option<Assign>| {
        let Some(slug) = slug() else { return };
        plan_muts.assign_block(slug, date, id, assign.map(to_assignment));
    };

    // Move/retime a block from a grid drag, possibly across days, then
    // persist the affected day plan(s).
    let move_block = move |orig: NaiveDate, target: NaiveDate, id: String, s: u16, e: u16| {
        let Some(slug) = slug() else { return };
        plan_muts.move_block(slug, orig, target, id, (s, e));
    };

    // Revert a date to its template — drop the saved plan, re-materialize.
    let mut reset_day = move |date: NaiveDate| {
        let Some(slug) = slug() else { return };
        plan_muts.reset_day(slug, date, &tpl_list().unwrap_or_default());
        editing.set(None);
    };

    // Tasks to drag onto blocks (cap the strip).
    let drag_tasks: PickList = tasks.iter().take(12).cloned().collect();

    // Allocatable-block usage across the visible range.
    let overview = {
        let r = range();
        let (mut alloc_min, mut blocks, mut assigned) = (0i64, 0u32, 0u32);
        for row in &plan_rows {
            if !r.is_some_and(|(s, e)| row.date >= s && row.date <= e) {
                continue;
            }
            for b in row.plan.blocks.iter() {
                if matches!(b.category, BlockCategory::Allocatable) {
                    blocks += 1;
                    alloc_min += i64::from(b.end.minutes_since_midnight)
                        - i64::from(b.start.minutes_since_midnight);
                    if b.assignment.is_some() {
                        assigned += 1;
                    }
                }
            }
        }
        (alloc_min.max(0) as f64 / 60.0, blocks, assigned)
    };

    rsx! {
        // Height: phones subtract the mobile chrome (sticky header ≈
        // 3.5rem + fixed bottom tab bar ≈ 3.5rem + slack, `dvh` so the
        // browser UI collapse doesn't hide the grid bottom); `md:` and
        // up keep the desktop top-bar math. `overflow-hidden` stays —
        // the calendar sizes its hour rows from this box.
        div { class: "h-[calc(100dvh-8rem)] md:h-[calc(100vh-3.5rem)] lg:h-screen p-2 sm:p-4 flex flex-col gap-3 overflow-hidden",
            match &*templates.read_unchecked() {
                Some(Err(e)) => rsx! {
                    div { class: "shrink-0 rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
                        "Couldn't load day-plan templates: {e}"
                    }
                },
                Some(Ok(t)) if t.is_empty() => rsx! {
                    Text {
                        variant: TextVariant::Muted,
                        "No day-plan templates for this org under Projects/Scheduling/templates/ (weekday.md / weekend.md)."
                    }
                },
                None => rsx! { Text { variant: TextVariant::Muted, "Loading schedule…" } },
                _ => rsx! {},
            }
            if let Some(e) = plans_err {
                div { class: "shrink-0 rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
                    "Couldn't load day plans: {e}"
                }
            }
            // Allocatable usage for the visible range.
            if overview.1 > 0 {
                Text {
                    variant: TextVariant::Muted,
                    "{overview.0:.1}h allocatable across {overview.1} blocks · {overview.2} assigned"
                }
            }
            // Draggable tasks — drop one onto an allocatable block.
            // HTML5 drag is mouse-only, so below `md` the strip is
            // dead chrome eating calendar height — hide it (assign
            // via the block editor instead).
            if !drag_tasks.is_empty() {
                div { class: "hidden md:flex shrink-0 items-center gap-2 overflow-x-auto pb-1",
                    span { class: "shrink-0 text-[0.7rem] uppercase tracking-wider text-muted-foreground",
                        "Drag onto a block:"
                    }
                    for (id, title) in drag_tasks.iter() {
                        {
                            let payload = format!("{id}|{title}");
                            rsx! {
                                div {
                                    key: "drag-{id}",
                                    draggable: true,
                                    "data-cal-drag": "true",
                                    class: "shrink-0 cursor-grab rounded-full border border-border bg-card px-2.5 py-1 text-xs text-foreground hover:border-primary active:cursor-grabbing",
                                    ondragstart: move |e: Event<DragData>| {
                                        let _ = e.data().data_transfer().set_data(TASK_DROP_MIME, &payload);
                                    },
                                    "{title}"
                                }
                            }
                        }
                    }
                }
            }
            Calendar {
                events,
                template_blocks,
                initial_view: Some(ViewMode::Week),
                on_range: move |(s, e)| range.set(Some((s, e))),
                on_block_click: move |(date, id)| editing.set(Some((date, id))),
                on_block_edit: move |(orig, target, id, s, e): BlockEdit| {
                    move_block(orig, target, id, s, e);
                },
                on_block_drop: move |(date, id, payload): (NaiveDate, String, String)| {
                    let (rid, title) = payload
                        .split_once('|')
                        .unwrap_or(("", payload.as_str()));
                    assign_block(
                        date,
                        id,
                        Some((
                            "task".to_string(),
                            title.to_string(),
                            (!rid.is_empty()).then(|| rid.to_string()),
                        )),
                    );
                },
                on_event: move |mu| on_event(mu),
            }
        }
        if let Some((date, id, label, start_min, end_min, assignment)) = editor {
            BlockEditor {
                key: "{date}-{id}",
                label,
                start_min,
                end_min,
                assignment,
                tasks,
                projects,
                on_save: move |(l, s, e, a): (String, u16, u16, Option<Assign>)| {
                    save_block((date, id.clone()), l, s, e, a);
                },
                on_reset: move |()| reset_day(date),
                on_cancel: move |()| editing.set(None),
            }
        }
    }
}

/// Modal to move / relabel / assign a plan block. Holds its own working
/// values so typing doesn't churn the page; commits on Save.
#[component]
fn BlockEditor(
    label: ReadSignal<String>,
    start_min: u16,
    end_min: u16,
    assignment: ReadSignal<Option<Assign>>,
    tasks: ReadSignal<PickList>,
    projects: ReadSignal<PickList>,
    on_save: EventHandler<(String, u16, u16, Option<Assign>)>,
    on_reset: EventHandler<()>,
    on_cancel: EventHandler<()>,
) -> Element {
    // Working copies seeded once per mount (the parent keys this
    // modal per block) — peek, so upstream writes don't churn the
    // user's draft mid-edit.
    let mut lbl = use_signal(move || label.peek().clone());
    let mut start = use_signal(|| start_min);
    let mut end = use_signal(|| end_min);
    let mut assign = use_signal(move || assignment.peek().clone());

    let assign_title = assign().map(|a| a.1).unwrap_or_default();
    // Copy signal handles for the picker's change handler — no list
    // clones; the `for` loops below read the same signals.
    let pick_tasks = tasks;
    let pick_projects = projects;
    let tasks = tasks.read();
    let projects = projects.read();

    let input_cls = "rounded-md border border-border bg-background px-2 py-1.5 text-sm text-foreground outline-none focus:ring-2 focus:ring-primary/40";

    rsx! {
        // Centered modal on desktop; below `sm` it docks to the
        // bottom edge (bottom-sheet convention), capped + scrollable,
        // safe-area padded.
        div {
            class: "fixed inset-0 z-50 flex items-end justify-center bg-black/40 p-0 sm:items-center sm:p-4",
            onclick: move |_| on_cancel.call(()),
            div {
                class: "flex max-h-[85dvh] w-full max-w-sm flex-col gap-3 overflow-y-auto rounded-t-xl border border-border bg-card p-5 pb-[calc(1.25rem+env(safe-area-inset-bottom,0px))] shadow-xl sm:rounded-xl sm:pb-5",
                onclick: move |e| e.stop_propagation(),
                Heading { level: HeadingLevel::H3, "Edit block" }
                label { class: "flex flex-col gap-1 text-xs text-muted-foreground",
                    "Label"
                    input {
                        class: "{input_cls}",
                        value: "{lbl}",
                        oninput: move |e| lbl.set(e.value()),
                    }
                }
                div { class: "flex gap-3",
                    label { class: "flex flex-1 flex-col gap-1 text-xs text-muted-foreground",
                        "Start"
                        input {
                            class: "{input_cls}",
                            r#type: "time",
                            value: "{fmt_time(start())}",
                            oninput: move |e| {
                                if let Some(m) = parse_time(&e.value()) {
                                    start.set(m);
                                }
                            },
                        }
                    }
                    label { class: "flex flex-1 flex-col gap-1 text-xs text-muted-foreground",
                        "End"
                        input {
                            class: "{input_cls}",
                            r#type: "time",
                            value: "{fmt_time(end())}",
                            oninput: move |e| {
                                if let Some(m) = parse_time(&e.value()) {
                                    end.set(m);
                                }
                            },
                        }
                    }
                }
                // Assignment — a free label, or pick a task / project.
                div { class: "flex flex-col gap-1 text-xs text-muted-foreground",
                    "Assignment"
                    input {
                        class: "{input_cls}",
                        placeholder: "Type a label, or pick below",
                        value: "{assign_title}",
                        oninput: move |e| {
                            let v = e.value();
                            assign.set(if v.trim().is_empty() {
                                None
                            } else {
                                Some(("label".into(), v, None))
                            });
                        },
                    }
                    select {
                        class: "{input_cls}",
                        onchange: move |e| {
                            let v = e.value();
                            if let Some(id) = v.strip_prefix("task:") {
                                if let Some((_, t)) = pick_tasks.read().iter().find(|(i, _)| i == id) {
                                    assign.set(Some(("task".into(), t.clone(), Some(id.to_string()))));
                                }
                            } else if let Some(id) = v.strip_prefix("project:") {
                                if let Some((_, t)) = pick_projects.read().iter().find(|(i, _)| i == id) {
                                    assign.set(Some(("project".into(), t.clone(), Some(id.to_string()))));
                                }
                            } else if v == "__clear" {
                                assign.set(None);
                            }
                        },
                        option { value: "", "— pick task / project —" }
                        option { value: "__clear", "— clear —" }
                        if !tasks.is_empty() {
                            optgroup { label: "Tasks",
                                for (id, title) in tasks.iter() {
                                    option { key: "t-{id}", value: "task:{id}", "{title}" }
                                }
                            }
                        }
                        if !projects.is_empty() {
                            optgroup { label: "Projects",
                                for (id, title) in projects.iter() {
                                    option { key: "p-{id}", value: "project:{id}", "{title}" }
                                }
                            }
                        }
                    }
                }
                div { class: "mt-1 flex items-center justify-between gap-2",
                    Button {
                        variant: ButtonVariant::Outline,
                        size: ButtonSize::Small,
                        on_click: move |_| on_reset.call(()),
                        "Reset day to template"
                    }
                    div { class: "flex gap-2",
                        Button {
                            variant: ButtonVariant::Outline,
                            size: ButtonSize::Small,
                            on_click: move |_| on_cancel.call(()),
                            "Cancel"
                        }
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            on_click: move |_| on_save.call((lbl(), start(), end(), assign())),
                            "Save"
                        }
                    }
                }
            }
        }
    }
}

// ── helpers ─────────────────────────────────────────────────────────

/// The editor's `(kind, title, ref_id)` tuple as the proto assignment.
fn to_assignment((kind, title, ref_id): Assign) -> BlockAssignment {
    BlockAssignment {
        kind,
        title,
        ref_id,
    }
}

/// `(date, slot)` → planned meal titles for the schedule's meal
/// preview. Only `planned`/`cooked` meals; skipped ones don't show.
fn build_meal_lookup(meals: &[mealplan_proto::Meal]) -> HashMap<(NaiveDate, String), Vec<String>> {
    let mut out: HashMap<(NaiveDate, String), Vec<String>> = HashMap::new();
    for m in meals {
        if matches!(m.status.as_str(), "skipped" | "eating-out") {
            continue;
        }
        let slot = mealplan_proto::Slot::from_str(&m.slot)
            .map_or_else(|| m.slot.to_ascii_lowercase(), |s| s.as_str().to_string());
        out.entry((m.scheduled_for, slot))
            .or_default()
            .push(m.name.clone());
    }
    out
}

/// Convert the loaded plans into dated calendar overlay blocks,
/// splitting any block that wraps past midnight. Soft (template-
/// fallback) blocks carry the `soft` flag for dashed/faded
/// rendering; Meal blocks with nothing assigned preview the meal
/// planned for that date + slot.
fn build_blocks(
    rows: &[DayPlanRow],
    meals: &HashMap<(NaiveDate, String), Vec<String>>,
) -> Vec<TemplateBlock> {
    let mut out = Vec::new();
    for row in rows {
        let date = row.date;
        for b in row.plan.blocks.iter() {
            let start = b.start.minutes_since_midnight;
            let end = b.end.minutes_since_midnight;
            let color = category_color(b.category);
            let soft = row.soft_ids.contains(&b.id.0);
            let assignment = b.assignment.as_ref().map(|a| a.title.clone()).or_else(|| {
                if b.category != BlockCategory::Meal {
                    return None;
                }
                let slot = scheduling_proto::resolve::meal_slot_for_block(&b.label, b.start);
                meals
                    .get(&(date, slot.to_string()))
                    .map(|names| names.join(" · "))
            });
            let mk = |start_min, end_min| TemplateBlock {
                id: b.id.0.clone(),
                date,
                label: b.label.clone(),
                start_min,
                end_min,
                color,
                assignment: assignment.clone(),
                soft,
            };
            if end <= start {
                if start < 1440 {
                    out.push(mk(start, 1440));
                }
                if end > 0 {
                    out.push(mk(0, end));
                }
            } else {
                out.push(mk(start, end));
            }
        }
    }
    out
}

/// Which event a mutation touches (`None` for removal — handled
/// separately).
fn affected_id(mu: &CalendarMutation) -> Option<EventId> {
    match mu {
        CalendarMutation::Create { event } => Some(event.id),
        CalendarMutation::Reschedule { id, .. }
        | CalendarMutation::Rename { id, .. }
        | CalendarMutation::Recolor { id, .. }
        | CalendarMutation::SetAllDay { id, .. }
        | CalendarMutation::SetDescription { id, .. }
        | CalendarMutation::SetRecurrence { id, .. } => Some(*id),
        CalendarMutation::Remove { .. } => None,
    }
}

fn to_proto(e: &CalendarEvent) -> CalEvent {
    CalEvent {
        id: e.id.to_string(),
        title: e.title.clone(),
        start: e.start.to_rfc3339(),
        end: e.end.to_rfc3339(),
        all_day: e.all_day,
        color: color_name(e.color).to_string(),
        description: e.description.clone(),
        recurrence: e.recurrence.clone(),
    }
}

fn from_proto(e: &CalEvent) -> Option<CalendarEvent> {
    Some(CalendarEvent {
        id: uuid::Uuid::parse_str(&e.id).ok()?,
        title: e.title.clone(),
        start: chrono::DateTime::parse_from_rfc3339(&e.start)
            .ok()?
            .with_timezone(&Utc),
        end: chrono::DateTime::parse_from_rfc3339(&e.end)
            .ok()?
            .with_timezone(&Utc),
        all_day: e.all_day,
        color: color_from_name(&e.color),
        description: e.description.clone(),
        recurrence: e.recurrence.clone(),
    })
}

fn color_name(c: ColorTag) -> &'static str {
    match c {
        ColorTag::Neutral => "neutral",
        ColorTag::Primary => "primary",
        ColorTag::Success => "success",
        ColorTag::Warning => "warning",
        ColorTag::Danger => "danger",
        ColorTag::Info => "info",
    }
}

fn color_from_name(s: &str) -> ColorTag {
    match s {
        "neutral" => ColorTag::Neutral,
        "success" => ColorTag::Success,
        "warning" => ColorTag::Warning,
        "danger" => ColorTag::Danger,
        "info" => ColorTag::Info,
        _ => ColorTag::Primary,
    }
}

fn category_color(c: BlockCategory) -> ColorTag {
    match c {
        BlockCategory::Allocatable => ColorTag::Success,
        BlockCategory::Reset | BlockCategory::Maintenance => ColorTag::Info,
        BlockCategory::Spiritual | BlockCategory::WindDown => ColorTag::Primary,
        BlockCategory::Meal => ColorTag::Warning,
        BlockCategory::Exercise => ColorTag::Danger,
        BlockCategory::Hygiene | BlockCategory::Sleep | BlockCategory::Other => ColorTag::Neutral,
    }
}

fn fmt_time(min: u16) -> String {
    let m = min.min(1439);
    format!("{:02}:{:02}", m / 60, m % 60)
}

fn parse_time(s: &str) -> Option<u16> {
    let (h, m) = s.split_once(':')?;
    let h: u16 = h.parse().ok()?;
    let m: u16 = m.parse().ok()?;
    if h < 24 && m < 60 {
        Some(h * 60 + m)
    } else {
        None
    }
}
