//! Routines — the agent working while you aren't watching.
//!
//! The second tab of the right agent sidebar. A routine is a prompt
//! on a schedule ("every morning at 8, summarize what's due and drop
//! it in my inbox"); the backend runs it and delivers the output. The
//! panel is the whole lifecycle: schedule one, watch when it next
//! fires, run it early, pause it, drop it.
//!
//! Presentation lives here; the decidable parts —
//! [`schedule_hint`], [`relative_when`], [`runs_label`] — are pure
//! and tested in [`logic`].

use agent_proto::service::routines::{NewRoutine, Routine};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{CalendarClock, Pause, Play, Plus, Trash2};
use fts_ui::prelude::*;

pub mod logic;
use logic::{relative_when, runs_label, schedule_hint};

#[component]
pub fn RoutinesPanel(slug: String) -> Element {
    // Clone up front: the org slug feeds the resource key *and*
    // every mutation closure below.
    let key = slug.clone();
    let mut routines = use_resource(use_reactive!(|(key,)| async move {
        crate::feeds::fetch_agent_routines(&key).await
    }));
    let mut error = use_signal(String::new);
    let mut composing = use_signal(|| false);
    let mut draft_name = use_signal(String::new);
    let mut draft_schedule = use_signal(String::new);
    let mut draft_prompt = use_signal(String::new);
    let mut busy_id = use_signal(String::new);

    let snapshot = routines.read().clone();
    let (rows, fetch_err): (Vec<Routine>, String) = match snapshot {
        Some(Ok(rows)) => (rows, String::new()),
        Some(Err(e)) => (Vec::new(), e),
        None => (Vec::new(), String::new()),
    };
    let loading = routines.read().is_none();

    // One mutation path for every row action — each returns the
    // updated routine, so the list just refetches.
    let act = use_callback({
        let slug = slug.clone();
        move |(id, action): (String, RowAction)| {
            let slug = slug.clone();
            busy_id.set(id.clone());
            spawn(async move {
                let res: Result<(), String> = match action {
                    RowAction::Pause(v) => crate::feeds::set_agent_routine_paused(&slug, &id, v)
                        .await
                        .map(|_| ()),
                    RowAction::RunNow => {
                        crate::feeds::run_agent_routine(&slug, &id).await.map(|_| ())
                    }
                    RowAction::Delete => crate::feeds::delete_agent_routine(&slug, &id).await,
                };
                busy_id.set(String::new());
                match res {
                    Ok(()) => {
                        error.set(String::new());
                        routines.restart();
                    }
                    Err(e) => error.set(e),
                }
            });
        }
    });

    let create = use_callback({
        let slug = slug.clone();
        move |()| {
            let schedule = draft_schedule.peek().trim().to_string();
            let prompt = draft_prompt.peek().trim().to_string();
            if schedule.is_empty() || prompt.is_empty() {
                error.set("A routine needs both a schedule and a prompt.".to_string());
                return;
            }
            let slug = slug.clone();
            let name = draft_name.peek().trim().to_string();
            spawn(async move {
                let new = NewRoutine {
                    backend_id: String::new(),
                    name,
                    prompt,
                    schedule,
                    deliver: String::new(),
                    skills: Vec::new(),
                    repeat: 0,
                };
                match crate::feeds::create_agent_routine(&slug, new).await {
                    Ok(_) => {
                        error.set(String::new());
                        draft_name.set(String::new());
                        draft_schedule.set(String::new());
                        draft_prompt.set(String::new());
                        composing.set(false);
                        routines.restart();
                    }
                    Err(e) => error.set(e),
                }
            });
        }
    });

    rsx! {
        div { class: "flex items-center justify-between gap-2 px-3 py-2",
            div { class: "flex items-center gap-1.5 text-[0.7rem] font-semibold uppercase tracking-[0.18em] text-muted-foreground",
                CalendarClock { size: 13 }
                span { "Routines" }
                if !rows.is_empty() {
                    span { class: "font-normal tabular-nums tracking-normal text-muted-foreground/60",
                        "{rows.len()}"
                    }
                }
            }
            button {
                r#type: "button",
                class: "rounded p-1 text-muted-foreground hover:bg-accent/40 hover:text-foreground",
                title: "Schedule a routine",
                onclick: move |_| {
                    let v = *composing.peek();
                    composing.set(!v);
                },
                Plus { size: 13 }
            }
        }

        div { class: "flex min-h-0 flex-1 flex-col gap-1 overflow-y-auto px-2 pb-2",
            if !error.read().is_empty() {
                div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-2 py-1 text-xs",
                    "{error}"
                }
            }
            if !fetch_err.is_empty() {
                div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-2 py-1 text-xs",
                    "Couldn't load routines: {fetch_err}"
                }
            }

            if composing() {
                div { class: "flex flex-col gap-1.5 rounded-lg border border-border/60 bg-card/40 p-2",
                    input {
                        class: "rounded-md border border-border/60 bg-card/30 px-2 py-1 text-xs outline-none focus:border-primary/60",
                        placeholder: "Name (optional)",
                        value: "{draft_name}",
                        oninput: move |e| draft_name.set(e.value()),
                    }
                    input {
                        class: "rounded-md border border-border/60 bg-card/30 px-2 py-1 font-mono text-xs outline-none focus:border-primary/60",
                        placeholder: "Schedule — every 2h · 0 8 * * * · 30m",
                        value: "{draft_schedule}",
                        oninput: move |e| draft_schedule.set(e.value()),
                    }
                    // The schedule grammar is the one thing people get
                    // wrong here, so echo the interpretation live.
                    if let Some(hint) = schedule_hint(&draft_schedule.read()) {
                        span { class: "px-0.5 text-[0.65rem] text-muted-foreground", "{hint}" }
                    }
                    textarea {
                        class: "min-h-16 resize-y rounded-md border border-border/60 bg-card/30 px-2 py-1 text-xs leading-relaxed outline-none focus:border-primary/60",
                        placeholder: "What should the agent do each time? Write it as a standalone instruction — nobody's in the chair to clarify.",
                        value: "{draft_prompt}",
                        oninput: move |e| draft_prompt.set(e.value()),
                    }
                    div { class: "flex items-center justify-end gap-1.5",
                        Button {
                            variant: ButtonVariant::Ghost,
                            size: ButtonSize::Small,
                            on_click: move |_| composing.set(false),
                            "Cancel"
                        }
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            on_click: move |_| create(()),
                            "Schedule"
                        }
                    }
                }
            }

            if loading {
                div { class: "px-2 py-3 text-xs text-muted-foreground", "Loading routines…" }
            } else if rows.is_empty() && fetch_err.is_empty() && !composing() {
                div { class: "flex flex-col gap-1 px-2 py-3",
                    Text { variant: TextVariant::Muted, class: "text-xs",
                        "No routines yet. Schedule one with + — a prompt the agent runs on its own, like a morning brief or a weekly review."
                    }
                }
            }

            for r in rows.iter() {
                {routine_row(r, busy_id.read().as_str() == r.id, act)}
            }
        }
    }
}

#[derive(Clone, PartialEq)]
enum RowAction {
    Pause(bool),
    RunNow,
    Delete,
}

fn routine_row(r: &Routine, busy: bool, act: Callback<(String, RowAction)>) -> Element {
    let paused = !r.enabled || r.state == "paused";
    let failed = !r.last_error.is_empty();
    let name = if r.name.trim().is_empty() {
        "(unnamed routine)".to_string()
    } else {
        r.name.clone()
    };
    let next = relative_when(&r.next_run_at, chrono::Utc::now());
    let last = relative_when(&r.last_run_at, chrono::Utc::now());
    let runs = runs_label(r.runs_completed, r.runs_total);
    let id = r.id.clone();

    let card = if paused {
        "flex flex-col gap-1 rounded-lg border border-border/40 bg-card/20 px-2 py-1.5 opacity-60"
    } else if failed {
        "flex flex-col gap-1 rounded-lg border border-destructive/40 bg-destructive/5 px-2 py-1.5"
    } else {
        "flex flex-col gap-1 rounded-lg border border-border/50 bg-card/30 px-2 py-1.5"
    };

    rsx! {
        div { key: "{r.id}", class: "{card}",
            div { class: "flex items-baseline gap-1.5",
                span { class: "truncate text-[13px] font-medium text-foreground", title: "{r.prompt}", "{name}" }
                span { class: "ml-auto shrink-0 font-mono text-[0.65rem] text-muted-foreground", "{r.schedule}" }
            }
            div { class: "flex flex-wrap items-center gap-x-2 gap-y-0.5 text-[0.68rem] text-muted-foreground",
                if paused {
                    span { "paused" }
                } else if let Some(n) = &next {
                    span { title: "{r.next_run_at}", "next {n}" }
                }
                if let Some(l) = &last {
                    span { title: "{r.last_run_at}", "· ran {l}" }
                }
                if let Some(runs) = &runs {
                    span { "· {runs}" }
                }
                if !r.deliver.is_empty() && r.deliver != "local" {
                    span { "· → {r.deliver}" }
                }
            }
            if failed {
                p { class: "line-clamp-2 text-[0.68rem] leading-snug text-destructive", "{r.last_error}" }
            }
            div { class: "flex items-center gap-0.5",
                button {
                    r#type: "button",
                    class: "rounded p-1 text-muted-foreground hover:bg-accent/40 hover:text-foreground disabled:opacity-40",
                    disabled: busy,
                    title: if paused { "Resume" } else { "Pause" },
                    onclick: {
                        let id = id.clone();
                        move |_| act((id.clone(), RowAction::Pause(!paused)))
                    },
                    if paused {
                        Play { size: 12 }
                    } else {
                        Pause { size: 12 }
                    }
                }
                button {
                    r#type: "button",
                    class: "rounded px-1.5 py-1 text-[0.68rem] text-muted-foreground hover:bg-accent/40 hover:text-foreground disabled:opacity-40",
                    disabled: busy,
                    title: "Run once now, without disturbing the schedule",
                    onclick: {
                        let id = id.clone();
                        move |_| act((id.clone(), RowAction::RunNow))
                    },
                    "Run now"
                }
                button {
                    r#type: "button",
                    class: "ml-auto rounded p-1 text-muted-foreground hover:bg-destructive/10 hover:text-destructive disabled:opacity-40",
                    disabled: busy,
                    title: "Delete routine",
                    onclick: {
                        let id = id.clone();
                        move |_| act((id.clone(), RowAction::Delete))
                    },
                    Trash2 { size: 12 }
                }
            }
        }
    }
}
