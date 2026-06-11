//! `/milestones` — project checkpoints.
//!
//! Milestones are GitHub-Projects-style checkpoints: each is
//! bound to a project and tasks roll up into it. They live as
//! markdown pages in the vault (`type: milestone`) under the
//! owning project's folder and carry a stable `id`.
//!
//! This page lists the selected org's milestones (title, due
//! date, project, status) and offers a friction-light "Add
//! milestone" form (title + project + optional due date).
//! Editing, status flips, and goal-laddering live in the CLI for
//! now; this is the read + create slice, mirroring the locations
//! page.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use milestone_proto::Milestone;

use crate::optimistic::{use_optimistic_list, RowState};
use crate::orgs::{OrgMeta, OrgSelection};

const INPUT_CLS: &str = "rounded-lg border border-input bg-input/30 px-3 py-2 text-sm transition-colors \
     focus-visible:border-ring focus-visible:outline-none focus-visible:ring-[3px] \
     focus-visible:ring-ring/50 placeholder:text-muted-foreground";

#[component]
pub fn MilestonesView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // The org we list / create into (first selected, or home).
    let slug = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    let mut title = use_signal(String::new);
    let mut project = use_signal(String::new);
    let mut due = use_signal(String::new);

    // Authoritative list: optimistic create, write-through to the org's
    // MilestoneService. Initial snapshot loaded below; no refetch on create.
    let list = use_optimistic_list::<Milestone, _>(|m| m.id);

    let loader = use_resource(move || async move {
        match slug() {
            Some(s) => crate::feeds::fetch_milestones(&s).await,
            None => Ok(Vec::new()),
        }
    });
    use_effect(move || {
        if let Some(Ok(rows)) = &*loader.read_unchecked() {
            list.set(rows.clone());
        }
    });

    // The org's projects power the picker (project_id is required).
    let projects = use_resource(move || async move {
        match slug() {
            Some(s) => crate::feeds::fetch_projects(&[s]).await.unwrap_or_default(),
            None => Vec::new(),
        }
    });
    let project_options = projects.read().clone().unwrap_or_default();

    // Create a provisional milestone (client-minted id; backend assigns its
    // own on write — the whole row is replaced when it returns), show it
    // instantly, then write through.
    let mut create = move || {
        let t = title.read().trim().to_string();
        if t.is_empty() {
            return;
        }
        let Some(s) = slug() else { return };
        let Ok(project_id) = uuid::Uuid::parse_str(project.read().trim()) else {
            return;
        };
        let due_date = {
            let d = due.read().trim().to_string();
            if d.is_empty() {
                None
            } else {
                d.parse::<chrono::NaiveDate>().ok()
            }
        };
        title.set(String::new());
        due.set(String::new());
        let provisional = Milestone {
            path: String::new(),
            id: uuid::Uuid::new_v4(),
            title: t.clone(),
            project_id,
            goal_id: None,
            status: "open".to_owned(),
            due_date,
            tags: milestone_proto::Tags::default(),
            forge_ref: None,
            date_created: None,
            date_modified: None,
            details: String::new(),
        };
        list.create(provisional, async move {
            crate::feeds::create_milestone(&s, &t, project_id, due_date).await
        });
    };

    let rows = list.items().read().clone();
    let load_err: Option<String> = match &*loader.read() {
        Some(Err(e)) => Some(e.clone()),
        _ => None,
    };

    // Project id → title, for resolving each milestone's owner.
    let project_name = {
        let opts = project_options.clone();
        move |id: uuid::Uuid| -> Option<String> {
            opts.iter().find(|p| p.id == id).map(|p| p.title.clone())
        }
    };

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-6 lg:p-10",
            div { class: "flex items-center justify-between gap-3",
                Heading { level: HeadingLevel::H1, "Milestones" }
                Text { variant: TextVariant::Muted, class: "text-sm", "{rows.len()} checkpoints" }
            }
            Text {
                variant: TextVariant::Muted,
                class: "text-sm -mt-2",
                "Project checkpoints — tasks roll up into these, GitHub-Projects style.",
            }

            // ── Add milestone ──────────────────────────────────────
            div { class: "flex flex-col gap-2 rounded-xl border border-border bg-card/40 p-3 sm:flex-row sm:items-center",
                input {
                    class: "{INPUT_CLS} flex-1",
                    placeholder: "Milestone title…",
                    value: "{title}",
                    oninput: move |e| title.set(e.value()),
                    onkeydown: move |e| {
                        if e.key() == Key::Enter {
                            create();
                        }
                    },
                }
                select {
                    class: "{INPUT_CLS}",
                    value: "{project}",
                    onchange: move |e| project.set(e.value()),
                    option { value: "", "Project…" }
                    for p in project_options.iter() {
                        option { value: "{p.id}", "{p.title}" }
                    }
                }
                input {
                    class: "{INPUT_CLS}",
                    r#type: "date",
                    value: "{due}",
                    oninput: move |e| due.set(e.value()),
                }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| create(),
                    "Add"
                }
            }

            if let Some(err) = load_err {
                div { class: "rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm text-destructive",
                    "Couldn't load milestones: {err}"
                }
            }

            // ── The register ───────────────────────────────────────
            if rows.is_empty() {
                div { class: "rounded-lg border border-dashed border-border px-4 py-10 text-center",
                    Text { variant: TextVariant::Muted, "No milestones yet — add your first checkpoint above." }
                }
            } else {
                div { class: "flex flex-col gap-2",
                    for ms in rows {
                        MilestoneRow {
                            key: "{ms.id}",
                            project_label: project_name(ms.project_id),
                            state: list.state(ms.id),
                            ms,
                        }
                    }
                }
            }
        }
    }
}

/// One milestone in the register: title + owning project + due
/// date + status badge. `state` reflects optimistic write-through:
/// dimmed while `Pending`, a destructive ring while `Failed`.
#[component]
fn MilestoneRow(ms: Milestone, project_label: Option<String>, state: RowState) -> Element {
    let title = ms.title.clone();
    let due = ms.due_date.map(|d| d.format("%b %-d, %Y").to_string());
    let closed = ms.status.eq_ignore_ascii_case("closed");
    let status_label = if closed { "closed" } else { "open" };

    let state_cls = match state {
        RowState::Settled => "border-border bg-card/40",
        RowState::Pending => "border-border bg-card/40 opacity-60",
        RowState::Failed => "border-destructive/40 bg-destructive/10",
    };

    rsx! {
        div { class: "flex items-start gap-3 rounded-lg border px-3 py-2 {state_cls}",
            div { class: "flex min-w-0 flex-1 flex-col gap-1",
                Text { class: "break-words text-sm font-medium", "{title}" }
                div { class: "flex flex-wrap items-center gap-2 text-[11px] text-muted-foreground",
                    if let Some(name) = project_label.as_ref() {
                        span { "{name}" }
                    }
                    if let Some(d) = due.as_ref() {
                        span { "due {d}" }
                    }
                }
            }
            div { class: "flex shrink-0 items-center gap-2",
                span { class: "rounded bg-muted px-1.5 py-px text-[11px] text-muted-foreground", "{status_label}" }
            }
        }
    }
}
