//! `/views/gantt` — stub demo of the `view-gantt` crate.
//!
//! No CRDT wiring yet; tasks live in a local signal and mutations are
//! applied to that signal via the gantt's event stream. Drop-in
//! replacement for a real `TaskRepoLoro`-backed wrapper later.

use chrono::{Duration, TimeZone, Utc};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use uuid::Uuid;
use view::gantt::{
    Gantt, GanttColumn, GanttEvent, GanttLink, GanttTask, LinkType, TaskType, store::apply,
};

#[component]
pub fn GanttView() -> Element {
    let mut tasks = use_signal(seed_tasks);
    let mut links = use_signal(seed_links);

    let on_event = EventHandler::new(move |ev: GanttEvent| {
        // Apply mutation to our local signal so the demo behaves the
        // same way a CRDT-backed consumer would, without the writes.
        let mut state = view::gantt::GanttState {
            tasks: tasks.read().clone(),
            links: links.read().clone(),
            ..view::gantt::GanttState::default()
        };
        apply(&mut state, &ev);
        tasks.set(state.tasks);
        links.set(state.links);
    });

    let mut readonly = use_signal(|| false);

    rsx! {
        div { class: "h-[calc(100vh-3.5rem)] p-4 flex flex-col gap-3",
            div { class: "flex items-center gap-3",
                Heading { level: HeadingLevel::H1, "Gantt" }
                Spacer {}
                label {
                    class: "flex items-center gap-2 text-sm text-muted-foreground select-none",
                    input {
                        r#type: "checkbox",
                        checked: *readonly.read(),
                        onchange: move |e: FormEvent| {
                            readonly.set(e.value() == "true");
                        },
                    }
                    "Read-only"
                }
            }
            Text { variant: TextVariant::Muted,
                "Drag bars to move, edges to resize. Drag the small white marker to change progress. Click an end-circle then another bar's circle to link. Double-click a label to rename, right-click for more actions. Drag rows in the sidebar to reparent."
            }
            div { class: "flex-1 min-h-0",
                Gantt {
                    tasks: tasks.read().clone(),
                    links: links.read().clone(),
                    columns: Some(vec![
                        GanttColumn::name(240.0),
                        GanttColumn::start(110.0),
                        GanttColumn::end(110.0),
                        GanttColumn::progress(70.0),
                    ]),
                    readonly: *readonly.read(),
                    on_event,
                }
            }
        }
    }
}

fn seed_tasks() -> Vec<GanttTask> {
    let base = Utc.with_ymd_and_hms(2026, 5, 1, 0, 0, 0).unwrap();
    let day = |n: i64| base + Duration::days(n);

    // Two parallel summary tracks so the chart has visual rhythm.
    let id_rel = uuid("11111111-1111-1111-1111-111111111111");
    let id_design = uuid("22222222-2222-2222-2222-222222222222");
    let id_build = uuid("33333333-3333-3333-3333-333333333333");
    let id_test = uuid("44444444-4444-4444-4444-444444444444");
    let id_ship = uuid("55555555-5555-5555-5555-555555555555");

    let id_infra = uuid("66666666-6666-6666-6666-666666666666");
    let id_db = uuid("77777777-7777-7777-7777-777777777777");
    let id_cdn = uuid("88888888-8888-8888-8888-888888888888");
    let id_audit = uuid("99999999-9999-9999-9999-999999999999");

    fn task(
        id: uuid::Uuid,
        parent: Option<uuid::Uuid>,
        text: &str,
        start: chrono::DateTime<Utc>,
        end: chrono::DateTime<Utc>,
        progress: f32,
        color: &str,
    ) -> GanttTask {
        GanttTask {
            id,
            parent,
            text: text.into(),
            start,
            end,
            progress,
            task_type: TaskType::Task,
            open: true,
            rollup: false,
            details: None,
            color: Some(color.into()),
        }
    }

    vec![
        GanttTask {
            id: id_rel,
            parent: None,
            text: "Q2 release".into(),
            start: day(0),
            end: day(35),
            progress: 0.4,
            task_type: TaskType::Summary,
            open: true,
            rollup: false,
            details: Some("Customer-facing release band.".into()),
            color: None,
        },
        task(
            id_design,
            Some(id_rel),
            "Design",
            day(0),
            day(7),
            0.9,
            "var(--color-violet-500, #8b5cf6)",
        ),
        task(
            id_build,
            Some(id_rel),
            "Build",
            day(7),
            day(20),
            0.55,
            "var(--color-blue-500, #3b82f6)",
        ),
        task(
            id_test,
            Some(id_rel),
            "Test",
            day(18),
            day(28),
            0.2,
            "var(--color-emerald-500, #10b981)",
        ),
        GanttTask {
            id: id_ship,
            parent: Some(id_rel),
            text: "Ship".into(),
            start: day(30),
            end: day(30),
            progress: 0.0,
            task_type: TaskType::Milestone,
            open: true,
            rollup: false,
            details: Some("Public launch".into()),
            color: None,
        },
        GanttTask {
            id: id_infra,
            parent: None,
            text: "Infrastructure".into(),
            start: day(3),
            end: day(33),
            progress: 0.35,
            task_type: TaskType::Summary,
            open: true,
            rollup: false,
            details: None,
            color: None,
        },
        task(
            id_db,
            Some(id_infra),
            "DB migration",
            day(3),
            day(12),
            0.7,
            "var(--color-amber-500, #f59e0b)",
        ),
        task(
            id_cdn,
            Some(id_infra),
            "CDN rollout",
            day(10),
            day(22),
            0.45,
            "var(--color-pink-500, #ec4899)",
        ),
        task(
            id_audit,
            Some(id_infra),
            "Security audit",
            day(20),
            day(33),
            0.05,
            "var(--color-red-500, #ef4444)",
        ),
    ]
}

fn seed_links() -> Vec<GanttLink> {
    let link = |s: &str, t: &str| GanttLink {
        id: Uuid::new_v4(),
        source: uuid(s),
        target: uuid(t),
        link_type: LinkType::E2s,
        lag: 0,
    };
    vec![
        link(
            "22222222-2222-2222-2222-222222222222",
            "33333333-3333-3333-3333-333333333333",
        ),
        link(
            "33333333-3333-3333-3333-333333333333",
            "44444444-4444-4444-4444-444444444444",
        ),
        link(
            "44444444-4444-4444-4444-444444444444",
            "55555555-5555-5555-5555-555555555555",
        ),
        link(
            "77777777-7777-7777-7777-777777777777",
            "88888888-8888-8888-8888-888888888888",
        ),
        link(
            "88888888-8888-8888-8888-888888888888",
            "99999999-9999-9999-9999-999999999999",
        ),
    ]
}

fn uuid(s: &str) -> Uuid {
    Uuid::parse_str(s).expect("valid uuid")
}
