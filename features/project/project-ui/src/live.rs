//! `TasksByProjectLive` — Knowledge-backed task list grouped by
//! project page.
//!
//! Phase 8.5 (post-Phase-10) unification: this used to read the
//! legacy `project_proto::Task` entity off the `workspace` doc.
//! Now it reads `kind: task` Knowledge pages off `vault/org` and
//! groups them by the *first* entry in their `projects:
//! [[Project Name]]` frontmatter list.
//!
//! Two improvements come from the switch:
//! 1. Tasks are full Knowledge pages — they can carry frontmatter
//!    (status / priority / due / contexts / blocked_by …), block
//!    body content, attachments. The legacy Task entity capped at
//!    four fields.
//! 2. The inline expand opens a properties pane (the same
//!    `knowledge_ui::PropertiesPane` used on `/knowledge`) so
//!    every Phase 6.5b type-specific editor works here too —
//!    status dropdown, priority enum, tag chips, etc.
//!
//! Toggle-done flips `status` between `todo` and `done`. The
//! Phase 6.5a `task` schema declares those as the canonical enum
//! values; the kanban demo uses the same set.

use std::collections::HashMap;
use std::sync::Arc;

use crdt::CrdtDoc;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use futures::StreamExt;
use futures::channel::mpsc::unbounded;
use knowledge_crdt::PageRepoLoro;
use knowledge_proto::property_schema::PropertySchemaRegistry;
use knowledge_proto::{Page, PageRepo, PageUpdate};
use project_proto::architect::Page as PageWindow;
use project_proto::{UpdateBytes, WorkspaceSyncClient};
use uuid::Uuid;

/// Filter mode for `/projects`. Stored on a Signal so the page
/// can re-render on change without re-running the snapshot
/// resource.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TaskFilter {
    All,
    Active,
    Done,
}

impl TaskFilter {
    fn label(self) -> &'static str {
        match self {
            Self::All => "All",
            Self::Active => "Active",
            Self::Done => "Done",
        }
    }
    fn value(self) -> &'static str {
        match self {
            Self::All => "all",
            Self::Active => "active",
            Self::Done => "done",
        }
    }
    fn parse(s: &str) -> Self {
        match s {
            "active" => Self::Active,
            "done" => Self::Done,
            _ => Self::All,
        }
    }
    fn matches(self, task: &TaskRow) -> bool {
        match self {
            Self::All => true,
            Self::Active => !task.done,
            Self::Done => task.done,
        }
    }
}

/// The full route entry. Spawns the sync loop and renders from the
/// local doc on every imported chunk.
#[component]
pub fn TasksByProjectLive(vox_url: String) -> Element {
    let local_doc: Signal<Arc<CrdtDoc>> = use_signal(|| Arc::new(CrdtDoc::ephemeral()));
    let version: Signal<u64> = use_signal(|| 0u64);
    let last_error: Signal<Option<String>> = use_signal(|| None::<String>);
    let mut expanded: Signal<Option<Uuid>> = use_signal(|| None);
    let mut filter: Signal<TaskFilter> = use_signal(|| TaskFilter::All);

    // Spawn the sync loop once. Subscribes to `vault/org` (the
    // org-wide reference vault) so tasks + projects are visible
    // to every client on the same server.
    let url_for_hook = vox_url.clone();
    let doc_for_hook = local_doc.read().clone();
    use_hook(move || {
        let url = url_for_hook.clone();
        let doc = doc_for_hook.clone();
        spawn(async move {
            run_sync_loop(url, doc, version, last_error).await;
        });
    });

    // Render half — re-runs whenever `version` changes (every
    // imported chunk). Reads come from the LOCAL doc, so they're
    // synchronous + offline-safe.
    let snapshot = use_resource(move || {
        let _v = version.read();
        let doc = local_doc.read().clone();
        async move { build_snapshot(doc).await }
    });

    // Mutation handler — toggles `status` between todo and done
    // on the matching Knowledge page.
    let toggle_doc = local_doc.read().clone();
    let on_toggle_done = use_callback(move |(page_id, currently_done): (Uuid, bool)| {
        let doc = toggle_doc.clone();
        spawn(async move {
            let next_status = if currently_done { "todo" } else { "done" };
            if let Err(e) = update_page_property(&doc, page_id, "status", next_status).await {
                tracing::warn!(?e, %page_id, "toggle status failed");
            }
        });
    });

    let on_toggle_expand = use_callback(move |page_id: Uuid| {
        let current = *expanded.read();
        expanded.set(if current == Some(page_id) {
            None
        } else {
            Some(page_id)
        });
    });

    // Inline property edits go through PageRepo::update on the
    // local doc, then upload to the server via the same sync
    // pipeline as toggle_done.
    let edit_doc = local_doc.read().clone();
    let on_edit_property = use_callback(
        move |(page_id, key, value): (Uuid, String, serde_json::Value)| {
            let doc = edit_doc.clone();
            spawn(async move {
                if let Err(e) = update_page_property_json(&doc, page_id, &key, value).await {
                    tracing::warn!(?e, %page_id, %key, "property update failed");
                }
            });
        },
    );

    // Per-project inline task creation.
    let add_doc = local_doc.read().clone();
    let on_add_task = use_callback(move |(project_name, title): (String, String)| {
        let doc = add_doc.clone();
        spawn(async move {
            if let Err(e) = create_task_page(&doc, &project_name, &title).await {
                tracing::warn!(?e, "create task failed");
            }
        });
    });

    let version_label = format!("v{}", version.read());
    let expanded_id = *expanded.read();
    let current_filter = *filter.read();
    let total_tasks: usize = match &*snapshot.read_unchecked() {
        Some(Ok(snap)) => snap.tasks_by_project.values().map(|v| v.len()).sum(),
        _ => 0,
    };
    let done_tasks: usize = match &*snapshot.read_unchecked() {
        Some(Ok(snap)) => snap
            .tasks_by_project
            .values()
            .flat_map(|v| v.iter())
            .filter(|t| t.done)
            .count(),
        _ => 0,
    };
    let active_tasks = total_tasks.saturating_sub(done_tasks);
    rsx! {
        div {
            id: "projects-route",
            class: "mx-auto flex max-w-5xl flex-col gap-4 sm:gap-6 p-4 sm:p-6 lg:p-10",
            // ── Page header ────────────────────────────────────
            // On mobile: stack title above the badge. On sm+:
            // inline justify-between.
            div { class: "flex flex-col sm:flex-row sm:items-center sm:justify-between gap-2",
                HStack { class: "items-baseline gap-3 flex-wrap",
                    Heading { level: HeadingLevel::H1, "Projects" }
                    if total_tasks > 0 {
                        Text { variant: TextVariant::Muted, "{done_tasks} of {total_tasks} done" }
                    }
                }
                span { "data-testid": "version-badge",
                    StatusBadge {
                        variant: if last_error.read().is_some() { StatusBadgeVariant::Danger } else { StatusBadgeVariant::Success },
                        label: version_label,
                    }
                }
            }
            // ── Filter tabs ─────────────────────────────────────
            if total_tasks > 0 {
                div { "data-testid": "tasks-filter-tabs",
                    Tabs {
                        value: Some(current_filter.value().to_string()),
                        on_change: Callback::new(move |v: String| {
                            filter.set(TaskFilter::parse(&v));
                        }),
                        TabList {
                            TabTrigger {
                                value: "all".to_string(),
                                index: 0,
                                "All ({total_tasks})"
                            }
                            TabTrigger {
                                value: "active".to_string(),
                                index: 1,
                                "Active ({active_tasks})"
                            }
                            TabTrigger {
                                value: "done".to_string(),
                                index: 2,
                                "Done ({done_tasks})"
                            }
                        }
                    }
                }
            }
            if let Some(err) = last_error.read().as_ref() {
                Alert {
                    variant: AlertVariant::Destructive,
                    AlertTitle { "Sync error" }
                    AlertDescription { "{err}" }
                }
            }
            // ── Body ────────────────────────────────────────────
            match &*snapshot.read_unchecked() {
                None => rsx! { ProjectSkeleton {} },
                Some(Err(err)) => rsx! {
                    Alert {
                        variant: AlertVariant::Destructive,
                        AlertTitle { "Decode failed" }
                        AlertDescription { "{err}" }
                    }
                },
                Some(Ok(snap)) => rsx! { TasksByProjectView {
                    snapshot: snap.clone(),
                    expanded: expanded_id,
                    filter: current_filter,
                    on_toggle_done,
                    on_toggle_expand,
                    on_edit_property,
                    on_add_task,
                } },
            }
        }
    }
}

#[component]
fn ProjectSkeleton() -> Element {
    rsx! {
        VStack { class: "gap-4",
            for _ in 0..2 {
                Skeleton { class: "h-32 w-full rounded-lg" }
            }
        }
    }
}

/// Render half — feed it a `Snapshot` + callbacks.
#[component]
pub fn TasksByProjectView(
    snapshot: Snapshot,
    expanded: Option<Uuid>,
    filter: TaskFilter,
    on_toggle_done: Callback<(Uuid, bool)>,
    on_toggle_expand: Callback<Uuid>,
    on_edit_property: Callback<(Uuid, String, serde_json::Value)>,
    on_add_task: Callback<(String, String)>,
) -> Element {
    if snapshot.ordered_projects.is_empty() && snapshot.tasks_by_project.is_empty() {
        return rsx! {
            EmptyState {
                message: "No projects yet — run task-server with TASK_SERVER_SEED=1 to get demo data, or add a task on /tasks-kanban.".to_string(),
            }
        };
    }
    rsx! {
        VStack { class: "gap-4",
            for (project_name, _project_page_id) in snapshot.ordered_projects.iter() {
                ProjectBlock {
                    key: "{project_name}",
                    name: project_name.clone(),
                    tasks: snapshot.tasks_by_project.get(project_name).cloned().unwrap_or_default(),
                    expanded,
                    filter,
                    on_toggle_done,
                    on_toggle_expand,
                    on_edit_property,
                    on_add_task,
                }
            }
        }
    }
}

#[component]
fn ProjectBlock(
    name: String,
    tasks: Vec<TaskRow>,
    expanded: Option<Uuid>,
    filter: TaskFilter,
    on_toggle_done: Callback<(Uuid, bool)>,
    on_toggle_expand: Callback<Uuid>,
    on_edit_property: Callback<(Uuid, String, serde_json::Value)>,
    on_add_task: Callback<(String, String)>,
) -> Element {
    let initials = project_initials(&name);
    let done_count = tasks.iter().filter(|t| t.done).count();
    let total = tasks.len();
    let visible_tasks: Vec<TaskRow> = tasks
        .iter()
        .filter(|t| filter.matches(t))
        .cloned()
        .collect();
    // Inline create only makes sense for real projects, not the
    // synthetic `(no project)` bucket.
    let can_add = name != "(no project)";
    let card_testid = format!("project-card-{name}");
    rsx! {
        div { "data-testid": card_testid,
            Card { class: "overflow-hidden",
            CardHeader { class: "py-3 sm:py-4",
                HStack { class: "items-center justify-between gap-3",
                    HStack { class: "items-center gap-3 min-w-0",
                        Avatar {
                            size: AvatarSize::Small,
                            AvatarFallback { "{initials}" }
                        }
                        Heading {
                            level: HeadingLevel::H3,
                            class: "truncate",
                            "{name}"
                        }
                    }
                    Badge { variant: BadgeVariant::Secondary, "{done_count}/{total}" }
                }
            }
            CardContent { class: "py-0 pb-3 sm:pb-4",
                if visible_tasks.is_empty() && !tasks.is_empty() {
                    div { class: "text-sm text-muted-foreground py-2",
                        "No tasks match the current filter."
                    }
                } else if tasks.is_empty() {
                    if can_add {
                        Text { variant: TextVariant::Muted, "No tasks yet — add one below." }
                    } else {
                        Text { variant: TextVariant::Muted, "No tasks in this project yet." }
                    }
                } else {
                    ItemGroup { class: "gap-1",
                        for task in visible_tasks.iter() {
                            TaskRowEl {
                                key: "{task.page_id}",
                                row: task.clone(),
                                is_expanded: expanded == Some(task.page_id),
                                on_toggle_done,
                                on_toggle_expand,
                                on_edit_property,
                            }
                        }
                    }
                }
                if can_add {
                    AddTaskInput {
                        project_name: name.clone(),
                        on_submit: on_add_task,
                    }
                }
            }
            }
        }
    }
}

#[component]
fn AddTaskInput(project_name: String, on_submit: Callback<(String, String)>) -> Element {
    let mut value = use_signal(String::new);
    let testid = format!("add-task-input-{project_name}");
    let submit_testid = format!("add-task-submit-{project_name}");
    let project_for_submit = project_name.clone();
    let mut submit = move |_: ()| {
        let v = value.read().trim().to_string();
        if v.is_empty() {
            return;
        }
        on_submit.call((project_for_submit.clone(), v));
        value.set(String::new());
    };
    let mut submit_for_enter = submit.clone();
    rsx! {
        div { class: "mt-2 flex items-center gap-2",
            // Plus glyph as a visual hint instead of a separate
            // button — single-input affordance is friendlier on
            // mobile (no fat-finger between two targets).
            span { class: "text-muted-foreground select-none", "+" }
            input {
                "data-testid": testid,
                r#type: "text",
                class: "flex-1 h-9 sm:h-8 rounded-md border border-border bg-background px-3 text-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring",
                value: "{value}",
                placeholder: "Add a task and press Enter",
                oninput: move |e| value.set(e.value()),
                onkeydown: move |e| {
                    if e.key() == Key::Enter { submit_for_enter(()); }
                },
            }
            span { "data-testid": submit_testid,
                Button {
                    variant: ButtonVariant::Secondary,
                    size: ButtonSize::Small,
                    on_click: move |_| submit(()),
                    "Add"
                }
            }
        }
    }
}

fn project_initials(name: &str) -> String {
    name.split_whitespace()
        .filter_map(|w| w.chars().next().map(|c| c.to_ascii_uppercase()))
        .take(2)
        .collect()
}

#[component]
fn TaskRowEl(
    row: TaskRow,
    is_expanded: bool,
    on_toggle_done: Callback<(Uuid, bool)>,
    on_toggle_expand: Callback<Uuid>,
    on_edit_property: Callback<(Uuid, String, serde_json::Value)>,
) -> Element {
    let page_id = row.page_id;
    let current_done = row.done;
    let row_testid = format!("task-row-{page_id}");
    let checkbox_testid = format!("task-checkbox-{page_id}");
    let expand_testid = format!("task-expand-{page_id}");
    let registry = PropertySchemaRegistry::with_builtins();
    let task_schema = registry.get("task");
    let priority_color = priority_dot_color(&row.priority);
    rsx! {
        div { class: "flex flex-col",
            // Row uses `Item` for clean affordance: padding,
            // hover, focus ring. data-testid lives on the wrapper
            // div so playwright doesn't need to chase Item's
            // generated class names.
            div {
                "data-testid": row_testid,
                "data-task-done": if current_done { "true" } else { "false" },
                Item {
                    variant: ItemVariant::Default,
                    size: ItemSize::Small,
                    interactive: true,
                    class: "group",
                    ItemMedia { class: "size-7 bg-transparent",
                        input {
                            r#type: "checkbox",
                            "data-testid": checkbox_testid,
                            class: "size-4 cursor-pointer accent-primary",
                            checked: current_done,
                            onchange: move |_| on_toggle_done.call((page_id, current_done)),
                            // Stop click from bubbling to row-click expand.
                            onclick: move |e| e.stop_propagation(),
                        }
                    }
                    ItemContent {
                        class: if current_done { "min-w-0 line-through text-muted-foreground cursor-pointer" } else { "min-w-0 cursor-pointer" },
                        // ItemContent doesn't take onclick directly;
                        // delegate to a child span.
                        span {
                            class: "block truncate",
                            onclick: move |_| on_toggle_expand.call(page_id),
                            ItemTitle { class: "truncate", "{row.title}" }
                        }
                    }
                    ItemActions { class: "gap-1.5",
                        if let Some(color) = priority_color {
                            StatusDot { color, size: StatusDotSize::Small }
                        }
                        StatusPill { status: row.status.clone() }
                        span { "data-testid": expand_testid,
                            Button {
                                variant: ButtonVariant::Ghost,
                                size: ButtonSize::Small,
                                on_click: move |_| on_toggle_expand.call(page_id),
                                if is_expanded { "▾" } else { "▸" }
                            }
                        }
                    }
                }
            }
            // Inline expansion — full properties pane for the
            // page, edits ride the same sync pipeline.
            if is_expanded {
                if let Some(schema) = task_schema {
                    div {
                        "data-testid": format!("task-properties-{page_id}"),
                        class: "mx-2 mb-2 mt-1 rounded-md border border-border/60 bg-muted/40 p-3",
                        knowledge_ui::PropertiesPane {
                            schema: schema.clone(),
                            frontmatter_json: row.frontmatter_json.clone(),
                            on_change: {
                                let on_edit = on_edit_property;
                                Callback::new(move |(key, value): (String, serde_json::Value)| {
                                    on_edit.call((page_id, key, value));
                                })
                            },
                        }
                    }
                }
            }
        }
    }
}

fn priority_dot_color(priority: &str) -> Option<StatusDotColor> {
    match priority {
        "urgent" => Some(StatusDotColor::Danger),
        "high" => Some(StatusDotColor::Warning),
        "low" => Some(StatusDotColor::Neutral),
        _ => None,
    }
}

#[component]
fn StatusPill(status: String) -> Element {
    let (variant, label) = match status.as_str() {
        "todo" => (StatusBadgeVariant::Neutral, "To do"),
        "in_progress" => (StatusBadgeVariant::Warning, "In progress"),
        "blocked" => (StatusBadgeVariant::Danger, "Blocked"),
        "done" => (StatusBadgeVariant::Success, "Done"),
        other => (StatusBadgeVariant::Neutral, other),
    };
    rsx! {
        StatusBadge { variant, label: label.to_string() }
    }
}

/// One task row in the rendered view.
#[derive(Clone, PartialEq)]
pub struct TaskRow {
    pub page_id: Uuid,
    pub title: String,
    pub done: bool,
    pub status: String,
    pub priority: String,
    pub frontmatter_json: String,
}

#[derive(Clone, PartialEq)]
pub struct Snapshot {
    /// `(project_name, page_id)` — page_id is `None` when the
    /// project is a synthetic bucket like `(no project)` or a
    /// link to a page that doesn't exist yet.
    pub ordered_projects: Vec<(String, Option<Uuid>)>,
    pub tasks_by_project: HashMap<String, Vec<TaskRow>>,
}

async fn build_snapshot(doc: Arc<CrdtDoc>) -> Result<Snapshot, String> {
    let page_repo = PageRepoLoro::new(&doc);
    let big_page = PageWindow {
        index: 0,
        size: 5000,
    };
    let pages = page_repo
        .list(big_page, None, None)
        .await
        .map_err(|e| format!("page list: {e}"))?;

    // Index project pages by name so we can decorate the
    // groupings with project page ids.
    let mut project_page_id: HashMap<String, Uuid> = HashMap::new();
    let mut tasks_by_project: HashMap<String, Vec<TaskRow>> = HashMap::new();
    let mut task_pages: Vec<&Page> = Vec::new();
    for page in &pages.items {
        let kind = page
            .frontmatter_json
            .parse::<serde_json::Value>()
            .ok()
            .as_ref()
            .and_then(|v| v.get("kind").and_then(|k| k.as_str()).map(String::from))
            .unwrap_or_default();
        match kind.as_str() {
            "project" => {
                project_page_id.insert(page.basename.clone(), page.id);
            }
            "task" => task_pages.push(page),
            _ => {}
        }
    }

    for page in task_pages {
        let row = page_to_task_row(page);
        let project_name = first_project_link(page).unwrap_or_else(|| "(no project)".into());
        tasks_by_project.entry(project_name).or_default().push(row);
    }
    for v in tasks_by_project.values_mut() {
        v.sort_by(|a, b| a.title.cmp(&b.title));
    }

    // Project ordering: kind:project pages alphabetical, then the
    // `(no project)` synthetic bucket at the end if it has any.
    let mut ordered_projects: Vec<(String, Option<Uuid>)> = tasks_by_project
        .keys()
        .filter(|n| *n != "(no project)")
        .cloned()
        .map(|n| {
            let pid = project_page_id.get(&n).copied();
            (n, pid)
        })
        .collect();
    ordered_projects.sort_by(|a, b| a.0.cmp(&b.0));
    if tasks_by_project.contains_key("(no project)") {
        ordered_projects.push(("(no project)".into(), None));
    }

    Ok(Snapshot {
        ordered_projects,
        tasks_by_project,
    })
}

fn page_to_task_row(page: &Page) -> TaskRow {
    let fm = page
        .frontmatter_json
        .parse::<serde_json::Value>()
        .unwrap_or(serde_json::Value::Null);
    let status = fm
        .get("status")
        .and_then(|v| v.as_str())
        .unwrap_or("todo")
        .to_string();
    let priority = fm
        .get("priority")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    let title = fm
        .get("title")
        .and_then(|v| v.as_str())
        .map(String::from)
        .unwrap_or_else(|| page.basename.clone());
    let done = status == "done";
    TaskRow {
        page_id: page.id,
        title,
        done,
        status,
        priority,
        frontmatter_json: page.frontmatter_json.clone(),
    }
}

/// Extract the first project link from a task page's `projects:`
/// frontmatter field. Strips `[[ ]]` wrappers if present.
fn first_project_link(page: &Page) -> Option<String> {
    let fm: serde_json::Value = page.frontmatter_json.parse().ok()?;
    let arr = fm.get("projects")?.as_array()?;
    let first = arr.first()?.as_str()?.trim().to_string();
    let unwrapped = first
        .strip_prefix("[[")
        .and_then(|s| s.strip_suffix("]]"))
        .map(|s| s.to_string())
        .unwrap_or(first);
    Some(unwrapped)
}

async fn update_page_property(
    doc: &Arc<CrdtDoc>,
    page_id: Uuid,
    key: &str,
    value: &str,
) -> Result<(), String> {
    update_page_property_json(doc, page_id, key, serde_json::Value::String(value.into())).await
}

async fn update_page_property_json(
    doc: &Arc<CrdtDoc>,
    page_id: Uuid,
    key: &str,
    value: serde_json::Value,
) -> Result<(), String> {
    let page_repo = PageRepoLoro::new(doc);
    let page = page_repo
        .get(page_id)
        .await
        .map_err(|e| format!("get: {e}"))?;
    let mut fm: indexmap::IndexMap<String, serde_json::Value> =
        serde_json::from_str(&page.frontmatter_json).unwrap_or_default();
    if matches!(value, serde_json::Value::Null) {
        fm.shift_remove(key);
    } else {
        fm.insert(key.to_string(), value);
    }
    let new_json = serde_json::to_string(&fm).map_err(|e| format!("encode: {e}"))?;
    page_repo
        .update(
            page_id,
            PageUpdate {
                frontmatter_json: Some(new_json),
                ..Default::default()
            },
        )
        .await
        .map_err(|e| format!("update: {e}"))?;
    Ok(())
}

/// Create a fresh `kind: task` Knowledge page linked to
/// `project_name` (via the `projects:` frontmatter array).
/// Defaults: status=todo, priority=normal. Vault id is taken
/// from the first vault on the local doc; if none exist, the
/// caller's seed flow should have planted one.
async fn create_task_page(
    doc: &Arc<CrdtDoc>,
    project_name: &str,
    title: &str,
) -> Result<(), String> {
    use knowledge_crdt::VaultRepoLoro;
    use knowledge_proto::{PageCreate, VaultRepo};
    let vault_repo = VaultRepoLoro::new(doc);
    let big = PageWindow {
        index: 0,
        size: 100,
    };
    let vaults = vault_repo
        .list(big, None, None)
        .await
        .map_err(|e| format!("vault list: {e}"))?;
    let vault = vaults
        .items
        .into_iter()
        .next()
        .ok_or_else(|| "no vault to attach the task to".to_string())?;

    let fm = serde_json::json!({
        "kind": "task",
        "title": title,
        "status": "todo",
        "priority": "normal",
        "projects": [project_name],
    })
    .to_string();
    let now = chrono::Utc::now();
    let page_repo = PageRepoLoro::new(doc);
    page_repo
        .create(PageCreate {
            vault_id: vault.id,
            folder_id: None,
            path: format!("{title}.md"),
            basename: title.into(),
            ext: "md".into(),
            aliases: Vec::new(),
            frontmatter_json: fm,
            stat_ctime: now,
            stat_mtime: now,
            stat_size: 0,
            is_journal: false,
            journal_day: None,
            shadow_for_kind: None,
            shadow_for_id: None,
        })
        .await
        .map_err(|e| format!("page create: {e}"))?;
    Ok(())
}

async fn run_sync_loop(
    url: String,
    doc: Arc<CrdtDoc>,
    mut version: Signal<u64>,
    mut last_error: Signal<Option<String>>,
) {
    if url.is_empty() {
        let _ = (doc, &mut version, &mut last_error);
        return;
    }
    let sub_client: WorkspaceSyncClient = match connect_client(&url).await {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!(%e, "sync subscribe-client connect failed");
            last_error.set(Some(e));
            return;
        }
    };
    let apply_client: WorkspaceSyncClient = match connect_client(&url).await {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!(%e, "sync apply-client connect failed");
            last_error.set(Some(e));
            return;
        }
    };

    let (upload_tx, mut upload_rx) = unbounded::<Vec<u8>>();
    let upload_sub = doc.loro().subscribe_local_update(Box::new(move |bytes| {
        let _ = upload_tx.unbounded_send(bytes.to_vec());
        true
    }));
    std::mem::forget(upload_sub);

    // Projects route now reads the ORG VAULT, not the legacy
    // `workspace` doc. Knowledge pages + their frontmatter are
    // the data model.
    let doc_id = project_proto::DocId::org_vault();
    let upload_doc_id = doc_id.clone();
    spawn(async move {
        while let Some(bytes) = upload_rx.next().await {
            if let Err(e) = apply_client
                .apply_update(upload_doc_id.clone(), UpdateBytes(bytes))
                .await
            {
                tracing::warn!(?e, "apply_update failed");
            }
        }
    });

    let (tx, mut rx) = vox::channel::<UpdateBytes>();
    let sub_doc_id = doc_id.clone();
    spawn(async move {
        if let Err(e) = sub_client.subscribe(sub_doc_id, tx).await {
            tracing::warn!(error = ?e, "WorkspaceSync::subscribe ended with error");
        }
    });

    loop {
        match rx.recv().await {
            Ok(Some(msg)) => {
                let bytes = &msg.get().0;
                if let Err(e) = doc.apply_remote(bytes) {
                    tracing::warn!(?e, "apply_remote failed");
                    last_error.set(Some(format!("apply_remote: {e}")));
                    continue;
                }
                version.with_mut(|v| *v += 1);
            }
            Ok(None) => {
                tracing::info!("sync stream closed by server");
                last_error.set(Some("stream closed by server".into()));
                return;
            }
            Err(e) => {
                tracing::warn!(?e, "rx.recv failed");
                last_error.set(Some(format!("recv: {e:?}")));
                return;
            }
        }
    }
}

/// WebSocket-backed vox client. `vox-websocket::WsLink::connect`
/// works on both native (tokio-tungstenite) and wasm
/// (web_sys::WebSocket), so the same code path serves both.
async fn connect_client<C>(url: &str) -> Result<C, String>
where
    C: vox_core::FromVoxSession,
{
    use vox_core::{TransportMode, initiator_on};
    let link = vox_websocket::WsLink::connect(url)
        .await
        .map_err(|e| format!("ws connect: {e:?}"))?;
    initiator_on(link, TransportMode::Bare)
        .establish::<C>()
        .await
        .map_err(|e| format!("vox establish: {e:?}"))
}
