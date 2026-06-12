//! Per-feature optimistic stores — the ONE state pattern for route pages.
//!
//! Built on `architect-atom` (`Store` + `AtomResult` + `use_mutation`),
//! mirroring the architect example app's derived-store idioms by hand
//! (Task's reads/writes are slug-routed through the multi-org
//! [`crate::feeds`] fan-out, which the derive can't know):
//!
//! - one shared [`architect::Store`] per entity, provided at the app
//!   root by [`provide_stores`];
//! - `use_<entity>_list()` — the rows as one [`AtomResult`]
//!   (stale-while-revalidate: an org switch shows the last data while
//!   the refetch is in flight, never a blank "Loading…");
//! - `use_<entity>_mutations()` — optimistic writes through
//!   [`architect::Mutation::run`]: the store is patched instantly
//!   (typed `Id::Temp` placeholders, no magic id sentinels), then
//!   reconciled against the server's row or rolled back, with failures
//!   reported to the app's `Notifications` tray.
//!
//! Entities that live in exactly one org at a time (locations,
//! inventory, …) store the proto type directly; multi-org views (tasks,
//! projects, timer sessions, invoices) wrap rows in `Org<X>` pairs so a
//! mutation can route back to the owning org's service.
//!
//! Supersedes the in-house optimistic write-through list helper
//! (`src/optimistic.rs`), the task wiring shim, and the
//! refresh-counter pages — see `plans/atom-store-migration.md`.

use architect::{
    AtomResult, Id, Mutation, Store, StoreEntity, use_mutation, use_store, use_store_entry,
    use_store_list,
};
use chrono::Utc;
use dioxus::prelude::*;
use project::ProjectInfo;
use task::TaskInfo as DbTask;
use task_ui::{TaskInfo as UiTask, TaskMutation, TimeEntry as UiTimeEntry};
use timer_proto::{StartTimerRequest, WorkSession};
use uuid::Uuid;

use crate::orgs::{OrgMeta, OrgSelection};

/// Provide every feature store at the app root (after
/// `architect::use_app_supervised`, which provides the notifications +
/// reactivity registries the mutations report into).
pub fn provide_stores() {
    provide_task_store();
    provide_project_store();
    provide_location_store();
    provide_item_store();
    provide_milestone_store();
    provide_body_metric_store();
    provide_exercise_store();
    provide_recipe_store();
    provide_pantry_store();
    provide_inbox_store();
    provide_booking_store();
    provide_event_type_store();
    provide_session_store();
    provide_invoice_store();
    provide_thread_store();
    provide_message_store();
}

// ── shared plumbing ─────────────────────────────────────────────────

/// The org-selection contexts every list hook keys its fetch off.
fn use_org_scope() -> (Signal<OrgSelection>, Signal<Vec<OrgMeta>>) {
    (use_context(), use_context())
}

/// Store-backed list scoped to the **first selected org** (or home) —
/// the shape of the single-org register pages. Re-fetches when the org
/// switcher moves (the closure reads the selection signals); `None`
/// (discovery pending) keeps the phase at `Loading`.
#[allow(clippy::type_complexity)] // `AtomResult<Vec<(Id, T)>, _>` reads fine.
fn use_first_org_list<T, F, Fut>(
    store: Store<T, String>,
    fetch: F,
) -> AtomResult<Vec<(Id<T::Key>, T)>, String>
where
    T: StoreEntity,
    F: Fn(String) -> Fut + 'static,
    Fut: std::future::Future<Output = Result<Vec<T>, String>> + 'static,
{
    let (selection, orgs) = use_org_scope();
    use_store_list(store, move || {
        let slug = crate::orgs::selected_slugs(&selection.read(), &orgs.read())
            .into_iter()
            .next();
        let pending = slug.map(&fetch);
        async move { Some(pending?.await) }
    })
}

/// Store-backed list fanned out over **every selected org** — the shape
/// of the multi-org views (tasks, projects, sessions, invoices). An
/// empty slug set (discovery pending) keeps the phase at `Loading`.
#[allow(clippy::type_complexity)] // `AtomResult<Vec<(Id, T)>, _>` reads fine.
fn use_multi_org_list<T, F, Fut>(
    store: Store<T, String>,
    fetch: F,
) -> AtomResult<Vec<(Id<T::Key>, T)>, String>
where
    T: StoreEntity,
    F: Fn(Vec<String>) -> Fut + 'static,
    Fut: std::future::Future<Output = Result<Vec<T>, String>> + 'static,
{
    let (selection, orgs) = use_org_scope();
    use_store_list(store, move || {
        let slugs = crate::orgs::selected_slugs(&selection.read(), &orgs.read());
        let pending = (!slugs.is_empty()).then(|| fetch(slugs));
        async move { Some(pending?.await) }
    })
}

/// The optimistic-create lifecycle every feature shares: insert the
/// draft now, swap it for the server's row on success, roll back (and
/// notify) on failure.
fn run_create<T, Fut>(
    write: Mutation<String>,
    store: Store<T, String>,
    draft: T,
    call: impl FnOnce(T) -> Fut + 'static,
) where
    T: StoreEntity,
    Fut: std::future::Future<Output = Result<T, String>> + 'static,
{
    let send = draft.clone();
    write.run(
        store,
        move |s| s.insert_optimistic(draft).0,
        move || async move { call(send).await.map(Some) },
    );
}

// ── tasks (multi-org, slug-tagged) ──────────────────────────────────

/// One task row tagged with the slug of the org it lives in, so an edit
/// made under "All" routes back to the right org's `TaskService`.
#[derive(Clone, PartialEq)]
pub struct OrgTask {
    pub slug: String,
    pub task: DbTask,
}

impl StoreEntity for OrgTask {
    type Key = Uuid;
    fn key(&self) -> Uuid {
        self.task.id
    }
}

pub type TaskStore = Store<OrgTask, String>;

pub fn provide_task_store() -> TaskStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_task_store() -> TaskStore {
    use_context()
}

/// Tasks across the selected orgs as one [`AtomResult`].
pub fn use_task_list() -> AtomResult<Vec<(Id<Uuid>, OrgTask)>, String> {
    use_multi_org_list(use_task_store(), |slugs| async move {
        crate::feeds::fetch_tasks_tagged(&slugs).await.map(|rows| {
            rows.into_iter()
                .map(|(slug, task)| OrgTask { slug, task })
                .collect()
        })
    })
}

/// Forward-convert the persistence model into the dumb UI model
/// (`task_ui::TaskInfo`) the `TasksApp` board renders.
pub fn to_ui(t: &DbTask) -> UiTask {
    UiTask {
        id: t.id,
        title: t.title.clone(),
        status: t.status.clone(),
        priority: t.priority.clone(),
        due: t.due.clone(),
        scheduled: t.scheduled.clone(),
        tags: t.tags.0.clone(),
        contexts: t.contexts.0.clone(),
        projects: t.projects.0.clone(),
        time_estimate: t.time_estimate,
        time_entries: t
            .time_entries
            .0
            .iter()
            .map(|e| UiTimeEntry {
                start_time: e.start_time,
                end_time: e.end_time,
            })
            .collect(),
        recurrence: t.recurrence.clone(),
        completed_date: t.completed_date,
        date_created: t.date_created,
        date_modified: t.date_modified,
        details: t.details.clone(),
    }
}

/// Map the UI-editable fields of a detail-sheet save back onto the
/// authoritative record (preserving server-only fields like `path`,
/// `project_id`, billing, agent attribution).
fn apply_ui_edits(t: &mut DbTask, ui: &UiTask) {
    t.title = ui.title.clone();
    t.status = ui.status.clone();
    t.priority = ui.priority.clone();
    t.due = ui.due.clone();
    t.scheduled = ui.scheduled.clone();
    t.tags = ui.tags.clone().into();
    t.contexts = ui.contexts.clone().into();
    t.projects = ui.projects.clone().into();
    t.details = ui.details.clone();
}

/// Optimistic writes for the task board, keyed off the store's slug tag.
#[derive(Clone, Copy)]
pub struct TaskMutations {
    store: TaskStore,
    write: Mutation<String>,
}

pub fn use_task_mutations() -> TaskMutations {
    TaskMutations {
        store: use_task_store(),
        write: use_mutation(),
    }
}

impl TaskMutations {
    /// Apply one `task_ui` board mutation: optimistic store patch +
    /// write-through to the owning org (`create_slug` for new rows).
    pub fn apply(&self, create_slug: &str, mu: TaskMutation) {
        match mu {
            TaskMutation::Create { task } => self.create(OrgTask {
                slug: create_slug.to_owned(),
                task: task::capture(&task.title),
            }),
            TaskMutation::Update { task } => {
                let id = task.id;
                self.edit(id, move |t| apply_ui_edits(t, &task));
            }
            TaskMutation::SetStatus { id, status } => self.edit(id, move |t| t.status = status),
            TaskMutation::SetPriority { id, priority } => {
                self.edit(id, move |t| t.priority = priority);
            }
            TaskMutation::Delete { id } => self.delete(id),
        }
    }

    fn create(&self, row: OrgTask) {
        run_create(self.write, self.store, row, |row| async move {
            let slug = row.slug;
            let client = crate::vox_clients::task_client(&slug).await?;
            client
                .create(row.task)
                .await
                .map_err(|e| format!("{slug}: create task: {e:?}"))
                .map(|task| OrgTask { slug, task })
        });
    }

    fn edit(&self, id: Uuid, patch: impl FnOnce(&mut DbTask)) {
        // Patch a snapshot of the current row (full-record service API),
        // then write the whole record through to its owning org.
        let Some(mut next) = self.store.get_real(id) else {
            return;
        };
        patch(&mut next.task);
        let row = next.clone();
        self.write.run(
            self.store,
            move |s| s.update_optimistic(Id::Real(id), move |r| *r = row),
            move || async move {
                let slug = next.slug;
                let client = crate::vox_clients::task_client(&slug).await?;
                client
                    .update(next.task)
                    .await
                    .map_err(|e| format!("{slug}: update task: {e:?}"))
                    .map(|task| Some(OrgTask { slug, task }))
            },
        );
    }

    fn delete(&self, id: Uuid) {
        let Some(row) = self.store.get_real(id) else {
            return;
        };
        let slug = row.slug;
        self.write.run(
            self.store,
            move |s| s.remove_optimistic(Id::Real(id)),
            move || async move {
                let client = crate::vox_clients::task_client(&slug).await?;
                client
                    .delete(id)
                    .await
                    .map(|()| None)
                    .map_err(|e| format!("{slug}: delete task: {e:?}"))
            },
        );
    }
}

// ── projects (multi-org, slug-tagged) ───────────────────────────────

/// One project tagged with its owning org's slug.
#[derive(Clone, PartialEq)]
pub struct OrgProject {
    pub slug: String,
    pub project: ProjectInfo,
}

impl StoreEntity for OrgProject {
    type Key = Uuid;
    fn key(&self) -> Uuid {
        self.project.id
    }
}

pub type ProjectStore = Store<OrgProject, String>;

pub fn provide_project_store() -> ProjectStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_project_store() -> ProjectStore {
    use_context()
}

/// Projects across the selected orgs as one [`AtomResult`]. Hydrates
/// the shared store, so `/projects/:id` is instant after a list visit.
pub fn use_project_list() -> AtomResult<Vec<(Id<Uuid>, OrgProject)>, String> {
    use_multi_org_list(use_project_store(), |slugs| async move {
        crate::feeds::fetch_projects_tagged(&slugs).await.map(|rows| {
            rows.into_iter()
                .map(|(slug, project)| OrgProject { slug, project })
                .collect()
        })
    })
}

fn parse_project_id(raw: &str) -> Result<Uuid, String> {
    raw.parse().map_err(|_| "invalid project id".to_owned())
}

/// One project by route id — cache-first (`Success` straight from the
/// store after a `/projects` visit), else a per-org `get` probe across
/// the selected orgs (no whole-list refetch).
pub fn use_project(id: String) -> AtomResult<OrgProject, String> {
    let store = use_project_store();
    let (selection, orgs) = use_org_scope();
    use_store_entry(store, id, parse_project_id, move |key| {
        let slugs = crate::orgs::selected_slugs(&selection.read(), &orgs.read());
        async move {
            if slugs.is_empty() {
                return None; // discovery pending → Loading
            }
            Some(
                crate::feeds::find_project(&key.to_string(), &slugs)
                    .await
                    .map(|(project, slug)| OrgProject { slug, project }),
            )
        }
    })
}

/// Optimistic writes for projects (full-record update).
#[derive(Clone, Copy)]
pub struct ProjectMutations {
    store: ProjectStore,
    write: Mutation<String>,
}

pub fn use_project_mutations() -> ProjectMutations {
    ProjectMutations {
        store: use_project_store(),
        write: use_mutation(),
    }
}

impl ProjectMutations {
    /// True while a project write is in flight.
    pub fn is_pending(&self) -> bool {
        self.write.is_pending()
    }

    /// The last failed write's error, for inline display.
    pub fn error(&self) -> Option<String> {
        self.write.error()
    }

    /// Optimistically replace the project record, then write through to
    /// its org's `ProjectService` (markdown frontmatter).
    pub fn update(&self, slug: String, project: ProjectInfo) {
        let id = project.id;
        let row = OrgProject {
            slug: slug.clone(),
            project: project.clone(),
        };
        self.write.run(
            self.store,
            move |s| s.update_optimistic(Id::Real(id), move |r| *r = row),
            move || async move {
                crate::feeds::update_project(&slug, project)
                    .await
                    .map(|project| Some(OrgProject { slug, project }))
            },
        );
    }
}

// ── locations ───────────────────────────────────────────────────────

pub type LocationStore = Store<locations_proto::Location, String>;

pub fn provide_location_store() -> LocationStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_location_store() -> LocationStore {
    use_context()
}

pub fn use_location_list() -> AtomResult<Vec<(Id<Uuid>, locations_proto::Location)>, String> {
    use_first_org_list(use_location_store(), |slug| async move {
        crate::feeds::fetch_locations(&slug).await
    })
}

/// Unsaved placeholder row for an optimistic location insert. The
/// backend assigns the real `id` and vault `path` on create.
pub fn draft_location(name: String, kind: String, address: Option<String>) -> locations_proto::Location {
    locations_proto::Location {
        path: String::new(),
        id: Uuid::nil(),
        name,
        kind,
        parent_id: None,
        address,
        tags: locations_proto::model::Tags::default(),
        same_as: None,
        date_created: None,
        date_modified: None,
        details: String::new(),
    }
}

#[derive(Clone, Copy)]
pub struct LocationMutations {
    store: LocationStore,
    write: Mutation<String>,
}

pub fn use_location_mutations() -> LocationMutations {
    LocationMutations {
        store: use_location_store(),
        write: use_mutation(),
    }
}

impl LocationMutations {
    pub fn create(&self, slug: String, draft: locations_proto::Location) {
        run_create(self.write, self.store, draft, move |loc| async move {
            crate::feeds::create_location(&slug, loc).await
        });
    }
}

// ── inventory ───────────────────────────────────────────────────────

pub type ItemStore = Store<inventory_proto::Item, String>;

pub fn provide_item_store() -> ItemStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_item_store() -> ItemStore {
    use_context()
}

pub fn use_item_list() -> AtomResult<Vec<(Id<Uuid>, inventory_proto::Item)>, String> {
    use_first_org_list(use_item_store(), |slug| async move {
        crate::feeds::fetch_inventory(&slug).await
    })
}

/// Unsaved placeholder row for an optimistic inventory insert.
pub fn draft_item(name: String, category: String) -> inventory_proto::Item {
    inventory_proto::Item {
        path: String::new(),
        id: Uuid::nil(),
        name,
        category,
        location_id: None,
        condition: inventory_proto::Condition::Good.as_str().to_owned(),
        status: inventory_proto::Status::Stored.as_str().to_owned(),
        manufacturer: None,
        model: None,
        serial: None,
        purchase_date: None,
        value: None,
        tasks: inventory_proto::StringList::default(),
        tags: inventory_proto::StringList::default(),
        date_created: None,
        date_modified: None,
        details: String::new(),
    }
}

#[derive(Clone, Copy)]
pub struct ItemMutations {
    store: ItemStore,
    write: Mutation<String>,
}

pub fn use_item_mutations() -> ItemMutations {
    ItemMutations {
        store: use_item_store(),
        write: use_mutation(),
    }
}

impl ItemMutations {
    pub fn create(&self, slug: String, draft: inventory_proto::Item) {
        run_create(self.write, self.store, draft, move |item| async move {
            crate::feeds::create_item(&slug, item).await
        });
    }
}

// ── milestones ──────────────────────────────────────────────────────

pub type MilestoneStore = Store<milestone_proto::Milestone, String>;

pub fn provide_milestone_store() -> MilestoneStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_milestone_store() -> MilestoneStore {
    use_context()
}

pub fn use_milestone_list() -> AtomResult<Vec<(Id<Uuid>, milestone_proto::Milestone)>, String> {
    use_first_org_list(use_milestone_store(), |slug| async move {
        crate::feeds::fetch_milestones(&slug).await
    })
}

/// Unsaved placeholder row for an optimistic milestone insert.
pub fn draft_milestone(
    title: String,
    project_id: Uuid,
    due_date: Option<chrono::NaiveDate>,
) -> milestone_proto::Milestone {
    milestone_proto::Milestone {
        path: String::new(),
        id: Uuid::nil(),
        title,
        project_id,
        goal_id: None,
        status: "open".to_owned(),
        due_date,
        tags: milestone_proto::Tags::default(),
        forge_ref: None,
        date_created: None,
        date_modified: None,
        details: String::new(),
    }
}

#[derive(Clone, Copy)]
pub struct MilestoneMutations {
    store: MilestoneStore,
    write: Mutation<String>,
}

pub fn use_milestone_mutations() -> MilestoneMutations {
    MilestoneMutations {
        store: use_milestone_store(),
        write: use_mutation(),
    }
}

impl MilestoneMutations {
    pub fn create(&self, slug: String, draft: milestone_proto::Milestone) {
        run_create(self.write, self.store, draft, move |ms| async move {
            crate::feeds::create_milestone(&slug, ms).await
        });
    }
}

// ── fitness: body metrics + exercises ───────────────────────────────

pub type BodyMetricStore = Store<fitness_proto::body::BodyMetric, String>;

pub fn provide_body_metric_store() -> BodyMetricStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_body_metric_store() -> BodyMetricStore {
    use_context()
}

pub fn use_body_metric_list() -> AtomResult<Vec<(Id<Uuid>, fitness_proto::body::BodyMetric)>, String>
{
    use_first_org_list(use_body_metric_store(), |slug| async move {
        crate::feeds::fetch_body_metrics(&slug).await
    })
}

/// Unsaved placeholder row for an optimistic body-metric insert.
pub fn draft_body_metric(name: String, kind: String, unit: String) -> fitness_proto::body::BodyMetric {
    fitness_proto::body::BodyMetric {
        path: String::new(),
        id: Uuid::nil(),
        name,
        kind,
        unit,
        goal: None,
        tags: fitness_proto::body::Tags::default(),
        entries: fitness_proto::body::Entries::default(),
        date_created: None,
        date_modified: None,
        details: String::new(),
    }
}

#[derive(Clone, Copy)]
pub struct BodyMetricMutations {
    store: BodyMetricStore,
    write: Mutation<String>,
}

pub fn use_body_metric_mutations() -> BodyMetricMutations {
    BodyMetricMutations {
        store: use_body_metric_store(),
        write: use_mutation(),
    }
}

impl BodyMetricMutations {
    pub fn create(&self, slug: String, draft: fitness_proto::body::BodyMetric) {
        run_create(self.write, self.store, draft, move |metric| async move {
            crate::feeds::create_body_metric(&slug, metric).await
        });
    }
}

pub type ExerciseStore = Store<fitness_proto::exercises::Exercise, String>;

pub fn provide_exercise_store() -> ExerciseStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_exercise_store() -> ExerciseStore {
    use_context()
}

pub fn use_exercise_list() -> AtomResult<Vec<(Id<Uuid>, fitness_proto::exercises::Exercise)>, String>
{
    use_first_org_list(use_exercise_store(), |slug| async move {
        crate::feeds::fetch_exercises(&slug).await
    })
}

/// Unsaved placeholder row for an optimistic exercise insert.
pub fn draft_exercise(name: String, category: String) -> fitness_proto::exercises::Exercise {
    fitness_proto::exercises::Exercise {
        path: String::new(),
        id: Uuid::nil(),
        name,
        aliases: fitness_proto::exercises::StringList::default(),
        description: None,
        category,
        primary_muscles: fitness_proto::exercises::StringList::default(),
        secondary_muscles: fitness_proto::exercises::StringList::default(),
        equipment: fitness_proto::exercises::StringList::default(),
        mechanics: None,
        force: None,
        instructions: fitness_proto::exercises::StringList::default(),
        video_url: None,
        image_url: None,
        tags: fitness_proto::exercises::StringList::default(),
        date_created: None,
        date_modified: None,
        details: String::new(),
    }
}

#[derive(Clone, Copy)]
pub struct ExerciseMutations {
    store: ExerciseStore,
    write: Mutation<String>,
}

pub fn use_exercise_mutations() -> ExerciseMutations {
    ExerciseMutations {
        store: use_exercise_store(),
        write: use_mutation(),
    }
}

impl ExerciseMutations {
    pub fn create(&self, slug: String, draft: fitness_proto::exercises::Exercise) {
        run_create(self.write, self.store, draft, move |exercise| async move {
            crate::feeds::create_exercise(&slug, exercise).await
        });
    }
}

// ── mealplan: recipes + pantry ──────────────────────────────────────

pub type RecipeStore = Store<cookbook_proto::Recipe, String>;

pub fn provide_recipe_store() -> RecipeStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_recipe_store() -> RecipeStore {
    use_context()
}

pub fn use_recipe_list() -> AtomResult<Vec<(Id<String>, cookbook_proto::Recipe)>, String> {
    use_first_org_list(use_recipe_store(), |slug| async move {
        crate::feeds::fetch_recipes(&slug).await
    })
}

/// Unsaved placeholder row for an optimistic recipe insert. Identity is
/// the vault-relative `path`; the store keys the draft by a typed
/// `Id::Temp` until the server's row reconciles in, so no magic
/// `__pending__` path sentinel is needed.
pub fn draft_recipe(name: String) -> cookbook_proto::Recipe {
    cookbook_proto::Recipe {
        path: format!("Cookbook/{name}.cook"),
        source: format!(">> title: {name}\n"),
        name,
        description: None,
        course: None,
        cuisine: None,
        prep_minutes: None,
        cook_minutes: None,
        servings: None,
        ingredients: cookbook_proto::Ingredients::default(),
        steps: cookbook_proto::StringList::default(),
        cookware: cookbook_proto::StringList::default(),
        nested_recipes: cookbook_proto::StringList::default(),
        tags: cookbook_proto::StringList::default(),
        source_url: None,
        date_modified: None,
    }
}

#[derive(Clone, Copy)]
pub struct RecipeMutations {
    store: RecipeStore,
    write: Mutation<String>,
}

pub fn use_recipe_mutations() -> RecipeMutations {
    RecipeMutations {
        store: use_recipe_store(),
        write: use_mutation(),
    }
}

impl RecipeMutations {
    pub fn create(&self, slug: String, draft: cookbook_proto::Recipe) {
        run_create(self.write, self.store, draft, move |recipe| async move {
            crate::feeds::create_recipe(&slug, recipe).await
        });
    }
}

pub type PantryStore = Store<pantry_proto::PantryItem, String>;

pub fn provide_pantry_store() -> PantryStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_pantry_store() -> PantryStore {
    use_context()
}

pub fn use_pantry_list() -> AtomResult<Vec<(Id<Uuid>, pantry_proto::PantryItem)>, String> {
    use_first_org_list(use_pantry_store(), |slug| async move {
        crate::feeds::fetch_pantry(&slug).await
    })
}

/// Unsaved placeholder row for an optimistic pantry insert.
pub fn draft_pantry_item(name: String, qty: Option<f64>, unit: String) -> pantry_proto::PantryItem {
    pantry_proto::PantryItem {
        path: String::new(),
        id: Uuid::nil(),
        name,
        category: "food".to_owned(),
        location_id: None,
        condition: "good".to_owned(),
        status: "stored".to_owned(),
        tags: pantry_proto::StringList(vec!["item".into(), "pantry".into()]),
        date_created: None,
        date_modified: None,
        food_category: String::new(),
        qty,
        unit,
        purchase_unit: None,
        purchase_to_stock_factor: None,
        expiry: None,
        opened: false,
        opened_date: None,
        brand: None,
        nutrition_per_unit: None,
        nutrition_unit: None,
        minimum: None,
        default_best_before_days: None,
        default_best_before_days_after_open: None,
        default_best_before_days_after_freezing: None,
        default_best_before_days_after_thawing: None,
        due_type: "best-before".to_owned(),
        stock_entries: pantry_proto::StockEntries::default(),
        substitutes: pantry_proto::Substitutions::default(),
        barcodes: pantry_proto::StringList::default(),
        image_url: None,
        details: String::new(),
    }
}

#[derive(Clone, Copy)]
pub struct PantryMutations {
    store: PantryStore,
    write: Mutation<String>,
}

pub fn use_pantry_mutations() -> PantryMutations {
    PantryMutations {
        store: use_pantry_store(),
        write: use_mutation(),
    }
}

impl PantryMutations {
    pub fn create(&self, slug: String, draft: pantry_proto::PantryItem) {
        run_create(self.write, self.store, draft, move |item| async move {
            crate::feeds::create_pantry_item(&slug, item).await
        });
    }
}

// ── inbox ───────────────────────────────────────────────────────────

pub type InboxStore = Store<inbox_proto::InboxItem, String>;

pub fn provide_inbox_store() -> InboxStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_inbox_store() -> InboxStore {
    use_context()
}

pub fn use_inbox_list() -> AtomResult<Vec<(Id<String>, inbox_proto::InboxItem)>, String> {
    use_first_org_list(use_inbox_store(), |slug| async move {
        crate::feeds::fetch_inbox(&slug).await
    })
}

/// Optimistic writes for the capture queue. Upserts return unit on the
/// wire, so the row we sent doubles as the reconciled value (the id is
/// client-minted and stable).
#[derive(Clone, Copy)]
pub struct InboxMutations {
    store: InboxStore,
    write: Mutation<String>,
}

pub fn use_inbox_mutations() -> InboxMutations {
    InboxMutations {
        store: use_inbox_store(),
        write: use_mutation(),
    }
}

impl InboxMutations {
    /// Capture a fresh item: it appears instantly, then persists.
    pub fn capture(&self, slug: String, item: inbox_proto::InboxItem) {
        run_create(self.write, self.store, item, move |item| async move {
            crate::feeds::upsert_inbox_item(&slug, item.clone())
                .await
                .map(|()| item)
        });
    }

    /// Replace an item in place (status flips, snoozes), then persist.
    pub fn save(&self, slug: String, next: inbox_proto::InboxItem) {
        let id = next.id.clone();
        let row = next.clone();
        self.write.run(
            self.store,
            move |s| s.update_optimistic(Id::Real(id), move |r| *r = row),
            move || async move {
                crate::feeds::upsert_inbox_item(&slug, next.clone())
                    .await
                    .map(|()| Some(next))
            },
        );
    }

    /// Remove an item now; restore (and notify) if the delete fails.
    pub fn delete(&self, slug: String, id: String) {
        let key = id.clone();
        self.write.run(
            self.store,
            move |s| s.remove_optimistic(Id::Real(key)),
            move || async move {
                crate::feeds::delete_inbox_item(&slug, &id)
                    .await
                    .map(|()| None)
            },
        );
    }

    /// Promote an item into a Task: optimistically mark it processed,
    /// then create the task and persist the provenance back-link.
    pub fn promote_to_task(&self, slug: String, item: inbox_proto::InboxItem, title: String, details: String) {
        let mut done = item;
        done.status = inbox_proto::InboxItem::STATUS_PROCESSED.to_string();
        let id = done.id.clone();
        let row = done.clone();
        self.write.run(
            self.store,
            move |s| s.update_optimistic(Id::Real(id), move |r| *r = row),
            move || async move {
                let task = crate::feeds::create_task(&slug, &title, &details).await?;
                let mut done = done;
                done.processed_into = Some(task.path);
                crate::feeds::upsert_inbox_item(&slug, done.clone())
                    .await
                    .map(|()| Some(done))
            },
        );
    }

    /// Promote an item into an atomic wiki note (same shape as
    /// [`Self::promote_to_task`], writing markdown to the vault).
    pub fn promote_to_note(&self, slug: String, item: inbox_proto::InboxItem, path: String, markdown: String) {
        let mut done = item;
        done.status = inbox_proto::InboxItem::STATUS_PROCESSED.to_string();
        let id = done.id.clone();
        let row = done.clone();
        self.write.run(
            self.store,
            move |s| s.update_optimistic(Id::Real(id), move |r| *r = row),
            move || async move {
                crate::feeds::create_wiki_note(&slug, &path, &markdown).await?;
                let mut done = done;
                done.processed_into = Some(path);
                crate::feeds::upsert_inbox_item(&slug, done.clone())
                    .await
                    .map(|()| Some(done))
            },
        );
    }
}

// ── bookings + event types ──────────────────────────────────────────

pub type BookingStore = Store<scheduling_proto::Booking, String>;

pub fn provide_booking_store() -> BookingStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_booking_store() -> BookingStore {
    use_context()
}

/// Bookings for the first selected org, soonest start first.
pub fn use_booking_list() -> AtomResult<Vec<(Id<String>, scheduling_proto::Booking)>, String> {
    use_first_org_list(use_booking_store(), |slug| async move {
        crate::feeds::fetch_bookings(&slug).await.map(|mut rows| {
            rows.sort_by(|a, b| a.start_utc.cmp(&b.start_utc));
            rows
        })
    })
}

#[derive(Clone, Copy)]
pub struct BookingMutations {
    store: BookingStore,
    write: Mutation<String>,
}

pub fn use_booking_mutations() -> BookingMutations {
    BookingMutations {
        store: use_booking_store(),
        write: use_mutation(),
    }
}

impl BookingMutations {
    /// Cancel a booking: the row vanishes instantly; restored (and the
    /// failure reported) if the server rejects it.
    pub fn cancel(&self, slug: String, id: String) {
        let key = id.clone();
        self.write.run(
            self.store,
            move |s| s.remove_optimistic(Id::Real(key)),
            move || async move {
                crate::feeds::cancel_booking(&slug, &id)
                    .await
                    .map(|()| None)
            },
        );
    }
}

pub type EventTypeStore = Store<scheduling_proto::EventType, String>;

pub fn provide_event_type_store() -> EventTypeStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_event_type_store() -> EventTypeStore {
    use_context()
}

pub fn use_event_type_list() -> AtomResult<Vec<(Id<String>, scheduling_proto::EventType)>, String> {
    use_first_org_list(use_event_type_store(), |slug| async move {
        crate::feeds::fetch_event_types(&slug).await
    })
}

/// Draft a bookable event type (client-minted stable id; the backend
/// derives the vault `path`).
pub fn draft_event_type(title: String, duration_min: u16) -> scheduling_proto::EventType {
    let url_slug = crate::feeds::slugify(&title);
    scheduling_proto::EventType {
        path: String::new(),
        id: scheduling_proto::EventTypeId(Uuid::new_v4().to_string()),
        title,
        slug: url_slug,
        description: None,
        duration_min,
        buffer_min: 0,
        location: scheduling_proto::EventTypeLocation::Tbd,
        schedule_id: None,
        published: true,
    }
}

#[derive(Clone, Copy)]
pub struct EventTypeMutations {
    store: EventTypeStore,
    write: Mutation<String>,
}

pub fn use_event_type_mutations() -> EventTypeMutations {
    EventTypeMutations {
        store: use_event_type_store(),
        write: use_mutation(),
    }
}

impl EventTypeMutations {
    pub fn create(&self, slug: String, draft: scheduling_proto::EventType) {
        run_create(self.write, self.store, draft, move |et| async move {
            crate::feeds::create_event_type(&slug, et).await
        });
    }
}

// ── timer sessions (multi-org, slug-tagged) ─────────────────────────

/// One work session tagged with its owning org's slug.
#[derive(Clone, PartialEq)]
pub struct OrgSession {
    pub slug: String,
    pub session: WorkSession,
}

impl StoreEntity for OrgSession {
    type Key = Uuid;
    fn key(&self) -> Uuid {
        self.session.id
    }
}

pub type SessionStore = Store<OrgSession, String>;

pub fn provide_session_store() -> SessionStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_session_store() -> SessionStore {
    use_context()
}

/// Sessions across the selected orgs, newest first. The running timer
/// is *derived* from this one list (the open row for the active org +
/// owner) — the single-record `active_timer` round-trip is gone.
pub fn use_session_list() -> AtomResult<Vec<(Id<Uuid>, OrgSession)>, String> {
    use_multi_org_list(use_session_store(), |slugs| async move {
        Ok(crate::feeds::fetch_sessions_multi(&slugs)
            .await
            .into_iter()
            .map(|(slug, session)| OrgSession { slug, session })
            .collect())
    })
}

/// Unsaved placeholder for an optimistic timer start. `billable` /
/// rate are server-snapshotted; the reconcile swaps in the truth.
pub fn draft_session(req: &StartTimerRequest) -> WorkSession {
    let now = Utc::now();
    WorkSession {
        id: Uuid::nil(),
        org_id: req.org_id,
        user_id: req.user_id,
        project_id: req.project_id,
        project_path: req.project_path.clone(),
        description: req.description.clone(),
        start_time: now,
        end_time: None,
        billable: false,
        rate_cents: 0,
        currency: String::new(),
        task_note_path: req.task_note_path.clone(),
        invoice_id: None,
        created_at: now,
        updated_at: now,
    }
}

#[derive(Clone, Copy)]
pub struct TimerMutations {
    store: SessionStore,
    write: Mutation<String>,
}

pub fn use_timer_mutations() -> TimerMutations {
    TimerMutations {
        store: use_session_store(),
        write: use_mutation(),
    }
}

impl TimerMutations {
    /// Start a timer: an open session appears instantly, then
    /// reconciles to the server's row.
    pub fn start(&self, slug: String, req: StartTimerRequest) {
        let row = OrgSession {
            slug: slug.clone(),
            session: draft_session(&req),
        };
        run_create(self.write, self.store, row, move |_| async move {
            crate::feeds::start_timer(&slug, req)
                .await
                .map(|session| OrgSession {
                    slug: slug.clone(),
                    session,
                })
        });
    }

    /// Stop the running session: it closes instantly (end = now), then
    /// reconciles to the server's closed row (authoritative end + rate).
    pub fn stop(&self, slug: String, user_id: Uuid, session_id: Uuid) {
        self.write.run(
            self.store,
            move |s| {
                s.update_optimistic(Id::Real(session_id), |r| {
                    r.session.end_time = Some(Utc::now());
                })
            },
            move || async move {
                crate::feeds::stop_timer(&slug, user_id)
                    .await
                    .map(|session| Some(OrgSession { slug, session }))
            },
        );
    }

    /// Edit a session (description / billable), then reconcile.
    pub fn update(&self, slug: String, req: timer_proto::service::UpdateSessionRequest) {
        let id = req.id;
        let desc = req.description.clone();
        let billable = req.billable;
        self.write.run(
            self.store,
            move |s| {
                s.update_optimistic(Id::Real(id), move |r| {
                    if let Some(d) = desc {
                        r.session.description = d;
                    }
                    if let Some(b) = billable {
                        r.session.billable = b;
                    }
                })
            },
            move || async move {
                crate::feeds::update_session(&slug, req)
                    .await
                    .map(|session| Some(OrgSession { slug, session }))
            },
        );
    }

    /// Delete a session now; restored (and reported) on failure.
    pub fn delete(&self, slug: String, id: Uuid) {
        self.write.run(
            self.store,
            move |s| s.remove_optimistic(Id::Real(id)),
            move || async move {
                crate::feeds::delete_session(&slug, id)
                    .await
                    .map(|()| None)
            },
        );
    }
}

// ── invoices (multi-org, slug-tagged) ───────────────────────────────

/// Reactivity key for the *derived* uninvoiced-time view: settled
/// invoice mutations invalidate it, refreshing the aggregate the store
/// can't reconcile itself.
pub const UNINVOICED_KEY: &str = "finance.uninvoiced";

/// One invoice tagged with its owning org's slug.
#[derive(Clone, PartialEq)]
pub struct OrgInvoice {
    pub slug: String,
    pub invoice: finance_proto::Invoice,
}

impl StoreEntity for OrgInvoice {
    type Key = Uuid;
    fn key(&self) -> Uuid {
        self.invoice.id
    }
}

pub type InvoiceStore = Store<OrgInvoice, String>;

pub fn provide_invoice_store() -> InvoiceStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_invoice_store() -> InvoiceStore {
    use_context()
}

/// Invoices across the selected orgs, newest first.
pub fn use_invoice_list() -> AtomResult<Vec<(Id<Uuid>, OrgInvoice)>, String> {
    use_multi_org_list(use_invoice_store(), |slugs| async move {
        Ok(crate::feeds::fetch_invoices_multi(&slugs)
            .await
            .into_iter()
            .map(|(slug, invoice)| OrgInvoice { slug, invoice })
            .collect())
    })
}

/// Unsaved placeholder for an optimistic draft-invoice generation,
/// seeded with the uninvoiced group's totals. The server's generated
/// invoice (line items, party, book) reconciles in.
pub fn draft_invoice(amount_minor: i64, currency: String) -> finance_proto::Invoice {
    use finance_proto::invoice::{InvoiceKind, InvoiceStatus};
    let now = Utc::now();
    finance_proto::Invoice {
        id: Uuid::nil(),
        book_id: Uuid::nil(),
        party_id: Uuid::nil(),
        kind: InvoiceKind::Invoice,
        number: String::new(),
        status: InvoiceStatus::Draft,
        issue_date: now.date_naive().to_string(),
        due_date: String::new(),
        currency,
        exchange_rate_micro: 1_000_000,
        line_items: finance_proto::invoice::InvoiceLineItems::default(),
        invoice_taxes: finance_proto::TaxLines::default(),
        uses_inclusive_taxes: false,
        subtotal_minor: amount_minor,
        tax_total_minor: 0,
        total_minor: amount_minor,
        amount_paid_minor: 0,
        balance_minor: amount_minor,
        notes_public: String::new(),
        notes_private: String::new(),
        terms: String::new(),
        footer: String::new(),
        locked: false,
        posted_at: chrono::DateTime::<Utc>::UNIX_EPOCH,
        created_at: now,
        updated_at: now,
    }
}

#[derive(Clone, Copy)]
pub struct InvoiceMutations {
    store: InvoiceStore,
    write: Mutation<String>,
}

pub fn use_invoice_mutations() -> InvoiceMutations {
    InvoiceMutations {
        store: use_invoice_store(),
        // Every settled invoice write reshapes the uninvoiced view
        // (generate consumes groups; delete un-bills sessions).
        write: use_mutation().invalidating(&[UNINVOICED_KEY]),
    }
}

impl InvoiceMutations {
    /// Generate a draft invoice from an uninvoiced group: a draft row
    /// (with the group's totals) appears instantly, then reconciles to
    /// the server's generated invoice.
    pub fn generate(
        &self,
        slug: String,
        req: finance_proto::GenerateInvoice,
        amount_minor: i64,
        currency: String,
    ) {
        let row = OrgInvoice {
            slug: slug.clone(),
            invoice: draft_invoice(amount_minor, currency),
        };
        run_create(self.write, self.store, row, move |_| async move {
            crate::feeds::generate_invoice(&slug, req)
                .await
                .map(|invoice| OrgInvoice {
                    slug: slug.clone(),
                    invoice,
                })
        });
    }

    /// Issue an invoice (assign number, lock).
    pub fn mark_sent(&self, slug: String, id: Uuid) {
        self.write.run(
            self.store,
            move |s| {
                s.update_optimistic(Id::Real(id), |r| {
                    r.invoice.status = finance_proto::invoice::InvoiceStatus::Sent;
                    r.invoice.locked = true;
                })
            },
            move || async move {
                crate::feeds::invoice_mark_sent(&slug, id)
                    .await
                    .map(|invoice| Some(OrgInvoice { slug, invoice }))
            },
        );
    }

    /// Record a payment against an invoice.
    pub fn record_payment(&self, slug: String, id: Uuid, amount_minor: i64, date: String) {
        self.write.run(
            self.store,
            move |s| {
                s.update_optimistic(Id::Real(id), move |r| {
                    r.invoice.amount_paid_minor += amount_minor;
                    r.invoice.balance_minor =
                        r.invoice.total_minor - r.invoice.amount_paid_minor;
                    if r.invoice.balance_minor <= 0 {
                        r.invoice.status = finance_proto::invoice::InvoiceStatus::Paid;
                    } else {
                        r.invoice.status = finance_proto::invoice::InvoiceStatus::PartiallyPaid;
                    }
                })
            },
            move || async move {
                crate::feeds::invoice_record_payment(&slug, id, amount_minor, date)
                    .await
                    .map(|invoice| Some(OrgInvoice { slug, invoice }))
            },
        );
    }

    /// Delete a draft invoice (un-bills its sessions).
    pub fn delete(&self, slug: String, id: Uuid) {
        self.write.run(
            self.store,
            move |s| s.remove_optimistic(Id::Real(id)),
            move || async move {
                crate::feeds::invoice_delete(&slug, id)
                    .await
                    .map(|()| None)
            },
        );
    }
}

// ── threads + messages (per-entity conversations) ───────────────────

pub type ThreadStore = Store<threads::Thread, String>;

pub fn provide_thread_store() -> ThreadStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_thread_store() -> ThreadStore {
    use_context()
}

/// Threads anchored to one project, as one [`AtomResult`]. `key` is
/// `(owning slug, project id)` — `None` while the project itself is
/// still resolving keeps the phase at `Loading`.
pub fn use_project_threads(
    key: Option<(String, Uuid)>,
) -> AtomResult<Vec<(Id<Uuid>, threads::Thread)>, String> {
    let store = use_thread_store();
    let key = use_memo(use_reactive!(|(key,)| key));
    use_store_list(store, move || {
        let k = key();
        async move {
            let (slug, pid) = k?;
            Some(crate::feeds::fetch_threads(&slug, "project", pid).await)
        }
    })
}

pub type MessageStore = Store<threads::Message, String>;

pub fn provide_message_store() -> MessageStore {
    let store = use_store();
    use_context_provider(move || store)
}

pub fn use_message_store() -> MessageStore {
    use_context()
}

/// Messages of the selected thread. `key` is `(owning slug, thread
/// id)`; `None` (nothing selected) stays `Loading` — render its
/// `value().unwrap_or_default()` for an empty panel.
pub fn use_thread_messages(
    key: Option<(String, Uuid)>,
) -> AtomResult<Vec<(Id<Uuid>, threads::Message)>, String> {
    let store = use_message_store();
    let key = use_memo(use_reactive!(|(key,)| key));
    use_store_list(store, move || {
        let k = key();
        async move {
            let (slug, tid) = k?;
            Some(crate::feeds::fetch_thread_messages(&slug, tid).await)
        }
    })
}

/// Unsaved placeholder thread built from the create request.
pub fn draft_thread(req: &threads::CreateThreadRequest) -> threads::Thread {
    let now = Utc::now();
    threads::Thread {
        id: Uuid::nil(),
        org_id: req.org_id,
        entity_type: req.entity_type.clone(),
        entity_id: req.entity_id,
        title: req.title.clone(),
        kind: req.kind.clone(),
        resolved: false,
        resolved_by: None,
        source_kind: req.source_kind.clone(),
        source_ref: req.source_ref.clone(),
        source_url: req.source_url.clone(),
        created_by: req.created_by,
        created_at: now,
        updated_at: now,
    }
}

/// Unsaved placeholder message built from the post request.
pub fn draft_message(req: &threads::PostMessageRequest) -> threads::Message {
    let now = Utc::now();
    threads::Message {
        id: Uuid::nil(),
        thread_id: req.thread_id,
        org_id: req.org_id,
        author_id: req.author_id,
        author_label: req.author_label.clone(),
        body: req.body.clone(),
        reply_to: req.reply_to,
        source_kind: req.source_kind.clone(),
        external_id: req.external_id.clone(),
        original_text: req.original_text.clone(),
        source_url: req.source_url.clone(),
        posted_at: req.posted_at.unwrap_or(now),
        created_at: now,
        updated_at: now,
    }
}

/// Optimistic writes for conversations: open a thread / post a message.
#[derive(Clone, Copy)]
pub struct ThreadMutations {
    threads: ThreadStore,
    messages: MessageStore,
    thread_m: Mutation<String>,
    message_m: Mutation<String>,
}

pub fn use_thread_mutations() -> ThreadMutations {
    ThreadMutations {
        threads: use_thread_store(),
        messages: use_message_store(),
        thread_m: use_mutation(),
        message_m: use_mutation(),
    }
}

impl ThreadMutations {
    pub fn create_thread(&self, slug: String, req: threads::CreateThreadRequest) {
        let draft = draft_thread(&req);
        run_create(self.thread_m, self.threads, draft, move |_| async move {
            crate::feeds::create_thread(&slug, req).await
        });
    }

    pub fn post_message(&self, slug: String, req: threads::PostMessageRequest) {
        let draft = draft_message(&req);
        run_create(self.message_m, self.messages, draft, move |_| async move {
            crate::feeds::post_thread_message(&slug, req).await
        });
    }
}
