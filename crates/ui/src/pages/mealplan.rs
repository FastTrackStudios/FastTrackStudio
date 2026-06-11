//! `/mealplan` — the food dashboard.
//!
//! Pulls the three mealplan sub-domains for the selected org into
//! a single sectioned page:
//!
//! - **Recipes** — the cookbook (`<wiki>/Cookbook/*.cook`). Minimal
//!   create form (name → seeds a `>> title:` cooklang stub).
//! - **Pantry** — food-on-hand. Minimal create form (name + qty +
//!   unit).
//! - **Meal Plan** — planned meals on the calendar (date + slot +
//!   status).
//!
//! Editing, cooking (pantry deduction), and nutrition resolution
//! live in the CLI for now; this is the read + light-create slice,
//! mirroring the `/fitness` page.

use dioxus::prelude::*;
use fts_ui::prelude::*;

use cookbook_proto::Recipe;
use mealplan_proto::Meal;
use pantry_proto::PantryItem;

use crate::optimistic::{use_optimistic_list, RowState};
use crate::orgs::{OrgMeta, OrgSelection};

const INPUT_CLS: &str = "rounded-lg border border-input bg-input/30 px-3 py-2 text-sm transition-colors \
     focus-visible:border-ring focus-visible:outline-none focus-visible:ring-[3px] \
     focus-visible:ring-ring/50 placeholder:text-muted-foreground";

#[component]
pub fn MealplanView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // The org we list / create into (first selected, or home).
    let slug = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-8 p-6 lg:p-10",
            div { class: "flex flex-col gap-1",
                Heading { level: HeadingLevel::H1, "Mealplan" }
                Text {
                    variant: TextVariant::Muted,
                    class: "text-sm",
                    "The cookbook, what's in the pantry, and the meals on the calendar.",
                }
            }

            RecipesSection { slug }
            PantrySection { slug }
            MealPlanSection { slug }
        }
    }
}

/// Shared section header — title + a count chip.
#[component]
fn SectionHeader(title: String, count: usize) -> Element {
    rsx! {
        div { class: "flex items-center justify-between gap-3",
            Heading { level: HeadingLevel::H2, "{title}" }
            Text { variant: TextVariant::Muted, class: "text-sm", "{count}" }
        }
    }
}

/// Inline error banner for a failed section load.
#[component]
fn LoadError(what: String, err: String) -> Element {
    rsx! {
        div { class: "rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm text-destructive",
            "Couldn't load {what}: {err}"
        }
    }
}

/// Empty-state placeholder for a section with no rows.
#[component]
fn EmptyState(message: String) -> Element {
    rsx! {
        div { class: "rounded-lg border border-dashed border-border px-4 py-6 text-center",
            Text { variant: TextVariant::Muted, class: "text-sm", "{message}" }
        }
    }
}

// ── Recipes ─────────────────────────────────────────────────────────

#[component]
fn RecipesSection(slug: Memo<Option<String>>) -> Element {
    let mut name = use_signal(String::new);

    // Authoritative list: optimistic create, write-through to the org's
    // CookbookService. Identity is the vault-relative `path` (no Uuid).
    // Initial snapshot loaded below; no refetch on create.
    let list = use_optimistic_list::<Recipe, _>(|r| r.path.clone());

    let recipes = use_resource(move || async move {
        match slug() {
            Some(s) => crate::feeds::fetch_recipes(&s).await,
            None => Ok(Vec::new()),
        }
    });
    use_effect(move || {
        if let Some(Ok(rows)) = &*recipes.read_unchecked() {
            list.set(rows.clone());
        }
    });

    let mut create = move || {
        let n = name.read().trim().to_string();
        if n.is_empty() {
            return;
        }
        let Some(s) = slug() else { return };
        name.set(String::new());
        // Provisional row. `path` is backend-assigned (from the name), so
        // a provisional can't key on the real path before the write lands;
        // mint a unique temp path so the row has a stable, collision-free
        // key for reconciliation. On the future's Ok the helper finds this
        // row by its temp path and replaces it in place with the canonical
        // recipe (real path) — the temp path never persists. Mirrors the
        // struct literal in `feeds::create_recipe`.
        let provisional = Recipe {
            path: format!("__pending__/{}", uuid::Uuid::new_v4()),
            name: n.clone(),
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
            source: format!(">> title: {n}\n"),
        };
        list.create(provisional, async move {
            crate::feeds::create_recipe(&s, &n).await
        });
    };

    let rows = list.items().read().clone();
    let load_err: Option<String> = match &*recipes.read() {
        Some(Err(e)) => Some(e.clone()),
        _ => None,
    };

    rsx! {
        section { class: "flex flex-col gap-3",
            SectionHeader { title: "Recipes".to_string(), count: rows.len() }

            div { class: "flex flex-col gap-2 rounded-xl border border-border bg-card/40 p-3 sm:flex-row sm:items-center",
                input {
                    class: "{INPUT_CLS} flex-1",
                    placeholder: "Recipe name (e.g. Truffle Pasta)…",
                    value: "{name}",
                    oninput: move |e| name.set(e.value()),
                    onkeydown: move |e| {
                        if e.key() == Key::Enter {
                            create();
                        }
                    },
                }
                Button { variant: ButtonVariant::Primary, on_click: move |_| create(), "Add" }
            }

            if let Some(err) = load_err {
                LoadError { what: "recipes".to_string(), err }
            }

            if rows.is_empty() {
                EmptyState { message: "No recipes yet — add one above.".to_string() }
            } else {
                div { class: "flex flex-col gap-2",
                    for r in rows {
                        RecipeRow { key: "{r.path}", state: list.state(r.path.clone()), recipe: r }
                    }
                }
            }
        }
    }
}

/// One recipe: name + course badge + ingredient count.
/// `state` reflects optimistic write-through: dimmed while `Pending`, a
/// destructive ring + tint while `Failed`.
#[component]
fn RecipeRow(recipe: Recipe, state: RowState) -> Element {
    let name = recipe.name.clone();
    let course = recipe.course.clone();
    let ingredients = recipe.ingredients.len();

    let state_cls = match state {
        RowState::Settled => "border-border bg-card/40",
        RowState::Pending => "border-border bg-card/40 opacity-60",
        RowState::Failed => "border-destructive/40 bg-destructive/10",
    };

    rsx! {
        div { class: "flex items-center gap-3 rounded-lg border px-3 py-2 {state_cls}",
            div { class: "flex min-w-0 flex-1 flex-col gap-0.5",
                Text { class: "break-words text-sm font-medium", "{name}" }
                span { class: "text-[11px] text-muted-foreground", "{ingredients} ingredients" }
            }
            if let Some(c) = course {
                span { class: "shrink-0 rounded bg-muted px-1.5 py-px text-[11px] text-muted-foreground", "{c}" }
            }
        }
    }
}

// ── Pantry ──────────────────────────────────────────────────────────

#[component]
fn PantrySection(slug: Memo<Option<String>>) -> Element {
    let mut name = use_signal(String::new);
    let mut qty = use_signal(String::new);
    let mut unit = use_signal(|| "g".to_string());

    // Authoritative list: optimistic create, write-through to the org's
    // PantryService. Initial snapshot loaded below; no refetch on create.
    let list = use_optimistic_list::<PantryItem, _>(|i| i.id);

    let items = use_resource(move || async move {
        match slug() {
            Some(s) => crate::feeds::fetch_pantry(&s).await,
            None => Ok(Vec::new()),
        }
    });
    use_effect(move || {
        if let Some(Ok(rows)) = &*items.read_unchecked() {
            list.set(rows.clone());
        }
    });

    let mut create = move || {
        let n = name.read().trim().to_string();
        if n.is_empty() {
            return;
        }
        let Some(s) = slug() else { return };
        let q = qty.read().trim().parse::<f64>().ok();
        let u = unit.read().trim().to_string();
        name.set(String::new());
        qty.set(String::new());
        // Provisional row (client-minted id; backend assigns its own on
        // write — the whole row is replaced when it returns). Mirrors the
        // struct literal in `feeds::create_pantry_item`.
        let provisional = PantryItem {
            path: String::new(),
            id: uuid::Uuid::new_v4(),
            name: n.clone(),
            category: "food".to_owned(),
            location_id: None,
            condition: "good".to_owned(),
            status: "stored".to_owned(),
            tags: pantry_proto::StringList(vec!["item".into(), "pantry".into()]),
            date_created: None,
            date_modified: None,
            food_category: String::new(),
            qty: q,
            unit: u.clone(),
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
        };
        list.create(provisional, async move {
            crate::feeds::create_pantry_item(&s, &n, q, &u).await
        });
    };

    let rows = list.items().read().clone();
    let load_err: Option<String> = match &*items.read() {
        Some(Err(e)) => Some(e.clone()),
        _ => None,
    };

    rsx! {
        section { class: "flex flex-col gap-3",
            SectionHeader { title: "Pantry".to_string(), count: rows.len() }

            div { class: "flex flex-col gap-2 rounded-xl border border-border bg-card/40 p-3 sm:flex-row sm:items-center",
                input {
                    class: "{INPUT_CLS} flex-1",
                    placeholder: "Item name (e.g. Flour)…",
                    value: "{name}",
                    oninput: move |e| name.set(e.value()),
                    onkeydown: move |e| {
                        if e.key() == Key::Enter {
                            create();
                        }
                    },
                }
                input {
                    class: "{INPUT_CLS} w-24",
                    placeholder: "qty",
                    value: "{qty}",
                    oninput: move |e| qty.set(e.value()),
                    onkeydown: move |e| {
                        if e.key() == Key::Enter {
                            create();
                        }
                    },
                }
                input {
                    class: "{INPUT_CLS} w-20",
                    placeholder: "unit",
                    value: "{unit}",
                    oninput: move |e| unit.set(e.value()),
                    onkeydown: move |e| {
                        if e.key() == Key::Enter {
                            create();
                        }
                    },
                }
                Button { variant: ButtonVariant::Primary, on_click: move |_| create(), "Add" }
            }

            if let Some(err) = load_err {
                LoadError { what: "pantry items".to_string(), err }
            }

            if rows.is_empty() {
                EmptyState { message: "No pantry items yet — add one above.".to_string() }
            } else {
                div { class: "flex flex-col gap-2",
                    for it in rows {
                        PantryRow { key: "{it.id}", state: list.state(it.id), item: it }
                    }
                }
            }
        }
    }
}

/// One pantry item: name + on-hand qty + food category badge.
/// `state` reflects optimistic write-through: dimmed while `Pending`, a
/// destructive ring + tint while `Failed`.
#[component]
fn PantryRow(item: PantryItem, state: RowState) -> Element {
    let name = item.name.clone();
    let unit = item.unit.clone();
    let on_hand = item.stock_total();
    let category = item.food_category.clone();

    let state_cls = match state {
        RowState::Settled => "border-border bg-card/40",
        RowState::Pending => "border-border bg-card/40 opacity-60",
        RowState::Failed => "border-destructive/40 bg-destructive/10",
    };

    rsx! {
        div { class: "flex items-center gap-3 rounded-lg border px-3 py-2 {state_cls}",
            div { class: "flex min-w-0 flex-1 flex-col gap-0.5",
                Text { class: "break-words text-sm font-medium", "{name}" }
                if let Some(q) = on_hand {
                    span { class: "text-[11px] text-muted-foreground", "{q} {unit}" }
                } else {
                    span { class: "text-[11px] text-muted-foreground", "no qty recorded" }
                }
            }
            if !category.is_empty() {
                span { class: "shrink-0 rounded bg-muted px-1.5 py-px text-[11px] text-muted-foreground", "{category}" }
            }
        }
    }
}

// ── Meal Plan ───────────────────────────────────────────────────────

#[component]
fn MealPlanSection(slug: Memo<Option<String>>) -> Element {
    let meals = use_resource(move || async move {
        match slug() {
            Some(s) => crate::feeds::fetch_meal_plans(&s).await,
            None => Ok(Vec::new()),
        }
    });

    let (rows, load_err): (Vec<Meal>, Option<String>) = match &*meals.read() {
        Some(Ok(all)) => (all.clone(), None),
        Some(Err(e)) => (Vec::new(), Some(e.clone())),
        None => (Vec::new(), None),
    };

    rsx! {
        section { class: "flex flex-col gap-3",
            SectionHeader { title: "Meal Plan".to_string(), count: rows.len() }

            if let Some(err) = load_err {
                LoadError { what: "meal plan".to_string(), err }
            }

            if rows.is_empty() {
                EmptyState { message: "No planned meals yet.".to_string() }
            } else {
                div { class: "flex flex-col gap-2",
                    for m in rows {
                        MealRow { key: "{m.id}", meal: m }
                    }
                }
            }
        }
    }
}

/// One planned meal: name + date + slot + status badge.
#[component]
fn MealRow(meal: Meal) -> Element {
    let name = meal.name.clone();
    let date = meal.scheduled_for;
    let slot = meal.slot.clone();
    let status = meal.status.clone();

    rsx! {
        div { class: "flex items-center gap-3 rounded-lg border border-border bg-card/40 px-3 py-2",
            div { class: "flex min-w-0 flex-1 flex-col gap-0.5",
                Text { class: "break-words text-sm font-medium", "{name}" }
                span { class: "text-[11px] text-muted-foreground", "{date} · {slot}" }
            }
            span { class: "shrink-0 rounded bg-muted px-1.5 py-px text-[11px] text-muted-foreground", "{status}" }
        }
    }
}
