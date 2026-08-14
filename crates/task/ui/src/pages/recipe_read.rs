//! `/mealplan/recipe/read` — the whole recipe on one scrollable page.
//!
//! Cook mode walks you through a recipe one phase at a time. This is
//! the other half: everything at once, for reading before you start,
//! shopping against, or finding your place again halfway through.
//!
//! ## The spine
//!
//! Steps run down a timeline rather than a list, because a recipe is a
//! sequence with *duration* — and the duration is the part a flat list
//! hides. A step that says "simmer for 25 minutes" is not the same size
//! as "season and serve", so the connector below a step carrying a
//! timer is drawn as a dashed dwell segment labelled with its length.
//! You can see, without reading a word, where the work clusters and
//! where you're just waiting. Phase bands (`= Prep`, `= Cook`) break
//! the spine into named runs.
//!
//! ## What the parser buys us
//!
//! Every step knows which ingredients it uses and exactly where each
//! one sits in its own text ([`cookbook_proto::StepIngredient`]), so:
//!
//! - step text renders as a token stream, with each ingredient mention
//!   marked in place rather than being flat prose;
//! - each step carries a recap of just its own ingredients, with
//!   quantities, so you never scroll up mid-step to find an amount;
//! - hovering or tapping any mention lights up every other mention of
//!   that ingredient *and* its row in the rail, in both directions.
//!
//! Apps whose recipes are prose have to guess at this by matching words
//! against an ingredient list. We don't.
//!
//! ## Shape
//!
//! Phone gets one column: the spine hugs the left edge, steps are set
//! large, and the ingredient list collapses into a summary you can open
//! at the top. From `lg` — an iPad on a stand, the actual reading
//! posture — the ingredients move into a sticky rail beside the spine,
//! which is what makes the cross-highlighting worth having.

use std::collections::HashSet;

use cookbook_proto::{CookStep, Ingredient, Recipe, StepIngredient};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    ChevronLeft, Clock, CookingPot, Flame, ListChecks, Pencil, Timer as TimerIcon, Users,
};
use fts_ui::prelude::*;

use super::cook_mode::{fmt_num, scaled_qty};
use crate::format::duration_hms;

/// A run of consecutive steps sharing a `= Section` heading.
struct Phase {
    label: Option<String>,
    steps: Vec<usize>,
}

fn phases(recipe: &Recipe) -> Vec<Phase> {
    let mut out: Vec<Phase> = Vec::new();
    for (i, step) in recipe.cook_steps.iter().enumerate() {
        match out.last_mut() {
            Some(p) if p.label == step.section => p.steps.push(i),
            _ => out.push(Phase {
                label: step.section.clone(),
                steps: vec![i],
            }),
        }
    }
    out
}

/// One piece of a step's text: either prose, or an ingredient mention
/// carrying the index of the row it belongs to.
enum Seg<'a> {
    Text(&'a str),
    Ing(&'a StepIngredient),
}

/// Split a step's text on its ingredient spans. The parser records
/// spans in source order and guarantees they slice cleanly, so this is
/// a walk rather than a search — no matching step words against
/// ingredient names and hoping.
fn segments(step: &CookStep) -> Vec<Seg<'_>> {
    let mut refs: Vec<&StepIngredient> = step.ingredients.iter().collect();
    refs.sort_by_key(|r| r.start);
    let mut out = Vec::new();
    let mut cursor = 0usize;
    for r in refs {
        let (start, end) = (r.start as usize, (r.start + r.len) as usize);
        if start < cursor || end > step.text.len() {
            continue;
        }
        if start > cursor {
            out.push(Seg::Text(&step.text[cursor..start]));
        }
        out.push(Seg::Ing(r));
        cursor = end;
    }
    if cursor < step.text.len() {
        out.push(Seg::Text(&step.text[cursor..]));
    }
    out
}

/// Total seconds a step will keep you waiting.
fn dwell(step: &CookStep) -> u32 {
    step.timers.iter().map(|t| t.seconds).sum()
}

#[component]
pub fn RecipeReadView(path: String) -> Element {
    let nav = use_navigator();
    let recipes = crate::stores::use_recipe_list();
    let store = crate::stores::use_recipe_store();
    let target = path.clone();
    let found = recipes.value().and_then(|rows| {
        rows.iter()
            .find(|(_, r)| r.path == target)
            .map(|(_, r)| r.clone())
    });

    match found {
        Some(recipe) => rsx! { Reader { recipe } },
        None if recipes.is_waiting() => rsx! {
            div { class: "flex h-full items-center justify-center p-8", crate::states::LoadingState {} }
        },
        None => rsx! {
            div { class: "mx-auto flex max-w-md flex-col gap-3 p-8 text-center",
                if let Some(err) = recipes.error() {
                    crate::states::ErrorState {
                        title: "Couldn't load the recipe",
                        message: err.clone(),
                        on_retry: move |()| store.reload(),
                    }
                } else {
                    crate::states::EmptyState {
                        title: "Recipe not found",
                        hint: "It may have been moved or renamed.",
                    }
                }
                Button {
                    variant: ButtonVariant::Secondary,
                    on_click: move |_| { nav.push(crate::routes::Route::MealplanRoute {}); },
                    "Back to mealplan"
                }
            }
        },
    }
}

#[component]
fn Reader(recipe: Recipe) -> Element {
    let nav = use_navigator();

    // Checked ingredients are keyed by NAME, not row index, so a
    // rescale — which rebuilds the rows — doesn't wipe what you've
    // already got out on the counter.
    let mut gathered = use_signal(HashSet::<String>::new);
    let mut done_steps = use_signal(HashSet::<usize>::new);
    // The ingredient currently under the pointer, or pinned by tap on
    // a touch screen where there is no hover to speak of.
    let mut focus = use_signal(|| None::<u32>);
    let mut show_ingredients = use_signal(|| false);

    let base = recipe.servings.unwrap_or(1).max(1);
    let mut servings = use_signal(move || base);
    let factor = f64::from(servings()) / f64::from(base);

    let plan = phases(&recipe);
    let total_steps = recipe.cook_steps.len();
    let done_now = done_steps.read().clone();
    let gathered_now = gathered.read().clone();
    let focus_now = focus();
    let done_count = done_now.len();

    let cook_path = recipe.path.clone();
    let edit_path = recipe.path.clone();

    rsx! {
        div { class: "flex h-full min-h-0 flex-col bg-background text-foreground",

            // ── Header ───────────────────────────────────────────
            header { class: "sticky top-0 z-20 flex items-center gap-3 border-b border-border bg-background/95 px-3 py-2 backdrop-blur",
                button {
                    class: "flex size-10 shrink-0 items-center justify-center rounded-lg text-muted-foreground hover:bg-muted hover:text-foreground",
                    aria_label: "Back to mealplan",
                    onclick: move |_| { nav.push(crate::routes::Route::MealplanRoute {}); },
                    ChevronLeft { size: 20 }
                }
                div { class: "flex min-w-0 flex-1 flex-col",
                    Heading { level: HeadingLevel::H1, class: "truncate text-base font-semibold leading-tight sm:text-lg", "{recipe.name}" }
                    if total_steps > 0 {
                        span { class: "text-xs tabular-nums text-muted-foreground", "{done_count} of {total_steps} steps done" }
                    }
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| { nav.push(crate::routes::Route::RecipeEditRoute { path: edit_path.clone() }); },
                    Pencil { size: 14 }
                    span { class: "hidden sm:inline", "Edit" }
                }
                Button {
                    size: ButtonSize::Small,
                    on_click: move |_| { nav.push(crate::routes::Route::RecipeCookRoute { path: cook_path.clone() }); },
                    CookingPot { size: 14 }
                    "Cook"
                }
            }

            // ── Scrollable body ──────────────────────────────────
            div { class: "min-h-0 flex-1 overflow-y-auto",
                div { class: "mx-auto w-full max-w-6xl px-3 pb-16 pt-4 sm:px-5",

                    // Meta strip — what you need to know before committing.
                    div { class: "flex flex-wrap items-center gap-x-4 gap-y-1 text-sm text-muted-foreground",
                        if let Some(s) = recipe.servings {
                            span { class: "inline-flex items-center gap-1.5", Users { size: 14 } "{s} servings" }
                        }
                        if let Some(p) = recipe.prep_minutes {
                            span { class: "inline-flex items-center gap-1.5", Clock { size: 14 } "{p} min prep" }
                        }
                        if let Some(c) = recipe.cook_minutes {
                            span { class: "inline-flex items-center gap-1.5", Flame { size: 14 } "{c} min cook" }
                        }
                        if let Some(course) = &recipe.course {
                            span { class: "rounded-full border border-border px-2 py-0.5 text-xs", "{course}" }
                        }
                    }
                    if let Some(d) = &recipe.description {
                        p { class: "mt-3 max-w-prose text-[15px] leading-relaxed text-muted-foreground", "{d}" }
                    }

                    // ── Two panes from lg: rail beside the spine ──
                    div { class: "mt-6 gap-8 lg:grid lg:grid-cols-[minmax(15rem,19rem)_1fr] lg:items-start",

                        // Ingredients. A sticky rail on a tablet, a
                        // collapsible summary on a phone — where the
                        // per-step recaps already carry the amounts, so
                        // the full list is reference rather than the
                        // thing you read.
                        aside { class: "lg:sticky lg:top-4",
                            div { class: "overflow-hidden rounded-2xl border border-border bg-card/40",
                                button {
                                    class: "flex w-full items-center gap-2 px-4 py-3 text-left lg:cursor-default",
                                    onclick: move |_| { let v = show_ingredients(); show_ingredients.set(!v); },
                                    ListChecks { size: 15 }
                                    span { class: "flex-1 text-sm font-semibold uppercase tracking-wide", "Ingredients" }
                                    span { class: "text-xs tabular-nums text-muted-foreground",
                                        "{gathered_now.len()}/{recipe.ingredients.len()}"
                                    }
                                    span { class: "text-muted-foreground lg:hidden",
                                        if show_ingredients() { "−" } else { "+" }
                                    }
                                }
                                div {
                                    class: if show_ingredients() {
                                        "border-t border-border/60"
                                    } else {
                                        "hidden border-t border-border/60 lg:block"
                                    },
                                    // Servings scaler. Scales the rail
                                    // AND the amounts inline in the
                                    // steps — the split between the two
                                    // is the most common complaint
                                    // levelled at recipe apps.
                                    if recipe.servings.is_some() {
                                        div { class: "flex items-center justify-between gap-2 border-b border-border/60 px-4 py-2.5",
                                            span { class: "text-xs text-muted-foreground", "Scale" }
                                            div { class: "flex items-center gap-1",
                                                button {
                                                    class: "flex size-7 items-center justify-center rounded-md border border-border text-muted-foreground hover:bg-muted disabled:opacity-40",
                                                    disabled: servings() <= 1,
                                                    aria_label: "Fewer servings",
                                                    onclick: move |_| { let v = servings(); if v > 1 { servings.set(v - 1); } },
                                                    "−"
                                                }
                                                span { class: "min-w-[5.5rem] text-center text-sm tabular-nums", "{servings()} servings" }
                                                button {
                                                    class: "flex size-7 items-center justify-center rounded-md border border-border text-muted-foreground hover:bg-muted",
                                                    aria_label: "More servings",
                                                    onclick: move |_| servings.set(servings() + 1),
                                                    "+"
                                                }
                                            }
                                        }
                                    }
                                    ul { class: "flex flex-col divide-y divide-border/40",
                                        for (i, ing) in recipe.ingredients.iter().enumerate() {
                                            {
                                                let idx = i as u32;
                                                let name = ing.name.clone();
                                                let key = name.to_lowercase();
                                                let checked = gathered_now.contains(&key);
                                                let lit = focus_now == Some(idx);
                                                let qty = scaled_qty(ing, factor);
                                                let row = if lit {
                                                    "bg-accent/40"
                                                } else {
                                                    "hover:bg-muted/40"
                                                };
                                                rsx! {
                                                    li { key: "{i}",
                                                        button {
                                                            class: "flex w-full items-baseline gap-3 px-4 py-2 text-left transition-colors {row}",
                                                            onmouseenter: move |_| focus.set(Some(idx)),
                                                            onmouseleave: move |_| focus.set(None),
                                                            onclick: move |_| {
                                                                let mut g = gathered.write();
                                                                if !g.insert(key.clone()) { g.remove(&key); }
                                                            },
                                                            span {
                                                                class: if checked {
                                                                    "flex-1 text-sm text-muted-foreground line-through"
                                                                } else {
                                                                    "flex-1 text-sm text-foreground"
                                                                },
                                                                "{name}"
                                                                if ing.optional {
                                                                    span { class: "text-xs text-muted-foreground", " · optional" }
                                                                }
                                                            }
                                                            if !qty.is_empty() {
                                                                span { class: "shrink-0 font-mono text-xs tabular-nums text-muted-foreground", "{qty}" }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            if !recipe.cookware.is_empty() {
                                div { class: "mt-3 rounded-2xl border border-border bg-card/20 px-4 py-3",
                                    span { class: "text-xs font-semibold uppercase tracking-wide text-muted-foreground", "Equipment" }
                                    p { class: "mt-1 text-sm text-foreground",
                                        {recipe.cookware.iter().cloned().collect::<Vec<_>>().join(" · ")}
                                    }
                                }
                            }
                        }

                        // ── The spine ─────────────────────────────
                        div { class: "mt-6 lg:mt-0",
                            for (pi, phase) in plan.iter().enumerate() {
                                div { key: "{pi}", class: "relative",
                                    if let Some(label) = &phase.label {
                                        div { class: "sticky top-0 z-10 -mx-1 bg-background/95 px-1 py-2 backdrop-blur",
                                            div { class: "flex items-center gap-3",
                                                span { class: "text-sm font-semibold uppercase tracking-[0.14em] text-foreground", "{label}" }
                                                span { class: "h-px flex-1 bg-border" }
                                                span { class: "text-xs tabular-nums text-muted-foreground",
                                                    {
                                                        let secs: u32 = phase.steps.iter().map(|i| dwell(&recipe.cook_steps[*i])).sum();
                                                        if secs > 0 { format!("{} waiting", duration_hms(secs)) } else { String::new() }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    for (si, i) in phase.steps.iter().copied().enumerate() {
                                        {
                                            let step = &recipe.cook_steps[i];
                                            let is_done = done_now.contains(&i);
                                            let wait = dwell(step);
                                            let last = si + 1 == phase.steps.len();
                                            rsx! {
                                                StepRow {
                                                    key: "{i}",
                                                    step: step.clone(),
                                                    number: i + 1,
                                                    ingredients: recipe.ingredients.iter().cloned().collect::<Vec<_>>(),
                                                    factor,
                                                    done: is_done,
                                                    wait,
                                                    last,
                                                    focus_now,
                                                    on_focus: move |v| focus.set(v),
                                                    on_toggle: move |()| {
                                                        let mut d = done_steps.write();
                                                        if !d.insert(i) { d.remove(&i); }
                                                    },
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// One node on the spine: the marker and its connector on the left, the
/// step's text and its own ingredients on the right.
#[component]
#[allow(clippy::too_many_arguments)]
fn StepRow(
    step: CookStep,
    number: usize,
    ingredients: Vec<Ingredient>,
    factor: f64,
    done: bool,
    wait: u32,
    last: bool,
    focus_now: Option<u32>,
    on_focus: EventHandler<Option<u32>>,
    on_toggle: EventHandler<()>,
) -> Element {
    let segs = segments(&step);
    let marker = if done {
        "border-success bg-success text-success-foreground"
    } else {
        "border-border bg-background text-muted-foreground"
    };

    rsx! {
        div { class: "relative flex gap-4 pb-2",

            // Spine. The connector below a step that makes you wait is
            // drawn dashed and labelled, so the shape of the recipe —
            // work, wait, work — is legible without reading it.
            div { class: "relative flex w-8 shrink-0 flex-col items-center",
                button {
                    class: "z-10 flex size-8 shrink-0 items-center justify-center rounded-full border text-[13px] font-semibold tabular-nums transition-colors {marker}",
                    aria_label: "Mark step {number} done",
                    onclick: move |_| on_toggle.call(()),
                    "{number}"
                }
                if !last {
                    div {
                        class: if wait > 0 {
                            "w-px flex-1 border-l border-dashed border-warning/60"
                        } else if done {
                            "w-px flex-1 bg-success/40"
                        } else {
                            "w-px flex-1 bg-border"
                        },
                    }
                }
            }

            // Body.
            div { class: "min-w-0 flex-1 pb-6",
                p {
                    class: if done {
                        "text-[17px] leading-relaxed text-muted-foreground line-through sm:text-lg"
                    } else {
                        "text-[17px] leading-relaxed text-foreground sm:text-lg"
                    },
                    for (k, seg) in segs.iter().enumerate() {
                        match seg {
                            Seg::Text(t) => rsx! { span { key: "{k}", "{t}" } },
                            Seg::Ing(r) => {
                                let idx = r.index;
                                let lit = focus_now == Some(idx);
                                let cls = if lit {
                                    "rounded bg-accent/60 px-0.5 font-medium text-accent-foreground"
                                } else {
                                    "rounded px-0.5 font-medium text-foreground underline decoration-dotted decoration-muted-foreground/60 underline-offset-4"
                                };
                                rsx! {
                                    span {
                                        key: "{k}",
                                        class: "{cls} cursor-help transition-colors",
                                        onmouseenter: move |_| on_focus.call(Some(idx)),
                                        onmouseleave: move |_| on_focus.call(None),
                                        onclick: move |_| on_focus.call(if lit { None } else { Some(idx) }),
                                        "{r.name}"
                                    }
                                }
                            }
                        }
                    }
                }

                // This step's own ingredients, with amounts. The reason
                // you never scroll back up mid-step.
                if !step.ingredients.is_empty() {
                    div { class: "mt-2 flex flex-wrap items-center gap-x-3 gap-y-1 border-l-2 border-accent/50 pl-3 text-[13px] text-muted-foreground",
                        for (k, r) in step.ingredients.iter().enumerate() {
                            {
                                let ing = ingredients.get(r.index as usize);
                                let qty = ing.map(|i| scaled_qty(i, factor)).unwrap_or_default();
                                let label = ing.map_or_else(|| r.name.clone(), |i| i.name.clone());
                                let idx = r.index;
                                let lit = focus_now == Some(idx);
                                rsx! {
                                    span {
                                        key: "{k}",
                                        class: if lit { "text-accent-foreground" } else { "" },
                                        onmouseenter: move |_| on_focus.call(Some(idx)),
                                        onmouseleave: move |_| on_focus.call(None),
                                        span { class: "font-mono tabular-nums", "{qty}" }
                                        if !qty.is_empty() { " " }
                                        "{label}"
                                    }
                                }
                            }
                        }
                    }
                }

                // Timers, and how long this step leaves you idle.
                if !step.timers.is_empty() {
                    div { class: "mt-2 flex flex-wrap items-center gap-2",
                        for (k, t) in step.timers.iter().enumerate() {
                            span {
                                key: "{k}",
                                class: "inline-flex items-center gap-1.5 rounded-full border border-warning/40 bg-warning/10 px-2.5 py-1 text-xs font-medium text-warning",
                                TimerIcon { size: 12 }
                                if let Some(n) = &t.name {
                                    if !n.is_empty() { "{n} · " }
                                }
                                span { class: "font-mono tabular-nums", "{duration_hms(t.seconds)}" }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Total hands-off time across the recipe, for the meta strip.
#[allow(dead_code)]
fn total_wait(recipe: &Recipe) -> String {
    let secs: u32 = recipe.cook_steps.iter().map(dwell).sum();
    if secs == 0 {
        String::new()
    } else {
        format!("{} hands-off", duration_hms(secs))
    }
}

/// Kept for the meta strip's numeric formatting.
#[allow(dead_code)]
fn pct(done: usize, total: usize) -> String {
    if total == 0 {
        return "0".into();
    }
    fmt_num((done as f64 / total as f64) * 100.0)
}
