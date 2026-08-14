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
//! timer is drawn as a dashed dwell segment. You can see, without
//! reading a word, where the work clusters and where you're waiting.
//! Phase bands (`= Prep`, `= Cook`) break the spine into named runs and
//! total their own waiting.
//!
//! ## What the parser buys us
//!
//! Every step knows which ingredients it uses and exactly where each
//! one sits in its own text ([`cookbook_proto::StepIngredient`]), so:
//!
//! - step text renders as a token stream, each ingredient marked in
//!   place rather than being flat prose;
//! - each step carries a recap of just its own ingredients, with
//!   amounts, so you never scroll up mid-step;
//! - hovering or tapping any mention lights every other mention of that
//!   ingredient *and* its row in the rail, in both directions.
//!
//! Scaling honours what cooklang knows: a pinned `{=1%tsp}` doesn't
//! move, and is marked so you can see why.
//!
//! ## Shape
//!
//! Phone gets one column: the spine hugs the left edge, steps are set
//! large, and the ingredient list collapses into a summary. From `lg` —
//! an iPad on a stand, the actual reading posture — ingredients move
//! into a sticky rail beside the spine, which is what makes the
//! cross-highlighting worth having.

use std::collections::HashSet;

use cookbook_proto::{CookStep, Ingredient, Recipe, StepIngredient};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    ChevronLeft, Clock, CookingPot, ExternalLink, Flame, Hourglass, Lock, Pencil,
    Timer as TimerIcon, Users,
};
use fts_ui::prelude::*;

use super::cook_mode::scaled_qty;
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

/// How much of each step's ingredient recap to show.
///
/// The right density depends on the cook, not the recipe: someone who
/// knows a dish wants the prose alone, someone following it closely
/// wants every amount under every step. Cheap to offer, so offer it.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Density {
    /// One wrapped line of `qty name · qty name`.
    Line,
    /// One row per ingredient.
    List,
    /// Nothing — the inline marks in the step text carry it.
    Off,
}

impl Density {
    fn next(self) -> Self {
        match self {
            Density::Line => Density::List,
            Density::List => Density::Off,
            Density::Off => Density::Line,
        }
    }
    fn label(self) -> &'static str {
        match self {
            Density::Line => "Compact",
            Density::List => "Detailed",
            Density::Off => "Hidden",
        }
    }
}

/// One piece of a step's text: prose, or an ingredient mention carrying
/// the index of the row it belongs to.
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

/// Seconds a step will keep you waiting.
fn dwell(step: &CookStep) -> u32 {
    step.timers.iter().map(|t| t.seconds).sum()
}

/// Total hands-off time across the recipe. Worth surfacing because it's
/// the number that decides whether a dish fits the evening, and it's
/// only knowable because timers are parsed rather than prose.
fn hands_off(recipe: &Recipe) -> u32 {
    recipe.cook_steps.iter().map(dwell).sum()
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
    // The ingredient under the pointer, or pinned by tap where there is
    // no hover to speak of.
    let mut focus = use_signal(|| None::<u32>);
    let mut show_ingredients = use_signal(|| false);
    let mut density = use_signal(|| Density::Line);

    let base = recipe.servings.unwrap_or(1).max(1);
    let mut servings = use_signal(move || base);
    let factor = f64::from(servings()) / f64::from(base);
    let scaled = servings() != base;

    let plan = phases(&recipe);
    let total_steps = recipe.cook_steps.len();
    let done_now = done_steps.read().clone();
    let gathered_now = gathered.read().clone();
    let focus_now = focus();
    let done_count = done_now.len();
    let ing_total = recipe.ingredients.len();
    let idle = hands_off(&recipe);

    let step_pct = if total_steps == 0 {
        0.0
    } else {
        (done_count as f64 / total_steps as f64) * 100.0
    };
    let gather_pct = if ing_total == 0 {
        0.0
    } else {
        (gathered_now.len() as f64 / ing_total as f64) * 100.0
    };

    let cook_path = recipe.path.clone();
    let edit_path = recipe.path.clone();

    rsx! {
        div { class: "flex h-full min-h-0 flex-col bg-background text-foreground",

            // ── Chrome ───────────────────────────────────────────
            // Deliberately thin: the recipe's own title lives in the
            // page, so this is just the way back and the way forward.
            header { class: "relative flex items-center gap-2 border-b border-border px-2 py-2 sm:px-3",
                button {
                    class: "flex size-10 shrink-0 items-center justify-center rounded-lg text-muted-foreground transition-colors hover:bg-muted hover:text-foreground",
                    aria_label: "Back to mealplan",
                    onclick: move |_| { nav.push(crate::routes::Route::MealplanRoute {}); },
                    ChevronLeft { size: 20 }
                }
                span { class: "min-w-0 flex-1 truncate text-sm text-muted-foreground", "{recipe.name}" }
                if total_steps > 0 {
                    span { class: "shrink-0 text-xs tabular-nums text-muted-foreground",
                        "{done_count}/{total_steps}"
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
                // Progress hairline, flush to the bottom edge.
                div {
                    class: "absolute inset-x-0 bottom-0 h-0.5 bg-success transition-all duration-300",
                    style: "width: {step_pct}%",
                }
            }

            // ── Scrollable body ──────────────────────────────────
            div { class: "min-h-0 flex-1 overflow-y-auto",
                div { class: "mx-auto w-full max-w-6xl px-4 pb-20 pt-6 sm:px-6",

                    // ── Title block ──────────────────────────────
                    // The dish leads. Everything here is a fact you'd
                    // want before committing an evening to it.
                    header { class: "flex flex-col gap-3 border-b border-border pb-6",
                        Heading {
                            level: HeadingLevel::H1,
                            class: "text-3xl font-semibold leading-[1.1] tracking-tight sm:text-4xl",
                            "{recipe.name}"
                        }
                        if let Some(d) = &recipe.description {
                            p { class: "max-w-prose text-[15px] leading-relaxed text-muted-foreground", "{d}" }
                        }
                        div { class: "flex flex-wrap items-center gap-x-5 gap-y-2 text-sm text-muted-foreground",
                            if let Some(s) = recipe.servings {
                                span { class: "inline-flex items-center gap-1.5", Users { size: 14 } "{s} servings" }
                            }
                            if let Some(p) = recipe.prep_minutes {
                                span { class: "inline-flex items-center gap-1.5", Clock { size: 14 } "{p} min prep" }
                            }
                            if let Some(c) = recipe.cook_minutes {
                                span { class: "inline-flex items-center gap-1.5", Flame { size: 14 } "{c} min cook" }
                            }
                            if idle > 0 {
                                span { class: "inline-flex items-center gap-1.5",
                                    Hourglass { size: 14 }
                                    "{duration_hms(idle)} hands-off"
                                }
                            }
                        }
                        if !recipe.tags.is_empty() || recipe.course.is_some() || recipe.source_url.is_some() {
                            div { class: "flex flex-wrap items-center gap-1.5",
                                if let Some(course) = &recipe.course {
                                    span { class: "rounded-full border border-border px-2.5 py-0.5 text-xs capitalize text-foreground", "{course}" }
                                }
                                for (i, tag) in recipe.tags.iter().enumerate() {
                                    span { key: "{i}", class: "rounded-full bg-muted/60 px-2.5 py-0.5 text-xs text-muted-foreground", "{tag}" }
                                }
                                if let Some(url) = &recipe.source_url {
                                    a {
                                        class: "inline-flex items-center gap-1 rounded-full px-2 py-0.5 text-xs text-muted-foreground underline-offset-4 hover:text-foreground hover:underline",
                                        href: "{url}",
                                        target: "_blank",
                                        rel: "noreferrer",
                                        ExternalLink { size: 11 }
                                        "Source"
                                    }
                                }
                            }
                        }
                    }

                    // ── Two panes from lg: rail beside the spine ──
                    div { class: "gap-10 pt-6 lg:grid lg:grid-cols-[minmax(16rem,20rem)_1fr] lg:items-start",

                        aside { class: "lg:sticky lg:top-6",
                            div { class: "overflow-hidden rounded-2xl border border-border bg-card/40",
                                button {
                                    class: "flex w-full items-center gap-2 px-4 py-3 text-left lg:cursor-default",
                                    onclick: move |_| { let v = show_ingredients(); show_ingredients.set(!v); },
                                    span { class: "flex-1 text-xs font-semibold uppercase tracking-[0.14em] text-muted-foreground", "Ingredients" }
                                    span { class: "text-xs tabular-nums text-muted-foreground",
                                        "{gathered_now.len()}/{ing_total}"
                                    }
                                    span { class: "text-muted-foreground lg:hidden",
                                        if show_ingredients() { "−" } else { "+" }
                                    }
                                }
                                // Gathering progress, so the rail says
                                // how far you are without counting.
                                div { class: "h-0.5 bg-border/40",
                                    div {
                                        class: "h-full bg-success transition-all duration-300",
                                        style: "width: {gather_pct}%",
                                    }
                                }
                                div {
                                    class: if show_ingredients() { "" } else { "hidden lg:block" },
                                    if recipe.servings.is_some() {
                                        div { class: "flex flex-col gap-2 border-b border-border/60 px-4 py-3",
                                            div { class: "flex items-center justify-between gap-2",
                                                span { class: "text-xs text-muted-foreground", "Scale" }
                                                div { class: "flex items-center gap-1",
                                                    button {
                                                        class: "flex size-7 items-center justify-center rounded-md border border-border text-muted-foreground transition-colors hover:bg-muted disabled:opacity-40",
                                                        disabled: servings() <= 1,
                                                        aria_label: "Fewer servings",
                                                        onclick: move |_| { let v = servings(); if v > 1 { servings.set(v - 1); } },
                                                        "−"
                                                    }
                                                    span { class: "min-w-[3rem] text-center text-sm font-medium tabular-nums text-foreground", "{servings()}" }
                                                    button {
                                                        class: "flex size-7 items-center justify-center rounded-md border border-border text-muted-foreground transition-colors hover:bg-muted",
                                                        aria_label: "More servings",
                                                        onclick: move |_| servings.set(servings() + 1),
                                                        "+"
                                                    }
                                                }
                                            }
                                            // Halving and doubling are what
                                            // people actually do; the stepper
                                            // is for the odd number.
                                            div { class: "flex items-center gap-1.5",
                                                button {
                                                    class: "rounded-md border border-border px-2 py-1 text-xs text-muted-foreground transition-colors hover:bg-muted hover:text-foreground",
                                                    onclick: move |_| servings.set((base / 2).max(1)),
                                                    "Half"
                                                }
                                                button {
                                                    class: "rounded-md border border-border px-2 py-1 text-xs text-muted-foreground transition-colors hover:bg-muted hover:text-foreground",
                                                    onclick: move |_| servings.set(base * 2),
                                                    "Double"
                                                }
                                                if scaled {
                                                    button {
                                                        class: "rounded-md px-2 py-1 text-xs text-muted-foreground underline underline-offset-4 transition-colors hover:text-foreground",
                                                        onclick: move |_| servings.set(base),
                                                        "Reset"
                                                    }
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
                                                let pinned = scaled && !ing.scalable && ing.qty.is_some();
                                                let row = if lit { "bg-accent/40" } else { "hover:bg-muted/40" };
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
                                                                span { class: "inline-flex shrink-0 items-center gap-1 font-mono text-xs tabular-nums text-muted-foreground",
                                                                    // A pinned quantity didn't move when
                                                                    // you scaled. Saying so is kinder than
                                                                    // letting it look like a bug.
                                                                    if pinned {
                                                                        span { title: "Fixed — doesn't scale", Lock { size: 10 } }
                                                                    }
                                                                    "{qty}"
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
                            if !recipe.cookware.is_empty() {
                                div { class: "mt-3 rounded-2xl border border-border bg-card/20 px-4 py-3",
                                    span { class: "text-xs font-semibold uppercase tracking-[0.14em] text-muted-foreground", "Equipment" }
                                    p { class: "mt-1.5 text-sm leading-relaxed text-foreground",
                                        {recipe.cookware.iter().cloned().collect::<Vec<_>>().join(" · ")}
                                    }
                                }
                            }
                        }

                        // ── The spine ─────────────────────────────
                        div { class: "mt-8 lg:mt-0",
                            if total_steps == 0 {
                                div { class: "rounded-2xl border border-dashed border-border px-4 py-10 text-center",
                                    Text { variant: TextVariant::Muted, class: "text-sm",
                                        "This recipe has no steps yet. Edit it to add some."
                                    }
                                }
                            } else {
                                // Method header carries the density
                                // control, which belongs to the steps.
                                div { class: "mb-4 flex items-center justify-between gap-3",
                                    span { class: "text-xs font-semibold uppercase tracking-[0.14em] text-muted-foreground", "Method" }
                                    button {
                                        class: "rounded-md border border-border px-2 py-1 text-xs text-muted-foreground transition-colors hover:bg-muted hover:text-foreground",
                                        title: "How much ingredient detail to show under each step",
                                        onclick: move |_| { let d = density(); density.set(d.next()); },
                                        "Amounts: {density().label()}"
                                    }
                                }
                            }
                            for (pi, phase) in plan.iter().enumerate() {
                                div { key: "{pi}", class: "relative",
                                    if let Some(label) = &phase.label {
                                        div { class: "sticky top-0 z-10 -mx-1 mb-1 bg-background/95 px-1 py-2.5 backdrop-blur",
                                            div { class: "flex items-center gap-3",
                                                span { class: "text-sm font-semibold uppercase tracking-[0.16em] text-foreground", "{label}" }
                                                span { class: "h-px flex-1 bg-border" }
                                                {
                                                    let secs: u32 = phase.steps.iter().map(|i| dwell(&recipe.cook_steps[*i])).sum();
                                                    if secs > 0 {
                                                        rsx! {
                                                            span { class: "inline-flex items-center gap-1 text-xs tabular-nums text-muted-foreground",
                                                                Hourglass { size: 11 }
                                                                "{duration_hms(secs)}"
                                                            }
                                                        }
                                                    } else {
                                                        rsx! {}
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
                                                    scaled,
                                                    done: is_done,
                                                    wait,
                                                    last,
                                                    density: density(),
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
    scaled: bool,
    done: bool,
    wait: u32,
    last: bool,
    density: Density,
    focus_now: Option<u32>,
    on_focus: EventHandler<Option<u32>>,
    on_toggle: EventHandler<()>,
) -> Element {
    let segs = segments(&step);
    // The markers are the spine's rhythm, so they have to read as
    // structure at a glance rather than as faint noise beside the text.
    let marker = if done {
        "border-success bg-success text-success-foreground"
    } else {
        "border-border bg-muted/60 text-foreground hover:border-primary hover:bg-primary/10"
    };

    rsx! {
        div { class: "relative flex gap-4",

            // Spine. The connector below a step that makes you wait is
            // drawn dashed, so the shape of the recipe — work, wait,
            // work — is legible without reading it.
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
                            "w-px flex-1 border-l border-dashed border-warning/50"
                        } else if done {
                            "w-px flex-1 bg-success/40"
                        } else {
                            "w-px flex-1 bg-border"
                        },
                    }
                }
            }

            // Body.
            div { class: "min-w-0 flex-1 pb-7",
                p {
                    class: if done {
                        "text-[17px] leading-[1.65] text-muted-foreground line-through sm:text-[18px] lg:text-[19px]"
                    } else {
                        "text-[17px] leading-[1.65] text-foreground sm:text-[18px] lg:text-[19px]"
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
                                    "rounded px-0.5 font-medium text-foreground decoration-muted-foreground/50 decoration-dotted underline underline-offset-[5px]"
                                };
                                rsx! {
                                    span {
                                        key: "{k}",
                                        class: "{cls} cursor-pointer transition-colors",
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
                if !step.ingredients.is_empty() && density != Density::Off {
                    div {
                        class: if density == Density::List {
                            "mt-2.5 flex flex-col gap-1 border-l-2 border-accent/60 pl-3 text-[13px]"
                        } else {
                            "mt-2.5 flex flex-wrap items-center gap-y-1 border-l-2 border-accent/60 pl-3 text-[13px]"
                        },
                        for (k, r) in step.ingredients.iter().enumerate() {
                            {
                                let ing = ingredients.get(r.index as usize);
                                let qty = ing.map(|i| scaled_qty(i, factor)).unwrap_or_default();
                                let label = ing.map_or_else(|| r.name.clone(), |i| i.name.clone());
                                let pinned = scaled && ing.is_some_and(|i| !i.scalable && i.qty.is_some());
                                let idx = r.index;
                                let lit = focus_now == Some(idx);
                                let more = k + 1 < step.ingredients.len();
                                rsx! {
                                    span { key: "{k}", class: "inline-flex items-center",
                                        span {
                                            // Kept on one line: an amount that
                                            // wraps away from its ingredient is
                                            // worse than no amount at all.
                                            class: if lit {
                                                "whitespace-nowrap rounded bg-accent/40 px-1 text-accent-foreground"
                                            } else {
                                                "whitespace-nowrap"
                                            },
                                            onmouseenter: move |_| on_focus.call(Some(idx)),
                                            onmouseleave: move |_| on_focus.call(None),
                                            if !qty.is_empty() {
                                                // The amount carries the weight —
                                                // you already read the name in
                                                // the step; what you came back
                                                // for is the number.
                                                span { class: "font-mono tabular-nums text-foreground", "{qty}" }
                                                if pinned {
                                                    span { class: "px-0.5 align-middle text-muted-foreground", title: "Fixed — doesn't scale", Lock { size: 9 } }
                                                }
                                                " "
                                            }
                                            span { class: "text-muted-foreground", "{label}" }
                                        }
                                        // A dot between entries, so the line
                                        // doesn't read as one run of words
                                        // with nowhere for the eye to break.
                                        //
                                        // Only from `sm`: on a phone the row
                                        // wraps to one ingredient per line and
                                        // already reads as a list, where a
                                        // trailing dot is just a dangling mark
                                        // at the end of every line.
                                        if more && density == Density::Line {
                                            span { class: "hidden select-none px-2.5 text-muted-foreground/40 sm:inline", "·" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Timers. Labels, not buttons — the countdown lives in
                // cook mode, and a chip that looks pressable but isn't
                // is worse than one that plainly reads as data.
                if !step.timers.is_empty() {
                    div { class: "mt-2.5 flex flex-wrap items-center gap-2",
                        for (k, t) in step.timers.iter().enumerate() {
                            span {
                                key: "{k}",
                                class: "inline-flex items-center gap-1.5 rounded-full border border-warning/30 bg-warning/10 px-2.5 py-1 text-xs font-medium text-warning",
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
