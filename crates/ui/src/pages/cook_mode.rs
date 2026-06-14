//! Cook mode — a full-screen, step-by-step cook-along for one recipe.
//!
//! Built straight off the structured [`cookbook_proto::Recipe`] the
//! parser now produces: ingredient/cookware/timer names are inlined in
//! each step's text, and every `~{…}` timer is a [`RecipeTimer`] with a
//! second count. So a step that reads "steep the tea for 4 minutes"
//! carries a one-tap **Start 4:00** countdown — no math, no leaving the
//! page. Multiple timers run at once; each fires a tray notice (and a
//! short beep on web) when it lands.
//!
//! Mobile-first: a fixed full-screen sheet, fat tap targets, the
//! running timers pinned under the header so they stay visible while
//! you scroll the steps.

use std::collections::HashSet;

use cookbook_proto::{Recipe, RecipeTimer};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Check, Clock, Flame, Play, Users, X};
use fts_ui::prelude::*;

/// A countdown started from a step's timer. `remaining` ticks down once
/// a second; at zero it's `done` and stays pinned until dismissed.
#[derive(Clone, PartialEq)]
struct RunningTimer {
    id: u64,
    label: String,
    total: u32,
    remaining: u32,
}

#[component]
pub fn CookMode(recipe: Recipe, on_close: EventHandler<()>) -> Element {
    let mut timers = use_signal(Vec::<RunningTimer>::new);
    let mut next_id = use_signal(|| 0u64);
    let mut gathered = use_signal(HashSet::<usize>::new);
    let mut done_steps = use_signal(HashSet::<usize>::new);
    let notices = architect::use_notifications();

    // Servings scaler — multiplies ingredient quantities (timers are
    // left alone; cooking time isn't proportional to batch size).
    let base_servings = recipe.servings.unwrap_or(1).max(1);
    let mut target_servings = use_signal(move || base_servings);
    let factor = f64::from(target_servings()) / f64::from(base_servings);

    // One ticker for every running timer. Decrement once a second;
    // announce + beep the ones that just hit zero (kept in the list so
    // the "Done" chip stays visible until dismissed).
    use_future(move || async move {
        loop {
            sleep_one_second().await;
            if timers.read().iter().all(|t| t.remaining == 0) {
                continue;
            }
            let mut just_finished: Vec<String> = Vec::new();
            timers.with_mut(|list| {
                for t in list.iter_mut() {
                    if t.remaining > 0 {
                        t.remaining -= 1;
                        if t.remaining == 0 {
                            just_finished.push(t.label.clone());
                        }
                    }
                }
            });
            for label in just_finished {
                notices.info(format!("⏰ {label} — time's up"));
                beep();
            }
        }
    });

    let mut start_timer = move |t: &RecipeTimer| {
        let secs = t.seconds.max(1);
        let label = t
            .name
            .clone()
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| "Timer".to_string());
        let id = next_id();
        next_id += 1;
        timers.write().push(RunningTimer {
            id,
            label,
            total: secs,
            remaining: secs,
        });
    };

    let running = timers.read().clone();
    let total_steps = recipe.cook_steps.len().max(recipe.steps.len());
    let done_count = done_steps.read().len();

    rsx! {
        div { class: "fixed inset-0 z-50 flex flex-col bg-background text-foreground",
            // ── Header ───────────────────────────────────────────
            header { class: "flex items-center gap-3 border-b border-border px-3 py-2 pt-[calc(0.5rem+env(safe-area-inset-top,0px))]",
                button {
                    class: "flex size-10 shrink-0 items-center justify-center rounded-lg text-muted-foreground hover:bg-muted hover:text-foreground",
                    aria_label: "Close cook mode",
                    onclick: move |_| on_close.call(()),
                    X { size: 20 }
                }
                div { class: "flex min-w-0 flex-1 flex-col",
                    Heading { level: HeadingLevel::H2, class: "truncate text-base font-semibold", "{recipe.name}" }
                    div { class: "flex flex-wrap items-center gap-x-3 gap-y-0.5 text-xs text-muted-foreground",
                        if let Some(s) = recipe.servings {
                            span { class: "inline-flex items-center gap-1", Users { size: 12 } "{s} servings" }
                        }
                        if let Some(p) = recipe.prep_minutes {
                            span { class: "inline-flex items-center gap-1", Clock { size: 12 } "{p}m prep" }
                        }
                        if let Some(c) = recipe.cook_minutes {
                            span { class: "inline-flex items-center gap-1", Flame { size: 12 } "{c}m cook" }
                        }
                        if total_steps > 0 {
                            span { "{done_count}/{total_steps} steps" }
                        }
                    }
                }
            }

            // ── Running timers (pinned) ──────────────────────────
            if !running.is_empty() {
                div { class: "flex gap-2 overflow-x-auto border-b border-border bg-muted/30 px-3 py-2",
                    for t in running.iter() {
                        {
                            let id = t.id;
                            let done = t.remaining == 0;
                            let pill = if done {
                                "border-success/50 bg-success/15 text-success"
                            } else {
                                "border-primary/40 bg-primary/10 text-foreground"
                            };
                            rsx! {
                                div {
                                    key: "{id}",
                                    class: "flex shrink-0 items-center gap-2 rounded-full border px-3 py-1.5 text-sm {pill}",
                                    span { class: "font-medium", "{t.label}" }
                                    span { class: "font-mono tabular-nums", "{fmt_mmss(t.remaining)}" }
                                    button {
                                        class: "flex size-5 items-center justify-center rounded-full hover:bg-foreground/10",
                                        aria_label: "Dismiss timer",
                                        onclick: move |_| { timers.write().retain(|x| x.id != id); },
                                        X { size: 13 }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // ── Scrollable body ──────────────────────────────────
            div { class: "flex-1 overflow-y-auto px-3 pb-[calc(2rem+env(safe-area-inset-bottom,0px))] pt-3",
                div { class: "mx-auto flex w-full max-w-2xl flex-col gap-5",

                    // Ingredients — tap to check off as you gather.
                    if !recipe.ingredients.is_empty() {
                        section { class: "flex flex-col gap-2",
                            div { class: "flex items-center justify-between gap-3",
                                Heading { level: HeadingLevel::H3, class: "text-sm font-semibold uppercase tracking-wide text-muted-foreground", "Ingredients" }
                                // Servings scaler — only when the recipe declares a yield.
                                if recipe.servings.is_some() {
                                    div { class: "flex items-center gap-1.5 rounded-full border border-border bg-card/60 px-1 py-0.5",
                                        button {
                                            class: "flex size-7 items-center justify-center rounded-full text-muted-foreground hover:bg-muted disabled:opacity-40",
                                            disabled: target_servings() <= 1,
                                            aria_label: "Fewer servings",
                                            onclick: move |_| { let v = target_servings(); if v > 1 { target_servings.set(v - 1); } },
                                            "−"
                                        }
                                        span { class: "min-w-[4.5rem] text-center text-xs tabular-nums text-foreground",
                                            "{target_servings()} servings"
                                        }
                                        button {
                                            class: "flex size-7 items-center justify-center rounded-full text-muted-foreground hover:bg-muted",
                                            aria_label: "More servings",
                                            onclick: move |_| target_servings.set(target_servings() + 1),
                                            "+"
                                        }
                                    }
                                }
                            }
                            div { class: "flex flex-col divide-y divide-border/50 overflow-hidden rounded-xl border border-border bg-card/40",
                                for (i, ing) in recipe.ingredients.iter().enumerate() {
                                    {
                                        let checked = gathered.read().contains(&i);
                                        let qty = scaled_qty(ing, factor);
                                        let name = ing.name.clone();
                                        rsx! {
                                            button {
                                                key: "{i}",
                                                class: "flex min-h-[44px] items-center gap-3 px-3 py-2 text-left transition-colors hover:bg-muted/40",
                                                onclick: move |_| {
                                                    let mut g = gathered.write();
                                                    if !g.insert(i) { g.remove(&i); }
                                                },
                                                span {
                                                    class: if checked {
                                                        "flex size-5 shrink-0 items-center justify-center rounded-md border border-success bg-success text-success-foreground"
                                                    } else {
                                                        "flex size-5 shrink-0 items-center justify-center rounded-md border border-border"
                                                    },
                                                    if checked { Check { size: 13 } }
                                                }
                                                span {
                                                    class: if checked { "text-sm text-muted-foreground line-through" } else { "text-sm text-foreground" },
                                                    if !qty.is_empty() {
                                                        span { class: "font-medium", "{qty} " }
                                                    }
                                                    "{name}"
                                                    if ing.optional {
                                                        span { class: "text-xs text-muted-foreground", " (optional)" }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Steps — each with its inline text + one-tap timers.
                    section { class: "flex flex-col gap-2",
                        Heading { level: HeadingLevel::H3, class: "text-sm font-semibold uppercase tracking-wide text-muted-foreground", "Steps" }
                        for (i, step) in recipe.cook_steps.iter().enumerate() {
                            {
                                let done = done_steps.read().contains(&i);
                                let card = if done {
                                    "border-border/60 bg-card/30 opacity-60"
                                } else {
                                    "border-border bg-card/60"
                                };
                                rsx! {
                                    div { key: "{i}", class: "flex gap-3 rounded-xl border p-3 {card}",
                                        // Step number doubles as the done toggle.
                                        button {
                                            class: if done {
                                                "flex size-7 shrink-0 items-center justify-center rounded-full bg-success text-success-foreground"
                                            } else {
                                                "flex size-7 shrink-0 items-center justify-center rounded-full border border-border text-sm font-semibold text-muted-foreground hover:border-primary hover:text-foreground"
                                            },
                                            aria_label: "Toggle step done",
                                            onclick: move |_| {
                                                let mut d = done_steps.write();
                                                if !d.insert(i) { d.remove(&i); }
                                            },
                                            if done { Check { size: 15 } } else { "{i + 1}" }
                                        }
                                        div { class: "flex min-w-0 flex-1 flex-col gap-2",
                                            p {
                                                class: if done { "text-sm leading-relaxed text-muted-foreground line-through" } else { "text-sm leading-relaxed text-foreground" },
                                                "{step.text}"
                                            }
                                            if !step.timers.is_empty() {
                                                div { class: "flex flex-wrap gap-2",
                                                    for (ti, timer) in step.timers.iter().enumerate() {
                                                        {
                                                            let t = timer.clone();
                                                            let secs = t.seconds.max(1);
                                                            rsx! {
                                                                button {
                                                                    key: "{ti}",
                                                                    class: "inline-flex min-h-[36px] items-center gap-1.5 rounded-full border border-primary/40 bg-primary/10 px-3 py-1 text-sm font-medium text-primary transition-colors hover:bg-primary/20",
                                                                    onclick: move |_| start_timer(&t),
                                                                    Play { size: 13 }
                                                                    if let Some(n) = &timer.name {
                                                                        if !n.is_empty() {
                                                                            span { "{n} · " }
                                                                        }
                                                                    }
                                                                    span { class: "font-mono tabular-nums", "{fmt_mmss(secs)}" }
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
            }
        }
    }
}

/// The gather-list quantity prefix, scaled by `factor`. Scales the
/// numeric `qty` when present; otherwise keeps the original display
/// form (ranges / fractions / text) unscaled — at `factor == 1` it
/// reads exactly as written.
fn scaled_qty(ing: &cookbook_proto::Ingredient, factor: f64) -> String {
    match ing.qty {
        Some(q) => {
            let num = fmt_num(q * factor);
            if ing.unit.is_empty() {
                num
            } else {
                format!("{num} {}", ing.unit)
            }
        }
        None => match (&ing.qty_display, ing.unit.as_str()) {
            (Some(q), "") => q.clone(),
            (Some(q), u) => format!("{q} {u}"),
            (None, "") => String::new(),
            (None, u) => u.to_string(),
        },
    }
}

/// Trim a scaled quantity to a tidy form — whole numbers lose the
/// decimal, others keep up to two places without trailing zeros.
fn fmt_num(v: f64) -> String {
    if (v.fract()).abs() < 1e-9 {
        format!("{}", v.round() as i64)
    } else {
        let s = format!("{v:.2}");
        s.trim_end_matches('0').trim_end_matches('.').to_string()
    }
}

/// Route target for `/mealplan/recipe?:path` — resolves one recipe from
/// the shared cookbook store (cache-first after a `/mealplan` visit)
/// and drops straight into [`CookMode`]. Back closes to `/mealplan`.
#[component]
pub fn RecipeCookView(path: String) -> Element {
    let nav = use_navigator();
    let recipes = crate::stores::use_recipe_list();
    let store = crate::stores::use_recipe_store();
    let target = path.clone();
    let found = recipes.value().and_then(|rows| {
        rows.iter()
            .find(|(_, r)| r.path == target)
            .map(|(_, r)| r.clone())
    });

    let to_mealplan = move |()| {
        nav.push(crate::routes::Route::MealplanRoute {});
    };

    match found {
        Some(recipe) => rsx! {
            CookMode { recipe, on_close: to_mealplan }
        },
        None if recipes.is_waiting() => rsx! {
            div { class: "flex h-full items-center justify-center p-8",
                crate::states::LoadingState {}
            }
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
                    on_click: move |_| to_mealplan(()),
                    "Back to mealplan"
                }
            }
        },
    }
}

/// Seconds → `M:SS` (or `H:MM:SS` past an hour).
fn fmt_mmss(secs: u32) -> String {
    let h = secs / 3600;
    let m = (secs % 3600) / 60;
    let s = secs % 60;
    if h > 0 {
        format!("{h}:{m:02}:{s:02}")
    } else {
        format!("{m}:{s:02}")
    }
}

/// A short two-tone chime when a timer lands. Web Audio only; a no-op
/// off the web (the tray notice still fires everywhere).
#[cfg(target_arch = "wasm32")]
fn beep() {
    let _ = dioxus::document::eval(
        "try{const c=new (window.AudioContext||window.webkitAudioContext)();\
         const o=c.createOscillator();const g=c.createGain();o.connect(g);g.connect(c.destination);\
         o.type='sine';o.frequency.value=880;g.gain.setValueAtTime(0.001,c.currentTime);\
         g.gain.exponentialRampToValueAtTime(0.3,c.currentTime+0.02);\
         g.gain.exponentialRampToValueAtTime(0.001,c.currentTime+0.6);\
         o.start();o.stop(c.currentTime+0.6);}catch(e){}",
    );
}

#[cfg(not(target_arch = "wasm32"))]
fn beep() {}

#[cfg(target_arch = "wasm32")]
async fn sleep_one_second() {
    gloo_timers::future::TimeoutFuture::new(1000).await;
}

#[cfg(not(target_arch = "wasm32"))]
async fn sleep_one_second() {
    futures_util::future::pending::<()>().await;
}
