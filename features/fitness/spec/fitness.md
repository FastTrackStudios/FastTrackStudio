+++
title = "Fitness contract"
description = "Tracey-tracked rules the FitnessRepo + FitnessService implementations must hold."
weight = 100
+++

The fitness feature is a **personal health + training tracker**.
Two halves that interlock:

1. **Nutrition**: daily food intake, calories, macros, micros,
   weight, water — everything OpenNutriTracker / Cronometer cover.
   Reuses `cookbook.food-product` as the canonical food database
   instead of duplicating an SKU list.
2. **Training**: workouts, routines, exercises, set logs, body
   composition — everything wger covers, plus the day-to-day "what
   did I do this week" view.

The two halves share `BodyMeasurement` (weight, BMI, body fat) and
roll up into a single daily summary: "today's calories, today's
training load, today's weight". Done well, the fitness feature is
the user's only health-data home — nothing else should need to be
in Apple Health or wger to answer "how am I doing".

The driving use case: someone tracking a recomposition arc — eating
slightly under maintenance, lifting four times a week, weighing in
weekly — needs a single dashboard that shows calorie deficit
trend, weight trend, lift PRs, and surfaces "you're 200kcal under
your target today, here's what's left to eat".

## Reference repos

| Repo | URL | What to copy |
|---|---|---|
| **OpenNutriTracker** | <https://github.com/simonoppowa/OpenNutriTracker> | Calorie & macro tracking UX — meal slots, food-quick-add, barcode scan, daily summary. The reference for the nutrition half. |
| **wger** | <https://github.com/wger-project/wger> | Exercise library, routine builder, workout logging, body measurements. The reference for the training half. Their database of exercises with images is the gold standard. |
| **OpenFoodFacts** | <https://world.openfoodfacts.org/> | Food database — barcodes, nutrition per 100g, allergens. Same upstream as cookbook (see `cookbook.pantry.barcode-scan`). |

Feature-parity goal: anything OpenNutriTracker tracks day-to-day,
plus anything wger tracks workout-side, in one feature that talks
to `cookbook` for the food catalog and `inventory` for any non-food
gear (e.g. tracking a power rack via inventory, not fitness).

Rules are linked to source via `r[impl <id>]` and `r[verify <id>]`
annotations. Run `cargo xtask tracey-validate` to confirm coverage.

# Nutrition half

## Food log

r[fitness.food-log-entry.shape]
A `FoodLogEntry` row records one eaten thing: `id`,
`food_product_id` (Option — refs `cookbook.FoodProduct`),
`recipe_id` (Option — refs `cookbook.Recipe`), `quantity` (f64),
`unit` (free-text — `g`, `cup`, `serving`, etc.), `meal_slot`
enum (`breakfast`, `lunch`, `dinner`, `snack`),
`consumed_at` (DateTime<Utc>), `notes`. Exactly one of
`food_product_id` or `recipe_id` is set; the repo rejects writes
that set both or neither.

r[fitness.food-log-entry.no-duplicate-catalog]
The fitness feature does NOT own a food catalog. Every entry
references `cookbook.FoodProduct` or `cookbook.Recipe` so nutrition
data is single-sourced. New foods get created in cookbook first
(`CookbookService.create_food_product` or
`CookbookService.lookup_barcode`), then logged in fitness.

r[fitness.food-log-entry.nutrition-snapshot]
On insert, the service captures a **snapshot** of the food's
nutrition values (`calories`, `protein_g`, `fat_g`, `carb_g`,
`fiber_g`, `sodium_mg`, plus optional micros) into
`FoodLogEntry.nutrition_snapshot_json` (Option). The snapshot
freezes the historical record so a later edit to the food product
doesn't retroactively change the day's totals. Re-snapshot on
explicit user action only.

r[fitness.food-log-entry.quick-add]
A free-text "quick add" path: `FoodLogEntry` with
`food_product_id=None`, `recipe_id=None`, `raw_text` set, and
`nutrition_snapshot_json` filled directly from the user's input.
Lets the user log "1 cup whatever, 250 kcal" without creating a
food product. Surfaces in summaries by raw_text instead of name.

r[fitness.food-log-entry.barcode-flow]
The compose UI's barcode-scan path calls
`cookbook.lookup_barcode(upc)` → returns or creates a `FoodProduct`
→ user enters quantity → service creates `FoodLogEntry` with the
product reference. Offline-cached so re-scanning previously seen
codes works without network.

## Daily targets

r[fitness.daily-target.shape]
A `DailyTarget` row holds the user's calorie + macro goals:
`id`, `effective_from` (Date), `effective_until` (Option Date),
`calorie_target` (u32), `protein_g_target` (u32), `fat_g_target`
(u32), `carb_g_target` (u32), `fiber_g_target` (Option u32),
`sodium_mg_target` (Option u32), `water_ml_target` (Option u32),
`reasoning` (free-text — "cut phase 2026Q2"). Most-recent record
where `effective_from <= today <= effective_until` (or open) is
the active target.

r[fitness.daily-target.calculator]
`FitnessService.suggest_targets(profile)` computes a default
target from the user's `BodyMeasurement`s and activity level
using a Mifflin-St Jeor BMR + activity multiplier + macro split
(default 40/30/30 carbs/protein/fat). User can accept, edit, or
discard. Suggestion only — never auto-applied.

## Daily summary

r[fitness.daily-summary.derivation]
`FitnessService.daily_summary(date)` aggregates the day's
`FoodLogEntry` rows: sum each macro and calorie field using
`nutrition_snapshot_json` (or recompute from
`food_product.nutrition_per_100` if no snapshot), then return
`{date, calories_in, protein_g, fat_g, carb_g, fiber_g,
sodium_mg, water_ml, meal_breakdown, target, deficit_or_surplus}`.

r[fitness.daily-summary.target-comparison]
The summary returns the active `DailyTarget` for the date and the
delta per macro. Negative deltas (under target) and positive
deltas (over target) are reported per field so the UI can render
"+220 kcal over, -15g protein under".

r[fitness.daily-summary.caching]
`FitnessDailySummary` table caches per-date summaries with
`recomputed_at` so the dashboard renders instantly. The cache
invalidates on any `FoodLogEntry` insert/update/delete for that
date; recomputation is fast (<5ms typical) so eager-rebuild is
fine.

## Water and hydration

r[fitness.water-log.shape]
A `WaterLogEntry` row: `id`, `volume_ml` (u32), `logged_at`. Simple
running tally; daily total feeds into `daily-summary.water_ml`.
Some users will care, others won't — purely additive feature.

# Training half

## Exercises (library)

r[fitness.exercise.shape]
An `Exercise` row is a movement in the user's library: `id`,
`name`, `category` (Option — `compound`, `isolation`, `cardio`,
`mobility`, `plyometric`), `muscle_groups_primary` (`Vec<String>`),
`muscle_groups_secondary` (`Vec<String>`), `equipment`
(`Vec<String>` — `barbell`, `dumbbell`, `cable`, `bodyweight`),
`tags` (`Vec<String>` — user-applied), `description` (LoroText —
long form, multi-peer editable), `video_url` (Option),
`image_url` (Option).

r[fitness.exercise.import-from-wger]
`FitnessService.import_wger_library()` fetches wger's public
exercise database and seeds `Exercise` rows. Idempotent: dedupes
by `(name, equipment)` tuple. Manual edits to exercises survive
re-import (the importer only fills in NULL fields on existing
rows). Disabled by default; user opts in.

r[fitness.exercise.user-additions]
A user adds private exercises that don't exist in wger
("DB curl with a pause"). User additions live in the same table
with `source` enum (`wger`, `user`, `imported-from-strong`, etc.)
distinguishing origin for analytics.

## Routines

r[fitness.routine.shape]
A `Routine` is a workout plan: `id`, `name`, `description`,
`difficulty` enum (`beginner`, `intermediate`, `advanced`),
`estimated_duration_minutes`, `tags`, `is_template` (bool — a
template can be copied into a `WorkoutSession`).

r[fitness.routine-exercise]
A `RoutineExercise` row links an exercise into a routine in order:
`id`, `routine_id`, `exercise_id`, `sort_key`, `target_sets`
(u32), `target_reps` (Option<u32> for fixed; Option<Range> for
e.g. 8-12), `target_weight_kg` (Option — for fixed-load
prescriptions), `target_rest_seconds` (Option), `target_rpe`
(Option<u8> — RPE 1-10), `notes`.

r[fitness.routine.versioning]
Editing a `Routine` does NOT cascade-update past `WorkoutSession`s
that referenced it — the session captures its prescription on
creation. The repo enforces this by snapshotting routine state
into `WorkoutSession.routine_snapshot_json` at session start.

## Workout sessions

r[fitness.workout-session.shape]
A `WorkoutSession` is one workout instance: `id`, `routine_id`
(Option — None for ad-hoc workouts), `name`, `started_at`,
`completed_at` (Option), `notes`, `bodyweight_kg` (Option —
recorded at session start for relative-to-bw analytics),
`perceived_effort` (Option<u8> — 1-10 RPE at end of session).

r[fitness.workout-session.from-routine]
`FitnessService.start_session_from_routine(routine_id)` clones the
routine's exercises into `SetLog` rows pre-populated with target
values. The user fills in actuals as they lift. Cancelling
mid-workout leaves the session with `completed_at=None` — surfaces
as "in progress" in the UI; user can resume or abandon.

r[fitness.workout-session.history-pr]
`FitnessService.session_summary(session_id)` returns per-exercise
totals (sets, reps, volume = `Σ weight × reps`) plus PR flags:
`pr_1rm_estimate`, `pr_volume`, `pr_reps_at_weight`. PRs computed
against all prior completed sessions for the same exercise.

## Set logs

r[fitness.set-log.shape]
A `SetLog` row is one set: `id`, `session_id`, `exercise_id`,
`sort_key` (lex within session), `set_kind` enum (`working`,
`warmup`, `dropset`, `amrap`, `paused`, `tempo`, `myo-reps`),
`weight_kg` (Option f64), `reps` (Option u32),
`distance_meters` (Option f64 — for cardio),
`duration_seconds` (Option u32 — for time-based work),
`rpe` (Option u8), `rir` (Option u8 — Reps in Reserve),
`rest_after_seconds` (Option u32), `notes`, `completed_at`
(DateTime — when the set was logged).

r[fitness.set-log.partial-mode]
A set may be partially filled — `weight_kg=80, reps=None` is
valid (the user logged the weight as they put it on, will fill
reps after the set). The repo accepts any combination of NULLs;
the analytics layer treats unfilled sets as "in progress".

## Rest timer

r[fitness.rest-timer.start-on-set]
When `SetLog.completed_at` is written, the service emits a
`RestTimerEvent` (`set_id`, `duration_seconds` from the routine
prescription or default), which the UI uses to drive a countdown.
No persistent timer entity in v1 — UI-side only with an event hook
the server emits.

## Body composition

r[fitness.body-measurement.shape]
`BodyMeasurement` extends to a full body-composition record:
`id`, `taken_at`, `weight_grams` (Option), `body_fat_percent`
(Option f64), `muscle_mass_grams` (Option), `waist_cm` (Option),
`hip_cm` (Option), `chest_cm` (Option), `arm_cm` (Option),
`thigh_cm` (Option), `neck_cm` (Option), `notes`, `source` enum
(`manual`, `smart-scale`, `inbody`, `dexa`).

r[fitness.body-measurement.trend]
`FitnessService.weight_trend(start, end, smoothing)` returns a
time-series of weight with optional 7-day or 14-day moving
average. Used for the recomposition view to filter out daily
water-weight noise.

r[fitness.body-measurement.bmi-and-rates]
The summary computes BMI when height is known (height lives in the
user's profile, not on every measurement). Rate of change (kg/week,
%bf/month) is derived from a linear regression over the last 4
weeks.

# Cross-cutting

## Sharing with cookbook

r[fitness.integration.cookbook-food-product]
`FoodLogEntry.food_product_id` and `cookbook.PantryItem.food_product_id`
reference the same `cookbook.FoodProduct` rows. Adding a food via
the fitness barcode flow shows up in the cookbook food catalog and
vice versa. Recipes logged in fitness pull from
`cookbook.Recipe`'s nutrition rollup (see
`cookbook.recipe.nutrition-rollup`).

r[fitness.integration.cookbook-meal-plan]
When a `cookbook.MealPlan` row is marked `cooked_at` AND
the user has the fitness↔cookbook integration enabled, the
service creates a `FoodLogEntry` referencing the recipe at the
planned servings. Bidirectional flag — disabled by default so
manual logging stays primary until the user opts in.

## Sharing with inventory

r[fitness.integration.inventory-equipment]
Gym equipment (rack, bench, plates, dumbbells) is owned by the
`inventory` feature, not fitness. A `RoutineExercise` may
optionally reference an `InventoryItem` (the cable machine,
the barbell) for analytics like "exercises I can do on home gym
without going to the commercial gym". Soft link; not required.

## Goals and reminders

r[fitness.goal.shape]
A `FitnessGoal` row is a long-running objective: `id`, `kind`
enum (`weight-target`, `body-fat-target`, `lift-pr`, `workout-
frequency`, `distance-goal`, `streak`), `target_value` (f64),
`target_unit`, `target_date` (Option), `progress_value`
(denormalized — last computed actual), `started_at`,
`completed_at` (Option), `notes`.

r[fitness.goal.progress-recompute]
`FitnessService.recompute_goal_progress()` updates
`progress_value` for every active goal on a schedule (daily) and
on relevant data changes (new `BodyMeasurement` updates weight
goals, new PR updates lift goals). Triggers a notification when a
goal completes.

## CRDT semantics

r[fitness.crdt.scalars-lww]
Scalar fields on `FoodLogEntry`, `WorkoutSession`, `SetLog`,
`BodyMeasurement` are LWW. Two peers editing the same set log
resolve to the most recent commit.

r[fitness.crdt.exercise-description-loro-text]
`Exercise.description` is `LoroText` so multiple peers can edit
the form-cue notes for an exercise concurrently. Same pattern as
knowledge blocks + recipe steps.

## What this spec does NOT cover (yet)

- **HealthKit / Apple Health / Google Fit sync**: future, separate
  integration spec. v1 is manual entry + barcode + import.
- **Activity / step tracking from a wearable**: out of scope.
  Could fit in a future `fitness.activity-log` entity. v1 records
  workouts and food, not background activity.
- **Workout periodization templates** (e.g. 5/3/1, RP
  hypertrophy): the user can build these as routines manually.
  Native periodization engine is v2.
- **Plate calculator** (given target weight + available plates,
  what to load): UI-side helper, not part of the proto.
- **Heart rate / GPS data from runs**: out of scope. v1 logs
  distance + duration as a set; raw stream data is its own future
  spec.
- **Coaching / AI form review**: future. Hook into the `agent`
  feature when it lands.
- **Nutritionist / shared accounts**: multi-tenant fitness data
  (a coach viewing a client's logs). Out of scope for v1,
  single-user.
