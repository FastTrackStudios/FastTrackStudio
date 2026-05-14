+++
title = "Cookbook contract"
description = "Tracey-tracked rules the CookbookRepo + CookbookService implementations must hold."
weight = 100
+++

The cookbook feature is a **full personal kitchen OS** — recipe
collection, meal planning, pantry tracking, grocery list, nutrition.
Targeted replacement for Mealie, Tandoor, Groceri.es, and Grocy:
self-hosted, local-first, and tightly integrated with the rest of
the Task system (calendar for meal slots, shopping for store lists,
inventory only for non-food gear).

The driving use case: someone who wants to (1) clip a recipe from a
URL, (2) know whether they already have the ingredients without
looking, (3) generate a shopping list for next week's plan that
excludes things already in the pantry, (4) record what's been eaten
so leftovers don't get forgotten, and (5) see what's about to expire
in the fridge.

Rules are linked to source via `r[impl <id>]` and `r[verify <id>]`
annotations. Run `cargo xtask tracey-validate` to confirm coverage.

## Recipes

r[cookbook.recipe.identity]
Every `Recipe` has a server-assigned UUIDv4 primary key. `name` is
required and free-text. `cookbook_id` is optional — a recipe can
exist without belonging to any cookbook (a "loose" recipe).
Recipes are referenced by `id` from `MealPlan` and elsewhere; the
ID is stable for the recipe's lifetime.

r[cookbook.recipe.times]
`prep_time_minutes` + `cook_time_minutes` + `total_time_minutes` are
optional `u32`. When both prep and cook are set, the UI defaults
`total = prep + cook` but the user can override (covers chilling,
rising, marination — time not in either bucket). The repo accepts
any combination including only `total`.

r[cookbook.recipe.servings-scale]
`Recipe.servings` is the *base* yield. The UI lets users render at
a scaled yield (`requested / base = factor`) which multiplies every
`RecipeIngredient.quantity` by the factor at display time. The
stored recipe never mutates — scaling is a view-time concern.

r[cookbook.recipe.cuisine-and-tags]
`cuisine` (single string, e.g. "italian", "thai") and `tags`
(`Vec<String>` like `"weeknight"`, `"vegan"`, `"freezer-friendly"`)
are independent free-text classification fields. Both are
filterable and the UI offers autocomplete from existing values.

r[cookbook.recipe.source]
`Recipe.source_url` (optional) records where the recipe came from —
a website, a video, a book ISBN. Used for attribution and so the
user can revisit the source. Not parsed for re-import.

r[cookbook.recipe.import-url]
The `CookbookService.import_from_url(url)` RPC fetches the page,
parses [schema.org Recipe JSON-LD] if present (Mealie / Tandoor
parity), falls back to a structured HTML extractor for known sites,
and creates a `Recipe` + `RecipeIngredient`s + `RecipeSteps` from
the result. On failure returns the partial result + a list of
warnings rather than rejecting outright.

r[cookbook.recipe.image]
`Recipe.image_url` is the primary cover image. Additional photos
attach via the `attachments` feature, same shape as inventory.

## Ingredients

r[cookbook.ingredient.line-shape]
A `RecipeIngredient` represents one line on the ingredients list:
`recipe_id`, `sort_key` (lex ordering), `quantity` (f64), `unit`
(free-text like `g`, `cup`, `tbsp`, empty for "to taste"),
`food_product_id` (Option — linked when the line is recognized as a
known product), `raw_text` (the original "2 cups all-purpose
flour" — preserved verbatim for display + for re-parsing if the
product link changes).

r[cookbook.ingredient.shopping-link]
When `food_product_id` is set, the ingredient contributes to
shopping-list generation by aggregating its `quantity` with other
recipes that need the same product. When it's `None`, the
shopping-list generator falls back to `raw_text` and the user has
to dedupe manually.

r[cookbook.ingredient.parser]
The `CookbookService.parse_ingredient(line)` RPC takes a raw line
("1 1/2 tsp salt") and returns `{quantity, unit, food_product_id,
notes}` by walking a unit table and fuzzy-matching against existing
`FoodProduct`s. Imperfect parses return what they could; the user
edits the rest.

## Steps

r[cookbook.step.ordered-list]
A `RecipeStep` has `recipe_id`, `sort_key`, `body` (markdown). Order
is by `sort_key` (lex). Steps are full markdown, including
sub-step images, links to other recipes (sub-recipes), and
timer-trigger annotations the UI parses as `[15 min]`.

r[cookbook.step.timer-syntax]
A step body containing `[5 min]` (or `[1 hr 30 min]`) signals a
timer suggestion. The cooking-mode UI offers to start a timer for
the parsed duration. The repo treats the body as opaque markdown;
parsing happens at render time.

## Cookbook collections

r[cookbook.cookbook.collection]
A `Cookbook` is a named collection of recipes — "Mom's recipes",
"Weeknight quick wins", "2026 favorites". Cookbooks are just a
grouping mechanism; deleting a cookbook does NOT delete recipes,
only unsets their `cookbook_id`.

## Meal planning

r[cookbook.meal-plan.slot]
A `MealPlan` row is one planned meal: `id`, `date`, `slot` enum
(`breakfast`, `lunch`, `dinner`, `snack`), `recipe_id` (Option —
nullable for "we plan to eat out" or "leftovers"), `servings`
override (Option — defaults to recipe's base servings), `notes`,
`cooked_at` (Option — set when the meal is actually cooked).

r[cookbook.meal-plan.calendar-integration]
Each `MealPlan` row appears on the calendar feature as a calendar
event on `date` in a "Meals" calendar. The integration is
read-only from the calendar side — editing the meal plan goes
through `cookbook` APIs. Two-way sync is out of scope (the
calendar feature surfaces it; cookbook owns the data).

r[cookbook.meal-plan.shopping-list-window]
`CookbookService.generate_shopping_list(start_date, end_date)`
walks every `MealPlan` in the window, expands its recipe's
ingredients (scaled to the plan's servings), aggregates by
`food_product_id` (summing quantities in compatible units),
subtracts current pantry stock, and emits `ShoppingListItem` rows.
Idempotent: re-running for the same window updates existing items
rather than duplicating.

## Pantry — food inventory

r[cookbook.pantry.is-food-inventory]
The pantry is the cookbook feature's analog of the inventory
feature, scoped to consumable food. A `PantryItem` has
`food_product_id` (the SKU), `quantity` + `unit` (how much you have),
`location` (free-text — "freezer", "pantry shelf B", "fridge crisper"),
`expires_at` (optional), `opened_at` (optional, for once-opened
shelf life), `notes`.

r[cookbook.pantry.multiple-locations]
Multiple `PantryItem` rows can reference the same `FoodProduct`
when stock is split across locations — "2 lbs flour in pantry" +
"500g flour in the chest freezer". Aggregation queries sum across
rows; the UI shows the split. This is intentional rather than
forcing one-row-per-product.

r[cookbook.pantry.expiring-soon]
The repo offers a filter "`expires_at < now + Duration::days(N)`"
for the expiring-soon view. Default N=7 in the UI. Items with
`expires_at IS NULL` (shelf-stable) are excluded from this view.

r[cookbook.pantry.consumption-on-cook]
When a `MealPlan` is marked `cooked_at`, the service walks the
recipe's ingredients and **decrements** matching pantry items by
the planned servings ratio. Underflow (cooked more than was in
pantry) is allowed — the pantry just goes to zero, and the
shopping-list generator picks it up next time. The user can opt
out per-meal via a flag.

r[cookbook.pantry.barcode-scan]
The `CookbookService.lookup_barcode(upc)` RPC checks an internal
cache of UPCs → `FoodProduct`s, falling back to OpenFoodFacts on
miss. Creates the `FoodProduct` if it's new and returns it to the
caller, who then creates a `PantryItem` referencing it. The lookup
result is cached locally so subsequent scans are offline.

## Food products

r[cookbook.food-product.shape]
A `FoodProduct` is a reusable SKU referenced by ingredients +
pantry items: `id`, `name` ("All-Purpose Flour"), `brand`
(Option), `default_unit` (the canonical unit — "g" for flour, "ea"
for eggs), `nutrition_per_100` (Option JSON: calories, protein,
fat, carb in standard nutrition fields), `category` (free-text:
"dairy", "produce", "pantry", "frozen", "spice"), `barcode`
(Option UPC string), `notes`.

r[cookbook.food-product.dedup-on-name-brand]
The service surfaces possible duplicates when creating: a `name +
brand` collision (case-insensitive) returns the existing product
rather than creating a new one. Cleanup of pre-existing
duplicates is a separate user action.

## Shopping list

r[cookbook.shopping-list-item.shape]
A `ShoppingListItem` has `id`, `food_product_id` (Option),
`raw_text` (free-text fallback when no product link), `quantity` +
`unit`, `aisle` (free-text — "produce", "dairy" — used for
grouping in the store), `checked_at` (Option — when the user
checked the item off), `added_from` enum (`manual`,
`meal-plan-generator`, `low-stock-rule`, `recurring`).

r[cookbook.shopping-list.grouping]
The UI groups shopping-list items by `aisle`. `aisle` populates
from the product's category by default but the user overrides per
list. Ordering of aisles within a list is configurable (matches
store layout).

r[cookbook.shopping-list.recurring]
The user can mark a `FoodProduct` as "always keep stocked";
`CookbookService.refresh_recurring()` adds shopping-list items for
any such product whose pantry stock is below a configured
threshold. Run on a schedule (daily) or on-demand.

## Nutrition

r[cookbook.recipe.nutrition-rollup]
A recipe's total nutrition is computed by summing
`nutrition_per_100 * (ingredient.quantity_in_g / 100)` across
linked ingredients. Reported as `nutrition_per_serving` (divide by
`servings`). Cached on the recipe with a `recomputed_at` timestamp;
recomputed on ingredient change.

r[cookbook.recipe.nutrition-confidence]
The rollup carries a `confidence` enum (`full`, `partial`,
`unknown`) — `full` means every ingredient has nutrition + a
parseable quantity, `partial` means at least one ingredient is
missing data, `unknown` means most ingredients lack data. The UI
surfaces the confidence next to the numbers.

## Cooking mode

r[cookbook.service.cooking-mode]
`CookbookService.start_cooking(meal_plan_id_or_recipe_id)` opens a
guided session: walks through steps, parses inline timers, locks
the screen awake. Implementation lives in the UI but the service
RPC records `cooked_at` on the `MealPlan` (if applicable) when the
user marks the session done.

## Sync and CRDT semantics

r[cookbook.crdt.recipe-text-fields-loro-text]
`Recipe.summary` and `RecipeStep.body` are stored as `LoroText`
containers — long-form text edited concurrently across peers must
merge at character granularity. Same pattern as
`knowledge.block.content`. Other scalar fields are LWW.

r[cookbook.crdt.pantry-quantity-additive]
`PantryItem.quantity` is a numeric LWW field — concurrent edits
take the most recent. Increments across peers do NOT sum (i.e. two
clients each adding 100g of flour from different peers do NOT
produce 200g — last write wins). The cooking-decrement path is
service-mediated; the UI funnels parallel updates through
optimistic concurrency hints instead of a counter CRDT.

## What this spec does NOT cover (yet)

- **Meal cost tracking**: tying recipe ingredients to
  `food_product.unit_price_cents` and rolling up. Possible later.
- **Diet / restriction matching**: filtering recipes by
  vegetarian / GF / nut-free / etc. Field exists on recipe
  (`tags`) but no automatic ingredient-level inference yet.
- **Multi-tenant cookbooks**: sharing a cookbook with another
  household. Out of scope for v1 — single-tenant only.
- **Recipe variations / forks**: "Mom's recipe but I use butter
  instead of margarine". Tracked as a separate recipe with
  `derived_from_id` in a future version, not v1.
- **Image-based ingredient detection**: photograph the fridge,
  detect contents. Future, separate AI integration.
