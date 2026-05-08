//! Idempotent demo-data seeder.
//!
//! Provides reproducible fixture data across the SeaORM domain entities so the
//! server can boot into a known state and the CLI / UI / real-time tests have
//! a stable target. Every seeded row uses a deterministic UUID derived from
//! [`DEMO_NAMESPACE`] + a stable string key — re-running [`seed_demo_data`]
//! against an already-seeded database is a no-op.
//!
//! The seeder reaches into the SeaORM `ActiveModel`s directly rather than the
//! crudcrate `*Api*` create surface because the API layer auto-generates new
//! UUIDs on create (idempotency-hostile). This is admin-only seed code; the
//! rest of the app continues to go through the generated CRUD surfaces.

use chrono::{Duration, NaiveDate, Utc};
use sea_orm::{
    ActiveModelTrait, ActiveValue::Set, ColumnTrait, DatabaseConnection, DbErr, EntityTrait,
    QueryFilter, QueryOrder,
};
use uuid::Uuid;

use task_core::activity;
use task_core::attachment;
use task_core::calendar_event::{self, CalendarEventStatus};
use task_core::comment;
use task_core::cookbook;
use task_core::cookbook_recipe;
use task_core::cycle::{self, CycleStatus, CycleTaskList};
use task_core::email;
use task_core::email::EmailStringList;
use task_core::expense::{self, ExpenseStatus};
use task_core::food::{self, FoodAliasList};
use task_core::food_log;
use task_core::food_product;
use task_core::integration::{
    self, IntegrationStringList, ProjectTemplate, ProjectTemplateList, StatusDef, StatusDefList,
    TaskTemplate, TaskTemplateList,
};
use task_core::invoice::{self, InvoiceLine, InvoiceLineList, InvoiceStatus, Payment, PaymentList};
use task_core::location;
use task_core::location::{LocationTagList, SpaceList, VenueDefaultList};
use task_core::meal_plan::{self, MealType};
use task_core::notification;
use task_core::pantry;
use task_core::people::{self, ContactMethod, ContactMethodList, ProviderRef, ProviderRefList};
use task_core::project::{self, ProjectStatus};
use task_core::property::JsonObject;
use task_core::reaction;
use task_core::recipe;
use task_core::recipe_ingredient;
use task_core::recipe_step;
use task_core::shopping_list;
use task_core::task::{
    self, EmailRefList, Priority, RecurrenceAnchor, ReminderList, Status, StringList,
    TaskDependencyList, TaskRelationList, TimeEntry, TimeEntryList, WikiLink, WikiLinkList,
};
use task_core::track::{self, TrackStatus};
use task_core::views::{self, ViewDisplay, ViewFilters};

/// Namespace for demo-data UUID derivation. Stable; do not change without
/// running `reset_demo_data` first.
pub const DEMO_NAMESPACE: Uuid = Uuid::from_u128(0x0d_e7_5e_ed_0d_e7_5e_ed_0d_e7_5e_ed_0d_e7_5e_ed);

// Org slugs match the auth-seed organizations in `apps/server/src/main.rs`
// (`seed_auth_data`). Cross-org collaboration is modeled by shared usernames
// across `Project::team` and `Task::assignees` — e.g. cody is a member of all
// five orgs, tom is in fta/fts/jf/tbm, marcus is in fta/tbm. A project
// owned by org A with assignees from org B + B's also-member-of-A users
// is a cross-org collab.
const ORG_FTA: &str = "fasttrackaudio";
const ORG_FTS: &str = "fasttrackstudio";
const ORG_JF: &str = "just-friends";
const ORG_TBM: &str = "tombrooksmusic";
const ORG_PERSONAL: &str = "personal";

/// Per-entity counts for a seed run.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct DemoSeedSummary {
    pub projects_created: usize,
    pub projects_unchanged: usize,
    pub tasks_created: usize,
    pub tasks_unchanged: usize,
    pub calendar_events_created: usize,
    pub calendar_events_unchanged: usize,
    pub people_created: usize,
    pub people_unchanged: usize,
    pub comments_created: usize,
    pub comments_unchanged: usize,
    pub reactions_created: usize,
    pub reactions_unchanged: usize,
    pub notifications_created: usize,
    pub notifications_unchanged: usize,
    pub saved_views_created: usize,
    pub saved_views_unchanged: usize,
    pub cycles_created: usize,
    pub cycles_unchanged: usize,
    pub activities_created: usize,
    pub activities_unchanged: usize,
    pub expenses_created: usize,
    pub expenses_unchanged: usize,
    pub invoices_created: usize,
    pub invoices_unchanged: usize,
    pub email_refs_created: usize,
    pub email_refs_unchanged: usize,
    pub attachments_created: usize,
    pub attachments_unchanged: usize,
    pub integrations_created: usize,
    pub integrations_unchanged: usize,
    pub tracks_created: usize,
    pub tracks_unchanged: usize,
    pub foods_created: usize,
    pub foods_unchanged: usize,
    pub food_products_created: usize,
    pub food_products_unchanged: usize,
    pub recipes_created: usize,
    pub recipes_unchanged: usize,
    pub cookbooks_created: usize,
    pub cookbooks_unchanged: usize,
    pub meal_plan_entries_created: usize,
    pub meal_plan_entries_unchanged: usize,
    pub shopping_lists_created: usize,
    pub shopping_lists_unchanged: usize,
    pub shopping_list_items_created: usize,
    pub shopping_list_items_unchanged: usize,
    pub locations_created: usize,
    pub locations_unchanged: usize,
    pub pantry_items_created: usize,
    pub pantry_items_unchanged: usize,
    pub food_logs_created: usize,
    pub food_logs_unchanged: usize,
}

impl DemoSeedSummary {
    pub fn total_created(&self) -> usize {
        self.projects_created
            + self.tasks_created
            + self.calendar_events_created
            + self.people_created
            + self.comments_created
            + self.reactions_created
            + self.notifications_created
            + self.saved_views_created
            + self.cycles_created
            + self.activities_created
            + self.expenses_created
            + self.invoices_created
            + self.email_refs_created
            + self.attachments_created
            + self.integrations_created
            + self.tracks_created
            + self.foods_created
            + self.food_products_created
            + self.recipes_created
            + self.cookbooks_created
            + self.meal_plan_entries_created
            + self.shopping_lists_created
            + self.shopping_list_items_created
            + self.locations_created
            + self.pantry_items_created
            + self.food_logs_created
    }

    pub fn total_unchanged(&self) -> usize {
        self.projects_unchanged
            + self.tasks_unchanged
            + self.calendar_events_unchanged
            + self.people_unchanged
            + self.comments_unchanged
            + self.reactions_unchanged
            + self.notifications_unchanged
            + self.saved_views_unchanged
            + self.cycles_unchanged
            + self.activities_unchanged
            + self.expenses_unchanged
            + self.invoices_unchanged
            + self.email_refs_unchanged
            + self.attachments_unchanged
            + self.integrations_unchanged
            + self.tracks_unchanged
            + self.foods_unchanged
            + self.food_products_unchanged
            + self.recipes_unchanged
            + self.cookbooks_unchanged
            + self.meal_plan_entries_unchanged
            + self.shopping_lists_unchanged
            + self.shopping_list_items_unchanged
            + self.locations_unchanged
            + self.pantry_items_unchanged
            + self.food_logs_unchanged
    }
}

pub fn demo_id(key: &str) -> Uuid {
    Uuid::new_v5(&DEMO_NAMESPACE, key.as_bytes())
}

/// Idempotently seed every entity flavor that has a server-side `*ServiceImpl`.
pub async fn seed_demo_data(db: &DatabaseConnection) -> Result<DemoSeedSummary, DbErr> {
    let mut summary = DemoSeedSummary::default();
    seed_projects(db, &mut summary).await?;
    seed_tasks(db, &mut summary).await?;
    seed_calendar_events(db, &mut summary).await?;
    seed_people(db, &mut summary).await?;
    seed_comments(db, &mut summary).await?;
    seed_reactions(db, &mut summary).await?;
    seed_notifications(db, &mut summary).await?;
    seed_saved_views(db, &mut summary).await?;
    seed_cycles(db, &mut summary).await?;
    seed_activities(db, &mut summary).await?;
    seed_expenses(db, &mut summary).await?;
    seed_invoices(db, &mut summary).await?;
    seed_email_refs(db, &mut summary).await?;
    seed_attachments(db, &mut summary).await?;
    seed_integrations(db, &mut summary).await?;
    seed_tracks(db, &mut summary).await?;
    // Foods must seed before recipes so RecipeIngredient.food_id
    // auto-link finds the catalog row on insert.
    seed_foods(db, &mut summary).await?;
    seed_cooking(db, &mut summary).await?;
    seed_food_products(db, &mut summary).await?;
    seed_locations(db, &mut summary).await?;
    seed_pantry(db, &mut summary).await?;
    // Backfill `food_id` on any pre-existing recipe_ingredient rows
    // (idempotent: only updates rows whose food_id is currently NULL).
    backfill_recipe_ingredient_food_ids(db).await?;
    recompute_demo_recipe_nutrition(db).await?;
    seed_food_logs(db, &mut summary).await?;
    Ok(summary)
}

/// Delete every row created by [`seed_demo_data`] (by deterministic id).
pub async fn reset_demo_data(db: &DatabaseConnection) -> Result<DemoSeedSummary, DbErr> {
    let mut summary = DemoSeedSummary::default();

    // Pantry first — soft FKs to foods/products/locations.
    for key in PANTRY_KEYS {
        let id = demo_id(key);
        if pantry::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.pantry_items_created += 1;
        }
    }
    for key in LOCATION_KEYS {
        let id = demo_id(key);
        if location::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.locations_created += 1;
        }
    }

    for key in INTEGRATION_KEYS {
        let id = demo_id(key);
        if integration::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.integrations_created += 1;
        }
    }
    for key in ATTACHMENT_KEYS {
        let id = demo_id(key);
        if attachment::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.attachments_created += 1;
        }
    }
    for key in EMAIL_REF_KEYS {
        let id = demo_id(key);
        if email::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.email_refs_created += 1;
        }
    }
    for key in INVOICE_KEYS {
        let id = demo_id(key);
        if invoice::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.invoices_created += 1;
        }
    }
    for key in EXPENSE_KEYS {
        let id = demo_id(key);
        if expense::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.expenses_created += 1;
        }
    }
    for key in ACTIVITY_KEYS {
        let id = demo_id(key);
        if activity::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.activities_created += 1;
        }
    }
    for key in CYCLE_KEYS {
        let id = demo_id(key);
        if cycle::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.cycles_created += 1;
        }
    }
    for key in SAVED_VIEW_KEYS {
        let id = demo_id(key);
        if views::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.saved_views_created += 1;
        }
    }
    for key in NOTIFICATION_KEYS {
        let id = demo_id(key);
        if notification::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.notifications_created += 1;
        }
    }
    for key in REACTION_KEYS {
        let id = demo_id(key);
        if reaction::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.reactions_created += 1;
        }
    }
    for key in COMMENT_KEYS {
        let id = demo_id(key);
        if comment::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.comments_created += 1;
        }
    }
    for key in PEOPLE_KEYS {
        let id = demo_id(key);
        if people::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.people_created += 1;
        }
    }
    for key in CALENDAR_EVENT_KEYS {
        let id = demo_id(key);
        if calendar_event::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.calendar_events_created += 1;
        }
    }
    for key in TASK_KEYS {
        let id = demo_id(key);
        if task::Entity::delete_by_id(id).exec(db).await?.rows_affected > 0 {
            summary.tasks_created += 1;
        }
    }
    for key in TRACK_KEYS {
        let id = demo_id(key);
        if track::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.tracks_created += 1;
        }
    }

    // Food logs delete first — they reference foods/products/recipes.
    for key in FOOD_LOG_KEYS {
        let id = demo_id(key);
        if task_core::food_log::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.food_logs_created += 1;
        }
    }

    // Cooking — child rows first, then parents (no FK enforcement in
    // SQLite default, but order is still the right discipline).
    for key in MEAL_PLAN_KEYS {
        let id = demo_id(key);
        if meal_plan::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.meal_plan_entries_created += 1;
        }
    }
    for key in COOKBOOK_KEYS {
        let cb_id = demo_id(key);
        cookbook_recipe::Entity::delete_many()
            .filter(cookbook_recipe::Column::CookbookId.eq(cb_id))
            .exec(db)
            .await?;
        if cookbook::Entity::delete_by_id(cb_id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.cookbooks_created += 1;
        }
    }
    // Food products before foods (FK ordering even without enforcement).
    for key in FOOD_PRODUCT_KEYS {
        let id = demo_id(key);
        if food_product::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.food_products_created += 1;
        }
    }
    for key in FOOD_KEYS {
        let id = demo_id(key);
        if food::Entity::delete_by_id(id).exec(db).await?.rows_affected > 0 {
            summary.foods_created += 1;
        }
    }
    for key in RECIPE_KEYS {
        let rid = demo_id(key);
        recipe_ingredient::Entity::delete_many()
            .filter(recipe_ingredient::Column::RecipeId.eq(rid))
            .exec(db)
            .await?;
        recipe_step::Entity::delete_many()
            .filter(recipe_step::Column::RecipeId.eq(rid))
            .exec(db)
            .await?;
        if recipe::Entity::delete_by_id(rid)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.recipes_created += 1;
        }
    }
    for key in SHOPPING_LIST_KEYS {
        let lid = demo_id(key);
        let items_removed = shopping_list::ItemEntity::delete_many()
            .filter(shopping_list::ItemColumn::ListId.eq(lid))
            .exec(db)
            .await?
            .rows_affected as usize;
        summary.shopping_list_items_created += items_removed;
        if shopping_list::Entity::delete_by_id(lid)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.shopping_lists_created += 1;
        }
    }
    for key in PROJECT_KEYS {
        let id = demo_id(key);
        if project::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.projects_created += 1;
        }
    }
    Ok(summary)
}

// ── Stable keys ──────────────────────────────────────────────────────────────

const PROJECT_KEYS: &[&str] = &[
    "project:task-app",
    "project:fasttrack-album",
    "project:venue-prep",
    "project:on-hold-r-and-d",
    "project:archived-2024",
    "project:personal-todo",
    // Cross-org collaborations
    "project:tom-solo-ep",
    "project:band-tour-2026",
];

const TASK_KEYS: &[&str] = &[
    // Task App (FTS) — cody/tom/kai/luna
    "task:fix-auth-bug",
    "task:design-portal-ui",
    "task:write-readme",
    "task:write-tests",
    "task:onboard-luna",
    "task:scheduled-future-deploy",
    "task:running-timer-perf-pass",
    "task:weekly-recurring-standup",
    // Montreal Album (FTA) — cody/amy/carter
    "task:billable-mix-mastering",
    "task:due-today-call-client",
    "task:montreal-track-sequencing",
    // Personal (PERSONAL) — cody only
    "task:overdue-tax-filing",
    "task:body-rich-spec",
    // Inbox bucket — unrouted, no project
    "task:inbox-misc-idea",
    "task:inbox-research-loro",
    // Done
    "task:done-publish-blog",
    "task:done-q1-review",
    // Cancelled
    "task:cancelled-old-spike",
    // Tom solo EP (TBM, cross-org) — cody contributing as fta engineer
    "task:tom-ep-tracking-bass",
    "task:tom-ep-mixing-collab",
    "task:tom-ep-master-prep",
    // JF Tour 2026 (cross-org) — bri lead, tom + amy + carter from other orgs
    "task:tour-book-venues",
    "task:tour-stage-plot",
    "task:tour-merch-design",
    // Venue prep (JF) — bri lead
    "task:venue-input-list",
    "task:venue-runner-coordination",
];

const CALENDAR_EVENT_KEYS: &[&str] = &[
    "event:standup-today",
    "event:client-meeting-tomorrow",
    "event:past-retro",
    "event:offsite-allday",
];

const PEOPLE_KEYS: &[&str] = &[
    "person:cody-wright",
    "person:amy-wright",
    "person:tom-brooks",
    "person:carter-whitlock",
];

const COMMENT_KEYS: &[&str] = &[
    "comment:auth-bug-1",
    "comment:auth-bug-2-reply",
    "comment:design-resolved",
];

const REACTION_KEYS: &[&str] = &[
    "reaction:fix-auth-thumbs-cody",
    "reaction:design-portal-fire-amy",
    "reaction:billable-mix-tada-cody",
    "reaction:auth-bug-comment-thumbs-cody",
];

const NOTIFICATION_KEYS: &[&str] = &[
    "notification:assigned-fix-auth",
    "notification:mentioned-design-portal",
    "notification:overdue-tax-filing",
];

const SAVED_VIEW_KEYS: &[&str] = &["view:my-today", "view:inbox-triage"];

const CYCLE_KEYS: &[&str] = &["cycle:sprint-2026-w19"];

const ACTIVITY_KEYS: &[&str] = &[
    "activity:fix-auth-created",
    "activity:fix-auth-status-inprogress",
    "activity:design-portal-created",
    "activity:design-portal-resolved",
    "activity:billable-mix-time-logged",
    "activity:done-publish-blog-completed",
    "activity:done-q1-review-completed",
    "activity:tour-book-venues-priority",
    "activity:venue-input-list-created",
    "activity:running-timer-perf-pass-started",
];

const EXPENSE_KEYS: &[&str] = &[
    "expense:montreal-studio-rental",
    "expense:montreal-mastering-software",
    "expense:tour-van-deposit",
    "expense:tour-printed-merch",
    "expense:misc-coffee-meeting",
];

const INVOICE_KEYS: &[&str] = &["invoice:montreal-mar", "invoice:tom-ep-feb-paid"];

const ATTACHMENT_KEYS: &[&str] = &[
    "attachment:montreal-master-wav",
    "attachment:montreal-stems-zip",
    "attachment:tom-ep-mix-notes-pdf",
    "attachment:tour-stage-plot-png",
];

const EMAIL_REF_KEYS: &[&str] = &[
    "email:fix-auth-stack-trace",
    "email:montreal-client-revisions",
    "email:tour-venue-confirmation",
];

const TRACK_KEYS: &[&str] = &[
    // Montreal Album (project:fasttrack-album) — 8 tracks
    "track:montreal-headlights",
    "track:montreal-saint-laurent",
    "track:montreal-tundra",
    "track:montreal-plateau",
    "track:montreal-rue-de-bleury",
    "track:montreal-mile-end",
    "track:montreal-notre-dame",
    "track:montreal-outremont",
    // Tom Solo EP (project:tom-solo-ep) — 4 tracks
    "track:tom-drive-south",
    "track:tom-slow-burn",
    "track:tom-sundown",
    "track:tom-half-light",
];

const INTEGRATION_KEYS: &[&str] = &[
    "integration:audio-production",
    "integration:video-production",
    "integration:cooking",
    "integration:fitness",
];

const RECIPE_KEYS: &[&str] = &[
    "recipe:weeknight-carbonara",
    "recipe:sheet-pan-chicken-veg",
    "recipe:chickpea-curry",
    "recipe:smashburger",
    "recipe:greek-salad",
    "recipe:banana-pancakes",
];

const COOKBOOK_KEYS: &[&str] = &["cookbook:codys-weeknight-rotation"];

const MEAL_PLAN_KEYS: &[&str] = &[
    "meal:day0-lunch",
    "meal:day0-dinner",
    "meal:day1-breakfast",
    "meal:day1-dinner",
    "meal:day2-dinner",
    "meal:day3-dinner",
    "meal:day4-dinner",
    "meal:day5-lunch",
    "meal:day6-dinner",
];

const SHOPPING_LIST_KEYS: &[&str] = &["shop:this-week"];

const FOOD_LOG_KEYS: &[&str] = &[
    "food_log:day0-breakfast-eggs",
    "food_log:day0-lunch-greek-salad",
    "food_log:day0-dinner-carbonara",
    "food_log:day1-breakfast-banana",
    "food_log:day1-lunch-sheetpan",
    "food_log:day2-dinner-chickpea",
    "food_log:day3-breakfast-oatmeal",
    "food_log:day4-dinner-smashburger",
    "food_log:day5-lunch-leftovers",
    "food_log:day6-dinner-takeout",
    "food_log:day0-snack-yogurt",
    "food_log:day1-snack-apple",
    "food_log:day2-breakfast-eggs",
    "food_log:day3-dinner-pasta",
];

const LOCATION_KEYS: &[&str] = &[
    "location:pantry-shelf",
    "location:refrigerator",
    "location:freezer",
];

const PANTRY_KEYS: &[&str] = &[
    "pantry:olive-oil-shelf",
    "pantry:kosher-salt-shelf",
    "pantry:flour-shelf",
    "pantry:sugar-shelf",
    "pantry:garlic-powder-shelf",
    "pantry:paprika-shelf",
    "pantry:soy-sauce-shelf",
    "pantry:eggs-fridge",
    "pantry:butter-fridge",
    "pantry:feta-fridge",
    "pantry:milk-fridge",
    "pantry:chicken-thighs-freezer",
    "pantry:ground-beef-freezer",
];

/// Cody's pantry-relevant catalog. Mix of pantry staples, produce,
/// dairy, protein, and spices. Each Food carries category +
/// default_unit + (when well-known) per-100g nutrition.
const FOOD_KEYS: &[&str] = &[
    // Pantry staples
    "food:olive-oil",
    "food:kosher-salt",
    "food:black-pepper",
    "food:all-purpose-flour",
    "food:white-sugar",
    "food:brown-sugar",
    "food:baking-powder",
    "food:baking-soda",
    "food:garlic-powder",
    "food:onion-powder",
    "food:smoked-paprika",
    "food:ground-cumin",
    "food:dried-oregano",
    "food:soy-sauce",
    "food:white-rice",
    "food:dried-pasta",
    "food:canned-tomatoes",
    "food:canned-coconut-milk",
    "food:spaghetti",
    "food:american-cheese",
    // Produce
    "food:yellow-onion",
    "food:red-onion",
    "food:garlic",
    "food:fresh-ginger",
    "food:lemon",
    "food:lime",
    "food:tomato",
    "food:cucumber",
    "food:kalamata-olives",
    "food:broccoli",
    "food:sweet-potato",
    "food:banana",
    // Dairy/Eggs
    "food:butter",
    "food:eggs",
    "food:whole-milk",
    "food:feta-cheese",
    "food:pecorino-romano",
    "food:brioche-bun",
    // Protein
    "food:chicken-thigh",
    "food:ground-beef",
    "food:guanciale",
    "food:chickpeas-canned",
    // Spices/Misc
    "food:ground-cinnamon",
    "food:garam-masala",
    "food:dried-thyme",
    "food:dried-rosemary",
];

const FOOD_PRODUCT_KEYS: &[&str] = &[
    "food_product:tj-evoo-500ml",
    "food_product:vital-farms-eggs-dozen",
    "food_product:generic-chickpeas-15oz",
    "food_product:bertolli-evoo-500ml",
    "food_product:demo-dozen-eggs",
];

// ── Project fixtures ────────────────────────────────────────────────────────

async fn seed_projects(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let today = Utc::now().date_naive();

    let projects = [
        // FastTrackStudio (software org) — Task App, led by cody, Tom is a
        // contributor though his primary org is FTA. tom + cody overlap into
        // both fta/fts so this is a clean intra-org collaboration.
        {
            let mut p = project_base("project:task-app", "Task App", ProjectStatus::Active);
            p.description = Set(Some(
                "The task management system this codebase is.".to_string(),
            ));
            p.area = Set(Some("Engineering".to_string()));
            p.organization = Set(Some(ORG_FTS.to_string()));
            p.project_type = Set(Some("Product".to_string()));
            p.workflow = Set(Some("kanban".to_string()));
            p.workflow_stage = Set(Some("In Progress".to_string()));
            p.identifier = Set(Some("TASK".to_string()));
            p.lead = Set(Some("cody".to_string()));
            p.default_assignee = Set(Some("cody".to_string()));
            p.emoji = Set(Some("🛠".to_string()));
            p.start = Set(Some(today - Duration::days(120)));
            p.due = Set(Some(today + Duration::days(60)));
            p.tags = Set(StringList::from(vec![
                "internal".to_string(),
                "product".to_string(),
            ]));
            p.team = Set(StringList::from(vec![
                "cody".to_string(),
                "tom".to_string(),
                "kai".to_string(),
                "luna".to_string(),
            ]));
            p
        },
        // FastTrackAudio (music) — Montreal Album, billable. cody/amy/carter.
        {
            let mut p = project_base(
                "project:fasttrack-album",
                "Montreal Album",
                ProjectStatus::Active,
            );
            p.description = Set(Some(
                "Audio production: mastering + sequencing.".to_string(),
            ));
            p.area = Set(Some("Music".to_string()));
            p.organization = Set(Some(ORG_FTA.to_string()));
            p.project_type = Set(Some("Audio".to_string()));
            p.lead = Set(Some("cody".to_string()));
            p.default_assignee = Set(Some("cody".to_string()));
            p.emoji = Set(Some("🎧".to_string()));
            p.start = Set(Some(today - Duration::days(45)));
            p.due = Set(Some(today + Duration::days(30)));
            p.tags = Set(StringList::from(vec![
                "billable".to_string(),
                "client-work".to_string(),
            ]));
            p.team = Set(StringList::from(vec![
                "cody".to_string(),
                "amy".to_string(),
                "carter".to_string(),
            ]));
            p
        },
        // Just Friends (band) — venue prep. Cross-org by nature: cody/amy/
        // carter/tom/bri are all in jf, but tom's primary is fta + tbm.
        {
            let mut p = project_base(
                "project:venue-prep",
                "Campus Jax Show Prep",
                ProjectStatus::Active,
            );
            p.area = Set(Some("Operations".to_string()));
            p.organization = Set(Some(ORG_JF.to_string()));
            p.project_type = Set(Some("Event".to_string()));
            p.lead = Set(Some("bri".to_string()));
            p.default_assignee = Set(Some("bri".to_string()));
            p.emoji = Set(Some("🎤".to_string()));
            p.start = Set(Some(today));
            p.due = Set(Some(today + Duration::days(14)));
            p.team = Set(StringList::from(vec![
                "cody".to_string(),
                "amy".to_string(),
                "carter".to_string(),
                "tom".to_string(),
                "bri".to_string(),
            ]));
            p
        },
        // FTS — research spike, on hold.
        {
            let mut p = project_base(
                "project:on-hold-r-and-d",
                "CRDT Research Spike",
                ProjectStatus::OnHold,
            );
            p.area = Set(Some("Engineering".to_string()));
            p.organization = Set(Some(ORG_FTS.to_string()));
            p.lead = Set(Some("cody".to_string()));
            p.description = Set(Some("Loro CRDT integration exploration.".to_string()));
            p.team = Set(StringList::from(vec![
                "cody".to_string(),
                "kai".to_string(),
            ]));
            p
        },
        // FTS — archived retro from last quarter.
        {
            let mut p = project_base(
                "project:archived-2024",
                "2024 Q4 Retrospective",
                ProjectStatus::Archived,
            );
            p.area = Set(Some("Operations".to_string()));
            p.organization = Set(Some(ORG_FTS.to_string()));
            p
        },
        // Personal — single-member.
        {
            let mut p = project_base(
                "project:personal-todo",
                "Personal Todos",
                ProjectStatus::Active,
            );
            p.area = Set(Some("Personal".to_string()));
            p.organization = Set(Some(ORG_PERSONAL.to_string()));
            p.lead = Set(Some("cody".to_string()));
            p.emoji = Set(Some("📝".to_string()));
            p.team = Set(StringList::from(vec!["cody".to_string()]));
            p
        },
        // ── Cross-org collaboration #1 ─────────────────────────────────────
        // TomBrooksMusic owns this solo EP; cody (primary: fta) and marcus
        // (primary: fta) collaborate as engineers. Both are members of tbm
        // through the auth org-membership table, so this is genuine cross-org
        // work — fta engineers contracted into tbm's release.
        {
            let mut p = project_base(
                "project:tom-solo-ep",
                "Tom Brooks: Solo EP",
                ProjectStatus::Active,
            );
            p.area = Set(Some("Music".to_string()));
            p.organization = Set(Some(ORG_TBM.to_string()));
            p.project_type = Set(Some("Audio".to_string()));
            p.lead = Set(Some("tom".to_string()));
            p.default_assignee = Set(Some("tom".to_string()));
            p.emoji = Set(Some("🎸".to_string()));
            p.start = Set(Some(today - Duration::days(20)));
            p.due = Set(Some(today + Duration::days(45)));
            p.tags = Set(StringList::from(vec![
                "billable".to_string(),
                "cross-org".to_string(),
            ]));
            p.team = Set(StringList::from(vec![
                "tom".to_string(),
                "cody".to_string(),
                "marcus".to_string(),
            ]));
            p
        },
        // ── Cross-org collaboration #2 ─────────────────────────────────────
        // Just Friends planning a 2026 tour — coordinates across fta (cody/amy/
        // carter handling stage/sound) and tbm (tom doing setlist/logistics).
        // Bri (jf-only, tour manager) leads.
        {
            let mut p = project_base(
                "project:band-tour-2026",
                "Just Friends 2026 Tour",
                ProjectStatus::Active,
            );
            p.area = Set(Some("Operations".to_string()));
            p.organization = Set(Some(ORG_JF.to_string()));
            p.project_type = Set(Some("Event".to_string()));
            p.lead = Set(Some("bri".to_string()));
            p.default_assignee = Set(Some("bri".to_string()));
            p.emoji = Set(Some("🚐".to_string()));
            p.start = Set(Some(today + Duration::days(30)));
            p.due = Set(Some(today + Duration::days(180)));
            p.tags = Set(StringList::from(vec![
                "tour".to_string(),
                "cross-org".to_string(),
            ]));
            p.team = Set(StringList::from(vec![
                "bri".to_string(),
                "cody".to_string(),
                "amy".to_string(),
                "carter".to_string(),
                "tom".to_string(),
            ]));
            p
        },
    ];

    for p in projects {
        let id = match &p.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if project::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.projects_unchanged += 1;
        } else {
            project::Entity::insert(p).exec(db).await?;
            summary.projects_created += 1;
        }
    }
    Ok(())
}

fn project_base(key: &str, title: &str, status: ProjectStatus) -> project::ActiveModel {
    project::ActiveModel {
        id: Set(demo_id(key)),
        title: Set(title.to_string()),
        status: Set(status),
        up: Set(WikiLinkList::default()),
        tags: Set(StringList::default()),
        team: Set(StringList::default()),
        references: Set(WikiLinkList::default()),
        favorited_by: Set(StringList::default()),
        email_tags: Set(StringList::default()),
        emails: Set(EmailRefList::default()),
        ..Default::default()
    }
}

// ── Task fixtures ────────────────────────────────────────────────────────────

async fn seed_tasks(db: &DatabaseConnection, summary: &mut DemoSeedSummary) -> Result<(), DbErr> {
    let now = Utc::now();
    let today = now.date_naive();

    let tasks = [
        // Active, varied
        task_active(
            "task:fix-auth-bug",
            "Fix login session timeout",
            Priority::High,
            Status::InProgress,
            Some("Task App"),
            vec!["backend", "bug"],
            Some(today + Duration::days(2)),
            Some("cody"),
            now,
        ),
        task_active(
            "task:design-portal-ui",
            "Design portal landing page",
            Priority::Normal,
            Status::Open,
            Some("Task App"),
            vec!["design"],
            Some(today + Duration::days(5)),
            Some("amy"),
            now,
        ),
        task_active(
            "task:write-readme",
            "Write README for new repo layout",
            Priority::Low,
            Status::Open,
            Some("Task App"),
            vec!["docs"],
            None,
            Some("cody"),
            now,
        ),
        task_active(
            "task:write-tests",
            "Add integration tests for realtime",
            Priority::High,
            Status::InProgress,
            Some("Task App"),
            vec!["testing"],
            Some(today + Duration::days(3)),
            Some("cody"),
            now,
        ),
        task_active(
            "task:onboard-luna",
            "Onboard Luna (FTS new hire)",
            Priority::Normal,
            Status::Open,
            Some("Task App"),
            vec!["ops"],
            Some(today + Duration::days(7)),
            Some("tom"),
            now,
        ),
        // Overdue
        task_active(
            "task:overdue-tax-filing",
            "File quarterly taxes",
            Priority::High,
            Status::Open,
            Some("Personal Todos"),
            vec!["finance"],
            Some(today - Duration::days(3)),
            Some("cody"),
            now,
        ),
        // Due today
        task_active(
            "task:due-today-call-client",
            "Call client about Montreal Album scope",
            Priority::High,
            Status::Open,
            Some("Montreal Album"),
            vec!["billable"],
            Some(today),
            Some("cody"),
            now,
        ),
        // Scheduled future
        {
            let mut t = task_active(
                "task:scheduled-future-deploy",
                "Deploy v0.3.0",
                Priority::Normal,
                Status::Open,
                Some("Task App"),
                vec!["release"],
                None,
                Some("cody"),
                now,
            );
            t.scheduled = Set(Some(today + Duration::days(10)));
            t
        },
        // Running timer
        {
            let mut t = task_active(
                "task:running-timer-perf-pass",
                "Profile slow query path",
                Priority::Normal,
                Status::InProgress,
                Some("Task App"),
                vec!["perf"],
                None,
                Some("cody"),
                now,
            );
            t.time_entries = Set(TimeEntryList::from(vec![TimeEntry {
                id: "te-perf-pass-1".to_string(),
                user: Some("cody".to_string()),
                start_time: now - Duration::minutes(45),
                end_time: None,
                ..Default::default()
            }]));
            t
        },
        // Billable mix completed entry
        {
            let mut t = task_active(
                "task:billable-mix-mastering",
                "Master track 3",
                Priority::Normal,
                Status::InProgress,
                Some("Montreal Album"),
                vec!["billable", "audio"],
                Some(today + Duration::days(4)),
                Some("cody"),
                now,
            );
            t.time_entries = Set(TimeEntryList::from(vec![TimeEntry {
                id: "te-mastering-1".to_string(),
                user: Some("cody".to_string()),
                start_time: now - Duration::hours(3),
                end_time: Some(now - Duration::hours(1)),
                billable: true,
                billable_rate: Some(15000),
                description: Some("Mastering pass + reference compare".to_string()),
                ..Default::default()
            }]));
            t
        },
        // Recurring
        {
            let mut t = task_active(
                "task:weekly-recurring-standup",
                "Weekly team standup notes",
                Priority::Low,
                Status::Open,
                Some("Task App"),
                vec!["recurring"],
                None,
                Some("cody"),
                now,
            );
            t.recurrence = Set(Some("FREQ=WEEKLY;BYDAY=MO".to_string()));
            t.recurrence_anchor = Set(RecurrenceAnchor::Scheduled);
            t
        },
        // Inbox
        {
            let mut t = task_active(
                "task:inbox-misc-idea",
                "Read article about effect-ts",
                Priority::Low,
                Status::Open,
                None,
                vec!["inbox"],
                None,
                None,
                now,
            );
            t.issue_type = Set(Some("inbox".to_string()));
            t
        },
        {
            let mut t = task_active(
                "task:inbox-research-loro",
                "Research Loro snapshot serialization",
                Priority::Normal,
                Status::Open,
                None,
                vec!["inbox", "research"],
                None,
                None,
                now,
            );
            t.issue_type = Set(Some("inbox".to_string()));
            t
        },
        // Done
        {
            let mut t = task_active(
                "task:done-publish-blog",
                "Publish blog post on SQLite migration",
                Priority::Normal,
                Status::Done,
                Some("Task App"),
                vec!["docs"],
                None,
                Some("cody"),
                now,
            );
            t.completed_date = Set(Some(today - Duration::days(2)));
            t
        },
        {
            let mut t = task_active(
                "task:done-q1-review",
                "Q1 review writeup",
                Priority::Normal,
                Status::Done,
                Some("Personal Todos"),
                vec![],
                None,
                Some("cody"),
                now,
            );
            t.completed_date = Set(Some(today - Duration::days(14)));
            t
        },
        // Cancelled
        task_with_status(
            "task:cancelled-old-spike",
            "Spike abandoned auth proposal",
            Status::Cancelled,
            Some("CRDT Research Spike"),
            now,
        ),
        // Rich body — kept on personal projects so it doesn't clutter the
        // Task App project tasks list.
        {
            let mut t = task_active(
                "task:body-rich-spec",
                "Write spec: realtime sync v2",
                Priority::High,
                Status::InProgress,
                Some("Personal Todos"),
                vec!["spec"],
                Some(today + Duration::days(7)),
                Some("cody"),
                now,
            );
            t.body = Set(
                "## Goals\n\n- Authoritative server\n- Optimistic clients\n- Conflict resolution\n\n## Open questions\n\n- Snapshot vs delta?\n".to_string(),
            );
            t
        },
        // ── Montreal Album: more depth ─────────────────────────────────────
        task_active(
            "task:montreal-track-sequencing",
            "Sequence album tracklist",
            Priority::Normal,
            Status::Open,
            Some("Montreal Album"),
            vec!["billable", "audio"],
            Some(today + Duration::days(10)),
            Some("amy"),
            now,
        ),
        // ── Tom Solo EP (TBM, cross-org) ───────────────────────────────────
        // tom = lead, cody (fta) tracks bass, marcus (fta) co-mixes.
        task_active(
            "task:tom-ep-tracking-bass",
            "Track bass for Tom's EP",
            Priority::Normal,
            Status::InProgress,
            Some("Tom Brooks: Solo EP"),
            vec!["billable", "cross-org", "audio"],
            Some(today + Duration::days(7)),
            Some("cody"),
            now,
        ),
        {
            let mut t = task_active(
                "task:tom-ep-mixing-collab",
                "Mix passes — split with Marcus",
                Priority::Normal,
                Status::Open,
                Some("Tom Brooks: Solo EP"),
                vec!["billable", "cross-org", "audio"],
                Some(today + Duration::days(20)),
                Some("marcus"),
                now,
            );
            t.assignees = Set(StringList::from(vec![
                "marcus".to_string(),
                "cody".to_string(),
            ]));
            t.subscribers = Set(StringList::from(vec!["tom".to_string()]));
            t
        },
        task_active(
            "task:tom-ep-master-prep",
            "Prepare reference mixes for mastering",
            Priority::Low,
            Status::Open,
            Some("Tom Brooks: Solo EP"),
            vec!["cross-org", "audio"],
            Some(today + Duration::days(35)),
            Some("tom"),
            now,
        ),
        // ── JF 2026 Tour (cross-org) ───────────────────────────────────────
        task_active(
            "task:tour-book-venues",
            "Book 12-city tour venues",
            Priority::High,
            Status::InProgress,
            Some("Just Friends 2026 Tour"),
            vec!["tour", "ops"],
            Some(today + Duration::days(21)),
            Some("bri"),
            now,
        ),
        {
            let mut t = task_active(
                "task:tour-stage-plot",
                "Draft stage plot + input list",
                Priority::Normal,
                Status::Open,
                Some("Just Friends 2026 Tour"),
                vec!["tour", "audio"],
                Some(today + Duration::days(40)),
                Some("carter"),
                now,
            );
            // Carter (jf+fta) is lead, but cody (fta) reviews; subscribe him.
            t.subscribers = Set(StringList::from(vec!["cody".to_string()]));
            t
        },
        task_active(
            "task:tour-merch-design",
            "Design tour merch (shirts, posters)",
            Priority::Low,
            Status::Open,
            Some("Just Friends 2026 Tour"),
            vec!["tour", "design"],
            Some(today + Duration::days(60)),
            Some("amy"),
            now,
        ),
        // ── Venue prep (JF) ────────────────────────────────────────────────
        task_active(
            "task:venue-input-list",
            "Confirm Campus Jax input list",
            Priority::High,
            Status::InProgress,
            Some("Campus Jax Show Prep"),
            vec!["audio"],
            Some(today + Duration::days(3)),
            Some("carter"),
            now,
        ),
        task_active(
            "task:venue-runner-coordination",
            "Coordinate day-of runners + load-in",
            Priority::Normal,
            Status::Open,
            Some("Campus Jax Show Prep"),
            vec!["ops"],
            Some(today + Duration::days(7)),
            Some("bri"),
            now,
        ),
    ];

    for t in tasks {
        let id = match &t.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if task::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.tasks_unchanged += 1;
        } else {
            task::Entity::insert(t).exec(db).await?;
            summary.tasks_created += 1;
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn task_active(
    key: &str,
    title: &str,
    priority: Priority,
    status: Status,
    project: Option<&str>,
    tags: Vec<&str>,
    due: Option<NaiveDate>,
    assignee: Option<&str>,
    now: chrono::DateTime<Utc>,
) -> task::ActiveModel {
    task::ActiveModel {
        id: Set(demo_id(key)),
        title: Set(title.to_string()),
        priority: Set(priority),
        status: Set(status),
        projects: Set(project
            .map(|p| WikiLinkList::from(vec![WikiLink(p.to_string())]))
            .unwrap_or_default()),
        contexts: Set(StringList::default()),
        tags: Set(StringList::from(
            tags.into_iter().map(|t| t.to_string()).collect::<Vec<_>>(),
        )),
        areas: Set(WikiLinkList::default()),
        due: Set(due),
        scheduled: Set(None),
        date_created: Set(Some(now)),
        date_modified: Set(Some(now)),
        time_entries: Set(TimeEntryList::default()),
        completed_instances: Set(StringList::default()),
        skipped_instances: Set(StringList::default()),
        blocked_by: Set(TaskDependencyList::default()),
        blocking: Set(WikiLinkList::default()),
        reminders: Set(ReminderList::default()),
        assignee: Set(assignee.map(str::to_string)),
        assignees: Set(StringList::default()),
        created_by: Set(Some("cody".to_string())),
        relations: Set(TaskRelationList::default()),
        subscribers: Set(StringList::default()),
        reactions: Set(Default::default()),
        is_draft: Set(false),
        email_tags: Set(StringList::default()),
        emails: Set(EmailRefList::default()),
        recurrence_anchor: Set(RecurrenceAnchor::Scheduled),
        body: Set(String::new()),
        ..Default::default()
    }
}

fn task_with_status(
    key: &str,
    title: &str,
    status: Status,
    project: Option<&str>,
    now: chrono::DateTime<Utc>,
) -> task::ActiveModel {
    task_active(
        key,
        title,
        Priority::Normal,
        status,
        project,
        vec![],
        None,
        None,
        now,
    )
}

// ── Calendar event fixtures ─────────────────────────────────────────────────

async fn seed_calendar_events(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let events = [
        calendar_event::ActiveModel {
            uuid: Set(demo_id("event:standup-today")),
            id: Set(Some("standup-today".to_string())),
            title: Set("Daily standup".to_string()),
            start: Set(now.date_naive().and_hms_opt(9, 30, 0).unwrap().and_utc()),
            end: Set(Some(
                now.date_naive().and_hms_opt(9, 45, 0).unwrap().and_utc(),
            )),
            all_day: Set(false),
            status: Set(CalendarEventStatus::Confirmed),
            attendees: Set(StringList::from(vec![
                "cody".to_string(),
                "amy".to_string(),
            ])),
            spaces: Set(WikiLinkList::default()),
            date_created: Set(Some(now)),
            date_modified: Set(Some(now)),
            body: Set(String::new()),
            ..Default::default()
        },
        calendar_event::ActiveModel {
            uuid: Set(demo_id("event:client-meeting-tomorrow")),
            id: Set(Some("client-mtg".to_string())),
            title: Set("Client check-in: Montreal Album".to_string()),
            start: Set((now + Duration::days(1))
                .date_naive()
                .and_hms_opt(15, 0, 0)
                .unwrap()
                .and_utc()),
            end: Set(Some(
                (now + Duration::days(1))
                    .date_naive()
                    .and_hms_opt(16, 0, 0)
                    .unwrap()
                    .and_utc(),
            )),
            all_day: Set(false),
            status: Set(CalendarEventStatus::Confirmed),
            spaces: Set(WikiLinkList::default()),
            attendees: Set(StringList::default()),
            date_created: Set(Some(now)),
            date_modified: Set(Some(now)),
            body: Set(String::new()),
            ..Default::default()
        },
        calendar_event::ActiveModel {
            uuid: Set(demo_id("event:past-retro")),
            id: Set(Some("retro-past".to_string())),
            title: Set("Sprint retro".to_string()),
            start: Set((now - Duration::days(7))
                .date_naive()
                .and_hms_opt(14, 0, 0)
                .unwrap()
                .and_utc()),
            end: Set(Some(
                (now - Duration::days(7))
                    .date_naive()
                    .and_hms_opt(15, 0, 0)
                    .unwrap()
                    .and_utc(),
            )),
            all_day: Set(false),
            status: Set(CalendarEventStatus::Confirmed),
            spaces: Set(WikiLinkList::default()),
            attendees: Set(StringList::default()),
            date_created: Set(Some(now)),
            date_modified: Set(Some(now)),
            body: Set(String::new()),
            ..Default::default()
        },
        calendar_event::ActiveModel {
            uuid: Set(demo_id("event:offsite-allday")),
            id: Set(Some("offsite".to_string())),
            title: Set("Team offsite".to_string()),
            start: Set((now + Duration::days(20))
                .date_naive()
                .and_hms_opt(0, 0, 0)
                .unwrap()
                .and_utc()),
            end: Set(Some(
                (now + Duration::days(21))
                    .date_naive()
                    .and_hms_opt(0, 0, 0)
                    .unwrap()
                    .and_utc(),
            )),
            all_day: Set(true),
            status: Set(CalendarEventStatus::Tentative),
            spaces: Set(WikiLinkList::default()),
            attendees: Set(StringList::default()),
            date_created: Set(Some(now)),
            date_modified: Set(Some(now)),
            body: Set(String::new()),
            ..Default::default()
        },
    ];

    for e in events {
        let id = match &e.uuid {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if calendar_event::Entity::find_by_id(id)
            .one(db)
            .await?
            .is_some()
        {
            summary.calendar_events_unchanged += 1;
        } else {
            calendar_event::Entity::insert(e).exec(db).await?;
            summary.calendar_events_created += 1;
        }
    }
    Ok(())
}

// ── People fixtures ─────────────────────────────────────────────────────────

async fn seed_people(db: &DatabaseConnection, summary: &mut DemoSeedSummary) -> Result<(), DbErr> {
    let now = Utc::now();
    let people = [
        people::ActiveModel {
            uuid: Set(demo_id("person:cody-wright")),
            id: Set(Some("cody".to_string())),
            display_name: Set("Cody Wright".to_string()),
            given_name: Set(Some("Cody".to_string())),
            family_name: Set(Some("Wright".to_string())),
            organization: Set(Some("FastTrack Studios".to_string())),
            title: Set(Some("Engineer".to_string())),
            contact_methods: Set(ContactMethodList::from(vec![
                ContactMethod {
                    kind: "email".to_string(),
                    value: "cody@fasttrackaudio.com".to_string(),
                    label: Some("work".to_string()),
                    primary: true,
                },
                ContactMethod {
                    kind: "phone".to_string(),
                    value: "+1-555-0100".to_string(),
                    label: Some("mobile".to_string()),
                    primary: false,
                },
            ])),
            provider_refs: Set(ProviderRefList::default()),
            notes: Set(None),
            ..Default::default()
        },
        people::ActiveModel {
            uuid: Set(demo_id("person:amy-wright")),
            id: Set(Some("amy".to_string())),
            display_name: Set("Amy Wright".to_string()),
            given_name: Set(Some("Amy".to_string())),
            family_name: Set(Some("Wright".to_string())),
            organization: Set(Some("FastTrack Studios".to_string())),
            contact_methods: Set(ContactMethodList::from(vec![ContactMethod {
                kind: "email".to_string(),
                value: "amy@fasttrackaudio.com".to_string(),
                label: None,
                primary: true,
            }])),
            provider_refs: Set(ProviderRefList::default()),
            notes: Set(None),
            ..Default::default()
        },
        people::ActiveModel {
            uuid: Set(demo_id("person:tom-brooks")),
            id: Set(Some("tombrooks".to_string())),
            display_name: Set("Tom Brooks".to_string()),
            given_name: Set(Some("Tom".to_string())),
            family_name: Set(Some("Brooks".to_string())),
            contact_methods: Set(ContactMethodList::default()),
            provider_refs: Set(ProviderRefList::from(vec![ProviderRef {
                provider: "nextcloud".to_string(),
                account: Some("starcommand".to_string()),
                collection: Some("addressbook-default".to_string()),
                href: None,
                etag: None,
                uid: Some("tombrooks-uid".to_string()),
            }])),
            notes: Set(Some("Sound engineer, Campus Jax shows.".to_string())),
            ..Default::default()
        },
        people::ActiveModel {
            uuid: Set(demo_id("person:carter-whitlock")),
            id: Set(Some("carterwhitlock".to_string())),
            display_name: Set("Carter Whitlock".to_string()),
            contact_methods: Set(ContactMethodList::default()),
            provider_refs: Set(ProviderRefList::default()),
            notes: Set(None),
            ..Default::default()
        },
    ];

    for p in people {
        let id = match &p.uuid {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if people::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.people_unchanged += 1;
        } else {
            people::Entity::insert(p).exec(db).await?;
            summary.people_created += 1;
        }
    }
    let _ = now;
    Ok(())
}

// ── Comment fixtures ────────────────────────────────────────────────────────

async fn seed_comments(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let auth_bug_id = demo_id("task:fix-auth-bug");
    let design_id = demo_id("task:design-portal-ui");

    let comments = [
        comment::ActiveModel {
            id: Set(demo_id("comment:auth-bug-1")),
            entity_id: Set(auth_bug_id),
            entity_type: Set("task".to_string()),
            author: Set("cody".to_string()),
            body: Set("Repro'd: session cookie expires after 1h instead of 24h.".to_string()),
            time_start: Set(None),
            time_end: Set(None),
            reply_to: Set(None),
            resolved: Set(false),
            resolved_by: Set(None),
            mentions: Set(serde_json::json!([]).into()),
            properties: Set(Default::default()),
            external_id: Set(None),
            deleted_at: Set(None),
            created_at: Set(now - Duration::hours(6)),
            updated_at: Set(now - Duration::hours(6)),
        },
        comment::ActiveModel {
            id: Set(demo_id("comment:auth-bug-2-reply")),
            entity_id: Set(auth_bug_id),
            entity_type: Set("task".to_string()),
            author: Set("amy".to_string()),
            body: Set("Likely the JWT exp claim — taking a look now.".to_string()),
            time_start: Set(None),
            time_end: Set(None),
            reply_to: Set(Some(demo_id("comment:auth-bug-1"))),
            resolved: Set(false),
            resolved_by: Set(None),
            mentions: Set(serde_json::json!([]).into()),
            properties: Set(Default::default()),
            external_id: Set(None),
            deleted_at: Set(None),
            created_at: Set(now - Duration::hours(5)),
            updated_at: Set(now - Duration::hours(5)),
        },
        comment::ActiveModel {
            id: Set(demo_id("comment:design-resolved")),
            entity_id: Set(design_id),
            entity_type: Set("task".to_string()),
            author: Set("amy".to_string()),
            body: Set("Mocks attached, signed off.".to_string()),
            time_start: Set(None),
            time_end: Set(None),
            reply_to: Set(None),
            resolved: Set(true),
            resolved_by: Set(Some("cody".to_string())),
            mentions: Set(serde_json::json!([]).into()),
            properties: Set(Default::default()),
            external_id: Set(None),
            deleted_at: Set(None),
            created_at: Set(now - Duration::days(1)),
            updated_at: Set(now - Duration::days(1)),
        },
    ];

    for c in comments {
        let id = match &c.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if comment::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.comments_unchanged += 1;
        } else {
            comment::Entity::insert(c).exec(db).await?;
            summary.comments_created += 1;
        }
    }
    Ok(())
}

// ── Reaction fixtures ───────────────────────────────────────────────────────

async fn seed_reactions(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let auth_bug = demo_id("task:fix-auth-bug");
    let design = demo_id("task:design-portal-ui");
    let billable = demo_id("task:billable-mix-mastering");
    let auth_comment = demo_id("comment:auth-bug-1");

    let reactions = [
        reaction::ActiveModel {
            id: Set(demo_id("reaction:fix-auth-thumbs-cody")),
            entity_id: Set(auth_bug),
            entity_type: Set("task".to_string()),
            emoji: Set("👍".to_string()),
            user: Set("cody".to_string()),
            created_at: Set(now - Duration::hours(2)),
        },
        reaction::ActiveModel {
            id: Set(demo_id("reaction:design-portal-fire-amy")),
            entity_id: Set(design),
            entity_type: Set("task".to_string()),
            emoji: Set("🔥".to_string()),
            user: Set("amy".to_string()),
            created_at: Set(now - Duration::hours(3)),
        },
        reaction::ActiveModel {
            id: Set(demo_id("reaction:billable-mix-tada-cody")),
            entity_id: Set(billable),
            entity_type: Set("task".to_string()),
            emoji: Set("🎉".to_string()),
            user: Set("cody".to_string()),
            created_at: Set(now - Duration::hours(1)),
        },
        reaction::ActiveModel {
            id: Set(demo_id("reaction:auth-bug-comment-thumbs-cody")),
            entity_id: Set(auth_comment),
            entity_type: Set("comment".to_string()),
            emoji: Set("👍".to_string()),
            user: Set("amy".to_string()),
            created_at: Set(now - Duration::hours(4)),
        },
    ];

    for r in reactions {
        let id = match &r.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if reaction::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.reactions_unchanged += 1;
        } else {
            reaction::Entity::insert(r).exec(db).await?;
            summary.reactions_created += 1;
        }
    }
    Ok(())
}

// ── Notification fixtures ───────────────────────────────────────────────────

async fn seed_notifications(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let auth_bug = demo_id("task:fix-auth-bug");
    let design = demo_id("task:design-portal-ui");
    let overdue_tax = demo_id("task:overdue-tax-filing");

    let notifications = [
        // Unread — assignment notification
        notification::ActiveModel {
            id: Set(demo_id("notification:assigned-fix-auth")),
            recipient: Set("cody".to_string()),
            kind: Set("assigned".to_string()),
            message: Set("You were assigned: Fix login session timeout".to_string()),
            actor: Set(Some("amy".to_string())),
            entity_id: Set(Some(auth_bug)),
            entity_type: Set(Some("task".to_string())),
            project: Set(Some("Task App".to_string())),
            read_at: Set(None),
            snoozed_till: Set(None),
            created_at: Set(now - Duration::hours(8)),
        },
        // Read
        notification::ActiveModel {
            id: Set(demo_id("notification:mentioned-design-portal")),
            recipient: Set("cody".to_string()),
            kind: Set("mention".to_string()),
            message: Set("Amy mentioned you in: Design portal landing page".to_string()),
            actor: Set(Some("amy".to_string())),
            entity_id: Set(Some(design)),
            entity_type: Set(Some("task".to_string())),
            project: Set(Some("Task App".to_string())),
            read_at: Set(Some(now - Duration::hours(20))),
            snoozed_till: Set(None),
            created_at: Set(now - Duration::days(1)),
        },
        // Read — system overdue reminder
        notification::ActiveModel {
            id: Set(demo_id("notification:overdue-tax-filing")),
            recipient: Set("cody".to_string()),
            kind: Set("overdue".to_string()),
            message: Set("Task is overdue: File quarterly taxes".to_string()),
            actor: Set(None),
            entity_id: Set(Some(overdue_tax)),
            entity_type: Set(Some("task".to_string())),
            project: Set(Some("Personal Todos".to_string())),
            read_at: Set(Some(now - Duration::hours(2))),
            snoozed_till: Set(None),
            created_at: Set(now - Duration::days(2)),
        },
    ];

    for n in notifications {
        let id = match &n.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if notification::Entity::find_by_id(id)
            .one(db)
            .await?
            .is_some()
        {
            summary.notifications_unchanged += 1;
        } else {
            notification::Entity::insert(n).exec(db).await?;
            summary.notifications_created += 1;
        }
    }
    Ok(())
}

// ── Saved-view fixtures ─────────────────────────────────────────────────────

async fn seed_saved_views(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let today = now.date_naive().to_string();

    let my_today_filters = ViewFilters {
        assignee: vec!["cody".to_string()],
        status: vec!["open".to_string(), "in_progress".to_string()],
        due_before: Some(today.clone()),
        ..Default::default()
    };
    let my_today_display = ViewDisplay {
        layout: Some("list".to_string()),
        order_by: Some("priority".to_string()),
        order_direction: Some("desc".to_string()),
        ..Default::default()
    };

    let inbox_filters = ViewFilters {
        tags: vec!["inbox".to_string()],
        ..Default::default()
    };
    let inbox_display = ViewDisplay {
        layout: Some("list".to_string()),
        order_by: Some("created".to_string()),
        order_direction: Some("desc".to_string()),
        ..Default::default()
    };

    let saved_views = [
        views::ActiveModel {
            id: Set(demo_id("view:my-today")),
            title: Set("My Today".to_string()),
            description: Set(Some(
                "Open + in-progress tasks assigned to me, due today or earlier.".to_string(),
            )),
            project: Set(None),
            filters: Set(my_today_filters),
            display: Set(my_today_display),
            created_by: Set(Some("cody".to_string())),
            is_shared: Set(false),
            sort_order: Set(Some(1.0)),
            created_at: Set(now - Duration::days(30)),
            updated_at: Set(now - Duration::days(1)),
        },
        views::ActiveModel {
            id: Set(demo_id("view:inbox-triage")),
            title: Set("Inbox Triage".to_string()),
            description: Set(Some(
                "Untriaged inbox items needing project routing.".to_string(),
            )),
            project: Set(None),
            filters: Set(inbox_filters),
            display: Set(inbox_display),
            created_by: Set(Some("cody".to_string())),
            is_shared: Set(true),
            sort_order: Set(Some(2.0)),
            created_at: Set(now - Duration::days(60)),
            updated_at: Set(now - Duration::days(7)),
        },
    ];

    for v in saved_views {
        let id = match &v.id {
            Set(value) => *value,
            _ => unreachable!(),
        };
        if views::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.saved_views_unchanged += 1;
        } else {
            views::Entity::insert(v).exec(db).await?;
            summary.saved_views_created += 1;
        }
    }
    Ok(())
}

// ── Cycle fixtures ──────────────────────────────────────────────────────────

async fn seed_cycles(db: &DatabaseConnection, summary: &mut DemoSeedSummary) -> Result<(), DbErr> {
    let today = Utc::now().date_naive();
    let cycles = [cycle::ActiveModel {
        id: Set(demo_id("cycle:sprint-2026-w19")),
        title: Set("Sprint 2026-W19".to_string()),
        description: Set(Some(
            "Mid-quarter sprint focused on auth + portal UI.".to_string(),
        )),
        start_date: Set(Some(today - Duration::days(7))),
        end_date: Set(Some(today + Duration::days(7))),
        owned_by: Set(Some("cody".to_string())),
        tasks: Set(CycleTaskList::from(vec![
            "Fix login session timeout".to_string(),
            "Design portal landing page".to_string(),
            "Add integration tests for realtime".to_string(),
            "Profile slow query path".to_string(),
        ])),
        status: Set(CycleStatus::Active),
        total_tasks: Set(Some(4)),
        completed_tasks: Set(Some(0)),
        sort_order: Set(Some(1.0)),
    }];

    for c in cycles {
        let id = match &c.id {
            Set(value) => *value,
            _ => unreachable!(),
        };
        if cycle::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.cycles_unchanged += 1;
        } else {
            cycle::Entity::insert(c).exec(db).await?;
            summary.cycles_created += 1;
        }
    }
    Ok(())
}

// ── Activity-log fixtures ───────────────────────────────────────────────────

async fn seed_activities(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let auth_bug = demo_id("task:fix-auth-bug");
    let design = demo_id("task:design-portal-ui");
    let billable = demo_id("task:billable-mix-mastering");
    let done_blog = demo_id("task:done-publish-blog");
    let done_q1 = demo_id("task:done-q1-review");
    let tour_book = demo_id("task:tour-book-venues");
    let venue_input = demo_id("task:venue-input-list");
    let perf_pass = demo_id("task:running-timer-perf-pass");

    let activities = [
        activity::ActiveModel {
            id: Set(demo_id("activity:fix-auth-created")),
            entity_id: Set(auth_bug),
            entity_type: Set("task".to_string()),
            verb: Set("created".to_string()),
            field: Set(None),
            old_value: Set(None),
            new_value: Set(None),
            actor: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::days(6)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:fix-auth-status-inprogress")),
            entity_id: Set(auth_bug),
            entity_type: Set("task".to_string()),
            verb: Set("updated".to_string()),
            field: Set(Some("status".to_string())),
            old_value: Set(Some("open".to_string())),
            new_value: Set(Some("in_progress".to_string())),
            actor: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::days(5)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:design-portal-created")),
            entity_id: Set(design),
            entity_type: Set("task".to_string()),
            verb: Set("created".to_string()),
            field: Set(None),
            old_value: Set(None),
            new_value: Set(None),
            actor: Set(Some("amy".to_string())),
            created_at: Set(now - Duration::days(4)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:design-portal-resolved")),
            entity_id: Set(design),
            entity_type: Set("task".to_string()),
            verb: Set("commented".to_string()),
            field: Set(None),
            old_value: Set(None),
            new_value: Set(Some("Mocks attached, signed off.".to_string())),
            actor: Set(Some("amy".to_string())),
            created_at: Set(now - Duration::days(1)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:billable-mix-time-logged")),
            entity_id: Set(billable),
            entity_type: Set("task".to_string()),
            verb: Set("time_logged".to_string()),
            field: Set(Some("time_entries".to_string())),
            old_value: Set(None),
            new_value: Set(Some("120m".to_string())),
            actor: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::hours(2)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:done-publish-blog-completed")),
            entity_id: Set(done_blog),
            entity_type: Set("task".to_string()),
            verb: Set("completed".to_string()),
            field: Set(Some("status".to_string())),
            old_value: Set(Some("in_progress".to_string())),
            new_value: Set(Some("done".to_string())),
            actor: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::days(2)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:done-q1-review-completed")),
            entity_id: Set(done_q1),
            entity_type: Set("task".to_string()),
            verb: Set("completed".to_string()),
            field: Set(Some("status".to_string())),
            old_value: Set(Some("open".to_string())),
            new_value: Set(Some("done".to_string())),
            actor: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::days(7)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:tour-book-venues-priority")),
            entity_id: Set(tour_book),
            entity_type: Set("task".to_string()),
            verb: Set("updated".to_string()),
            field: Set(Some("priority".to_string())),
            old_value: Set(Some("normal".to_string())),
            new_value: Set(Some("high".to_string())),
            actor: Set(Some("bri".to_string())),
            created_at: Set(now - Duration::days(3)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:venue-input-list-created")),
            entity_id: Set(venue_input),
            entity_type: Set("task".to_string()),
            verb: Set("created".to_string()),
            field: Set(None),
            old_value: Set(None),
            new_value: Set(None),
            actor: Set(Some("carter".to_string())),
            created_at: Set(now - Duration::days(2)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:running-timer-perf-pass-started")),
            entity_id: Set(perf_pass),
            entity_type: Set("task".to_string()),
            verb: Set("time_started".to_string()),
            field: Set(Some("time_entries".to_string())),
            old_value: Set(None),
            new_value: Set(Some("running".to_string())),
            actor: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::minutes(45)),
        },
    ];

    for a in activities {
        let id = match &a.id {
            Set(value) => *value,
            _ => unreachable!(),
        };
        if activity::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.activities_unchanged += 1;
        } else {
            activity::Entity::insert(a).exec(db).await?;
            summary.activities_created += 1;
        }
    }
    Ok(())
}

// ── Expense fixtures ────────────────────────────────────────────────────────

async fn seed_expenses(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let today = now.date_naive();

    let expenses = [
        expense::ActiveModel {
            uuid: Set(demo_id("expense:montreal-studio-rental")),
            id: Set("EXP-2026-0001".to_string()),
            number: Set(1),
            status: Set(ExpenseStatus::Open),
            date: Set(today - Duration::days(10)),
            amount_cents: Set(85000),
            currency_code: Set("USD".to_string()),
            project: Set(Some(WikiLink("Montreal Album".to_string()))),
            client: Set(None),
            deliverable: Set(None),
            category: Set(Some("studio".to_string())),
            vendor: Set(Some("Eastside Audio".to_string())),
            description: Set("Studio day rate — vocals tracking".to_string()),
            receipt: Set(None),
            reference: Set(None),
            reimbursable: Set(true),
            notes: Set(None),
            created_by: Set(Some("cody".to_string())),
            date_created: Set(Some(now - Duration::days(10))),
            date_modified: Set(Some(now - Duration::days(10))),
            body: Set(String::new()),
        },
        expense::ActiveModel {
            uuid: Set(demo_id("expense:montreal-mastering-software")),
            id: Set("EXP-2026-0002".to_string()),
            number: Set(2),
            status: Set(ExpenseStatus::Paid),
            date: Set(today - Duration::days(20)),
            amount_cents: Set(29900),
            currency_code: Set("USD".to_string()),
            project: Set(Some(WikiLink("Montreal Album".to_string()))),
            client: Set(None),
            deliverable: Set(None),
            category: Set(Some("software".to_string())),
            vendor: Set(Some("FabFilter".to_string())),
            description: Set("Pro-L 2 mastering limiter license".to_string()),
            receipt: Set(None),
            reference: Set(Some("CC-2025-1129".to_string())),
            reimbursable: Set(false),
            notes: Set(None),
            created_by: Set(Some("cody".to_string())),
            date_created: Set(Some(now - Duration::days(20))),
            date_modified: Set(Some(now - Duration::days(20))),
            body: Set(String::new()),
        },
        expense::ActiveModel {
            uuid: Set(demo_id("expense:tour-van-deposit")),
            id: Set("EXP-2026-0003".to_string()),
            number: Set(3),
            status: Set(ExpenseStatus::Open),
            date: Set(today - Duration::days(2)),
            amount_cents: Set(150000),
            currency_code: Set("USD".to_string()),
            project: Set(Some(WikiLink("Just Friends 2026 Tour".to_string()))),
            client: Set(None),
            deliverable: Set(None),
            category: Set(Some("travel".to_string())),
            vendor: Set(Some("Sprinter Rentals".to_string())),
            description: Set("Sprinter van deposit (3 weeks)".to_string()),
            receipt: Set(None),
            reference: Set(None),
            reimbursable: Set(true),
            notes: Set(None),
            created_by: Set(Some("bri".to_string())),
            date_created: Set(Some(now - Duration::days(2))),
            date_modified: Set(Some(now - Duration::days(2))),
            body: Set(String::new()),
        },
        expense::ActiveModel {
            uuid: Set(demo_id("expense:tour-printed-merch")),
            id: Set("EXP-2026-0004".to_string()),
            number: Set(4),
            status: Set(ExpenseStatus::Draft),
            date: Set(today),
            amount_cents: Set(67500),
            currency_code: Set("USD".to_string()),
            project: Set(Some(WikiLink("Just Friends 2026 Tour".to_string()))),
            client: Set(None),
            deliverable: Set(None),
            category: Set(Some("merch".to_string())),
            vendor: Set(Some("Threadhouse".to_string())),
            description: Set("Tour t-shirt run — 100 units".to_string()),
            receipt: Set(None),
            reference: Set(None),
            reimbursable: Set(true),
            notes: Set(Some("Awaiting design approval.".to_string())),
            created_by: Set(Some("amy".to_string())),
            date_created: Set(Some(now)),
            date_modified: Set(Some(now)),
            body: Set(String::new()),
        },
        expense::ActiveModel {
            uuid: Set(demo_id("expense:misc-coffee-meeting")),
            id: Set("EXP-2026-0005".to_string()),
            number: Set(5),
            status: Set(ExpenseStatus::Cancelled),
            date: Set(today - Duration::days(15)),
            amount_cents: Set(2400),
            currency_code: Set("USD".to_string()),
            project: Set(None),
            client: Set(None),
            deliverable: Set(None),
            category: Set(Some("meals".to_string())),
            vendor: Set(Some("Workshop Cafe".to_string())),
            description: Set("Coffee meeting — duplicate entry".to_string()),
            receipt: Set(None),
            reference: Set(None),
            reimbursable: Set(false),
            notes: Set(Some(
                "Voided — already filed under expense 0001.".to_string(),
            )),
            created_by: Set(Some("cody".to_string())),
            date_created: Set(Some(now - Duration::days(15))),
            date_modified: Set(Some(now - Duration::days(14))),
            body: Set(String::new()),
        },
    ];

    for e in expenses {
        let id = match &e.uuid {
            Set(value) => *value,
            _ => unreachable!(),
        };
        if expense::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.expenses_unchanged += 1;
        } else {
            expense::Entity::insert(e).exec(db).await?;
            summary.expenses_created += 1;
        }
    }
    Ok(())
}

// ── Invoice fixtures ────────────────────────────────────────────────────────

async fn seed_invoices(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let today = now.date_naive();

    let montreal_lines = InvoiceLineList::from(vec![
        InvoiceLine {
            id: "line-1".to_string(),
            task_title: "Master track 3".to_string(),
            description: "Mastering pass + reference compare".to_string(),
            hours: 2.0,
            rate_cents: 15000,
            tax_rate_percent: None,
            discount_percent: None,
        },
        InvoiceLine {
            id: "line-2".to_string(),
            task_title: "Sequence album tracklist".to_string(),
            description: "Sequencing + crossfade prep".to_string(),
            hours: 1.5,
            rate_cents: 15000,
            tax_rate_percent: None,
            discount_percent: None,
        },
    ]);

    let tom_ep_lines = InvoiceLineList::from(vec![InvoiceLine {
        id: "line-1".to_string(),
        task_title: "Track bass for Tom's EP".to_string(),
        description: "Bass tracking + edits".to_string(),
        hours: 6.0,
        rate_cents: 12000,
        tax_rate_percent: None,
        discount_percent: None,
    }]);

    let tom_ep_payment = PaymentList::from(vec![Payment {
        id: "pay-1".to_string(),
        amount_cents: 72000,
        received_at: now - Duration::days(10),
        method: "ach".to_string(),
        reference: Some("ACH-20260214".to_string()),
        recorded_by: Some("cody".to_string()),
        notes: None,
    }]);

    let invoices = [
        invoice::ActiveModel {
            uuid: Set(demo_id("invoice:montreal-mar")),
            id: Set("INV-2026-0001".to_string()),
            number: Set(1),
            status: Set(InvoiceStatus::Sent),
            client: Set(WikiLink("Montreal Records".to_string())),
            issue_date: Set(today - Duration::days(5)),
            due_date: Set(today + Duration::days(25)),
            currency_code: Set("USD".to_string()),
            line_items: Set(montreal_lines),
            tax_rate_percent: Set(None),
            discount_percent: Set(None),
            po_number: Set(None),
            public_notes: Set(Some("Net 30. Thanks for the work!".to_string())),
            private_notes: Set(None),
            payments: Set(PaymentList::default()),
            entry_ids: Set(StringList::from(vec!["te-mastering-1".to_string()])),
            sent_at: Set(Some(now - Duration::days(5))),
            paid_at: Set(None),
            cancelled_at: Set(None),
            cancelled_reason: Set(None),
            created_by: Set(Some("cody".to_string())),
            date_created: Set(Some(now - Duration::days(5))),
            date_modified: Set(Some(now - Duration::days(5))),
        },
        invoice::ActiveModel {
            uuid: Set(demo_id("invoice:tom-ep-feb-paid")),
            id: Set("INV-2026-0002".to_string()),
            number: Set(2),
            status: Set(InvoiceStatus::Paid),
            client: Set(WikiLink("TomBrooksMusic".to_string())),
            issue_date: Set(today - Duration::days(40)),
            due_date: Set(today - Duration::days(10)),
            currency_code: Set("USD".to_string()),
            line_items: Set(tom_ep_lines),
            tax_rate_percent: Set(None),
            discount_percent: Set(None),
            po_number: Set(None),
            public_notes: Set(None),
            private_notes: Set(None),
            payments: Set(tom_ep_payment),
            entry_ids: Set(StringList::default()),
            sent_at: Set(Some(now - Duration::days(40))),
            paid_at: Set(Some(now - Duration::days(10))),
            cancelled_at: Set(None),
            cancelled_reason: Set(None),
            created_by: Set(Some("cody".to_string())),
            date_created: Set(Some(now - Duration::days(40))),
            date_modified: Set(Some(now - Duration::days(10))),
        },
    ];

    for inv in invoices {
        let id = match &inv.uuid {
            Set(value) => *value,
            _ => unreachable!(),
        };
        if invoice::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.invoices_unchanged += 1;
        } else {
            invoice::Entity::insert(inv).exec(db).await?;
            summary.invoices_created += 1;
        }
    }
    Ok(())
}

// ── Email-ref fixtures ──────────────────────────────────────────────────────

async fn seed_email_refs(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let emails = [
        email::ActiveModel {
            uuid: Set(demo_id("email:fix-auth-stack-trace")),
            message_id: Set("<auth-bug-trace@fasttrackstudio.com>".to_string()),
            subject: Set("[bug] Auth session timeout — stack trace".to_string()),
            from: Set("amy@fasttrackstudio.com".to_string()),
            to: Set(EmailStringList::from(vec![
                "cody@fasttrackstudio.com".to_string(),
            ])),
            date: Set(now - Duration::days(2)),
            snippet: Set(Some(
                "Reproduced again this morning — stack trace attached.".to_string(),
            )),
            account_id: Set(Some(1)),
            mailbox: Set(Some("INBOX".to_string())),
            imap_uid: Set(Some(1042)),
            nc_db_id: Set(Some(9001)),
            has_attachments: Set(true),
            attachment_count: Set(1),
            linked_by: Set(Some("amy".to_string())),
            linked_at: Set(Some(now - Duration::days(2))),
            user_tags: Set(EmailStringList::from(vec!["bug".to_string()])),
        },
        email::ActiveModel {
            uuid: Set(demo_id("email:montreal-client-revisions")),
            message_id: Set("<mtl-revisions-2@montrealrecords.com>".to_string()),
            subject: Set("Montreal Album — round 2 revisions".to_string()),
            from: Set("a&r@montrealrecords.com".to_string()),
            to: Set(EmailStringList::from(vec![
                "cody@fasttrackaudio.com".to_string(),
            ])),
            date: Set(now - Duration::days(3)),
            snippet: Set(Some(
                "A few notes on track 3 — see timestamps below.".to_string(),
            )),
            account_id: Set(Some(1)),
            mailbox: Set(Some("Clients/Montreal".to_string())),
            imap_uid: Set(Some(204)),
            nc_db_id: Set(Some(9002)),
            has_attachments: Set(false),
            attachment_count: Set(0),
            linked_by: Set(Some("cody".to_string())),
            linked_at: Set(Some(now - Duration::days(3))),
            user_tags: Set(EmailStringList::from(vec![
                "client".to_string(),
                "billable".to_string(),
            ])),
        },
        email::ActiveModel {
            uuid: Set(demo_id("email:tour-venue-confirmation")),
            message_id: Set("<venue-confirm-9281@campusjax.com>".to_string()),
            subject: Set("Campus Jax — show confirmation".to_string()),
            from: Set("booking@campusjax.com".to_string()),
            to: Set(EmailStringList::from(vec![
                "bri@fasttrackstudio.com".to_string(),
            ])),
            date: Set(now - Duration::days(1)),
            snippet: Set(Some(
                "Confirmed for the date — load-in starts at 4pm.".to_string(),
            )),
            account_id: Set(Some(1)),
            mailbox: Set(Some("INBOX".to_string())),
            imap_uid: Set(Some(2008)),
            nc_db_id: Set(Some(9003)),
            has_attachments: Set(false),
            attachment_count: Set(0),
            linked_by: Set(Some("bri".to_string())),
            linked_at: Set(Some(now - Duration::days(1))),
            user_tags: Set(EmailStringList::from(vec!["tour".to_string()])),
        },
    ];

    for e in emails {
        let id = match &e.uuid {
            Set(value) => *value,
            _ => unreachable!(),
        };
        if email::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.email_refs_unchanged += 1;
        } else {
            email::Entity::insert(e).exec(db).await?;
            summary.email_refs_created += 1;
        }
    }
    Ok(())
}

// ── Attachment fixtures ─────────────────────────────────────────────────────

async fn seed_attachments(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let mix_master = demo_id("task:billable-mix-mastering");
    let track_seq = demo_id("task:montreal-track-sequencing");
    let tom_mixing = demo_id("task:tom-ep-mixing-collab");
    let tour_stage = demo_id("task:tour-stage-plot");

    let attachments = [
        attachment::ActiveModel {
            id: Set(demo_id("attachment:montreal-master-wav")),
            owner_id: Set(mix_master),
            owner_type: Set("task".to_string()),
            source: Set("nextcloud".to_string()),
            path: Set("Projects/Montreal Album/masters/master-v3.wav".to_string()),
            label: Set(Some("master-v3.wav".to_string())),
            mime: Set(Some("audio/wav".to_string())),
            size_bytes: Set(Some(48_123_456)),
            checksum: Set(Some(
                "9f0c3f2c2b6e5b48b7c0a8d8f9a3b1e8d5c4a2b1e0f9d8c7b6a5f4e3d2c1b0a9".to_string(),
            )),
            uploader: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::days(2)),
            updated_at: Set(now - Duration::days(2)),
        },
        attachment::ActiveModel {
            id: Set(demo_id("attachment:montreal-stems-zip")),
            owner_id: Set(track_seq),
            owner_type: Set("task".to_string()),
            source: Set("nextcloud".to_string()),
            path: Set("Projects/Montreal Album/stems/stems-bundle.zip".to_string()),
            label: Set(Some("stems-bundle.zip".to_string())),
            mime: Set(Some("application/zip".to_string())),
            size_bytes: Set(Some(312_456_789)),
            checksum: Set(Some(
                "1a2b3c4d5e6f70819293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c6d7e8f9".to_string(),
            )),
            uploader: Set(Some("amy".to_string())),
            created_at: Set(now - Duration::days(4)),
            updated_at: Set(now - Duration::days(4)),
        },
        attachment::ActiveModel {
            id: Set(demo_id("attachment:tom-ep-mix-notes-pdf")),
            owner_id: Set(tom_mixing),
            owner_type: Set("task".to_string()),
            source: Set("nextcloud".to_string()),
            path: Set("Projects/Tom EP/notes/mix-notes-feb.pdf".to_string()),
            label: Set(Some("Mix notes — Feb".to_string())),
            mime: Set(Some("application/pdf".to_string())),
            size_bytes: Set(Some(184_320)),
            checksum: Set(Some(
                "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210".to_string(),
            )),
            uploader: Set(Some("tom".to_string())),
            created_at: Set(now - Duration::days(1)),
            updated_at: Set(now - Duration::days(1)),
        },
        attachment::ActiveModel {
            id: Set(demo_id("attachment:tour-stage-plot-png")),
            owner_id: Set(tour_stage),
            owner_type: Set("task".to_string()),
            source: Set("nextcloud".to_string()),
            path: Set("Projects/JF Tour 2026/plots/stage-plot-v1.png".to_string()),
            label: Set(Some("stage-plot-v1.png".to_string())),
            mime: Set(Some("image/png".to_string())),
            size_bytes: Set(Some(2_456_789)),
            checksum: Set(Some(
                "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".to_string(),
            )),
            uploader: Set(Some("bri".to_string())),
            created_at: Set(now - Duration::hours(8)),
            updated_at: Set(now - Duration::hours(8)),
        },
    ];

    for a in attachments {
        let id = match &a.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if attachment::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.attachments_unchanged += 1;
        } else {
            attachment::Entity::insert(a).exec(db).await?;
            summary.attachments_created += 1;
        }
    }
    Ok(())
}

// ── Integration / project-type fixtures ────────────────────────────────────

async fn seed_integrations(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let integrations = [
        integration_active_full(
            "integration:audio-production",
            "audio-production",
            &[
                ("Tracking", false, Some("#3b82f6")),
                ("Mixing", false, Some("#8b5cf6")),
                ("Mastering", false, Some("#ec4899")),
                ("Approved", true, Some("#10b981")),
                ("On Hold", false, Some("#f59e0b")),
            ],
            &[
                ("Set up session", "tracking", "high", "audio,session-prep"),
                ("Reference compare", "mixing", "normal", "audio,review"),
                (
                    "Master & loudness check",
                    "mastering",
                    "high",
                    "audio,master",
                ),
                (
                    "Client approval round",
                    "approved",
                    "normal",
                    "audio,client",
                ),
            ],
            &[
                (
                    "Album Production",
                    Some("Full album: pre-pro → tracking → mix → master → approval"),
                    &[
                        (
                            "Pre-production: scope + reference",
                            "tracking",
                            "high",
                            "audio,planning",
                        ),
                        (
                            "Tracking: drums & bass",
                            "tracking",
                            "high",
                            "audio,tracking",
                        ),
                        (
                            "Tracking: guitars & keys",
                            "tracking",
                            "normal",
                            "audio,tracking",
                        ),
                        (
                            "Tracking: vocals",
                            "tracking",
                            "high",
                            "audio,tracking,vocals",
                        ),
                        ("Mixing: rough mixes", "mixing", "high", "audio,mix"),
                        (
                            "Mixing: client revisions",
                            "mixing",
                            "normal",
                            "audio,mix,client",
                        ),
                        ("Mastering pass", "mastering", "high", "audio,master"),
                        (
                            "Client approval + delivery",
                            "approved",
                            "high",
                            "audio,client,delivery",
                        ),
                    ],
                ),
                (
                    "Live Show Prep",
                    Some("Live performance: input list, stage plot, runners, day-of"),
                    &[
                        (
                            "Confirm venue + stage size",
                            "tracking",
                            "high",
                            "live,venue",
                        ),
                        ("Build input list", "tracking", "high", "live,audio"),
                        ("Draft stage plot", "mixing", "normal", "live,plot"),
                        ("Soundcheck + line check", "mastering", "high", "live,audio"),
                        ("Show", "approved", "high", "live,show"),
                    ],
                ),
            ],
            &["Music"],
            &["studio", "remote"],
        ),
        integration_active(
            "integration:video-production",
            "video-production",
            &[
                ("Pre-production", false, Some("#3b82f6")),
                ("Shoot", false, Some("#f97316")),
                ("Editing", false, Some("#8b5cf6")),
                ("Color", false, Some("#ec4899")),
                ("Sound", false, Some("#06b6d4")),
                ("Final", true, Some("#10b981")),
            ],
            &[
                ("Shot list", "pre-production", "high", "video,planning"),
                (
                    "Location scout",
                    "pre-production",
                    "normal",
                    "video,planning",
                ),
                ("Rough cut", "editing", "normal", "video,edit"),
                ("Color pass", "color", "normal", "video,color"),
                ("Audio mix", "sound", "normal", "video,audio"),
            ],
            &["Video"],
            &["set", "studio", "post"],
        ),
        integration_active(
            "integration:cooking",
            "cooking",
            &[
                ("Planned", false, Some("#3b82f6")),
                ("Shopping", false, Some("#f59e0b")),
                ("Prepping", false, Some("#8b5cf6")),
                ("Cooked", true, Some("#10b981")),
            ],
            &[
                (
                    "Plan meals for the week",
                    "planned",
                    "normal",
                    "cooking,plan",
                ),
                (
                    "Generate shopping list",
                    "shopping",
                    "normal",
                    "cooking,shopping",
                ),
                ("Mise en place", "prepping", "normal", "cooking,prep"),
                ("Cook & plate", "cooked", "normal", "cooking"),
            ],
            &["Personal"],
            &["kitchen", "shop"],
        ),
        integration_active(
            "integration:fitness",
            "fitness",
            &[
                ("Scheduled", false, Some("#3b82f6")),
                ("In Progress", false, Some("#8b5cf6")),
                ("Completed", true, Some("#10b981")),
                ("Skipped", false, Some("#6b7280")),
            ],
            &[
                ("Warm-up", "scheduled", "low", "fitness,warmup"),
                ("Main lift", "in-progress", "high", "fitness,strength"),
                (
                    "Accessory work",
                    "in-progress",
                    "normal",
                    "fitness,accessory",
                ),
                ("Cooldown & log", "completed", "low", "fitness,log"),
            ],
            &["Personal"],
            &["gym", "home"],
        ),
    ];

    for i in integrations {
        let id = match &i.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if integration::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.integrations_unchanged += 1;
        } else {
            integration::Entity::insert(i).exec(db).await?;
            summary.integrations_created += 1;
        }
    }
    Ok(())
}

fn integration_active(
    key: &str,
    name: &str,
    statuses: &[(&str, bool, Option<&str>)],
    task_templates: &[(&str, &str, &str, &str)],
    area_conventions: &[&str],
    context_conventions: &[&str],
) -> integration::ActiveModel {
    integration_active_full(
        key,
        name,
        statuses,
        task_templates,
        &[],
        area_conventions,
        context_conventions,
    )
}

#[allow(clippy::type_complexity)]
fn integration_active_full(
    key: &str,
    name: &str,
    statuses: &[(&str, bool, Option<&str>)],
    task_templates: &[(&str, &str, &str, &str)],
    project_templates: &[(&str, Option<&str>, &[(&str, &str, &str, &str)])],
    area_conventions: &[&str],
    context_conventions: &[&str],
) -> integration::ActiveModel {
    let status_list = StatusDefList(
        statuses
            .iter()
            .map(|(s, completion, color)| StatusDef {
                name: (*s).to_string(),
                is_completion: *completion,
                color: color.map(str::to_string),
            })
            .collect(),
    );

    let task_template_list = TaskTemplateList(
        task_templates
            .iter()
            .map(|(title, status, priority, tags)| TaskTemplate {
                title: (*title).to_string(),
                status: Some((*status).to_string()),
                priority: Some((*priority).to_string()),
                contexts: IntegrationStringList::default(),
                tags: IntegrationStringList(
                    tags.split(',').map(|t| t.trim().to_string()).collect(),
                ),
                recurrence: None,
                time_estimate_minutes: None,
                body: None,
            })
            .collect(),
    );

    let project_template_list = ProjectTemplateList(
        project_templates
            .iter()
            .map(|(tname, desc, tasks)| ProjectTemplate {
                name: (*tname).to_string(),
                description: desc.map(str::to_string),
                tasks: TaskTemplateList(
                    tasks
                        .iter()
                        .map(|(title, status, priority, tags)| TaskTemplate {
                            title: (*title).to_string(),
                            status: Some((*status).to_string()),
                            priority: Some((*priority).to_string()),
                            contexts: IntegrationStringList::default(),
                            tags: IntegrationStringList(
                                tags.split(',').map(|t| t.trim().to_string()).collect(),
                            ),
                            recurrence: None,
                            time_estimate_minutes: None,
                            body: None,
                        })
                        .collect(),
                ),
            })
            .collect(),
    );

    integration::ActiveModel {
        id: Set(demo_id(key)),
        name: Set(name.to_string()),
        statuses: Set(status_list),
        project_templates: Set(project_template_list),
        task_templates: Set(task_template_list),
        area_conventions: Set(IntegrationStringList(
            area_conventions.iter().map(|s| s.to_string()).collect(),
        )),
        context_conventions: Set(IntegrationStringList(
            context_conventions.iter().map(|s| s.to_string()).collect(),
        )),
    }
}

// ── Track fixtures ──────────────────────────────────────────────────────────

async fn seed_tracks(db: &DatabaseConnection, summary: &mut DemoSeedSummary) -> Result<(), DbErr> {
    let montreal = demo_id("project:fasttrack-album");
    let tom_ep = demo_id("project:tom-solo-ep");

    // (key, project_id, sequence, title, status, bpm, key, artist, lead,
    //  revision, genre, mood, with_paths, approved_by)
    type Spec<'a> = (
        &'a str,         // key
        Uuid,            // project_id
        u32,             // sequence
        &'a str,         // title
        TrackStatus,     // status
        f64,             // bpm
        &'a str,         // musical key
        &'a str,         // artist
        &'a str,         // created_by
        i32,             // revision_number
        &'a str,         // genre
        &'a str,         // mood
        bool,            // with_paths (daw + stems + reference_url)
        Option<&'a str>, // approved_by (sets status to Approved + properties)
    );

    let specs: &[Spec] = &[
        // Montreal Album
        (
            "track:montreal-headlights",
            montreal,
            1,
            "Headlights",
            TrackStatus::Mastering,
            96.5,
            "Em",
            "Just Friends",
            "cody",
            3,
            "indie rock",
            "introspective",
            true,
            None,
        ),
        (
            "track:montreal-saint-laurent",
            montreal,
            2,
            "Saint Laurent",
            TrackStatus::Mixing,
            124.0,
            "F#m",
            "Just Friends",
            "cody",
            2,
            "indie rock",
            "energetic",
            false,
            None,
        ),
        (
            "track:montreal-tundra",
            montreal,
            3,
            "Tundra",
            TrackStatus::Mixing,
            88.0,
            "Dm",
            "Just Friends",
            "cody",
            1,
            "indie rock",
            "melancholic",
            false,
            None,
        ),
        (
            "track:montreal-plateau",
            montreal,
            4,
            "Plateau",
            TrackStatus::Mastering,
            110.5,
            "C",
            "Just Friends",
            "cody",
            2,
            "indie rock",
            "uplifting",
            false,
            None,
        ),
        (
            "track:montreal-rue-de-bleury",
            montreal,
            5,
            "Rue de Bleury",
            TrackStatus::Tracking,
            76.0,
            "Bm",
            "Just Friends",
            "cody",
            0,
            "indie rock",
            "introspective",
            false,
            None,
        ),
        (
            "track:montreal-mile-end",
            montreal,
            6,
            "Mile End",
            TrackStatus::Mixing,
            92.0,
            "G",
            "Just Friends",
            "cody",
            1,
            "indie rock",
            "warm",
            false,
            None,
        ),
        (
            "track:montreal-notre-dame",
            montreal,
            7,
            "Notre-Dame",
            TrackStatus::Editing,
            67.0,
            "Am",
            "Just Friends",
            "cody",
            0,
            "indie rock",
            "reverent",
            false,
            None,
        ),
        (
            "track:montreal-outremont",
            montreal,
            8,
            "Outremont",
            TrackStatus::Composing,
            132.0,
            "E",
            "Just Friends",
            "cody",
            0,
            "indie rock",
            "driving",
            false,
            None,
        ),
        // Tom Solo EP
        (
            "track:tom-drive-south",
            tom_ep,
            1,
            "Drive South",
            TrackStatus::Approved,
            102.0,
            "A",
            "Tom Brooks",
            "tom",
            4,
            "americana",
            "wistful",
            true,
            Some("tom"),
        ),
        (
            "track:tom-slow-burn",
            tom_ep,
            2,
            "Slow Burn",
            TrackStatus::Mastering,
            78.5,
            "Cm",
            "Tom Brooks",
            "tom",
            3,
            "americana",
            "smouldering",
            false,
            None,
        ),
        (
            "track:tom-sundown",
            tom_ep,
            3,
            "Sundown",
            TrackStatus::Mixing,
            95.0,
            "D",
            "Tom Brooks",
            "tom",
            2,
            "americana",
            "warm",
            false,
            None,
        ),
        (
            "track:tom-half-light",
            tom_ep,
            4,
            "Half Light",
            TrackStatus::Tracking,
            88.0,
            "F",
            "Tom Brooks",
            "tom",
            0,
            "americana",
            "tender",
            false,
            None,
        ),
    ];

    let now = Utc::now();
    for spec in specs {
        let (
            key,
            project_id,
            sequence,
            title,
            status,
            bpm,
            musical_key,
            artist,
            created_by,
            revision,
            genre,
            mood,
            with_paths,
            approved_by,
        ) = *spec;

        let id = demo_id(key);
        if track::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.tracks_unchanged += 1;
            continue;
        }

        let mut props = serde_json::Map::new();
        props.insert(
            "genre".to_string(),
            serde_json::Value::String(genre.to_string()),
        );
        props.insert(
            "mood".to_string(),
            serde_json::Value::String(mood.to_string()),
        );
        if let Some(actor) = approved_by {
            props.insert(
                "approved_by".to_string(),
                serde_json::Value::String(actor.to_string()),
            );
            props.insert(
                "approved_at".to_string(),
                serde_json::Value::String(now.to_rfc3339()),
            );
        }

        let (daw, stems, reference) = if with_paths {
            (
                Some(format!("Projects/{title}/{title}.als")),
                Some(format!("Projects/{title}/stems")),
                Some(format!("https://reference.example/{}", slugify(title))),
            )
        } else {
            (None, None, None)
        };

        let active = track::ActiveModel {
            id: Set(id),
            project_id: Set(project_id),
            title: Set(title.to_string()),
            sequence: Set(sequence),
            status: Set(status),
            bpm: Set(Some(bpm)),
            key: Set(Some(musical_key.to_string())),
            duration_ms: Set(None),
            time_signature: Set(Some("4/4".to_string())),
            daw_session_path: Set(daw),
            stems_path: Set(stems),
            reference_url: Set(reference),
            isrc: Set(None),
            artist: Set(Some(artist.to_string())),
            notes: Set(None),
            revision_number: Set(revision),
            created_by: Set(Some(created_by.to_string())),
            properties: Set(JsonObject::from_value(serde_json::Value::Object(props))),
            created_at: Set(now),
            updated_at: Set(now),
        };
        track::Entity::insert(active).exec(db).await?;
        summary.tracks_created += 1;
    }
    Ok(())
}

// ── Cooking fixtures ─────────────────────────────────────────────────────────

struct RecipeFixture {
    key: &'static str,
    name: &'static str,
    description: &'static str,
    prep: u32,
    cook: u32,
    servings: u32,
    cuisine: &'static str,
    dietary: &'static [&'static str],
    /// (quantity, unit, food)
    ingredients: &'static [(Option<f64>, Option<&'static str>, &'static str)],
    /// Plain step text in order.
    steps: &'static [&'static str],
}

const RECIPE_FIXTURES: &[RecipeFixture] = &[
    RecipeFixture {
        key: "recipe:weeknight-carbonara",
        name: "Weeknight Carbonara",
        description: "Classic Roman pasta — guanciale, eggs, pecorino, lots of pepper.",
        prep: 10,
        cook: 20,
        servings: 4,
        cuisine: "italian",
        dietary: &[],
        ingredients: &[
            (Some(1.0), Some("lb"), "spaghetti"),
            (Some(4.0), None, "egg yolks"),
            (Some(1.0), None, "whole egg"),
            (Some(1.0), Some("cup"), "pecorino romano"),
            (Some(6.0), Some("oz"), "guanciale"),
            (Some(2.0), Some("tsp"), "black pepper"),
        ],
        steps: &[
            "Bring a large pot of salted water to a boil and cook the spaghetti to al dente.",
            "While pasta cooks, render diced guanciale in a wide skillet until crisp.",
            "Whisk yolks, whole egg, pecorino and pepper. Temper with a ladle of pasta water.",
            "Toss pasta with guanciale off heat, add egg mixture, toss to a glossy sauce.",
        ],
    },
    RecipeFixture {
        key: "recipe:sheet-pan-chicken-veg",
        name: "Sheet-pan Chicken & Veg",
        description: "Chicken thighs roasted with broccoli and sweet potato — minimal cleanup.",
        prep: 10,
        cook: 35,
        servings: 4,
        cuisine: "american",
        dietary: &["gluten-free"],
        ingredients: &[
            (Some(2.0), Some("lb"), "chicken thighs"),
            (Some(1.0), Some("head"), "broccoli, cut into florets"),
            (Some(2.0), None, "sweet potatoes, cubed"),
            (Some(3.0), Some("tbsp"), "olive oil"),
            (Some(2.0), Some("tsp"), "smoked paprika"),
            (Some(4.0), Some("clove"), "garlic, minced"),
        ],
        steps: &[
            "Preheat oven to 425°F. Toss vegetables with oil, paprika, garlic, salt, pepper.",
            "Spread on sheet pan, nestle chicken thighs between, brush thighs with oil.",
            "Roast 30–35 minutes until thighs hit 165°F and vegetables are caramelized.",
        ],
    },
    RecipeFixture {
        key: "recipe:chickpea-curry",
        name: "Chickpea Curry",
        description: "Pantry-friendly chana masala with coconut milk and garam masala.",
        prep: 10,
        cook: 25,
        servings: 4,
        cuisine: "indian",
        dietary: &["vegetarian", "gluten-free"],
        ingredients: &[
            (Some(2.0), None, "cans chickpeas, drained"),
            (Some(1.0), None, "yellow onion, diced"),
            (Some(4.0), Some("clove"), "garlic, minced"),
            (Some(1.0), Some("tbsp"), "fresh ginger, grated"),
            (Some(1.0), None, "can crushed tomatoes"),
            (Some(1.0), None, "can coconut milk"),
            (Some(2.0), Some("tsp"), "garam masala"),
        ],
        steps: &[
            "Sauté onion in oil until translucent, add garlic and ginger, cook one minute.",
            "Stir in tomatoes and garam masala, simmer 5 minutes to bloom spices.",
            "Add chickpeas and coconut milk, simmer 15 minutes until thickened.",
            "Season with salt; serve over rice.",
        ],
    },
    RecipeFixture {
        key: "recipe:smashburger",
        name: "Smashburger",
        description: "Smash-style griddle burger on toasted brioche with American cheese.",
        prep: 5,
        cook: 15,
        servings: 2,
        cuisine: "american",
        dietary: &[],
        ingredients: &[
            (Some(8.0), Some("oz"), "ground beef (80/20)"),
            (Some(2.0), None, "brioche buns"),
            (Some(2.0), Some("slice"), "american cheese"),
            (Some(2.0), Some("tbsp"), "butter"),
            (Some(1.0), Some("tsp"), "kosher salt"),
        ],
        steps: &[
            "Heat cast-iron skillet smoking hot. Butter and toast bun halves.",
            "Form beef into two loose balls. Smash flat in pan, season heavily.",
            "Flip after 2 minutes, top with cheese, melt. Stack on bun.",
        ],
    },
    RecipeFixture {
        key: "recipe:greek-salad",
        name: "Greek Salad",
        description: "Crisp Mediterranean salad — no lettuce, lots of feta and olives.",
        prep: 10,
        cook: 0,
        servings: 2,
        cuisine: "mediterranean",
        dietary: &["vegetarian", "gluten-free"],
        ingredients: &[
            (Some(1.0), None, "english cucumber, diced"),
            (Some(2.0), None, "tomatoes, diced"),
            (Some(0.5), None, "red onion, sliced"),
            (Some(0.5), Some("cup"), "kalamata olives"),
            (Some(4.0), Some("oz"), "feta cheese, cubed"),
            (Some(3.0), Some("tbsp"), "olive oil"),
            (Some(1.0), Some("tsp"), "dried oregano"),
        ],
        steps: &[
            "Combine cucumber, tomato, onion, olives in a wide bowl.",
            "Drizzle olive oil, scatter feta and oregano, season with salt.",
        ],
    },
    RecipeFixture {
        key: "recipe:banana-pancakes",
        name: "Banana Pancakes",
        description: "Fluffy banana-flecked pancakes — weekend breakfast staple.",
        prep: 5,
        cook: 15,
        servings: 4,
        cuisine: "breakfast",
        dietary: &["vegetarian"],
        ingredients: &[
            (Some(2.0), None, "ripe bananas, mashed"),
            (Some(2.0), None, "eggs"),
            (Some(1.5), Some("cup"), "all-purpose flour"),
            (Some(2.0), Some("tsp"), "baking powder"),
            (Some(1.25), Some("cup"), "milk"),
            (Some(1.0), Some("tsp"), "cinnamon"),
        ],
        steps: &[
            "Whisk dry ingredients. In a separate bowl, whisk bananas, eggs, milk.",
            "Combine wet and dry until just incorporated.",
            "Cook on medium-hot griddle, ~2 min per side until golden.",
        ],
    },
];

async fn seed_cooking(db: &DatabaseConnection, summary: &mut DemoSeedSummary) -> Result<(), DbErr> {
    let now = Utc::now();

    // ── Recipes (with ingredients + steps) ───────────────────────────
    for fix in RECIPE_FIXTURES {
        let id = demo_id(fix.key);
        if recipe::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.recipes_unchanged += 1;
            continue;
        }
        let mut props = serde_json::Map::new();
        props.insert(
            "cuisine".to_string(),
            serde_json::Value::String(fix.cuisine.to_string()),
        );
        if !fix.dietary.is_empty() {
            props.insert(
                "dietary".to_string(),
                serde_json::Value::Array(
                    fix.dietary
                        .iter()
                        .map(|s| serde_json::Value::String((*s).to_string()))
                        .collect(),
                ),
            );
        }
        let active = recipe::ActiveModel {
            id: Set(id),
            name: Set(fix.name.to_string()),
            slug: Set(slugify(fix.name)),
            description: Set(Some(fix.description.to_string())),
            organization: Set(Some(ORG_PERSONAL.to_string())),
            prep_time_minutes: Set(Some(fix.prep)),
            cook_time_minutes: Set(Some(fix.cook)),
            total_time_minutes: Set(Some(fix.prep + fix.cook)),
            servings: Set(Some(fix.servings)),
            yield_label: Set(None),
            source_url: Set(None),
            image_url: Set(None),
            rating: Set(None),
            last_made: Set(None),
            notes: Set(None),
            created_by: Set(Some("cody".to_string())),
            properties: Set(JsonObject::from_value(serde_json::Value::Object(props))),
            nutrition_summary: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        recipe::Entity::insert(active).exec(db).await?;
        summary.recipes_created += 1;

        for (idx, (qty, unit, food_text)) in fix.ingredients.iter().enumerate() {
            // Auto-link to canonical Food by name when one already
            // exists (foods are seeded earlier in this routine — see
            // `seed_foods`). Idempotent across re-runs.
            let food_id = task_core::food::find_food_by_name(db, Some(ORG_PERSONAL), food_text)
                .await?
                .map(|f| f.id);
            let ing_active = recipe_ingredient::ActiveModel {
                id: Set(demo_id(&format!("{}:ing:{}", fix.key, idx))),
                recipe_id: Set(id),
                sequence: Set((idx + 1) as u32),
                quantity: Set(*qty),
                unit: Set(unit.map(|s| s.to_string())),
                food: Set(food_text.to_string()),
                food_id: Set(food_id),
                note: Set(None),
                is_section: Set(false),
                created_at: Set(now),
                updated_at: Set(now),
            };
            recipe_ingredient::Entity::insert(ing_active)
                .exec(db)
                .await?;
        }
        for (idx, text) in fix.steps.iter().enumerate() {
            let step_active = recipe_step::ActiveModel {
                id: Set(demo_id(&format!("{}:step:{}", fix.key, idx))),
                recipe_id: Set(id),
                sequence: Set((idx + 1) as u32),
                text: Set((*text).to_string()),
                duration_minutes: Set(None),
                created_at: Set(now),
                updated_at: Set(now),
            };
            recipe_step::Entity::insert(step_active).exec(db).await?;
        }
    }

    // ── Cookbook (Cody's Weeknight Rotation) ─────────────────────────
    let cookbook_key = "cookbook:codys-weeknight-rotation";
    let cookbook_id = demo_id(cookbook_key);
    if cookbook::Entity::find_by_id(cookbook_id)
        .one(db)
        .await?
        .is_none()
    {
        let active = cookbook::ActiveModel {
            id: Set(cookbook_id),
            name: Set("Cody's Weeknight Rotation".to_string()),
            description: Set(Some(
                "Six recipes I rotate through on busy weeknights.".to_string(),
            )),
            organization: Set(Some(ORG_PERSONAL.to_string())),
            created_by: Set(Some("cody".to_string())),
            created_at: Set(now),
            updated_at: Set(now),
        };
        cookbook::Entity::insert(active).exec(db).await?;
        summary.cookbooks_created += 1;
        for (idx, fix) in RECIPE_FIXTURES.iter().enumerate() {
            let join_id = demo_id(&format!("{}:join:{}", cookbook_key, fix.key));
            let active = cookbook_recipe::ActiveModel {
                id: Set(join_id),
                cookbook_id: Set(cookbook_id),
                recipe_id: Set(demo_id(fix.key)),
                sequence: Set((idx + 1) as u32),
                added_at: Set(now),
            };
            cookbook_recipe::Entity::insert(active).exec(db).await?;
        }
    } else {
        summary.cookbooks_unchanged += 1;
    }

    // ── Meal plan (today + next 6 days) ──────────────────────────────
    let today = chrono::Local::now().date_naive();
    type MealPlanFixture = (
        &'static str,
        i64,
        MealType,
        Option<&'static str>,
        Option<&'static str>,
    );
    let entries: &[MealPlanFixture] = &[
        (
            "meal:day0-lunch",
            0,
            MealType::Lunch,
            Some("recipe:greek-salad"),
            None,
        ),
        (
            "meal:day0-dinner",
            0,
            MealType::Dinner,
            Some("recipe:weeknight-carbonara"),
            None,
        ),
        (
            "meal:day1-breakfast",
            1,
            MealType::Breakfast,
            Some("recipe:banana-pancakes"),
            None,
        ),
        (
            "meal:day1-dinner",
            1,
            MealType::Dinner,
            Some("recipe:sheet-pan-chicken-veg"),
            None,
        ),
        (
            "meal:day2-dinner",
            2,
            MealType::Dinner,
            Some("recipe:chickpea-curry"),
            None,
        ),
        (
            "meal:day3-dinner",
            3,
            MealType::Dinner,
            Some("recipe:smashburger"),
            Some("movie night"),
        ),
        (
            "meal:day4-dinner",
            4,
            MealType::Dinner,
            Some("recipe:weeknight-carbonara"),
            None,
        ),
        (
            "meal:day5-lunch",
            5,
            MealType::Lunch,
            Some("recipe:greek-salad"),
            None,
        ),
        (
            "meal:day6-dinner",
            6,
            MealType::Dinner,
            None,
            Some("leftovers"),
        ),
    ];
    for (key, day_offset, meal, recipe_key, title) in entries {
        let id = demo_id(key);
        if meal_plan::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.meal_plan_entries_unchanged += 1;
            continue;
        }
        let date = today + Duration::days(*day_offset);
        let active = meal_plan::ActiveModel {
            id: Set(id),
            date: Set(date),
            meal_type: Set(*meal),
            organization: Set(Some(ORG_PERSONAL.to_string())),
            recipe_id: Set(recipe_key.map(demo_id)),
            title: Set(title.map(|s| s.to_string())),
            servings_planned: Set(None),
            notes: Set(None),
            created_by: Set(Some("cody".to_string())),
            created_at: Set(now),
            updated_at: Set(now),
        };
        meal_plan::Entity::insert(active).exec(db).await?;
        summary.meal_plan_entries_created += 1;
    }

    // ── Shopping list ("This Week") ──────────────────────────────────
    let list_key = "shop:this-week";
    let list_id = demo_id(list_key);
    if shopping_list::Entity::find_by_id(list_id)
        .one(db)
        .await?
        .is_none()
    {
        let active = shopping_list::ActiveModel {
            id: Set(list_id),
            name: Set("This Week".to_string()),
            organization: Set(Some(ORG_PERSONAL.to_string())),
            completed_at: Set(None),
            created_by: Set(Some("cody".to_string())),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        shopping_list::Entity::insert(active).exec(db).await?;
        summary.shopping_lists_created += 1;

        // Pre-populate items inline by mirroring `generate_from_meal_plan`
        // so the seeder doesn't depend on the service layer.
        let plan_rows = meal_plan::Entity::find().all(db).await?;
        let mut next_seq: u32 = 1;
        for entry in plan_rows {
            let Some(rid) = entry.recipe_id else { continue };
            let ingredients = recipe_ingredient::Entity::find()
                .filter(recipe_ingredient::Column::RecipeId.eq(rid))
                .all(db)
                .await?;
            for ing in ingredients {
                if ing.is_section {
                    continue;
                }
                let item_id = demo_id(&format!("{}:item:{}:{}", list_key, entry.id, ing.id));
                let item_active = shopping_list::ItemActiveModel {
                    id: Set(item_id),
                    list_id: Set(list_id),
                    sequence: Set(next_seq),
                    quantity: Set(ing.quantity),
                    unit: Set(ing.unit.clone()),
                    food: Set(ing.food.clone()),
                    note: Set(ing.note.clone()),
                    recipe_id: Set(Some(rid)),
                    meal_plan_id: Set(Some(entry.id)),
                    checked: Set(false),
                    label: Set(None),
                    created_at: Set(now),
                    updated_at: Set(now),
                };
                shopping_list::ItemEntity::insert(item_active)
                    .exec(db)
                    .await?;
                summary.shopping_list_items_created += 1;
                next_seq += 1;
            }
        }
    } else {
        summary.shopping_lists_unchanged += 1;
        // Count existing items so re-runs report unchanged correctly.
        let existing_items = shopping_list::ItemEntity::find()
            .filter(shopping_list::ItemColumn::ListId.eq(list_id))
            .all(db)
            .await?;
        summary.shopping_list_items_unchanged += existing_items.len();
    }

    Ok(())
}

/// One row of the Food catalog seed table.
struct FoodFixture {
    key: &'static str,
    name: &'static str,
    aliases: &'static [&'static str],
    category: &'static str,
    default_unit: Option<&'static str>,
    /// (kcal, protein_g, carbs_g, fat_g) per 100g — None when we don't
    /// have a confident value (e.g. "lemon" varies). Other macros stay
    /// `None` for the demo seed; the OpenFoodFacts bead fills them.
    nutrition: Option<(f64, f64, f64, f64)>,
}

const FOOD_FIXTURES: &[FoodFixture] = &[
    // Pantry staples
    FoodFixture {
        key: "food:olive-oil",
        name: "olive oil",
        aliases: &["evoo", "extra virgin olive oil"],
        category: "pantry-staple",
        default_unit: Some("tbsp"),
        nutrition: Some((884.0, 0.0, 0.0, 100.0)),
    },
    FoodFixture {
        key: "food:kosher-salt",
        name: "kosher salt",
        aliases: &["salt"],
        category: "pantry-staple",
        default_unit: Some("tsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:black-pepper",
        name: "black pepper",
        aliases: &["pepper", "ground black pepper"],
        category: "pantry-staple",
        default_unit: Some("tsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:all-purpose-flour",
        name: "all-purpose flour",
        aliases: &["flour", "ap flour"],
        category: "pantry-staple",
        default_unit: Some("cup"),
        nutrition: Some((364.0, 10.0, 76.0, 1.0)),
    },
    FoodFixture {
        key: "food:white-sugar",
        name: "white sugar",
        aliases: &["sugar", "granulated sugar"],
        category: "pantry-staple",
        default_unit: Some("cup"),
        nutrition: Some((387.0, 0.0, 100.0, 0.0)),
    },
    FoodFixture {
        key: "food:brown-sugar",
        name: "brown sugar",
        aliases: &[],
        category: "pantry-staple",
        default_unit: Some("cup"),
        nutrition: Some((380.0, 0.1, 98.0, 0.0)),
    },
    FoodFixture {
        key: "food:baking-powder",
        name: "baking powder",
        aliases: &[],
        category: "pantry-staple",
        default_unit: Some("tsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:baking-soda",
        name: "baking soda",
        aliases: &[],
        category: "pantry-staple",
        default_unit: Some("tsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:garlic-powder",
        name: "garlic powder",
        aliases: &[],
        category: "spices",
        default_unit: Some("tsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:onion-powder",
        name: "onion powder",
        aliases: &[],
        category: "spices",
        default_unit: Some("tsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:smoked-paprika",
        name: "smoked paprika",
        aliases: &["paprika"],
        category: "spices",
        default_unit: Some("tsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:ground-cumin",
        name: "ground cumin",
        aliases: &["cumin"],
        category: "spices",
        default_unit: Some("tsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:dried-oregano",
        name: "dried oregano",
        aliases: &["oregano"],
        category: "spices",
        default_unit: Some("tsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:soy-sauce",
        name: "soy sauce",
        aliases: &[],
        category: "pantry-staple",
        default_unit: Some("tbsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:white-rice",
        name: "white rice",
        aliases: &["rice"],
        category: "pantry-staple",
        default_unit: Some("cup"),
        nutrition: Some((130.0, 2.7, 28.0, 0.3)),
    },
    FoodFixture {
        key: "food:dried-pasta",
        name: "dried pasta",
        aliases: &["pasta"],
        category: "pantry-staple",
        default_unit: Some("oz"),
        nutrition: Some((371.0, 13.0, 75.0, 1.5)),
    },
    FoodFixture {
        key: "food:canned-tomatoes",
        name: "canned tomatoes",
        aliases: &["crushed tomatoes", "canned crushed tomatoes"],
        category: "pantry-staple",
        default_unit: Some("can"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:canned-coconut-milk",
        name: "coconut milk",
        aliases: &["canned coconut milk"],
        category: "pantry-staple",
        default_unit: Some("can"),
        nutrition: Some((230.0, 2.3, 6.0, 24.0)),
    },
    FoodFixture {
        key: "food:spaghetti",
        name: "spaghetti",
        aliases: &[],
        category: "pantry-staple",
        default_unit: Some("oz"),
        nutrition: Some((371.0, 13.0, 75.0, 1.5)),
    },
    FoodFixture {
        key: "food:american-cheese",
        name: "american cheese",
        aliases: &[],
        category: "dairy",
        default_unit: Some("slice"),
        nutrition: Some((375.0, 18.0, 7.3, 31.0)),
    },
    // Produce
    FoodFixture {
        key: "food:yellow-onion",
        name: "yellow onion",
        aliases: &["onion"],
        category: "produce",
        default_unit: Some("piece"),
        nutrition: Some((40.0, 1.1, 9.3, 0.1)),
    },
    FoodFixture {
        key: "food:red-onion",
        name: "red onion",
        aliases: &[],
        category: "produce",
        default_unit: Some("piece"),
        nutrition: Some((40.0, 1.1, 9.3, 0.1)),
    },
    FoodFixture {
        key: "food:garlic",
        name: "garlic",
        aliases: &["garlic clove", "clove of garlic"],
        category: "produce",
        default_unit: Some("clove"),
        nutrition: Some((149.0, 6.4, 33.0, 0.5)),
    },
    FoodFixture {
        key: "food:fresh-ginger",
        name: "fresh ginger",
        aliases: &["ginger"],
        category: "produce",
        default_unit: Some("tbsp"),
        nutrition: Some((80.0, 1.8, 18.0, 0.8)),
    },
    FoodFixture {
        key: "food:lemon",
        name: "lemon",
        aliases: &[],
        category: "produce",
        default_unit: Some("piece"),
        nutrition: Some((29.0, 1.1, 9.0, 0.3)),
    },
    FoodFixture {
        key: "food:lime",
        name: "lime",
        aliases: &[],
        category: "produce",
        default_unit: Some("piece"),
        nutrition: Some((30.0, 0.7, 11.0, 0.2)),
    },
    FoodFixture {
        key: "food:tomato",
        name: "tomato",
        aliases: &["tomatoes"],
        category: "produce",
        default_unit: Some("piece"),
        nutrition: Some((18.0, 0.9, 3.9, 0.2)),
    },
    FoodFixture {
        key: "food:cucumber",
        name: "cucumber",
        aliases: &["english cucumber"],
        category: "produce",
        default_unit: Some("piece"),
        nutrition: Some((15.0, 0.7, 3.6, 0.1)),
    },
    FoodFixture {
        key: "food:kalamata-olives",
        name: "kalamata olives",
        aliases: &["olives"],
        category: "produce",
        default_unit: Some("cup"),
        nutrition: Some((115.0, 0.8, 6.3, 11.0)),
    },
    FoodFixture {
        key: "food:broccoli",
        name: "broccoli",
        aliases: &[],
        category: "produce",
        default_unit: Some("head"),
        nutrition: Some((34.0, 2.8, 7.0, 0.4)),
    },
    FoodFixture {
        key: "food:sweet-potato",
        name: "sweet potato",
        aliases: &["sweet potatoes"],
        category: "produce",
        default_unit: Some("piece"),
        nutrition: Some((86.0, 1.6, 20.0, 0.1)),
    },
    FoodFixture {
        key: "food:banana",
        name: "banana",
        aliases: &["bananas", "ripe bananas"],
        category: "produce",
        default_unit: Some("piece"),
        nutrition: Some((89.0, 1.1, 23.0, 0.3)),
    },
    // Dairy/Eggs
    FoodFixture {
        key: "food:butter",
        name: "butter",
        aliases: &[],
        category: "dairy",
        default_unit: Some("tbsp"),
        nutrition: Some((717.0, 0.9, 0.1, 81.0)),
    },
    FoodFixture {
        key: "food:eggs",
        name: "eggs",
        aliases: &["egg", "whole egg", "egg yolks", "egg yolk"],
        category: "dairy",
        default_unit: Some("piece"),
        nutrition: Some((155.0, 13.0, 1.0, 11.0)),
    },
    FoodFixture {
        key: "food:whole-milk",
        name: "whole milk",
        aliases: &["milk"],
        category: "dairy",
        default_unit: Some("cup"),
        nutrition: Some((61.0, 3.2, 4.8, 3.3)),
    },
    FoodFixture {
        key: "food:feta-cheese",
        name: "feta cheese",
        aliases: &["feta"],
        category: "dairy",
        default_unit: Some("oz"),
        nutrition: Some((264.0, 14.0, 4.1, 21.0)),
    },
    FoodFixture {
        key: "food:pecorino-romano",
        name: "pecorino romano",
        aliases: &["pecorino"],
        category: "dairy",
        default_unit: Some("cup"),
        nutrition: Some((387.0, 32.0, 0.0, 27.0)),
    },
    FoodFixture {
        key: "food:brioche-bun",
        name: "brioche bun",
        aliases: &["brioche buns"],
        category: "bakery",
        default_unit: Some("piece"),
        nutrition: Some((315.0, 9.0, 50.0, 8.0)),
    },
    // Protein
    FoodFixture {
        key: "food:chicken-thigh",
        name: "chicken thigh",
        aliases: &["chicken thighs"],
        category: "meat",
        default_unit: Some("lb"),
        nutrition: Some((209.0, 26.0, 0.0, 11.0)),
    },
    FoodFixture {
        key: "food:ground-beef",
        name: "ground beef",
        aliases: &["ground beef (80/20)"],
        category: "meat",
        default_unit: Some("oz"),
        nutrition: Some((254.0, 17.0, 0.0, 20.0)),
    },
    FoodFixture {
        key: "food:guanciale",
        name: "guanciale",
        aliases: &[],
        category: "meat",
        default_unit: Some("oz"),
        nutrition: Some((650.0, 7.0, 0.0, 69.0)),
    },
    FoodFixture {
        key: "food:chickpeas-canned",
        name: "chickpeas (canned)",
        aliases: &["chickpeas", "cans chickpeas", "garbanzo beans"],
        category: "pantry-staple",
        default_unit: Some("can"),
        nutrition: Some((164.0, 8.9, 27.0, 2.6)),
    },
    // Spices/Misc
    FoodFixture {
        key: "food:ground-cinnamon",
        name: "ground cinnamon",
        aliases: &["cinnamon"],
        category: "spices",
        default_unit: Some("tsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:garam-masala",
        name: "garam masala",
        aliases: &[],
        category: "spices",
        default_unit: Some("tsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:dried-thyme",
        name: "dried thyme",
        aliases: &["thyme"],
        category: "spices",
        default_unit: Some("tsp"),
        nutrition: None,
    },
    FoodFixture {
        key: "food:dried-rosemary",
        name: "dried rosemary",
        aliases: &["rosemary"],
        category: "spices",
        default_unit: Some("tsp"),
        nutrition: None,
    },
];

async fn seed_foods(db: &DatabaseConnection, summary: &mut DemoSeedSummary) -> Result<(), DbErr> {
    let now = Utc::now();
    for fix in FOOD_FIXTURES {
        let id = demo_id(fix.key);
        if food::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.foods_unchanged += 1;
            continue;
        }
        let nutrition = fix
            .nutrition
            .map(|(kcal, protein, carbs, fat)| {
                serde_json::json!({
                    "kcal_per_100g": kcal,
                    "protein_g": protein,
                    "carbs_g": carbs,
                    "fat_g": fat,
                    "source": "manual",
                })
            })
            .map_or(JsonObject::default(), JsonObject::from_value);
        let aliases = FoodAliasList::from(
            fix.aliases
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>(),
        );
        let active = food::ActiveModel {
            id: Set(id),
            name: Set(fix.name.to_string()),
            aliases: Set(aliases),
            category: Set(Some(fix.category.to_string())),
            default_unit: Set(fix.default_unit.map(|s| s.to_string())),
            organization: Set(Some(ORG_PERSONAL.to_string())),
            nutrition_per_100g: Set(nutrition),
            notes: Set(None),
            properties: Set(JsonObject::default()),
            created_by: Set(Some("cody".to_string())),
            created_at: Set(now),
            updated_at: Set(now),
        };
        food::Entity::insert(active).exec(db).await?;
        summary.foods_created += 1;
    }
    Ok(())
}

async fn seed_food_products(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    /// Demo product fixture. Two of these (`bertolli-evoo-500ml` and
    /// the existing manual rows) are barcode-less for the pantry bead;
    /// the others ship realistic Open Food Facts barcodes so the
    /// barcode-lookup CLI surface has something to exercise.
    struct ProductFixture {
        key: &'static str,
        food_key: &'static str,
        barcode: Option<&'static str>,
        brand: Option<&'static str>,
        name: &'static str,
        size_g: Option<f64>,
        size_label: Option<&'static str>,
        source: &'static str,
        /// Pre-populated NutritionFacts in JSON form. Empty string =
        /// no nutrition data on this row (matches the existing manual
        /// fixtures' behavior).
        nutrition_json: &'static str,
    }
    let products = [
        ProductFixture {
            key: "food_product:tj-evoo-500ml",
            food_key: "food:olive-oil",
            barcode: None,
            brand: Some("Trader Joe's"),
            name: "Trader Joe's Extra Virgin Olive Oil 500ml",
            size_g: Some(500.0),
            size_label: Some("500ml"),
            source: "manual",
            nutrition_json: "",
        },
        ProductFixture {
            key: "food_product:vital-farms-eggs-dozen",
            food_key: "food:eggs",
            barcode: None,
            brand: Some("Vital Farms"),
            name: "Vital Farms Pasture-Raised Eggs Dozen",
            size_g: None,
            size_label: Some("dozen"),
            source: "manual",
            nutrition_json: "",
        },
        ProductFixture {
            key: "food_product:generic-chickpeas-15oz",
            food_key: "food:chickpeas-canned",
            barcode: None,
            brand: Some("Generic"),
            name: "Generic Canned Chickpeas 15oz",
            size_g: Some(425.0),
            size_label: Some("15oz"),
            source: "manual",
            nutrition_json: "",
        },
        // Real-world Open Food Facts hit; nutrition copied from a
        // captured response so we don't need a network call at seed
        // time.
        ProductFixture {
            key: "food_product:bertolli-evoo-500ml",
            food_key: "food:olive-oil",
            barcode: Some("0048500201497"),
            brand: Some("Bertolli"),
            name: "Bertolli Extra Virgin Olive Oil",
            size_g: Some(500.0),
            size_label: Some("500 ml"),
            source: "openfoodfacts",
            nutrition_json: r#"{"kcal_per_100g":884.0,"protein_g":0.0,"carbs_g":0.0,"sugars_g":0.0,"fiber_g":0.0,"fat_g":100.0,"saturated_fat_g":14.0,"sodium_mg":0.0,"source":"openfoodfacts","notes":null}"#,
        },
        // Useful pantry stock fixture; intentionally barcode-less so
        // the pantry bead has a "loose item" to play with.
        ProductFixture {
            key: "food_product:demo-dozen-eggs",
            food_key: "food:eggs",
            barcode: None,
            brand: None,
            name: "Demo Dozen Eggs",
            size_g: None,
            size_label: Some("dozen"),
            source: "manual",
            nutrition_json: "",
        },
    ];
    for p in products {
        let id = demo_id(p.key);
        if food_product::Entity::find_by_id(id)
            .one(db)
            .await?
            .is_some()
        {
            summary.food_products_unchanged += 1;
            continue;
        }
        let nutrition = if p.nutrition_json.trim().is_empty() {
            JsonObject::default()
        } else {
            match serde_json::from_str::<serde_json::Value>(p.nutrition_json) {
                Ok(v) => JsonObject::from_value(v),
                Err(_) => JsonObject::default(),
            }
        };
        let last_synced = if p.source == "openfoodfacts" {
            Some(now)
        } else {
            None
        };
        let active = food_product::ActiveModel {
            id: Set(id),
            food_id: Set(demo_id(p.food_key)),
            barcode: Set(p.barcode.map(|s| s.to_string())),
            brand: Set(p.brand.map(|s| s.to_string())),
            name: Set(p.name.to_string()),
            package_size_g: Set(p.size_g),
            package_size_label: Set(p.size_label.map(|s| s.to_string())),
            source: Set(p.source.to_string()),
            external_id: Set(p.barcode.map(|s| s.to_string())),
            nutrition_per_100g: Set(nutrition),
            image_url: Set(None),
            last_synced_at: Set(last_synced),
            organization: Set(Some(ORG_PERSONAL.to_string())),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        food_product::Entity::insert(active).exec(db).await?;
        summary.food_products_created += 1;
    }
    Ok(())
}

async fn seed_locations(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let locations: &[(&str, &str)] = &[
        ("location:pantry-shelf", "Pantry Shelf"),
        ("location:refrigerator", "Refrigerator"),
        ("location:freezer", "Freezer"),
    ];
    for (key, name) in locations {
        let id = demo_id(key);
        if location::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.locations_unchanged += 1;
            continue;
        }
        let active = location::ActiveModel {
            uuid: Set(id),
            id: Set(Some(id.to_string())),
            name: Set((*name).to_string()),
            address1: Set(None),
            address2: Set(None),
            city: Set(None),
            state: Set(None),
            postal_code: Set(None),
            country_code: Set(None),
            contact_name: Set(None),
            contact_email: Set(None),
            contact_phone: Set(None),
            access_notes: Set(None),
            parking_load_in: Set(None),
            network_power: Set(None),
            // `pantry-storage` distinguishes these from real venues; the
            // pantry surfaces filter on this string.
            venue_type: Set(Some("pantry-storage".to_string())),
            default_files: Set(VenueDefaultList::default()),
            spaces: Set(SpaceList::default()),
            tags: Set(LocationTagList::default()),
            properties: Set(JsonObject::default()),
            date_created: Set(Some(now)),
            date_modified: Set(Some(now)),
            deleted_at: Set(None),
            body: Set(String::new()),
        };
        location::Entity::insert(active).exec(db).await?;
        summary.locations_created += 1;
    }
    Ok(())
}

async fn seed_pantry(db: &DatabaseConnection, summary: &mut DemoSeedSummary) -> Result<(), DbErr> {
    let now = Utc::now();
    let today = now.date_naive();
    let pantry_shelf = demo_id("location:pantry-shelf");
    let fridge = demo_id("location:refrigerator");
    let freezer = demo_id("location:freezer");

    struct Fix {
        key: &'static str,
        food_key: &'static str,
        product_key: Option<&'static str>,
        location_id: Uuid,
        quantity: f64,
        unit: &'static str,
        expiration_offset_days: Option<i64>,
        min_stock: Option<f64>,
    }
    let pantry_shelf_id = pantry_shelf;
    let fridge_id = fridge;
    let freezer_id = freezer;
    let fixtures: &[Fix] = &[
        Fix {
            key: "pantry:olive-oil-shelf",
            food_key: "food:olive-oil",
            product_key: Some("food_product:bertolli-evoo-500ml"),
            location_id: pantry_shelf_id,
            quantity: 500.0,
            unit: "ml",
            expiration_offset_days: Some(365),
            min_stock: Some(100.0),
        },
        Fix {
            key: "pantry:kosher-salt-shelf",
            food_key: "food:kosher-salt",
            product_key: None,
            location_id: pantry_shelf_id,
            quantity: 1.0,
            unit: "kg",
            expiration_offset_days: None,
            min_stock: Some(0.2),
        },
        Fix {
            key: "pantry:flour-shelf",
            food_key: "food:all-purpose-flour",
            product_key: None,
            location_id: pantry_shelf_id,
            quantity: 2.0,
            unit: "kg",
            expiration_offset_days: Some(180),
            min_stock: Some(0.5),
        },
        Fix {
            key: "pantry:sugar-shelf",
            food_key: "food:white-sugar",
            product_key: None,
            location_id: pantry_shelf_id,
            quantity: 1.5,
            unit: "kg",
            expiration_offset_days: None,
            min_stock: Some(0.5),
        },
        Fix {
            key: "pantry:garlic-powder-shelf",
            food_key: "food:garlic-powder",
            product_key: None,
            location_id: pantry_shelf_id,
            quantity: 80.0,
            unit: "g",
            expiration_offset_days: Some(540),
            min_stock: Some(20.0),
        },
        Fix {
            key: "pantry:paprika-shelf",
            food_key: "food:smoked-paprika",
            product_key: None,
            location_id: pantry_shelf_id,
            quantity: 60.0,
            unit: "g",
            expiration_offset_days: Some(540),
            min_stock: Some(20.0),
        },
        // Low-stock fixture: quantity <= min_stock so the low-stock
        // report has something to surface.
        Fix {
            key: "pantry:soy-sauce-shelf",
            food_key: "food:soy-sauce",
            product_key: None,
            location_id: pantry_shelf_id,
            quantity: 50.0,
            unit: "ml",
            expiration_offset_days: Some(365),
            min_stock: Some(100.0),
        },
        // Expiring-soon fixture (5 days).
        Fix {
            key: "pantry:eggs-fridge",
            food_key: "food:eggs",
            product_key: Some("food_product:vital-farms-eggs-dozen"),
            location_id: fridge_id,
            quantity: 12.0,
            unit: "piece",
            expiration_offset_days: Some(5),
            min_stock: Some(4.0),
        },
        Fix {
            key: "pantry:butter-fridge",
            food_key: "food:butter",
            product_key: None,
            location_id: fridge_id,
            quantity: 454.0,
            unit: "g",
            expiration_offset_days: Some(45),
            min_stock: Some(100.0),
        },
        Fix {
            key: "pantry:feta-fridge",
            food_key: "food:feta-cheese",
            product_key: None,
            location_id: fridge_id,
            quantity: 200.0,
            unit: "g",
            expiration_offset_days: Some(20),
            min_stock: None,
        },
        Fix {
            key: "pantry:milk-fridge",
            food_key: "food:whole-milk",
            product_key: None,
            location_id: fridge_id,
            quantity: 1.0,
            unit: "l",
            expiration_offset_days: Some(10),
            min_stock: Some(0.5),
        },
        Fix {
            key: "pantry:chicken-thighs-freezer",
            food_key: "food:chicken-thigh",
            product_key: None,
            location_id: freezer_id,
            quantity: 1.0,
            unit: "kg",
            expiration_offset_days: Some(60),
            min_stock: Some(0.5),
        },
        Fix {
            key: "pantry:ground-beef-freezer",
            food_key: "food:ground-beef",
            product_key: None,
            location_id: freezer_id,
            quantity: 0.5,
            unit: "kg",
            expiration_offset_days: Some(45),
            min_stock: Some(0.5),
        },
    ];

    for f in fixtures {
        let id = demo_id(f.key);
        if pantry::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.pantry_items_unchanged += 1;
            continue;
        }
        let expiration = f.expiration_offset_days.map(|d| today + Duration::days(d));
        let active = pantry::ActiveModel {
            id: Set(id),
            food_id: Set(Some(demo_id(f.food_key))),
            product_id: Set(f.product_key.map(demo_id)),
            location_id: Set(Some(f.location_id)),
            quantity: Set(f.quantity),
            unit: Set(f.unit.to_string()),
            expiration_date: Set(expiration),
            opened_at: Set(None),
            min_stock: Set(f.min_stock),
            purchased_at: Set(Some(today - Duration::days(2))),
            notes: Set(None),
            organization: Set(Some(ORG_PERSONAL.to_string())),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        pantry::Entity::insert(active).exec(db).await?;
        summary.pantry_items_created += 1;
    }
    Ok(())
}

/// Backfill `food_id` on `recipe_ingredient` rows whose value is
/// currently NULL. Idempotent — pre-populated rows are left alone.
async fn backfill_recipe_ingredient_food_ids(db: &DatabaseConnection) -> Result<(), DbErr> {
    use task_core::recipe;
    let rows = recipe_ingredient::Entity::find()
        .filter(recipe_ingredient::Column::FoodId.is_null())
        .all(db)
        .await?;
    for row in rows {
        if row.is_section || row.food.trim().is_empty() {
            continue;
        }
        // Look up the recipe's organization scope.
        let org = recipe::Entity::find_by_id(row.recipe_id)
            .one(db)
            .await?
            .and_then(|r| r.organization);
        let Some(food_row) =
            task_core::food::find_food_by_name(db, org.as_deref(), &row.food).await?
        else {
            continue;
        };
        let mut active: recipe_ingredient::ActiveModel = row.into();
        active.food_id = sea_orm::ActiveValue::Set(Some(food_row.id));
        active.update(db).await?;
    }
    Ok(())
}

/// Recompute `nutrition_summary` for every demo recipe so the cached
/// blob is populated. Idempotent.
async fn recompute_demo_recipe_nutrition(db: &DatabaseConnection) -> Result<(), DbErr> {
    use task_core::nutrition::{
        IngredientNutritionInput, NutritionFacts, aggregate_recipe_nutrition,
    };
    for key in RECIPE_KEYS {
        let rid = demo_id(key);
        let Some(rec) = recipe::Entity::find_by_id(rid).one(db).await? else {
            continue;
        };
        let ings = recipe_ingredient::Entity::find()
            .filter(recipe_ingredient::Column::RecipeId.eq(rid))
            .order_by_asc(recipe_ingredient::Column::Sequence)
            .all(db)
            .await?;
        let mut inputs: Vec<IngredientNutritionInput> = Vec::new();
        for ing in ings {
            if ing.is_section {
                continue;
            }
            let nutrition = match ing.food_id {
                Some(fid) => food::Entity::find_by_id(fid)
                    .one(db)
                    .await?
                    .map(|f| NutritionFacts::from_json_object(&f.nutrition_per_100g)),
                None => None,
            };
            inputs.push(IngredientNutritionInput {
                food_id: ing.food_id,
                food_name: ing.food.clone(),
                quantity: ing.quantity,
                unit: ing.unit.clone(),
                nutrition_per_100g: nutrition,
            });
        }
        let aggregated = aggregate_recipe_nutrition(rec.servings, &inputs);
        let summary_value = serde_json::json!({
            "total": aggregated.total,
            "per_serving": aggregated.per_serving,
            "warnings": aggregated.warnings,
        });
        let mut active: recipe::ActiveModel = rec.into();
        active.nutrition_summary = sea_orm::ActiveValue::Set(JsonObject::from_value(summary_value));
        active.updated_at = sea_orm::ActiveValue::Set(Utc::now());
        active.update(db).await?;
    }
    Ok(())
}

/// Seed ~14 deterministic FoodLog rows spanning today and the previous
/// 6 days so daily/weekly aggregates have data on first boot.
async fn seed_food_logs(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    use task_core::nutrition::NutritionFacts;
    let today = chrono::Local::now().date_naive();
    let now = Utc::now();
    let org = ORG_PERSONAL;

    /// (key, day_offset_back, meal, food_name, qty_g, recipe_key, snapshot_kcal)
    struct Fixture {
        key: &'static str,
        day_back: i64,
        meal: MealType,
        food_name: &'static str,
        quantity_grams: f64,
        recipe_key: Option<&'static str>,
        meal_plan_key: Option<&'static str>,
        // When `Some`, used directly. When `None`, looked up from Food.
        kcal: Option<f64>,
        protein: Option<f64>,
        carbs: Option<f64>,
        fat: Option<f64>,
    }
    let fixtures: &[Fixture] = &[
        // Today (day 0)
        Fixture {
            key: "food_log:day0-breakfast-eggs",
            day_back: 0,
            meal: MealType::Breakfast,
            food_name: "eggs",
            quantity_grams: 100.0,
            recipe_key: None,
            meal_plan_key: None,
            kcal: Some(155.0),
            protein: Some(13.0),
            carbs: Some(1.1),
            fat: Some(11.0),
        },
        Fixture {
            key: "food_log:day0-lunch-greek-salad",
            day_back: 0,
            meal: MealType::Lunch,
            food_name: "Greek Salad",
            quantity_grams: 0.0,
            recipe_key: Some("recipe:greek-salad"),
            meal_plan_key: Some("meal:day0-lunch"),
            kcal: Some(420.0),
            protein: Some(10.0),
            carbs: Some(20.0),
            fat: Some(30.0),
        },
        Fixture {
            key: "food_log:day0-dinner-carbonara",
            day_back: 0,
            meal: MealType::Dinner,
            food_name: "Carbonara",
            quantity_grams: 0.0,
            recipe_key: Some("recipe:carbonara"),
            meal_plan_key: Some("meal:day0-dinner"),
            kcal: Some(720.0),
            protein: Some(28.0),
            carbs: Some(75.0),
            fat: Some(32.0),
        },
        Fixture {
            key: "food_log:day0-snack-yogurt",
            day_back: 0,
            meal: MealType::Snack,
            food_name: "Greek yogurt",
            quantity_grams: 170.0,
            recipe_key: None,
            meal_plan_key: None,
            kcal: Some(100.0),
            protein: Some(17.0),
            carbs: Some(6.0),
            fat: Some(0.5),
        },
        // Yesterday (day 1)
        Fixture {
            key: "food_log:day1-breakfast-banana",
            day_back: 1,
            meal: MealType::Breakfast,
            food_name: "banana",
            quantity_grams: 120.0,
            recipe_key: None,
            meal_plan_key: None,
            kcal: Some(105.0),
            protein: Some(1.3),
            carbs: Some(27.0),
            fat: Some(0.4),
        },
        Fixture {
            key: "food_log:day1-lunch-sheetpan",
            day_back: 1,
            meal: MealType::Lunch,
            food_name: "Sheet-pan Chicken",
            quantity_grams: 0.0,
            recipe_key: Some("recipe:sheet-pan-chicken"),
            meal_plan_key: None,
            kcal: Some(550.0),
            protein: Some(40.0),
            carbs: Some(30.0),
            fat: Some(28.0),
        },
        Fixture {
            key: "food_log:day1-snack-apple",
            day_back: 1,
            meal: MealType::Snack,
            food_name: "apple",
            quantity_grams: 180.0,
            recipe_key: None,
            meal_plan_key: None,
            kcal: Some(95.0),
            protein: Some(0.5),
            carbs: Some(25.0),
            fat: Some(0.3),
        },
        // Day 2
        Fixture {
            key: "food_log:day2-dinner-chickpea",
            day_back: 2,
            meal: MealType::Dinner,
            food_name: "Chickpea Curry",
            quantity_grams: 0.0,
            recipe_key: Some("recipe:chickpea-curry"),
            meal_plan_key: Some("meal:day2-dinner"),
            kcal: Some(480.0),
            protein: Some(18.0),
            carbs: Some(60.0),
            fat: Some(18.0),
        },
        Fixture {
            key: "food_log:day2-breakfast-eggs",
            day_back: 2,
            meal: MealType::Breakfast,
            food_name: "eggs",
            quantity_grams: 150.0,
            recipe_key: None,
            meal_plan_key: None,
            kcal: Some(232.5),
            protein: Some(19.5),
            carbs: Some(1.65),
            fat: Some(16.5),
        },
        // Day 3
        Fixture {
            key: "food_log:day3-breakfast-oatmeal",
            day_back: 3,
            meal: MealType::Breakfast,
            food_name: "oatmeal",
            quantity_grams: 250.0,
            recipe_key: None,
            meal_plan_key: None,
            kcal: Some(150.0),
            protein: Some(5.0),
            carbs: Some(27.0),
            fat: Some(2.5),
        },
        Fixture {
            key: "food_log:day3-dinner-pasta",
            day_back: 3,
            meal: MealType::Dinner,
            food_name: "pasta with marinara",
            quantity_grams: 350.0,
            recipe_key: None,
            meal_plan_key: None,
            kcal: Some(520.0),
            protein: Some(15.0),
            carbs: Some(95.0),
            fat: Some(8.0),
        },
        // Day 4
        Fixture {
            key: "food_log:day4-dinner-smashburger",
            day_back: 4,
            meal: MealType::Dinner,
            food_name: "Smashburger",
            quantity_grams: 0.0,
            recipe_key: Some("recipe:smashburger"),
            meal_plan_key: Some("meal:day4-dinner"),
            kcal: Some(820.0),
            protein: Some(38.0),
            carbs: Some(45.0),
            fat: Some(52.0),
        },
        // Day 5 — leftovers (free-form, no recipe link)
        Fixture {
            key: "food_log:day5-lunch-leftovers",
            day_back: 5,
            meal: MealType::Lunch,
            food_name: "Leftover chickpea curry",
            quantity_grams: 400.0,
            recipe_key: None,
            meal_plan_key: Some("meal:day5-lunch"),
            kcal: Some(450.0),
            protein: Some(17.0),
            carbs: Some(58.0),
            fat: Some(16.0),
        },
        // Day 6 — takeout, no nutrition data
        Fixture {
            key: "food_log:day6-dinner-takeout",
            day_back: 6,
            meal: MealType::Dinner,
            food_name: "Takeout (unknown)",
            quantity_grams: 0.0,
            recipe_key: None,
            meal_plan_key: Some("meal:day6-dinner"),
            kcal: None,
            protein: None,
            carbs: None,
            fat: None,
        },
    ];

    for fix in fixtures {
        let id = demo_id(fix.key);
        if food_log::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.food_logs_unchanged += 1;
            continue;
        }
        let date = today - Duration::days(fix.day_back);
        let recipe_id = fix.recipe_key.map(demo_id);
        let meal_plan_id = fix.meal_plan_key.map(demo_id);
        let food_id = task_core::food::find_food_by_name(db, Some(org), fix.food_name)
            .await?
            .map(|f| f.id);
        // Snapshot fields: prefer fixture overrides; otherwise scale
        // from the linked Food's nutrition.
        let (kcal, protein, carbs, fat, fiber, sodium) =
            match (fix.kcal, food_id.and_then(|_| None::<NutritionFacts>)) {
                (Some(_), _) => (fix.kcal, fix.protein, fix.carbs, fix.fat, None, None),
                _ => (None, None, None, None, None, None),
            };
        let mut active = <food_log::ActiveModel as sea_orm::ActiveModelTrait>::default();
        active.id = sea_orm::ActiveValue::Set(id);
        active.date = sea_orm::ActiveValue::Set(date);
        active.meal_type = sea_orm::ActiveValue::Set(fix.meal);
        active.food_id = sea_orm::ActiveValue::Set(food_id);
        active.product_id = sea_orm::ActiveValue::Set(None);
        active.food_name = sea_orm::ActiveValue::Set(fix.food_name.to_string());
        active.quantity_grams = sea_orm::ActiveValue::Set(fix.quantity_grams);
        active.kcal = sea_orm::ActiveValue::Set(kcal);
        active.protein_g = sea_orm::ActiveValue::Set(protein);
        active.carbs_g = sea_orm::ActiveValue::Set(carbs);
        active.sugars_g = sea_orm::ActiveValue::Set(None);
        active.fiber_g = sea_orm::ActiveValue::Set(fiber);
        active.fat_g = sea_orm::ActiveValue::Set(fat);
        active.saturated_fat_g = sea_orm::ActiveValue::Set(None);
        active.sodium_mg = sea_orm::ActiveValue::Set(sodium);
        active.notes = sea_orm::ActiveValue::Set(None);
        active.created_by = sea_orm::ActiveValue::Set(Some("cody".to_string()));
        active.meal_plan_entry_id = sea_orm::ActiveValue::Set(meal_plan_id);
        active.recipe_id = sea_orm::ActiveValue::Set(recipe_id);
        active.organization = sea_orm::ActiveValue::Set(Some(org.to_string()));
        active.properties = sea_orm::ActiveValue::Set(JsonObject::default());
        active.created_at = sea_orm::ActiveValue::Set(now);
        active.updated_at = sea_orm::ActiveValue::Set(now);
        food_log::Entity::insert(active).exec(db).await?;
        summary.food_logs_created += 1;
    }
    Ok(())
}

fn slugify(value: &str) -> String {
    value
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() {
                c.to_ascii_lowercase()
            } else {
                '-'
            }
        })
        .collect::<String>()
        .trim_matches('-')
        .to_string()
}
