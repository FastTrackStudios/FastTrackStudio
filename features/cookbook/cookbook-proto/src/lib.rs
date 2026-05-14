//! `cookbook-proto` — wire contract for the `cookbook` feature.
//!
//! Eight top-level entities:
//!
//! - `Cookbook`          — a collection of recipes
//! - `Recipe`            — a recipe belonging to a cookbook
//! - `RecipeIngredient`  — a single ingredient line for a recipe
//! - `RecipeStep`        — a single instruction step for a recipe
//! - `MealPlan`          — a planned meal on a date
//! - `FoodProduct`       — a recurring ingredient / pantry SKU
//! - `PantryItem`        — what's actually in your pantry right now
//! - `ShoppingListItem`  — an item on the shopping list

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Cookbook ──────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "cookbooks", repo)]
pub struct Cookbook {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CookbookName"))]
    pub name: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::name::en::Name()"))]
    pub author: Option<String>,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub description: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ImageUrl"))]
    pub cover_image_url: Option<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CookbookTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Recipe ────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "recipes", repo)]
pub struct Recipe {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub cookbook_id: Option<Uuid>,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecipeName"))]
    pub name: String,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(5..15)")
    )]
    pub summary: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "1u32..12"))]
    pub servings: Option<u32>,

    #[cfg_attr(feature = "fake", dummy(faker = "5u32..60"))]
    pub prep_time_minutes: Option<u32>,

    #[cfg_attr(feature = "fake", dummy(faker = "10u32..120"))]
    pub cook_time_minutes: Option<u32>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "15u32..180"))]
    pub total_time_minutes: Option<u32>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Cuisine"))]
    pub cuisine: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecipeSourceUrl"))]
    pub source_url: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ImageUrl"))]
    pub image_url: Option<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecipeTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── RecipeIngredient ──────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "recipe_ingredients", repo)]
pub struct RecipeIngredient {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    pub recipe_id: Uuid,

    #[architect(fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::IngredientName"))]
    pub name: String,

    #[cfg_attr(feature = "fake", dummy(faker = "100i64..10000"))]
    pub qty_thousandths: i64,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Unit"))]
    pub unit: Option<String>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..8)")
    )]
    pub notes: Option<String>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "0i64..20"))]
    pub sort_index: i64,

    #[architect(filterable)]
    pub food_product_id: Option<Uuid>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── RecipeStep ────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "recipe_steps", repo)]
pub struct RecipeStep {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    pub recipe_id: Uuid,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "1u32..20"))]
    pub step_number: u32,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(5..15)")
    )]
    pub instruction: String,

    #[cfg_attr(feature = "fake", dummy(faker = "1u32..60"))]
    pub duration_minutes: Option<u32>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ImageUrl"))]
    pub image_url: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── MealPlan ──────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "meal_plans", repo)]
pub struct MealPlan {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub recipe_id: Option<Uuid>,

    #[architect(filterable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecipeName"))]
    pub name: String,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AroundNowDateTime"))]
    pub planned_for: DateTime<Utc>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MealType"))]
    pub meal_type: String,

    #[cfg_attr(feature = "fake", dummy(faker = "1u32..8"))]
    pub servings: Option<u32>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..8)")
    )]
    pub notes: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── FoodProduct ───────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "food_products", repo)]
pub struct FoodProduct {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FoodProductName"))]
    pub name: String,

    #[architect(filterable)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::company::en::CompanyName()")
    )]
    pub brand: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FoodCategory"))]
    pub category: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Barcode"))]
    pub barcode: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PantryUnit"))]
    pub default_unit: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "100i64..5000"))]
    pub default_qty_thousandths: Option<i64>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(2..6)")
    )]
    pub notes: Option<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PantryTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── PantryItem ────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "pantry_items", repo)]
pub struct PantryItem {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub product_id: Option<Uuid>,

    #[architect(filterable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FoodProductName"))]
    pub name: String,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "100i64..5000"))]
    pub qty_thousandths: i64,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PantryUnit"))]
    pub unit: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PantryLocation"))]
    pub location: Option<String>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FutureDateTime"))]
    pub expires_at: Option<DateTime<Utc>>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub opened_at: Option<DateTime<Utc>>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(2..6)")
    )]
    pub notes: Option<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PantryTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── ShoppingListItem ──────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "shopping_list_items", repo)]
pub struct ShoppingListItem {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub product_id: Option<Uuid>,

    #[architect(filterable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FoodProductName"))]
    pub name: String,

    #[cfg_attr(feature = "fake", dummy(faker = "100i64..5000"))]
    pub qty_thousandths: i64,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PantryUnit"))]
    pub unit: String,

    #[architect(filterable)]
    pub purchased: bool,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub purchased_at: Option<DateTime<Utc>>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "0i64..50"))]
    pub sort_index: i64,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(2..6)")
    )]
    pub notes: Option<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PantryTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── InventoryService ──────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum InventoryServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait InventoryService {
    /// Mark a shopping list item as purchased.
    async fn mark_purchased(&self, item_id: Uuid) -> Result<(), InventoryServiceError>;
}

// ── CookbookService ───────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum CookbookServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait CookbookService {
    /// Duplicate a recipe (including its ingredients and steps) into
    /// the given cookbook (or leave standalone if `None`). Returns the
    /// id of the newly created recipe.
    async fn duplicate_recipe(
        &self,
        recipe_id: Uuid,
        into_cookbook_id: Option<Uuid>,
    ) -> Result<Uuid, CookbookServiceError>;
}

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct CookbookName;
    impl Dummy<CookbookName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CookbookName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Weeknight Dinners",
                    "Sunday Brunch",
                    "Family Favorites",
                    "Holiday Classics",
                    "Quick & Easy",
                    "Plant-Based Bowls",
                    "Backyard BBQ",
                    "Comfort Food",
                    "Travel-Inspired",
                    "Baked Goods",
                ],
            )
        }
    }

    pub struct RecipeName;
    impl Dummy<RecipeName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecipeName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Garlic Roasted Chicken",
                    "Miso Glazed Salmon",
                    "Spaghetti Carbonara",
                    "Thai Green Curry",
                    "Beef Bourguignon",
                    "Margherita Pizza",
                    "Caesar Salad",
                    "Chocolate Chip Cookies",
                    "Vegetable Stir-fry",
                    "Lentil Soup",
                    "Shakshuka",
                    "Banana Bread",
                    "Risotto Milanese",
                    "Pad See Ew",
                    "Tacos al Pastor",
                ],
            )
        }
    }

    pub struct Cuisine;
    impl Dummy<Cuisine> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Cuisine, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Italian",
                    "Mexican",
                    "Thai",
                    "Japanese",
                    "French",
                    "American",
                    "Indian",
                    "Chinese",
                    "Mediterranean",
                    "Greek",
                    "Korean",
                    "Vietnamese",
                ],
            )
        }
    }

    pub struct IngredientName;
    impl Dummy<IngredientName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &IngredientName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "olive oil",
                    "garlic",
                    "yellow onion",
                    "salt",
                    "black pepper",
                    "butter",
                    "flour",
                    "sugar",
                    "eggs",
                    "milk",
                    "chicken thighs",
                    "salmon fillet",
                    "soy sauce",
                    "lemon",
                    "parsley",
                    "tomato",
                    "rice",
                    "pasta",
                    "parmesan",
                    "basil",
                ],
            )
        }
    }

    pub struct Unit;
    impl Dummy<Unit> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Unit, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "g", "kg", "ml", "l", "tsp", "tbsp", "cup", "oz", "lb", "piece", "clove",
                ],
            )
        }
    }

    pub struct MealType;
    impl Dummy<MealType> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MealType, rng: &mut R) -> Self {
            pick(rng, &["breakfast", "lunch", "dinner", "snack", "dessert"])
        }
    }

    pub struct RecipeSourceUrl;
    impl Dummy<RecipeSourceUrl> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecipeSourceUrl, rng: &mut R) -> Self {
            const HOSTS: &[&str] = &[
                "nytimes.com/recipes",
                "seriouseats.com/recipes",
                "bonappetit.com/recipe",
                "allrecipes.com/recipe",
                "foodnetwork.com/recipes",
            ];
            let host = HOSTS.choose(rng).unwrap();
            let id: u64 = rng.random_range(1000..999_999);
            format!("https://{}/{}", host, id)
        }
    }

    pub struct ImageUrl;
    impl Dummy<ImageUrl> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ImageUrl, rng: &mut R) -> Self {
            let id: u32 = rng.random_range(1..1000);
            format!("https://images.example.com/recipes/{}.jpg", id)
        }
    }

    pub struct AroundNowDateTime;
    impl Dummy<AroundNowDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AroundNowDateTime, rng: &mut R) -> Self {
            let offset_min: i64 = rng.random_range(-20_160..20_160);
            Utc::now() + Duration::minutes(offset_min)
        }
    }

    pub struct CookbookTags;
    impl Dummy<CookbookTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CookbookTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "favorites",
                "quick",
                "kid-friendly",
                "vegan",
                "gluten-free",
                "make-ahead",
                "holiday",
                "weeknight",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }

    pub struct FoodProductName;
    impl Dummy<FoodProductName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &FoodProductName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Whole Milk",
                    "Free-range Eggs",
                    "Sourdough Bread",
                    "Olive Oil",
                    "Sea Salt",
                    "Black Pepper",
                    "Garlic",
                    "Yellow Onions",
                    "Roma Tomatoes",
                    "Chicken Breast",
                    "Salmon Fillet",
                    "Ground Beef",
                    "Basmati Rice",
                    "Spaghetti",
                    "Parmesan Cheese",
                    "Greek Yogurt",
                    "Almond Butter",
                    "Bananas",
                    "Apples",
                    "Spinach",
                    "Avocados",
                    "Black Beans",
                ],
            )
        }
    }

    pub struct FoodCategory;
    impl Dummy<FoodCategory> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &FoodCategory, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "produce",
                    "dairy",
                    "meat",
                    "seafood",
                    "pantry",
                    "frozen",
                    "bakery",
                    "beverages",
                    "snacks",
                    "condiments",
                    "grains",
                ],
            )
        }
    }

    pub struct Barcode;
    impl Dummy<Barcode> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Barcode, rng: &mut R) -> Self {
            // EAN-13 (no checksum validity, but right shape).
            let n: u64 = rng.random_range(1_000_000_000_000..9_999_999_999_999);
            format!("{:013}", n)
        }
    }

    pub struct PantryUnit;
    impl Dummy<PantryUnit> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &PantryUnit, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "g", "kg", "ml", "l", "piece", "pack", "box", "can", "bottle", "bag",
                ],
            )
        }
    }

    pub struct PantryLocation;
    impl Dummy<PantryLocation> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &PantryLocation, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "fridge",
                    "freezer",
                    "pantry",
                    "spice rack",
                    "counter",
                    "cabinet",
                    "cellar",
                ],
            )
        }
    }

    pub struct RecentDateTime;
    impl Dummy<RecentDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecentDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::days(rng.random_range(0..30))
        }
    }

    pub struct FutureDateTime;
    impl Dummy<FutureDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &FutureDateTime, rng: &mut R) -> Self {
            Utc::now() + Duration::days(rng.random_range(0..120))
        }
    }

    pub struct PantryTags;
    impl Dummy<PantryTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &PantryTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "staple", "organic", "fresh", "frozen", "bulk", "snack", "sale", "low",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }

    pub struct RecipeTags;
    impl Dummy<RecipeTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecipeTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "vegetarian",
                "vegan",
                "gluten-free",
                "dairy-free",
                "low-carb",
                "high-protein",
                "one-pot",
                "30-minute",
                "comfort",
                "spicy",
                "kid-friendly",
                "make-ahead",
            ];
            let n = rng.random_range(1..=4usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
