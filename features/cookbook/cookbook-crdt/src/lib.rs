//! Loro-backed source-of-truth for the cookbook feature. Five
//! entities, five `EntityCrdt` impls, five `*RepoLoro` newtypes.

use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use cookbook_proto::{
    Cookbook, CookbookCreate, CookbookList, CookbookRepo, CookbookUpdate, MealPlan, MealPlanCreate,
    MealPlanList, MealPlanRepo, MealPlanUpdate, Recipe, RecipeCreate, RecipeIngredient,
    RecipeIngredientCreate, RecipeIngredientList, RecipeIngredientRepo, RecipeIngredientUpdate,
    RecipeList, RecipeRepo, RecipeStep, RecipeStepCreate, RecipeStepList, RecipeStepRepo,
    RecipeStepUpdate, RecipeUpdate,
};
use crdt::EntityCrdt;
use crdt::codec::{
    read_dt, read_i64, read_opt_dt, read_opt_str, read_opt_u32, read_opt_uuid, read_str,
    read_string_list, read_u32, read_uuid, write_dt, write_i64, write_opt_dt, write_opt_str,
    write_opt_string_list, write_opt_u32, write_opt_uuid, write_str, write_string_list, write_u32,
    write_uuid,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

// ── Cookbook ──────────────────────────────────────────────────────────

pub struct CookbookEntity;

#[derive(Clone)]
pub struct CookbookRepoLoro {
    inner: LoroRepo<CookbookEntity>,
}

impl CookbookRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<CookbookEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for CookbookEntity {
    type Wire = Cookbook;
    type Create = CookbookCreate;
    type Update = CookbookUpdate;
    type List = CookbookList;

    const ROOT: &'static str = "cookbooks";

    fn id(w: &Cookbook) -> Uuid {
        w.id
    }

    fn from_create(input: CookbookCreate) -> Cookbook {
        let now = Utc::now();
        Cookbook {
            id: Uuid::new_v4(),
            name: input.name,
            author: input.author,
            description: input.description,
            cover_image_url: input.cover_image_url,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Cookbook) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "author", e.author.as_deref())?;
        write_opt_str(m, "description", e.description.as_deref())?;
        write_opt_str(m, "cover_image_url", e.cover_image_url.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Cookbook, RepoError> {
        Ok(Cookbook {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            author: read_opt_str(m, "author")?,
            description: read_opt_str(m, "description")?,
            cover_image_url: read_opt_str(m, "cover_image_url")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: CookbookUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.author {
            write_opt_str(m, "author", v.as_deref())?;
        }
        if let Some(v) = u.description {
            write_opt_str(m, "description", v.as_deref())?;
        }
        if let Some(v) = u.cover_image_url {
            write_opt_str(m, "cover_image_url", v.as_deref())?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Cookbook], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<Cookbook>, total: u32, page: Page) -> CookbookList {
        CookbookList { items, total, page }
    }
}

impl CookbookRepo for CookbookRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Cookbook, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<CookbookList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: CookbookCreate) -> Result<Cookbook, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: CookbookUpdate) -> Result<Cookbook, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Recipe ────────────────────────────────────────────────────────────

pub struct RecipeEntity;

#[derive(Clone)]
pub struct RecipeRepoLoro {
    inner: LoroRepo<RecipeEntity>,
}

impl RecipeRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<RecipeEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for RecipeEntity {
    type Wire = Recipe;
    type Create = RecipeCreate;
    type Update = RecipeUpdate;
    type List = RecipeList;

    const ROOT: &'static str = "recipes";

    fn id(w: &Recipe) -> Uuid {
        w.id
    }

    fn from_create(input: RecipeCreate) -> Recipe {
        let now = Utc::now();
        Recipe {
            id: Uuid::new_v4(),
            cookbook_id: input.cookbook_id,
            name: input.name,
            summary: input.summary,
            servings: input.servings,
            prep_time_minutes: input.prep_time_minutes,
            cook_time_minutes: input.cook_time_minutes,
            total_time_minutes: input.total_time_minutes,
            cuisine: input.cuisine,
            source_url: input.source_url,
            image_url: input.image_url,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Recipe) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_opt_uuid(m, "cookbook_id", e.cookbook_id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "summary", e.summary.as_deref())?;
        write_opt_u32(m, "servings", e.servings)?;
        write_opt_u32(m, "prep_time_minutes", e.prep_time_minutes)?;
        write_opt_u32(m, "cook_time_minutes", e.cook_time_minutes)?;
        write_opt_u32(m, "total_time_minutes", e.total_time_minutes)?;
        write_opt_str(m, "cuisine", e.cuisine.as_deref())?;
        write_opt_str(m, "source_url", e.source_url.as_deref())?;
        write_opt_str(m, "image_url", e.image_url.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Recipe, RepoError> {
        Ok(Recipe {
            id: read_uuid(m, "id")?,
            cookbook_id: read_opt_uuid(m, "cookbook_id")?,
            name: read_str(m, "name")?,
            summary: read_opt_str(m, "summary")?,
            servings: read_opt_u32(m, "servings")?,
            prep_time_minutes: read_opt_u32(m, "prep_time_minutes")?,
            cook_time_minutes: read_opt_u32(m, "cook_time_minutes")?,
            total_time_minutes: read_opt_u32(m, "total_time_minutes")?,
            cuisine: read_opt_str(m, "cuisine")?,
            source_url: read_opt_str(m, "source_url")?,
            image_url: read_opt_str(m, "image_url")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: RecipeUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.cookbook_id {
            write_opt_uuid(m, "cookbook_id", v)?;
        }
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.summary {
            write_opt_str(m, "summary", v.as_deref())?;
        }
        if let Some(v) = u.servings {
            write_opt_u32(m, "servings", v)?;
        }
        if let Some(v) = u.prep_time_minutes {
            write_opt_u32(m, "prep_time_minutes", v)?;
        }
        if let Some(v) = u.cook_time_minutes {
            write_opt_u32(m, "cook_time_minutes", v)?;
        }
        if let Some(v) = u.total_time_minutes {
            write_opt_u32(m, "total_time_minutes", v)?;
        }
        if let Some(v) = u.cuisine {
            write_opt_str(m, "cuisine", v.as_deref())?;
        }
        if let Some(v) = u.source_url {
            write_opt_str(m, "source_url", v.as_deref())?;
        }
        if let Some(v) = u.image_url {
            write_opt_str(m, "image_url", v.as_deref())?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Recipe], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "total_time_minutes" => {
                items.sort_by(|a, b| a.total_time_minutes.cmp(&b.total_time_minutes))
            }
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<Recipe>, total: u32, page: Page) -> RecipeList {
        RecipeList { items, total, page }
    }
}

impl RecipeRepo for RecipeRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Recipe, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<RecipeList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: RecipeCreate) -> Result<Recipe, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: RecipeUpdate) -> Result<Recipe, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── RecipeIngredient ──────────────────────────────────────────────────

pub struct RecipeIngredientEntity;

#[derive(Clone)]
pub struct RecipeIngredientRepoLoro {
    inner: LoroRepo<RecipeIngredientEntity>,
}

impl RecipeIngredientRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<RecipeIngredientEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for RecipeIngredientEntity {
    type Wire = RecipeIngredient;
    type Create = RecipeIngredientCreate;
    type Update = RecipeIngredientUpdate;
    type List = RecipeIngredientList;

    const ROOT: &'static str = "recipe_ingredients";

    fn id(w: &RecipeIngredient) -> Uuid {
        w.id
    }

    fn from_create(input: RecipeIngredientCreate) -> RecipeIngredient {
        let now = Utc::now();
        RecipeIngredient {
            id: Uuid::new_v4(),
            recipe_id: input.recipe_id,
            name: input.name,
            qty_thousandths: input.qty_thousandths,
            unit: input.unit,
            notes: input.notes,
            sort_index: input.sort_index,
            food_product_id: input.food_product_id,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &RecipeIngredient) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "recipe_id", e.recipe_id)?;
        write_str(m, "name", &e.name)?;
        write_i64(m, "qty_thousandths", e.qty_thousandths)?;
        write_opt_str(m, "unit", e.unit.as_deref())?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_i64(m, "sort_index", e.sort_index)?;
        write_opt_uuid(m, "food_product_id", e.food_product_id)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<RecipeIngredient, RepoError> {
        Ok(RecipeIngredient {
            id: read_uuid(m, "id")?,
            recipe_id: read_uuid(m, "recipe_id")?,
            name: read_str(m, "name")?,
            qty_thousandths: read_i64(m, "qty_thousandths")?,
            unit: read_opt_str(m, "unit")?,
            notes: read_opt_str(m, "notes")?,
            sort_index: read_i64(m, "sort_index")?,
            food_product_id: read_opt_uuid(m, "food_product_id")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: RecipeIngredientUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.recipe_id {
            write_uuid(m, "recipe_id", v)?;
        }
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.qty_thousandths {
            write_i64(m, "qty_thousandths", v)?;
        }
        if let Some(v) = u.unit {
            write_opt_str(m, "unit", v.as_deref())?;
        }
        if let Some(v) = u.notes {
            write_opt_str(m, "notes", v.as_deref())?;
        }
        if let Some(v) = u.sort_index {
            write_i64(m, "sort_index", v)?;
        }
        if let Some(v) = u.food_product_id {
            write_opt_uuid(m, "food_product_id", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [RecipeIngredient],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "recipe_id" => items.sort_by(|a, b| a.recipe_id.cmp(&b.recipe_id)),
            "sort_index" => items.sort_by(|a, b| a.sort_index.cmp(&b.sort_index)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<RecipeIngredient>, total: u32, page: Page) -> RecipeIngredientList {
        RecipeIngredientList { items, total, page }
    }
}

impl RecipeIngredientRepo for RecipeIngredientRepoLoro {
    async fn get(&self, id: Uuid) -> Result<RecipeIngredient, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<RecipeIngredientList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: RecipeIngredientCreate) -> Result<RecipeIngredient, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: RecipeIngredientUpdate,
    ) -> Result<RecipeIngredient, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── RecipeStep ────────────────────────────────────────────────────────

pub struct RecipeStepEntity;

#[derive(Clone)]
pub struct RecipeStepRepoLoro {
    inner: LoroRepo<RecipeStepEntity>,
}

impl RecipeStepRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<RecipeStepEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for RecipeStepEntity {
    type Wire = RecipeStep;
    type Create = RecipeStepCreate;
    type Update = RecipeStepUpdate;
    type List = RecipeStepList;

    const ROOT: &'static str = "recipe_steps";

    fn id(w: &RecipeStep) -> Uuid {
        w.id
    }

    fn from_create(input: RecipeStepCreate) -> RecipeStep {
        let now = Utc::now();
        RecipeStep {
            id: Uuid::new_v4(),
            recipe_id: input.recipe_id,
            step_number: input.step_number,
            instruction: input.instruction,
            duration_minutes: input.duration_minutes,
            image_url: input.image_url,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &RecipeStep) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "recipe_id", e.recipe_id)?;
        write_u32(m, "step_number", e.step_number)?;
        write_str(m, "instruction", &e.instruction)?;
        write_opt_u32(m, "duration_minutes", e.duration_minutes)?;
        write_opt_str(m, "image_url", e.image_url.as_deref())?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<RecipeStep, RepoError> {
        Ok(RecipeStep {
            id: read_uuid(m, "id")?,
            recipe_id: read_uuid(m, "recipe_id")?,
            step_number: read_u32(m, "step_number")?,
            instruction: read_str(m, "instruction")?,
            duration_minutes: read_opt_u32(m, "duration_minutes")?,
            image_url: read_opt_str(m, "image_url")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: RecipeStepUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.recipe_id {
            write_uuid(m, "recipe_id", v)?;
        }
        if let Some(v) = u.step_number {
            write_u32(m, "step_number", v)?;
        }
        if let Some(v) = u.instruction {
            write_str(m, "instruction", &v)?;
        }
        if let Some(v) = u.duration_minutes {
            write_opt_u32(m, "duration_minutes", v)?;
        }
        if let Some(v) = u.image_url {
            write_opt_str(m, "image_url", v.as_deref())?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [RecipeStep],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "recipe_id" => items.sort_by(|a, b| a.recipe_id.cmp(&b.recipe_id)),
            "step_number" => items.sort_by(|a, b| a.step_number.cmp(&b.step_number)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<RecipeStep>, total: u32, page: Page) -> RecipeStepList {
        RecipeStepList { items, total, page }
    }
}

impl RecipeStepRepo for RecipeStepRepoLoro {
    async fn get(&self, id: Uuid) -> Result<RecipeStep, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<RecipeStepList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: RecipeStepCreate) -> Result<RecipeStep, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: RecipeStepUpdate) -> Result<RecipeStep, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── MealPlan ──────────────────────────────────────────────────────────

pub struct MealPlanEntity;

#[derive(Clone)]
pub struct MealPlanRepoLoro {
    inner: LoroRepo<MealPlanEntity>,
}

impl MealPlanRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<MealPlanEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for MealPlanEntity {
    type Wire = MealPlan;
    type Create = MealPlanCreate;
    type Update = MealPlanUpdate;
    type List = MealPlanList;

    const ROOT: &'static str = "meal_plans";

    fn id(w: &MealPlan) -> Uuid {
        w.id
    }

    fn from_create(input: MealPlanCreate) -> MealPlan {
        let now = Utc::now();
        MealPlan {
            id: Uuid::new_v4(),
            recipe_id: input.recipe_id,
            name: input.name,
            planned_for: input.planned_for,
            meal_type: input.meal_type,
            servings: input.servings,
            notes: input.notes,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &MealPlan) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_opt_uuid(m, "recipe_id", e.recipe_id)?;
        write_str(m, "name", &e.name)?;
        write_dt(m, "planned_for", e.planned_for)?;
        write_str(m, "meal_type", &e.meal_type)?;
        write_opt_u32(m, "servings", e.servings)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<MealPlan, RepoError> {
        Ok(MealPlan {
            id: read_uuid(m, "id")?,
            recipe_id: read_opt_uuid(m, "recipe_id")?,
            name: read_str(m, "name")?,
            planned_for: read_dt(m, "planned_for")?,
            meal_type: read_str(m, "meal_type")?,
            servings: read_opt_u32(m, "servings")?,
            notes: read_opt_str(m, "notes")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: MealPlanUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.recipe_id {
            write_opt_uuid(m, "recipe_id", v)?;
        }
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.planned_for {
            write_dt(m, "planned_for", v)?;
        }
        if let Some(v) = u.meal_type {
            write_str(m, "meal_type", &v)?;
        }
        if let Some(v) = u.servings {
            write_opt_u32(m, "servings", v)?;
        }
        if let Some(v) = u.notes {
            write_opt_str(m, "notes", v.as_deref())?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [MealPlan], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "planned_for" => items.sort_by(|a, b| a.planned_for.cmp(&b.planned_for)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<MealPlan>, total: u32, page: Page) -> MealPlanList {
        MealPlanList { items, total, page }
    }
}

impl MealPlanRepo for MealPlanRepoLoro {
    async fn get(&self, id: Uuid) -> Result<MealPlan, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<MealPlanList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: MealPlanCreate) -> Result<MealPlan, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: MealPlanUpdate) -> Result<MealPlan, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
