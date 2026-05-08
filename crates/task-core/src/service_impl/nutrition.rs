//! SeaORM-backed [`NutritionService`] implementation.
//!
//! Owns a raw `DatabaseConnection` plus an optional
//! [`OpenFoodFactsClient`] for barcode-driven `log_food` resolution.
//! Macros resolved at log time are snapshotted into the `food_logs`
//! row — see the row docs for immutability rationale.

use std::sync::Arc;

use chrono::{Duration, NaiveDate, Utc};
use sea_orm::{
    ActiveModelTrait, ColumnTrait, DatabaseConnection, EntityTrait, QueryFilter, QueryOrder, Set,
};
use uuid::Uuid;

use crate::food;
use crate::food_log::{self, FoodLogApi};
use crate::food_product;
use crate::meal_plan::MealType;
use crate::nutrition::{NutritionFacts, units::to_grams_best_effort};
use crate::provider::OpenFoodFactsClient;
use crate::service::{
    DailyTotalsView, FoodLogPatch, LogFoodRequest, LogListRequest, NutritionService, VaultError,
    WeeklySummaryView,
};

use super::helpers::convert_model;

/// Typed dependencies for [`NutritionServiceImpl`].
pub struct NutritionServiceDeps {
    pub db: DatabaseConnection,
    /// Used only when `LogFoodRequest::barcode` is set and the local
    /// FoodProduct cache misses.
    pub openfoodfacts: Option<Arc<OpenFoodFactsClient>>,
}

#[derive(Clone)]
pub struct NutritionServiceImpl {
    db: DatabaseConnection,
    #[allow(dead_code)] // Reserved for barcode-resolve path; v1 uses cached FoodProduct only.
    openfoodfacts: Option<Arc<OpenFoodFactsClient>>,
}

impl NutritionServiceImpl {
    pub fn new(deps: NutritionServiceDeps) -> Self {
        Self {
            db: deps.db,
            openfoodfacts: deps.openfoodfacts,
        }
    }
}

fn io(err: impl std::fmt::Display, ctx: &str) -> VaultError {
    VaultError::IoError(format!("{ctx}: {err}"))
}

fn parse(err: impl std::fmt::Display, ctx: &str) -> VaultError {
    VaultError::ParseError(format!("{ctx}: {err}"))
}

fn log_to_api(model: food_log::Model) -> Result<FoodLogApi, VaultError> {
    convert_model::<food_log::Model, FoodLogApi>(model)
}

/// Resolve a Food + optional FoodProduct from a [`LogFoodRequest`].
async fn resolve_food(
    db: &DatabaseConnection,
    request: &LogFoodRequest,
) -> Result<(Option<food::Model>, Option<food_product::Model>, String), VaultError> {
    // Direct food_id wins.
    if let Some(fid) = request.food_id {
        let f = food::Entity::find_by_id(fid)
            .one(db)
            .await
            .map_err(|e| io(e, "load food by id"))?;
        let name = f
            .as_ref()
            .map(|x| x.name.clone())
            .or_else(|| request.food_name.clone())
            .unwrap_or_default();
        return Ok((f, None, name));
    }
    if let Some(pid) = request.product_id {
        let prod = food_product::Entity::find_by_id(pid)
            .one(db)
            .await
            .map_err(|e| io(e, "load food_product by id"))?;
        let food = match prod.as_ref().map(|p| p.food_id) {
            Some(fid) => food::Entity::find_by_id(fid)
                .one(db)
                .await
                .map_err(|e| io(e, "load food via product"))?,
            None => None,
        };
        let name = prod
            .as_ref()
            .map(|p| p.name.clone())
            .or_else(|| request.food_name.clone())
            .unwrap_or_default();
        return Ok((food, prod, name));
    }
    if let Some(barcode) = request.barcode.as_deref() {
        let mut q = food_product::Entity::find().filter(food_product::Column::Barcode.eq(barcode));
        q = match request.organization.as_deref() {
            Some(org) => q.filter(food_product::Column::Organization.eq(org)),
            None => q.filter(food_product::Column::Organization.is_null()),
        };
        let prod = q
            .one(db)
            .await
            .map_err(|e| io(e, "load food_product by barcode"))?;
        let food = match prod.as_ref().map(|p| p.food_id) {
            Some(fid) => food::Entity::find_by_id(fid)
                .one(db)
                .await
                .map_err(|e| io(e, "load food via product"))?,
            None => None,
        };
        let name = prod
            .as_ref()
            .map(|p| p.name.clone())
            .or_else(|| request.food_name.clone())
            .unwrap_or_else(|| barcode.to_string());
        return Ok((food, prod, name));
    }
    if let Some(name) = request.food_name.as_deref() {
        let hit = crate::food::find_food_by_name(db, request.organization.as_deref(), name)
            .await
            .map_err(|e| io(e, "find_food_by_name"))?;
        let resolved_name = hit
            .as_ref()
            .map(|x| x.name.clone())
            .unwrap_or_else(|| name.to_string());
        return Ok((hit, None, resolved_name));
    }
    Err(VaultError::ParseError(
        "log_food requires one of food_id / food_name / product_id / barcode".to_string(),
    ))
}

impl NutritionService for NutritionServiceImpl {
    async fn log_food(&self, request: LogFoodRequest) -> Result<FoodLogApi, VaultError> {
        let meal_type = MealType::parse(&request.meal_type)
            .ok_or_else(|| parse(&request.meal_type, "unknown meal_type"))?;

        let (food_row, product_row, food_name) = resolve_food(&self.db, &request).await?;
        // Prefer product nutrition, fall back to food.
        let nutrition: Option<NutritionFacts> = product_row
            .as_ref()
            .map(|p| NutritionFacts::from_json_object(&p.nutrition_per_100g))
            .filter(|n| !is_blank(n))
            .or_else(|| {
                food_row
                    .as_ref()
                    .map(|f| NutritionFacts::from_json_object(&f.nutrition_per_100g))
            });

        // Convert quantity to grams. Volume without density falls back
        // to water-equivalent for log convenience (matches typical
        // diet-app expectations where the user logs "1 cup milk").
        let grams =
            to_grams_best_effort(request.quantity, &request.unit, None, true).unwrap_or(0.0);
        let scaled = nutrition.as_ref().map(|n| n.scale_to_grams(grams));

        let now = Utc::now();
        let id = Uuid::new_v4();
        let mut active = <food_log::ActiveModel as sea_orm::ActiveModelTrait>::default();
        active.properties = Set(crate::property::JsonObject::default());
        active.id = Set(id);
        active.date = Set(request.date);
        active.meal_type = Set(meal_type);
        active.food_id = Set(food_row.as_ref().map(|f| f.id));
        active.product_id = Set(product_row.as_ref().map(|p| p.id));
        active.food_name = Set(food_name);
        active.quantity_grams = Set(grams);
        if let Some(s) = scaled.as_ref() {
            active.kcal = Set(s.kcal_per_100g);
            active.protein_g = Set(s.protein_g);
            active.carbs_g = Set(s.carbs_g);
            active.sugars_g = Set(s.sugars_g);
            active.fiber_g = Set(s.fiber_g);
            active.fat_g = Set(s.fat_g);
            active.saturated_fat_g = Set(s.saturated_fat_g);
            active.sodium_mg = Set(s.sodium_mg);
        }
        active.notes = Set(request.notes);
        active.created_by = Set(request.created_by);
        active.organization = Set(request.organization);
        active.created_at = Set(now);
        active.updated_at = Set(now);
        let saved = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert food_log"))?;
        log_to_api(saved)
    }

    async fn list_log(&self, request: LogListRequest) -> Result<Vec<FoodLogApi>, VaultError> {
        let mut q = food_log::Entity::find()
            .filter(food_log::Column::Date.gte(request.from_date))
            .filter(food_log::Column::Date.lte(request.to_date))
            .order_by_asc(food_log::Column::Date)
            .order_by_asc(food_log::Column::CreatedAt);
        q = match request.organization.as_deref() {
            Some(org) => q.filter(food_log::Column::Organization.eq(org)),
            None => q,
        };
        let rows = q.all(&self.db).await.map_err(|e| io(e, "list food_log"))?;
        rows.into_iter().map(log_to_api).collect()
    }

    async fn daily_totals(
        &self,
        organization: Option<String>,
        date: NaiveDate,
    ) -> Result<DailyTotalsView, VaultError> {
        let mut q = food_log::Entity::find().filter(food_log::Column::Date.eq(date));
        q = match organization.as_deref() {
            Some(org) => q.filter(food_log::Column::Organization.eq(org)),
            None => q,
        };
        let rows = q.all(&self.db).await.map_err(|e| io(e, "daily_totals"))?;
        Ok(sum_rows(date, &rows))
    }

    async fn weekly_summary(
        &self,
        organization: Option<String>,
        from_date: NaiveDate,
    ) -> Result<WeeklySummaryView, VaultError> {
        let to_date = from_date + Duration::days(6);
        let mut q = food_log::Entity::find()
            .filter(food_log::Column::Date.gte(from_date))
            .filter(food_log::Column::Date.lte(to_date));
        q = match organization.as_deref() {
            Some(org) => q.filter(food_log::Column::Organization.eq(org)),
            None => q,
        };
        let rows = q.all(&self.db).await.map_err(|e| io(e, "weekly_summary"))?;
        let mut days: Vec<DailyTotalsView> = Vec::with_capacity(7);
        for offset in 0..7 {
            let date = from_date + Duration::days(offset);
            let day_rows: Vec<&food_log::Model> = rows.iter().filter(|r| r.date == date).collect();
            days.push(sum_refs(date, &day_rows));
        }
        let count = u32::try_from(days.len()).unwrap_or(7).max(1);
        let avg_count = f64::from(count);
        let total = days.iter().fold(
            DailyTotalsView {
                date: from_date,
                ..Default::default()
            },
            |mut acc, d| {
                acc.kcal += d.kcal;
                acc.protein_g += d.protein_g;
                acc.carbs_g += d.carbs_g;
                acc.fat_g += d.fat_g;
                acc.fiber_g += d.fiber_g;
                acc.sodium_mg += d.sodium_mg;
                acc.log_count += d.log_count;
                acc
            },
        );
        let averages = DailyTotalsView {
            date: from_date,
            kcal: total.kcal / avg_count,
            protein_g: total.protein_g / avg_count,
            carbs_g: total.carbs_g / avg_count,
            fat_g: total.fat_g / avg_count,
            fiber_g: total.fiber_g / avg_count,
            sodium_mg: total.sodium_mg / avg_count,
            log_count: total.log_count / count,
        };
        Ok(WeeklySummaryView { days, averages })
    }

    async fn update_log(&self, id: Uuid, patch: FoodLogPatch) -> Result<FoodLogApi, VaultError> {
        let row = food_log::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load food_log"))?
            .ok_or_else(|| VaultError::NotFound(format!("food_log:{id}")))?;
        let mut active: food_log::ActiveModel = row.into();
        if let Some(q) = patch.quantity_grams {
            active.quantity_grams = Set(q);
        }
        if let Some(v) = patch.kcal {
            active.kcal = Set(Some(v));
        }
        if let Some(v) = patch.protein_g {
            active.protein_g = Set(Some(v));
        }
        if let Some(v) = patch.carbs_g {
            active.carbs_g = Set(Some(v));
        }
        if let Some(v) = patch.fat_g {
            active.fat_g = Set(Some(v));
        }
        if let Some(n) = patch.notes {
            active.notes = Set(Some(n));
        }
        active.updated_at = Set(Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "update food_log"))?;
        log_to_api(saved)
    }

    async fn delete_log(&self, id: Uuid) -> Result<(), VaultError> {
        food_log::Entity::delete_by_id(id)
            .exec(&self.db)
            .await
            .map_err(|e| io(e, "delete food_log"))?;
        Ok(())
    }
}

fn sum_rows(date: NaiveDate, rows: &[food_log::Model]) -> DailyTotalsView {
    sum_refs(date, &rows.iter().collect::<Vec<_>>())
}

fn sum_refs(date: NaiveDate, rows: &[&food_log::Model]) -> DailyTotalsView {
    let mut out = DailyTotalsView {
        date,
        ..Default::default()
    };
    for r in rows {
        out.kcal += r.kcal.unwrap_or(0.0);
        out.protein_g += r.protein_g.unwrap_or(0.0);
        out.carbs_g += r.carbs_g.unwrap_or(0.0);
        out.fat_g += r.fat_g.unwrap_or(0.0);
        out.fiber_g += r.fiber_g.unwrap_or(0.0);
        out.sodium_mg += r.sodium_mg.unwrap_or(0.0);
        out.log_count += 1;
    }
    out
}

fn is_blank(n: &NutritionFacts) -> bool {
    n.kcal_per_100g.is_none()
        && n.protein_g.is_none()
        && n.carbs_g.is_none()
        && n.fat_g.is_none()
        && n.fiber_g.is_none()
        && n.sodium_mg.is_none()
}
