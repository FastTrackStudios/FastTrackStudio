//! `BodyMeasurement` entity — point-in-time body metrics row.

use chrono::{DateTime, Utc};
use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(
    Clone,
    Debug,
    Default,
    PartialEq,
    Facet,
    DeriveEntityModel,
    EntityToModels,
    Serialize,
    Deserialize,
)]
#[sea_orm(table_name = "body_measurements")]
#[crudcrate(
    api_struct = "BodyMeasurementApi",
    generate_vox_service,
    name_singular = "body_measurement",
    name_plural = "body_measurements"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable)]
    pub measured_at: DateTime<Utc>,

    pub weight_kg: Option<f64>,
    pub body_fat_percent: Option<f32>,
    pub muscle_mass_kg: Option<f64>,
    pub water_percent: Option<f32>,

    pub neck_cm: Option<f64>,
    pub chest_cm: Option<f64>,
    pub waist_cm: Option<f64>,
    pub hip_cm: Option<f64>,
    pub left_thigh_cm: Option<f64>,
    pub right_thigh_cm: Option<f64>,
    pub left_arm_cm: Option<f64>,
    pub right_arm_cm: Option<f64>,
    pub left_calf_cm: Option<f64>,
    pub right_calf_cm: Option<f64>,

    pub resting_hr: Option<u32>,
    pub blood_pressure_systolic: Option<u32>,
    pub blood_pressure_diastolic: Option<u32>,

    pub notes: String,

    #[crudcrate(filterable)]
    pub organization: Option<String>,

    pub created_by: Option<String>,

    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub properties: crate::property::JsonObject,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: DateTime<Utc>,
    #[crudcrate(exclude(create), exclude(update), on_create = chrono::Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {
    fn new() -> Self {
        Self {
            notes: sea_orm::ActiveValue::Set(String::new()),
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

pub type BodyMeasurement = Model;
