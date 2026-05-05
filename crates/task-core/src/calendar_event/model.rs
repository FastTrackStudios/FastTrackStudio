use chrono::{DateTime, Utc};
use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;
use uuid::Uuid;

use crate::task::{WikiLink, WikiLinkList};

/// A first-class calendar event backed by iCalendar VEVENT.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Default,
    Facet,
    DeriveEntityModel,
    EntityToModels,
    Serialize,
    Deserialize,
    ToSchema,
)]
#[sea_orm(table_name = "calendar_events")]
#[crudcrate(
    api_struct = "CalendarEventApi",
    generate_vox_service,
    name_singular = "calendar event",
    name_plural = "calendar events"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(
        primary_key,
        exclude(create),
        on_create = uuid::Uuid::new_v4()
    )]
    pub uuid: Uuid,

    #[crudcrate(filterable, sortable)]
    pub id: Option<String>,
    #[crudcrate(filterable, sortable, fulltext)]
    pub title: String,
    #[crudcrate(fulltext)]
    pub description: Option<String>,
    /// Freeform iCalendar LOCATION text (preserved for interop).
    pub location: Option<String>,
    /// Stable reference to a reusable Location/Venue record.
    pub venue: Option<WikiLink>,
    /// Optional spaces within the venue (e.g. "Studio A", "Main Stage").
    #[facet(default)]
    pub spaces: WikiLinkList,
    #[crudcrate(filterable, sortable)]
    pub start: DateTime<Utc>,
    #[crudcrate(sortable)]
    pub end: Option<DateTime<Utc>>,
    #[facet(default)]
    #[crudcrate(filterable)]
    pub all_day: bool,
    #[crudcrate(filterable, sortable)]
    pub status: CalendarEventStatus,
    pub recurrence: Option<String>,
    #[facet(default)]
    pub attendees: crate::task::StringList,
    pub date_created: Option<DateTime<Utc>>,
    pub date_modified: Option<DateTime<Utc>>,
    #[crudcrate(filterable, sortable)]
    pub external_id: Option<String>,
    #[crudcrate(filterable, sortable)]
    pub external_source: Option<String>,
    #[facet(skip)]
    #[facet(default)]
    pub body: String,
}

pub type CalendarEvent = Model;

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}

#[derive(
    Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize, ToSchema, EnumIter, DeriveActiveEnum,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(32))")]
#[repr(u8)]
pub enum CalendarEventStatus {
    #[sea_orm(string_value = "confirmed")]
    Confirmed,
    #[sea_orm(string_value = "tentative")]
    Tentative,
    #[sea_orm(string_value = "cancelled")]
    Cancelled,
}

impl Default for CalendarEventStatus {
    fn default() -> Self {
        CalendarEventStatus::Confirmed
    }
}
