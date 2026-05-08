//! Asset inventory and maintenance tracking.
//!
//! Assets are files-first Markdown/YAML records stored in `assets/*.md`.
//! The model is intentionally Obsidian-friendly: frontmatter carries the
//! structured inventory fields while the body remains human-readable for notes,
//! maintenance context, and repair history summaries.

use chrono::{DateTime, NaiveDate, Utc};
use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use sea_orm::sea_query::{ArrayType, ColumnType, Nullable, Value, ValueType, ValueTypeErr};
use sea_orm::{ColIdx, QueryResult, TryGetError, TryGetable};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use utoipa::ToSchema;
use uuid::Uuid;

use crate::task::WikiLink;

macro_rules! json_vec_type {
    ($name:ident, $item:ty) => {
        #[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
        #[facet(transparent)]
        #[serde(transparent)]
        pub struct $name(pub Vec<$item>);

        impl Default for $name {
            fn default() -> Self {
                Self(Vec::new())
            }
        }

        impl Deref for $name {
            type Target = Vec<$item>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }

        impl From<Vec<$item>> for $name {
            fn from(value: Vec<$item>) -> Self {
                Self(value)
            }
        }

        impl IntoIterator for $name {
            type Item = $item;
            type IntoIter = std::vec::IntoIter<$item>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.into_iter()
            }
        }

        impl<'a> IntoIterator for &'a $name {
            type Item = &'a $item;
            type IntoIter = std::slice::Iter<'a, $item>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.iter()
            }
        }

        impl<'a> IntoIterator for &'a mut $name {
            type Item = &'a mut $item;
            type IntoIter = std::slice::IterMut<'a, $item>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.iter_mut()
            }
        }

        impl From<$name> for Value {
            fn from(value: $name) -> Self {
                Value::Json(Some(Box::new(
                    serde_json::to_value(value.0).unwrap_or(serde_json::Value::Array(Vec::new())),
                )))
            }
        }

        impl Nullable for $name {
            fn null() -> Value {
                Value::Json(None)
            }
        }

        impl TryGetable for $name {
            fn try_get_by<I: ColIdx>(res: &QueryResult, idx: I) -> Result<Self, TryGetError> {
                let value: serde_json::Value = res.try_get_by(idx)?;
                let items = serde_json::from_value(value).map_err(|err| {
                    TryGetError::DbErr(sea_orm::DbErr::Type(format!(
                        "failed to deserialize JSON array: {err}"
                    )))
                })?;
                Ok(Self(items))
            }
        }

        impl ValueType for $name {
            fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
                match value {
                    Value::Json(Some(value)) => serde_json::from_value(*value)
                        .map(Self)
                        .map_err(|_| ValueTypeErr),
                    _ => Err(ValueTypeErr),
                }
            }

            fn type_name() -> String {
                stringify!($name).to_string()
            }

            fn array_type() -> ArrayType {
                ArrayType::Json
            }

            fn column_type() -> ColumnType {
                ColumnType::Json
            }
        }
    };
}

/// A tracked equipment / inventory record.
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
#[sea_orm(table_name = "assets")]
#[crudcrate(
    api_struct = "AssetApi",
    generate_vox_service,
    name_singular = "asset",
    name_plural = "assets"
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

    /// Stable id, e.g. `AST-2026-0001`.
    #[crudcrate(filterable, sortable, fulltext)]
    pub id: String,

    /// Monotonic yearly sequence number.
    #[crudcrate(sortable)]
    pub number: u32,

    /// Human-readable asset name.
    #[crudcrate(filterable, sortable, fulltext)]
    pub name: String,

    /// Lifecycle state for the asset.
    #[crudcrate(filterable, sortable)]
    pub status: AssetStatus,

    /// Manufacturer / brand.
    #[crudcrate(filterable, sortable)]
    pub manufacturer: Option<String>,

    /// Product model / series.
    pub model: Option<String>,

    /// Serial number / asset tag.
    #[crudcrate(filterable, sortable)]
    pub serial_number: Option<String>,

    /// Broad inventory category, e.g. `audio`, `video`, `network`, `lighting`.
    #[crudcrate(filterable, sortable)]
    pub category: Option<String>,

    /// Owning org / workspace.
    #[crudcrate(filterable, sortable)]
    pub organization: Option<String>,

    /// Current location (venue / building / site / room).
    pub location: Option<WikiLink>,

    /// Current space within the location (studio / room / bay / desk).
    pub space: Option<WikiLink>,

    /// Rack, case, or container assignment.
    pub rack_or_case: Option<String>,

    /// Who or what this asset is currently assigned to.
    #[crudcrate(filterable, sortable)]
    pub assigned_to: Option<String>,

    /// Purchase date.
    pub purchase_date: Option<NaiveDate>,

    /// Warranty expiry date.
    pub warranty_until: Option<NaiveDate>,

    /// Vendor / supplier.
    pub vendor: Option<String>,

    /// Cost in cents.
    #[crudcrate(sortable)]
    pub cost_cents: Option<i64>,

    /// Maintenance / repair log entries.
    #[facet(default)]
    pub maintenance: AssetMaintenanceList,

    /// Event / booking / project reservations using this asset.
    #[facet(default)]
    pub reservations: AssetReservationList,

    /// Links to task records for repair / maintenance work.
    #[facet(default)]
    pub linked_tasks: AssetWikiLinkList,

    /// Free-form notes.
    pub notes: Option<String>,

    /// Who created the asset entry.
    pub created_by: Option<String>,

    /// Obsidian-style frontmatter sidecar: flat key→value map of
    /// extension properties not modeled as first-class columns.
    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    #[serde(default)]
    pub properties: crate::property::JsonObject,

    pub date_created: Option<DateTime<Utc>>,
    pub date_modified: Option<DateTime<Utc>>,

    /// Optional generated markdown body.
    #[facet(skip)]
    #[facet(default)]
    pub body: String,
}

pub type Asset = Model;

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {
    fn new() -> Self {
        Self {
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize, ToSchema, EnumIter, DeriveActiveEnum,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(32))")]
#[repr(u8)]
#[derive(Default)]
pub enum AssetStatus {
    /// Available for use.
    #[sea_orm(string_value = "available")]
    #[default]
    Available,
    /// Currently in use.
    #[sea_orm(string_value = "in_use")]
    InUse,
    /// Reserved for a future booking / project.
    #[sea_orm(string_value = "reserved")]
    Reserved,
    /// Needs repair.
    #[sea_orm(string_value = "needs_repair")]
    NeedsRepair,
    /// Under repair / service.
    #[sea_orm(string_value = "in_repair")]
    InRepair,
    /// Maintenance is due.
    #[sea_orm(string_value = "maintenance_due")]
    MaintenanceDue,
    /// No longer active / retired.
    #[sea_orm(string_value = "retired")]
    Retired,
    /// Missing or unaccounted for.
    #[sea_orm(string_value = "lost")]
    Lost,
}

#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
pub struct AssetMaintenanceRecord {
    pub date: NaiveDate,
    pub issue: String,
    pub vendor: Option<String>,
    pub contact: Option<String>,
    pub cost_cents: Option<i64>,
    pub warranty: bool,
    pub rma: Option<String>,
    pub task: Option<WikiLink>,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
pub struct AssetReservationRecord {
    pub id: String,
    pub reference: WikiLink,
    pub starts_at: Option<DateTime<Utc>>,
    pub ends_at: Option<DateTime<Utc>>,
    pub reserved_by: Option<String>,
    pub notes: Option<String>,
}

json_vec_type!(AssetMaintenanceList, AssetMaintenanceRecord);
json_vec_type!(AssetReservationList, AssetReservationRecord);
json_vec_type!(AssetWikiLinkList, WikiLink);

#[derive(Debug, Clone, Default, Facet)]
pub struct AssetCreateRequest {
    pub name: String,
    pub status: Option<String>,
    pub manufacturer: Option<String>,
    pub model: Option<String>,
    pub serial_number: Option<String>,
    pub category: Option<String>,
    pub organization: Option<String>,
    pub location: Option<String>,
    pub space: Option<String>,
    pub rack_or_case: Option<String>,
    pub assigned_to: Option<String>,
    pub purchase_date: Option<NaiveDate>,
    pub warranty_until: Option<NaiveDate>,
    pub vendor: Option<String>,
    pub cost_cents: Option<u64>,
    pub notes: Option<String>,
    pub actor: Option<String>,
}

#[derive(Debug, Clone, Default, Facet)]
pub struct AssetPatch {
    pub name: Option<String>,
    pub status: Option<String>,
    pub manufacturer: Option<String>,
    pub model: Option<String>,
    pub serial_number: Option<String>,
    pub category: Option<String>,
    pub organization: Option<String>,
    pub location: Option<String>,
    pub space: Option<String>,
    pub rack_or_case: Option<String>,
    pub assigned_to: Option<String>,
    pub purchase_date: Option<String>,
    pub warranty_until: Option<String>,
    pub vendor: Option<String>,
    pub cost_cents: Option<Option<u64>>,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, Default, Facet)]
pub struct AssetFilter {
    pub location: Option<String>,
    pub space: Option<String>,
    pub status: Option<String>,
    pub category: Option<String>,
    pub organization: Option<String>,
    pub query: Option<String>,
    pub needs_repair_only: bool,
}

#[derive(Debug, Clone, Default, Facet)]
pub struct AssetBucket {
    pub name: String,
    pub asset_count: u32,
}

#[derive(Debug, Clone, Facet)]
pub struct AssetReport {
    pub generated_at: DateTime<Utc>,
    pub today: String,
    pub asset_count: u32,
    #[facet(default)]
    pub by_status: Vec<AssetBucket>,
    #[facet(default)]
    pub by_category: Vec<AssetBucket>,
    #[facet(default)]
    pub by_organization: Vec<AssetBucket>,
    #[facet(default)]
    pub assets: Vec<Asset>,
}

impl Default for AssetReport {
    fn default() -> Self {
        Self {
            generated_at: Utc::now(),
            today: String::new(),
            asset_count: 0,
            by_status: Vec::new(),
            by_category: Vec::new(),
            by_organization: Vec::new(),
            assets: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Default, Facet)]
pub struct AssetMaintenanceRequest {
    pub date: Option<NaiveDate>,
    pub issue: String,
    pub vendor: Option<String>,
    pub contact: Option<String>,
    pub cost_cents: Option<u64>,
    pub warranty: bool,
    pub rma: Option<String>,
    pub task: Option<String>,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, Default, Facet)]
pub struct AssetRepairRequest {
    pub title: String,
    pub notes: Option<String>,
    pub vendor: Option<String>,
    pub contact: Option<String>,
    pub cost_cents: Option<u64>,
    pub warranty: bool,
    pub rma: Option<String>,
    pub actor: Option<String>,
}

#[derive(Debug, Clone, Facet)]
pub struct AssetRepairResponse {
    pub asset: Asset,
    pub task: crate::task::Task,
}

#[derive(Debug, Clone, Default, Facet)]
pub struct AssetReserveRequest {
    pub reference: String,
    pub starts_at: Option<DateTime<Utc>>,
    pub ends_at: Option<DateTime<Utc>>,
    pub reserved_by: Option<String>,
    pub notes: Option<String>,
    pub force: bool,
}

#[derive(Debug, Clone, Default, Facet)]
pub struct AssetConflict {
    pub asset_id: String,
    pub asset_name: String,
    pub reason: String,
    pub reservation: Option<AssetReservationRecord>,
}

#[derive(Debug, Clone, Facet)]
pub struct AssetReservationResponse {
    pub asset: Asset,
    pub reservation: AssetReservationRecord,
    #[facet(default)]
    pub conflicts: Vec<AssetConflict>,
}

pub fn format_asset_id(year: i32, number: u32) -> String {
    format!("AST-{year:04}-{number:04}")
}

pub fn parse_asset_status(status: &str) -> Option<AssetStatus> {
    match status.trim().to_ascii_lowercase().as_str() {
        "available" => Some(AssetStatus::Available),
        "in-use" | "in use" | "inuse" => Some(AssetStatus::InUse),
        "reserved" => Some(AssetStatus::Reserved),
        "needs-repair" | "needs repair" | "repair-needed" => Some(AssetStatus::NeedsRepair),
        "in-repair" | "in repair" | "repairing" => Some(AssetStatus::InRepair),
        "maintenance-due" | "maintenance due" | "due" => Some(AssetStatus::MaintenanceDue),
        "retired" => Some(AssetStatus::Retired),
        "lost" => Some(AssetStatus::Lost),
        _ => None,
    }
}

pub fn render_asset_body(asset: &Asset) -> String {
    use std::fmt::Write;

    let mut out = String::new();
    let _ = writeln!(out, "# Asset {}", asset.name);
    let _ = writeln!(out);
    let _ = writeln!(out, "**ID:** {}", asset.id);
    let _ = writeln!(out, "**Status:** {:?}", asset.status);
    if let Some(manufacturer) = &asset.manufacturer {
        let _ = writeln!(out, "**Manufacturer:** {}", manufacturer);
    }
    if let Some(model) = &asset.model {
        let _ = writeln!(out, "**Model:** {}", model);
    }
    if let Some(serial_number) = &asset.serial_number {
        let _ = writeln!(out, "**Serial:** {}", serial_number);
    }
    if let Some(category) = &asset.category {
        let _ = writeln!(out, "**Category:** {}", category);
    }
    if let Some(org) = &asset.organization {
        let _ = writeln!(out, "**Organization:** {}", org);
    }
    if let Some(location) = &asset.location {
        let _ = writeln!(out, "**Location:** {}", location.0);
    }
    if let Some(space) = &asset.space {
        let _ = writeln!(out, "**Space:** {}", space.0);
    }
    if let Some(rack_or_case) = &asset.rack_or_case {
        let _ = writeln!(out, "**Rack/Case:** {}", rack_or_case);
    }
    if let Some(assigned_to) = &asset.assigned_to {
        let _ = writeln!(out, "**Assigned to:** {}", assigned_to);
    }
    if let Some(purchase_date) = &asset.purchase_date {
        let _ = writeln!(out, "**Purchase date:** {}", purchase_date);
    }
    if let Some(warranty_until) = &asset.warranty_until {
        let _ = writeln!(out, "**Warranty until:** {}", warranty_until);
    }
    if let Some(vendor) = &asset.vendor {
        let _ = writeln!(out, "**Vendor:** {}", vendor);
    }
    if let Some(cost_cents) = asset.cost_cents {
        let _ = writeln!(out, "**Cost:** ${:.2}", cost_cents as f64 / 100.0);
    }
    if !asset.linked_tasks.is_empty() {
        let tasks = asset
            .linked_tasks
            .iter()
            .map(|l| l.0.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        let _ = writeln!(out, "**Linked tasks:** {}", tasks);
    }
    if !asset.reservations.is_empty() {
        let reservations = asset
            .reservations
            .iter()
            .map(|reservation| reservation.reference.0.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        let _ = writeln!(out, "**Reservations:** {}", reservations);
    }
    let _ = writeln!(out);
    let _ = writeln!(out, "## Notes");
    let _ = writeln!(out);
    let _ = writeln!(out, "{}", asset.notes.as_deref().unwrap_or(""));
    let _ = writeln!(out);

    if !asset.maintenance.is_empty() {
        let _ = writeln!(out, "## Maintenance");
        let _ = writeln!(out);
        for entry in &asset.maintenance {
            let _ = writeln!(out, "- {} — {}", entry.date, entry.issue);
            if entry.vendor.is_some()
                || entry.contact.is_some()
                || entry.cost_cents.is_some()
                || entry.task.is_some()
            {
                let mut details = Vec::new();
                if let Some(vendor) = &entry.vendor {
                    details.push(format!("vendor: {vendor}"));
                }
                if let Some(contact) = &entry.contact {
                    details.push(format!("contact: {contact}"));
                }
                if let Some(cost_cents) = entry.cost_cents {
                    details.push(format!("cost: ${:.2}", cost_cents as f64 / 100.0));
                }
                if let Some(task) = &entry.task {
                    details.push(format!("task: {}", task.0));
                }
                if entry.warranty {
                    details.push("warranty".into());
                }
                if let Some(rma) = &entry.rma {
                    details.push(format!("RMA: {rma}"));
                }
                if !details.is_empty() {
                    let _ = writeln!(out, "  - {}", details.join(", "));
                }
            }
            if let Some(notes) = &entry.notes {
                let _ = writeln!(out, "  - notes: {}", notes);
            }
        }
        let _ = writeln!(out);
    }

    out
}

pub fn matches_asset_filter(asset: &Asset, filter: &AssetFilter) -> bool {
    if filter.needs_repair_only {
        let matches = matches!(
            asset.status,
            AssetStatus::NeedsRepair | AssetStatus::InRepair | AssetStatus::MaintenanceDue
        );
        if !matches {
            return false;
        }
    }
    if filter.status.as_deref().is_some_and(|wanted| {
        parse_asset_status(wanted).is_none_or(|parsed| parsed != asset.status)
    }) {
        return false;
    }
    if filter
        .location
        .as_deref()
        .is_some_and(|wanted| asset.location.as_ref().map(|l| l.0.as_str()) != Some(wanted))
    {
        return false;
    }
    if filter
        .space
        .as_deref()
        .is_some_and(|wanted| asset.space.as_ref().map(|s| s.0.as_str()) != Some(wanted))
    {
        return false;
    }
    if filter
        .category
        .as_deref()
        .is_some_and(|wanted| asset.category.as_deref() != Some(wanted))
    {
        return false;
    }
    if filter
        .organization
        .as_deref()
        .is_some_and(|wanted| asset.organization.as_deref() != Some(wanted))
    {
        return false;
    }
    if let Some(query) = filter.query.as_deref() {
        let haystack = [
            asset.id.as_str(),
            asset.name.as_str(),
            asset.manufacturer.as_deref().unwrap_or(""),
            asset.model.as_deref().unwrap_or(""),
            asset.serial_number.as_deref().unwrap_or(""),
            asset.category.as_deref().unwrap_or(""),
            asset.organization.as_deref().unwrap_or(""),
            asset.location.as_ref().map(|l| l.0.as_str()).unwrap_or(""),
            asset.space.as_ref().map(|s| s.0.as_str()).unwrap_or(""),
            asset.rack_or_case.as_deref().unwrap_or(""),
            asset.assigned_to.as_deref().unwrap_or(""),
            asset.vendor.as_deref().unwrap_or(""),
            asset.notes.as_deref().unwrap_or(""),
            asset
                .reservations
                .iter()
                .map(|reservation| reservation.reference.0.as_str())
                .collect::<Vec<_>>()
                .join(" ")
                .as_str(),
        ]
        .join(" ")
        .to_ascii_lowercase();
        if !haystack.contains(&query.to_ascii_lowercase()) {
            return false;
        }
    }
    true
}

pub fn reservation_windows_overlap(
    a_start: Option<DateTime<Utc>>,
    a_end: Option<DateTime<Utc>>,
    b_start: Option<DateTime<Utc>>,
    b_end: Option<DateTime<Utc>>,
) -> bool {
    match (a_start, a_end, b_start, b_end) {
        (Some(a_start), Some(a_end), Some(b_start), Some(b_end)) => {
            a_start < b_end && b_start < a_end
        }
        _ => true,
    }
}

pub fn asset_unavailable_reason(asset: &Asset) -> Option<&'static str> {
    match asset.status {
        AssetStatus::NeedsRepair => Some("asset needs repair"),
        AssetStatus::InRepair => Some("asset is in repair"),
        AssetStatus::MaintenanceDue => Some("asset maintenance is due"),
        AssetStatus::Retired => Some("asset is retired"),
        AssetStatus::Lost => Some("asset is lost"),
        _ => None,
    }
}

pub fn conflicts_for_reservation(
    asset: &Asset,
    reservation: &AssetReservationRecord,
) -> Vec<AssetConflict> {
    let mut conflicts = Vec::new();
    if let Some(reason) = asset_unavailable_reason(asset) {
        conflicts.push(AssetConflict {
            asset_id: asset.id.clone(),
            asset_name: asset.name.clone(),
            reason: reason.to_string(),
            reservation: Some(reservation.clone()),
        });
    }

    for existing in &asset.reservations {
        if existing.id == reservation.id {
            continue;
        }
        if reservation_windows_overlap(
            existing.starts_at,
            existing.ends_at,
            reservation.starts_at,
            reservation.ends_at,
        ) {
            conflicts.push(AssetConflict {
                asset_id: asset.id.clone(),
                asset_name: asset.name.clone(),
                reason: format!("overlaps reservation {}", existing.reference.0),
                reservation: Some(existing.clone()),
            });
        }
    }

    conflicts
}

pub fn collect_asset_conflicts(assets: &[Asset]) -> Vec<AssetConflict> {
    let mut conflicts = Vec::new();
    for asset in assets {
        for reservation in &asset.reservations {
            conflicts.extend(conflicts_for_reservation(asset, reservation));
        }
    }
    conflicts
}

pub fn build_asset_report(assets: &[Asset], today: NaiveDate) -> AssetReport {
    use std::collections::BTreeMap;

    let mut report = AssetReport {
        generated_at: Utc::now(),
        today: today.to_string(),
        asset_count: assets.len() as u32,
        ..AssetReport::default()
    };
    report.assets = assets.to_vec();

    let mut by_status: BTreeMap<String, u32> = BTreeMap::new();
    let mut by_category: BTreeMap<String, u32> = BTreeMap::new();
    let mut by_org: BTreeMap<String, u32> = BTreeMap::new();

    for asset in assets {
        *by_status.entry(format!("{:?}", asset.status)).or_default() += 1;
        *by_category
            .entry(
                asset
                    .category
                    .clone()
                    .unwrap_or_else(|| "Uncategorized".into()),
            )
            .or_default() += 1;
        *by_org
            .entry(
                asset
                    .organization
                    .clone()
                    .unwrap_or_else(|| "Unassigned".into()),
            )
            .or_default() += 1;
    }

    report.by_status = by_status
        .into_iter()
        .map(|(name, asset_count)| AssetBucket { name, asset_count })
        .collect();
    report.by_category = by_category
        .into_iter()
        .map(|(name, asset_count)| AssetBucket { name, asset_count })
        .collect();
    report.by_organization = by_org
        .into_iter()
        .map(|(name, asset_count)| AssetBucket { name, asset_count })
        .collect();

    report
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_asset_status_names() {
        assert_eq!(
            parse_asset_status("available"),
            Some(AssetStatus::Available)
        );
        assert_eq!(parse_asset_status("in use"), Some(AssetStatus::InUse));
        assert_eq!(
            parse_asset_status("needs-repair"),
            Some(AssetStatus::NeedsRepair)
        );
        assert_eq!(
            parse_asset_status("maintenance due"),
            Some(AssetStatus::MaintenanceDue)
        );
        assert_eq!(parse_asset_status("nope"), None);
    }

    #[test]
    fn renders_asset_body_with_key_fields() {
        let asset = Asset {
            uuid: Uuid::new_v4(),
            id: "AST-2026-0001".into(),
            number: 1,
            name: "Shure SM58".into(),
            status: AssetStatus::InUse,
            manufacturer: Some("Shure".into()),
            model: Some("SM58".into()),
            serial_number: Some("SN123".into()),
            category: Some("audio".into()),
            organization: Some("FastTrack".into()),
            location: Some(WikiLink("Studio A".into())),
            space: Some(WikiLink("Vocal Booth".into())),
            rack_or_case: Some("Mic case 1".into()),
            assigned_to: Some("Session 42".into()),
            purchase_date: Some(NaiveDate::from_ymd_opt(2026, 1, 2).unwrap()),
            warranty_until: Some(NaiveDate::from_ymd_opt(2027, 1, 2).unwrap()),
            vendor: Some("Sweetwater".into()),
            cost_cents: Some(9999),
            maintenance: vec![AssetMaintenanceRecord {
                date: NaiveDate::from_ymd_opt(2026, 4, 1).unwrap(),
                issue: "Replaced grille".into(),
                vendor: Some("Local repair".into()),
                contact: Some("Alex".into()),
                cost_cents: Some(1500),
                warranty: false,
                rma: Some("RMA-123".into()),
                task: Some(WikiLink("Fix mic".into())),
                notes: Some("Handled before show".into()),
            }]
            .into(),
            reservations: vec![AssetReservationRecord {
                id: "res-1".into(),
                reference: WikiLink("Show Night".into()),
                ..Default::default()
            }]
            .into(),
            linked_tasks: vec![WikiLink("Repair mic".into())].into(),
            notes: Some("Keep as spare if possible".into()),
            created_by: Some("cody".into()),
            properties: crate::property::JsonObject::default(),
            date_created: None,
            date_modified: None,
            body: String::new(),
        };

        let body = render_asset_body(&asset);
        assert!(body.contains("# Asset Shure SM58"));
        assert!(body.contains("**Location:** Studio A"));
        assert!(body.contains("**Reservations:** Show Night"));
        assert!(body.contains("## Maintenance"));
    }

    #[test]
    fn filters_and_reports_assets() {
        let assets = vec![
            Asset {
                id: "AST-2026-0001".into(),
                number: 1,
                name: "Mic".into(),
                status: AssetStatus::Available,
                category: Some("audio".into()),
                organization: Some("FastTrack".into()),
                location: Some(WikiLink("Studio A".into())),
                space: Some(WikiLink("Booth".into())),
                body: String::new(),
                ..Default::default()
            },
            Asset {
                id: "AST-2026-0002".into(),
                number: 2,
                name: "Amp".into(),
                status: AssetStatus::NeedsRepair,
                category: Some("audio".into()),
                organization: Some("FastTrack".into()),
                location: Some(WikiLink("Studio B".into())),
                space: Some(WikiLink("Rack".into())),
                body: String::new(),
                ..Default::default()
            },
        ];

        let filter = AssetFilter {
            location: Some("Studio A".into()),
            query: Some("mic".into()),
            ..Default::default()
        };
        assert!(matches_asset_filter(&assets[0], &filter));
        assert!(!matches_asset_filter(&assets[1], &filter));

        let report = build_asset_report(&assets, NaiveDate::from_ymd_opt(2026, 5, 2).unwrap());
        assert_eq!(report.asset_count, 2);
        assert_eq!(report.by_status.len(), 2);
    }

    #[test]
    fn detects_unavailable_and_overlapping_asset_reservations() {
        let start = DateTime::parse_from_rfc3339("2026-05-04T20:00:00Z")
            .unwrap()
            .to_utc();
        let end = DateTime::parse_from_rfc3339("2026-05-04T22:00:00Z")
            .unwrap()
            .to_utc();
        let asset = Asset {
            id: "AST-2026-0001".into(),
            number: 1,
            name: "Console".into(),
            status: AssetStatus::NeedsRepair,
            reservations: vec![AssetReservationRecord {
                id: "existing".into(),
                reference: WikiLink("Show A".into()),
                starts_at: Some(start),
                ends_at: Some(end),
                ..Default::default()
            }]
            .into(),
            ..Default::default()
        };
        let reservation = AssetReservationRecord {
            id: "new".into(),
            reference: WikiLink("Show B".into()),
            starts_at: Some(start + chrono::Duration::hours(1)),
            ends_at: Some(end + chrono::Duration::hours(1)),
            ..Default::default()
        };

        let conflicts = conflicts_for_reservation(&asset, &reservation);
        assert_eq!(conflicts.len(), 2);
        assert!(
            conflicts
                .iter()
                .any(|conflict| conflict.reason == "asset needs repair")
        );
        assert!(
            conflicts
                .iter()
                .any(|conflict| conflict.reason.contains("Show A"))
        );
    }
}
