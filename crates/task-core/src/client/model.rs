//! Client entity — billable party for invoicing.
//!
//! Clients live as markdown notes (`clients/<Name>.md`) with YAML
//! frontmatter. The schema is Invoice-Ninja-compatible: the hashed
//! `invoice_ninja_id` lets us push invoices to IN without mirroring
//! the client's row in their database.
//!
//! Rate cascade (highest-to-lowest priority, matches Invoice Ninja):
//!
//! ```text
//! TimeEntry.billable_rate  (per-entry override)
//!   └─ Project.default_rate
//!        └─ Client.default_hourly_rate
//!             └─ caller-provided fallback (--rate on reports / org default)
//! ```

use chrono::DateTime;
use chrono::Utc;
use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;
use uuid::Uuid;

/// A billable client.
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
#[sea_orm(table_name = "clients")]
#[crudcrate(
    api_struct = "ClientApi",
    generate_vox_service,
    name_singular = "client",
    name_plural = "clients"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(
        primary_key,
        exclude(create),
        on_create = uuid::Uuid::new_v4()
    )]
    pub id: Uuid,

    /// Display name (e.g. "Acme Records"). Becomes the file name too.
    #[crudcrate(filterable, sortable, fulltext)]
    pub name: String,

    // ── Invoice Ninja sync ──────────────────────────────────────────
    /// Opaque IN client hashed id — set after the first push. All IN
    /// entity ids are strings, not integers.
    #[crudcrate(filterable)]
    pub invoice_ninja_id: Option<String>,

    /// Invoice Ninja currency id (numeric FK into IN's currency table —
    /// USD=1, GBP=2, EUR=3, etc.). Stored separate from `currency_code`
    /// because IN expects its own id, not the ISO code.
    pub invoice_ninja_currency_id: Option<u32>,

    /// Timestamp of the most recent pull from Invoice Ninja, if any.
    pub last_synced_at: Option<DateTime<Utc>>,

    // ── Billing ─────────────────────────────────────────────────────
    /// Default billable rate in cents per hour. Falls back only when the
    /// project and entry don't specify their own rate.
    pub default_hourly_rate: Option<u32>,

    /// ISO 4217 currency code (e.g. "USD"). Used for display; the
    /// Invoice-Ninja-side currency is in `invoice_ninja_currency_id`.
    #[facet(default)]
    #[crudcrate(filterable, sortable)]
    pub currency_code: String,

    /// Net payment terms in days (e.g. 30 for net-30). None leaves it to
    /// the organization default.
    pub payment_terms_days: Option<u32>,

    // ── Contact ─────────────────────────────────────────────────────
    #[crudcrate(filterable)]
    pub email: Option<String>,
    pub phone: Option<String>,
    pub contact_name: Option<String>,

    pub address1: Option<String>,
    pub address2: Option<String>,
    pub city: Option<String>,
    pub state: Option<String>,
    pub postal_code: Option<String>,
    /// ISO 3166-1 alpha-2 country code (e.g. "US", "CA").
    pub country_code: Option<String>,

    pub vat_number: Option<String>,

    // ── Bookkeeping ─────────────────────────────────────────────────
    pub private_notes: Option<String>,
    pub public_notes: Option<String>,

    /// Soft delete.
    pub deleted_at: Option<DateTime<Utc>>,
}

pub type Client = Model;

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}

impl Client {
    pub fn is_deleted(&self) -> bool {
        self.deleted_at.is_some()
    }
}

/// Resolve an effective billable rate (cents/hour) for a time entry using
/// the IN-compatible cascade.
pub fn resolve_rate(
    entry_rate: Option<u32>,
    project_rate: Option<u32>,
    client_rate: Option<u32>,
    fallback: Option<u32>,
) -> u32 {
    entry_rate
        .or(project_rate)
        .or(client_rate)
        .or(fallback)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cascade_prefers_entry() {
        assert_eq!(
            resolve_rate(Some(12_000), Some(10_000), Some(8_000), None),
            12_000
        );
    }

    #[test]
    fn cascade_falls_through_to_project() {
        assert_eq!(
            resolve_rate(None, Some(10_000), Some(8_000), Some(5_000)),
            10_000
        );
    }

    #[test]
    fn cascade_falls_through_to_client() {
        assert_eq!(resolve_rate(None, None, Some(8_000), Some(5_000)), 8_000);
    }

    #[test]
    fn cascade_falls_through_to_fallback() {
        assert_eq!(resolve_rate(None, None, None, Some(5_000)), 5_000);
    }

    #[test]
    fn cascade_zero_when_nothing_set() {
        assert_eq!(resolve_rate(None, None, None, None), 0);
    }
}
