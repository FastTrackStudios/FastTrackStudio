//! Inventory feature components.

pub mod breadcrumb;
pub mod card;
pub mod checkout;
pub mod common;
pub mod dashboard;
pub mod filters;
pub mod sheet;
pub mod table;
pub mod warranty;

pub use breadcrumb::LocationBreadcrumb;
pub use card::InventoryCard;
pub use checkout::{CheckoutCmd, CheckoutDialog};
pub use common::{
    WarrantyHealth, category_icon, category_label, format_value_cents, status_to_badge_variant,
    warranty_class, warranty_health,
};
pub use dashboard::InventoryDashboard;
pub use filters::{InventoryFilterState, InventoryFilters};
pub use sheet::InventoryItemDetailSheet;
pub use table::{InventoryTable, category_swatch_class};
pub use warranty::WarrantyBadge;
