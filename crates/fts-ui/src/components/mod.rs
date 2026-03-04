//! FTS UI components.

// --- Wrapped components (FTS defaults applied) ---
mod button;
mod input;

pub use button::*;
pub use input::*;

// --- Re-exported from lumen-blocks ---
pub mod checkbox;
pub mod context_menu;
pub mod dropdown;
pub mod label;
pub mod progress;
pub mod side_sheet;
pub mod switch;
pub mod toast;

// --- Migrated from signal-ui ---
mod dialog;
mod tabs;

pub use dialog::*;
pub use tabs::*;

// --- New FTS components ---
mod badge;
mod card;

pub use badge::*;
pub use card::*;
