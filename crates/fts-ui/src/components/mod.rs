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
mod nav_tab;
mod progress_bar;

pub use nav_tab::*;
pub use progress_bar::*;

mod badge;
mod card;
mod empty_state;
mod form_field;
mod inline_edit;
mod key_value_row;
mod list_row;
mod searchable_dropdown;
mod searchable_list;
mod section_header;
mod segmented_control;
mod status;

pub use badge::*;
pub use card::*;
pub use empty_state::*;
pub use form_field::*;
pub use inline_edit::*;
pub use key_value_row::*;
pub use list_row::*;
pub use searchable_dropdown::*;
pub use searchable_list::*;
pub use section_header::*;
pub use segmented_control::*;
pub use status::*;
