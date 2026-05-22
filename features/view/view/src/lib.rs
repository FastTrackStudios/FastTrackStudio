//! `view` — facade for the view feature family.
//!
//! Project-management chart crates (gantt, kanban, calendar, table,
//! …) live under `features/view/view-*`. Every consumer outside
//! `features/view/` should depend on this crate. Internal view
//! crates depend on each other directly and skip the facade.
//!
//! Feature flags:
//! - `gantt` (default) — pull in `view-gantt`, exposed as
//!   `view::gantt`. Drop-in Dioxus gantt chart.
//! - `kanban` (default) — pull in `view-kanban`, exposed as
//!   `view::kanban`. Drop-in Dioxus kanban board.
//!
//! Future flags (placeholders, not yet implemented):
//! - `calendar` — `view-calendar` month/week grid.
//! - `table` — `view-table` Obsidian-Bases-style table.

#[cfg(feature = "gantt")]
pub use view_gantt as gantt;

#[cfg(feature = "kanban")]
pub use view_kanban as kanban;
