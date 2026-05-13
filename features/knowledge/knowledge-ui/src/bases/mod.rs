//! Bases — Obsidian Bases (.base) parsing, evaluation, and views.
//!
//! Wire-level parsing lives in [`knowledge_proto::bases`]; this
//! module supplies the runtime evaluator + Dioxus components.

pub mod evaluator;
pub mod views;

pub use evaluator::{BaseResultSet, ColumnSpec, PageRow, ResultRow, run_base};
pub use views::dispatch::BaseView;
pub use views::table::BaseTableView;
