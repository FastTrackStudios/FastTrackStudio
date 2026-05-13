//! Project-management components — list / board / sheet / palette /
//! header. Built on top of `project-proto`'s `Task` + `Project` wire
//! types using the `fts-ui` design system.

pub mod board;
pub mod common;
pub mod header;
pub mod list;
pub mod overview;
pub mod palette;
pub mod priority;
pub mod sheet;
pub mod theme;

pub use board::TaskKanbanBoard;
pub use common::{color_swatch_class, fuzzy_match, status_to_badge_variant, task_id_chip};
pub use header::{ProjectHeaderCard, ProjectTab, TaskStats, TaskView};
pub use list::{BulkAction, TaskGroupBy, TaskListView};
pub use overview::{ProjectOverviewCard, ProjectOverviewGrid};
pub use palette::{PaletteIntent, TaskCommandPalette};
pub use priority::{Priority, PriorityBadge};
pub use sheet::TaskDetailSheet;
pub use theme::ProjectThemeSettings;
