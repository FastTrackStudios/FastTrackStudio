//! Solidtime-style time tracker UI — composable submodules.

pub mod bar;
pub mod common;
pub mod dashboard;
pub mod filter;
pub mod idle;
pub mod idle_detector;
pub mod manual;
pub mod report;
pub mod stats;
pub mod table;

pub use bar::{RunningTimerBar, RunningTimerBarProps, TimeEntryStartInput};
pub use common::{
    DayTotals, bucket_by_day, entry_seconds, format_duration_compact, format_duration_hms,
    format_money, week_start, weekly_totals,
};
pub use dashboard::{SolidtimeDashboard, SolidtimeDashboardProps};
pub use filter::{
    TimerBillableFilter, TimerFilterBar, TimerFilterBarProps, TimerFilterState, apply_filter,
};
pub use idle::{IdleWindow, TimerIdleDialog, TimerIdleDialogProps};
pub use idle_detector::use_idle_detector;
pub use manual::{ManualTimeEntryDialog, ManualTimeEntryDialogProps};
pub use report::{TimerWeekReport, TimerWeekReportProps};
pub use stats::{TimerSummaryStats, TimerSummaryStatsProps};
pub use table::{TimeEntryTable, TimeEntryTableProps};
