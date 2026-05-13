//! Interactive calendar — month / week / day / agenda views, keyboard nav,
//! mini-month sidebar, click-to-create / click-to-edit dialogs.

pub mod agenda;
pub mod calendar;
pub mod chip;
pub mod dialog;
pub mod layout;
pub mod mini_month;
pub mod month;
pub mod week;

pub use calendar::{CalendarView, InteractiveCalendar};
