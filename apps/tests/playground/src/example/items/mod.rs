//! Items feature — complete vertical slice
//!
//! Proto (types + service trait) → Entity (SeaORM) → Repo → Live → Control → UI

pub mod control;
pub mod entity;
pub mod live;
pub mod proto;
pub mod repo;
mod ui;

use dioxus::prelude::*;
use crate::example::ui_state::AppUiContext;

#[allow(unused_imports)]
pub use control::{ItemControl, ItemName, RawName, ValidName};
#[allow(unused_imports)]
pub use proto::{ItemEvent, ItemInfo, ItemService};
pub type ControlApi = ItemControl;

pub struct Item;

impl Item {
    pub fn start_subscription(app: AppUiContext) {
        ui::start_subscription(app);
    }

    pub fn clear_items(app: AppUiContext) {
        ui::clear_items(app);
    }
}

#[component]
pub fn Screen() -> Element {
    rsx! { ui::AuthenticatedApp {} }
}
