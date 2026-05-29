//! Dioxus desktop entry point.

use dioxus::prelude::*;
use app_ui::App;

fn main() {
    dioxus::launch(Root);
}

#[component]
fn Root() -> Element {
    rsx! { App {} }
}
