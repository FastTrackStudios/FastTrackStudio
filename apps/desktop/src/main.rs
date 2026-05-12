//! Dioxus desktop entry point.

use dioxus::prelude::*;
use example_ui::App;

fn main() {
    dioxus::launch(Root);
}

#[component]
fn Root() -> Element {
    rsx! { App {} }
}
