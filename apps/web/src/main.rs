//! Dioxus web entry point. Hands off rendering to `example-ui::App`
//! and provides a one-time vox connection in app context so any view
//! can call `ExampleServiceClient` methods without re-connecting.

use dioxus::prelude::*;
use example_ui::App;

fn main() {
    dioxus::launch(Root);
}

#[component]
fn Root() -> Element {
    rsx! { App {} }
}
