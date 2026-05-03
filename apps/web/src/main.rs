use dioxus::prelude::*;
use task_ui::App;

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    dioxus::launch(Root);
}

#[component]
fn Root() -> Element {
    rsx! {
        document::Stylesheet { href: TAILWIND_CSS }
        App {}
    }
}
