use dioxus::prelude::*;
use fts_ui::showcase::Showcase;

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    rsx! {
        document::Stylesheet { href: TAILWIND_CSS }
        Showcase { renderer: "Mobile".to_string() }
    }
}
