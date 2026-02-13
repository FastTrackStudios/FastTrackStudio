//! Playground for signal2 collection/variant UI.

use dioxus::prelude::*;
use signal2::{bootstrap_in_memory_controller_async, SignalController};

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");
const MAIN_CSS: Asset = asset!("/assets/main.css");

fn main() {
    dioxus::LaunchBuilder::desktop().launch(App);
}

#[component]
fn App() -> Element {
    let mut controller = use_signal(|| None::<SignalController>);
    use_effect(move || {
        spawn(async move {
            let built = bootstrap_in_memory_controller_async()
                .await
                .expect("failed to initialize in-memory signal storage");
            controller.set(Some(built));
        });
    });

    rsx! {
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        document::Link { rel: "stylesheet", href: MAIN_CSS }

        div { class: "min-h-screen flex items-center justify-center",
            if let Some(controller) = controller() {
                signal2_ui::SignalSlider { controller }
            } else {
                p { class: "text-sm text-zinc-600", "Loading signal..." }
            }
        }
    }
}
