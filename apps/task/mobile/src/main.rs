use dioxus::prelude::*;
use ui::App;

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    // Error/crash telemetry — hold `_sentry` for the life of `main`
    // (`dioxus::launch` diverges, so binding it here is sufficient).
    // The tracing subscriber carries the Sentry layer so `warn!`/
    // `error!` events are captured; `.try_init()` (inside init_tracing)
    // makes a later dioxus subscriber-init a no-op rather than a panic.
    let _sentry = task_telemetry::init_tracing("task-mobile", "info");

    dioxus::launch(Root);
}

#[component]
fn Root() -> Element {
    rsx! {
        document::Stylesheet { href: TAILWIND_CSS }
        App {}
    }
}
