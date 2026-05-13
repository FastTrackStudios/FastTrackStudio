use dioxus::desktop::{Config, tao::window::WindowBuilder};
use dioxus::prelude::*;
use task_ui::App;

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    let cfg = Config::new()
        .with_window(
            WindowBuilder::new()
                .with_title("Task")
                .with_inner_size(dioxus::desktop::tao::dpi::LogicalSize::new(1280.0, 800.0)),
        )
        .with_menu(None);
    LaunchBuilder::desktop().with_cfg(cfg).launch(Root);
}

#[component]
fn Root() -> Element {
    rsx! {
        document::Stylesheet { href: TAILWIND_CSS }
        App {}
    }
}
