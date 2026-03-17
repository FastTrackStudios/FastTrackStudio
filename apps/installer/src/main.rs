//! FastTrackStudio Installer — downloads REAPER, installs extensions and presets.

mod app;
mod wizard;

use dioxus::desktop::tao::dpi::LogicalSize;
use dioxus::desktop::{Config, WindowBuilder};
use dioxus::prelude::*;

const MAIN_CSS: Asset = asset!("/assets/main.css");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter("info,installer_core=debug")
        .init();

    let config = Config::new().with_window(
        WindowBuilder::new()
            .with_title("FastTrackStudio Installer")
            .with_inner_size(LogicalSize::new(640.0_f64, 500.0_f64))
            .with_resizable(false),
    );

    dioxus::LaunchBuilder::desktop()
        .with_cfg(config)
        .launch(app::App);
}
