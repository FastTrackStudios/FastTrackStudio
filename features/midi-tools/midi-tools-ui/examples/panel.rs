//! Run the velocity panel in a desktop window.
//!
//! ```sh
//! cargo run -p midi-tools-ui --example panel
//! dx serve --package midi-tools-ui --example panel --platform linux --hot-patch false
//! ```
//!
//! The iteration loop, not the shipping target. This uses Dioxus's
//! desktop (WebView) renderer, while the REAPER panel renders through
//! Blitz — CSS support differs, so keep styles simple and check the
//! REAPER panel before believing a layout.
//!
//! No sink is provided, so the panel falls back to `midi_tools::DemoSink`
//! and shapes a synthetic 32-note hi-hat pattern. Every control is live;
//! only Apply and Revert report that there's no DAW behind them.
use dioxus::desktop::tao::dpi::LogicalSize;
use dioxus::desktop::{Config, WindowBuilder};

fn main() {
    let window = WindowBuilder::new()
        .with_title("FTS MIDI Velocity")
        .with_decorations(false)
        .with_inner_size(LogicalSize::new(520.0, 760.0))
        .with_min_inner_size(LogicalSize::new(380.0, 520.0));

    dioxus::LaunchBuilder::new()
        .with_cfg(Config::new().with_window(window).with_menu(None))
        .launch(midi_tools_ui::VelocityPanel);
}
