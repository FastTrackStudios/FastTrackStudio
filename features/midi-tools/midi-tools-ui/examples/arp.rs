//! Run the arpeggiator panel in a desktop window.
//!
//! ```sh
//! cargo run -p midi-tools-ui --example arp
//! ```
//!
//! No sink is provided, so it falls back to `midi_tools::DemoArpSink` and
//! arpeggiates an Am → F progression. Every control is live; only Apply
//! reports that there's no DAW behind it.
use dioxus::desktop::tao::dpi::LogicalSize;
use dioxus::desktop::{Config, WindowBuilder};

fn main() {
    let window = WindowBuilder::new()
        .with_title("FTS MIDI Arpeggiator")
        .with_decorations(false)
        .with_inner_size(LogicalSize::new(520.0, 700.0))
        .with_min_inner_size(LogicalSize::new(380.0, 480.0));

    dioxus::LaunchBuilder::new()
        .with_cfg(Config::new().with_window(window).with_menu(None))
        .launch(midi_tools_ui::ArpPanel);
}
