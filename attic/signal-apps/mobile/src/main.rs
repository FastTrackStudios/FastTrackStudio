//! Signal Mobile — live audio processing app for iPhone and iPad.
//!
//! Audio engine starts before the UI and runs for the entire session.
//! ProcessingChain is injected via Dioxus context so all components
//! can read meters and update parameters without prop-drilling.
//!
//! Rendering: Dioxus mobile (WKWebView on iOS). NOT nice_plug_dioxus/Blitz —
//! mobile uses the WebView renderer, so all layout is CSS-based.

use signal_ui::ProcessingChain;

mod app;
mod channel;
mod mixer;
mod piano_view;
mod scene_bar;
mod styles;

fn main() {
    // UI meter state shared via context. Audio I/O lives in daw; the mobile
    // shell currently renders UI only (no standalone audio engine).
    let chain = ProcessingChain::new(48000.0);

    dioxus::LaunchBuilder::mobile()
        .with_context(chain)
        .launch(app::App);
}
