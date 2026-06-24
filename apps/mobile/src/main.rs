//! Signal Mobile — live audio processing app for iPhone and iPad.
//!
//! Audio engine starts before the UI and runs for the entire session.
//! ProcessingChain is injected via Dioxus context so all components
//! can read meters and update parameters without prop-drilling.
//!
//! Rendering: Dioxus mobile (WKWebView on iOS). NOT nice_plug_dioxus/Blitz —
//! mobile uses the WebView renderer, so all layout is CSS-based.

use signal_audio::{LiveAudioEngine, ProcessingChain};

mod app;
mod channel;
mod mixer;
mod piano_view;
mod scene_bar;
mod styles;

fn main() {
    // Start audio engine before handing control to the WebView event loop.
    // 48 kHz, stereo — matches typical iOS audio session sample rate.
    let chain = ProcessingChain::new(48000.0);
    // Keep the engine alive for the app's lifetime.
    let _engine = LiveAudioEngine::new(chain.clone());

    dioxus::LaunchBuilder::mobile()
        .with_context(chain)
        .launch(app::App);
}
