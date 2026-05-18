//! Cross-platform multi-track audio engine.
//!
//! Decodes audio files with Symphonia, mixes multiple tracks with per-track
//! gain/mute/solo, and outputs via cpal. Works on native (macOS/Windows/Linux/
//! iOS/Android) and WASM (via cpal's wasm-bindgen backend).
//!
//! # Architecture
//!
//! ```text
//! ┌──────────┐     ┌─────────────────────────────────────────┐
//! │ Symphonia │────►│ DecodedAudio (interleaved f32 PCM)      │
//! │ (decode)  │     └──────────────┬──────────────────────────┘
//! └──────────┘                     │
//!                    ┌─────────────▼──────────────┐
//!                    │ MixerState (shared state)   │
//!                    │  • playing: bool            │
//!                    │  • position: sample frame   │
//!                    │  • tracks: Vec<TrackAudio>  │
//!                    │    - gain, muted, soloed    │
//!                    └─────────────┬──────────────┘
//!                                  │ (real-time callback)
//!                    ┌─────────────▼──────────────┐
//!                    │ cpal OutputStream           │
//!                    │  • pulls mixed PCM          │
//!                    │  • platform audio output    │
//!                    └────────────────────────────┘
//! ```

pub mod decoder;
pub mod materialize;
#[cfg(feature = "audio")]
mod mixer;
pub mod render;
// Legacy mixer-direct RPP loader retired in favor of
// `crate::project_loader::load_rpp`, which populates the full
// `ProjectState` and routes audio through the renderer. Keep the
// gate so old `rpp-loader` consumers still compile (now a no-op).
#[cfg(feature = "audio")]
pub mod test_tone;

pub use decoder::{DecodedAudio, decode_audio, decode_audio_with_extension};
#[cfg(feature = "audio")]
pub use mixer::{AudioEngine, TrackHandle};
