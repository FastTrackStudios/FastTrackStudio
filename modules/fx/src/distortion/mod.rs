//! Distortion effect blocks.
//!
//! This module provides distortion/saturation effects:
//! - Overdrive (soft clipping)
//! - Distortion (hard clipping)
//! - Fuzz
//! - Tube saturation
//! - Tape saturation
//! - Bitcrusher
//! - Waveshaper
//!
//! ## Structure
//!
//! Each distortion type has:
//! - `processor.rs` - DSP implementation (often with oversampling)
//! - `params.rs` - Parameters (drive, tone, mix, etc.)
//! - `presets.rs` - Preset definitions ("Tube Screamer", "Big Muff", etc.)
//! - `ui.rs` - UI components (feature-gated)

// TODO: Implement distortion blocks
