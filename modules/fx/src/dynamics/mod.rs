//! Dynamics effect blocks.
//!
//! This module provides dynamics processors:
//! - Compressor (with various knee types and detection modes)
//! - Limiter (brickwall, soft-knee)
//! - Gate / Expander
//! - Transient shaper
//! - De-esser
//!
//! ## Structure
//!
//! Each dynamics processor has:
//! - `processor.rs` - DSP implementation
//! - `params.rs` - Parameters (threshold, ratio, attack, release, etc.)
//! - `presets.rs` - Preset definitions ("Vocal", "Drums", "Bus Glue", etc.)
//! - `ui.rs` - UI components (feature-gated)

// TODO: Implement dynamics blocks
