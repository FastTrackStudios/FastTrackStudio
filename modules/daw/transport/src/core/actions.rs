//! Transport Actions Trait
//!
//! This module defines the TransportActions trait that provides behavioral
//! methods for transport control operations.

use super::Tempo;

/// Transport actions trait for behavioral methods
pub trait TransportActions {
    /// Check if transport is currently playing
    fn is_playing(&self) -> bool;

    /// Check if transport is currently recording
    fn is_recording(&self) -> bool;

    /// Check if transport is stopped
    fn is_stopped(&self) -> bool;

    /// Check if transport is paused
    fn is_paused(&self) -> bool;

    /// Set the tempo, validating it's within acceptable range
    fn set_tempo(&mut self, tempo: Tempo) -> Result<(), String>;

    /// Set the playback rate
    fn set_playrate(&mut self, rate: f64) -> Result<(), String>;

    /// Get the effective BPM (accounting for playrate)
    fn effective_bpm(&self) -> f64;

    /// Reset transport to initial state
    fn reset(&mut self);
}
