//! Macro knob event handling and DAW parameter synchronization.
//!
//! This module encapsulates all macro-related event handling logic,
//! separating concerns between UI state management and DAW communication.
//!
//! # Architecture
//!
//! - **MacroChangeHandler**: Processes knob changes and coordinates:
//!   - Recording (if active)
//!   - DAW RPC calls to set FX parameters
//!   - Parallel parameter updates for smooth automation
//!
//! - **RecordingManager**: Manages recording state and UI synchronization
//!
//! # Example
//!
//! ```ignore
//! let recorder = signal::MacroRecorder::new();
//! let on_macro_change = create_macro_change_handler(recorder.clone());
//!
//! // Wire to UI callback
//! MacroBar { on_macro_change: Some(EventHandler::new(on_macro_change)) }
//! ```

use crate::daw_registry;
use dioxus::prelude::*;
use signal::macro_registry;

/// Create a macro knob change handler that drives DAW parameters and records changes.
///
/// # Arguments
///
/// - `recorder`: Reference to the macro recorder for capturing knob movements
/// - `is_recording`: Signal indicating whether recording is currently active
///
/// # Returns
///
/// A closure that handles `(knob_id, new_value)` tuples from macro knobs.
/// The closure:
/// 1. Records the change if recording is active
/// 2. Looks up parameter targets in the registry
/// 3. Maps the macro value through the parameter's [min, max] curve
/// 4. Sets the FX parameter in parallel (join_all) for smooth automation
///
/// # Error Handling
///
/// Silently skips missing targets or DAW connection issues. Logs warnings for debugging.
pub fn create_macro_change_handler(
    recorder: Signal<signal::MacroRecorder>,
    is_recording: Signal<bool>,
) -> impl Fn((String, f32)) {
    move |(knob_id, new_val): (String, f32)| {
        // Step 1: Record if recording
        if is_recording() {
            recorder.read().record(knob_id.clone(), new_val);
        }

        // Step 2: Get parameter targets
        let targets = macro_registry::get_targets(&knob_id);
        if targets.is_empty() {
            return;
        }

        // Step 3: Get DAW handle
        let Some(daw) = daw_registry::signal_daw() else {
            return;
        };

        // Step 4: Set parameters in parallel
        spawn(async move {
            let Ok(project) = daw.current_project().await else {
                tracing::warn!("Failed to get current REAPER project for macro parameter update");
                return;
            };

            for target in targets {
                // Map macro value (0.0–1.0) through parameter curve [min, max]
                let param_val = (target.min + (target.max - target.min) * new_val) as f64;

                let Ok(Some(track)) = project.tracks().by_guid(&target.track_guid).await else {
                    tracing::debug!("Track not found: {}", target.track_guid);
                    continue;
                };

                let Ok(Some(fx)) = track.fx_chain().by_guid(&target.fx_guid).await else {
                    tracing::debug!("FX not found on track: {}", target.fx_guid);
                    continue;
                };

                if let Err(e) = fx.param(target.param_index).set(param_val).await {
                    tracing::warn!("Failed to set FX param {}: {}", target.param_index, e);
                }
            }
        });
    }
}

/// Recording state and UI coordination.
///
/// Handles the lifecycle of macro recording with visual feedback.
/// Currently unused in performance_tab, but provided for future UI enhancements.
#[allow(dead_code)]
pub struct RecordingManager {
    /// Whether recording is currently active
    pub is_recording: Signal<bool>,
    /// The macro recorder instance
    pub recorder: Signal<signal::MacroRecorder>,
}

impl RecordingManager {
    /// Create a new recording manager.
    pub fn new(recorder: Signal<signal::MacroRecorder>) -> Self {
        Self {
            is_recording: use_signal(|| false),
            recorder,
        }
    }

    /// Start recording macro changes.
    pub fn start(&mut self) {
        self.recorder.read().start();
        self.is_recording.set(true);
        tracing::info!("Started macro recording");
    }

    /// Stop recording and return the captured sequence.
    pub fn stop(&mut self) -> Vec<signal::MacroRecord> {
        let records = self.recorder.read().stop();
        self.is_recording.set(false);
        let count = records.len();
        if count > 0 {
            tracing::info!("Stopped macro recording: {} changes captured", count);
        }
        records
    }

    /// Get current recording duration in milliseconds.
    pub fn elapsed_ms(&self) -> Option<u64> {
        self.recorder.read().elapsed_ms()
    }

    /// Get recording statistics (count, duration, knobs).
    pub fn stats(&self) -> (usize, u64, Vec<String>) {
        self.recorder.read().stats()
    }
}
