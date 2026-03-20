//! Drift correction for PeerMesh transport sync.
//!
//! Uses proportional control to adjust playback rate based on the position
//! difference between master and follower. At 5Hz heartbeat rate, the
//! semitone-nudge approach from the Link engine (designed for 30Hz) causes
//! runaway oscillation. Instead, we compute a playrate proportional to the
//! drift, which naturally converges without overshooting.
//!
//! The [`DriftCorrector`] is host-agnostic: it takes a position difference and
//! returns a [`DriftAction`] describing what the caller should do.

use tracing::debug;

// ── Rolling Average ─────────────────────────────────────────────────────────

/// Fixed-size circular buffer with interquartile-mean smoothing.
///
/// Port of ReaBlink's `RollingAverage.hpp`. Maintains a bounded window of
/// samples and returns the trimmed mean (drops the lowest and highest
/// quarters before averaging), which rejects outliers from timer jitter
/// and scheduling noise.
pub struct RollingAverage {
    values: Vec<f64>,
    max_size: usize,
    /// Write cursor — next position to overwrite in the circular buffer.
    cursor: usize,
    /// Number of values added so far (saturates at `max_size`).
    len: usize,
}

impl RollingAverage {
    /// Create a new rolling average with the given window size.
    pub fn new(size: usize) -> Self {
        assert!(size > 0, "RollingAverage size must be > 0");
        Self {
            values: vec![0.0; size],
            max_size: size,
            cursor: 0,
            len: 0,
        }
    }

    /// Push a new sample into the buffer, evicting the oldest if full.
    pub fn add(&mut self, value: f64) {
        self.values[self.cursor] = value;
        self.cursor = (self.cursor + 1) % self.max_size;
        if self.len < self.max_size {
            self.len += 1;
        }
    }

    /// Interquartile mean of the buffered samples.
    ///
    /// Sorts a copy of the live window, trims the bottom and top quarters,
    /// and averages the remaining middle half. Returns `0.0` when empty.
    pub fn average(&self) -> f64 {
        if self.len == 0 {
            return 0.0;
        }

        let mut sorted: Vec<f64> = self.values[..self.len].to_vec();
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        let quarter = sorted.len() / 4;
        let trimmed = &sorted[quarter..sorted.len() - quarter];
        if trimmed.is_empty() {
            // Fewer than 4 samples — just average everything.
            return sorted.iter().sum::<f64>() / sorted.len() as f64;
        }
        trimmed.iter().sum::<f64>() / trimmed.len() as f64
    }

    /// Number of samples currently in the buffer.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Whether the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Reset the buffer, clearing all samples.
    pub fn reset(&mut self) {
        self.cursor = 0;
        self.len = 0;
    }
}

// ── Drift Correction ────────────────────────────────────────────────────────

/// Maximum playrate correction away from 1.0.
/// Keeps playrate in [1 - MAX, 1 + MAX] = [0.5, 1.5].
const MAX_RATE_CORRECTION: f64 = 0.5;

/// Proportional gain — how aggressively to correct.
///
/// With gain=2.0 and 30Hz heartbeat: a 0.1s drift produces playrate=1.2,
/// which closes the gap quickly. Larger drifts are clamped by MAX_RATE_CORRECTION.
const PROPORTIONAL_GAIN: f64 = 2.0;

/// Action to take for drift correction.
#[derive(Debug, Clone, PartialEq)]
pub enum DriftAction {
    /// No correction needed — drift within tolerance and playrate is normal.
    None,
    /// Set playrate to the given value to correct drift.
    SetRate { new_playrate: f64 },
    /// Reset playrate to 1.0 (drift within tolerance, playrate was nudged).
    Reset,
    /// Hard seek to master position (drift exceeds hard-seek threshold).
    HardSeek,
}

/// Drift corrector using proportional control.
///
/// Instead of fixed semitone nudges (which compound and oscillate at 5Hz),
/// computes a playrate proportional to the drift. Larger drift = faster
/// correction, and the correction naturally decreases as positions converge.
///
/// ```text
/// playrate = 1.0 + clamp(drift * gain, -max, +max)
/// ```
pub struct DriftCorrector {
    /// Tolerance in seconds. Drift below this is considered "in sync".
    tolerance: f64,
    /// Hard seek threshold in seconds.
    hard_seek_threshold: f64,
}

impl DriftCorrector {
    /// Create a new drift corrector with default thresholds.
    ///
    /// - Tolerance: 10ms
    /// - Hard seek: 5.0s
    pub fn new() -> Self {
        Self {
            tolerance: 0.01,
            hard_seek_threshold: 5.0,
        }
    }

    /// Create a drift corrector with custom thresholds.
    pub fn with_thresholds(tolerance: f64, hard_seek_threshold: f64) -> Self {
        Self {
            tolerance,
            hard_seek_threshold,
        }
    }

    /// Feed a position difference and get the correction action.
    ///
    /// # Arguments
    /// - `drift_seconds`: `remote_position - local_position`. Positive means
    ///   local is behind the master (needs to speed up).
    /// - `current_playrate`: current playback rate (1.0 = normal speed).
    ///
    /// # Returns
    /// A [`DriftAction`] describing what the caller should do.
    pub fn correct(&mut self, drift_seconds: f64, current_playrate: f64) -> DriftAction {
        let abs_drift = drift_seconds.abs();

        // Way too far off — hard seek
        if abs_drift > self.hard_seek_threshold {
            debug!("Drift {drift_seconds:.3}s exceeds hard-seek threshold");
            return DriftAction::HardSeek;
        }

        if abs_drift > self.tolerance {
            // Proportional correction: playrate = 1.0 + clamp(drift * gain, -max, +max)
            let correction = (drift_seconds * PROPORTIONAL_GAIN)
                .clamp(-MAX_RATE_CORRECTION, MAX_RATE_CORRECTION);
            let new_rate = 1.0 + correction;
            debug!(
                "Drift correction: {drift_seconds:.4}s, playrate {current_playrate:.4} → {new_rate:.4}"
            );
            DriftAction::SetRate {
                new_playrate: new_rate,
            }
        } else if (current_playrate - 1.0).abs() > 0.001 {
            // Within tolerance but playrate was previously adjusted — reset
            debug!("Drift within tolerance ({drift_seconds:.4}s), resetting playrate");
            DriftAction::Reset
        } else {
            DriftAction::None
        }
    }

    /// Reset the corrector state (e.g., on transport stop/start).
    pub fn reset(&mut self) {
        // Proportional control is stateless — nothing to reset.
    }
}

impl Default for DriftCorrector {
    fn default() -> Self {
        Self::new()
    }
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rolling_average_basic() {
        let mut ra = RollingAverage::new(4);
        assert!(ra.is_empty());
        assert_eq!(ra.average(), 0.0);

        ra.add(1.0);
        ra.add(2.0);
        ra.add(3.0);
        let avg = ra.average();
        assert!((avg - 2.0).abs() < 1e-9);
    }

    #[test]
    fn rolling_average_circular_eviction() {
        let mut ra = RollingAverage::new(3);
        ra.add(10.0);
        ra.add(20.0);
        ra.add(30.0);
        ra.add(40.0);
        assert_eq!(ra.len(), 3);
        assert!((ra.average() - 30.0).abs() < 1e-9);
    }

    #[test]
    fn rolling_average_outlier_rejection() {
        let mut ra = RollingAverage::new(8);
        ra.add(100.0);
        ra.add(10.0);
        ra.add(10.0);
        ra.add(10.0);
        ra.add(10.0);
        ra.add(10.0);
        ra.add(10.0);
        ra.add(0.001);
        assert!((ra.average() - 10.0).abs() < 1e-9);
    }

    #[test]
    fn rolling_average_reset() {
        let mut ra = RollingAverage::new(4);
        ra.add(1.0);
        ra.add(2.0);
        assert_eq!(ra.len(), 2);
        ra.reset();
        assert!(ra.is_empty());
        assert_eq!(ra.average(), 0.0);
    }

    // ── DriftCorrector tests ────────────────────────────────────────────

    #[test]
    fn no_correction_when_in_sync() {
        let mut dc = DriftCorrector::new();
        let action = dc.correct(0.0, 1.0);
        assert_eq!(action, DriftAction::None);
    }

    #[test]
    fn proportional_rate_when_behind() {
        let mut dc = DriftCorrector::new();
        let action = dc.correct(0.1, 1.0); // 100ms behind
        match action {
            DriftAction::SetRate { new_playrate } => {
                // 1.0 + 0.1 * 2.0 = 1.2
                assert!(
                    (new_playrate - 1.2).abs() < 0.01,
                    "expected ~1.2, got {new_playrate}"
                );
            }
            other => panic!("expected SetRate, got {other:?}"),
        }
    }

    #[test]
    fn proportional_rate_when_ahead() {
        let mut dc = DriftCorrector::new();
        let action = dc.correct(-0.1, 1.0); // 100ms ahead
        match action {
            DriftAction::SetRate { new_playrate } => {
                // 1.0 + (-0.1 * 2.0) = 0.8
                assert!(
                    (new_playrate - 0.8).abs() < 0.01,
                    "expected ~0.8, got {new_playrate}"
                );
            }
            other => panic!("expected SetRate, got {other:?}"),
        }
    }

    #[test]
    fn rate_clamped_on_large_drift() {
        let mut dc = DriftCorrector::new();
        let action = dc.correct(3.0, 1.0); // 3s behind, 3.0*2.0=6.0 but clamped to 0.5
        match action {
            DriftAction::SetRate { new_playrate } => {
                // Clamped: 1.0 + 0.5 = 1.5
                assert!(
                    (new_playrate - 1.5).abs() < 0.01,
                    "expected 1.5 (clamped), got {new_playrate}"
                );
            }
            other => panic!("expected SetRate, got {other:?}"),
        }
    }

    #[test]
    fn reset_when_converged() {
        let mut dc = DriftCorrector::new();
        // Within tolerance, but playrate still adjusted
        let action = dc.correct(0.001, 1.25);
        assert_eq!(action, DriftAction::Reset);
    }

    #[test]
    fn hard_seek_on_large_drift() {
        let mut dc = DriftCorrector::new();
        let action = dc.correct(6.0, 1.0); // 6 seconds off (threshold is 5s)
        assert_eq!(action, DriftAction::HardSeek);
    }

    #[test]
    fn convergence_simulation() {
        // Simulate a follower that starts 500ms behind the master.
        // Proportional control should converge smoothly without oscillation.
        let mut dc = DriftCorrector::new();
        let mut local_pos = 0.0;
        let mut master_pos = 0.5; // 500ms ahead
        let mut playrate = 1.0;
        let tick_interval = 0.033; // 33ms heartbeat (30Hz)

        let mut max_playrate = 1.0_f64;
        let mut min_playrate = 1.0_f64;
        let mut converged = false;

        for tick in 0..300 {
            let drift = master_pos - local_pos;
            let action = dc.correct(drift, playrate);

            match action {
                DriftAction::SetRate { new_playrate } => playrate = new_playrate,
                DriftAction::Reset => playrate = 1.0,
                DriftAction::None => {}
                DriftAction::HardSeek => {
                    local_pos = master_pos;
                    playrate = 1.0;
                }
            }

            max_playrate = max_playrate.max(playrate);
            min_playrate = min_playrate.min(playrate);

            // Advance positions
            master_pos += tick_interval; // master at 1.0x
            local_pos += tick_interval * playrate; // follower at adjusted rate

            if tick > 3 && drift.abs() < 0.01 {
                converged = true;
                println!("Converged at tick {tick}: drift={drift:.4}s, playrate={playrate:.4}");
                break;
            }
        }

        assert!(converged, "should converge within 300 ticks (~10 seconds)");
        // Playrate should never go below 0.5 or above 1.5 (clamped)
        assert!(
            min_playrate >= 0.5,
            "playrate should not drop below 0.5, got {min_playrate}"
        );
        assert!(
            max_playrate <= 1.5,
            "playrate should not exceed 1.5, got {max_playrate}"
        );
    }

    #[test]
    fn no_oscillation() {
        // Ensure we don't oscillate around zero.
        // Start 200ms behind, track the drift direction changes.
        let mut dc = DriftCorrector::new();
        let mut local_pos = 0.0;
        let mut master_pos = 0.2;
        let mut playrate = 1.0;
        let tick_interval = 0.033;
        let mut sign_changes = 0;
        let mut prev_drift_sign = 1;

        for _ in 0..50 {
            let drift = master_pos - local_pos;
            let action = dc.correct(drift, playrate);

            match action {
                DriftAction::SetRate { new_playrate } => playrate = new_playrate,
                DriftAction::Reset => playrate = 1.0,
                _ => {}
            }

            let current_sign = if drift > 0.001 {
                1
            } else if drift < -0.001 {
                -1
            } else {
                0
            };
            if current_sign != 0 && current_sign != prev_drift_sign {
                sign_changes += 1;
            }
            if current_sign != 0 {
                prev_drift_sign = current_sign;
            }

            master_pos += tick_interval;
            local_pos += tick_interval * playrate;
        }

        // With proportional control, there should be at most 1 sign change
        // (when it crosses zero). No oscillation.
        assert!(
            sign_changes <= 1,
            "too many drift sign changes ({sign_changes}), indicates oscillation"
        );
    }
}
