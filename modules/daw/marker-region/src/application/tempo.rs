//! Tempo and Time Signature domain abstraction
//!
//! This module provides a unified interface for accessing tempo and time signature information
//! from different sources (REAPER API, RPP files, etc.)

use std::fmt;

/// A tempo/time signature change point
#[derive(Debug, Clone, PartialEq)]
pub struct TempoTimePoint {
    /// Position in seconds when this change occurs
    pub position: f64,
    /// Tempo in BPM
    pub tempo: f64,
    /// Envelope shape (0=linear, 1=square, etc.)
    pub shape: Option<i32>,
    /// Time signature as (numerator, denominator) - e.g., (4, 4) or (7, 8)
    pub time_signature: Option<(i32, i32)>,
    /// Whether this point is selected
    pub selected: Option<bool>,
    /// Bezier tension for curves
    pub bezier_tension: Option<f64>,
    /// Metronome pattern (e.g., "ABBB")
    pub metronome_pattern: Option<String>,
}

impl TempoTimePoint {
    /// Create a new tempo/time signature point
    pub fn new(position: f64, tempo: f64) -> Self {
        Self {
            position,
            tempo,
            shape: None,
            time_signature: None,
            selected: None,
            bezier_tension: None,
            metronome_pattern: None,
        }
    }

    /// Create a new tempo/time signature point with full information
    pub fn new_full(
        position: f64,
        tempo: f64,
        shape: Option<i32>,
        time_signature: Option<(i32, i32)>,
        selected: Option<bool>,
        bezier_tension: Option<f64>,
        metronome_pattern: Option<String>,
    ) -> Self {
        Self {
            position,
            tempo,
            shape,
            time_signature,
            selected,
            bezier_tension,
            metronome_pattern,
        }
    }

    /// Get time signature as a string (e.g., "4/4")
    pub fn time_signature_string(&self) -> String {
        if let Some((num, den)) = self.time_signature {
            format!("{}/{}", num, den)
        } else {
            "".to_string()
        }
    }
}

impl fmt::Display for TempoTimePoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Tempo Change at {:.3}s:", self.position)?;
        writeln!(f, "  Tempo: {:.1} BPM", self.tempo)?;
        if let Some((num, den)) = self.time_signature {
            writeln!(f, "  Time Signature: {}/{}", num, den)?;
        }
        if let Some(pattern) = &self.metronome_pattern {
            if !pattern.is_empty() {
                writeln!(f, "  Metronome Pattern: {}", pattern)?;
            }
        }
        if let Some(shape) = self.shape {
            writeln!(f, "  Shape: {}", shape)?;
        }
        if let Some(selected) = self.selected {
            if selected {
                writeln!(f, "  Selected: Yes")?;
            }
        }
        Ok(())
    }
}

/// Collection of tempo and time signature changes
#[derive(Debug, Clone, PartialEq)]
pub struct TempoTimeEnvelope {
    /// All tempo/time signature change points, sorted by position
    pub points: Vec<TempoTimePoint>,
    /// Default tempo (from project properties)
    pub default_tempo: f64,
    /// Default time signature (from project properties)
    pub default_time_signature: (i32, i32),
}

impl TempoTimeEnvelope {
    /// Create a new tempo envelope with defaults
    pub fn new(default_tempo: f64, default_time_signature: (i32, i32)) -> Self {
        Self {
            points: Vec::new(),
            default_tempo,
            default_time_signature,
        }
    }

    /// Add a tempo/time signature change point
    pub fn add_point(&mut self, point: TempoTimePoint) {
        self.points.push(point);
        // Keep points sorted by position
        self.points.sort_by(|a, b| {
            a.position
                .partial_cmp(&b.position)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
    }

    /// Get the tempo and time signature at a given time position
    pub fn get_at_time(&self, time: f64) -> (f64, (i32, i32)) {
        // Find the last point before or at the given time
        let mut current_tempo = self.default_tempo;
        let mut current_time_sig = self.default_time_signature;

        for point in &self.points {
            if point.position <= time {
                current_tempo = point.tempo;
                if let Some(time_sig) = point.time_signature {
                    current_time_sig = time_sig;
                }
            } else {
                break;
            }
        }

        (current_tempo, current_time_sig)
    }

    /// Calculate the total number of beats up to a given time
    /// This integrates tempo changes over time
    pub fn beats_at_time(&self, time: f64) -> f64 {
        if self.points.is_empty() {
            // No tempo changes, simple calculation
            return time * self.default_tempo / 60.0;
        }

        let mut total_beats = 0.0;
        let mut last_time = 0.0;
        let mut current_tempo = self.default_tempo;

        for point in &self.points {
            if point.position <= time {
                // Add beats for the time segment before this point
                let segment_duration = point.position - last_time;
                let segment_beats = segment_duration * current_tempo / 60.0;
                total_beats += segment_beats;

                // Update for next segment
                last_time = point.position;
                current_tempo = point.tempo;
            } else {
                // This point is after our target time, add final segment
                let segment_duration = time - last_time;
                let segment_beats = segment_duration * current_tempo / 60.0;
                total_beats += segment_beats;

                return total_beats;
            }
        }

        // Add final segment if we haven't reached the target time yet
        if last_time < time {
            let segment_duration = time - last_time;
            let segment_beats = segment_duration * current_tempo / 60.0;
            total_beats += segment_beats;
        }

        total_beats
    }

    /// Calculate musical position (measure and beat) at a given time
    /// Returns (measure, beat, beat_fraction) where measure is 1-based
    /// This accounts for tempo and time signature changes throughout the song
    pub fn musical_position_at_time(&self, time: f64) -> (i32, i32, f64) {
        if self.points.is_empty() {
            // No tempo changes, simple calculation with effective tempo
            let tempo_ratio = self.default_time_signature.1 as f64 / 4.0;
            let effective_tempo = self.default_tempo * tempo_ratio;
            let total_beats = time * effective_tempo / 60.0;
            let beats_per_measure = self.default_time_signature.0 as f64;
            let measure = (total_beats / beats_per_measure).floor() as i32 + 1;
            let beat_in_measure = ((total_beats - 1.0) % beats_per_measure + 1.0) as i32;
            let beat_fraction = total_beats
                - (measure - 1) as f64 * beats_per_measure
                - (beat_in_measure - 1) as f64;
            return (measure, beat_in_measure, beat_fraction);
        }

        // Need to account for time signature changes throughout the song
        let mut total_beats = 0.0;
        let mut last_time = 0.0;
        let mut current_tempo = self.default_tempo;
        let mut current_time_sig = self.default_time_signature;
        let mut current_measure = 1.0; // Track measures as we go

        // Add bounds checking to prevent overflow
        let max_measures = 1000.0;

        for point in &self.points {
            if point.position <= time {
                // Add beats for the time segment before this point
                let segment_duration = point.position - last_time;

                // Calculate effective tempo based on time signature
                // Convert tempo to quarter note equivalents based on the note value
                // Ratio = denominator / 4 (e.g., 8/4 = 2.0 for eighth notes, 2/4 = 0.5 for half notes)
                let tempo_ratio = current_time_sig.1 as f64 / 4.0;
                let effective_tempo = current_tempo * tempo_ratio;

                let segment_beats = segment_duration * effective_tempo / 60.0;
                total_beats += segment_beats;

                // Calculate how many measures this segment represents
                let beats_per_measure = current_time_sig.0 as f64;
                let segment_measures = segment_beats / beats_per_measure;
                current_measure += segment_measures;

                // Prevent overflow
                if current_measure > max_measures {
                    current_measure = max_measures;
                }

                // Update for next segment
                last_time = point.position;
                current_tempo = point.tempo;
                if let Some(time_sig) = point.time_signature {
                    current_time_sig = time_sig;
                }
            } else {
                // This point is after our target time, add final segment
                let segment_duration = time - last_time;

                // Calculate effective tempo based on current time signature
                let tempo_ratio = current_time_sig.1 as f64 / 4.0;
                let effective_tempo = current_tempo * tempo_ratio;

                let segment_beats = segment_duration * effective_tempo / 60.0;
                total_beats += segment_beats;

                // Calculate final segment measures
                let beats_per_measure = current_time_sig.0 as f64;
                let segment_measures = segment_beats / beats_per_measure;
                current_measure += segment_measures;

                // Prevent overflow
                if current_measure > max_measures {
                    current_measure = max_measures;
                }

                // Calculate final position with bounds checking
                let measure = (current_measure.floor() as i32 + 1).max(1).min(1000);
                let beat_in_measure =
                    ((current_measure - current_measure.floor()) * beats_per_measure + 1.0) as i32;
                let beat_in_measure = beat_in_measure.max(1).min(beats_per_measure as i32);
                let beat_fraction = (current_measure - current_measure.floor()) * beats_per_measure;

                return (measure, beat_in_measure, beat_fraction);
            }
        }

        // Add final segment if we haven't reached the target time yet
        if last_time < time {
            let segment_duration = time - last_time;

            // Calculate effective tempo based on current time signature
            let tempo_ratio = current_time_sig.1 as f64 / 4.0;
            let effective_tempo = current_tempo * tempo_ratio;

            let segment_beats = segment_duration * effective_tempo / 60.0;
            total_beats += segment_beats;

            let beats_per_measure = current_time_sig.0 as f64;
            let segment_measures = segment_beats / beats_per_measure;
            current_measure += segment_measures;

            // Prevent overflow
            if current_measure > max_measures {
                current_measure = max_measures;
            }
        }

        // Calculate final position with bounds checking
        let measure = (current_measure.floor() as i32 + 1).max(1).min(1000);
        let beat_in_measure =
            ((current_measure - current_measure.floor()) * current_time_sig.0 as f64 + 1.0) as i32;
        let beat_in_measure = beat_in_measure.max(1).min(current_time_sig.0);
        let beat_fraction = (current_measure - current_measure.floor()) * current_time_sig.0 as f64;

        (measure, beat_in_measure, beat_fraction)
    }

    /// Get musical position as a formatted string in REAPER's format (measure.beat.fraction)
    pub fn musical_position_string_at_time(&self, time: f64) -> String {
        let (measure, beat, fraction) = self.musical_position_at_time(time);

        // REAPER format: measure.beat.fraction (e.g., "12.1.00", "14.5.25")
        // Convert fraction to hundredths (0.25 becomes 25)
        let fraction_hundredths = (fraction * 100.0).round() as i32;

        format!("{}.{}.{:02}", measure, beat, fraction_hundredths)
    }
}

impl Default for TempoTimeEnvelope {
    fn default() -> Self {
        Self::new(120.0, (4, 4))
    }
}

impl fmt::Display for TempoTimeEnvelope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Tempo/Time Signature Envelope")?;
        writeln!(
            f,
            "  Default: {} BPM, {}/{}",
            self.default_tempo, self.default_time_signature.0, self.default_time_signature.1
        )?;
        writeln!(f, "  Changes: {} points", self.points.len())?;

        if !self.points.is_empty() {
            writeln!(f)?;
            for point in &self.points {
                write!(f, "{}", point)?;
            }
        }

        Ok(())
    }
}
