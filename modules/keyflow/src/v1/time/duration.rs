//! Musical Duration and Position Types
//!
//! This module defines duration and position structures for musical timing.

use std::fmt;

/// Represents a duration in measure.beats.subdivision format
/// 
/// Examples:
/// - 0.2.00 = half note (2 beats in 4/4)
/// - 0.1.00 = quarter note (1 beat in 4/4) 
/// - 0.0.50 = eighth note (0.5 beats in 4/4)
/// - 0.3.00 = dotted half note (3 beats in 4/4)
/// 
/// In 6/8 time:
/// - 0.2.00 = quarter note (2 eighth-note beats in 6/8)
/// - 0.1.00 = eighth note (1 eighth-note beat in 6/8)
#[derive(Debug, Clone, PartialEq)]
pub struct MusicalDuration {
    pub measures: u32,      // Whole measures (usually 0 for single notes)
    pub beats: u32,         // Main beats (depends on time signature denominator)
    pub subdivision: u32,   // Subdivision of beats (e.g., 50 = half beat, 25 = quarter beat)
}

impl MusicalDuration {
    /// Create a new MusicalDuration
    pub fn new(measures: u32, beats: u32, subdivision: u32) -> Self {
        Self { measures, beats, subdivision }
    }
    
    /// Add two MusicalDuration values together
    /// This method is time signature aware and will properly handle measure overflow
    pub fn add(&self, other: &MusicalDuration, time_signature: (u8, u8)) -> MusicalDuration {
        let (numerator, _denominator) = time_signature;
        let beats_per_measure = numerator as u32;
        
        let mut total_subdivision = self.subdivision + other.subdivision;
        let mut total_beats = self.beats + other.beats;
        let mut total_measures = self.measures + other.measures;
        
        // Handle subdivision overflow (assuming 100 subdivisions per beat)
        if total_subdivision >= 100 {
            total_beats += total_subdivision / 100;
            total_subdivision = total_subdivision % 100;
        }
        
        // Handle beat overflow based on time signature
        if total_beats >= beats_per_measure {
            total_measures += total_beats / beats_per_measure;
            total_beats = total_beats % beats_per_measure;
        }
        
        MusicalDuration {
            measures: total_measures,
            beats: total_beats,
            subdivision: total_subdivision,
        }
    }
    
    /// Convert to total beats as f64
    pub fn to_beats(&self, time_signature: (u8, u8)) -> f64 {
        let (numerator, _denominator) = time_signature;
        let beats_per_measure = numerator as f64;
        
        (self.measures as f64 * beats_per_measure) + 
        (self.beats as f64) + 
        (self.subdivision as f64 / 100.0)
    }
    
    /// Create from total beats
    pub fn from_beats(total_beats: f64, time_signature: (u8, u8)) -> Self {
        let (numerator, _denominator) = time_signature;
        let beats_per_measure = numerator as f64;
        
        // The time signature denominator tells us what note gets the beat
        // 4/4: quarter note gets the beat, so 4 beats = whole note
        // 6/8: eighth note gets the beat, so 6 beats = dotted whole note
        
        let measures = (total_beats / beats_per_measure) as u32;
        let remaining_beats = total_beats - (measures as f64 * beats_per_measure);
        
        // Convert remaining beats to the beat unit of the time signature
        // For 4/4: each beat is a quarter note
        // For 6/8: each beat is an eighth note
        let beats = remaining_beats as u32;
        let subdivision = ((remaining_beats - beats as f64) * 100.0) as u32;
        
        Self { measures, beats, subdivision }
    }
    
    /// Format as string (e.g., "0.2.00")
    pub fn to_string(&self) -> String {
        format!("{}.{}.{:02}", self.measures, self.beats, self.subdivision)
    }
}

impl std::ops::Add for MusicalDuration {
    type Output = MusicalDuration;
    
    fn add(self, other: MusicalDuration) -> MusicalDuration {
        // Simple addition without time signature awareness
        // Assumes 4/4 time (4 beats per measure) for this default implementation
        let mut total_subdivision = self.subdivision + other.subdivision;
        let mut total_beats = self.beats + other.beats;
        let mut total_measures = self.measures + other.measures;
        
        // Handle subdivision overflow (100 subdivisions per beat)
        if total_subdivision >= 100 {
            total_beats += total_subdivision / 100;
            total_subdivision = total_subdivision % 100;
        }
        
        // Handle beat overflow (assumes 4 beats per measure)
        if total_beats >= 4 {
            total_measures += total_beats / 4;
            total_beats = total_beats % 4;
        }
        
        MusicalDuration {
            measures: total_measures,
            beats: total_beats,
            subdivision: total_subdivision,
        }
    }
}

impl fmt::Display for MusicalDuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{:02}", self.measures, self.beats, self.subdivision)
    }
}

/// Represents an absolute position in the entire song
#[derive(Debug, Clone, PartialEq)]
pub struct MusicalPosition {
    pub total_duration: MusicalDuration, // Absolute position in the entire song
    pub section_index: usize,            // Which section this position is in
}

impl MusicalPosition {
    /// Create a new MusicalPosition
    pub fn new(total_duration: MusicalDuration, section_index: usize) -> Self {
        Self {
            total_duration,
            section_index,
        }
    }
    
    /// Create at the start of a section
    pub fn at_section_start(total_duration: MusicalDuration, section_index: usize) -> Self {
        Self {
            total_duration,
            section_index,
        }
    }
}

impl fmt::Display for MusicalPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Section {} @ {}",
            self.section_index,
            self.total_duration
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_musical_duration_add() {
        let d1 = MusicalDuration::new(0, 2, 0);
        let d2 = MusicalDuration::new(0, 2, 0);
        let result = d1.add(&d2, (4, 4));
        
        assert_eq!(result.measures, 1);
        assert_eq!(result.beats, 0);
        assert_eq!(result.subdivision, 0);
    }
    
    #[test]
    fn test_musical_duration_to_beats() {
        let duration = MusicalDuration::new(1, 2, 50);
        let beats = duration.to_beats((4, 4));
        
        assert_eq!(beats, 6.5); // 1 measure (4 beats) + 2 beats + 0.5 beats
    }
    
    #[test]
    fn test_musical_duration_from_beats() {
        let duration = MusicalDuration::from_beats(6.5, (4, 4));
        
        assert_eq!(duration.measures, 1);
        assert_eq!(duration.beats, 2);
        assert_eq!(duration.subdivision, 50);
    }
}

