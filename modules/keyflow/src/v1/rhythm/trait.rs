//! Rhythm trait for musical elements
//!
//! This trait provides a common interface for any musical element that has
//! rhythmic information (duration and position in time).

use crate::time::{MusicalDuration, MusicalPosition};

/// Trait for musical elements that have rhythm (duration and position)
pub trait Rhythm {
    /// Get the duration of this element
    fn duration(&self) -> &MusicalDuration;
    
    /// Get the position of this element in the song
    fn position(&self) -> &MusicalPosition;
    
    /// Set the position of this element
    fn set_position(&mut self, position: MusicalPosition);
    
    /// Calculate the end position based on duration and current position
    fn end_position(&self) -> MusicalPosition {
        let mut end_pos = self.position().clone();
        end_pos.total_duration = end_pos.total_duration.clone() + self.duration().clone();
        end_pos
    }
    
    /// Check if this element spans multiple measures
    fn spans_multiple_measures(&self) -> bool {
        self.duration().measures > 0 || 
        (self.duration().measures == 0 && self.duration().beats >= 4)
    }
}

/// Extension trait for parsing rhythmic elements from strings
pub trait RhythmParser: Sized {
    /// Parse a rhythmic element from a string
    /// 
    /// The `time_signature` parameter is optional and may be needed for
    /// context-dependent parsing (e.g., slash notation that depends on meter)
    fn parse_with_rhythm(input: &str, time_signature: Option<(u8, u8)>) -> Result<Self, String>;
}

#[cfg(test)]
mod tests {
    use super::*;
    
    // Example implementation for testing
    #[derive(Debug, Clone)]
    struct TestRhythmicElement {
        duration: MusicalDuration,
        position: MusicalPosition,
    }
    
    impl Rhythm for TestRhythmicElement {
        fn duration(&self) -> &MusicalDuration {
            &self.duration
        }
        
        fn position(&self) -> &MusicalPosition {
            &self.position
        }
        
        fn set_position(&mut self, position: MusicalPosition) {
            self.position = position;
        }
    }
    
    #[test]
    fn test_end_position_calculation() {
        let element = TestRhythmicElement {
            duration: MusicalDuration::new(1, 2, 0),
            position: MusicalPosition {
                total_duration: MusicalDuration::new(0, 0, 0),
                section_index: 0,
            },
        };
        
        let end_pos = element.end_position();
        assert_eq!(end_pos.total_duration.measures, 1);
        assert_eq!(end_pos.total_duration.beats, 2);
    }
    
    #[test]
    fn test_spans_multiple_measures() {
        let element = TestRhythmicElement {
            duration: MusicalDuration::new(2, 0, 0),
            position: MusicalPosition {
                total_duration: MusicalDuration::new(0, 0, 0),
                section_index: 0,
            },
        };
        
        assert!(element.spans_multiple_measures());
        
        let element2 = TestRhythmicElement {
            duration: MusicalDuration::new(0, 2, 0),
            position: MusicalPosition {
                total_duration: MusicalDuration::new(0, 0, 0),
                section_index: 0,
            },
        };
        
        assert!(!element2.spans_multiple_measures());
    }
}

