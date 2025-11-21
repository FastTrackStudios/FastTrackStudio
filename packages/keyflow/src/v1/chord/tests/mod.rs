//! Chord test cases organized by category
//! This module contains comprehensive test cases for all chord types
//! using semitone sequences, root intervals, and sequential intervals.

use crate::primitives::note::Note;
use crate::primitives::intervals::Interval;
use crate::chord::chord::Chord;

/// A test case for chord naming and conversion
#[derive(Debug, Clone)]
pub struct ChordTestCase {
    pub name: String,
    pub chord: String, // The expected display name
    pub constructor: fn() -> String, // Function that creates the chord
    pub semitone_sequence: Vec<u32>, // Semitone intervals from root (e.g., [0, 4, 7] for major)
    pub root_intervals: Vec<String>, // Intervals from root (e.g., ["Root", "Major 3rd", "Perfect 5th"])
    pub sequential_intervals: Vec<String>, // Intervals between consecutive notes (e.g., ["Major 3rd", "Minor 3rd"])
}

/// Helper function to create semitone sequence from intervals
pub fn semitones_from_intervals(intervals: &[Interval]) -> Vec<u32> {
    intervals.iter().map(|i| i.st() as u32).collect()
}

/// Helper function to create root interval names from intervals
pub fn root_interval_names(intervals: &[Interval]) -> Vec<String> {
    intervals.iter().map(|i| format!("{:?}", i)).collect()
}

/// Helper function to create sequential interval names
pub fn sequential_interval_names(intervals: &[Interval]) -> Vec<String> {
    if intervals.len() < 2 {
        return Vec::new();
    }
    
    let mut sequential = Vec::new();
    for i in 1..intervals.len() {
        let prev_semitones = intervals[i-1].st() as i32;
        let curr_semitones = intervals[i].st() as i32;
        let diff = (curr_semitones - prev_semitones + 12) % 12;
        
        // Use the new from_semitones function
        if let Some(interval) = Interval::from_semitones(diff as u8) {
            sequential.push(format!("{:?}", interval));
        }
    }
    sequential
}

pub mod major;
pub mod minor;
pub mod dominant;
pub mod diminished;
pub mod augmented;
pub mod suspended;
pub mod altered;
pub mod additions;
pub mod sixth;
pub mod slash;
pub mod omitted;
