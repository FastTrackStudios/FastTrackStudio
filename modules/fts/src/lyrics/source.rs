//! Source-of-truth lyrics system
//!
//! This module implements a single-source-of-truth architecture where:
//! - `LyricsSheet` is the raw text (source of truth)
//! - `LyricsAnnotations` stores user edits (timing, notes, slide breaks)
//! - Everything else is derived dynamically from these two

use super::core::Lyrics;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use thiserror::Error;
use uuid::Uuid;

/// Represents the source of truth - the raw lyrics text
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LyricsSheet {
    /// Unique identifier for this lyrics sheet
    pub id: Uuid,
    /// Song name
    pub song_name: String,
    /// The raw text content (source of truth)
    pub raw_text: String,
    /// Version number (increments on each edit)
    pub version: u64,
    /// Optional metadata
    pub metadata: HashMap<String, String>,
}

impl LyricsSheet {
    /// Create a new lyrics sheet from raw text
    pub fn new(song_name: String, raw_text: String) -> Self {
        Self {
            id: Uuid::new_v4(),
            song_name,
            raw_text,
            version: 1,
            metadata: HashMap::new(),
        }
    }

    /// Update the raw text (increments version)
    pub fn update_text(&mut self, new_text: String) {
        self.raw_text = new_text;
        self.version += 1;
    }

    /// Get metadata value
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }

    /// Set metadata value
    pub fn set_metadata<K: Into<String>, V: Into<String>>(&mut self, key: K, value: V) {
        self.metadata.insert(key.into(), value.into());
    }
}

/// Time format for lyrics synchronization
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TimeFormat {
    /// Linear Time Code (LTC) - HH:MM:SS:FF format
    LTC,
    /// Lyrics format (LRC) - MM:SS.CC format (minutes:seconds.centiseconds)
    LRC,
    /// Seconds as f64
    Seconds,
}

/// Represents a time value in various formats
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LyricTime {
    /// The time value in seconds (canonical format)
    pub seconds: f64,
    /// The original format
    pub format: TimeFormat,
}

impl LyricTime {
    /// Create from seconds
    pub fn from_seconds(seconds: f64) -> Self {
        Self {
            seconds,
            format: TimeFormat::Seconds,
        }
    }

    /// Create from LRC format (MM:SS.CC)
    pub fn from_lrc(lrc: &str) -> Result<Self, TimeParseError> {
        // Parse MM:SS.CC format
        let parts: Vec<&str> = lrc.split(':').collect();
        if parts.len() != 2 {
            return Err(TimeParseError::InvalidFormat);
        }

        let minutes: u64 = parts[0]
            .parse()
            .map_err(|_| TimeParseError::InvalidFormat)?;
        let sec_parts: Vec<&str> = parts[1].split('.').collect();
        if sec_parts.len() != 2 {
            return Err(TimeParseError::InvalidFormat);
        }

        let seconds: u64 = sec_parts[0]
            .parse()
            .map_err(|_| TimeParseError::InvalidFormat)?;
        let centiseconds: u64 = sec_parts[1]
            .parse()
            .map_err(|_| TimeParseError::InvalidFormat)?;

        let total_seconds = (minutes * 60) as f64 + seconds as f64 + (centiseconds as f64 / 100.0);

        Ok(Self {
            seconds: total_seconds,
            format: TimeFormat::LRC,
        })
    }

    /// Format as LRC (MM:SS.CC)
    pub fn to_lrc(&self) -> String {
        let total_seconds = self.seconds;
        let minutes = (total_seconds / 60.0) as u64;
        let seconds = (total_seconds % 60.0) as u64;
        let centiseconds = ((total_seconds % 1.0) * 100.0) as u64;

        format!("{:02}:{:02}.{:02}", minutes, seconds, centiseconds)
    }

    /// Format as LTC (HH:MM:SS:FF) - assumes 30fps
    pub fn to_ltc(&self) -> String {
        let total_seconds = self.seconds;
        let hours = (total_seconds / 3600.0) as u64;
        let minutes = ((total_seconds % 3600.0) / 60.0) as u64;
        let seconds = (total_seconds % 60.0) as u64;
        let frames = ((total_seconds % 1.0) * 30.0) as u64; // 30fps

        format!("{:02}:{:02}:{:02}:{:02}", hours, minutes, seconds, frames)
    }
}

#[derive(Debug, Error)]
pub enum TimeParseError {
    #[error("Invalid time format")]
    InvalidFormat,
}

/// User annotations for lyrics (timing, notes, slide breaks, etc.)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LyricsAnnotations {
    /// Unique identifier matching the LyricsSheet
    pub sheet_id: Uuid,
    /// Version of the sheet this annotation is for
    pub sheet_version: u64,
    /// Line-based syllable timings (keyed by line text content)
    /// This allows timing to stay with the line even if it moves around
    pub line_syllable_timings: HashMap<String, LineSyllableTiming>,
    /// MIDI note assignments per line (keyed by line text + word index + syllable index)
    pub line_syllable_notes: HashMap<String, HashMap<(usize, usize), MidiNote>>,
    /// Custom slide breaks (overrides auto-generation)
    pub slide_breaks: Vec<SlideBreak>,
    /// Section-to-song-section mappings
    pub section_mappings: HashMap<usize, Uuid>, // section index -> song section id
    /// Optional metadata
    pub metadata: HashMap<String, String>,
    /// Legacy: Syllable timings (keyed by section index, line index, word index, syllable index)
    /// Kept for backward compatibility during migration
    #[serde(default)]
    pub syllable_timings: HashMap<SyllableKey, SyllableTiming>,
    /// Legacy: MIDI note assignments per syllable
    /// Kept for backward compatibility during migration
    #[serde(default)]
    pub syllable_notes: HashMap<SyllableKey, MidiNote>,
}

/// Timing information for all syllables in a line
/// Keyed by line text content, so timing stays with the line even if it moves
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LineSyllableTiming {
    /// The line text this timing applies to (normalized - regular text only)
    pub line_text: String,
    /// Timing for each word in the line
    /// Vec index = word index, contains timing for each syllable in that word
    pub word_timings: Vec<Vec<SyllableTiming>>,
}

/// Key for identifying a specific syllable in the lyrics structure (legacy)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SyllableKey {
    /// Section index
    pub section_idx: usize,
    /// Line index within section
    pub line_idx: usize,
    /// Word index within line
    pub word_idx: usize,
    /// Syllable index within word
    pub syllable_idx: usize,
}

/// Timing information for a syllable
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SyllableTiming {
    /// Start time
    pub start: LyricTime,
    /// End time
    pub end: LyricTime,
}

/// MIDI note information for a syllable
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MidiNote {
    /// MIDI note number (0-127)
    pub note: u8,
    /// MIDI velocity (0-127)
    pub velocity: u8,
}

/// Custom slide break (overrides auto-generation)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SlideBreak {
    /// Section index
    pub section_idx: usize,
    /// Line index where the break occurs (this line starts a new slide)
    pub line_idx: usize,
}

impl LyricsAnnotations {
    /// Create new annotations for a lyrics sheet
    pub fn new(sheet_id: Uuid, sheet_version: u64) -> Self {
        Self {
            sheet_id,
            sheet_version,
            line_syllable_timings: HashMap::new(),
            line_syllable_notes: HashMap::new(),
            slide_breaks: Vec::new(),
            section_mappings: HashMap::new(),
            metadata: HashMap::new(),
            syllable_timings: HashMap::new(), // Legacy
            syllable_notes: HashMap::new(),   // Legacy
        }
    }

    /// Set timing for all syllables in a line
    /// The line_text should be the normalized regular text (without parenthetical parts)
    pub fn set_line_syllable_timing(
        &mut self,
        line_text: String,
        word_timings: Vec<Vec<SyllableTiming>>,
    ) {
        self.line_syllable_timings.insert(
            line_text.clone(),
            LineSyllableTiming {
                line_text,
                word_timings,
            },
        );
    }

    /// Get timing for a line (if annotated)
    pub fn get_line_syllable_timing(&self, line_text: &str) -> Option<&LineSyllableTiming> {
        self.line_syllable_timings.get(line_text)
    }

    /// Set MIDI note for a syllable in a line
    pub fn set_line_syllable_note(
        &mut self,
        line_text: String,
        word_idx: usize,
        syllable_idx: usize,
        note: u8,
        velocity: u8,
    ) {
        self.line_syllable_notes
            .entry(line_text)
            .or_insert_with(HashMap::new)
            .insert((word_idx, syllable_idx), MidiNote { note, velocity });
    }

    /// Get MIDI note for a syllable in a line
    pub fn get_line_syllable_note(
        &self,
        line_text: &str,
        word_idx: usize,
        syllable_idx: usize,
    ) -> Option<&MidiNote> {
        self.line_syllable_notes
            .get(line_text)
            .and_then(|notes| notes.get(&(word_idx, syllable_idx)))
    }

    /// Legacy: Set timing for a syllable (old key-based system)
    pub fn set_syllable_timing(&mut self, key: SyllableKey, start: LyricTime, end: LyricTime) {
        self.syllable_timings
            .insert(key, SyllableTiming { start, end });
    }

    /// Legacy: Set MIDI note for a syllable (old key-based system)
    pub fn set_syllable_note(&mut self, key: SyllableKey, note: u8, velocity: u8) {
        self.syllable_notes.insert(key, MidiNote { note, velocity });
    }

    /// Add a custom slide break
    pub fn add_slide_break(&mut self, section_idx: usize, line_idx: usize) {
        self.slide_breaks.push(SlideBreak {
            section_idx,
            line_idx,
        });
    }

    /// Map a lyrics section to a song section
    pub fn map_section(&mut self, section_idx: usize, song_section_id: Uuid) {
        self.section_mappings.insert(section_idx, song_section_id);
    }
}

/// Derived lyrics structure - computed from LyricsSheet + LyricsAnnotations
#[derive(Debug, Clone)]
pub struct DerivedLyrics {
    /// The parsed lyrics structure
    pub lyrics: Lyrics,
    /// The annotations used to derive this
    pub annotations: LyricsAnnotations,
}

impl DerivedLyrics {
    /// Derive lyrics from a sheet and annotations
    pub fn from_sheet_and_annotations(
        sheet: &LyricsSheet,
        annotations: LyricsAnnotations,
        parser_config: crate::lyrics::parser::ParserConfig,
    ) -> Result<Self, crate::lyrics::parser::ParseError> {
        // Parse the raw text
        let mut lyrics = crate::lyrics::parser::parse_lyrics_with_config(
            &sheet.raw_text,
            sheet.song_name.clone(),
            parser_config,
        )?;

        // Apply annotations to the parsed lyrics
        Self::apply_annotations(&mut lyrics, &annotations);

        Ok(Self {
            lyrics,
            annotations,
        })
    }

    /// Apply annotations to parsed lyrics
    fn apply_annotations(lyrics: &mut Lyrics, annotations: &LyricsAnnotations) {
        // Apply syllable timings and notes
        // Note: This requires reconstructing words from lines, which we'll do on-demand
        // For now, we store the annotations separately and apply them when generating outputs

        // Apply section mappings
        for (section_idx, song_section_id) in &annotations.section_mappings {
            if let Some(section) = lyrics.sections.get_mut(*section_idx) {
                section.set_metadata("song_section_id", song_section_id.to_string());
            }
        }
    }

    /// Get timing for a specific syllable (if annotated)
    pub fn get_syllable_timing(&self, key: &SyllableKey) -> Option<&SyllableTiming> {
        self.annotations.syllable_timings.get(key)
    }

    /// Get MIDI note for a specific syllable (if annotated)
    pub fn get_syllable_note(&self, key: &SyllableKey) -> Option<&MidiNote> {
        self.annotations.syllable_notes.get(key)
    }

    /// Check if a slide break is custom (user-defined)
    pub fn is_custom_slide_break(&self, section_idx: usize, line_idx: usize) -> bool {
        self.annotations
            .slide_breaks
            .iter()
            .any(|b| b.section_idx == section_idx && b.line_idx == line_idx)
    }

    /// Update the source text and reconcile annotations
    pub fn update_source(
        &mut self,
        new_text: String,
        parser_config: crate::lyrics::parser::ParserConfig,
    ) -> Result<ReconciliationResult, crate::lyrics::parser::ParseError> {
        // Parse new text
        let new_lyrics = crate::lyrics::parser::parse_lyrics_with_config(
            &new_text,
            self.lyrics.song_name.clone(),
            parser_config,
        )?;

        // Reconcile annotations
        let result = Self::reconcile_annotations(&self.lyrics, &new_lyrics, &mut self.annotations);

        // Update lyrics
        self.lyrics = new_lyrics;
        Self::apply_annotations(&mut self.lyrics, &self.annotations);

        Ok(result)
    }

    /// Reconcile annotations when source text changes
    fn reconcile_annotations(
        old_lyrics: &Lyrics,
        new_lyrics: &Lyrics,
        annotations: &mut LyricsAnnotations,
    ) -> ReconciliationResult {
        let mut result = ReconciliationResult::default();

        // Try to match sections by name and position
        let mut matched_sections: Vec<(usize, usize)> = Vec::new(); // (old_idx, new_idx)

        for (old_idx, old_section) in old_lyrics.sections.iter().enumerate() {
            // Try to find matching section in new lyrics
            for (new_idx, new_section) in new_lyrics.sections.iter().enumerate() {
                if old_section.name == new_section.name
                    && old_section.lines.len() == new_section.lines.len()
                {
                    // Potential match - check if lines are similar
                    let lines_match = old_section.lines.iter().zip(new_section.lines.iter()).all(
                        |(old_line, new_line)| old_line.regular_text() == new_line.regular_text(),
                    );

                    if lines_match {
                        matched_sections.push((old_idx, new_idx));
                        break;
                    }
                }
            }
        }

        // Update syllable keys for matched sections
        let mut new_timings = HashMap::new();
        let mut new_notes = HashMap::new();

        for (old_section_idx, new_section_idx) in &matched_sections {
            // Remap syllable keys
            for (key, timing) in annotations.syllable_timings.iter() {
                if key.section_idx == *old_section_idx {
                    let new_key = SyllableKey {
                        section_idx: *new_section_idx,
                        line_idx: key.line_idx,
                        word_idx: key.word_idx,
                        syllable_idx: key.syllable_idx,
                    };
                    new_timings.insert(new_key, timing.clone());
                }
            }

            for (key, note) in annotations.syllable_notes.iter() {
                if key.section_idx == *old_section_idx {
                    let new_key = SyllableKey {
                        section_idx: *new_section_idx,
                        line_idx: key.line_idx,
                        word_idx: key.word_idx,
                        syllable_idx: key.syllable_idx,
                    };
                    new_notes.insert(new_key, note.clone());
                }
            }

            // Update section mappings
            if let Some(song_section_id) = annotations.section_mappings.remove(old_section_idx) {
                annotations
                    .section_mappings
                    .insert(*new_section_idx, song_section_id);
            }
        }

        // Calculate lost annotations before updating
        let old_timing_count = annotations.syllable_timings.len();
        let new_timing_count = new_timings.len();

        // Update annotations
        annotations.syllable_timings = new_timings;
        annotations.syllable_notes = new_notes;

        // Update slide breaks
        let mut new_slide_breaks = Vec::new();
        for break_point in &annotations.slide_breaks {
            if let Some((_, new_section_idx)) = matched_sections
                .iter()
                .find(|(old_idx, _)| *old_idx == break_point.section_idx)
            {
                new_slide_breaks.push(SlideBreak {
                    section_idx: *new_section_idx,
                    line_idx: break_point.line_idx,
                });
            }
        }
        annotations.slide_breaks = new_slide_breaks;

        result.matched_sections = matched_sections.len();
        result.lost_annotations = old_timing_count.saturating_sub(new_timing_count);

        result
    }
}

/// Result of reconciling annotations after source text changes
#[derive(Debug, Clone, Default)]
pub struct ReconciliationResult {
    /// Number of sections that were successfully matched
    pub matched_sections: usize,
    /// Number of annotations that were lost (couldn't be matched)
    pub lost_annotations: usize,
}
