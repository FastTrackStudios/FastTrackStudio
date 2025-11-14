//! Core Chart structure and display implementation
//!
//! This module defines the Chart struct which holds all parsed chart data
//! and implements the Display trait for pretty-printing.

use std::collections::HashMap;
use std::fmt;
use crate::sections::{Section, SectionType};
use crate::key::keys::{Key, ScaleType, ScaleMode};
use crate::metadata::SongMetadata;
use crate::time::{Tempo, TimeSignature, MusicalDuration};
use crate::chart::types::{ChartSection, Measure, ChordInstance, KeyChange};
use crate::chart::chord_memory::ChordMemory;
use colored::*;

/// The complete parsed chart structure
#[derive(Debug, Clone)]
pub struct Chart {
    pub metadata: SongMetadata,
    pub sections: Vec<ChartSection>,
    pub chord_memory: HashMap<String, String>, // DEPRECATED: Use chord_memory_manager instead
    pub section_chord_memory: HashMap<SectionType, HashMap<String, String>>, // DEPRECATED
    pub section_templates: HashMap<SectionType, Vec<Measure>>, // Reusable chord progressions
    pub template_original_keys: HashMap<SectionType, Key>, // Original keys when templates were created
    pub current_key: Option<Key>,
    pub initial_key: Option<Key>,  // The key at the start of the song
    pub ending_key: Option<Key>,   // The key at the end of the song
    pub key_changes: Vec<KeyChange>, // All key changes throughout the song
    pub tempo: Option<Tempo>,
    pub time_signature: Option<TimeSignature>,
    // New chord memory manager
    pub(crate) chord_memory_manager: ChordMemory,
}

impl Chart {
    /// Create a new empty Chart
    pub fn new() -> Self {
        Self {
            metadata: SongMetadata::default(),
            sections: Vec::new(),
            chord_memory: HashMap::new(),
            section_chord_memory: HashMap::new(),
            section_templates: HashMap::new(),
            template_original_keys: HashMap::new(),
            current_key: None,
            initial_key: None,
            ending_key: None,
            key_changes: Vec::new(),
            tempo: None,
            time_signature: None,
            chord_memory_manager: ChordMemory::new(),
        }
    }

    /// Convert a section to a pretty colored string
    pub fn section_to_pretty_string(&self, section: &Section) -> String {
        let base_name = match &section.section_type {
            SectionType::Intro => "Intro".bright_blue(),
            SectionType::Verse => "Verse".bright_green(),
            SectionType::Chorus => "Chorus".bright_red(),
            SectionType::Bridge => "Bridge".bright_yellow(),
            SectionType::Outro => "Outro".bright_blue(),
            SectionType::Instrumental => "Instrumental".bright_cyan(),
            SectionType::Pre(target) => {
                let target_name = match target.as_ref() {
                    SectionType::Chorus => "Chorus",
                    SectionType::Verse => "Verse",
                    SectionType::Bridge => "Bridge",
                    _ => "Section",
                };
                format!("Pre-{}", target_name).bright_magenta()
            }
            SectionType::Post(target) => {
                let target_name = match target.as_ref() {
                    SectionType::Chorus => "Chorus",
                    SectionType::Verse => "Verse",
                    SectionType::Bridge => "Bridge",
                    _ => "Section",
                };
                format!("Post-{}", target_name).bright_magenta()
            }
        };

        if let Some(number) = section.number {
            format!("{} {}", base_name, number).to_string()
        } else {
            base_name.to_string()
        }
    }

    /// Convert a chord instance to a pretty colored string
    pub fn chord_to_pretty_string(&self, chord: &ChordInstance) -> String {
        let base_chord = if let Ok(degree) = chord.root.original_format.parse::<u8>() {
            if degree >= 1 && degree <= 7 {
                // Format scale degree with colon: "1:maj7"
                // Extract just the quality part, not the root note
                let chord_display = chord.parsed.to_display_name();

                // For scale degrees, we want to show the quality without the root note
                // The chord_display might be something like "C#maj7" but we want just "maj7"
                let quality = if chord_display.len() > 1 {
                    // Find where the quality starts (after the root note and any accidentals)
                    let mut quality_start = 1;
                    // Skip any accidentals in the root note
                    while quality_start < chord_display.len()
                        && (chord_display.chars().nth(quality_start).unwrap() == '#'
                            || chord_display.chars().nth(quality_start).unwrap() == 'b')
                    {
                        quality_start += 1;
                    }
                    &chord_display[quality_start..]
                } else {
                    ""
                };

                if quality.is_empty() {
                    format!("{}", degree)
                } else {
                    format!("{}:{}", degree, quality.bright_green())
                }
            } else {
                // Not a scale degree, use normal chord display
                chord.parsed.to_display_name()
            }
        } else {
            // Not a scale degree, use normal chord display
            chord.parsed.to_display_name()
        };

        // Add duration information if present
        let time_signature = self
            .time_signature
            .unwrap_or(TimeSignature {
                numerator: 4,
                denominator: 4,
            });
        let default_duration = MusicalDuration::from_beats(
            4.0,
            (time_signature.numerator, time_signature.denominator),
        );

        if chord.duration != default_duration {
            // Only show if not default
            format!("{}:{}", base_chord, chord.duration.to_string())
                .bright_cyan()
                .to_string()
        } else {
            base_chord
        }
    }

    /// Convert a key to a pretty colored string
    pub fn key_to_pretty_string(&self, key: &Key) -> String {
        let root = key.root.to_string().bright_white().bold().to_string();
        let mode = match key.scale_type {
            ScaleType::Diatonic => match key.mode {
                ScaleMode::Ionian => "Major".bright_blue(),
                ScaleMode::Aeolian => "Minor".bright_red(),
                _ => "Major".bright_blue(),
            },
            _ => "Major".bright_blue(),
        };

        format!("{} {}", root, mode)
    }

    /// Calculate the duration of a section as a string
    pub fn calculate_section_duration(&self, section: &ChartSection) -> String {
        if section.measures.is_empty() {
            return "0 measures".to_string();
        }

        let time_signature = self.time_signature.unwrap_or(TimeSignature {
            numerator: 4,
            denominator: 4,
        });
        let mut total_beats = 0.0;
        let mut has_any_chords = false;

        for measure in &section.measures {
            if !measure.chords.is_empty() {
                has_any_chords = true;
            }
            for chord in &measure.chords {
                let chord_beats = chord
                    .duration
                    .to_beats((time_signature.numerator, time_signature.denominator));
                total_beats += chord_beats;
            }
        }

        // If no chords in any measure, the duration is the number of measures
        // This represents the explicit length specified (e.g., "intro 2" = 2 measures)
        if !has_any_chords {
            let total_measures = section.measures.len() as u32;
            return if total_measures == 1 {
                "1 measure".to_string()
            } else {
                format!("{} measures", total_measures)
            };
        }

        // Convert total beats to measures
        let beats_per_measure = time_signature.numerator as f64;
        let total_measures = total_beats / beats_per_measure;

        if total_measures == 1.0 {
            "1 measure".to_string()
        } else if total_measures == total_measures.floor() {
            // Whole number of measures
            format!("{} measures", total_measures as u32)
        } else {
            // Fractional measures - show as decimal
            format!("{:.1} measures", total_measures)
        }
    }
}

impl fmt::Display for Chart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Header with title and artist
        if let Some(ref title) = self.metadata.title {
            writeln!(f, "{}", title.bright_white().bold())?;
        }

        if let Some(ref subtitle) = self.metadata.subtitle {
            writeln!(f, "{}", subtitle.bright_cyan())?;
        }

        if let Some(ref artist) = self.metadata.artist {
            writeln!(f, "by {}", artist.bright_blue())?;
        }

        if let Some(ref composer) = self.metadata.composer {
            writeln!(f, "composed by {}", composer.bright_blue())?;
        }

        writeln!(f)?;

        // Song info section
        writeln!(f, "{}", "═".repeat(50).bright_black())?;

        // Tempo
        if let Some(ref tempo) = self.tempo {
            writeln!(
                f,
                "{} {}",
                "Tempo:".bright_yellow().bold(),
                format!("{} bpm", tempo.bpm).bright_white()
            )?;
        }

        // Time signature
        if let Some(ref time_sig) = self.time_signature {
            writeln!(
                f,
                "{} {}",
                "Time:".bright_yellow().bold(),
                format!("{}/{}", time_sig.numerator, time_sig.denominator).bright_white()
            )?;
        }

        // Key signature - show initial key
        if let Some(ref key) = self.initial_key {
            writeln!(
                f,
                "{} {}",
                "Key:".bright_yellow().bold(),
                self.key_to_pretty_string(key).bright_white()
            )?;
        }

        // Show key changes if any
        if !self.key_changes.is_empty() {
            writeln!(
                f,
                "{} {}",
                "Key Changes:".bright_yellow().bold(),
                self.key_changes.len().to_string().bright_white()
            )?;
        }

        writeln!(f)?;

        // Sections breakdown
        writeln!(f, "{}", "SECTIONS".bright_magenta().bold())?;
        writeln!(f, "{}", "─".repeat(20).bright_black())?;

        for (i, section) in self.sections.iter().enumerate() {
            let section_name = self.section_to_pretty_string(&section.section);
            let total_duration = self.calculate_section_duration(section);
            let source_indicator = if section.from_template {
                " [template]".bright_cyan()
            } else {
                " [explicit]".bright_green()
            };

            writeln!(
                f,
                "{}. {} {} {} {}",
                (i + 1).to_string().bright_black(),
                section_name,
                "─"
                    .repeat(30_usize.saturating_sub(section_name.len()))
                    .bright_black(),
                format!("({})", total_duration).bright_black(),
                source_indicator
            )?;

            // Show chord progression for this section with inline key changes
            if !section.measures.is_empty() {
                let mut chord_strings = Vec::new();

                for measure in &section.measures {
                    for chord in measure.chords.iter() {
                        let chord_str = self.chord_to_pretty_string(chord);
                        chord_strings.push(chord_str.clone());

                        // Check if there's a key change after this chord
                        for key_change in &self.key_changes {
                            if key_change.position.section_index == i {
                                let to_key_str = self.key_to_pretty_string(&key_change.to_key);
                                chord_strings.push(format!(
                                    "[{}]",
                                    to_key_str.bright_yellow().bold()
                                ));
                            }
                        }
                    }
                }

                if !chord_strings.is_empty() {
                    writeln!(f, "   {}", chord_strings.join(" ").bright_green())?;
                }
            }
        }

        writeln!(f)?;

        // Key changes detail
        if !self.key_changes.is_empty() {
            writeln!(f, "{}", "KEY CHANGES".bright_yellow().bold())?;
            writeln!(f, "{}", "─".repeat(20).bright_black())?;

            for (i, key_change) in self.key_changes.iter().enumerate() {
                let from_key_str = if let Some(ref from_key) = key_change.from_key {
                    self.key_to_pretty_string(from_key)
                } else {
                    "Unknown".to_string()
                };
                let to_key_str = self.key_to_pretty_string(&key_change.to_key);

                writeln!(
                    f,
                    "{}. {} → {} (Section {}, Position {})",
                    (i + 1).to_string().bright_black(),
                    from_key_str.bright_red(),
                    to_key_str.bright_green(),
                    (key_change.position.section_index + 1)
                        .to_string()
                        .bright_white(),
                    key_change
                        .position
                        .total_duration
                        .to_string()
                        .bright_white()
                )?;
            }

            writeln!(f)?;
        }

        // Chord memory summary
        if !self.chord_memory.is_empty() {
            writeln!(f, "{}", "CHORD MEMORY".bright_cyan().bold())?;
            writeln!(f, "{}", "─".repeat(20).bright_black())?;

            let mut memory_items: Vec<_> = self.chord_memory.iter().collect();
            memory_items.sort_by_key(|(root, _)| *root);

            for (root, full_symbol) in memory_items {
                writeln!(
                    f,
                    "{} → {}",
                    root.bright_white().bold(),
                    full_symbol.bright_green()
                )?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

impl Default for Chart {
    fn default() -> Self {
        Self::new()
    }
}

