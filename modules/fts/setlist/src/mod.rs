//! Setlist domain abstraction
//!
//! This module provides a unified interface for accessing setlists, songs, and sections
//! from different sources (REAPER API, RPP files, etc.)

use std::collections::HashMap;
use crate::marker_region::{Marker, Region};

#[cfg(feature = "reaper")]
pub mod reaper;

pub mod order;

/// Represents different types of song sections with their full names and abbreviations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SectionType {
    Verse,
    Chorus,
    Bridge,
    Intro,
    Outro,
    Instrumental,
    Pre(Box<SectionType>),   // Pre-Chorus, Pre-Verse, etc.
    Post(Box<SectionType>),  // Post-Chorus, Post-Verse, etc.
}

impl SectionType {
    /// Get the full name of the section (dynamic for Pre/Post)
    pub fn full_name(&self) -> String {
        match self {
            SectionType::Verse => "Verse".to_string(),
            SectionType::Chorus => "Chorus".to_string(),
            SectionType::Bridge => "Bridge".to_string(),
            SectionType::Intro => "Intro".to_string(),
            SectionType::Outro => "Outro".to_string(),
            SectionType::Instrumental => "Instrumental".to_string(),
            SectionType::Pre(inner) => format!("Pre-{}", inner.full_name()),
            SectionType::Post(inner) => format!("Post-{}", inner.full_name()),
        }
    }

    /// Get the abbreviated name of the section (always capitalized)
    /// 
    /// Returns uppercase abbreviations:
    /// - Verse -> "VS"
    /// - Chorus -> "CH"
    /// - Bridge -> "BR"
    /// - Intro -> "IN"
    /// - Outro -> "OUT"
    /// - Instrumental -> "INST"
    /// - Pre-Chorus -> "PRE-CH"
    /// - Post-Verse -> "POST-VS"
    pub fn abbreviation(&self) -> String {
        match self {
            SectionType::Verse => "VS".to_string(),
            SectionType::Chorus => "CH".to_string(),
            SectionType::Bridge => "BR".to_string(),
            SectionType::Intro => "IN".to_string(),
            SectionType::Outro => "OUT".to_string(),
            SectionType::Instrumental => "INST".to_string(),
            SectionType::Pre(inner) => format!("PRE-{}", inner.abbreviation()),
            SectionType::Post(inner) => format!("POST-{}", inner.abbreviation()),
        }
    }

    /// Try to parse a section type from a string (name or abbreviation)
    /// 
    /// Handles case-insensitive matching and common typos/variations:
    /// - "verse", "Verse", "VERSE", "vs", "VS", "vErSe", "vrse" -> Verse
    /// - "chorus", "Chorus", "CHORUS", "ch", "CH", "chorous", "corus" -> Chorus
    /// - etc.
    pub fn from_str(s: &str) -> Option<Self> {
        let s_lower = s.to_lowercase();
        let s_lower = s_lower.trim();
        
        // Try exact matches first (case-insensitive)
        match s_lower {
            "verse" | "vs" | "v" => return Some(SectionType::Verse),
            "chorus" | "ch" | "c" => return Some(SectionType::Chorus),
            "bridge" | "br" | "b" => return Some(SectionType::Bridge),
            "intro" | "in" | "i" => return Some(SectionType::Intro),
            "outro" | "out" | "o" => return Some(SectionType::Outro),
            "instrumental" | "inst" | "instrument" => return Some(SectionType::Instrumental),
            _ => {}
        }
        
        // Try fuzzy matching for common typos and variations
        // Verse variations
        if Self::fuzzy_match(&s_lower, "verse", &["vrse", "verce", "vers", "versa"]) {
            return Some(SectionType::Verse);
        }
        
        // Chorus variations
        if Self::fuzzy_match(&s_lower, "chorus", &["chorous", "corus", "chrous", "chors", "chor"]) {
            return Some(SectionType::Chorus);
        }
        
        // Bridge variations
        if Self::fuzzy_match(&s_lower, "bridge", &["bridg", "brige", "brid"]) {
            return Some(SectionType::Bridge);
        }
        
        // Intro variations - handle "introduction", "intro", etc.
        if Self::fuzzy_match(&s_lower, "intro", &["intr", "int", "introo", "introduction"]) {
            return Some(SectionType::Intro);
        }
        // Also check if it starts with "introduction"
        if s_lower.starts_with("introduction") {
            return Some(SectionType::Intro);
        }
        
        // Outro variations - handle "outroduction", "outro", etc.
        if Self::fuzzy_match(&s_lower, "outro", &["outr", "out", "outroo", "outroduction"]) {
            return Some(SectionType::Outro);
        }
        // Also check if it starts with "outroduction"
        if s_lower.starts_with("outroduction") {
            return Some(SectionType::Outro);
        }
        
        // Instrumental variations
        if Self::fuzzy_match(&s_lower, "instrumental", &["instumental", "instrumantal", "instrument"]) {
            return Some(SectionType::Instrumental);
        }
        
        // Try to parse Pre/Post
        if let Some(rest) = s_lower.strip_prefix("pre-") {
            if let Some(inner) = Self::from_str(rest) {
                return Some(SectionType::Pre(Box::new(inner)));
            }
        }
        if let Some(rest) = s_lower.strip_prefix("post-") {
            if let Some(inner) = Self::from_str(rest) {
                return Some(SectionType::Post(Box::new(inner)));
            }
        }
        
        None
    }
    
    /// Fuzzy matching helper - checks if the input matches the target or any variations
    fn fuzzy_match(input: &str, target: &str, variations: &[&str]) -> bool {
        // Exact match with target
        if input == target {
            return true;
        }
        
        // Check if input starts with target (allows for trailing characters like numbers)
        if input.starts_with(target) {
            return true;
        }
        
        // Check variations
        for variation in variations {
            if input == *variation || input.starts_with(variation) {
                return true;
            }
        }
        
        // Check if input is close enough to target (simple edit distance check)
        // For very short strings, just check if first few chars match
        if input.len() >= 3 && target.len() >= 3 {
            let input_prefix = &input[..input.len().min(3)];
            let target_prefix = &target[..target.len().min(3)];
            if input_prefix == target_prefix {
                return true;
            }
        }
        
        false
    }
}

/// Represents a section within a song
#[derive(Debug, Clone, PartialEq)]
pub struct Section {
    /// Section type
    pub section_type: SectionType,
    /// Optional section number (e.g., 1, 2, 3 for Verse 1, Verse 2, etc.)
    pub number: Option<u32>,
    /// Split letter for consecutive sections (e.g., 'a', 'b', 'c')
    pub split_letter: Option<char>,
    /// Start position in seconds
    pub start_seconds: f64,
    /// End position in seconds
    pub end_seconds: f64,
    /// Section name (from region name)
    pub name: String,
}

impl Section {
    /// Create a new section
    pub fn new(
        section_type: SectionType,
        start_seconds: f64,
        end_seconds: f64,
        name: String,
        number: Option<u32>,
    ) -> Self {
        Self {
            section_type,
            number,
            split_letter: None,
            start_seconds,
            end_seconds,
            name,
        }
    }

    /// Calculate the duration of the section in seconds
    pub fn duration(&self) -> f64 {
        self.end_seconds - self.start_seconds
    }

    /// Generate the display name for the section
    /// 
    /// Rules:
    /// - Intro and Outro: Use full names ("Intro", "Outro")
    /// - Everything else: Use abbreviations ("VS 1", "CH 2", "BR")
    /// - Include number and split letter if present
    pub fn display_name(&self) -> String {
        let base_name = match &self.section_type {
            SectionType::Intro | SectionType::Outro => self.section_type.full_name(),
            _ => self.section_type.abbreviation(),
        };
        
        // Add number if present
        let with_number = if let Some(num) = self.number {
            format!("{} {}", base_name, num)
        } else {
            base_name
        };
        
        // Add split letter if present
        if let Some(letter) = self.split_letter {
            format!("{}{}", with_number, letter)
        } else {
            with_number
        }
    }
}

/// Represents a song in the setlist
#[derive(Debug, Clone, PartialEq)]
pub struct Song {
    /// Song name (from the song region name)
    pub name: String,
    /// Count-In marker position in seconds (marker named "Count-In")
    pub count_in_position: Option<f64>,
    /// SONGSTART marker position in seconds (render start, where real song content begins)
    pub start_position: Option<f64>,
    /// SONGEND marker position in seconds (last hit of song, but may have audio after)
    pub song_end_position: Option<f64>,
    /// =END marker position in seconds (hard cut, no more audio)
    pub end_position: Option<f64>,
    /// Song region (the entire song region)
    pub song_region: Option<Region>,
    /// List of sections in the song (from regions between SONGSTART and SONGEND)
    pub sections: Vec<Section>,
}

impl Song {
    /// Create a new song
    pub fn new(name: String) -> Self {
        Self {
            name,
            count_in_position: None,
            start_position: None,
            song_end_position: None,
            end_position: None,
            song_region: None,
            sections: Vec::new(),
        }
    }

    /// Get the effective start position (either SONGSTART or song region start)
    pub fn effective_start(&self) -> f64 {
        self.start_position
            .or_else(|| self.song_region.as_ref().map(|r| r.start_seconds()))
            .unwrap_or(0.0)
    }

    /// Get the effective end position (either =END or SONGEND or song region end)
    pub fn effective_end(&self) -> f64 {
        self.end_position
            .or_else(|| self.song_end_position)
            .or_else(|| self.song_region.as_ref().map(|r| r.end_seconds()))
            .unwrap_or(0.0)
    }

    /// Get the render start position (SONGSTART or song region start if no SONGSTART)
    pub fn render_start(&self) -> f64 {
        self.start_position
            .or_else(|| self.song_region.as_ref().map(|r| r.start_seconds()))
            .unwrap_or(0.0)
    }

    /// Get the render end position (SONGEND or song region end if no SONGEND)
    pub fn render_end(&self) -> f64 {
        self.song_end_position
            .or_else(|| self.song_region.as_ref().map(|r| r.end_seconds()))
            .unwrap_or(0.0)
    }

    /// Get the hard cut position (=END or song region end if no =END)
    pub fn hard_cut(&self) -> f64 {
        self.end_position
            .or_else(|| self.song_region.as_ref().map(|r| r.end_seconds()))
            .unwrap_or(0.0)
    }

    /// Auto-number sections that don't have numbers
    /// 
    /// Rules:
    /// - Intro and Outro are never numbered
    /// - Sections of the same type are numbered sequentially (1, 2, 3, etc.)
    /// - If there are consecutive sections of the same type, they get split letters (a, b, c)
    pub fn auto_number_sections(&mut self) {
        // Count occurrences of each section type
        let mut type_counts: HashMap<SectionType, u32> = HashMap::new();
        for section in &self.sections {
            let stype = &section.section_type;
            if !matches!(stype, SectionType::Intro | SectionType::Outro | 
                         SectionType::Pre(_) | SectionType::Post(_)) {
                *type_counts.entry(stype.clone()).or_insert(0) += 1;
            }
        }
        
        // Assign numbers and split letters
        let mut counters: HashMap<SectionType, u32> = HashMap::new();
        let mut prev_type: Option<SectionType> = None;
        let mut consecutive_count = 0;
        let mut current_group_number: Option<u32> = None;
        
        for i in 0..self.sections.len() {
            let stype = self.sections[i].section_type.clone();
            
            // Don't number Intro/Outro/Pre/Post
            if matches!(stype, SectionType::Intro | SectionType::Outro | 
                        SectionType::Pre(_) | SectionType::Post(_)) {
                prev_type = Some(stype);
                consecutive_count = 0;
                current_group_number = None;
                continue;
            }
            
            // Only number if appears more than once
            let total_count = type_counts.get(&stype).copied().unwrap_or(0);
            let should_number = total_count > 1;
            
            // Check if consecutive
            let is_consecutive = prev_type.as_ref() == Some(&stype);
            
            if is_consecutive {
                consecutive_count += 1;
                
                // First consecutive: setup group
                if consecutive_count == 1 && i > 0 {
                    if let Some(prev_num) = self.sections[i - 1].number {
                        current_group_number = Some(prev_num);
                    } else if should_number {
                        let count = counters.entry(stype.clone()).or_insert(0);
                        *count += 1;
                        current_group_number = Some(*count);
                        self.sections[i - 1].number = Some(*count);
                    }
                    self.sections[i - 1].split_letter = Some('a');
                    consecutive_count = 2;
                }
                
                // Assign split letter
                let letter = (b'a' + consecutive_count as u8 - 1) as char;
                self.sections[i].split_letter = Some(letter);
                if should_number {
                    self.sections[i].number = current_group_number;
                }
            } else {
                // Not consecutive
                consecutive_count = 0;
                current_group_number = None;
                
                if should_number {
                    let count = counters.entry(stype.clone()).or_insert(0);
                    *count += 1;
                    self.sections[i].number = Some(*count);
                }
            }
            
            prev_type = Some(stype);
        }
    }
}

/// Represents a complete setlist
#[derive(Debug, Clone, PartialEq)]
pub struct Setlist {
    /// List of songs in the setlist
    pub songs: Vec<Song>,
}

impl Setlist {
    /// Create a new empty setlist
    pub fn new() -> Self {
        Self {
            songs: Vec::new(),
        }
    }

    /// Add a song to the setlist
    pub fn add_song(&mut self, song: Song) {
        self.songs.push(song);
    }

    /// Get the total number of songs
    pub fn song_count(&self) -> usize {
        self.songs.len()
    }

    /// Get a song by index
    pub fn get_song(&self, index: usize) -> Option<&Song> {
        self.songs.get(index)
    }

    /// Get a song by name
    pub fn get_song_by_name(&self, name: &str) -> Option<&Song> {
        self.songs.iter().find(|s| s.name == name)
    }
}

impl Default for Setlist {
    fn default() -> Self {
        Self::new()
    }
}

/// Trait for sources that can provide setlist information
pub trait SetlistSource {
    /// Build a setlist from markers and regions
    fn build_setlist(&self) -> Result<Setlist, Box<dyn std::error::Error>>;

    /// Get the name of this source (for debugging/logging)
    fn source_name(&self) -> &'static str;
}

#[cfg(feature = "reaper")]
pub use reaper::ReaperSetlistSource;

pub use order::{SetlistOrder, SetlistEntry};

