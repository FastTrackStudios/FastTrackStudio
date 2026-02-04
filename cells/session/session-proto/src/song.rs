//! Song domain types
//!
//! Represents a song as a structured collection of sections derived from
//! DAW markers and regions.

use daw_proto::{Position, TimeSignature};
use facet::Facet;

/// A comment marker within a song
///
/// Comments are markers that aren't structural (not SONGSTART, SONGEND, =START, =END, COUNT-IN).
/// They serve as reminders or notes displayed above the progress bar.
/// Examples: "Keys only", "Build energy", "Watch conductor", "Count-In" (when mid-song)
///
/// Markers prefixed with `>` are section-only comments (e.g., ">Watch tempo")
/// and will only appear in the section progress bar, not the main song progress bar.
#[derive(Clone, Debug, PartialEq, Facet)]
pub struct Comment {
    /// DAW marker ID (if applicable)
    pub id: Option<u32>,
    /// Comment text (the marker name, with `>` prefix stripped if present)
    pub text: String,
    /// Position in seconds (absolute project time)
    pub position_seconds: f64,
    /// Color for visual representation (from marker color, None for default)
    pub color: Option<u32>,
    /// Whether this is a count-in marker that appears mid-song
    /// (used for special styling)
    pub is_count_in: bool,
    /// Whether this comment should only appear in the section progress bar
    /// (marker was prefixed with `>`)
    pub section_only: bool,
}

impl Comment {
    /// Create a new comment
    pub fn new(text: String, position_seconds: f64) -> Self {
        let (clean_text, section_only) = Self::parse_section_only_prefix(&text);
        Self {
            id: None,
            text: clean_text,
            position_seconds,
            color: None,
            is_count_in: false,
            section_only,
        }
    }

    /// Create a count-in comment (for mid-song count-in markers)
    pub fn count_in(position_seconds: f64) -> Self {
        Self {
            id: None,
            text: "Count-In".to_string(),
            position_seconds,
            color: None,
            is_count_in: true,
            section_only: false,
        }
    }

    /// Parse the `>` prefix from marker text
    /// Returns (cleaned_text, is_section_only)
    fn parse_section_only_prefix(text: &str) -> (String, bool) {
        let trimmed = text.trim();
        if let Some(rest) = trimmed.strip_prefix('>') {
            (rest.trim().to_string(), true)
        } else {
            (trimmed.to_string(), false)
        }
    }

    /// Get the color as a CSS hex string
    pub fn color_hex(&self) -> Option<String> {
        self.color.map(|c| {
            // REAPER colors are in BGR format, convert to RGB hex
            let r = (c >> 16) & 0xFF;
            let g = (c >> 8) & 0xFF;
            let b = c & 0xFF;
            format!("#{:02x}{:02x}{:02x}", b, g, r)
        })
    }
}

/// A section within a song (derived from a region or markers)
///
/// Sections represent structural parts of a song like verses, choruses, bridges.
/// They are typically extracted from DAW regions or marker pairs.
#[derive(Clone, Debug, PartialEq, Facet)]
pub struct Section {
    /// DAW region/marker ID (if applicable)
    pub id: Option<u32>,
    /// Section name (e.g., "Verse 1", "Chorus") - without the comment portion
    pub name: String,
    /// Optional comment/description (e.g., "Woodwinds" from 'Interlude C "Woodwinds"')
    pub comment: Option<String>,
    /// Type of section
    pub section_type: SectionType,
    /// Start position in seconds
    pub start_seconds: f64,
    /// End position in seconds
    pub end_seconds: f64,
    /// Section number (e.g., 1 for "Verse 1", 2 for "Verse 2")
    pub number: Option<u32>,
    /// Color for visual representation (0 for default)
    pub color: Option<u32>,
}

impl Section {
    /// Get the duration of this section in seconds
    pub fn duration(&self) -> f64 {
        self.end_seconds - self.start_seconds
    }

    /// Check if a position falls within this section
    pub fn contains(&self, seconds: f64) -> bool {
        seconds >= self.start_seconds && seconds < self.end_seconds
    }

    /// Get display name for this section (name only, without comment)
    pub fn display_name(&self) -> String {
        self.name.clone()
    }

    /// Get display name with comment if present (e.g., "Interlude C (Woodwinds)")
    pub fn display_name_with_comment(&self) -> String {
        match &self.comment {
            Some(comment) => format!("{} ({})", self.name, comment),
            None => self.name.clone(),
        }
    }

    /// Get a short display name for space-constrained UI contexts
    ///
    /// Converts section names to abbreviated forms:
    /// - "Interlude A" -> "INT A"
    /// - "Verse 1" -> "VS 1"
    /// - "Chorus 2" -> "CH 2"
    /// - "Pre-Chorus" -> "PC"
    /// - "Bridge 1" -> "BR 1"
    /// - "Outro A" -> "OUT A"
    ///
    /// For Other types, attempts to abbreviate to first 3-4 chars + suffix
    pub fn short_display(&self) -> String {
        let abbrev = self.section_type.short_name();

        // Extract any suffix (number or letter) from the name
        // e.g., "Verse 1" -> "1", "Interlude A" -> "A", "Chorus 3B" -> "3B"
        let suffix = self.extract_suffix();

        if suffix.is_empty() {
            abbrev.to_string()
        } else {
            format!("{} {}", abbrev, suffix)
        }
    }

    /// Extract suffix (number/letter) from section name
    fn extract_suffix(&self) -> String {
        let name = &self.name;

        // Find where the suffix starts - look for trailing numbers/letters after a space
        // or just trailing alphanumeric at the end
        if let Some(space_idx) = name.rfind(' ') {
            let suffix = name[space_idx + 1..].trim();
            // Check if this looks like a suffix (number, letter, or combo like "3A")
            if !suffix.is_empty() && suffix.chars().all(|c| c.is_ascii_alphanumeric()) {
                return suffix.to_string();
            }
        }

        // Also handle cases like "Verse1" without space
        let trailing: String = name
            .chars()
            .rev()
            .take_while(|c| c.is_ascii_digit() || c.is_ascii_uppercase())
            .collect::<String>()
            .chars()
            .rev()
            .collect();

        // Only return if it's not the whole name and is a reasonable suffix
        if !trailing.is_empty() && trailing.len() < name.len() && trailing.len() <= 3 {
            trailing
        } else {
            String::new()
        }
    }

    /// Get bright color for UI display
    pub fn bright_color(&self) -> String {
        match &self.section_type {
            SectionType::CountIn => "#f472b6".to_string(), // pink-400 (count-in)
            SectionType::Intro => "#3b82f6".to_string(),   // blue
            SectionType::Verse => "#22c55e".to_string(),   // green
            SectionType::PreChorus => "#eab308".to_string(), // yellow
            SectionType::Chorus => "#ef4444".to_string(),  // red
            SectionType::Bridge => "#a855f7".to_string(),  // purple
            SectionType::Outro => "#6366f1".to_string(),   // indigo
            SectionType::Solo => "#f97316".to_string(),    // orange
            SectionType::Breakdown => "#ec4899".to_string(), // pink (different shade for breakdown)
            SectionType::Instrumental => "#14b8a6".to_string(), // teal
            SectionType::Interlude => "#06b6d4".to_string(), // cyan
            SectionType::Vamp => "#84cc16".to_string(),    // lime
            SectionType::End => "#374151".to_string(),     // gray-700 (muted, tail section)
            SectionType::Other(_) => "#64748b".to_string(), // slate
        }
    }

    /// Get muted color for UI display
    pub fn muted_color(&self) -> String {
        match &self.section_type {
            SectionType::CountIn => "#9d174d".to_string(), // pink-800 (count-in)
            SectionType::Intro => "#1e3a8a".to_string(),   // blue-900
            SectionType::Verse => "#166534".to_string(),   // green-900
            SectionType::PreChorus => "#713f12".to_string(), // yellow-900
            SectionType::Chorus => "#7f1d1d".to_string(),  // red-900
            SectionType::Bridge => "#581c87".to_string(),  // purple-900
            SectionType::Outro => "#312e81".to_string(),   // indigo-900
            SectionType::Solo => "#7c2d12".to_string(),    // orange-900
            SectionType::Breakdown => "#831843".to_string(), // pink-900 (different shade for breakdown)
            SectionType::Instrumental => "#134e4a".to_string(), // teal-900
            SectionType::Interlude => "#164e63".to_string(), // cyan-900
            SectionType::Vamp => "#365314".to_string(),      // lime-900
            SectionType::End => "#1f2937".to_string(),       // gray-800 (muted, tail section)
            SectionType::Other(_) => "#334155".to_string(),  // slate-700
        }
    }

    /// Calculate progress percentage (0-100) based on transport position
    pub fn progress(&self, transport_position: f64) -> f64 {
        let section_duration = self.duration();

        if section_duration <= 0.0 {
            return 0.0;
        }

        let relative_position = (transport_position - self.start_seconds).max(0.0);
        (relative_position / section_duration).min(1.0) * 100.0
    }
}

/// Type of section in a song
#[repr(u8)]
#[derive(Clone, Debug, PartialEq, Eq, Facet)]
pub enum SectionType {
    /// Count-in section - from COUNT-IN marker to SONGSTART
    /// This is the pre-roll before the song content begins
    CountIn,
    Intro,
    Verse,
    PreChorus,
    Chorus,
    Bridge,
    Outro,
    Solo,
    Breakdown,
    Instrumental,
    /// Interlude - transitional section between main parts
    Interlude,
    /// Vamp - repeated section, often for improvisation or transitions
    Vamp,
    /// Special END section - the tail from SONGEND to =END marker
    /// This captures reverb/sustain after the musical content ends
    End,
    /// Custom section type not in the standard list
    Other(String),
}

impl SectionType {
    /// Parse a section type from a string (case-insensitive)
    ///
    /// Recognizes common abbreviations:
    /// - "COUNT-IN" or "COUNTIN" -> CountIn
    /// - "V" or "Verse" -> Verse
    /// - "C" or "Chorus" -> Chorus
    /// - "PC" or "Pre-Chorus" -> PreChorus
    /// - "B" or "Bridge" -> Bridge
    /// - "I" or "Intro" -> Intro
    /// - "O" or "Outro" -> Outro
    /// - "INT" or "Interlude" -> Interlude
    /// - "VAMP" -> Vamp
    /// - "END" -> End (special tail section)
    pub fn parse(s: &str) -> Self {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "count-in" | "countin" | "count in" => SectionType::CountIn,
            "intro" => SectionType::Intro,
            "verse" | "v" => SectionType::Verse,
            "prechorus" | "pre-chorus" | "pc" => SectionType::PreChorus,
            "chorus" | "c" => SectionType::Chorus,
            "bridge" | "b" => SectionType::Bridge,
            "outro" | "o" => SectionType::Outro,
            "solo" | "s" => SectionType::Solo,
            "breakdown" | "bd" => SectionType::Breakdown,
            "instrumental" | "inst" => SectionType::Instrumental,
            "interlude" | "int" => SectionType::Interlude,
            "vamp" => SectionType::Vamp,
            "end" => SectionType::End,
            _ => SectionType::Other(s.to_string()),
        }
    }

    /// Convert to display string
    pub fn as_str(&self) -> &str {
        match self {
            SectionType::CountIn => "Count-In",
            SectionType::Intro => "Intro",
            SectionType::Verse => "Verse",
            SectionType::PreChorus => "Pre-Chorus",
            SectionType::Chorus => "Chorus",
            SectionType::Bridge => "Bridge",
            SectionType::Outro => "Outro",
            SectionType::Solo => "Solo",
            SectionType::Breakdown => "Breakdown",
            SectionType::Instrumental => "Instrumental",
            SectionType::Interlude => "Interlude",
            SectionType::Vamp => "Vamp",
            SectionType::End => "End",
            SectionType::Other(s) => s,
        }
    }

    /// Get abbreviated name for space-constrained UI
    ///
    /// Returns short 2-4 character abbreviations:
    /// - Verse -> VS
    /// - Chorus -> CH
    /// - Pre-Chorus -> PC
    /// - Bridge -> BR
    /// - Intro -> IN
    /// - Outro -> OUT
    /// - Solo -> SOL
    /// - Breakdown -> BD
    /// - Instrumental -> INST
    /// - Interlude -> INT
    /// - Vamp -> VMP
    pub fn short_name(&self) -> &str {
        match self {
            SectionType::CountIn => "CI",
            SectionType::Intro => "IN",
            SectionType::Verse => "VS",
            SectionType::PreChorus => "PC",
            SectionType::Chorus => "CH",
            SectionType::Bridge => "BR",
            SectionType::Outro => "OUT",
            SectionType::Solo => "SOL",
            SectionType::Breakdown => "BD",
            SectionType::Instrumental => "INST",
            SectionType::Interlude => "INT",
            SectionType::Vamp => "VMP",
            SectionType::End => "END",
            SectionType::Other(s) => {
                // For custom types, check common patterns
                let lower = s.to_lowercase();
                if lower.starts_with("tag") {
                    "TAG"
                } else if lower.starts_with("hook") {
                    "HK"
                } else if lower.starts_with("drop") {
                    "DRP"
                } else if lower.starts_with("build") {
                    "BLD"
                } else if lower.starts_with("riff") {
                    "RIF"
                } else {
                    // Return first 3-4 chars uppercased
                    // We can't return a dynamic string here, so return the original
                    s
                }
            }
        }
    }
}

/// A song in the setlist
///
/// Songs are extracted from DAW projects using markers and regions.
/// A song typically has a start marker (SONGSTART), end marker (SONGEND),
/// and regions defining its internal structure (sections).
#[derive(Clone, Debug, PartialEq, Facet)]
pub struct Song {
    /// Unique identifier for this song (UUID or custom string)
    pub id: String,
    /// Song name
    pub name: String,
    /// DAW project GUID this song belongs to
    pub project_guid: String,
    /// Start position in seconds (from SONGSTART marker or region start)
    pub start_seconds: f64,
    /// End position in seconds (from SONGEND marker or region end)
    pub end_seconds: f64,
    /// Count-in duration before song start (optional)
    pub count_in_seconds: Option<f64>,
    /// Sections within this song
    pub sections: Vec<Section>,
    /// Comment markers within this song (non-structural markers)
    pub comments: Vec<Comment>,
    /// Tempo at song start (if available)
    pub tempo: Option<f64>,
    /// Time signature at song start (if available)
    pub time_signature: Option<TimeSignature>,
    /// Measure positions within this song (for beat grid display)
    pub measure_positions: Vec<Position>,
}

impl Song {
    /// Get the total duration of the song in seconds
    pub fn duration(&self) -> f64 {
        self.end_seconds - self.start_seconds
    }

    /// Get the duration including count-in
    pub fn duration_with_count_in(&self) -> f64 {
        self.duration() + self.count_in_seconds.unwrap_or(0.0)
    }

    /// Find the section at a given absolute position (in project time)
    pub fn section_at(&self, seconds: f64) -> Option<&Section> {
        self.sections.iter().find(|s| s.contains(seconds))
    }

    /// Find the section at a given position, returning both index and reference
    pub fn section_at_position(&self, seconds: f64) -> Option<&Section> {
        self.section_at(seconds)
    }

    /// Find the section at a given position with its index
    pub fn section_at_position_with_index(&self, seconds: f64) -> Option<(usize, &Section)> {
        self.sections
            .iter()
            .enumerate()
            .find(|(_, s)| s.contains(seconds))
    }

    /// Get start position in seconds (for compatibility with method-style access)
    pub fn start_seconds(&self) -> f64 {
        self.start_seconds
    }

    /// Get end position in seconds (for compatibility with method-style access)
    pub fn end_seconds(&self) -> f64 {
        self.end_seconds
    }

    /// Get song-relative position (0.0 = start of song)
    pub fn relative_position(&self, absolute_seconds: f64) -> f64 {
        absolute_seconds - self.start_seconds
    }

    /// Get absolute position from song-relative position
    pub fn absolute_position(&self, relative_seconds: f64) -> f64 {
        self.start_seconds + relative_seconds
    }

    /// Get bright color for UI display (uses first section's color or default)
    pub fn bright_color(&self) -> String {
        self.sections
            .first()
            .map(|s| s.bright_color())
            .unwrap_or_else(|| "#3b82f6".to_string()) // default blue
    }

    /// Get muted color for UI display (uses first section's color or default)
    pub fn muted_color(&self) -> String {
        self.sections
            .first()
            .map(|s| s.muted_color())
            .unwrap_or_else(|| "#1e3a8a".to_string()) // default blue-900
    }

    /// Calculate progress percentage (0-100) based on transport position
    pub fn progress(&self, transport_position: f64) -> f64 {
        let song_duration = self.duration();

        if song_duration <= 0.0 {
            return 0.0;
        }

        let relative_position = (transport_position - self.start_seconds).max(0.0);
        (relative_position / song_duration).min(1.0) * 100.0
    }
}
