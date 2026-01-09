// Text Cues Parser
//
// Parses text cues for different instrument groups:
// - @all "Intro Groove!"
// - @keys "arps here"
// - @drums "Big Fill!"
// - @bass "Walking bass"
// - @guitar "Solo starts"
//
// From test.ly, cues are positioned at specific measures and can target
// different instrument groups with custom colors.

use std::fmt;

/// Instrument group that a cue targets
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstrumentGroup {
    All,
    Keys,
    Drums,
    Bass,
    Guitar,
    Vocals,
    Custom(String),
}

impl InstrumentGroup {
    /// Parse an instrument group from string
    pub fn parse(input: &str) -> Result<Self, String> {
        let input = input.trim().to_lowercase();

        match input.as_str() {
            "all" => Ok(InstrumentGroup::All),
            "keys" | "keyboard" | "piano" => Ok(InstrumentGroup::Keys),
            "drums" | "drum" | "percussion" => Ok(InstrumentGroup::Drums),
            "bass" => Ok(InstrumentGroup::Bass),
            "guitar" | "gtr" => Ok(InstrumentGroup::Guitar),
            "vocals" | "vox" | "voice" => Ok(InstrumentGroup::Vocals),
            _ => Ok(InstrumentGroup::Custom(input.to_string())),
        }
    }

    /// Get default color for this instrument group (RGB format)
    /// These match the colors from test.ly
    pub fn default_color(&self) -> (f32, f32, f32) {
        match self {
            InstrumentGroup::All => (0.898, 0.129, 0.0), // Red-orange
            InstrumentGroup::Drums => (1.0, 0.471, 0.0), // Orange
            InstrumentGroup::Bass => (0.569, 0.255, 0.675), // Purple
            InstrumentGroup::Guitar => (0.208, 0.518, 0.894), // Blue
            InstrumentGroup::Keys => (0.149, 0.635, 0.412), // Green
            InstrumentGroup::Vocals => (0.8, 0.2, 0.8),  // Magenta
            InstrumentGroup::Custom(_) => (0.5, 0.5, 0.5), // Gray
        }
    }

    /// Get display name
    pub fn display_name(&self) -> &str {
        match self {
            InstrumentGroup::All => "All",
            InstrumentGroup::Keys => "Keys",
            InstrumentGroup::Drums => "Drums",
            InstrumentGroup::Bass => "Bass",
            InstrumentGroup::Guitar => "Guitar",
            InstrumentGroup::Vocals => "Vocals",
            InstrumentGroup::Custom(name) => name,
        }
    }
}

impl fmt::Display for InstrumentGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_name())
    }
}

/// Represents a text cue at a specific position
#[derive(Debug, Clone, PartialEq)]
pub struct TextCue {
    pub group: InstrumentGroup,
    pub text: String,
    pub measure: Option<u32>,           // Optional measure number
    pub beat: Option<f32>,              // Optional beat within measure (0.0 = start of measure)
    pub color: Option<(f32, f32, f32)>, // Optional custom color (RGB 0.0-1.0)
}

impl TextCue {
    /// Create a new text cue
    pub fn new(group: InstrumentGroup, text: String) -> Self {
        Self {
            group,
            text,
            measure: None,
            beat: None,
            color: None,
        }
    }

    /// Set the measure number
    pub fn at_measure(mut self, measure: u32) -> Self {
        self.measure = Some(measure);
        self
    }

    /// Set the beat within the measure
    pub fn at_beat(mut self, beat: f32) -> Self {
        self.beat = Some(beat);
        self
    }

    /// Set a custom color
    pub fn with_color(mut self, r: f32, g: f32, b: f32) -> Self {
        self.color = Some((r, g, b));
        self
    }

    /// Get the effective color (custom or default)
    pub fn effective_color(&self) -> (f32, f32, f32) {
        self.color.unwrap_or_else(|| self.group.default_color())
    }

    /// Parse a text cue line
    ///
    /// Format: @group "text"
    /// Examples:
    /// - @all "Intro Groove!"
    /// - @keys "arps here"
    /// - @drums "Big Fill!"
    pub fn parse(input: &str) -> Result<Self, String> {
        let input = input.trim();

        // Must start with @
        if !input.starts_with('@') {
            return Err(format!("Text cue must start with @: '{}'", input));
        }

        // Find the space between @group and "text"
        let rest = &input[1..];

        // Split on first whitespace or quote
        let mut parts = rest.splitn(2, |c: char| c.is_whitespace() || c == '"');

        let group_str = parts
            .next()
            .ok_or_else(|| "Missing instrument group".to_string())?;

        if group_str.is_empty() {
            return Err("Instrument group is empty".to_string());
        }

        let group = InstrumentGroup::parse(group_str)?;

        // Find the quoted text
        let text_part = parts
            .next()
            .ok_or_else(|| "Missing text content".to_string())?
            .trim();

        // Extract text from quotes
        let text = if text_part.starts_with('"') {
            // Find closing quote
            if let Some(end_quote) = text_part[1..].find('"') {
                text_part[1..=end_quote].to_string()
            } else {
                return Err("Unterminated quote in text cue".to_string());
            }
        } else {
            // No quotes, take everything
            text_part.to_string()
        };

        if text.is_empty() {
            return Err("Text cue content is empty".to_string());
        }

        Ok(TextCue::new(group, text))
    }

    /// Convert to Lilypond markup
    ///
    /// Output: \markup \with-color #(rgb-color 0.898 0.129 0.0) "Intro Groove!"
    pub fn to_lilypond_markup(&self) -> String {
        let (r, g, b) = self.effective_color();
        format!(
            "\\markup \\with-color #(rgb-color {} {} {}) \"{}\"",
            r, g, b, self.text
        )
    }
}

impl fmt::Display for TextCue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{} \"{}\"", self.group, self.text)
    }
}

/// Collection of text cues
#[derive(Debug, Clone)]
pub struct TextCueList {
    pub cues: Vec<TextCue>,
}

impl TextCueList {
    pub fn new() -> Self {
        Self { cues: Vec::new() }
    }

    /// Add a cue to the list
    pub fn add(&mut self, cue: TextCue) {
        self.cues.push(cue);
    }

    /// Get all cues for a specific instrument group
    pub fn for_group(&self, group: &InstrumentGroup) -> Vec<&TextCue> {
        self.cues
            .iter()
            .filter(|c| &c.group == group || matches!(c.group, InstrumentGroup::All))
            .collect()
    }

    /// Get all cues at a specific measure
    pub fn at_measure(&self, measure: u32) -> Vec<&TextCue> {
        self.cues
            .iter()
            .filter(|c| c.measure == Some(measure))
            .collect()
    }

    /// Convert to Lilypond textCuesAtMeasures format
    ///
    /// From test.ly format:
    /// ```lilypond
    /// allNotesCues = \textCuesAtMeasures #'(
    ///   (27 . "Intro Groove!")
    ///   (76 . "Band Hits!")
    /// ) #(rgb-color 0.898 0.129 0.0)
    /// ```
    pub fn to_lilypond_function(&self, group: &InstrumentGroup, var_name: &str) -> String {
        let cues_for_group: Vec<&TextCue> = self.for_group(group);

        if cues_for_group.is_empty() {
            return String::new();
        }

        let mut result = format!("{} = \\textCuesAtMeasures #'(\n", var_name);

        for cue in &cues_for_group {
            if let Some(measure) = cue.measure {
                result.push_str(&format!("  ({} . \"{}\")\n", measure, cue.text));
            }
        }

        let (r, g, b) = group.default_color();
        result.push_str(&format!(") #(rgb-color {} {} {})\n", r, g, b));

        result
    }
}

impl Default for TextCueList {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_instrument_group() {
        assert_eq!(InstrumentGroup::parse("all").unwrap(), InstrumentGroup::All);
        assert_eq!(
            InstrumentGroup::parse("keys").unwrap(),
            InstrumentGroup::Keys
        );
        assert_eq!(
            InstrumentGroup::parse("keyboard").unwrap(),
            InstrumentGroup::Keys
        );
        assert_eq!(
            InstrumentGroup::parse("drums").unwrap(),
            InstrumentGroup::Drums
        );
        assert_eq!(
            InstrumentGroup::parse("bass").unwrap(),
            InstrumentGroup::Bass
        );
        assert_eq!(
            InstrumentGroup::parse("guitar").unwrap(),
            InstrumentGroup::Guitar
        );
        assert_eq!(
            InstrumentGroup::parse("gtr").unwrap(),
            InstrumentGroup::Guitar
        );
    }

    #[test]
    fn test_parse_custom_instrument_group() {
        match InstrumentGroup::parse("horns").unwrap() {
            InstrumentGroup::Custom(name) => assert_eq!(name, "horns"),
            _ => panic!("Expected custom group"),
        }
    }

    #[test]
    fn test_instrument_group_colors() {
        let all = InstrumentGroup::All;
        let (r, g, b) = all.default_color();
        assert_eq!((r, g, b), (0.898, 0.129, 0.0));

        let drums = InstrumentGroup::Drums;
        let (r, g, b) = drums.default_color();
        assert_eq!((r, g, b), (1.0, 0.471, 0.0));
    }

    #[test]
    fn test_parse_text_cue_with_quotes() {
        let cue = TextCue::parse("@all \"Intro Groove!\"").unwrap();
        assert_eq!(cue.group, InstrumentGroup::All);
        assert_eq!(cue.text, "Intro Groove!");
    }

    #[test]
    fn test_parse_text_cue_different_groups() {
        let cue = TextCue::parse("@keys \"arps here\"").unwrap();
        assert_eq!(cue.group, InstrumentGroup::Keys);
        assert_eq!(cue.text, "arps here");

        let cue = TextCue::parse("@drums \"Big Fill!\"").unwrap();
        assert_eq!(cue.group, InstrumentGroup::Drums);
        assert_eq!(cue.text, "Big Fill!");
    }

    #[test]
    fn test_parse_text_cue_without_quotes() {
        let cue = TextCue::parse("@bass Walking bass").unwrap();
        assert_eq!(cue.group, InstrumentGroup::Bass);
        assert_eq!(cue.text, "Walking bass");
    }

    #[test]
    fn test_parse_text_cue_invalid() {
        assert!(TextCue::parse("no at symbol").is_err());
        assert!(TextCue::parse("@ \"no group\"").is_err());
    }

    #[test]
    fn test_text_cue_with_measure() {
        let cue = TextCue::parse("@all \"Test\"").unwrap().at_measure(27);

        assert_eq!(cue.measure, Some(27));
    }

    #[test]
    fn test_text_cue_with_beat() {
        let cue = TextCue::parse("@keys \"Test\"").unwrap().at_beat(2.5);

        assert_eq!(cue.beat, Some(2.5));
    }

    #[test]
    fn test_text_cue_with_custom_color() {
        let cue = TextCue::parse("@guitar \"Solo\"")
            .unwrap()
            .with_color(1.0, 0.0, 0.0);

        assert_eq!(cue.effective_color(), (1.0, 0.0, 0.0));
    }

    #[test]
    fn test_text_cue_default_color() {
        let cue = TextCue::parse("@drums \"Fill\"").unwrap();
        assert_eq!(cue.effective_color(), (1.0, 0.471, 0.0));
    }

    #[test]
    fn test_text_cue_to_lilypond_markup() {
        let cue = TextCue::parse("@all \"Intro Groove!\"").unwrap();
        let markup = cue.to_lilypond_markup();

        assert!(markup.contains("\\markup"));
        assert!(markup.contains("\\with-color"));
        assert!(markup.contains("rgb-color"));
        assert!(markup.contains("Intro Groove!"));
    }

    #[test]
    fn test_text_cue_list() {
        let mut list = TextCueList::new();

        list.add(TextCue::parse("@all \"Test 1\"").unwrap().at_measure(1));
        list.add(TextCue::parse("@keys \"Test 2\"").unwrap().at_measure(2));
        list.add(TextCue::parse("@all \"Test 3\"").unwrap().at_measure(3));

        let all_cues = list.for_group(&InstrumentGroup::All);
        assert_eq!(all_cues.len(), 2);

        let at_measure_2 = list.at_measure(2);
        assert_eq!(at_measure_2.len(), 1);
    }

    #[test]
    fn test_text_cue_display() {
        let cue = TextCue::parse("@drums \"Big Fill!\"").unwrap();
        assert_eq!(cue.to_string(), "@Drums \"Big Fill!\"");
    }

    #[test]
    fn test_to_lilypond_function() {
        let mut list = TextCueList::new();

        list.add(
            TextCue::parse("@all \"Intro Groove!\"")
                .unwrap()
                .at_measure(27),
        );
        list.add(
            TextCue::parse("@all \"Band Hits!\"")
                .unwrap()
                .at_measure(76),
        );

        let lilypond = list.to_lilypond_function(&InstrumentGroup::All, "allNotesCues");

        assert!(lilypond.contains("allNotesCues = \\textCuesAtMeasures"));
        assert!(lilypond.contains("(27 . \"Intro Groove!\")"));
        assert!(lilypond.contains("(76 . \"Band Hits!\")"));
        assert!(lilypond.contains("rgb-color"));
    }
}
