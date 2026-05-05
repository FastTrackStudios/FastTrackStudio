use facet::Facet;

/// A song section inside a music project.
///
/// This is intended for section-level structure like verse / chorus / bridge
/// plus attached lyric notes or chart snippets.
#[derive(Clone, Debug, PartialEq, Facet)]
pub struct SongSection {
    /// Display name, e.g. `Verse 1` or `Chorus`.
    pub name: String,
    /// Canonical section type from Keyflow.
    pub section_type: crate::SectionType,
    /// Optional lyrics for this section, represented as a keyflow lyric line.
    pub lyrics: Option<crate::LyricLine>,
    /// Optional chart or arrangement notes for this section.
    pub chart_notes: Option<String>,
    /// Freeform notes for production/editing/mix decisions.
    pub notes: Option<String>,
}

impl SongSection {
    pub fn new(name: impl Into<String>, section_type: crate::SectionType) -> Self {
        Self {
            name: name.into(),
            section_type,
            lyrics: None,
            chart_notes: None,
            notes: None,
        }
    }

    pub fn with_lyrics(mut self, lyrics: crate::LyricLine) -> Self {
        self.lyrics = Some(lyrics);
        self
    }

    pub fn with_chart_notes(mut self, chart_notes: impl Into<String>) -> Self {
        self.chart_notes = Some(chart_notes.into());
        self
    }

    pub fn with_notes(mut self, notes: impl Into<String>) -> Self {
        self.notes = Some(notes.into());
        self
    }
}
