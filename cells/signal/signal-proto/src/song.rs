//! Song types — performance structure with sections and overrides.
//!
//! A [`Song`] contains [`SongSection`]s. Each section references a scene
//! (at any hierarchy level) and can apply validated overrides on top.


use crate::id::{SongId, SongSectionId};
use crate::module::MidiTriggerConfig;
use crate::override_tree::{SceneOverride, Validated};
use crate::scene::{SceneTransition, ScopedSceneRef};
use crate::tags::Tags;

// ─── Song ────────────────────────────────────────────────────────

/// A performance song with named sections.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct Song {
    pub id: SongId,
    pub name: String,
    pub artist: Option<String>,
    pub sections: Vec<SongSection>,
    pub default_section_index: usize,
    pub tags: Tags,
}

impl Song {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: SongId::new(),
            name: name.into(),
            artist: None,
            sections: Vec::new(),
            default_section_index: 0,
            tags: Tags::new(),
        }
    }

    #[must_use]
    pub fn with_artist(mut self, artist: impl Into<String>) -> Self {
        self.artist = Some(artist.into());
        self
    }

    pub fn add_section(&mut self, section: SongSection) {
        self.sections.push(section);
    }

    pub fn section(&self, index: usize) -> Option<&SongSection> {
        self.sections.get(index)
    }

    pub fn default_section(&self) -> Option<&SongSection> {
        self.sections.get(self.default_section_index)
    }
}

// ─── SongSection ─────────────────────────────────────────────────

/// A section within a song — references a scene and applies overrides.
///
/// **Key typestate guarantee**: `overrides` can only hold `SceneOverride<Validated>`.
/// You cannot add an unvalidated override — it's a compile error.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct SongSection {
    pub id: SongSectionId,
    pub name: String,
    pub scene_ref: ScopedSceneRef,
    pub overrides: Vec<SceneOverride<Validated>>,
    pub transition: SceneTransition,
    pub midi_triggers: Vec<MidiTriggerConfig>,
    pub tags: Tags,
}

impl SongSection {
    /// Create a new section with a scene reference and no overrides.
    pub fn new(name: impl Into<String>, scene_ref: ScopedSceneRef) -> Self {
        Self {
            id: SongSectionId::new(),
            name: name.into(),
            scene_ref,
            overrides: Vec::new(),
            transition: SceneTransition::default(),
            midi_triggers: Vec::new(),
            tags: Tags::new(),
        }
    }

    /// Add a validated override.
    pub fn add_override(&mut self, ov: SceneOverride<Validated>) {
        self.overrides.push(ov);
    }

    /// Builder: add a validated override.
    #[must_use]
    pub fn with_override(mut self, ov: SceneOverride<Validated>) -> Self {
        self.overrides.push(ov);
        self
    }

    /// Builder: set the transition.
    #[must_use]
    pub fn with_transition(mut self, transition: SceneTransition) -> Self {
        self.transition = transition;
        self
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id::EngineSceneId;
    use crate::version::VersionedRef;

    #[test]
    fn song_creation() {
        let song = Song::new("Amazing Grace").with_artist("Traditional");
        assert_eq!(song.name, "Amazing Grace");
        assert_eq!(song.artist.as_deref(), Some("Traditional"));
        assert!(song.sections.is_empty());
    }

    #[test]
    fn song_with_sections() {
        let mut song = Song::new("Test Song");
        let scene_ref = ScopedSceneRef::Engine(VersionedRef::new(EngineSceneId::new(), 1));
        let section = SongSection::new("Verse", scene_ref);
        song.add_section(section);

        assert_eq!(song.sections.len(), 1);
        assert_eq!(song.section(0).unwrap().name, "Verse");
    }

    #[test]
    fn section_transition() {
        let scene_ref = ScopedSceneRef::Engine(VersionedRef::new(EngineSceneId::new(), 1));
        let section =
            SongSection::new("Chorus", scene_ref).with_transition(SceneTransition::Crossfade(500));
        assert_eq!(section.transition, SceneTransition::Crossfade(500));
    }
}
