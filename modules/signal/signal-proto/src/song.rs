//! Song domain — performance songs with section variants.
//!
//! A [`Song`] is a collection of [`Section`] variants. Each Section
//! references either a Patch or a Rig variant, with optional overrides.

use facet::Facet;
use serde::{Deserialize, Serialize};

use crate::metadata::Metadata;
use crate::overrides::Override;
use crate::profile::PatchId;
use crate::rig::{RigId, RigSceneId};

// ─── IDs ────────────────────────────────────────────────────────

crate::typed_string_id!(
    /// Identifies a Song collection.
    SongId
);
crate::typed_string_id!(
    /// Identifies a specific Section variant within a Song.
    SectionId
);

// ─── Section source ─────────────────────────────────────────────

/// What a song section references — either a Patch or a direct Rig variant.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub enum SectionSource {
    /// Reference a Patch from a Profile.
    Patch { patch_id: PatchId },
    /// Reference a Rig scene directly.
    RigScene { rig_id: RigId, scene_id: RigSceneId },
}

// ─── Section ────────────────────────────────────────────────────

/// A Section variant — one part of a song's performance.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Section {
    pub id: SectionId,
    pub name: String,
    pub source: SectionSource,
    pub overrides: Vec<Override>,
    pub metadata: Metadata,
}

impl Section {
    pub fn from_patch(
        id: impl Into<SectionId>,
        name: impl Into<String>,
        patch_id: impl Into<PatchId>,
    ) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            source: SectionSource::Patch {
                patch_id: patch_id.into(),
            },
            overrides: Vec::new(),
            metadata: Metadata::new(),
        }
    }

    pub fn from_rig_scene(
        id: impl Into<SectionId>,
        name: impl Into<String>,
        rig_id: impl Into<RigId>,
        scene_id: impl Into<RigSceneId>,
    ) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            source: SectionSource::RigScene {
                rig_id: rig_id.into(),
                scene_id: scene_id.into(),
            },
            overrides: Vec::new(),
            metadata: Metadata::new(),
        }
    }

    #[must_use]
    pub fn with_override(mut self, ov: Override) -> Self {
        self.overrides.push(ov);
        self
    }

    #[must_use]
    pub fn with_metadata(mut self, metadata: Metadata) -> Self {
        self.metadata = metadata;
        self
    }
}

// ─── Song ───────────────────────────────────────────────────────

/// A Song collection — performance structure with named sections.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Song {
    pub id: SongId,
    pub name: String,
    pub artist: Option<String>,
    pub default_section_id: SectionId,
    pub sections: Vec<Section>,
    pub metadata: Metadata,
}

impl Song {
    pub fn new(id: impl Into<SongId>, name: impl Into<String>, default_section: Section) -> Self {
        let default_section_id = default_section.id.clone();
        Self {
            id: id.into(),
            name: name.into(),
            artist: None,
            default_section_id,
            sections: vec![default_section],
            metadata: Metadata::new(),
        }
    }

    pub fn add_section(&mut self, section: Section) {
        self.sections.push(section);
    }

    pub fn default_section(&self) -> Option<&Section> {
        self.sections
            .iter()
            .find(|s| s.id == self.default_section_id)
    }

    pub fn section(&self, id: &SectionId) -> Option<&Section> {
        self.sections.iter().find(|s| &s.id == id)
    }

    #[must_use]
    pub fn with_artist(mut self, artist: impl Into<String>) -> Self {
        self.artist = Some(artist.into());
        self
    }

    #[must_use]
    pub fn with_metadata(mut self, metadata: Metadata) -> Self {
        self.metadata = metadata;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_song_with_patch_sections() {
        let verse = Section::from_patch("sec-verse", "Verse", "patch-clean");
        let chorus = Section::from_patch("sec-chorus", "Chorus", "patch-lead");

        let mut song = Song::new("song-1", "Amazing Grace", verse).with_artist("Traditional");
        song.add_section(chorus);

        assert_eq!(song.name, "Amazing Grace");
        assert_eq!(song.artist.as_deref(), Some("Traditional"));
        assert_eq!(song.sections.len(), 2);
        assert_eq!(song.default_section().unwrap().name, "Verse");
    }

    #[test]
    fn test_section_from_rig_scene() {
        let section = Section::from_rig_scene("sec-1", "Intro", "rig-1", "rv-ambient");
        match &section.source {
            SectionSource::RigScene { rig_id, scene_id } => {
                assert_eq!(rig_id.as_str(), "rig-1");
                assert_eq!(scene_id.as_str(), "rv-ambient");
            }
            _ => panic!("expected RigScene source"),
        }
    }
}
