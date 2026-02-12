//! Profile — a collection of named patches.
//!
//! A [`Profile`] holds [`Patch`]es, each of which references a scene
//! at any hierarchy level via [`ScopedSceneRef`].

use crate::id::{PatchId, ProfileId};
use crate::scene::ScopedSceneRef;
use crate::tags::Tags;

// ─── Profile ─────────────────────────────────────────────────────

/// A named collection of patches.
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct Profile {
    pub id: ProfileId,
    pub name: String,
    pub patches: Vec<Patch>,
    pub tags: Tags,
}

impl Profile {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: ProfileId::new(),
            name: name.into(),
            patches: Vec::new(),
            tags: Tags::new(),
        }
    }

    pub fn add_patch(&mut self, patch: Patch) {
        self.patches.push(patch);
    }

    pub fn patch(&self, id: PatchId) -> Option<&Patch> {
        self.patches.iter().find(|p| p.id == id)
    }
}

// ─── Patch ───────────────────────────────────────────────────────

/// A named scene reference within a profile.
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct Patch {
    pub id: PatchId,
    pub name: String,
    pub scene_ref: ScopedSceneRef,
    pub tags: Tags,
}

impl Patch {
    pub fn new(name: impl Into<String>, scene_ref: ScopedSceneRef) -> Self {
        Self {
            id: PatchId::new(),
            name: name.into(),
            scene_ref,
            tags: Tags::new(),
        }
    }
}

// ─── Taggable implementations ────────────────────────────────────

use crate::tags::Taggable;

impl Taggable for Profile {
    fn tags(&self) -> &Tags {
        &self.tags
    }
    fn tags_mut(&mut self) -> &mut Tags {
        &mut self.tags
    }
    fn name(&self) -> &str {
        &self.name
    }
}

impl Taggable for Patch {
    fn tags(&self) -> &Tags {
        &self.tags
    }
    fn tags_mut(&mut self) -> &mut Tags {
        &mut self.tags
    }
    fn name(&self) -> &str {
        &self.name
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id::RigSceneId;
    use crate::version::VersionedRef;

    #[test]
    fn profile_creation() {
        let profile = Profile::new("Live Show");
        assert_eq!(profile.name, "Live Show");
        assert!(profile.patches.is_empty());
    }

    #[test]
    fn profile_with_patches() {
        let mut profile = Profile::new("Live");
        let scene_ref = ScopedSceneRef::Rig(VersionedRef::new(RigSceneId::new(), 1));
        let patch = Patch::new("Clean Verse", scene_ref);
        let patch_id = patch.id;
        profile.add_patch(patch);

        assert_eq!(profile.patches.len(), 1);
        assert!(profile.patch(patch_id).is_some());
        assert!(profile.patch(PatchId::new()).is_none());
    }

    #[test]
    fn profile_json_roundtrip() {
        let mut profile = Profile::new("Live");
        let scene_ref = ScopedSceneRef::Rig(VersionedRef::new(RigSceneId::new(), 1));
        profile.add_patch(Patch::new("Clean Verse", scene_ref));

        let json = facet_json::to_string(&profile).unwrap();
        let decoded: Profile = facet_json::from_str(&json).unwrap();
        assert_eq!(decoded.name, "Live");
        assert_eq!(decoded.patches.len(), 1);
        assert_eq!(decoded.patches[0].name, "Clean Verse");
    }
}
