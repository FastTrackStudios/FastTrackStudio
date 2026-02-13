//! Profile domain — named configurations referencing Rig variants.
//!
//! A [`Profile`] is a collection of [`Patch`] variants. Each Patch
//! references a specific Rig variant and can apply additional overrides.
//! Profiles are used for quick sound switching (e.g. "Worship", "Blues").

use facet::Facet;
use serde::{Deserialize, Serialize};

use crate::metadata::Metadata;
use crate::overrides::Override;
use crate::rig::{RigId, RigSceneId};

// ─── IDs ────────────────────────────────────────────────────────

crate::typed_string_id!(
    /// Identifies a Profile collection.
    ProfileId
);
crate::typed_string_id!(
    /// Identifies a specific Patch variant within a Profile.
    PatchId
);

// ─── Patch ──────────────────────────────────────────────────────

/// A Patch variant — references a Rig variant with optional overrides.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Patch {
    pub id: PatchId,
    pub name: String,
    pub rig_id: RigId,
    pub rig_variant_id: RigSceneId,
    pub overrides: Vec<Override>,
    pub metadata: Metadata,
}

impl Patch {
    pub fn new(
        id: impl Into<PatchId>,
        name: impl Into<String>,
        rig_id: impl Into<RigId>,
        rig_variant_id: impl Into<RigSceneId>,
    ) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            rig_id: rig_id.into(),
            rig_variant_id: rig_variant_id.into(),
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

// ─── Profile ────────────────────────────────────────────────────

/// A Profile collection — named grouping of Patch variants.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Profile {
    pub id: ProfileId,
    pub name: String,
    pub default_patch_id: PatchId,
    pub patches: Vec<Patch>,
    pub metadata: Metadata,
}

impl Profile {
    pub fn new(id: impl Into<ProfileId>, name: impl Into<String>, default_patch: Patch) -> Self {
        let default_patch_id = default_patch.id.clone();
        Self {
            id: id.into(),
            name: name.into(),
            default_patch_id,
            patches: vec![default_patch],
            metadata: Metadata::new(),
        }
    }

    pub fn add_patch(&mut self, patch: Patch) {
        self.patches.push(patch);
    }

    pub fn default_patch(&self) -> Option<&Patch> {
        self.patches.iter().find(|p| p.id == self.default_patch_id)
    }

    pub fn patch(&self, id: &PatchId) -> Option<&Patch> {
        self.patches.iter().find(|p| &p.id == id)
    }

    #[must_use]
    pub fn with_metadata(mut self, metadata: Metadata) -> Self {
        self.metadata = metadata;
        self
    }
}

// ─── Trait impls ────────────────────────────────────────────────

impl crate::traits::Variant for Patch {
    type Id = PatchId;
    fn variant_id(&self) -> &PatchId {
        &self.id
    }
    fn variant_name(&self) -> &str {
        &self.name
    }
}

impl crate::traits::Collection for Profile {
    type Variant = Patch;

    fn variants(&self) -> &[Patch] {
        &self.patches
    }
    fn variants_mut(&mut self) -> &mut Vec<Patch> {
        &mut self.patches
    }
    fn default_variant_id(&self) -> &PatchId {
        &self.default_patch_id
    }
    fn set_default_variant_id(&mut self, id: PatchId) {
        self.default_patch_id = id;
    }
}

impl crate::traits::HasMetadata for Patch {
    fn metadata(&self) -> &Metadata {
        &self.metadata
    }
    fn metadata_mut(&mut self) -> &mut Metadata {
        &mut self.metadata
    }
}

impl crate::traits::HasMetadata for Profile {
    fn metadata(&self) -> &Metadata {
        &self.metadata
    }
    fn metadata_mut(&mut self) -> &mut Metadata {
        &mut self.metadata
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_profile_creation() {
        let patch = Patch::new("patch-clean", "Clean", "rig-1", "rv-clean");
        let mut profile = Profile::new("profile-1", "Worship", patch);
        profile.add_patch(Patch::new("patch-lead", "Lead", "rig-1", "rv-lead"));

        assert_eq!(profile.name, "Worship");
        assert_eq!(profile.patches.len(), 2);
        assert_eq!(profile.default_patch().unwrap().name, "Clean");
        assert!(profile.patch(&PatchId::new("patch-lead")).is_some());
    }
}
