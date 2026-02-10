//! Typed identifiers — prevent mixing up IDs from different domains.
//!
//! Each domain entity gets its own newtype wrapper around `Uuid`.
//! Passing a `PresetId` where a `SnapshotId` is expected is a compile error.

use facet::Facet;
use uuid::Uuid;

macro_rules! typed_id {
    ($(#[$meta:meta])* $name:ident) => {
        $(#[$meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
        pub struct $name(Uuid);

        impl $name {
            /// Generate a new random ID.
            pub fn new() -> Self {
                Self(Uuid::new_v4())
            }

            /// Wrap an existing UUID.
            pub fn from_uuid(uuid: Uuid) -> Self {
                Self(uuid)
            }

            /// Get the underlying UUID.
            pub fn as_uuid(self) -> Uuid {
                self.0
            }
        }

        impl Default for $name {
            fn default() -> Self {
                Self::new()
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl From<$name> for Uuid {
            fn from(id: $name) -> Uuid {
                id.0
            }
        }
    };
}

typed_id!(
    /// Identifies a [`Preset`](crate::preset::Preset).
    PresetId
);
typed_id!(
    /// Identifies a [`Snapshot`](crate::preset::Snapshot) within a preset.
    SnapshotId
);
typed_id!(
    /// Identifies a [`Profile`](crate::profile::Profile).
    ProfileId
);
typed_id!(
    /// Identifies a [`SceneTemplate`](crate::profile::SceneTemplate) within a profile.
    SceneTemplateId
);
typed_id!(
    /// Identifies a [`Section`](crate::section::Section) in a rig.
    SectionId
);
typed_id!(
    /// Identifies a [`Layer`](crate::layer::Layer) within a section.
    LayerId
);
typed_id!(
    /// Identifies a [`Block`](crate::block::Block) (DSP processing unit).
    BlockId
);
typed_id!(
    /// Identifies a [`Patch`](crate::patch::Patch).
    PatchId
);
typed_id!(
    /// Identifies a [`PatchVariation`](crate::patch::PatchVariation).
    VariationId
);
typed_id!(
    /// Identifies a [`PerformanceSong`](crate::performance::PerformanceSong).
    SongId
);
typed_id!(
    /// Identifies a [`Scene`](crate::performance::Scene) within a song.
    SceneId
);
typed_id!(
    /// Identifies a [`Rig`](crate::rig::Rig).
    RigId
);
typed_id!(
    /// Identifies a [`Module`](crate::module::Module).
    ModuleId
);
typed_id!(
    /// Identifies a role in the [`RigDirector`](crate::director::RigDirector).
    RoleId
);
typed_id!(
    /// Identifies a [`Tag`](crate::tags::Tag) in the tag registry.
    TagId
);
typed_id!(
    /// Identifies a global block configuration.
    GlobalBlockId
);
typed_id!(
    /// Identifies a [`ModulePreset`](crate::module_preset::ModulePreset).
    ModulePresetId
);
typed_id!(
    /// Identifies a [`ModuleSnapshot`](crate::module_preset::ModuleSnapshot) within a module preset.
    ModuleSnapshotId
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typed_ids_are_distinct_types() {
        // These are different types — passing one where the other is expected
        // would be a compile error.
        let preset_id = PresetId::new();
        let snapshot_id = SnapshotId::new();

        // They wrap UUIDs but are not interchangeable
        assert_ne!(
            std::any::TypeId::of::<PresetId>(),
            std::any::TypeId::of::<SnapshotId>()
        );

        // Each has its own UUID
        assert_ne!(preset_id.as_uuid(), snapshot_id.as_uuid());
    }

    #[test]
    fn from_uuid_roundtrip() {
        let uuid = Uuid::new_v4();
        let id = PresetId::from_uuid(uuid);
        assert_eq!(id.as_uuid(), uuid);
    }

    #[test]
    fn equality_by_value() {
        let uuid = Uuid::new_v4();
        let a = BlockId::from_uuid(uuid);
        let b = BlockId::from_uuid(uuid);
        assert_eq!(a, b);
    }

    #[test]
    fn display_shows_uuid() {
        let uuid = Uuid::new_v4();
        let id = TagId::from_uuid(uuid);
        assert_eq!(id.to_string(), uuid.to_string());
    }

}
