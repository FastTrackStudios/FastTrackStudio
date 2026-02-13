//! Typed identifiers — prevent mixing up IDs from different domains.
//!
//! Each domain entity gets its own newtype wrapper around `Uuid`.
//! Passing a `BlockId` where a `EngineId` is expected is a compile error.

use uuid::Uuid;

macro_rules! typed_id {
    ($(#[$meta:meta])* $name:ident) => {
        $(#[$meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::facet::Facet)]
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

// ─── Physical hierarchy ──────────────────────────────────────────

typed_id!(
    /// Identifies a [`Director`](crate::director::Director) — top-level manager of racks.
    DirectorId
);
typed_id!(
    /// Identifies a [`Rack`](crate::rack::Rack) — container for related rigs.
    RackId
);
typed_id!(
    /// Identifies a [`Rig`](crate::rig::Rig).
    RigId
);
typed_id!(
    /// Identifies an [`Engine`](crate::engine::Engine) — pure container for layers.
    EngineId
);
typed_id!(
    /// Identifies a [`Layer`](crate::layer::Layer) within an engine.
    LayerId
);
typed_id!(
    /// Identifies a [`Module`](crate::module::Module) within a layer.
    ModuleId
);
typed_id!(
    /// Identifies a [`Block`](crate::block::Block) — leaf DSP processing unit.
    BlockId
);

// ─── State hierarchy: Snapshots ──────────────────────────────────

typed_id!(
    /// Identifies a [`BlockSnapshot`](crate::snapshot::BlockSnapshot) — parameter state for a block.
    BlockSnapshotId
);
typed_id!(
    /// Identifies a [`ModuleSnapshot`](crate::snapshot::ModuleSnapshot) — composed block snapshot refs.
    ModuleSnapshotId
);

// ─── State hierarchy: Scenes ─────────────────────────────────────

typed_id!(
    /// Identifies a [`LayerScene`](crate::scene::LayerScene) — selects module/block snapshots.
    LayerSceneId
);
typed_id!(
    /// Identifies an [`EngineScene`](crate::scene::EngineScene) — selects layer scenes.
    EngineSceneId
);
typed_id!(
    /// Identifies a [`RigScene`](crate::scene::RigScene) — selects engine scenes.
    RigSceneId
);
typed_id!(
    /// Identifies a [`RackScene`](crate::scene::RackScene) — selects rig scenes.
    RackSceneId
);

// ─── Presets ─────────────────────────────────────────────────────

typed_id!(
    /// Identifies a block-level preset.
    BlockPresetId
);
typed_id!(
    /// Identifies a module-level preset.
    ModulePresetId
);
typed_id!(
    /// Identifies a layer-level preset.
    LayerPresetId
);
typed_id!(
    /// Identifies an engine-level preset.
    EnginePresetId
);
typed_id!(
    /// Identifies a rig-level preset.
    RigPresetId
);
typed_id!(
    /// Identifies a rack-level preset.
    RackPresetId
);

// ─── Performance ─────────────────────────────────────────────────

typed_id!(
    /// Identifies a [`Song`](crate::song::Song).
    SongId
);
typed_id!(
    /// Identifies a [`SongSection`](crate::song::SongSection) within a song.
    SongSectionId
);
typed_id!(
    /// Identifies a [`Profile`](crate::profile::Profile).
    ProfileId
);
typed_id!(
    /// Identifies a [`Patch`](crate::profile::Patch) within a profile.
    PatchId
);

// ─── Versioning ──────────────────────────────────────────────────

typed_id!(
    /// Identifies a [`Version`](crate::version::Version) in a version chain.
    VersionId
);

// ─── Templates ───────────────────────────────────────────────────

typed_id!(
    /// Identifies a [`RigTemplate`](crate::template::RigTemplate).
    RigTemplateId
);
typed_id!(
    /// Identifies an [`EngineTemplate`](crate::template::EngineTemplate).
    EngineTemplateId
);
typed_id!(
    /// Identifies a [`RackTemplate`](crate::template::RackTemplate).
    RackTemplateId
);

// ─── Module internals ────────────────────────────────────────────

typed_id!(
    /// Identifies a [`ModuleBlock`](crate::module::ModuleBlock) within a module.
    ModuleBlockId
);
typed_id!(
    /// Identifies a [`ModuleMacro`](crate::module::ModuleMacro) within a module.
    ModuleMacroId
);

// ─── Source ──────────────────────────────────────────────────────

typed_id!(
    /// Identifies a [`Source`](crate::source::Source) — a sound generator.
    SourceId
);

// ─── Tags ────────────────────────────────────────────────────────

typed_id!(
    /// Identifies a [`Tag`](crate::tags::Tag) in the tag registry.
    TagId
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typed_ids_are_distinct_types() {
        let block_id = BlockId::new();
        let engine_id = EngineId::new();

        assert_ne!(
            std::any::TypeId::of::<BlockId>(),
            std::any::TypeId::of::<EngineId>()
        );
        assert_ne!(block_id.as_uuid(), engine_id.as_uuid());
    }

    #[test]
    fn from_uuid_roundtrip() {
        let uuid = Uuid::new_v4();
        let id = BlockSnapshotId::from_uuid(uuid);
        assert_eq!(id.as_uuid(), uuid);
    }

    #[test]
    fn equality_by_value() {
        let uuid = Uuid::new_v4();
        let a = EngineId::from_uuid(uuid);
        let b = EngineId::from_uuid(uuid);
        assert_eq!(a, b);
    }

    #[test]
    fn display_shows_uuid() {
        let uuid = Uuid::new_v4();
        let id = TagId::from_uuid(uuid);
        assert_eq!(id.to_string(), uuid.to_string());
    }
}
