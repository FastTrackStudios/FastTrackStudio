//! Visibility Management Domain
//! 
//! This module provides domain models for managing track visibility, groups, and snapshots
//! without being tied to a specific DAW implementation.
//!
//! Key concepts:
//! - **Sorting Groups**: Hierarchical groups for organizing tracks (can be nested)
//! - **Track Types**: Specific configurations within groups (e.g., "Mic Input", "Modeler Stereo")
//! - **Versions**: Variations of track types (e.g., "With FX", "No FX", "DI Only")
//! - **Snapshots**: Saved visibility and layout states for groups
//! - **View Modes**: How groups interact (Toggle, Exclusive, LimitedExclusive)

pub mod group;
pub mod group_config;
pub mod track_type;
pub mod track_structure;
pub mod version;
pub mod snapshot;
pub mod view_mode;
pub mod track_scope;

#[cfg(feature = "default-groups")]
pub mod default_groups;

pub use group::{SortingGroup, SortingGroupData};

// Type alias for convenience (avoiding naming conflict with naming_convention::Group trait)
pub type Group = SortingGroupData;
pub use track_type::{TrackType, TrackTypeId, TrackTypeCategory};
pub use version::{TrackVersion, VersionId, VersionConfig};
pub use snapshot::{Snapshot, SnapshotId, SnapshotType, SnapshotMetadata};
pub use view_mode::ViewMode;
pub use track_scope::{TrackScope, TrackIdentifier};
pub use track_structure::{
    TrackEntry, EntryType, EntryMetadata,
    TrackNode, TrackRole, TrackCategory,
    TrackStructure, GroupingStrategy,
    FilterRule, FilterType,
};

