//! Visibility Manager
//!
//! Dynamic track visibility management using instrument classification from dynamic-template.
//! Unlike hardcoded presets, this system analyzes the current track list and creates
//! visibility groups based on actual content.

mod classification;
mod error;
mod group;
mod snapshot;
mod state;
mod visibility;

pub use classification::{TrackClassification, classify_track, classify_tracks};
pub use error::{Error, Result};
pub use group::{VisibilityGroup, VisibilityGroupId};
pub use snapshot::{Snapshot, SnapshotId, SnapshotStore};
pub use state::{ViewMode, VisibilityManager};
pub use visibility::{TrackVisibility, VisibilityChanges, VisibilityTarget};

/// Re-export dynamic-template types that are commonly used
pub use dynamic_template::{
    Bass, Choir, Drums, Guitars, Harmonica, Horns, Keys, Orchestra, Percussion, Reference, SFX,
    Synths, Vocals,
};

/// Prelude for convenient imports
pub mod prelude {
    pub use crate::{
        Error, Result, Snapshot, SnapshotId, SnapshotStore, TrackClassification, TrackVisibility,
        ViewMode, VisibilityGroup, VisibilityGroupId, VisibilityManager, VisibilityTarget,
        classify_track, classify_tracks,
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_classification() {
        let classification = classify_track("Kick In");
        assert!(classification.primary_group.is_some());
        assert_eq!(classification.primary_group.as_deref(), Some("Drums"));
    }
}
