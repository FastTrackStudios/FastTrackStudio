#![deny(unsafe_code)]

mod project;
mod section;
mod workflow;

pub use project::{MusicProject, MusicProjectError};
pub use section::SongSection;
pub use workflow::MusicProjectStep;

pub use keyflow::chart::{Track, TrackType};
pub use keyflow::{Chart, LyricLine, SectionType};
pub use uuid::Uuid;

use facet::Facet;

/// Unique identifier for a FastTrackAudio music project.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Facet)]
pub struct MusicProjectId {
    pub value: Uuid,
}

impl MusicProjectId {
    pub fn new() -> Self {
        Self {
            value: Uuid::new_v4(),
        }
    }
}

impl Default for MusicProjectId {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Uuid> for MusicProjectId {
    fn from(value: Uuid) -> Self {
        Self { value }
    }
}
