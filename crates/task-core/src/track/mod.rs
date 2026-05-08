//! Track entity — first-class audio-production deliverable.
//!
//! A `Track` belongs to a [`Project`](crate::project::Project) (typically a
//! project of type `audio-production`) and carries the workflow-specific
//! metadata for one song or recording: BPM, key, duration, paths to the
//! DAW session and stems, plus the production-status enum that drives the
//! Tracking → Mixing → Mastering → Approved workflow.
//!
//! Polymorphic `properties: JsonObject` matches the Obsidian-frontmatter
//! shape used elsewhere in the schema, and attachments hung off
//! `owner_type = "track"` carry mix-bounce / stems / reference files.

pub mod model;

pub use model::{
    ActiveModel, Column, Entity, Model, Model as Track, TrackApi, TrackApiCreate, TrackApiList,
    TrackApiUpdate, TrackStatus,
};
