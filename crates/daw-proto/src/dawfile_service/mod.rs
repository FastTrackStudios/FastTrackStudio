//! DAW file ops — types + service trait.

mod service;
mod types;

pub use service::{DawFileOps, DawFileOpsRpc};
pub use types::{
    CombineSetlistOptions, CombineSetlistResult, ProjectSummary, ProjectTrackSummary, SetlistSong,
};

#[cfg(feature = "vox")]
pub use service::{DawFileOpsClient, Dispatcher, Service, descriptor, layer, serve};
