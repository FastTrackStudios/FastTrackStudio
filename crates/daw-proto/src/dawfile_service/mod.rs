//! DAW file service — types + service trait.

mod service;
mod types;

pub use service::{
    DawFileService, DawFileServiceClient, DawFileServiceDispatcher,
    daw_file_service_service_descriptor,
};
pub use types::{
    CombineSetlistOptions, CombineSetlistResult, ProjectSummary, ProjectTrackSummary, SetlistSong,
};
