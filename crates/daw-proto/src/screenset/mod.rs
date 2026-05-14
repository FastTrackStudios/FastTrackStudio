//! Screensets — named workspace snapshots (window layout, track
//! visibility, selection state) — types + service trait.

mod service;
mod types;

pub use service::{
    ScreensetService, ScreensetServiceClient, ScreensetServiceDispatcher,
    screenset_service_service_descriptor,
};
pub use types::{
    CaptureScreensetRequest, Screenset, ScreensetKind, ScreensetMonitor, ScreensetOptions,
    ScreensetRect, ScreensetResult, ScreensetScope, ScreensetSelection, ScreensetSummary,
    ScreensetTrackVisibility, ScreensetWindow,
};
