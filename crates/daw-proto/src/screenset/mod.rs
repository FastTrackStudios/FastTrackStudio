//! Screensets — types + service trait.

mod service;
mod types;

pub use service::{Screensets, ScreensetsRpc};
pub use types::{
    CaptureScreensetRequest, Screenset, ScreensetKind, ScreensetMonitor, ScreensetOptions,
    ScreensetRect, ScreensetResult, ScreensetScope, ScreensetSelection, ScreensetSummary,
    ScreensetTrackVisibility, ScreensetWindow,
};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, ScreensetsClient, descriptor, serve};
