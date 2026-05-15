//! Screensets service — named workspace snapshots.

use super::{
    CaptureScreensetRequest, Screenset, ScreensetOptions, ScreensetResult, ScreensetSummary,
};

#[architect_rpc_derive::rpc]
pub trait Screensets {
    /// Capture the current managed screenset state into a named entry.
    fn capture(&self, request: CaptureScreensetRequest) -> ScreensetResult;

    /// Save an explicit screenset snapshot.
    fn save(&self, screenset: Screenset, options: ScreensetOptions) -> ScreensetResult;

    fn list(&self, options: ScreensetOptions) -> Vec<ScreensetSummary>;
    fn get(&self, id: &str, options: ScreensetOptions) -> Option<Screenset>;
    fn apply(&self, id: &str, options: ScreensetOptions) -> ScreensetResult;
    fn delete(&self, id: &str, options: ScreensetOptions) -> ScreensetResult;
}

#[cfg(feature = "vox")]
pub use ScreensetsRpcDispatcher as Dispatcher;
#[cfg(feature = "vox")]
pub use screensets_rpc_service_descriptor as descriptor;
