//! Screenset service trait.

use super::{
    CaptureScreensetRequest, Screenset, ScreensetOptions, ScreensetResult, ScreensetSummary,
};
use vox::service;

#[service]
pub trait ScreensetService {
    /// Capture the current managed screenset state into a named entry.
    async fn capture_screenset(&self, request: CaptureScreensetRequest) -> ScreensetResult;

    /// Save an explicit screenset snapshot.
    async fn save_screenset(
        &self,
        screenset: Screenset,
        options: ScreensetOptions,
    ) -> ScreensetResult;

    async fn list_screensets(&self, options: ScreensetOptions) -> Vec<ScreensetSummary>;
    async fn get_screenset(&self, id: String, options: ScreensetOptions) -> Option<Screenset>;
    async fn apply_screenset(&self, id: String, options: ScreensetOptions) -> ScreensetResult;
    async fn delete_screenset(&self, id: String, options: ScreensetOptions) -> ScreensetResult;
}
