//! Signal Control — ergonomic API facade for rig control.
//!
//! Wraps the ROAM service trait behind simple async methods so consumers
//! never need to think about `roam::Context` or service plumbing.
//!
//! # Usage
//!
//! ```ignore
//! let ctl = SignalControl::mock_guitar();
//!
//! // Simple async calls — no ROAM context needed
//! let presets = ctl.get_available_presets().await;
//! let profile = ctl.get_current_profile().await;
//!
//! // Execute commands
//! ctl.next_song().await;
//! ctl.load_preset(preset_id).await;
//!
//! // Subscribe to events
//! let mut rx = ctl.subscribe().await;
//! while let Ok(Some(event)) = rx.recv().await {
//!     // handle event
//! }
//! ```

use std::sync::Arc;
use uuid::Uuid;

// Import the trait so its methods are in scope on Arc<MockRigControlService>
use signal::RigControlService;

// Re-export domain type modules from signal-proto (via signal)
pub use signal::{
    block, category, defaults, director, id, layer, module, module_preset, non_empty, normalized,
    parameter, patch, performance, preset, profile, rig, routing, section, selection, source, tags,
};

// Re-export service/engine types that consumers need
pub use signal::engine::PreloadPriority;
pub use signal::module::ModuleType;
pub use signal::service::{
    EngineStateInfo, InstanceInfo, PreloadStatusInfo, PresetInfo, PresetSnapshotInfo, ProfileInfo,
    ProfileSceneInfo, RigControlCommand, RigControlEvent, RigInfo, SetlistInfo, SlotErrorInfo,
    SlotStateInfo, SongInfo, SwitchOutcomeInfo, TransitionResultInfo,
};

/// Ergonomic rig control API.
///
/// Wraps any `RigControlService` implementation and provides simple
/// async methods without exposing ROAM internals.
#[derive(Clone)]
pub struct SignalControl {
    service: Arc<signal::MockRigControlService>,
}

impl PartialEq for SignalControl {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.service, &other.service)
    }
}

impl SignalControl {
    /// Create from a mock service instance.
    pub fn new(service: signal::MockRigControlService) -> Self {
        Self {
            service: Arc::new(service),
        }
    }

    /// Create with default guitar rig mock data.
    pub fn mock_guitar() -> Self {
        Self::new(signal::MockRigControlService::with_guitar_defaults())
    }

    /// Get a reference to the underlying service (for advanced usage).
    pub fn inner(&self) -> &Arc<signal::MockRigControlService> {
        &self.service
    }

    // ── Queries ──────────────────────────────────────────────────────

    pub async fn get_available_profiles(&self) -> Vec<ProfileInfo> {
        self.service.get_available_profiles(&Self::cx()).await
    }

    pub async fn get_current_profile(&self) -> Option<ProfileInfo> {
        self.service.get_current_profile(&Self::cx()).await
    }

    pub async fn get_available_presets(&self) -> Vec<PresetInfo> {
        self.service.get_available_presets(&Self::cx()).await
    }

    pub async fn get_current_preset(&self) -> Option<PresetInfo> {
        self.service.get_current_preset(&Self::cx()).await
    }

    pub async fn get_current_rig(&self) -> Option<RigInfo> {
        self.service.get_current_rig(&Self::cx()).await
    }

    pub async fn get_available_setlists(&self) -> Vec<SetlistInfo> {
        self.service.get_available_setlists(&Self::cx()).await
    }

    pub async fn get_current_setlist(&self) -> Option<SetlistInfo> {
        self.service.get_current_setlist(&Self::cx()).await
    }

    pub async fn get_setlist_songs(&self) -> Vec<SongInfo> {
        self.service.get_setlist_songs(&Self::cx()).await
    }

    pub async fn get_current_song(&self) -> Option<SongInfo> {
        self.service.get_current_song(&Self::cx()).await
    }

    pub async fn get_current_scene(&self) -> Option<ProfileSceneInfo> {
        self.service.get_current_scene(&Self::cx()).await
    }

    /// Get the current preset's modules materialized for UI display.
    ///
    /// Synchronous — reads directly from the mock service's data store.
    pub fn get_current_modules(&self) -> Vec<signal::module::Module> {
        self.service.build_current_modules()
    }

    // ── Commands ─────────────────────────────────────────────────────

    pub async fn execute(&self, cmd: RigControlCommand) {
        self.service.execute(&Self::cx(), cmd).await;
    }

    pub async fn load_profile(&self, profile_id: Uuid) {
        self.execute(RigControlCommand::LoadProfile { profile_id })
            .await;
    }

    pub async fn load_preset(&self, preset_id: Uuid) {
        self.execute(RigControlCommand::LoadPreset { preset_id })
            .await;
    }

    pub async fn load_preset_with_scene(&self, preset_id: Uuid, scene_index: usize) {
        self.execute(RigControlCommand::LoadPresetWithScene {
            preset_id,
            scene_index,
        })
        .await;
    }

    pub async fn next_song(&self) {
        self.execute(RigControlCommand::NextSong).await;
    }

    pub async fn previous_song(&self) {
        self.execute(RigControlCommand::PreviousSong).await;
    }

    pub async fn next_scene(&self) {
        self.execute(RigControlCommand::NextScene).await;
    }

    pub async fn previous_scene(&self) {
        self.execute(RigControlCommand::PreviousScene).await;
    }

    // ── Subscriptions ────────────────────────────────────────────────

    /// Subscribe to rig events. Returns a receiver channel.
    pub async fn subscribe(&self) -> roam::Rx<RigControlEvent> {
        let (tx, rx) = roam::channel::<RigControlEvent>();
        self.service.subscribe(&Self::cx(), tx).await;
        rx
    }

    // ── Internal ─────────────────────────────────────────────────────

    /// Create a default ROAM context for local service calls.
    fn cx() -> roam::Context {
        roam::Context::new(
            Default::default(),
            Default::default(),
            Default::default(),
            Default::default(),
            vec![],
        )
    }
}
