//! Dock host service trait.
//!
//! Implemented by a host adapter (REAPER, standalone, browser, mock).
//! Guests/extensions register docks and toggle visibility without
//! knowing which adapter is in use.

use super::{DockEvent, DockHandle, DockKind, PanelPixels, UiEventDto};
use vox::{Tx, service};

#[service]
pub trait DockHostService {
    /// Register a dock by stable string id. If already registered,
    /// returns the existing handle.
    async fn register_dock(&self, id: String, title: String, kind: DockKind) -> DockHandle;

    /// Idempotent. Returns `false` if already gone.
    async fn unregister_dock(&self, handle: DockHandle) -> bool;

    async fn show(&self, handle: DockHandle);
    async fn hide(&self, handle: DockHandle);
    /// Toggle visibility — returns the new visibility state.
    async fn toggle(&self, handle: DockHandle) -> bool;
    async fn is_visible(&self, handle: DockHandle) -> bool;

    /// Serialize the current layout for persistence. The blob is
    /// opaque — only the same adapter version guarantees round-trip.
    async fn save_layout(&self) -> Vec<u8>;

    /// Restore a previously-saved layout blob. Returns `false` if the
    /// blob is unrecognized; the dock host stays usable.
    async fn restore_layout(&self, blob: Vec<u8>) -> bool;

    async fn subscribe_dock_events(&self, tx: Tx<DockEvent>);

    /// Capture the current rendered pixels of a dock panel for
    /// visual regression / interaction tests. Returns `None` if the
    /// handle has no live panel mounted or first render hasn't
    /// completed yet.
    async fn capture_panel_pixels(&self, handle: DockHandle) -> Option<PanelPixels>;

    /// Inject a synthetic UI event into a panel. Returns `false` if
    /// the handle has no live panel mounted.
    async fn inject_ui_event(&self, handle: DockHandle, event: UiEventDto) -> bool;
}
