//! Compile-time wiring verification for the `#[architect::rpc]` port
//! of the `Markers` service.
//!
//! This test doesn't talk to REAPER — it doesn't need to. The goal is
//! to confirm that:
//!
//! 1. `MarkersHost` (auto-emitted by `#[architect::rpc]`) accepts our
//!    owning backend + the REAPER main-thread dispatcher
//! 2. `MarkersRpc` (the hidden async mirror trait) is implemented on
//!    the host, so callers can `.await` any sync method
//! 3. The backend impls `Markers` directly, so in-process callers can
//!    still hit it with zero ceremony
//!
//! If any of those break (trait shape drifts, bridge codegen
//! regresses, vox version misaligns), this test fails to compile,
//! which is the whole point.

use daw_proto::sync::{Markers, MarkersHost, MarkersRpc};
use daw_reaper::architect_bridge::{ReaperMainThreadDispatcher, ReaperMarkersBackend};

#[test]
fn host_constructs_with_backend_and_dispatcher() {
    let backend = ReaperMarkersBackend::new("00000000-0000-0000-0000-000000000000");
    let _host = MarkersHost::new(backend, ReaperMainThreadDispatcher);
}

#[test]
fn backend_is_directly_callable_as_sync_markers() {
    // Don't actually call REAPER methods — they'd panic without
    // TaskSupport bootstrapped. Just prove the trait surface exists.
    fn assert_markers<T: Markers>(_: &T) {}
    let backend = ReaperMarkersBackend::new("guid");
    assert_markers(&backend);
}

#[test]
fn host_implements_async_mirror() {
    // Verify the bridge implements MarkersRpc on the host so callers
    // can `.await` sync methods over RPC. We don't drive the future;
    // we just check the trait bound holds.
    fn assert_mirror<T: MarkersRpc>(_: &T) {}
    let backend = ReaperMarkersBackend::new("guid");
    let host = MarkersHost::new(backend, ReaperMainThreadDispatcher);
    assert_mirror(&host);
}
