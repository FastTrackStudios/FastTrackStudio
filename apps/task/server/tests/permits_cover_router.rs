//! The drift guard between `org_layer_router` and `permits::mounts`.
//!
//! `permits::mounts()` is a hand-kept parallel list of what the router
//! mounts — the same shape `schema_stamps` used to be, and the same shape
//! that silently rotted into "2 of 71 services actually gated". This test
//! makes the rot a build failure: mount a service without adding its
//! permit table and the counts diverge here.
//!
//! It boots a real `AppState` (against a throwaway data root) because the
//! router can only be built from a live org state.

use task_server::{AppState, org_layer_router, permits};

/// Serializes the env twiddle below — `AppState::new` reads
/// `TASK_DATA_ROOT` once at boot.
static ENV_LOCK: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

#[tokio::test(flavor = "multi_thread")]
async fn every_mounted_service_has_a_permit_table() {
    let tmp = tempfile::tempdir().expect("temp data root");
    let guard = ENV_LOCK.lock().await;
    // SAFETY: held under `ENV_LOCK` for the duration of `AppState::new`,
    // which reads the var exactly once.
    unsafe {
        std::env::set_var("TASK_DATA_ROOT", tmp.path());
    }
    // An empty data root hosts no orgs (bootstrap mode), so scaffold one.
    let data_root = org_proto::DataRoot::from_env().expect("data root");
    data_root
        .init_org("permits-test", "Permits Test", true)
        .expect("scaffold org");
    let state = AppState::new(None).await.expect("boot AppState");
    drop(guard);

    let slug = state
        .org_slugs()
        .into_iter()
        .next()
        .expect("the org scaffolded above");
    let org = state.org(&slug).expect("the org it just listed");
    let router = org_layer_router(&org);

    assert_eq!(
        router.len(),
        permits::mounts().len(),
        "org_layer_router mounts {} services but permits::mounts() lists {} — \
         add the new service (and its permit table) to `permits::mounts`",
        router.len(),
        permits::mounts().len(),
    );

    // And the tables themselves are whole: no untabled service, no method
    // missing a permit (which would be fail-closed under enforcement), no
    // permit naming a method that does not exist.
    let coverage = permits::coverage();
    assert!(
        coverage.is_complete(),
        "permit coverage is incomplete:\n{}",
        permits::coverage_summary(),
    );
    assert_eq!(coverage.services, router.len());
}
