//! End-to-end tests for `#[architect::rpc]` codegen.
//!
//! The macro is run without the `vox` cargo feature here, so the
//! mirror trait is a plain async trait (no `#[vox::service]` decoration).
//! That lets us exercise the bridge logic in isolation — wiring it onto
//! a real vox link is covered by the architect example crates.

use std::sync::Mutex;

use architect::dispatch::CurrentThreadDispatcher;
use architect::rpc;

// ── All-sync trait ──────────────────────────────────────────────────

#[rpc]
pub trait AllSync {
    fn read(&self, key: u32) -> Option<String>;
    fn write(&self, key: u32, value: String) -> Result<(), String>;
    fn echo_str(&self, s: &str) -> String;
}

#[derive(Default)]
struct AllSyncBackend {
    store: Mutex<Vec<(u32, String)>>,
}

impl AllSync for AllSyncBackend {
    fn read(&self, key: u32) -> Option<String> {
        self.store
            .lock()
            .unwrap()
            .iter()
            .find(|(k, _)| *k == key)
            .map(|(_, v)| v.clone())
    }

    fn write(&self, key: u32, value: String) -> Result<(), String> {
        self.store.lock().unwrap().push((key, value));
        Ok(())
    }

    fn echo_str(&self, s: &str) -> String {
        s.to_string()
    }
}

#[test]
fn all_sync_bridge_marshals_through_dispatcher() {
    let host = AllSyncHost::new(AllSyncBackend::default(), CurrentThreadDispatcher);

    // The host implements the AllSyncRpc mirror trait; calling its
    // async methods runs the underlying sync ops through the
    // dispatcher.
    futures_lite::future::block_on(async {
        // owned-arg path
        AllSyncRpc::write(&host, 1, "hello".into()).await.unwrap();
        let v = AllSyncRpc::read(&host, 1).await;
        assert_eq!(v.as_deref(), Some("hello"));

        // borrowed-arg path: `&str` was rewritten to `String` in the
        // mirror; the bridge passes `&owned` back into the sync trait.
        let echoed = AllSyncRpc::echo_str(&host, "ping".into()).await;
        assert_eq!(echoed, "ping");
    });
}

#[test]
fn user_trait_remains_directly_callable_in_process() {
    // The whole point of #[architect::rpc] is that the user-written
    // trait still works as a plain sync API — no .await, no bridge.
    let backend = AllSyncBackend::default();
    backend.write(7, "direct".into()).unwrap();
    assert_eq!(backend.read(7).as_deref(), Some("direct"));
    assert_eq!(backend.echo_str("x"), "x");
}

// ── All-async trait ─────────────────────────────────────────────────

#[rpc]
pub trait AllAsync {
    async fn read(&self, key: u32) -> Option<String>;
    async fn write(&self, key: u32, value: String) -> Result<(), String>;
}

#[derive(Default)]
struct AllAsyncBackend {
    store: Mutex<Vec<(u32, String)>>,
}

impl AllAsync for AllAsyncBackend {
    async fn read(&self, key: u32) -> Option<String> {
        self.store
            .lock()
            .unwrap()
            .iter()
            .find(|(k, _)| *k == key)
            .map(|(_, v)| v.clone())
    }

    async fn write(&self, key: u32, value: String) -> Result<(), String> {
        self.store.lock().unwrap().push((key, value));
        Ok(())
    }
}

#[test]
fn all_async_host_passes_through() {
    let host = AllAsyncHost::new(AllAsyncBackend::default());

    futures_lite::future::block_on(async {
        AllAsync::write(&host, 1, "async".into()).await.unwrap();
        let v = AllAsync::read(&host, 1).await;
        assert_eq!(v.as_deref(), Some("async"));
    });
}

// ── Mixed trait ─────────────────────────────────────────────────────

#[rpc]
pub trait Mixed {
    fn read(&self, key: u32) -> Option<String>;
    async fn write(&self, key: u32, value: String) -> Result<(), String>;
}

#[derive(Default)]
struct MixedBackend {
    store: Mutex<Vec<(u32, String)>>,
}

impl Mixed for MixedBackend {
    fn read(&self, key: u32) -> Option<String> {
        self.store
            .lock()
            .unwrap()
            .iter()
            .find(|(k, _)| *k == key)
            .map(|(_, v)| v.clone())
    }

    async fn write(&self, key: u32, value: String) -> Result<(), String> {
        self.store.lock().unwrap().push((key, value));
        Ok(())
    }
}

#[test]
fn mixed_bridge_marshals_sync_and_passes_async() {
    let host = MixedHost::new(MixedBackend::default(), CurrentThreadDispatcher);

    futures_lite::future::block_on(async {
        MixedRpc::write(&host, 1, "x".into()).await.unwrap();
        let v = MixedRpc::read(&host, 1).await;
        assert_eq!(v.as_deref(), Some("x"));
    });
}

// ── Empty trait ─────────────────────────────────────────────────────

#[rpc]
pub trait Empty {}

#[test]
fn empty_trait_emits_passthrough_host() {
    struct Backend;
    impl Empty for Backend {}
    let _host = EmptyHost::new(Backend);
}
