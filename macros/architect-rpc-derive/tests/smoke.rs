//! End-to-end tests for `#[architect::rpc]` codegen.
//!
//! Tests run without the `vox` cargo feature, so the mirror trait is
//! a plain async trait (no `#[vox::service]` decoration). That lets us
//! exercise the bridge logic in isolation — wiring it onto a real vox
//! link is covered by the architect example crates.
//!
//! The public surface the macro exposes is three nouns + one verb:
//!
//! - the user's trait (e.g. `AllSync`)
//! - `<T>Client` (auto, vox-emitted) — for callers
//! - `serve` (auto, in the trait's module) — for mounting
//!
//! Each `#[rpc]` trait lives in its own module — the macro emits a
//! `Service` token at the trait's module scope, so two `#[rpc]`
//! traits in the same module would collide on `Service` /
//! `BindAny`/`Bind`/`Append`/`Descriptors` impls. Real consumers
//! (scheduling-proto, finance-proto, …) already structure
//! per-capability submodules; the tests mirror that.
//!
//! The internal bridge (`__<T>Bridge`) is `#[doc(hidden)]` and isn't
//! intended for direct use, but tests reach for it to exercise the
//! bridge body without going through vox::service.

use architect::rpc;

// ── All-sync trait ──────────────────────────────────────────────────

mod all_sync {
    use std::sync::Mutex;

    use architect::dispatch::CurrentThreadDispatcher;

    use super::rpc;

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
        let bridge = __AllSyncBridge::new(AllSyncBackend::default(), CurrentThreadDispatcher);

        // The bridge implements the AllSyncRpc mirror; calling its async
        // methods runs the underlying sync ops through the dispatcher.
        futures_lite::future::block_on(async {
            AllSyncRpc::write(&bridge, 1, "hello".into()).await.unwrap();
            let v = AllSyncRpc::read(&bridge, 1).await;
            assert_eq!(v.as_deref(), Some("hello"));

            // Borrowed-arg path: `&str` was rewritten to `String` in the
            // mirror; the bridge passes `&owned` back into the sync trait.
            let echoed = AllSyncRpc::echo_str(&bridge, "ping".into()).await;
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
}

// ── All-async trait ─────────────────────────────────────────────────

mod all_async {
    use std::sync::{Arc, Mutex};

    use super::rpc;

    // `async_fn_in_trait` is the whole point of the AllAsync shape —
    // vox::service is applied directly and rewrites the futures itself.
    #[rpc]
    #[allow(async_fn_in_trait)]
    pub trait AllAsync {
        async fn read(&self, key: u32) -> Option<String>;
        async fn write(&self, key: u32, value: String) -> Result<(), String>;
    }

    // Backend is `Clone` because the vox-enabled `Service::bind_into`
    // path clones the backend to seed the layer's dispatcher. Arc
    // around the inner state keeps the writes visible across clones.
    #[derive(Default, Clone)]
    struct AllAsyncBackend {
        store: Arc<Mutex<Vec<(u32, String)>>>,
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
    fn all_async_bridge_passes_through() {
        let bridge = __AllAsyncBridge::new(AllAsyncBackend::default());

        futures_lite::future::block_on(async {
            AllAsync::write(&bridge, 1, "async".into()).await.unwrap();
            let v = AllAsync::read(&bridge, 1).await;
            assert_eq!(v.as_deref(), Some("async"));
        });
    }
}

// ── Mixed trait ─────────────────────────────────────────────────────

mod mixed {
    use std::sync::Mutex;

    use architect::dispatch::CurrentThreadDispatcher;

    use super::rpc;

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
        let bridge = __MixedBridge::new(MixedBackend::default(), CurrentThreadDispatcher);

        futures_lite::future::block_on(async {
            MixedRpc::write(&bridge, 1, "x".into()).await.unwrap();
            let v = MixedRpc::read(&bridge, 1).await;
            assert_eq!(v.as_deref(), Some("x"));
        });
    }
}

// ── #[derive(HasDispatcher)] ────────────────────────────────────────

mod derived_dispatcher {
    use std::sync::Mutex;

    // One `use` pulls both namespaces: the trait (type ns) and the
    // derive (macro ns) share the `architect::HasDispatcher` path.
    use architect::HasDispatcher;
    use architect::dispatch::{self, CurrentThreadDispatcher};

    use super::rpc;

    #[rpc]
    pub trait Counter {
        fn add(&self, n: u32) -> u32;
    }

    #[derive(Default, HasDispatcher)]
    #[dispatch(CurrentThreadDispatcher)]
    struct ExplicitBackend {
        total: Mutex<u32>,
    }

    impl Counter for ExplicitBackend {
        fn add(&self, n: u32) -> u32 {
            let mut total = self.total.lock().unwrap();
            *total += n;
            *total
        }
    }

    // No #[dispatch] attr → DefaultDispatcher. Which concrete type that
    // is depends on architect's `dispatch-tokio` feature; the test only
    // asserts the impl exists and constructs.
    #[derive(Default, HasDispatcher)]
    struct DefaultBackend;

    impl Counter for DefaultBackend {
        fn add(&self, n: u32) -> u32 {
            n
        }
    }

    #[test]
    fn derive_with_explicit_dispatch_attr_round_trips() {
        let backend = ExplicitBackend::default();
        let dispatcher = backend.dispatcher();
        let bridge = __CounterBridge::new(backend, dispatcher);

        futures_lite::future::block_on(async {
            assert_eq!(CounterRpc::add(&bridge, 2).await, 2);
            assert_eq!(CounterRpc::add(&bridge, 3).await, 5);
        });
    }

    #[test]
    fn derive_without_attr_uses_default_dispatcher() {
        let backend = DefaultBackend;
        // Type-level assertion: the derived impl names DefaultDispatcher.
        let _dispatcher: dispatch::DefaultDispatcher = backend.dispatcher();
    }

    #[test]
    fn derive_works_on_generic_backends() {
        #[derive(HasDispatcher)]
        #[dispatch(CurrentThreadDispatcher)]
        struct Generic<T: Send + Sync + 'static> {
            _inner: T,
        }

        let backend = Generic { _inner: 7u8 };
        let _dispatcher: CurrentThreadDispatcher = backend.dispatcher();
    }
}

// ── prelude module ──────────────────────────────────────────────────

mod prelude_reexports {
    use super::rpc;

    pub mod ping {
        #[super::rpc]
        pub trait Ping {
            fn ping(&self) -> u32;
        }
    }

    // Without the `vox` feature the prelude re-exports just the trait —
    // the client/dispatcher/Service items don't exist. The glob is the
    // whole point: proto crates write one `pub use m::prelude::*` per
    // service instead of hand-renaming five items.
    use ping::prelude::*;

    struct Backend;

    impl Ping for Backend {
        fn ping(&self) -> u32 {
            42
        }
    }

    #[test]
    fn prelude_glob_exposes_the_trait() {
        assert_eq!(Backend.ping(), 42);
    }
}

// ── Empty trait ─────────────────────────────────────────────────────

mod empty {
    use super::rpc;

    #[rpc]
    #[allow(dead_code)]
    pub trait Empty {}

    #[test]
    fn empty_trait_compiles() {
        // Empty traits don't get a `serve` function — there's nothing to
        // serve. This test just verifies the macro accepts the shape.
        #[allow(dead_code)]
        struct Backend;
        impl Empty for Backend {}
    }
}
