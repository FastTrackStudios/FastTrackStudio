//! Layered services — the `#[architect::rpc]` + `Layer<B>`
//! composition story, end-to-end, in one runnable file.
//!
//! # What this shows
//!
//! 1. Declare service traits with `#[architect::rpc]`. The derive
//!    emits, per trait: the user trait (kept sync), a hidden async
//!    mirror, a `<T>Host` bridge, a `<T>Client` async caller, a
//!    `serve(backend)` mount verb, a `layer(backend)` immediate-bind
//!    shortcut, and a `Service` token that slots into `layers!`.
//!
//! 2. Implement those traits on two backends — one "live" with real
//!    state, one "mock" stateless. Both impl `Services`, declaring
//!    their canonical bundles via `layers![…::Service]`.
//!
//! 3. Three deployment shapes from the same trait:
//!
//!    - **Direct sync.** Call trait methods on the backend. No
//!      router, no dispatcher, no future. Zero overhead beyond a
//!      virtual call (monomorphized away in release builds).
//!
//!    - **Backend swap at mount.** `LiveBackend.into_router()` vs
//!      `MockBackend.into_router()` — same trait, same call site,
//!      different runtime behavior.
//!
//!    - **Per-service override.** Start from a live bundle, merge in
//!      a pre-mounted service bound to a different backend. Last
//!      merge wins on method-id collision — that's how mocks and
//!      bolt-ons compose.
//!
//! # What this deliberately doesn't show
//!
//! Transport. `LayerRouter` already implements
//! `vox::Handler<DriverReplySink>` — to actually drive it from a
//! client you plug it into a vox `Driver` paired with a transport.
//! That's covered elsewhere:
//!
//! - **Axum WebSocket**: see `examples/custom-server/` — wraps the
//!   same router shape in `architect::axum_ws::serve`.
//! - **Unix socket / IPC**: see `daw-bridge` in the FastTrackStudio
//!   daw repo — the dock-host extension mounts its
//!   `Reaper.into_router()` on a Unix socket so out-of-process tools
//!   share the same client types.
//!
//! Run with:
//!   cargo run -p example-layered-services

use std::sync::{Arc, Mutex};

use architect::{
    HasDispatcher, Layer, LayerGraph, LayerNode, LayerRouter, Services,
    dispatch::CurrentThreadDispatcher, layers,
};
use example_memory::ExampleRepoMemory;
use example_proto::{ExampleRepo, example_repo_layer};
use example_stub_backend::StubBackend;
use tracing::info;

/// One generic mount fn — identical code for an in-memory repo, a
/// third-party stub, or (with the `server` feature) the SeaORM storage.
/// The `#[derive(Entity)]`-emitted `<Entity>RepoLayer` token makes the
/// repo a `Layer` participant, so the backend is injected at this call
/// site and nothing downstream changes.
fn mount<B>(backend: B) -> LayerRouter
where
    B: ExampleRepo + Services + Clone + Send + Sync + 'static,
{
    backend.into_router()
}

// ── Service traits ────────────────────────────────────────────────────
//
// Each `#[architect::rpc]` trait lives in its own module so the
// emitted `Service` token (and the `serve` / `layer` / `<T>Client`
// items that come with it) gets its own namespace — `counter::Service`,
// `greeter::Service`, etc. This is the same pattern daw-proto uses
// for its 26-service surface.

pub mod counter {
    /// Trivial counter. Sync methods are bridged through the
    /// backend's dispatcher when invoked via the async client; called
    /// directly they cost a single virtual call.
    #[architect::rpc]
    pub trait Counter {
        fn increment(&self, by: i64) -> i64;
        fn current(&self) -> i64;
    }
}

pub mod greeter {
    /// Trivial greeter. Borrowed args (`&str`) auto-convert to
    /// `String` on the async-mirror side; the sync trait keeps the
    /// borrowed signature so direct callers don't pay an allocation.
    #[architect::rpc]
    pub trait Greeter {
        fn greet(&self, name: &str) -> String;
    }
}

use counter::Counter;
use greeter::Greeter;

// ── Backends ──────────────────────────────────────────────────────────
//
// A backend is the "DI container" for our architecture — one struct
// that implements every service trait. State lives on the struct;
// service methods reach into `self` rather than dispatching through
// the router. Different backends provide different state and
// different impls.

/// Live backend: holds real counter state. Both service traits impl
/// directly against this type. The `HasDispatcher` derive points the
/// rpc bridge at a default-constructible dispatcher — the manual
/// four-line impl is only needed for dispatchers with runtime state.
#[derive(Clone, Default, HasDispatcher)]
#[dispatch(CurrentThreadDispatcher)]
pub struct LiveBackend {
    counter: Arc<Mutex<i64>>,
}

impl Counter for LiveBackend {
    fn increment(&self, by: i64) -> i64 {
        let mut g = self.counter.lock().expect("counter poisoned");
        *g += by;
        *g
    }
    fn current(&self) -> i64 {
        *self.counter.lock().expect("counter poisoned")
    }
}

impl Greeter for LiveBackend {
    fn greet(&self, name: &str) -> String {
        format!("Hello, {name}! (from LiveBackend)")
    }
}

/// Mock backend: stateless, returns canned values. Same traits, same
/// bundle declaration, different runtime behavior. This is the
/// "swap the layer" testing pattern Effect users get from
/// `Service.DefaultWithoutDependencies` — in Rust it's just a
/// different backend struct that impls the same traits.
#[derive(Clone, Copy, Default, HasDispatcher)]
#[dispatch(CurrentThreadDispatcher)]
pub struct MockBackend;

impl Counter for MockBackend {
    fn increment(&self, _by: i64) -> i64 {
        0
    }
    fn current(&self) -> i64 {
        0
    }
}

impl Greeter for MockBackend {
    fn greet(&self, name: &str) -> String {
        format!("[mock] hi {name}")
    }
}

// ── Bundle declarations ───────────────────────────────────────────────
//
// `impl Services for B` declares the canonical service surface for
// backend `B`. The body is just a `layers![ … ]` list of service
// tokens — one ident per service. The return type `impl Layer<B>`
// recursively checks (at compile time) that every service token has
// a `Bind<B>` impl. Missing impls surface as compile errors at the
// `.provide(...)` call site, naming the missing trait.

impl Services for LiveBackend {
    fn layers() -> impl Layer<LiveBackend> {
        layers![counter::Service, greeter::Service]
    }
}

impl Services for MockBackend {
    fn layers() -> impl Layer<MockBackend> {
        layers![counter::Service, greeter::Service]
    }
}

// ── Walk through the shapes ───────────────────────────────────────────

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into()),
        )
        .init();

    let live = LiveBackend::default();

    // ── 1. Direct sync ───────────────────────────────────────────────
    //
    // No router. No dispatcher. No async. The trait methods are
    // callable directly on the backend with their original sync
    // signatures — zero abstraction overhead beyond one virtual
    // call. This is the path daw-reaper's extension runtime uses
    // for REAPER's main-thread hot loop.
    info!("── 1. Direct sync ───────────────────────────────────────");
    let n = Counter::increment(&live, 5);
    let greeting = Greeter::greet(&live, "world");
    info!(counter = n, %greeting, "direct sync calls returned");

    // ── 2. Build a router via the canonical bundle ───────────────────
    //
    // `into_router()` is sugar for `Self::layers().provide(self)`.
    // Returns a `LayerRouter` — method-id-keyed dispatch table that
    // implements `vox::Handler<DriverReplySink>`. Plug it into any
    // vox transport and out-of-process clients can call every
    // service in the bundle.
    info!("── 2. Build router from canonical bundle ───────────────");
    let live_router = live.clone().into_router();
    info!(services = live_router.len(), "LiveBackend router built");

    // ── 3. Backend swap ──────────────────────────────────────────────
    //
    // Same trait, same bundle declaration, different backend type.
    // The router for `MockBackend` carries the mock impls of every
    // service. Test code that uses the live router in production
    // just swaps the construction line:
    //   let router = MockBackend.into_router();
    info!("── 3. Backend swap ─────────────────────────────────────");
    let mock_router = MockBackend.into_router();
    info!(services = mock_router.len(), "MockBackend router built");

    // ── 4. Per-service override (hybrid router) ──────────────────────
    //
    // Start from the live bundle, merge in a single service
    // pre-bound to a different backend. `LayerRouter` resolves
    // duplicate method IDs by **last merge wins**, so `Greeter`
    // calls on this router now route to MockBackend while
    // `Counter` calls keep hitting live state.
    //
    // This is the bolt-on pattern. Real use in daw-bridge:
    //   Reaper::layers()
    //       .merge(dock_host::layer(dock_host_backend))
    //       .provide(Reaper)
    // — the dock-host bolt-on uses a Dioxus-side backend that
    // Reaper itself doesn't impl.
    info!("── 4. Per-service override ─────────────────────────────");
    let hybrid_router = LiveBackend::layers()
        .merge(greeter::layer(MockBackend))
        .provide(live.clone());
    info!(
        services = hybrid_router.len(),
        "hybrid router (live counter + mock greeter) built"
    );

    // ── Introspection ────────────────────────────────────────────────
    //
    // `Layer<B>` carries the `Descriptors` walker as a supertrait, so
    // you can list a bundle's services before binding a backend.
    // Useful for capability negotiation, schema export, generated
    // docs.
    let descriptors: Vec<_> = LiveBackend::layers()
        .descriptors()
        .iter()
        .map(|d| d.service_name)
        .collect();
    info!(?descriptors, "LiveBackend canonical surface");

    // ── What you'd do next ───────────────────────────────────────────
    //
    // To actually drive any of these routers from a client:
    //
    //   - Pair the router with a vox `Driver` and a transport. See
    //     `examples/custom-server/` for the axum WebSocket variant
    //     and `daw-bridge` for the Unix-socket variant.
    //
    //   - Async client types (`CounterClient`, `GreeterClient`) are
    //     emitted by the derive when the consumer crate enables its
    //     `vox` feature. They wrap a `vox::Caller` — a handle to
    //     the driver — and expose the trait's methods as `async fn`.
    //
    // The composition surface shown here doesn't change between
    // in-process and cross-process: the `LayerRouter` is the same
    // object, only the transport in front of it differs.

    // ── Entity repos are layers too ──────────────────────────────────
    //
    // Everything above used hand-written `#[architect::rpc]` traits. A
    // `#[derive(Entity)]` repo is the same shape — an all-async vox
    // service — so the derive emits an `ExampleRepoLayer` token and the
    // repo composes/swaps through the layer system identically.

    // Same call site, three interchangeable backends. Neither `mount`
    // nor any consumer code changes when the backend does.
    let _mem = mount(ExampleRepoMemory::new());
    let _stub = mount(StubBackend::with_seed_data());

    let mem_surface: Vec<_> = ExampleRepoMemory::layers()
        .descriptors()
        .iter()
        .map(|d| d.service_name)
        .collect();
    let stub_surface: Vec<_> = StubBackend::layers()
        .descriptors()
        .iter()
        .map(|d| d.service_name)
        .collect();
    info!(
        ?mem_surface,
        ?stub_surface,
        "same repo surface, different backend — zero consumer change"
    );

    // Per-service override: keep the in-memory bundle, but bind the repo
    // to the third-party stub instead (last-merge-wins on method id).
    let _hybrid = ExampleRepoMemory::layers()
        .merge(example_repo_layer(StubBackend::with_seed_data()))
        .provide(ExampleRepoMemory::new());
    info!("override: repo impl swapped at the call site, bundle unchanged");

    // ── Dependency planner ───────────────────────────────────────────
    //
    // For a multi-node backend graph, declare what each node requires +
    // provides and get a validated build order — or a precise diagnostic.
    // (The async construction side — building these with deps,
    // memoization, and scoped teardown — is `architect::Resource`; see
    // `examples/app/server` for the real graceful-shutdown wiring.)
    info!("── Layer planner ───────────────────────────────────────");
    let plan = LayerGraph::new()
        .add(LayerNode::new("config", [] as [&str; 0], ["config"]))
        .add(LayerNode::new("db", ["config"], ["db"]))
        .add(LayerNode::new("repo", ["db"], ["repo"]))
        .plan()
        .expect("valid graph");
    info!(build_order = ?plan.build_order, "planned backend build order");

    // A deliberate mistake surfaces a diagnostic instead of a panic far
    // away at wiring time.
    let broken = LayerGraph::new()
        .add(LayerNode::new("repo", ["db"], ["repo"])) // nothing provides "db"
        .plan();
    match broken {
        Err(e) => info!(diagnostic = %e, "planner caught a wiring mistake"),
        Ok(_) => unreachable!("graph should be missing a provider"),
    }
}
