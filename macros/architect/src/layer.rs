//! `Layer` — Effect-style service composition for architect-rpc backends.
//!
//! A [`Layer`] pairs an architect-rpc service descriptor with a type-erased
//! handler ready for mount. Layers compose via [`LayerSet::merge`] (the
//! horizontal `Layer.merge` from Effect) and mount via [`LayerSet::mount`]
//! onto anything that implements [`LayerRouter`].
//!
//! # The grain
//!
//! Effect's insight is that **mounting is a noun, not a verb**. Without
//! layers, registration sites end up writing one `.with(descriptor, serve)`
//! line per service — the bundle never becomes a thing you can name. Here,
//! a backend's surface is a single value:
//!
//! ```ignore
//! let app = MyBackendLayers::new(reaper).merge(extra_layer).mount(handler);
//! ```
//!
//! and adding/removing a service is an edit in *one* place.
//!
//! # Three primitives
//!
//! - [`Layer`] — one mounted service. Produced by the `#[architect::rpc]`
//!   macro's emitted `<svc>::layer(backend)` function for each trait.
//! - [`LayerSet`] — a bundle. Built via `.add(...)` / `.merge(...)`.
//!   `merge` accepts a single `Layer` or another `LayerSet` (anything that
//!   implements [`IntoLayerSet`]).
//! - [`LayerRouter`] — anything that knows how to receive a `Layer`. The
//!   per-binary RPC routers (vox method-id routers, in-process handler
//!   tables, etc.) implement this so `LayerSet::mount` can hand the layers
//!   over without knowing the router's concrete type.
//!
//! Vertical composition (Effect's `Layer.provide`) is intentionally *not*
//! in this module — per-handler middleware already covers that case via
//! `vox`'s `with_middleware`, and a set-wide middleware story can layer
//! cleanly on top of `LayerSet` later (a `LayerSet::with_middleware(M)`
//! method that wraps every contained handler).
//!
//! # Naming
//!
//! The shape borrows from Effect's `HttpLayerRouter` (in `@effect/platform`):
//! a single router type fed from a layer bundle. We split into a trait +
//! concrete pair to keep Rust idioms — [`LayerSink`] is the trait (any
//! type that absorbs layers) and [`LayerRouter`] is the canonical
//! concrete impl (method-id-keyed dispatch + a `vox::Handler` impl).

use core::any::Any;
use std::pin::Pin;
use std::sync::Arc;

use vox::{
    DriverReplySink, Handler, MethodId, RequestCall, RetryPolicy, SchemaRecvTracker, SelfRef,
    ServiceDescriptor,
};

// ── Erased handler ────────────────────────────────────────────────────────

/// Object-safe shim over [`vox::Handler<DriverReplySink>`]. The vox
/// `Handler` trait's `handle` method returns `impl Future`, which prevents
/// it from being used behind a trait object directly; this wrapper boxes
/// the future so `Arc<dyn DynHandler>` works.
///
/// You should not implement this manually — the blanket impl picks up any
/// `H: Handler<DriverReplySink>` automatically.
pub trait DynHandler: Send + Sync + 'static {
    fn handle(
        &self,
        call: SelfRef<RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: Arc<SchemaRecvTracker>,
    ) -> Pin<Box<dyn core::future::Future<Output = ()> + Send + '_>>;

    /// Forwarded from [`vox::Handler::retry_policy`] so routers can
    /// answer protocol queries about the wrapped service without
    /// downcasting.
    fn retry_policy(&self, method_id: MethodId) -> RetryPolicy;

    /// Forwarded from [`vox::Handler::args_have_channels`].
    fn args_have_channels(&self, method_id: MethodId) -> bool;

    /// Forwarded from [`vox::Handler::response_wire_shape`].
    fn response_wire_shape(&self, method_id: MethodId) -> Option<&'static facet::Shape>;

    /// Escape hatch for routers that want behaviour from the underlying
    /// `Handler` trait that `DynHandler` doesn't expose. Returns the
    /// same value the concrete `Handler` would.
    fn as_any(&self) -> &dyn Any;
}

impl<H> DynHandler for H
where
    H: Handler<DriverReplySink> + Send + Sync + 'static,
{
    fn handle(
        &self,
        call: SelfRef<RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: Arc<SchemaRecvTracker>,
    ) -> Pin<Box<dyn core::future::Future<Output = ()> + Send + '_>> {
        Box::pin(Handler::handle(self, call, reply, schemas))
    }

    fn retry_policy(&self, method_id: MethodId) -> RetryPolicy {
        Handler::retry_policy(self, method_id)
    }

    fn args_have_channels(&self, method_id: MethodId) -> bool {
        Handler::args_have_channels(self, method_id)
    }

    fn response_wire_shape(&self, method_id: MethodId) -> Option<&'static facet::Shape> {
        Handler::response_wire_shape(self, method_id)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

// ── Layer ─────────────────────────────────────────────────────────────────

/// One service mounted on a backend. Produced by the per-trait `layer()`
/// function the `#[architect::rpc]` macro emits.
///
/// Cheap to clone — the handler is behind an `Arc`. The descriptor is
/// already `'static`.
#[derive(Clone)]
pub struct Layer {
    descriptor: &'static ServiceDescriptor,
    handler: Arc<dyn DynHandler>,
}

impl Layer {
    /// Wrap a `(descriptor, handler)` pair. Most callers should reach for
    /// the per-service `layer()` function the rpc macro emits instead;
    /// this constructor is for routers building layers from raw parts
    /// (tests, custom dispatchers, etc.).
    pub fn new<H>(descriptor: &'static ServiceDescriptor, handler: H) -> Self
    where
        H: Handler<DriverReplySink> + Send + Sync + 'static,
    {
        Self {
            descriptor,
            handler: Arc::new(handler),
        }
    }

    /// Wrap an already-erased handler. Useful when a router has built up
    /// an `Arc<dyn DynHandler>` (e.g. after applying middleware) and wants
    /// to fold it back into the layer pipeline.
    pub fn from_arc(descriptor: &'static ServiceDescriptor, handler: Arc<dyn DynHandler>) -> Self {
        Self {
            descriptor,
            handler,
        }
    }

    pub fn descriptor(&self) -> &'static ServiceDescriptor {
        self.descriptor
    }

    pub fn handler(&self) -> &Arc<dyn DynHandler> {
        &self.handler
    }

    /// Consume the layer and return its parts.
    pub fn into_parts(self) -> (&'static ServiceDescriptor, Arc<dyn DynHandler>) {
        (self.descriptor, self.handler)
    }
}

impl core::fmt::Debug for Layer {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Layer")
            .field("descriptor", &self.descriptor.service_name)
            .finish_non_exhaustive()
    }
}

// ── LayerSet ──────────────────────────────────────────────────────────────

/// A bundle of [`Layer`]s. Built incrementally with `add` / `merge`, then
/// mounted onto a [`LayerRouter`].
///
/// The order of layers is preserved; routers that care about insertion
/// order (e.g. for tie-breaking on duplicate method IDs) see layers in the
/// order they were added.
#[derive(Default, Clone)]
pub struct LayerSet {
    layers: Vec<Layer>,
}

impl LayerSet {
    pub fn new() -> Self {
        Self::default()
    }

    /// Number of layers in the set.
    pub fn len(&self) -> usize {
        self.layers.len()
    }

    pub fn is_empty(&self) -> bool {
        self.layers.is_empty()
    }

    /// Add one layer (or anything convertible to a `LayerSet` — including
    /// another `LayerSet`). Method-chain friendly.
    ///
    /// Semantically equivalent to [`Self::merge`]; `add` reads better when
    /// the right-hand side is a single layer, `merge` reads better when
    /// it's another set. Both accept either.
    pub fn add<L: IntoLayerSet>(self, layer: L) -> Self {
        self.merge(layer)
    }

    /// Merge another layer / layer set into this one. Returns the
    /// combined set.
    ///
    /// ```ignore
    /// let core = core_layers(reaper);
    /// let ui   = ui_layers(dock_host);
    /// let all  = core.merge(ui).merge(extra_layer);
    /// ```
    pub fn merge<L: IntoLayerSet>(mut self, other: L) -> Self {
        self.layers.extend(other.into_layer_set().layers);
        self
    }

    /// Merge many layers / sets at once. Convenient when the bundle
    /// arrives as an iterator / array.
    pub fn extend<I>(mut self, layers: I) -> Self
    where
        I: IntoIterator,
        I::Item: IntoLayerSet,
    {
        for l in layers {
            self.layers.extend(l.into_layer_set().layers);
        }
        self
    }

    /// Mount every contained layer onto the supplied router.
    pub fn mount<R: LayerSink>(self, mut router: R) -> R {
        for layer in self.layers {
            router.add_layer(layer);
        }
        router
    }

    /// Borrow the descriptors without consuming the set. Useful for
    /// building service catalogs / docs without tearing down the bundle.
    pub fn descriptors(&self) -> impl Iterator<Item = &'static ServiceDescriptor> + '_ {
        self.layers.iter().map(|l| l.descriptor)
    }

    /// Iterate the contained layers without consuming the set.
    pub fn iter(&self) -> impl Iterator<Item = &Layer> + '_ {
        self.layers.iter()
    }

    /// Consume the set and yield the layers in insertion order.
    pub fn into_layers(self) -> Vec<Layer> {
        self.layers
    }
}

impl core::fmt::Debug for LayerSet {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("LayerSet")
            .field("len", &self.layers.len())
            .finish_non_exhaustive()
    }
}

impl FromIterator<Layer> for LayerSet {
    fn from_iter<I: IntoIterator<Item = Layer>>(iter: I) -> Self {
        Self {
            layers: iter.into_iter().collect(),
        }
    }
}

// ── IntoLayerSet ──────────────────────────────────────────────────────────

/// Anything that turns into a [`LayerSet`]. Implemented for `Layer`
/// (single-item set) and for `LayerSet` itself (identity), so
/// [`LayerSet::merge`] / [`LayerSet::add`] accept either.
pub trait IntoLayerSet {
    fn into_layer_set(self) -> LayerSet;
}

impl IntoLayerSet for Layer {
    fn into_layer_set(self) -> LayerSet {
        LayerSet { layers: vec![self] }
    }
}

impl IntoLayerSet for LayerSet {
    fn into_layer_set(self) -> LayerSet {
        self
    }
}

// ── LayerRouter ───────────────────────────────────────────────────────────

/// Anything that can receive layers. Implemented by per-binary RPC
/// routers (vox method-id routers, in-process handler tables, etc.).
///
/// Method-chain ergonomics: [`LayerSet::mount`] returns the router by
/// value, so call sites can be chained:
///
/// ```ignore
/// let routed = my_layers(reaper)
///     .merge(dock_layer)
///     .mount(RoutedHandler::new());
/// ```
pub trait LayerSink {
    /// Register `layer` with the router. The router decides how to wire
    /// the descriptor's methods to the layer's handler — typically by
    /// mapping each `method_id` to the handler.
    fn add_layer(&mut self, layer: Layer);
}

// ── Router (the canonical LayerRouter impl) ───────────────────────────────

/// Method-ID-keyed handler table. The canonical [`LayerRouter`] /
/// [`vox::Handler<DriverReplySink>`] sink for layers.
///
/// Most callers never build this directly — [`LayerSet::serve`] returns
/// one fully populated. The constructor and [`Router::with`] are exposed
/// for one-off registrations and for routers that want to hand-build
/// dispatch tables.
///
/// Cloning is cheap (handlers are behind `Arc`s).
#[derive(Default, Clone)]
pub struct LayerRouter {
    method_map: std::collections::HashMap<vox::MethodId, usize>,
    handlers: Vec<Arc<dyn DynHandler>>,
}

impl LayerRouter {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a single (descriptor, handler) pair. Lower-level entry
    /// point — prefer `LayerSet::serve()` for bundles.
    pub fn with<H>(mut self, descriptor: &'static ServiceDescriptor, handler: H) -> Self
    where
        H: Handler<DriverReplySink> + Send + Sync + 'static,
    {
        self.register(descriptor, Arc::new(handler));
        self
    }

    fn register(&mut self, descriptor: &ServiceDescriptor, handler: Arc<dyn DynHandler>) {
        let idx = self.handlers.len();
        self.handlers.push(handler);
        for method in descriptor.methods {
            self.method_map.insert(method.id, idx);
        }
    }

    /// Number of registered service descriptors / handlers. Each
    /// service contributes one entry regardless of how many methods it
    /// exposes.
    pub fn len(&self) -> usize {
        self.handlers.len()
    }

    pub fn is_empty(&self) -> bool {
        self.handlers.is_empty()
    }
}

impl LayerSink for LayerRouter {
    fn add_layer(&mut self, layer: Layer) {
        let (descriptor, handler) = layer.into_parts();
        self.register(descriptor, handler);
    }
}

impl Handler<DriverReplySink> for LayerRouter {
    fn retry_policy(&self, method_id: vox::MethodId) -> vox::RetryPolicy {
        self.method_map
            .get(&method_id)
            .map(|&idx| self.handlers[idx].retry_policy(method_id))
            .unwrap_or(vox::RetryPolicy::VOLATILE)
    }

    fn args_have_channels(&self, method_id: vox::MethodId) -> bool {
        self.method_map
            .get(&method_id)
            .map(|&idx| self.handlers[idx].args_have_channels(method_id))
            .unwrap_or(false)
    }

    fn response_wire_shape(&self, method_id: vox::MethodId) -> Option<&'static facet::Shape> {
        self.method_map
            .get(&method_id)
            .and_then(|&idx| self.handlers[idx].response_wire_shape(method_id))
    }

    async fn handle(
        &self,
        call: SelfRef<RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: Arc<SchemaRecvTracker>,
    ) {
        let method_id = call.get().method_id;
        if let Some(&idx) = self.method_map.get(&method_id) {
            self.handlers[idx].handle(call, reply, schemas).await;
        } else {
            use vox::ReplySink as _;
            reply
                .send_error(vox::VoxError::<core::convert::Infallible>::UnknownMethod)
                .await;
        }
    }
}

// ── LayerSet::serve — the one-call mount ──────────────────────────────────

impl LayerSet {
    /// Mount every layer onto a fresh [`Router`] and return it. The
    /// produced router implements [`vox::Handler<DriverReplySink>`] —
    /// drop it straight into a vox driver / connection acceptor.
    ///
    /// ```ignore
    /// let handler = daw_layers(reaper)
    ///     .merge(dock_host::layer(dock_host))
    ///     .serve();
    /// ```
    pub fn serve(self) -> LayerRouter {
        self.mount(LayerRouter::new())
    }
}
