//! Effect-style layer composition for architect-rpc backends.
//!
//! Mirrors Effect's `@effect/platform` shape: a single composable
//! [`Layer`] value accumulates services, [`Layer::merge`] folds two
//! layers into one, and [`Layer::provide`] binds the backend value
//! and produces a ready-to-serve [`LayerRouter`].
//!
//! ```ignore
//! use architect::Layer;
//! use daw_proto::{transport, project, marker};
//!
//! let router = Layer::new()
//!     .add(transport::Service)
//!     .add(project::Service)
//!     .add(marker::Service)
//!     .provide(reaper);
//! ```
//!
//! # Why deferred bind
//!
//! Pre-deferred, every consumer wrote `transport::layer(reaper)` per
//! service — the backend value was threaded through 25× per mount
//! site. The deferred shape separates **what services do I want** from
//! **what backend serves them**:
//!
//! - One `Layer` value can be reused across backends by varying only
//!   `.provide()`.
//! - Service tokens (`transport::Service`, …) are backend-agnostic
//!   values, mergeable into bundles before any backend is in scope.
//! - Bolt-ons with different backends mix cleanly via [`Layer::add`]
//!   — a pre-mounted [`Mounted`] still implements [`Bind`].
//!
//! # The pieces
//!
//! - [`Mounted`] — a service that's been bound to a backend.
//! - [`Bind`] — "given a backend, produce a [`Mounted`]." Implemented
//!   by the per-service `Service` token the rpc macro emits, and
//!   trivially by [`Mounted`] itself.
//! - [`Layer<B>`] — the composable layer value. `.add(...)` /
//!   `.merge(...)` / `.provide(B)`.
//! - [`LayerRouter`] — method-id-keyed dispatch + [`vox::Handler`]
//!   impl. The terminal sink.
//! - [`LayerSink`] — trait for "absorbs a [`Mounted`]."

use core::any::Any;
use core::marker::PhantomData;
use std::collections::HashMap;
use std::pin::Pin;
use std::sync::Arc;

use vox::{
    DriverReplySink, Handler, MethodId, RequestCall, RetryPolicy, SchemaRecvTracker, SelfRef,
    ServiceDescriptor,
};

// ── Erased handler ────────────────────────────────────────────────────────
//
// Send / Sync requirements are gated on target_arch — vox's Handler
// returns `impl Future + MaybeSend` which is non-Send on wasm32.
// Without the cfg the wasm build can't satisfy `+ Send`.

#[cfg(not(target_arch = "wasm32"))]
pub trait DynHandler: Send + Sync + 'static {
    fn handle(
        &self,
        call: SelfRef<RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: Arc<SchemaRecvTracker>,
    ) -> Pin<Box<dyn core::future::Future<Output = ()> + Send + '_>>;

    fn retry_policy(&self, method_id: MethodId) -> RetryPolicy;
    fn args_have_channels(&self, method_id: MethodId) -> bool;
    fn response_wire_shape(&self, method_id: MethodId) -> Option<&'static facet::Shape>;
    fn as_any(&self) -> &dyn Any;
}

#[cfg(not(target_arch = "wasm32"))]
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

// ── wasm32 variant: no Send/Sync ──────────────────────────────────────────

#[cfg(target_arch = "wasm32")]
pub trait DynHandler: 'static {
    fn handle(
        &self,
        call: SelfRef<RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: Arc<SchemaRecvTracker>,
    ) -> Pin<Box<dyn core::future::Future<Output = ()> + '_>>;

    fn retry_policy(&self, method_id: MethodId) -> RetryPolicy;
    fn args_have_channels(&self, method_id: MethodId) -> bool;
    fn response_wire_shape(&self, method_id: MethodId) -> Option<&'static facet::Shape>;
    fn as_any(&self) -> &dyn Any;
}

#[cfg(target_arch = "wasm32")]
impl<H> DynHandler for H
where
    H: Handler<DriverReplySink> + 'static,
{
    fn handle(
        &self,
        call: SelfRef<RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: Arc<SchemaRecvTracker>,
    ) -> Pin<Box<dyn core::future::Future<Output = ()> + '_>> {
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

// ── Mounted ───────────────────────────────────────────────────────────────

/// A service that has been bound to a backend — descriptor + erased
/// handler ready for [`LayerSink`].
#[derive(Clone)]
pub struct Mounted {
    descriptor: &'static ServiceDescriptor,
    handler: Arc<dyn DynHandler>,
}

impl Mounted {
    #[cfg(not(target_arch = "wasm32"))]
    pub fn new<H>(descriptor: &'static ServiceDescriptor, handler: H) -> Self
    where
        H: Handler<DriverReplySink> + Send + Sync + 'static,
    {
        Self {
            descriptor,
            handler: Arc::new(handler),
        }
    }

    #[cfg(target_arch = "wasm32")]
    pub fn new<H>(descriptor: &'static ServiceDescriptor, handler: H) -> Self
    where
        H: Handler<DriverReplySink> + 'static,
    {
        Self {
            descriptor,
            handler: Arc::new(handler),
        }
    }

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

    pub fn into_parts(self) -> (&'static ServiceDescriptor, Arc<dyn DynHandler>) {
        (self.descriptor, self.handler)
    }
}

impl core::fmt::Debug for Mounted {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Mounted")
            .field("descriptor", &self.descriptor.service_name)
            .finish_non_exhaustive()
    }
}

// ── Bind trait ────────────────────────────────────────────────────────────

/// "Given a backend `B`, produce a [`Mounted`]."
///
/// Implemented by the per-service `Service` token the
/// `#[architect::rpc]` macro emits — the bound on `B` is the same set
/// of supertraits the architect-rpc-emitted `serve(backend)` requires.
///
/// Also implemented trivially by [`Mounted`] itself (returns self,
/// ignores `B`), so already-mounted services compose with deferred
/// tokens through the same [`Layer::add`] entry point.
pub trait Bind<B> {
    /// Descriptor of the service this binder will produce. Available
    /// before `.bind()` so layers can be introspected before
    /// providing a backend.
    fn descriptor(&self) -> &'static ServiceDescriptor;

    fn bind(self, backend: B) -> Mounted;
}

impl<B> Bind<B> for Mounted {
    fn descriptor(&self) -> &'static ServiceDescriptor {
        self.descriptor
    }
    fn bind(self, _: B) -> Mounted {
        self
    }
}

// ── Layer ─────────────────────────────────────────────────────────────────

/// A composable layer of services keyed to a backend type.
///
/// `Layer<B>` accumulates `Bind<B>` entries via [`Self::add`] /
/// [`Self::merge`] and resolves them into a [`LayerRouter`] via
/// [`Self::provide`].
///
/// **Inference**: when the chain ends with `.provide(backend)`, `B`
/// usually infers from there — `Layer::new().add(...).provide(reaper)`
/// works without turbofish. If inference can't see the eventual
/// `.provide`, give `B` explicitly with `Layer::<Reaper>::new()`.
pub struct Layer<B> {
    entries: Vec<Entry<B>>,
    _phantom: PhantomData<fn(B)>,
}

struct Entry<B> {
    descriptor: &'static ServiceDescriptor,
    binder: Box<dyn FnOnce(B) -> Mounted + Send>,
}

impl<B> Default for Layer<B> {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
            _phantom: PhantomData,
        }
    }
}

impl<B> Layer<B>
where
    B: Clone + Send + 'static,
{
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a service (token or pre-mounted) to the layer.
    pub fn add<S>(mut self, svc: S) -> Self
    where
        S: Bind<B> + Send + 'static,
    {
        let descriptor = svc.descriptor();
        let binder: Box<dyn FnOnce(B) -> Mounted + Send> = Box::new(move |b| svc.bind(b));
        self.entries.push(Entry { descriptor, binder });
        self
    }

    /// Merge another layer in — the result is another `Layer<B>`.
    pub fn merge(mut self, other: Layer<B>) -> Self {
        self.entries.extend(other.entries);
        self
    }

    /// Bind the backend and produce a [`LayerRouter`].
    pub fn provide(self, backend: B) -> LayerRouter {
        let mut router = LayerRouter::new();
        for entry in self.entries {
            let mounted = (entry.binder)(backend.clone());
            router.add_mounted(mounted);
        }
        router
    }

    pub fn descriptors(&self) -> impl Iterator<Item = &'static ServiceDescriptor> + '_ {
        self.entries.iter().map(|e| e.descriptor)
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

impl<B> core::fmt::Debug for Layer<B> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Layer")
            .field("len", &self.entries.len())
            .finish_non_exhaustive()
    }
}

// ── layers! macro ─────────────────────────────────────────────────────────

/// Build a [`Layer`] from a comma-separated list of services. The
/// Rust analogue of Effect's `Layer.mergeAll(...)` — Rust has no
/// variadic generics, so the variadic call site arrives as a macro.
///
/// Each argument can be any [`Bind<B>`]: a service token, a
/// pre-mounted [`Mounted`], or another value implementing the trait.
///
/// ```ignore
/// use architect::layers;
/// use daw_proto::{transport, project, marker};
///
/// pub fn services() -> architect::Layer<Reaper> {
///     layers![
///         transport::Service,
///         project::Service,
///         marker::Service,
///     ]
/// }
/// ```
///
/// Expands to `Layer::new().add(a).add(b).add(c)` — saves boilerplate,
/// not runtime cost.
#[macro_export]
macro_rules! layers {
    () => { $crate::Layer::new() };
    ($($svc:expr),+ $(,)?) => {{
        let __layer = $crate::Layer::new();
        $(let __layer = $crate::Layer::add(__layer, $svc);)+
        __layer
    }};
}

/// Build a [`Layer`] from a comma-separated list of **service modules**.
///
/// Sibling of [`layers!`] for the common case where every entry is the
/// macro-emitted `Service` token of a per-trait module — `services!`
/// appends `::Service` to each ident so call sites read as a list of
/// service names rather than a list of values.
///
/// ```ignore
/// use architect::services;
/// use daw_proto::{transport, project, marker};
///
/// pub fn reaper_services() -> architect::Layer<Reaper> {
///     services![transport, project, marker]
/// }
/// ```
///
/// Equivalent to `layers![transport::Service, project::Service,
/// marker::Service]`. Use [`layers!`] instead when you need to mix
/// service tokens with pre-mounted services or other `Bind<B>`
/// values.
#[macro_export]
macro_rules! services {
    () => { $crate::Layer::new() };
    ($($svc:ident),+ $(,)?) => {{
        let __layer = $crate::Layer::new();
        $(let __layer = $crate::Layer::add(__layer, $svc::Service);)+
        __layer
    }};
}

// ── LayerSink ─────────────────────────────────────────────────────────────

pub trait LayerSink {
    fn add_mounted(&mut self, mounted: Mounted);
}

// ── LayerRouter ───────────────────────────────────────────────────────────

#[derive(Default, Clone)]
pub struct LayerRouter {
    method_map: HashMap<MethodId, usize>,
    handlers: Vec<Arc<dyn DynHandler>>,
}

impl LayerRouter {
    pub fn new() -> Self {
        Self::default()
    }

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

    pub fn len(&self) -> usize {
        self.handlers.len()
    }

    pub fn is_empty(&self) -> bool {
        self.handlers.is_empty()
    }
}

impl LayerSink for LayerRouter {
    fn add_mounted(&mut self, mounted: Mounted) {
        let (descriptor, handler) = mounted.into_parts();
        self.register(descriptor, handler);
    }
}

impl Handler<DriverReplySink> for LayerRouter {
    fn retry_policy(&self, method_id: MethodId) -> RetryPolicy {
        self.method_map
            .get(&method_id)
            .map(|&idx| self.handlers[idx].retry_policy(method_id))
            .unwrap_or(RetryPolicy::VOLATILE)
    }

    fn args_have_channels(&self, method_id: MethodId) -> bool {
        self.method_map
            .get(&method_id)
            .map(|&idx| self.handlers[idx].args_have_channels(method_id))
            .unwrap_or(false)
    }

    fn response_wire_shape(&self, method_id: MethodId) -> Option<&'static facet::Shape> {
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
