//! Effect-style layer composition — one combinator, [`Layer::merge`].
//!
//! Mirrors Effect-ts: a [`Layer`] is the single composable unit, and
//! [`Layer::merge`] is the only combinator users need. Service tokens
//! emitted by `#[architect::rpc]` are themselves one-element layers,
//! so tokens and pre-built bundles compose the same way.
//!
//! ```ignore
//! use architect::{Layer, layers};
//! use daw_proto::{transport, project, marker};
//!
//! // Build a bundle:
//! let bundle = layers![transport::Service, project::Service, marker::Service];
//!
//! // Bind and route:
//! let router = bundle.provide(Reaper);
//!
//! // Compose sub-bundles via .merge() — same call site shape:
//! let timeline = layers![transport::Service, marker::Service];
//! let routing  = layers![project::Service];
//! let router   = timeline.merge(routing).provide(Reaper);
//!
//! // Override / bolt-on (last-add wins on method_id):
//! let router = layers![transport::Service, project::Service]
//!     .merge(fx_chains::mock())          // override
//!     .merge(dock_host::layer(dh))       // bolt-on, different backend
//!     .provide(Reaper);
//! ```
//!
//! Bundle definitions need **no where clause** — service tokens defer
//! backend binding to `.provide(B)` time. Forgetting an impl surfaces
//! at the `.provide(...)` call site, naming the missing trait.
//!
//! # The pieces
//!
//! - [`BindAny`] — "I know my descriptor." Backend-free.
//! - [`Bind<B>`] — `BindAny` + "given backend B, produce a [`Mounted`]."
//!   Macro-emitted per service.
//! - [`Mounted`] — a service that's been bound. One-element layer.
//! - [`Empty`] / [`Cons<S, R>`] — type-level list of services.
//!   Hidden behind `impl Layer` at function return sites.
//! - [`Layer`] — exposes `merge` / `provide` / `descriptors`.
//!   The `Bind<B>` chain impl is recursive: `Cons<S, R>: Bind<B>`
//!   requires `S: Bind<B>` and `R: Bind<B>`, so a missing per-service
//!   impl surfaces at `.provide(B)` naming the trait.
//! - [`Append<R>`] — type-level concat backing `Layer::merge`.
//! - [`LayerRouter`] — the terminal sink, implements
//!   [`vox::Handler<DriverReplySink>`].

use core::any::Any;
use std::collections::HashMap;
use std::pin::Pin;
use std::sync::Arc;

use vox::{
    DriverReplySink, Handler, MethodId, RequestCall, RetryPolicy, SchemaRecvTracker, SelfRef,
    ServiceDescriptor,
};

// ── Erased handler ────────────────────────────────────────────────────────
//
// Send / Sync requirements gated on target_arch — vox's Handler
// future is `+ MaybeSend` (non-Send on wasm32). Native keeps the
// thread bounds for tokio multi-thread executors.

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

/// A service bound to a backend — descriptor + erased handler.
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

// ── BindAny / Bind ────────────────────────────────────────────────────────

/// Backend-free trait — "I know my descriptor." Implemented by every
/// service token and by [`Mounted`].
pub trait BindAny {
    fn descriptor(&self) -> &'static ServiceDescriptor;
}

/// Backend-aware bind — "given backend `B`, register this thing
/// (service token, pre-mounted service, or whole chain) into a
/// [`LayerRouter`]."
///
/// One trait for every form of binding. The `#[architect::rpc]`
/// derive emits an impl per service token; [`Empty`] / [`Cons`] /
/// [`Mounted`] get blanket impls in this crate. The chain impl
/// walks recursively, requiring each service to impl `Bind<B>` —
/// the bound check at [`Layer::provide`] cascades through and
/// surfaces missing impls at the call site.
#[diagnostic::on_unimplemented(
    message = "backend `{B}` cannot serve this service",
    label = "no `Bind<{B}>` impl — `{Self}` likely does not implement \
             the underlying RPC trait for `{B}` (or `{B}` is missing a \
             required bound such as `HasDispatcher` / `Send` / `Sync` / \
             `'static`).",
    note = "every service token in a `Layer` must impl `Bind<B>` for \
            the backend you pass to `.provide(B)`. Check the trait \
            impls on `{B}` for this service's underlying trait."
)]
pub trait Bind<B>: BindAny {
    /// Register self into the router. Per-service tokens build a
    /// `Mounted` from `backend.clone()` and call `router.add_mounted`;
    /// chains (`Cons`) walk their elements; `Mounted` registers
    /// itself directly.
    fn bind_into(self, backend: &B, router: &mut LayerRouter);
}

impl BindAny for Mounted {
    fn descriptor(&self) -> &'static ServiceDescriptor {
        self.descriptor
    }
}

impl<B> Bind<B> for Mounted {
    fn bind_into(self, _: &B, router: &mut LayerRouter) {
        router.add_mounted(self);
    }
}

impl BindAny for Empty {
    fn descriptor(&self) -> &'static ServiceDescriptor {
        &ServiceDescriptor::EMPTY
    }
}

impl<B> Bind<B> for Empty {
    fn bind_into(self, _: &B, _: &mut LayerRouter) {}
}

impl<S, R> BindAny for Cons<S, R>
where
    S: BindAny,
{
    fn descriptor(&self) -> &'static ServiceDescriptor {
        self.svc.descriptor()
    }
}

impl<B, S, R> Bind<B> for Cons<S, R>
where
    S: Bind<B>,
    R: Bind<B>,
{
    fn bind_into(self, backend: &B, router: &mut LayerRouter) {
        self.svc.bind_into(backend, router);
        self.rest.bind_into(backend, router);
    }
}

// ── Empty / Cons ──────────────────────────────────────────────────────────

/// Empty layer — base case of the cons chain.
#[derive(Debug, Default, Clone, Copy)]
pub struct Empty;

/// One service cell prepended to a tail layer. Built by
/// [`Layer::merge`] when a service token is merged into a layer.
pub struct Cons<S, R> {
    svc: S,
    rest: R,
}

impl<S, R> Cons<S, R> {
    pub fn new(svc: S, rest: R) -> Self {
        Self { svc, rest }
    }
}

// ── Layer trait ───────────────────────────────────────────────────────────

/// The composable layer. Implemented by [`Empty`], [`Cons`],
/// [`Mounted`], and (via the `#[architect::rpc]` derive) by each
/// service's `Service` token. User code only interacts through the
/// trait's methods.
pub trait Layer: Descriptors + Sized {
    /// Merge a bound service into this layer. Mirrors Effect-ts's
    /// `Layer.merge` for the common bolt-on / override case — pass
    /// anything convertible into a [`Mounted`] (typically the result
    /// of a service's `layer(backend)` function or a `mock()`
    /// builder).
    ///
    /// Works on any [`Layer`] including the opaque return of
    /// [`Services::layers`], so call sites can chain
    /// `Reaper::layers().merge(mock()).merge(bolt_on()).provide(Reaper)`.
    ///
    /// On duplicate method IDs the **last merged** handler wins —
    /// that's how overrides and mocks compose.
    ///
    /// To compose two cons-chained sub-bundles (both built from
    /// service tokens), use the [`layers!`] macro instead — it
    /// concatenates via [`Append`] internally.
    fn merge<M>(self, m: M) -> Cons<Mounted, Self>
    where
        M: Into<Mounted>,
    {
        Cons::new(m.into(), self)
    }

    /// Bind a backend and produce a [`LayerRouter`]. The bound
    /// `Self: Bind<B>` recursively requires every service in this
    /// chain to implement `Bind<B>`. If any one fails, the error
    /// surfaces here, naming the missing trait.
    ///
    /// Per-service `Bind<B>` impls usually require `B: Clone`
    /// (they clone the backend per service to construct each
    /// `Mounted`). For non-`Clone` backends, wrap in `Arc<Backend>`
    /// and impl the per-service traits on `Arc<Backend>` (or use a
    /// `&'static` borrow). Zero-sized / `Copy` backends like
    /// REAPER's stateless `Reaper` token pay nothing here.
    fn provide<B>(self, backend: B) -> LayerRouter
    where
        Self: Bind<B>,
    {
        let mut router = LayerRouter::new();
        self.bind_into(&backend, &mut router);
        router
    }

    /// Collect descriptors of every service in this layer — useful
    /// for capability lists / introspection before providing a
    /// backend. Available on any [`Layer`]; the [`Descriptors`]
    /// supertrait guarantees the walk.
    fn descriptors(&self) -> Vec<&'static ServiceDescriptor> {
        let mut v = Vec::new();
        Descriptors::collect(self, &mut v);
        v
    }
}

impl Layer for Empty {}
impl<S, R> Layer for Cons<S, R>
where
    S: BindAny,
    R: Layer,
{
}
impl Layer for Mounted {}

// ── Append<R> ─────────────────────────────────────────────────────────────

/// Type-level concat. `<Cons<A, Cons<B, Empty>> as Append<R>>::Output
/// = Cons<A, Cons<B, R>>`.
pub trait Append<R: Layer>: Layer {
    type Output: Layer;
    fn append(self, rhs: R) -> Self::Output;
}

impl<R: Layer> Append<R> for Empty {
    type Output = R;
    fn append(self, rhs: R) -> R {
        rhs
    }
}

impl<S, T, R> Append<R> for Cons<S, T>
where
    S: BindAny,
    T: Append<R>,
    R: Layer,
{
    type Output = Cons<S, <T as Append<R>>::Output>;
    fn append(self, rhs: R) -> Self::Output {
        Cons {
            svc: self.svc,
            rest: self.rest.append(rhs),
        }
    }
}

impl<R: Layer> Append<R> for Mounted {
    type Output = Cons<Mounted, R>;
    fn append(self, rhs: R) -> Self::Output {
        Cons {
            svc: self,
            rest: rhs,
        }
    }
}

// ── Descriptors ───────────────────────────────────────────────────────────

/// Walks the chain producing each service's descriptor.
pub trait Descriptors {
    fn collect(&self, out: &mut Vec<&'static ServiceDescriptor>);
}

impl Descriptors for Empty {
    fn collect(&self, _: &mut Vec<&'static ServiceDescriptor>) {}
}

impl<S, R> Descriptors for Cons<S, R>
where
    S: BindAny,
    R: Descriptors,
{
    fn collect(&self, out: &mut Vec<&'static ServiceDescriptor>) {
        out.push(self.svc.descriptor());
        self.rest.collect(out);
    }
}

impl Descriptors for Mounted {
    fn collect(&self, out: &mut Vec<&'static ServiceDescriptor>) {
        out.push(self.descriptor);
    }
}

// ── layers! macro ─────────────────────────────────────────────────────────

/// Build a [`Layer`] from a variadic list of layers — service tokens,
/// pre-mounted services, or already-composed sub-bundles all compose
/// uniformly. Rust's analog of Effect-ts's `Layer.mergeAll(...)`.
///
/// ```ignore
/// // Tokens only:
/// let router = layers![
///     transport::Service,
///     project::Service,
///     marker::Service,
/// ].provide(Reaper);
///
/// // Mix tokens, pre-mounted bolt-ons, and sub-bundles:
/// let timeline = layers![transport::Service, marker::Service];
/// let router = layers![
///     timeline,
///     project::Service,
///     dock_host::layer(dock_host_backend),  // pre-mounted, different backend
/// ].provide(Reaper);
/// ```
#[macro_export]
macro_rules! layers {
    () => { $crate::Empty };
    ($($svc:expr),+ $(,)?) => {{
        // Always terminate the cons chain in `Empty` so the per-layer
        // walker trait (`Descriptors`) bottoms out on the `Empty`
        // base impl rather than on the last service token.
        let __l = $crate::Empty;
        $(let __l = $crate::Append::append($svc, __l);)+
        __l
    }};
}

// ── Services trait ────────────────────────────────────────────────────────

/// "This backend provides a canonical bundle of services."
///
/// Implement once per backend (REAPER, Pro Tools, mock, …) declaring
/// which services the backend ships as its default surface. Callers
/// then get the full router in one call:
///
/// ```ignore
/// use architect::Services;
///
/// let router = Reaper.into_router();
/// ```
///
/// # Overriding a service
///
/// `LayerRouter` resolves duplicate method-ids by **last-merge wins** —
/// merge the override after the default bundle and it takes effect.
/// The default handler stays in memory but becomes unreachable.
///
/// ```ignore
/// let router = Reaper::layers()
///     .merge(fx_chains::mock())     // overrides the default fx_chains
///     .merge(dock_host::layer(dh))  // bolt-on, different backend
///     .provide(Reaper);
/// ```
///
/// # Sub-bundles
///
/// Compose groups of services with [`Layer::merge`] or `layers![...]`:
///
/// ```ignore
/// let timeline = layers![transport::Service, marker::Service, region::Service];
/// let routing  = layers![project::Service, routing::Service, track::Service];
/// let bundle   = layers![timeline, routing, fx_chains::mock()];
/// let router   = bundle.provide(Reaper);
/// ```
pub trait Services: Sized {
    /// Build the deferred bundle for this backend. Returns an opaque
    /// [`LayerBundle<Self>`] — a [`Layer`] that can be bound to
    /// `Self` via `.provide(self)`, introspected with
    /// `.descriptors()`, and extended with `.merge(...)`.
    fn layers() -> impl LayerBundle<Self>;

    /// Convenience: build the bundle, bind `self`, return the
    /// terminal router. One-call mount when no overrides are needed.
    fn into_router(self) -> LayerRouter
    where
        Self: Clone + Send + Sync + 'static,
    {
        Self::layers().provide(self)
    }
}

/// Trait alias for the three bounds every `Services::layers()` return
/// type carries: composable ([`Layer`]), bindable to backend `B`
/// ([`Bind<B>`]), and walkable for introspection
/// ([`Descriptors`]).
///
/// Lets per-backend `Services` impls write:
///
/// ```ignore
/// fn layers() -> impl LayerBundle<Reaper> {
///     layers![transport::Service, project::Service, /* … */]
/// }
/// ```
///
/// instead of repeating `impl Layer + Bind<Reaper>`.
/// Auto-implemented for every type satisfying the three bounds — you
/// never write the impl by hand.
pub trait LayerBundle<B>: Layer + Bind<B> {}

impl<T, B> LayerBundle<B> for T where T: Layer + Bind<B> {}

// ── LayerSink ─────────────────────────────────────────────────────────────

/// Anything that can absorb a [`Mounted`]. Implemented by
/// [`LayerRouter`]; downstream consumers can implement for custom
/// dispatchers.
pub trait LayerSink {
    fn add_mounted(&mut self, mounted: Mounted);
}

// ── LayerRouter ───────────────────────────────────────────────────────────

/// Method-id-keyed dispatch + canonical [`vox::Handler<DriverReplySink>`]
/// impl. The terminal sink for layers.
#[derive(Default, Clone)]
pub struct LayerRouter {
    method_map: HashMap<MethodId, usize>,
    handlers: Vec<Arc<dyn DynHandler>>,
}

impl LayerRouter {
    pub fn new() -> Self {
        Self::default()
    }

    /// Lower-level entry — prefer [`Layer::provide`] for bundles.
    pub fn with<H>(mut self, descriptor: &'static ServiceDescriptor, handler: H) -> Self
    where
        H: Handler<DriverReplySink> + Send + Sync + 'static,
    {
        self.register(descriptor, Arc::new(handler));
        self
    }

    /// Runtime bolt-on: merge a [`Mounted`] (or anything `Into<Mounted>`,
    /// like a service's `layer(backend)` result) into this already-built
    /// router. Parallels [`Layer::merge`] for the
    /// already-provided-then-extended case, e.g. loading a plugin
    /// service after the main bundle is mounted. Last-merge wins on
    /// duplicate method IDs.
    pub fn merge<M: Into<Mounted>>(mut self, m: M) -> Self {
        let (descriptor, handler) = m.into().into_parts();
        self.register(descriptor, handler);
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
