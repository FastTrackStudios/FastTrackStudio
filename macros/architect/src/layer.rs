//! Effect-style layer composition — bounds-free bundle definition.
//!
//! A [`Layer`] is built from a list of services. The list is a
//! type-level cons chain ([`Empty`] / [`Cons<S, R>`]) so the bundle
//! definition needs **no where clause** — each service's backend
//! requirements only flow into the type when [`Layer::provide`] picks
//! a concrete backend. Forgetting a trait impl on a backend surfaces
//! as a compile error at the `.provide(...)` call site, naming the
//! missing trait.
//!
//! ```ignore
//! use architect::{Layer, services};
//! use daw_proto::{transport, project, marker};
//!
//! pub fn services() -> impl Layer {
//!     services![transport, project, marker]   // no bounds anywhere
//! }
//!
//! // Or inline:
//! let router = services![transport, project, marker].provide(Reaper);
//! ```
//!
//! # The pieces
//!
//! - [`BindAny`] — "I know my descriptor." Backend-free.
//! - [`Bind<B>`] — `BindAny` + "given backend B, produce a [`Mounted`]."
//!   Macro-emitted per service.
//! - [`Mounted`] — a service that's been bound. Implements both
//!   `BindAny` and `Bind<B> for any B`, so already-mounted services
//!   compose into a layer the same way deferred tokens do.
//! - [`Empty`] / [`Cons<S, R>`] — the type-level list of services.
//!   Hidden behind `impl Layer` at function return sites.
//! - [`Layer`] — exposes `add` / `merge` / `provide` / `descriptors`.
//! - [`ProvideAll<B>`] — bound checked at `.provide(B)`; recursively
//!   requires `S: Bind<B>` for every service in the chain.
//! - [`Append<R>`] — type-level concat for `Layer::merge`.
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

// ── Mounted ───────────────────────────────────────────────────────────────

/// A service bound to a backend — descriptor + erased handler.
#[derive(Clone)]
pub struct Mounted {
    descriptor: &'static ServiceDescriptor,
    handler: Arc<dyn DynHandler>,
}

impl Mounted {
    pub fn new<H>(descriptor: &'static ServiceDescriptor, handler: H) -> Self
    where
        H: Handler<DriverReplySink> + Send + Sync + 'static,
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
/// service token and by [`Mounted`]. Lets [`Layer::add`] accept
/// services without committing to a backend type.
pub trait BindAny {
    fn descriptor(&self) -> &'static ServiceDescriptor;
}

/// Backend-aware bind — "given backend `B`, produce a [`Mounted`]."
/// The architect-rpc macro emits an impl of this trait per service,
/// with the trait bounds the underlying RPC dispatcher requires.
/// [`Mounted`] implements `Bind<B>` for any `B` (returns self,
/// ignores `B`) so already-bound services compose in the same chain.
pub trait Bind<B>: BindAny {
    fn bind(self, backend: B) -> Mounted;
}

impl BindAny for Mounted {
    fn descriptor(&self) -> &'static ServiceDescriptor {
        self.descriptor
    }
}

impl<B> Bind<B> for Mounted {
    fn bind(self, _: B) -> Mounted {
        self
    }
}

// ── Empty / Cons ──────────────────────────────────────────────────────────

/// Empty layer — base case of the cons chain.
#[derive(Debug, Default, Clone, Copy)]
pub struct Empty;

/// One service cell prepended to a tail layer. Built by
/// [`Layer::add`].
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

/// The composable layer. Sealed at the impl level via [`Empty`],
/// [`Cons`], and [`Mounted`]; user code only interacts through the
/// trait's methods.
pub trait Layer: Sized {
    /// Prepend a service (or a pre-mounted service via [`Mounted`])
    /// to this layer. Order of services in the resulting chain
    /// doesn't matter for dispatch (method IDs are unique).
    fn add<S>(self, svc: S) -> Cons<S, Self>
    where
        S: BindAny,
    {
        Cons { svc, rest: self }
    }

    /// Type-level concat with another layer. The result is a new
    /// `Layer` containing every service from both sides.
    fn merge<R>(self, other: R) -> <Self as Append<R>>::Output
    where
        Self: Append<R>,
        R: Layer,
    {
        Append::append(self, other)
    }

    /// Bind a backend and produce a [`LayerRouter`]. The bound
    /// `Self: ProvideAll<B>` recursively requires every service in
    /// the chain to implement `Bind<B>`. If any one fails, the error
    /// surfaces here, naming the missing trait.
    fn provide<B>(self, backend: B) -> LayerRouter
    where
        Self: ProvideAll<B>,
        B: Clone,
    {
        let mut router = LayerRouter::new();
        self.provide_into(&backend, &mut router);
        router
    }

    /// Collect descriptors of every service in this layer — useful
    /// for capability lists / introspection before providing a
    /// backend.
    fn descriptors(&self) -> Vec<&'static ServiceDescriptor>
    where
        Self: Descriptors,
    {
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

// ── ProvideAll<B> ─────────────────────────────────────────────────────────

/// Implemented by any chain whose services all implement `Bind<B>`.
/// The bound check at [`Layer::provide`] uses this — `Self:
/// ProvideAll<B>` cascades down into per-service `Bind<B>` checks.
pub trait ProvideAll<B>: Layer {
    fn provide_into(self, backend: &B, router: &mut LayerRouter);
}

impl<B> ProvideAll<B> for Empty {
    fn provide_into(self, _: &B, _: &mut LayerRouter) {}
}

impl<B, S, R> ProvideAll<B> for Cons<S, R>
where
    B: Clone,
    S: Bind<B>,
    R: ProvideAll<B>,
{
    fn provide_into(self, backend: &B, router: &mut LayerRouter) {
        router.add_mounted(self.svc.bind(backend.clone()));
        self.rest.provide_into(backend, router);
    }
}

impl<B> ProvideAll<B> for Mounted {
    fn provide_into(self, _: &B, router: &mut LayerRouter) {
        router.add_mounted(self);
    }
}

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
pub trait Descriptors: Layer {
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

// ── layers! / services! macros ────────────────────────────────────────────

/// Build a [`Layer`] from a list of [`BindAny`] values (service tokens,
/// pre-mounted services, …). Rust's analog of Effect's
/// `Layer.mergeAll(...)`.
///
/// ```ignore
/// let router = layers![
///     transport::Service,
///     project::Service,
///     dock_host::layer(dock_host_backend),   // pre-mounted, different backend
/// ].provide(Reaper);
/// ```
#[macro_export]
macro_rules! layers {
    () => { $crate::Empty };
    ($($svc:expr),+ $(,)?) => {{
        let __l = $crate::Empty;
        $(let __l = $crate::Layer::add(__l, $svc);)+
        __l
    }};
}

/// Ident-form shortcut for [`layers!`] — appends `::Service` to each
/// ident. The terser form when every entry is the macro-emitted
/// `Service` token of a per-trait module.
///
/// ```ignore
/// let router = services![transport, project, marker].provide(Reaper);
/// ```
#[macro_export]
macro_rules! services {
    () => { $crate::Empty };
    ($($svc:ident),+ $(,)?) => {{
        let __l = $crate::Empty;
        $(let __l = $crate::Layer::add(__l, $svc::Service);)+
        __l
    }};
}

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
