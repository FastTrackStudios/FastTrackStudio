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
//! - [`ProvideAll<B>`] — bound checked at `.provide(B)`; recursively
//!   requires `S: Bind<B>` for every service in the chain.
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

/// Backend-aware bind — "given backend `B`, produce a [`Mounted`]."
/// The architect-rpc macro emits an impl of this trait per service,
/// with the trait bounds the underlying RPC dispatcher requires.
/// [`Mounted`] implements `Bind<B>` for any `B` (returns self,
/// ignores `B`) so already-bound services compose in the same chain.
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
pub trait Layer: Sized {
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
    /// `Self: ProvideAll<B>` recursively requires every service in
    /// the chain to implement `Bind<B>`. If any one fails, the error
    /// surfaces here, naming the missing trait.
    ///
    /// `B: Clone` is required because each service's `bind(backend)`
    /// consumes a copy of the backend. For non-`Clone` backends, wrap
    /// in `Arc<Backend>` and impl the per-service traits on
    /// `Arc<Backend>` (or use a `&'static` borrow). Zero-sized /
    /// `Copy` backends like REAPER's stateless `Reaper` token pay
    /// nothing here.
    fn provide<B>(self, backend: B) -> LayerRouter
    where
        Self: ProvideAll<B> + Descriptors + RequiresAll,
        B: Clone,
    {
        self.check_requirements()
            .expect("layer requirements unsatisfied");
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

    /// Assert that every service this layer declares (via
    /// [`Requires`]) is also present in this same layer. Returns the
    /// list of unsatisfied `(owner, missing_required_service)` pairs
    /// — empty on success.
    ///
    /// Called automatically by [`Layer::provide`]; call directly if
    /// you want a non-panicking check (e.g. error-channel mount).
    fn check_requirements(&self) -> Result<(), Vec<RequirementGap>>
    where
        Self: Descriptors + RequiresAll,
    {
        let mut provided = Vec::new();
        Descriptors::collect(self, &mut provided);
        let mut needed = Vec::new();
        RequiresAll::collect(self, &mut needed);

        let provided_names: std::collections::HashSet<&'static str> =
            provided.iter().map(|d| d.service_name).collect();

        let gaps: Vec<RequirementGap> = needed
            .into_iter()
            .filter(|(_, req)| !provided_names.contains(req.service_name))
            .map(|(owner, req)| RequirementGap {
                owner: owner.service_name,
                missing: req.service_name,
            })
            .collect();

        if gaps.is_empty() { Ok(()) } else { Err(gaps) }
    }
}

/// A `(owner, missing)` pair returned by
/// [`Layer::check_requirements`]: service `owner` declares it needs
/// `missing` in the same router, but `missing` was not mounted.
#[derive(Clone, Copy, Debug)]
pub struct RequirementGap {
    pub owner: &'static str,
    pub missing: &'static str,
}

impl core::fmt::Display for RequirementGap {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "service `{}` requires `{}` in the same router, but it was not mounted",
            self.owner, self.missing
        )
    }
}

impl std::error::Error for RequirementGap {}

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

// ── Requires ──────────────────────────────────────────────────────────────

/// Per-token declaration: "this service expects to find these other
/// services in the same [`LayerRouter`] at dispatch time."
///
/// Requirements live with the **implementation**, not the trait —
/// the REAPER backend's `Fx` impl may dispatch through `Markers` and
/// `Items`, while a mock `Fx` impl returns canned data and needs
/// nothing. So the `#[architect::rpc]` derive emits a default-empty
/// `Requires` impl per service token, and each backend's
/// `Services::layers()` body declares its own edges using
/// [`Needs::needs`]:
///
/// ```ignore
/// impl Services for Reaper {
///     fn layers() -> impl Layer + ProvideAll<Self> + Descriptors + RequiresAll {
///         layers![
///             fx::Service.needs(&[markers::descriptor(), items::descriptor()]),
///             markers::Service,
///             items::Service,
///             // ...
///         ]
///     }
/// }
///
/// impl Services for MockReaper {
///     fn layers() -> impl Layer + ProvideAll<Self> + Descriptors + RequiresAll {
///         // No .needs() — the mock doesn't dispatch cross-service.
///         layers![fx::Service, markers::Service, items::Service]
///     }
/// }
/// ```
///
/// Checked at `.provide()` time — fails fast with a clear panic
/// naming which service needs which. [`Mounted`] reports no
/// requirements: pre-bound bolt-ons are considered self-contained.
pub trait Requires {
    fn requires(&self) -> &'static [&'static ServiceDescriptor];
}

impl Requires for Mounted {
    fn requires(&self) -> &'static [&'static ServiceDescriptor] {
        &[]
    }
}

/// Walks the chain collecting per-service requirements. Sibling of
/// [`Descriptors`] — used by [`Layer::check_requirements`] /
/// [`Layer::provide`] to assert the bundle is dependency-closed.
pub trait RequiresAll: Layer {
    fn collect(&self, out: &mut Vec<(&'static ServiceDescriptor, &'static ServiceDescriptor)>);
}

impl RequiresAll for Empty {
    fn collect(&self, _: &mut Vec<(&'static ServiceDescriptor, &'static ServiceDescriptor)>) {}
}

impl<S, R> RequiresAll for Cons<S, R>
where
    S: BindAny + Requires,
    R: RequiresAll,
{
    fn collect(&self, out: &mut Vec<(&'static ServiceDescriptor, &'static ServiceDescriptor)>) {
        let owner = self.svc.descriptor();
        for req in self.svc.requires() {
            out.push((owner, *req));
        }
        self.rest.collect(out);
    }
}

impl RequiresAll for Mounted {
    fn collect(&self, _: &mut Vec<(&'static ServiceDescriptor, &'static ServiceDescriptor)>) {}
}

// ── WithRequires + Needs builder ──────────────────────────────────────────

/// A service token wrapped with a per-backend requirement list.
/// Built via [`Needs::needs`]; behaves identically to the inner token
/// for descriptor lookup and binding, but reports its declared
/// requirements to [`RequiresAll`].
#[derive(Clone, Copy)]
pub struct WithRequires<S> {
    svc: S,
    requires: &'static [&'static ServiceDescriptor],
}

impl<S: core::fmt::Debug> core::fmt::Debug for WithRequires<S> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("WithRequires")
            .field("svc", &self.svc)
            .field("requires", &self.requires.len())
            .finish()
    }
}

impl<S: BindAny> BindAny for WithRequires<S> {
    fn descriptor(&self) -> &'static ServiceDescriptor {
        self.svc.descriptor()
    }
}

impl<S, B> Bind<B> for WithRequires<S>
where
    S: Bind<B>,
{
    fn bind(self, backend: B) -> Mounted {
        self.svc.bind(backend)
    }
}

impl<S> Requires for WithRequires<S> {
    fn requires(&self) -> &'static [&'static ServiceDescriptor] {
        self.requires
    }
}

impl<S: BindAny> Layer for WithRequires<S> {}

impl<S, R> Append<R> for WithRequires<S>
where
    S: BindAny,
    R: Layer,
{
    type Output = Cons<WithRequires<S>, R>;
    fn append(self, rhs: R) -> Self::Output {
        Cons::new(self, rhs)
    }
}

impl<S: BindAny> Descriptors for WithRequires<S> {
    fn collect(&self, out: &mut Vec<&'static ServiceDescriptor>) {
        out.push(self.svc.descriptor());
    }
}

impl<S, B> ProvideAll<B> for WithRequires<S>
where
    B: Clone,
    S: Bind<B>,
{
    fn provide_into(self, backend: &B, router: &mut LayerRouter) {
        router.add_mounted(self.svc.bind(backend.clone()));
    }
}

impl<S: BindAny> RequiresAll for WithRequires<S> {
    fn collect(&self, out: &mut Vec<(&'static ServiceDescriptor, &'static ServiceDescriptor)>) {
        let owner = self.svc.descriptor();
        for req in self.requires {
            out.push((owner, *req));
        }
    }
}

/// Builder method for attaching a per-backend requirement list to a
/// service token. Blanket-implemented for every [`BindAny`] type, so
/// any service token (or pre-mounted bolt-on) can declare its
/// dependencies inline at the bundle site.
///
/// ```ignore
/// use architect::Needs;
///
/// layers![
///     fx::Service.needs(&[markers::descriptor(), items::descriptor()]),
///     markers::Service,
///     items::Service,
/// ]
/// ```
pub trait Needs: BindAny + Sized {
    /// Wrap this token with the given dependency list. Each entry
    /// must be a `&'static ServiceDescriptor` — typically obtained
    /// via the sibling service's `descriptor()` re-export.
    fn needs(self, requires: &'static [&'static ServiceDescriptor]) -> WithRequires<Self> {
        WithRequires {
            svc: self,
            requires,
        }
    }
}

impl<T: BindAny> Needs for T {}

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
        // walker traits (`Descriptors`, `RequiresAll`) bottom out on
        // the `Empty` base impl rather than on the last service token.
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
    /// type carrying enough trait bounds to:
    ///
    /// - chain `.merge(...)` for bolt-ons/overrides ([`Layer`])
    /// - bind via `.provide(self)` ([`ProvideAll<Self>`])
    /// - introspect with `.descriptors()` before binding
    ///   ([`Descriptors`])
    fn layers() -> impl Layer + ProvideAll<Self> + Descriptors + RequiresAll;

    /// Convenience: build the bundle, bind `self`, return the
    /// terminal router. One-call mount when no overrides are needed.
    fn into_router(self) -> LayerRouter
    where
        Self: Clone + Send + Sync + 'static,
    {
        Self::layers().provide(self)
    }
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
