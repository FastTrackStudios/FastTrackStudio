//! `architect-rpc-derive` — the `#[architect::rpc]` proc-macro.
//!
//! Given a trait declaration, emit a complete RPC face — async client,
//! server-side host, hidden async mirror — so the **same trait** is
//! callable in-process (sync where natural) and across the network
//! (async, vox-served).
//!
//! The macro inspects each method, classifies it sync vs. async, and
//! adapts what it emits to the trait's natural shape:
//!
//! - **All-sync**  → bridge marshals every call onto a `Dispatcher`,
//!   emits an async mirror used by the vox client/host.
//! - **All-async** → trait already its own RPC face. Decorate with
//!   `#[vox::service]` directly; `<T>Host` is a thin newtype.
//! - **Mixed**    → sync methods bridged, async methods pass through.
//!
//! See `architect/DESIGN.md` for the full design.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{
    FnArg, ItemTrait, Pat, PatIdent, ReturnType, TraitItem, TraitItemFn, Type, parse_macro_input,
    parse_quote,
};

/// `#[architect::rpc]` — annotate a trait to derive its RPC face.
///
/// Usage:
///
/// ```ignore
/// #[architect::rpc]
/// pub trait Tracks {
///     fn all(&self) -> Vec<Track>;
///     fn set_muted(&self, guid: &str, muted: bool) -> Result<(), DawError>;
/// }
/// ```
///
/// emits, in addition to the input trait:
///
/// - `TracksRpc` — hidden async mirror, vox::service-decorated. The
///   trait `#[vox::service]` would generate clients and dispatchers
///   against; user code touches it only by name when mounting.
/// - `TracksHost<S, D>` — server-side wrapper accepting any `impl
///   Tracks` plus a `Dispatcher`, implementing `TracksRpc` by
///   marshaling each sync method through the dispatcher.
/// - `TracksClient` — type alias for the vox-emitted `TracksRpcClient`,
///   so callers see a clean name.
///
/// # Streams: `#[subscribe]`
///
/// A method marked `#[subscribe]` is a stream declaration, not a
/// callable method — it names an event type and is stripped from the
/// trait:
///
/// ```ignore
/// #[architect::rpc]
/// pub trait Tracks {
///     fn all(&self) -> Vec<Track>;
///     /// Every track-set change, as it happens.
///     #[subscribe]
///     fn events(&self) -> TrackEvent;
/// }
/// ```
///
/// emits (vox-gated) the `TracksStream` sibling service
/// (`async fn events(&self, sink: Tx<TrackEvent>)`), the
/// `TracksStreamSource` backend contract (`fn events_hub(&self) ->
/// &PubSub<TrackEvent>` — the backend owns and publishes into the
/// hub), and `stream_serve` / `stream_layer` / `StreamService` mount
/// verbs. Declarations with filter params (`#[subscribe] fn events(&self,
/// filter: F) -> E`) ask the backend for `fn events_attach(&self,
/// filter: F, sink: Tx<E>)` instead, so filtering and
/// snapshot-then-changes (`PubSub::begin_attach`) stay backend-owned.
#[proc_macro_attribute]
pub fn rpc(args: TokenStream, input: TokenStream) -> TokenStream {
    if !args.is_empty() {
        let span = proc_macro2::TokenStream::from(args);
        return syn::Error::new_spanned(
            span,
            "#[architect::rpc] takes no arguments — requirements are \
             declared per backend at the bundle site via \
             `Service.needs(&[...])`, not on the trait.",
        )
        .to_compile_error()
        .into();
    }

    let trait_item = parse_macro_input!(input as ItemTrait);

    match expand(trait_item) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// `#[derive(HasDispatcher)]` — emit the boilerplate `HasDispatcher`
/// impl for a backend whose dispatcher is default-constructible.
///
/// Without arguments the impl points at
/// `::architect::dispatch::DefaultDispatcher` — the build's default
/// (`TokioBlockingDispatcher` when architect's `dispatch-tokio`
/// feature is on, `CurrentThreadDispatcher` otherwise):
///
/// ```ignore
/// #[derive(architect::HasDispatcher)]
/// pub struct TaskBackend { /* … */ }
/// ```
///
/// An explicit `#[dispatch(SomeDispatcher)]` attribute overrides the
/// type (any path is accepted; it must implement
/// `architect::dispatch::Dispatcher + Default`):
///
/// ```ignore
/// #[derive(architect::HasDispatcher)]
/// #[dispatch(architect::dispatch::CurrentThreadDispatcher)]
/// pub struct TestBackend { /* … */ }
/// ```
///
/// Backends whose dispatcher needs runtime state (a REAPER main-thread
/// queue handle, etc.) keep writing the manual impl — the derive only
/// covers the `Default`-constructible case, which is the overwhelming
/// majority in server binaries.
#[proc_macro_derive(HasDispatcher, attributes(dispatch))]
pub fn derive_has_dispatcher(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    match expand_has_dispatcher(input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn expand_has_dispatcher(input: syn::DeriveInput) -> syn::Result<TokenStream2> {
    let mut dispatcher_ty: Option<Type> = None;
    for attr in &input.attrs {
        if attr.path().is_ident("dispatch") {
            if dispatcher_ty.is_some() {
                return Err(syn::Error::new_spanned(
                    attr,
                    "duplicate #[dispatch(...)] attribute — a backend has exactly one dispatcher",
                ));
            }
            dispatcher_ty = Some(attr.parse_args::<Type>()?);
        }
    }
    let dispatcher_ty: Type =
        dispatcher_ty.unwrap_or_else(|| parse_quote! { ::architect::dispatch::DefaultDispatcher });

    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    Ok(quote! {
        impl #impl_generics ::architect::HasDispatcher for #name #ty_generics #where_clause {
            type Dispatcher = #dispatcher_ty;

            fn dispatcher(&self) -> Self::Dispatcher {
                <#dispatcher_ty as ::core::default::Default>::default()
            }
        }
    })
}

/// Main expansion entry. Split out from the proc-macro shim so it can
/// be exercised from unit tests once they exist.
fn expand(trait_item: ItemTrait) -> syn::Result<TokenStream2> {
    let trait_name = &trait_item.ident;
    let vis = &trait_item.vis;
    let rpc_trait_name = format_ident!("{}Rpc", trait_name);
    // Bridge struct is hidden — users only see `serve`. The name is
    // doc(hidden) and underscored so it doesn't show up in completions.
    let host_name = format_ident!("__{}Bridge", trait_name);
    let client_name = format_ident!("{}Client", trait_name);
    let rpc_client_name = format_ident!("{}RpcClient", trait_name);
    let rpc_dispatcher_name = format_ident!("{}RpcDispatcher", trait_name);

    // Classify + validate each method. `#[subscribe]` declarations are
    // split off — they're stream declarations, not callable methods.
    let mut methods = Vec::new();
    let mut subscriptions = Vec::new();
    for item in &trait_item.items {
        if let TraitItem::Fn(method) = item {
            if has_subscribe_attr(method) {
                subscriptions.push(classify_subscription(method)?);
            } else {
                methods.push(classify_method(method)?);
            }
        }
    }

    let shape = classify_shape(&methods);

    // Always emit the user's trait — adding the `Send + Sync + 'static`
    // bound so it's safe to share through `Arc<dyn Trait>` from inside
    // the bridge. We don't mutate the input AST; we re-emit with the
    // augmented supertraits. `#[subscribe]` declarations are dropped
    // here — they materialize as the stream sibling, not as methods.
    let user_trait = emit_user_trait(&trait_item, shape);

    let (mirror_trait, host_struct, host_impl) = match shape {
        Shape::Empty | Shape::AllAsync => {
            // The trait is already its own RPC face. We still emit a
            // mirror alias + host so consumer wiring is uniform across
            // shapes — `<T>Host::new(impl T)` mounts the same way the
            // sync bridge does.
            (
                quote! {},
                emit_passthrough_host(trait_name, &host_name, vis),
                emit_passthrough_impl(trait_name, &host_name, &methods),
            )
        }
        Shape::AllSync | Shape::Mixed => (
            emit_mirror_trait(&rpc_trait_name, vis, &methods),
            emit_bridge_host(trait_name, &host_name, vis),
            emit_bridge_impl(
                trait_name,
                &host_name,
                &rpc_trait_name,
                &methods,
                matches!(shape, Shape::AllSync),
            ),
        ),
    };

    // Client alias — only emitted when the consumer crate enables its
    // `vox` feature, since the underlying `<T>RpcClient` type is
    // generated by `#[vox::service]` and otherwise doesn't exist. For
    // AllAsync the vox-generated client is named off the user trait
    // directly, so no alias is needed.
    let client_alias = match shape {
        Shape::Empty => quote! {},
        Shape::AllAsync => {
            // vox::service is applied to the user trait directly, so it
            // emits `<Trait>Client`, `<Trait>Dispatcher`, and
            // `<snake_name>_service_descriptor`. Architect downstream
            // (serve / layer) refers to the Rpc-suffixed names, so
            // alias them through.
            let vox_dispatcher = format_ident!("{}Dispatcher", trait_name);
            let vox_descriptor = format_ident!(
                "{}_service_descriptor",
                to_snake_case(&trait_name.to_string())
            );
            let rpc_descriptor = format_ident!(
                "{}_rpc_service_descriptor",
                to_snake_case(&trait_name.to_string())
            );
            quote! {
                #[cfg(feature = "vox")]
                #vis use #vox_dispatcher as #rpc_dispatcher_name;
                #[cfg(feature = "vox")]
                #vis use #vox_descriptor as #rpc_descriptor;
            }
        }
        Shape::AllSync | Shape::Mixed => quote! {
            /// Async caller proxy — type alias for the vox-emitted
            /// `<Trait>RpcClient` generated from the hidden mirror.
            #[cfg(feature = "vox")]
            #vis type #client_name = #rpc_client_name;
        },
    };

    // `serve` — the public mount verb. Users write
    // `router.mount(marker::serve(backend, dispatcher))` and never see
    // the hidden bridge struct or the `<T>RpcDispatcher` plumbing.
    let serve_fn = emit_serve_fn(trait_name, &host_name, &rpc_dispatcher_name, vis, shape);

    // `layer` — bundles (descriptor, serve(backend)) into an
    // `architect::Mounted` so callers can compose mounting via
    // `Layer::merge` instead of writing per-service `.with(...)` calls.
    let layer_fn = emit_layer_fn(trait_name, vis, shape);

    // `prelude` — crate-root-ready re-exports with the trait name baked
    // in, so proto crates glob one module per service instead of
    // hand-renaming five items.
    let prelude = emit_prelude(
        trait_name,
        &client_name,
        &rpc_trait_name,
        &rpc_dispatcher_name,
        vis,
        shape,
        &subscriptions,
    );

    // Stream sibling — emitted when the trait declares `#[subscribe]`
    // methods: the `<Trait>Stream` vox service, the `<Trait>StreamSource`
    // backend contract, and the stream mount verbs.
    let stream_block = emit_stream_block(trait_name, vis, &subscriptions);

    Ok(quote! {
        #user_trait
        #mirror_trait
        #host_struct
        #host_impl
        #client_alias
        #serve_fn
        #layer_fn
        #stream_block
        #prelude
    })
}

/// Emit the `prelude` module: every public item this expansion
/// produces, re-exported under names that stay unique after a glob —
/// the module-scoped `Service` / `layer` / `serve` get the trait name
/// baked in (`TracksService`, `tracks_layer`, `tracks_serve`). A proto
/// crate's root then collapses its per-service re-export block to:
///
/// ```ignore
/// pub use service::{bookings::prelude::*, slots::prelude::*};
/// ```
///
/// The vox-only items carry the same `#[cfg(feature = "vox")]` gates
/// as their definitions, so the glob works in non-vox builds too (it
/// just re-exports the bare trait).
fn emit_prelude(
    trait_name: &syn::Ident,
    client_name: &syn::Ident,
    rpc_trait_name: &syn::Ident,
    rpc_dispatcher_name: &syn::Ident,
    vis: &syn::Visibility,
    shape: Shape,
    subs: &[Subscription],
) -> TokenStream2 {
    let snake = to_snake_case(&trait_name.to_string());
    let service_alias = format_ident!("{}Service", trait_name);
    let layer_alias = format_ident!("{snake}_layer");
    let serve_alias = format_ident!("{snake}_serve");
    let descriptor_fn = format_ident!("{snake}_rpc_service_descriptor");

    // Stream-sibling re-exports — names are already trait-prefixed
    // except the module-scoped verbs/token, which get renamed here.
    let stream_items = if subs.is_empty() {
        quote! {}
    } else {
        let source_trait = format_ident!("{}StreamSource", trait_name);
        let stream_trait = format_ident!("{}Stream", trait_name);
        let stream_client = format_ident!("{}StreamClient", trait_name);
        let stream_dispatcher = format_ident!("{}StreamDispatcher", trait_name);
        let stream_service_alias = format_ident!("{}StreamService", trait_name);
        let stream_layer_alias = format_ident!("{snake}_stream_layer");
        let stream_serve_alias = format_ident!("{snake}_stream_serve");
        let stream_descriptor_fn = format_ident!("{snake}_stream_service_descriptor");
        quote! {
            #[cfg(feature = "vox")]
            pub use super::{
                #source_trait, #stream_trait, #stream_client, #stream_dispatcher,
                #stream_descriptor_fn,
                StreamService as #stream_service_alias,
                stream_layer as #stream_layer_alias,
                stream_serve as #stream_serve_alias,
            };
        }
    };

    let vox_items = match shape {
        Shape::Empty => quote! {},
        // AllAsync: vox::service decorates the user trait directly, so
        // the client is already `<Trait>Client` and the mirror trait
        // doesn't exist. The dispatcher/descriptor go through the
        // Rpc-suffixed aliases emitted alongside.
        Shape::AllAsync => quote! {
            #[cfg(feature = "vox")]
            pub use super::{
                #client_name, #rpc_dispatcher_name, #descriptor_fn,
                Service as #service_alias,
                layer as #layer_alias,
                serve as #serve_alias,
            };
        },
        Shape::AllSync | Shape::Mixed => quote! {
            #[cfg(feature = "vox")]
            pub use super::{
                #client_name, #rpc_trait_name, #rpc_dispatcher_name, #descriptor_fn,
                Service as #service_alias,
                layer as #layer_alias,
                serve as #serve_alias,
            };
        },
    };

    quote! {
        /// Glob-safe re-exports for this service: the trait itself plus
        /// (with the `vox` feature) the client, dispatcher, descriptor,
        /// and the `Service` token / `layer` / `serve` verbs renamed
        /// with the trait's name baked in. Intended for proto-crate
        /// roots: `pub use my_service::prelude::*;`.
        #vis mod prelude {
            pub use super::#trait_name;
            #vox_items
            #stream_items
        }
    }
}

/// Snake-case an UpperCamelCase identifier. Mirrors what `#[vox::service]`
/// does when it derives `<snake_name>_service_descriptor` from the
/// trait's name, so we can refer to that emitted function by name from
/// inside the `layer()` body.
fn to_snake_case(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 4);
    let mut prev_lower = false;
    for ch in input.chars() {
        if ch.is_uppercase() {
            if prev_lower {
                out.push('_');
            }
            for lc in ch.to_lowercase() {
                out.push(lc);
            }
            prev_lower = false;
        } else {
            out.push(ch);
            prev_lower = ch.is_alphanumeric();
        }
    }
    out
}

/// Emit:
///
/// - `pub fn layer(backend) -> architect::Mounted` — immediate-bind
///   shortcut. Useful when a caller already has the backend and just
///   wants a bolt-on `Mounted` to drop into someone else's `Layer`.
/// - `pub struct Service;` + `impl architect::Bind<B> for Service` —
///   the deferred-bind token. Architectures compose these into
///   `architect::Layer<B>` and bind once at `.provide(B)` time.
fn emit_layer_fn(trait_name: &syn::Ident, vis: &syn::Visibility, shape: Shape) -> TokenStream2 {
    let descriptor_fn = format_ident!(
        "{}_rpc_service_descriptor",
        to_snake_case(&trait_name.to_string())
    );

    let (bounds, immediate_doc) = match shape {
        Shape::Empty => return quote! {},
        Shape::AllAsync => (
            quote! {
                S: #trait_name + ::core::marker::Send + ::core::marker::Sync + 'static,
            },
            "Async-native trait — no `HasDispatcher` requirement.",
        ),
        Shape::AllSync | Shape::Mixed => (
            quote! {
                S: #trait_name
                    + ::architect::HasDispatcher
                    + ::core::marker::Send
                    + ::core::marker::Sync
                    + 'static,
            },
            "Sync/mixed trait — backend must implement `HasDispatcher`.",
        ),
    };

    quote! {
        /// Immediate-bind shortcut: wrap a backend in this service's
        /// vox dispatcher and return a `Mounted`. For deferred
        /// binding, prefer `Service` + [`architect::Layer::merge`] +
        /// [`architect::Layer::provide`].
        ///
        #[doc = #immediate_doc]
        #[cfg(feature = "vox")]
        #vis fn layer<S>(backend: S) -> ::architect::Mounted
        where
            #bounds
        {
            ::architect::Mounted::new(#descriptor_fn(), serve(backend))
        }

        /// Deferred-bind service token. Acts as a one-element
        /// [`architect::Layer`]; compose with [`architect::Layer::merge`]
        /// and bind a backend at [`architect::Layer::provide`] time.
        #[cfg(feature = "vox")]
        #[derive(Debug, Default, Clone, Copy)]
        #vis struct Service;

        #[cfg(feature = "vox")]
        impl ::architect::BindAny for Service {
            fn descriptor(&self) -> &'static ::architect::vox::ServiceDescriptor {
                #descriptor_fn()
            }
        }

        #[cfg(feature = "vox")]
        impl<S> ::architect::Bind<S> for Service
        where
            S: ::core::clone::Clone,
            #bounds
        {
            fn bind_into(
                self,
                backend: &S,
                router: &mut ::architect::LayerRouter,
            ) {
                use ::architect::LayerSink as _;
                router.add_mounted(::architect::Mounted::new(
                    #descriptor_fn(),
                    serve(backend.clone()),
                ));
            }
        }

        // ─ Composition impls ───────────────────────────────────────
        // `Layer<B>` itself is blanket-implemented by architect for
        // any `T: Bind<B> + Descriptors + Sized`, so the Service
        // token gets it for free once we emit Bind / Descriptors.
        // We still need `Append<R>` (the structural cons-concat)
        // emitted per-token so `layers![a, b]` builds a chain.

        #[cfg(feature = "vox")]
        impl<R> ::architect::Append<R> for Service {
            type Output = ::architect::Cons<Service, R>;
            fn append(self, rhs: R) -> Self::Output {
                ::architect::Cons::new(self, rhs)
            }
        }

        #[cfg(feature = "vox")]
        impl ::architect::Descriptors for Service {
            fn collect(
                &self,
                out: &mut ::std::vec::Vec<&'static ::architect::vox::ServiceDescriptor>,
            ) {
                out.push(::architect::BindAny::descriptor(self));
            }
        }

    }
}

/// Emit the `serve` free function — the public mount verb.
///
/// Sync / mixed: takes a backend + dispatcher, builds the hidden bridge,
/// wraps it in the vox-emitted dispatcher, returns the mountable.
///
/// All-async: takes just a backend (the trait is its own RPC face).
fn emit_serve_fn(
    trait_name: &syn::Ident,
    host_name: &syn::Ident,
    rpc_dispatcher_name: &syn::Ident,
    vis: &syn::Visibility,
    shape: Shape,
) -> TokenStream2 {
    match shape {
        Shape::Empty => quote! {},
        Shape::AllAsync => quote! {
            /// Wrap a backend in the vox-emitted dispatcher so it can be
            /// mounted on a vox router. The trait is async-native, so no
            /// thread dispatcher is required.
            #[cfg(feature = "vox")]
            #vis fn serve<S>(backend: S) -> #rpc_dispatcher_name<#host_name<S>>
            where
                S: #trait_name + ::core::marker::Send + ::core::marker::Sync + 'static,
            {
                #rpc_dispatcher_name::new(#host_name::new(backend))
            }
        },
        Shape::AllSync | Shape::Mixed => quote! {
            /// Wrap a backend in the vox-emitted dispatcher so it can
            /// be mounted on a vox router. Each call to a sync trait
            /// method is marshaled through the backend's dispatcher
            /// (pulled via `HasDispatcher`) to the thread where the
            /// backend's work runs.
            #[cfg(feature = "vox")]
            #vis fn serve<S>(
                backend: S,
            ) -> #rpc_dispatcher_name<#host_name<S, <S as ::architect::HasDispatcher>::Dispatcher>>
            where
                S: #trait_name
                    + ::architect::HasDispatcher
                    + ::core::marker::Send
                    + ::core::marker::Sync
                    + 'static,
            {
                let dispatcher = ::architect::HasDispatcher::dispatcher(&backend);
                #rpc_dispatcher_name::new(#host_name::new(backend, dispatcher))
            }
        },
    }
}

// ── Subscription classification + emission ─────────────────────────────

/// One `#[subscribe]` declaration: `fn name(&self, filters..) -> EventTy`.
#[derive(Debug)]
struct Subscription {
    name: syn::Ident,
    event_ty: Type,
    /// Filter params in owned form (what crosses the wire), excluding
    /// the receiver.
    mirror_inputs: Vec<FnArg>,
    arg_idents: Vec<syn::Ident>,
    arg_was_ref: Vec<bool>,
    docs: Vec<syn::Attribute>,
}

fn has_subscribe_attr(method: &TraitItemFn) -> bool {
    method.attrs.iter().any(|a| a.path().is_ident("subscribe"))
}

fn classify_subscription(method: &TraitItemFn) -> syn::Result<Subscription> {
    if method.sig.asyncness.is_some() {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "#[subscribe] declarations are sync markers — write \
             `fn events(&self) -> EventType;`, not `async fn`. The async \
             subscribe RPC is emitted on the `<Trait>Stream` sibling.",
        ));
    }
    let event_ty = match &method.sig.output {
        ReturnType::Type(_, ty) => (**ty).clone(),
        ReturnType::Default => {
            return Err(syn::Error::new_spanned(
                &method.sig,
                "#[subscribe] declarations name their event type as the \
                 return type: `fn events(&self) -> EventType;`.",
            ));
        }
    };
    match method.sig.inputs.first() {
        Some(FnArg::Receiver(rec)) if rec.reference.is_some() && rec.mutability.is_none() => {}
        _ => {
            return Err(syn::Error::new_spanned(
                &method.sig,
                "#[subscribe] declarations must take `&self` as the first \
                 parameter.",
            ));
        }
    }

    let mut mirror_inputs = Vec::new();
    let mut arg_idents = Vec::new();
    let mut arg_was_ref = Vec::new();
    for (i, input) in method.sig.inputs.iter().enumerate() {
        if let FnArg::Typed(pat_ty) = input {
            let ident = match &*pat_ty.pat {
                Pat::Ident(PatIdent { ident, .. }) => ident.clone(),
                _ => format_ident!("__arg{i}"),
            };
            let (owned_ty, was_ref) = owned_form(&pat_ty.ty);
            let mut mirror_pat = pat_ty.clone();
            mirror_pat.ty = Box::new(owned_ty);
            mirror_pat.pat = Box::new(parse_quote! { #ident });
            mirror_inputs.push(FnArg::Typed(mirror_pat));
            arg_idents.push(ident);
            arg_was_ref.push(was_ref);
        }
    }

    let docs = method
        .attrs
        .iter()
        .filter(|a| a.path().is_ident("doc"))
        .cloned()
        .collect();

    Ok(Subscription {
        name: method.sig.ident.clone(),
        event_ty,
        mirror_inputs,
        arg_idents,
        arg_was_ref,
        docs,
    })
}

/// Emit the stream sibling for a trait with `#[subscribe]` declarations.
///
/// For `#[architect::rpc] trait Fx { #[subscribe] fn events(&self) -> FxEvent; }`:
///
/// - `FxStreamSource` — the backend contract. Parameterless
///   subscriptions ask for a hub accessor (`fn events_hub(&self) ->
///   &PubSub<FxEvent>`); the bridge attaches sinks to it, and the
///   backend publishes into it. Subscriptions **with** filter params
///   ask for an attach method (`fn events_attach(&self, args.., sink)`)
///   so the backend owns the filtering.
/// - `FxStream` — the `#[vox::service]` sibling: one
///   `async fn events(&self, args.., sink: Tx<FxEvent>)` per
///   declaration. Vox emits `FxStreamClient` / `FxStreamDispatcher` /
///   `fx_stream_service_descriptor` from it.
/// - `__FxStreamHost<S>` — hidden adapter implementing `FxStream` over
///   any `S: FxStreamSource`.
/// - `stream_serve` / `stream_layer` / `StreamService` — mount verbs,
///   parallel to the base trait's `serve` / `layer` / `Service`.
///
/// Snapshot-then-changes: construct the hub with
/// `PubSub::sliding(n).with_replay(1)` and publish full-state events —
/// late subscribers get the last event on attach. Subscriptions that
/// need a real snapshot read (the Entity-events pattern) use a filter
/// param + `begin_attach`/`complete_attach` inside their `_attach` impl.
fn emit_stream_block(
    trait_name: &syn::Ident,
    vis: &syn::Visibility,
    subs: &[Subscription],
) -> TokenStream2 {
    if subs.is_empty() {
        return quote! {};
    }

    let snake = to_snake_case(&trait_name.to_string());
    let source_trait = format_ident!("{}StreamSource", trait_name);
    let stream_trait = format_ident!("{}Stream", trait_name);
    let stream_host = format_ident!("__{}StreamHost", trait_name);
    let stream_dispatcher = format_ident!("{}StreamDispatcher", trait_name);
    let stream_descriptor_fn = format_ident!("{snake}_stream_service_descriptor");

    // Backend contract methods.
    let source_methods = subs.iter().map(|s| {
        let ev = &s.event_ty;
        let docs = &s.docs;
        if s.arg_idents.is_empty() {
            let hub_fn = format_ident!("{}_hub", s.name);
            let doc = format!(
                "Fan-out hub backing the `{}` subscription. Construct it \
                 once on the backend (e.g. `PubSub::sliding(256)`, add \
                 `.with_replay(1)` for state-shaped events) and publish \
                 into it; the stream host attaches every subscriber sink.",
                s.name
            );
            quote! {
                #(#docs)*
                #[doc = #doc]
                fn #hub_fn(&self) -> &::architect::PubSub<#ev>;
            }
        } else {
            let attach_fn = format_ident!("{}_attach", s.name);
            let inputs = &s.mirror_inputs;
            let doc = format!(
                "Attach `sink` to the filtered `{}` feed. The backend owns \
                 the filter semantics — typically a per-filter `PubSub`, or \
                 `begin_attach`/`complete_attach` for snapshot-then-changes.",
                s.name
            );
            quote! {
                #(#docs)*
                #[doc = #doc]
                fn #attach_fn(&self, #(#inputs,)* sink: ::architect::vox::Tx<#ev>);
            }
        }
    });

    // Sibling vox-service methods.
    let stream_methods = subs.iter().map(|s| {
        let name = &s.name;
        let ev = &s.event_ty;
        let inputs = &s.mirror_inputs;
        let docs = &s.docs;
        quote! {
            #(#docs)*
            async fn #name(&self, #(#inputs,)* sink: ::architect::vox::Tx<#ev>);
        }
    });

    // Host adapter bodies.
    let host_methods = subs.iter().map(|s| {
        let name = &s.name;
        let ev = &s.event_ty;
        let inputs = &s.mirror_inputs;
        if s.arg_idents.is_empty() {
            let hub_fn = format_ident!("{}_hub", s.name);
            quote! {
                async fn #name(&self, sink: ::architect::vox::Tx<#ev>) {
                    self.inner.#hub_fn().attach(sink);
                }
            }
        } else {
            let attach_fn = format_ident!("{}_attach", s.name);
            let call_args = s
                .arg_idents
                .iter()
                .zip(s.arg_was_ref.iter())
                .map(|(id, &was_ref)| {
                    if was_ref {
                        quote! { &#id }
                    } else {
                        quote! { #id }
                    }
                });
            quote! {
                async fn #name(&self, #(#inputs,)* sink: ::architect::vox::Tx<#ev>) {
                    self.inner.#attach_fn(#(#call_args,)* sink);
                }
            }
        }
    });

    let source_doc = format!(
        "Backend contract for [`{trait_name}`]'s `#[subscribe]` streams. \
         Implement on the backend alongside `{trait_name}`; mount with \
         `stream_layer` / `StreamService` next to the base service."
    );
    let stream_doc = format!(
        "Streaming sibling of [`{trait_name}`]: pass a channel sink in, \
         receive events on it until the connection closes. Clients use \
         `{trait_name}StreamClient` (see `architect::use_stream`)."
    );

    quote! {
        #[doc = #source_doc]
        #[cfg(feature = "vox")]
        #vis trait #source_trait {
            #(#source_methods)*
        }

        #[doc = #stream_doc]
        #[cfg(feature = "vox")]
        #[::architect::vox::service]
        #vis trait #stream_trait {
            #(#stream_methods)*
        }

        /// Internal adapter: implements the stream sibling over any
        /// backend that fulfills the `StreamSource` contract. Hidden —
        /// mount via `stream_serve` / `stream_layer`.
        #[doc(hidden)]
        #[cfg(feature = "vox")]
        #vis struct #stream_host<S>
        where
            S: #source_trait + ::core::marker::Send + ::core::marker::Sync + 'static,
        {
            inner: ::std::sync::Arc<S>,
        }

        #[cfg(feature = "vox")]
        impl<S> #stream_host<S>
        where
            S: #source_trait + ::core::marker::Send + ::core::marker::Sync + 'static,
        {
            pub fn new(inner: S) -> Self {
                Self { inner: ::std::sync::Arc::new(inner) }
            }
        }

        #[cfg(feature = "vox")]
        impl<S> ::core::clone::Clone for #stream_host<S>
        where
            S: #source_trait + ::core::marker::Send + ::core::marker::Sync + 'static,
        {
            fn clone(&self) -> Self {
                Self { inner: ::std::sync::Arc::clone(&self.inner) }
            }
        }

        #[cfg(feature = "vox")]
        impl<S> #stream_trait for #stream_host<S>
        where
            S: #source_trait + ::core::marker::Send + ::core::marker::Sync + 'static,
        {
            #(#host_methods)*
        }

        /// Wrap a backend's stream source in the vox-emitted dispatcher
        /// so the subscription service can be mounted on a vox router.
        #[cfg(feature = "vox")]
        #vis fn stream_serve<S>(backend: S) -> #stream_dispatcher<#stream_host<S>>
        where
            S: #source_trait + ::core::marker::Send + ::core::marker::Sync + 'static,
        {
            #stream_dispatcher::new(#stream_host::new(backend))
        }

        /// Immediate-bind shortcut for the stream sibling — parallel to
        /// `layer` for the base service.
        #[cfg(feature = "vox")]
        #vis fn stream_layer<S>(backend: S) -> ::architect::Mounted
        where
            S: #source_trait + ::core::marker::Send + ::core::marker::Sync + 'static,
        {
            ::architect::Mounted::new(#stream_descriptor_fn(), stream_serve(backend))
        }

        /// Deferred-bind token for the stream sibling — slots into
        /// `layers![Service, StreamService]` next to the base token.
        #[cfg(feature = "vox")]
        #[derive(Debug, Default, Clone, Copy)]
        #vis struct StreamService;

        #[cfg(feature = "vox")]
        impl ::architect::BindAny for StreamService {
            fn descriptor(&self) -> &'static ::architect::vox::ServiceDescriptor {
                #stream_descriptor_fn()
            }
        }

        #[cfg(feature = "vox")]
        impl<S> ::architect::Bind<S> for StreamService
        where
            S: ::core::clone::Clone
                + #source_trait
                + ::core::marker::Send
                + ::core::marker::Sync
                + 'static,
        {
            fn bind_into(
                self,
                backend: &S,
                router: &mut ::architect::LayerRouter,
            ) {
                use ::architect::LayerSink as _;
                router.add_mounted(::architect::Mounted::new(
                    #stream_descriptor_fn(),
                    stream_serve(backend.clone()),
                ));
            }
        }

        #[cfg(feature = "vox")]
        impl<R> ::architect::Append<R> for StreamService {
            type Output = ::architect::Cons<StreamService, R>;
            fn append(self, rhs: R) -> Self::Output {
                ::architect::Cons::new(self, rhs)
            }
        }

        #[cfg(feature = "vox")]
        impl ::architect::Descriptors for StreamService {
            fn collect(
                &self,
                out: &mut ::std::vec::Vec<&'static ::architect::vox::ServiceDescriptor>,
            ) {
                out.push(::architect::BindAny::descriptor(self));
            }
        }
    }
}

// ── Method classification ──────────────────────────────────────────────

#[derive(Debug)]
struct Method {
    decl: TraitItemFn,
    is_async: bool,
    /// Owned (non-reference) parameter list rebuilt for the async
    /// mirror trait. Borrowed args (`&str`, etc.) get rewritten here
    /// to owned types so the bridge's closures can capture them.
    mirror_inputs: Vec<FnArg>,
    /// Argument idents in order — needed when emitting the call from
    /// the bridge body back into the sync trait.
    arg_idents: Vec<syn::Ident>,
    /// Whether each argument was originally a reference (so the
    /// bridge passes `&owned` when calling back into the sync trait).
    arg_was_ref: Vec<bool>,
    return_ty: ReturnType,
}

fn classify_method(method: &TraitItemFn) -> syn::Result<Method> {
    // Reject what the bridge can't currently express.
    if !method.sig.generics.params.is_empty() {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "#[architect::rpc] cannot expose methods with generic \
             type or const parameters — the trait must be object-safe.",
        ));
    }

    if let Some(FnArg::Receiver(rec)) = method.sig.inputs.first() {
        if rec.mutability.is_some() {
            return Err(syn::Error::new_spanned(
                rec,
                "#[architect::rpc] methods must take `&self`, not `&mut self`. \
                 Use interior mutability in the backend if needed.",
            ));
        }
        if rec.reference.is_none() {
            return Err(syn::Error::new_spanned(
                rec,
                "#[architect::rpc] methods must take `&self` — `self` and \
                 `mut self` consume the backend and aren't dyn-callable.",
            ));
        }
    } else {
        return Err(syn::Error::new_spanned(
            &method.sig,
            "#[architect::rpc] methods must take `&self` as the first parameter.",
        ));
    }

    if let ReturnType::Type(_, ty) = &method.sig.output {
        if matches!(&**ty, Type::Reference(_)) {
            return Err(syn::Error::new_spanned(
                ty,
                "#[architect::rpc] methods must return owned values — \
                 borrowed returns cannot cross the bridge or the wire.",
            ));
        }
        if let Type::Path(p) = &**ty
            && p.path.is_ident("Self")
        {
            return Err(syn::Error::new_spanned(
                ty,
                "#[architect::rpc] methods cannot return `Self` — the \
                 trait must be object-safe.",
            ));
        }
    }

    let is_async = method.sig.asyncness.is_some();

    // Walk inputs, build mirror-side inputs (owned) and the per-arg
    // info the bridge body needs.
    let mut mirror_inputs = Vec::new();
    let mut arg_idents = Vec::new();
    let mut arg_was_ref = Vec::new();

    for (i, input) in method.sig.inputs.iter().enumerate() {
        match input {
            FnArg::Receiver(_) => mirror_inputs.push(input.clone()),
            FnArg::Typed(pat_ty) => {
                let ident = match &*pat_ty.pat {
                    Pat::Ident(PatIdent { ident, .. }) => ident.clone(),
                    _ => format_ident!("__arg{i}"),
                };
                let (owned_ty, was_ref) = owned_form(&pat_ty.ty);
                let mut mirror_pat = pat_ty.clone();
                mirror_pat.ty = Box::new(owned_ty);
                mirror_pat.pat = Box::new(parse_quote! { #ident });
                mirror_inputs.push(FnArg::Typed(mirror_pat));
                arg_idents.push(ident);
                arg_was_ref.push(was_ref);
            }
        }
    }

    Ok(Method {
        decl: method.clone(),
        is_async,
        mirror_inputs,
        arg_idents,
        arg_was_ref,
        return_ty: method.sig.output.clone(),
    })
}

/// Convert a parameter type to its owned `'static` form, returning
/// whether a reference was unwrapped (so the bridge knows to pass
/// `&owned` when calling the sync trait).
fn owned_form(ty: &Type) -> (Type, bool) {
    if let Type::Reference(ty_ref) = ty {
        let inner = &*ty_ref.elem;
        // `&str` → `String`
        if let Type::Path(p) = inner
            && p.path.is_ident("str")
        {
            return (parse_quote! { ::std::string::String }, true);
        }
        // `&[T]` → `Vec<T>`
        if let Type::Slice(slice) = inner {
            let elem = &slice.elem;
            return (parse_quote! { ::std::vec::Vec<#elem> }, true);
        }
        // Other `&T` — clone the inner type. Caller must impl Clone.
        return (inner.clone(), true);
    }
    (ty.clone(), false)
}

// ── Trait shape classification ─────────────────────────────────────────

#[derive(Debug, Clone, Copy)]
enum Shape {
    Empty,
    AllSync,
    AllAsync,
    Mixed,
}

fn classify_shape(methods: &[Method]) -> Shape {
    if methods.is_empty() {
        return Shape::Empty;
    }
    let async_count = methods.iter().filter(|m| m.is_async).count();
    let sync_count = methods.len() - async_count;
    match (sync_count, async_count) {
        (0, _) => Shape::AllAsync,
        (_, 0) => Shape::AllSync,
        _ => Shape::Mixed,
    }
}

// ── Emission ───────────────────────────────────────────────────────────

/// Re-emit the user's trait, rewriting any `async fn` to
/// `fn -> impl Future<Output = R> + Send` so backends promise
/// `Send` futures. The bridge calls `self.inner.method(...).await`
/// in a context that requires Send (the mirror's `async fn`
/// expands via vox::service to a Send-bounded future), so the
/// inner trait must guarantee Send too.
///
/// Sync methods are re-emitted verbatim. `Send + Sync + 'static`
/// bounds live on the bridge's where-clauses (not the trait
/// itself) so the trait remains impl-able by borrowed-view types.
fn emit_user_trait(trait_item: &ItemTrait, shape: Shape) -> TokenStream2 {
    // AllAsync: vox::service is applied to the user trait directly
    // (no hidden mirror), so leave `async fn` intact — vox's parser
    // rewrites it to `fn -> impl Future + MaybeSend` itself. Sync/
    // Mixed shapes go through the architect bridge, where we rewrite
    // async signatures to `fn -> impl Future + Send` so the bridge's
    // `.await` on the inner method satisfies the mirror's Send bound.
    let apply_vox = matches!(shape, Shape::AllAsync);
    let mut out = trait_item.clone();
    // Drop `#[subscribe]` declarations — they exist only to drive the
    // stream-sibling emission, never as callable trait methods.
    out.items.retain(|item| match item {
        TraitItem::Fn(f) => !has_subscribe_attr(f),
        _ => true,
    });
    if !apply_vox {
        for item in &mut out.items {
            if let syn::TraitItem::Fn(f) = item
                && f.sig.asyncness.is_some()
            {
                f.sig.asyncness = None;
                let ret_ty: Type = match &f.sig.output {
                    ReturnType::Default => parse_quote! { () },
                    ReturnType::Type(_, ty) => (**ty).clone(),
                };
                f.sig.output = parse_quote! {
                    -> impl ::core::future::Future<Output = #ret_ty> + ::core::marker::Send
                };
            }
        }
    }
    if apply_vox {
        quote! {
            #[cfg_attr(feature = "vox", ::architect::vox::service)]
            #out
        }
    } else {
        quote! { #out }
    }
}

/// Hidden mirror trait — the vox-served async surface. Each user
/// method becomes an `async fn` with owned arguments.
fn emit_mirror_trait(
    rpc_trait_name: &syn::Ident,
    vis: &syn::Visibility,
    methods: &[Method],
) -> TokenStream2 {
    let methods_iter = methods.iter().map(|m| {
        let name = &m.decl.sig.ident;
        let inputs = &m.mirror_inputs;
        let output = &m.return_ty;
        // Strip doc comments to keep the hidden trait clean.
        quote! {
            async fn #name(#(#inputs),*) #output;
        }
    });

    // No supertraits on the mirror — `#[vox::service]` is picky about
    // the trait shape it accepts (no `:` bounds in its parser). Auto
    // trait bounds on the generated client/dispatcher come from vox's
    // own emission; the bridge's impl block carries the Send/Sync
    // bounds where they're actually needed.
    quote! {
        #[doc(hidden)]
        #[cfg_attr(feature = "vox", ::architect::vox::service)]
        #vis trait #rpc_trait_name {
            #(#methods_iter)*
        }
    }
}

/// Bridge struct for sync / mixed traits.
fn emit_bridge_host(
    trait_name: &syn::Ident,
    host_name: &syn::Ident,
    vis: &syn::Visibility,
) -> TokenStream2 {
    quote! {
        /// Internal bridge: holds a backend + dispatcher and implements
        /// the auto-emitted `<T>Rpc` async mirror by marshaling sync
        /// methods through the dispatcher. Hidden from documentation
        /// and users — the public mount surface is the `serve`
        /// function emitted alongside.
        #[doc(hidden)]
        #vis struct #host_name<S, D>
        where
            S: #trait_name + ::core::marker::Send + ::core::marker::Sync + 'static,
            D: ::architect::dispatch::Dispatcher,
        {
            inner: ::std::sync::Arc<S>,
            dispatcher: ::std::sync::Arc<D>,
        }

        impl<S, D> #host_name<S, D>
        where
            S: #trait_name + ::core::marker::Send + ::core::marker::Sync + 'static,
            D: ::architect::dispatch::Dispatcher,
        {
            /// Wrap a backend impl with a dispatcher. Both are kept
            /// behind `Arc` so the host is cheap to clone.
            pub fn new(inner: S, dispatcher: D) -> Self {
                Self {
                    inner: ::std::sync::Arc::new(inner),
                    dispatcher: ::std::sync::Arc::new(dispatcher),
                }
            }

            /// Wrap pre-arc'd dependencies — useful when the backend or
            /// dispatcher is already shared across multiple hosts.
            pub fn from_arc(
                inner: ::std::sync::Arc<S>,
                dispatcher: ::std::sync::Arc<D>,
            ) -> Self {
                Self { inner, dispatcher }
            }
        }

        impl<S, D> ::core::clone::Clone for #host_name<S, D>
        where
            S: #trait_name + ::core::marker::Send + ::core::marker::Sync + 'static,
            D: ::architect::dispatch::Dispatcher,
        {
            fn clone(&self) -> Self {
                Self {
                    inner: ::std::sync::Arc::clone(&self.inner),
                    dispatcher: ::std::sync::Arc::clone(&self.dispatcher),
                }
            }
        }
    }
}

/// Bridge impl: implement the mirror trait on the host by marshaling
/// each sync method through `Dispatcher::dispatch` and passing async
/// methods through directly.
fn emit_bridge_impl(
    trait_name: &syn::Ident,
    host_name: &syn::Ident,
    rpc_trait_name: &syn::Ident,
    methods: &[Method],
    _all_sync: bool,
) -> TokenStream2 {
    let method_impls = methods.iter().map(|m| {
        let name = &m.decl.sig.ident;
        let mirror_inputs = &m.mirror_inputs;
        let output = &m.return_ty;
        let arg_idents = &m.arg_idents;
        let arg_was_ref = &m.arg_was_ref;

        // Build the call expression that invokes the user trait method
        // from inside the dispatcher closure / async block, taking
        // `&owned` for any arg that was originally a reference.
        let call_args = arg_idents
            .iter()
            .zip(arg_was_ref.iter())
            .map(|(id, &was_ref)| {
                if was_ref {
                    quote! { &#id }
                } else {
                    quote! { #id }
                }
            });
        let call_args2 = call_args.clone();

        if m.is_async {
            // Pass-through: backend's async method is awaited directly.
            quote! {
                async fn #name(#(#mirror_inputs),*) #output {
                    self.inner.#name(#(#call_args),*).await
                }
            }
        } else {
            // Marshal: clone the inner Arc, capture args by move, run
            // through the dispatcher.
            quote! {
                async fn #name(#(#mirror_inputs),*) #output {
                    let __inner = ::std::sync::Arc::clone(&self.inner);
                    let __disp = ::std::sync::Arc::clone(&self.dispatcher);
                    ::architect::dispatch::run(
                        &*__disp,
                        move || __inner.#name(#(#call_args2),*),
                    )
                    .await
                    .expect("#[architect::rpc] bridge: dispatcher failed")
                }
            }
        }
    });

    quote! {
        impl<S, D> #rpc_trait_name for #host_name<S, D>
        where
            S: #trait_name + ::core::marker::Send + ::core::marker::Sync + 'static,
            D: ::architect::dispatch::Dispatcher,
        {
            #(#method_impls)*
        }
    }
}

/// Pass-through host for all-async traits. The user trait *is* its own
/// async surface, so the host is a thin newtype wrapping a backend
/// `Arc<S>`. Construction takes only the backend — no dispatcher.
fn emit_passthrough_host(
    trait_name: &syn::Ident,
    host_name: &syn::Ident,
    vis: &syn::Visibility,
) -> TokenStream2 {
    quote! {
        /// Internal newtype wrapper for an all-async backend. Hidden
        /// from documentation; user code mounts via `serve`.
        #[doc(hidden)]
        #vis struct #host_name<S>
        where
            S: #trait_name + ::core::marker::Send + ::core::marker::Sync + 'static,
        {
            inner: ::std::sync::Arc<S>,
        }

        impl<S> #host_name<S>
        where
            S: #trait_name + ::core::marker::Send + ::core::marker::Sync + 'static,
        {
            pub fn new(inner: S) -> Self {
                Self { inner: ::std::sync::Arc::new(inner) }
            }

            pub fn from_arc(inner: ::std::sync::Arc<S>) -> Self {
                Self { inner }
            }
        }

        impl<S> ::core::clone::Clone for #host_name<S>
        where
            S: #trait_name + ::core::marker::Send + ::core::marker::Sync + 'static,
        {
            fn clone(&self) -> Self {
                Self { inner: ::std::sync::Arc::clone(&self.inner) }
            }
        }
    }
}

/// Pass-through impl: each method calls through to the inner backend.
/// All methods are async by definition for this shape.
fn emit_passthrough_impl(
    trait_name: &syn::Ident,
    host_name: &syn::Ident,
    methods: &[Method],
) -> TokenStream2 {
    let method_impls = methods.iter().map(|m| {
        let name = &m.decl.sig.ident;
        let mirror_inputs = &m.mirror_inputs;
        let output = &m.return_ty;
        let arg_idents = &m.arg_idents;
        let arg_was_ref = &m.arg_was_ref;

        let call_args = arg_idents
            .iter()
            .zip(arg_was_ref.iter())
            .map(|(id, &was_ref)| {
                if was_ref {
                    quote! { &#id }
                } else {
                    quote! { #id }
                }
            });

        quote! {
            async fn #name(#(#mirror_inputs),*) #output {
                self.inner.#name(#(#call_args),*).await
            }
        }
    });

    quote! {
        impl<S> #trait_name for #host_name<S>
        where
            S: #trait_name + ::core::marker::Send + ::core::marker::Sync + 'static,
        {
            #(#method_impls)*
        }
    }
}
