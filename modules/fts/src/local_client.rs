//! Local client utilities for in-process ROAM service calls.
//!
//! Provides `local_context()` and the `define_local_client!` macro for
//! generating `LocalXxxClient<S>` wrappers that eliminate serialization
//! overhead when calling services in-process.

use roam::wire::{ConnectionId, MethodId, RequestId};

/// Create a dummy ROAM context for in-process service calls.
///
/// This avoids serialization/deserialization overhead when the service
/// is called directly in the same process (tests, desktop app, etc.).
pub(crate) fn local_context() -> roam::Context {
    roam::Context::new(
        ConnectionId(0),
        RequestId(0),
        MethodId(0),
        vec![],
        vec![],
    )
}

/// Macro to generate a `LocalClient` wrapper for a ROAM service.
///
/// Generates a struct wrapping `Arc<S>` that forwards all method calls
/// through a dummy local context — no serialization, no transport.
///
/// # Example
///
/// ```rust,ignore
/// define_local_client! {
///     /// Local client for SetlistService
///     client: LocalSetlistClient,
///     service: SetlistService,
///     methods: {
///         async fn get_current_song() -> Option<SongInfo>;
///         async fn execute(cmd: SetlistCommand) -> ();
///     }
/// }
/// ```
#[macro_export]
macro_rules! define_local_client {
    (
        $(#[$meta:meta])*
        client: $client_name:ident,
        service: $service_trait:ident,
        methods: {
            $(
                $(#[$method_meta:meta])*
                async fn $method:ident( $($param:ident: $param_ty:ty),* $(,)? ) -> $ret:ty;
            )*
        }
    ) => {
        $(#[$meta])*
        pub struct $client_name<S: $service_trait + Send + Sync + 'static> {
            service: ::std::sync::Arc<S>,
        }

        impl<S: $service_trait + Send + Sync + 'static> $client_name<S> {
            /// Create a new local client wrapping the given service.
            pub fn new(service: ::std::sync::Arc<S>) -> Self {
                Self { service }
            }

            $(
                $(#[$method_meta])*
                pub async fn $method(&self, $($param: $param_ty),*) -> $ret {
                    self.service.$method(&$crate::local_client::local_context(), $($param),*).await
                }
            )*
        }

        impl<S: $service_trait + Send + Sync + 'static> Clone for $client_name<S> {
            fn clone(&self) -> Self {
                Self {
                    service: ::std::sync::Arc::clone(&self.service),
                }
            }
        }

        impl<S: $service_trait + Send + Sync + 'static> PartialEq for $client_name<S> {
            fn eq(&self, other: &Self) -> bool {
                ::std::sync::Arc::ptr_eq(&self.service, &other.service)
            }
        }
    };
}
