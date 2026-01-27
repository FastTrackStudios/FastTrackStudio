//! Shared utilities for local ROAM service clients
//!
//! This module provides the `local_context()` function and macro for generating
//! LocalClient wrappers that eliminate boilerplate across service implementations.

use roam::wire::{ConnectionId, MethodId, RequestId};
use std::sync::Arc;

/// Create a local context for in-process service calls.
///
/// This is the shared implementation used by all LocalClient wrappers.
/// It creates a dummy ROAM context that works for in-process calls where
/// no actual network serialization occurs.
pub(crate) fn local_context() -> roam::Context {
    roam::Context::new(
        ConnectionId(0),
        RequestId(0),
        MethodId(0),
        vec![],
        vec![],
    )
}

/// Macro to generate a LocalClient wrapper for a ROAM service.
///
/// This eliminates the boilerplate of:
/// - Creating the struct with Arc<S>
/// - Implementing new()
/// - Implementing Clone and PartialEq
/// - Forwarding all service methods with local_context() injection
///
/// # Example
///
/// ```rust,ignore
/// define_local_client! {
///     /// Local client for BlockService
///     client: LocalBlockClient,
///     service: BlockService,
///     methods: {
///         async fn get_all_block_presets() -> Vec<BlockPresetInfo>;
///         async fn get_block_presets_by_type(block_type: BlockType) -> Vec<BlockPresetInfo>;
///         async fn search_block_presets(query: String) -> Vec<BlockPresetInfo>;
///         async fn get_block_preset(preset_id: Uuid) -> Option<BlockPresetInfo>;
///         async fn execute(command: BlockCommand) -> Result<(), String>;
///     }
/// }
/// ```
///
/// This generates:
/// - `pub struct LocalBlockClient<S: BlockService + Send + Sync + 'static>`
/// - `impl LocalBlockClient<S>` with `new()` and all method forwarders
/// - `Clone` and `PartialEq` implementations
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
            /// Create a new local client wrapping the given service
            pub fn new(service: ::std::sync::Arc<S>) -> Self {
                Self { service }
            }

            $(
                $(#[$method_meta])*
                pub async fn $method(&self, $($param: $param_ty),*) -> $ret {
                    self.service.$method(&$crate::rig::local_client::local_context(), $($param),*).await
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
