//! Shared macros for Dioxus service context providers
//!
//! This module provides the `define_service_context!` macro that eliminates
//! boilerplate across service context implementations.

/// Macro to generate a Dioxus context provider for a ROAM service client.
///
/// This eliminates the boilerplate of:
/// - Creating the XxxServiceCtx struct
/// - Implementing use_xxx_service() hook
/// - Implementing XxxServiceProvider component
///
/// # Example
///
/// ```rust,ignore
/// define_service_context! {
///     /// Block service context
///     name: Block,
///     client_type: LocalBlockClient<MockBlockService>,
/// }
/// ```
///
/// This generates:
/// - `pub struct BlockServiceCtx { pub client: LocalBlockClient<MockBlockService> }`
/// - `pub fn use_block_service() -> BlockServiceCtx`
/// - `#[component] pub fn BlockServiceProvider(...) -> Element`
#[macro_export]
macro_rules! define_service_context {
    (
        $(#[$meta:meta])*
        name: $name:ident,
        client_type: $client_ty:ty,
    ) => {
        ::paste::paste! {
            $(#[$meta])*
            #[derive(Clone)]
            pub struct [<$name ServiceCtx>] {
                /// The local client wrapping the service implementation
                pub client: $client_ty,
            }

            #[doc = "Hook to access the " $name:lower " service from context\n\n"]
            #[doc = "# Panics\n"]
            #[doc = "Panics if called outside of a `" [<$name ServiceProvider>] "`"]
            pub fn [<use_ $name:snake _service>]() -> [<$name ServiceCtx>] {
                ::dioxus::prelude::use_context::<[<$name ServiceCtx>]>()
            }

            #[doc = "Provider component that injects the " $name:lower " service into context"]
            #[::dioxus::prelude::component]
            pub fn [<$name ServiceProvider>](
                client: $client_ty,
                children: ::dioxus::prelude::Element
            ) -> ::dioxus::prelude::Element {
                ::dioxus::prelude::use_context_provider(|| [<$name ServiceCtx>] {
                    client: client.clone(),
                });
                ::dioxus::prelude::rsx! { {children} }
            }
        }
    };
}
