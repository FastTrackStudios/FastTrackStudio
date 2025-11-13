//! FastTrackStudio Proc Macros
//!
//! This crate provides procedural macros for generating action enums from traits.

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, ItemTrait, TraitItem,
    FnArg, PatType, Signature
};
use convert_case::{Case, Casing};

/// Attribute macro for specifying action traits
///
/// This generates an Action enum with variants for each trait method.
/// The trait itself provides the clean domain interface.
/// Supports both sync and async methods in the same trait.
///
/// # Example
///
/// ```rust,ignore
/// #[actions]
/// trait TransportActions {
///     fn play(&mut self) -> Result<String, TransportError>;
///     async fn load_project(&mut self, path: String) -> Result<String, TransportError>;
///     fn get_tempo(&self) -> Result<Tempo, TransportError>;
/// }
/// ```
///
/// This generates a `TransportAction` enum with variants for each method.
#[proc_macro_attribute]
pub fn actions(_args: TokenStream, input: TokenStream) -> TokenStream {
    let trait_item = parse_macro_input!(input as ItemTrait);

    let trait_name = &trait_item.ident;
    let action_enum_name = syn::Ident::new(
        &format!("{}Action", trait_name.to_string().replace("Actions", "")),
        trait_name.span()
    );

    // Extract all methods (both sync and async)
    let methods = trait_item.items.iter().filter_map(|item| {
        if let TraitItem::Fn(method) = item {
            Some(method)
        } else {
            None
        }
    }).collect::<Vec<_>>();

    if methods.is_empty() {
        return syn::Error::new_spanned(
            &trait_item,
            "Actions trait must have at least one method"
        ).to_compile_error().into();
    }

    // Generate Action enum variants (same for both sync and async methods)
    let enum_variants = methods.iter().map(|method| {
        let method_name = &method.sig.ident;
        let variant_name = method_name_to_variant_name(method_name);

        let params = extract_method_parameters(&method.sig);

        if params.is_empty() {
            quote! {
                #[doc = concat!("Action for ", stringify!(#method_name), " method")]
                #variant_name
            }
        } else if params.len() == 1 {
            let param_type = &params[0].1;
            quote! {
                #[doc = concat!("Action for ", stringify!(#method_name), " method")]
                #variant_name(#param_type)
            }
        } else {
            let param_types: Vec<_> = params.iter().map(|(_, ty)| ty).collect();
            quote! {
                #[doc = concat!("Action for ", stringify!(#method_name), " method")]
                #variant_name(#(#param_types),*)
            }
        }
    });

    // Generate dispatch match arms for all methods
    let dispatch_matches = methods.iter().map(|method| {
        let method_name = &method.sig.ident;
        let variant_name = method_name_to_variant_name(method_name);
        let params = extract_method_parameters(&method.sig);
        let is_async = method.sig.asyncness.is_some();

        if params.is_empty() {
            if is_async {
                quote! {
                    #action_enum_name::#variant_name => $impl.#method_name().await,
                }
            } else {
                quote! {
                    #action_enum_name::#variant_name => $impl.#method_name(),
                }
            }
        } else if params.len() == 1 {
            let param_name = &params[0].0;
            if is_async {
                quote! {
                    #action_enum_name::#variant_name(#param_name) => $impl.#method_name(#param_name).await,
                }
            } else {
                quote! {
                    #action_enum_name::#variant_name(#param_name) => $impl.#method_name(#param_name),
                }
            }
        } else {
            let param_names: Vec<_> = params.iter().map(|(name, _)| name).collect();
            if is_async {
                quote! {
                    #action_enum_name::#variant_name(#(#param_names),*) => $impl.#method_name(#(#param_names),*).await,
                }
            } else {
                quote! {
                    #action_enum_name::#variant_name(#(#param_names),*) => $impl.#method_name(#(#param_names),*),
                }
            }
        }
    });

    // Generate handler macro name
    let handler_macro_name = syn::Ident::new(&format!("{}_handler", action_enum_name.to_string().to_lowercase()), trait_name.span());

    let expanded = quote! {
        // Original trait definition
        #trait_item

        /// Auto-generated Action enum for #trait_name
        ///
        /// This enum contains variants for all methods in the trait,
        /// regardless of whether they are sync or async. Each variant
        /// captures the method parameters if any.
        #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
        #[serde(tag = "action", content = "params")]
        pub enum #action_enum_name {
            #(#enum_variants),*
        }

        /// Protocol-agnostic handler macro for calling trait methods based on action enum variants
        ///
        /// Usage: `transportaction_handler!(impl, action)`
        /// This will call the appropriate method on the implementation and return the result.
        /// Can be used by HTTP servers, OSC servers, or any other protocol implementation.
        ///
        /// Note: This generates the exact match statement that maps enum variants to method calls.
        /// The caller is responsible for handling different return types appropriately.
        #[macro_export]
        macro_rules! #handler_macro_name {
            ($impl:expr, $action:expr) => {
                match $action {
                    #(#dispatch_matches)*
                }
            };
        }

        // Re-export the macro for convenience
        pub use #handler_macro_name;
    };

    TokenStream::from(expanded)
}

/// Convert a method name to a PascalCase variant name
fn method_name_to_variant_name(method_name: &syn::Ident) -> syn::Ident {
    let variant_name = method_name.to_string().to_case(Case::Pascal);
    syn::Ident::new(&variant_name, method_name.span())
}

/// Extract parameters from a method signature, excluding self
fn extract_method_parameters(sig: &Signature) -> Vec<(syn::Ident, syn::Type)> {
    let mut params = Vec::new();

    for input in &sig.inputs {
        match input {
            FnArg::Typed(PatType { pat, ty, .. }) => {
                if let syn::Pat::Ident(ident) = pat.as_ref() {
                    // Skip self parameters
                    if ident.ident == "self" {
                        continue;
                    }
                    params.push((ident.ident.clone(), (**ty).clone()));
                }
            }
            FnArg::Receiver(_) => {
                // Skip self receiver
                continue;
            }
        }
    }

    params
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_method_name_conversion() {
        let ident = syn::Ident::new("get_tempo", proc_macro2::Span::call_site());
        let variant = method_name_to_variant_name(&ident);
        assert_eq!(variant.to_string(), "GetTempo");

        let ident = syn::Ident::new("play", proc_macro2::Span::call_site());
        let variant = method_name_to_variant_name(&ident);
        assert_eq!(variant.to_string(), "Play");

        let ident = syn::Ident::new("play_pause", proc_macro2::Span::call_site());
        let variant = method_name_to_variant_name(&ident);
        assert_eq!(variant.to_string(), "PlayPause");

        let ident = syn::Ident::new("set_tempo", proc_macro2::Span::call_site());
        let variant = method_name_to_variant_name(&ident);
        assert_eq!(variant.to_string(), "SetTempo");
    }
}
