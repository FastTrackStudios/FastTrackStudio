use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident, Type};

/// Derive macro for the Metadata trait
///
/// This macro automatically generates:
/// - A Field enum with variants for each struct field
/// - A Value enum that can hold any of the field types
/// - The Metadata trait implementation
///
/// # Example
///
/// ```ignore
/// use monarchy::Metadata;
///
/// #[derive(Metadata, Clone, Default)]
/// struct AudioMetadata {
///     instrument: Option<String>,
///     performer: Option<String>,
///     layer: Option<u32>,
/// }
/// ```
///
/// This generates:
/// - `AudioMetadataField` enum with variants: Instrument, Performer, Layer
/// - `AudioMetadataValue` enum with variants holding the respective types
/// - Complete `Metadata` trait implementation
///
/// Supports attributes:
/// - `#[metadata(skip)]` - Exclude field from metadata
/// - `#[monarchy(variant)]` - Mark field as a variant differentiator
#[proc_macro_derive(Metadata, attributes(metadata, monarchy))]
pub fn derive_metadata(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    // Get the struct name
    let struct_name = &input.ident;

    // Generate field and value enum names
    let field_enum_name = Ident::new(&format!("{}Field", struct_name), Span::call_site());
    let value_enum_name = Ident::new(&format!("{}Value", struct_name), Span::call_site());

    // Extract fields from the struct
    let fields = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields) => &fields.named,
            _ => {
                return syn::Error::new(
                    Span::call_site(),
                    "Metadata can only be derived for structs with named fields",
                )
                .to_compile_error()
                .into();
            }
        },
        _ => {
            return syn::Error::new(
                Span::call_site(),
                "Metadata can only be derived for structs",
            )
            .to_compile_error()
            .into();
        }
    };

    // Collect variant fields
    let mut variant_fields = Vec::new();

    // Collect field information
    let field_info: Vec<FieldInfo> = fields
        .iter()
        .filter_map(|field| {
            let field_name = field.ident.as_ref()?;
            let field_type = extract_option_type(&field.ty)?;

            // Check for #[monarchy(variant)] attribute
            let is_variant = field.attrs.iter().any(|attr| {
                if attr.path().is_ident("monarchy") {
                    attr.parse_args::<syn::Ident>()
                        .map(|i| i == "variant")
                        .unwrap_or(false)
                } else {
                    false
                }
            });

            // Check for #[metadata(skip)] attribute
            let skip = field.attrs.iter().any(|attr| {
                if attr.path().is_ident("metadata") {
                    attr.parse_args::<syn::Ident>()
                        .map(|i| i == "skip")
                        .unwrap_or(false)
                } else {
                    false
                }
            });

            if skip {
                return None;
            }

            let variant_name = Ident::new(
                &field_name.to_string().to_case(Case::Pascal),
                field_name.span(),
            );

            if is_variant {
                variant_fields.push(variant_name.clone());
            }

            Some(FieldInfo {
                field_name: field_name.clone(),
                variant_name,
                field_type: field_type.clone(),
            })
        })
        .collect();

    // Generate field enum variants
    let field_variants = field_info.iter().map(|info| {
        let variant = &info.variant_name;
        quote! { #variant }
    });

    // Generate value enum variants
    let value_variants = field_info.iter().map(|info| {
        let variant = &info.variant_name;
        let ty = &info.field_type;
        quote! { #variant(#ty) }
    });

    // Generate match arms for get()
    let get_arms = field_info.iter().map(|info| {
        let variant = &info.variant_name;
        let field = &info.field_name;
        quote! {
            #field_enum_name::#variant => {
                self.#field.clone().map(#value_enum_name::#variant)
            }
        }
    });

    // Generate match arms for set()
    let set_arms = field_info.iter().map(|info| {
        let variant = &info.variant_name;
        let field = &info.field_name;
        quote! {
            (#field_enum_name::#variant, #value_enum_name::#variant(v)) => {
                self.#field = Some(v);
            }
        }
    });

    // Generate fields() implementation
    let field_list = field_info.iter().map(|info| {
        let variant = &info.variant_name;
        quote! { #field_enum_name::#variant }
    });

    // Generate variant_fields() implementation
    let variant_field_list = variant_fields.iter().map(|variant| {
        quote! { #field_enum_name::#variant }
    });

    // Generate the output tokens
    let output = quote! {
        // Field enum
        #[derive(Debug, Clone, PartialEq, Eq, ::serde::Serialize, ::serde::Deserialize)]
        #[serde(rename_all = "snake_case")]
        pub enum #field_enum_name {
            #(#field_variants),*
        }

        // Value enum
        #[derive(Debug, Clone)]
        pub enum #value_enum_name {
            #(#value_variants),*
        }

        // Metadata trait implementation
        impl monarchy::Metadata for #struct_name {
            type Field = #field_enum_name;
            type Value = #value_enum_name;

            fn get(&self, field: &Self::Field) -> Option<Self::Value> {
                match field {
                    #(#get_arms),*
                }
            }

            fn set(&mut self, field: Self::Field, value: Self::Value) {
                match (field, value) {
                    #(#set_arms),*
                    _ => {}
                }
            }

            fn fields() -> Vec<Self::Field> {
                vec![
                    #(#field_list),*
                ]
            }

            fn variant_fields() -> Vec<Self::Field> {
                vec![
                    #(#variant_field_list),*
                ]
            }
        }
    };

    output.into()
}

struct FieldInfo {
    field_name: Ident,
    variant_name: Ident,
    field_type: Type,
}

/// Extract the inner type from Option<T>
fn extract_option_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(syn::GenericArgument::Type(inner_type)) = args.args.first() {
                        return Some(inner_type);
                    }
                }
            }
        }
    }
    None
}
