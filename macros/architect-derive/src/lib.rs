//! `#[derive(Entity)]` — the architect macro.
//!
//! User writes a plain Rust struct with `#[architect(...)]` attributes
//! that describe both **wire shape** (what the type looks like over
//! vox/HTTP) and **storage shape** (how SeaORM persists it). The macro
//! emits both, gated by the consumer's `server` cargo feature.
//!
//! ## Input
//!
//! ```ignore
//! #[derive(architect::Entity)]
//! #[architect(
//!     table_name = "examples",
//!     repo,                          // emit ExampleRepo vox trait
//! )]
//! pub struct Example {
//!     #[architect(primary_key, on_create = Uuid::new_v4())]
//!     pub id: Uuid,
//!
//!     #[architect(filterable, sortable, fulltext)]
//!     pub name: String,
//!
//!     #[architect(exclude(create, update), on_create = Utc::now())]
//!     pub created_at: DateTime<Utc>,
//! }
//! ```
//!
//! ## Output (sketch)
//!
//! ```ignore
//! // Always emitted — wasm-safe.
//! #[derive(Clone, Debug, PartialEq, facet::Facet)]
//! pub struct Example { ... }
//!
//! #[derive(Clone, Debug, facet::Facet)]
//! pub struct ExampleCreate { ... fields minus excluded ... }
//!
//! #[derive(Clone, Debug, facet::Facet)]
//! pub struct ExampleUpdate { ... }
//!
//! #[vox::service]
//! pub trait ExampleRepo {
//!     async fn get_example(&self, id: Uuid) -> Result<Example, RepoError>;
//!     async fn list_examples(&self) -> Result<Vec<Example>, RepoError>;
//!     async fn create_example(&self, input: ExampleCreate) -> Result<Example, RepoError>;
//!     async fn update_example(&self, id: Uuid, input: ExampleUpdate) -> Result<Example, RepoError>;
//!     async fn delete_example(&self, id: Uuid) -> Result<(), RepoError>;
//! }
//!
//! // Server-only — gated.
//! #[cfg(feature = "server")]
//! mod __architect_server {
//!     #[derive(sea_orm::DeriveEntityModel)]
//!     #[sea_orm(table_name = "examples")]
//!     pub struct Model { ... same fields ... }
//!
//!     impl From<Model> for super::Example { ... }
//!     impl From<super::Example> for ActiveModel { ... }
//!
//!     pub struct ExampleRepoStorage<C> { db: C }
//!
//!     impl<C: sea_orm::ConnectionTrait + Clone + Send + Sync + 'static>
//!         super::ExampleRepo for ExampleRepoStorage<C>
//!     { ... CRUD impls via SeaORM ... }
//! }
//! ```
//!
//! ## Why "100% facet"
//!
//! Vox encodes payloads via `facet` already. By dropping the parallel
//! serde derives entirely, the wire format is one cohesive system —
//! every architect type is automatically `Facet`-able, which means
//! vox can transport it without any per-type glue.

use proc_macro::TokenStream;

/// Entry point for `#[derive(Entity)]`. Today this is a stub that
/// emits the input struct unchanged — full emission lands in the next
/// pass. Kept here so the rest of the workspace can compile against
/// the final macro signature before any user code changes.
#[proc_macro_derive(Entity, attributes(architect))]
pub fn derive_entity(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    // TODO: parse #[architect(...)] container + field attrs into a
    // metadata struct, then emit the four sections above. For now
    // re-emit the struct so the crate skeleton compiles and downstream
    // example-proto can switch its derive without a full impl behind it.
    let ident = &input.ident;
    let vis = &input.vis;
    let fields = match &input.data {
        syn::Data::Struct(s) => &s.fields,
        _ => {
            return syn::Error::new_spanned(ident, "architect::Entity expects a struct")
                .to_compile_error()
                .into();
        }
    };
    let field_defs: Vec<_> = fields
        .iter()
        .map(|f| {
            let attrs = f
                .attrs
                .iter()
                .filter(|a| !a.path().is_ident("architect"))
                .collect::<Vec<_>>();
            let vis = &f.vis;
            let ident = &f.ident;
            let ty = &f.ty;
            quote::quote! { #(#attrs)* #vis #ident: #ty }
        })
        .collect();

    quote::quote! {
        #[derive(Clone, Debug, PartialEq, ::facet::Facet)]
        #vis struct #ident {
            #(#field_defs,)*
        }
    }
    .into()
}
