//! `#[derive(Entity)]` — the architect macro.
//!
//! See the crate-level docs in `architect` for the conceptual overview.
//! This file is the emission engine: parse the `#[architect(...)]`
//! container + field attributes, then synthesise the wire types, the
//! repo trait, and (under `--features server`) the SeaORM bridge.

use heck::{ToPascalCase, ToSnakeCase};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{
    Data, DeriveInput, Expr, Field, Fields, Ident, LitBool, LitStr, Result, Type, parse_macro_input,
};

// ── Attribute parsing ─────────────────────────────────────────────────

#[derive(Default)]
struct ContainerAttrs {
    table_name: Option<String>,
    emit_repo: bool,
}

#[derive(Default)]
struct FieldAttrs {
    primary_key: bool,
    auto_increment: Option<bool>,
    on_create: Option<Expr>,
    on_update: Option<Expr>,
    filterable: bool,
    sortable: bool,
    fulltext: bool,
    exclude_create: bool,
    exclude_update: bool,
    /// Store this field as a JSON column in the SeaORM model. Required
    /// for `Vec<T>` / structured types — `sea_orm::Value` has no
    /// blanket `From<Vec<T>>` impl, so without this attribute the
    /// generated DeriveEntityModel fails to compile on `server`
    /// builds. The field type must be `serde::Serialize +
    /// DeserializeOwned`; sea-orm's `with-json` feature handles the
    /// rest. No-op when the `server` feature isn't active.
    json: bool,
}

fn parse_container_attrs(attrs: &[syn::Attribute]) -> Result<ContainerAttrs> {
    let mut out = ContainerAttrs::default();
    for attr in attrs {
        if !attr.path().is_ident("architect") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("table_name") {
                let s: LitStr = meta.value()?.parse()?;
                out.table_name = Some(s.value());
            } else if meta.path.is_ident("repo") {
                out.emit_repo = true;
            } else {
                return Err(meta.error("unknown architect container attribute"));
            }
            Ok(())
        })?;
    }
    Ok(out)
}

fn parse_field_attrs(field: &Field) -> Result<FieldAttrs> {
    let mut out = FieldAttrs::default();
    for attr in &field.attrs {
        if !attr.path().is_ident("architect") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            let p = &meta.path;
            if p.is_ident("primary_key") {
                out.primary_key = true;
            } else if p.is_ident("auto_increment") {
                let b: LitBool = meta.value()?.parse()?;
                out.auto_increment = Some(b.value);
            } else if p.is_ident("on_create") {
                let e: Expr = meta.value()?.parse()?;
                out.on_create = Some(e);
            } else if p.is_ident("on_update") {
                let e: Expr = meta.value()?.parse()?;
                out.on_update = Some(e);
            } else if p.is_ident("filterable") {
                out.filterable = true;
            } else if p.is_ident("sortable") {
                out.sortable = true;
            } else if p.is_ident("fulltext") {
                out.fulltext = true;
            } else if p.is_ident("json") {
                out.json = true;
            } else if p.is_ident("exclude") {
                meta.parse_nested_meta(|inner| {
                    if inner.path.is_ident("create") {
                        out.exclude_create = true;
                    } else if inner.path.is_ident("update") {
                        out.exclude_update = true;
                    } else {
                        return Err(inner.error("unknown exclude target"));
                    }
                    Ok(())
                })?;
            } else {
                return Err(meta.error("unknown architect field attribute"));
            }
            Ok(())
        })?;
    }
    Ok(out)
}

// ── Derive entry point ────────────────────────────────────────────────

#[proc_macro_derive(Entity, attributes(architect))]
pub fn derive_entity(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match expand(input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

struct ParsedField<'a> {
    ident: &'a Ident,
    ty: &'a Type,
    attrs: FieldAttrs,
    /// Pass-through attributes from the source field — everything
    /// that isn't `#[architect(...)]`. Forwarded onto the emitted
    /// `<Entity>Create` field so `#[dummy(faker = "...")]` annotations
    /// (and any other field-level derive helpers like `#[serde(...)]`)
    /// reach the generated Dummy impl that `seed_fake_<entity>`
    /// drives.
    forward_attrs: Vec<syn::Attribute>,
}

fn expand(input: DeriveInput) -> Result<TokenStream2> {
    let ident = input.ident.clone();
    let vis = input.vis.clone();
    let container = parse_container_attrs(&input.attrs)?;

    let named = match &input.data {
        Data::Struct(s) => match &s.fields {
            Fields::Named(n) => &n.named,
            _ => {
                return Err(syn::Error::new_spanned(
                    &ident,
                    "architect::Entity requires named fields",
                ));
            }
        },
        _ => {
            return Err(syn::Error::new_spanned(
                &ident,
                "architect::Entity requires a struct",
            ));
        }
    };

    let mut parsed: Vec<ParsedField> = Vec::with_capacity(named.len());
    for f in named.iter() {
        let attrs = parse_field_attrs(f)?;
        // Pass-through attrs end up on the emitted `Create`
        // struct (whose only derive is `Facet` + optional
        // `Dummy`). Strip helper attributes whose owning
        // derive ISN'T applied to `Create` — otherwise rustc
        // errors "cannot find attribute `serde` in this scope"
        // because the attribute has no claimant on the Create
        // struct. `#[serde(...)]` is the common case (entities
        // that double as a markdown/JSON wire format on the
        // ORIGINAL struct don't want their field-level serde
        // attrs leaked onto the synthetic Create).
        let forward_attrs: Vec<syn::Attribute> = f
            .attrs
            .iter()
            .filter(|a| {
                !a.path().is_ident("architect")
                    && !a.path().is_ident("serde")
                    && !a.path().is_ident("schemars")
            })
            .cloned()
            .collect();
        parsed.push(ParsedField {
            ident: f.ident.as_ref().unwrap(),
            ty: &f.ty,
            attrs,
            forward_attrs,
        });
    }

    let pk = parsed.iter().find(|f| f.attrs.primary_key).ok_or_else(|| {
        syn::Error::new_spanned(
            &ident,
            "architect::Entity needs a #[architect(primary_key)] field",
        )
    })?;
    let pk_ident = pk.ident;
    let pk_ty = pk.ty;

    let create_ident = format_ident!("{}Create", ident);
    let update_ident = format_ident!("{}Update", ident);
    let list_ident = format_ident!("{}List", ident);
    let repo_ident = format_ident!("{}Repo", ident);
    let storage_ident = format_ident!("{}RepoStorage", ident);

    // Wire struct stays in user code (the derive can't modify it). User
    // writes `#[derive(architect::Entity, facet::Facet, Clone, Debug, PartialEq)]`.
    let wire_struct = quote! {};

    // ── Create payload ──
    let create_fields: Vec<_> = parsed
        .iter()
        .filter(|f| !f.attrs.exclude_create && f.attrs.on_create.is_none())
        .collect();
    let create_field_defs = create_fields.iter().map(|f| {
        let id = f.ident;
        let ty = f.ty;
        let fwd = &f.forward_attrs;
        quote! { #(#fwd)* pub #id: #ty }
    });
    let create_struct = quote! {
        #[cfg_attr(feature = "fake", derive(::architect::fake::Dummy))]
        #[derive(Clone, Debug, PartialEq, ::architect::facet::Facet)]
        #vis struct #create_ident {
            #(#create_field_defs,)*
        }
    };

    // ── Update payload (all Option<T>) ──
    let update_fields: Vec<_> = parsed
        .iter()
        .filter(|f| !f.attrs.exclude_update && !f.attrs.primary_key && f.attrs.on_update.is_none())
        .collect();
    let update_field_defs = update_fields.iter().map(|f| {
        let id = f.ident;
        let ty = f.ty;
        quote! { pub #id: ::core::option::Option<#ty> }
    });
    let update_struct = quote! {
        #[cfg_attr(feature = "fake", derive(::architect::fake::Dummy))]
        #[derive(Clone, Debug, PartialEq, ::architect::facet::Facet, Default)]
        #vis struct #update_ident {
            #(#update_field_defs,)*
        }
    };

    // ── List payload ──
    let list_struct = quote! {
        #[cfg_attr(feature = "fake", derive(::architect::fake::Dummy))]
        #[derive(Clone, Debug, PartialEq, ::architect::facet::Facet)]
        #vis struct #list_ident {
            pub items: ::std::vec::Vec<#ident>,
            pub total: u32,
            pub page: ::architect::Page,
        }
    };

    // ── Repo trait ──
    //
    // `cfg_attr` gates the `#[vox::service]` decoration on the user
    // crate's `vox` feature. With the feature on, vox generates the
    // dispatcher + client types as usual. With it off, the trait is
    // just a plain Rust 2024 async trait and no RPC machinery
    // compiles. This is the seam that lets a consumer use the trait
    // in-process without pulling vox.
    let repo_trait = if container.emit_repo {
        quote! {
            #[cfg_attr(feature = "vox", ::vox::service)]
            #vis trait #repo_ident {
                async fn get(&self, id: #pk_ty)
                    -> ::core::result::Result<#ident, ::architect::RepoError>;
                async fn list(
                    &self,
                    page: ::architect::Page,
                    sort: ::core::option::Option<::architect::Sort>,
                    filter: ::core::option::Option<::architect::Filter>,
                ) -> ::core::result::Result<#list_ident, ::architect::RepoError>;
                async fn create(&self, input: #create_ident)
                    -> ::core::result::Result<#ident, ::architect::RepoError>;
                async fn update(&self, id: #pk_ty, input: #update_ident)
                    -> ::core::result::Result<#ident, ::architect::RepoError>;
                async fn delete(&self, id: #pk_ty)
                    -> ::core::result::Result<(), ::architect::RepoError>;
            }
        }
    } else {
        quote! {}
    };

    // ── Repo as a Layer service ──
    //
    // The `<Entity>Repo` trait is an all-async `#[vox::service]`, exactly
    // the shape `#[architect::rpc]` turns into a composable `Service`
    // token. Emit the same surface for repos so they drop into a
    // `layers![…]` bundle and `.provide(backend)` / `.into_router()` like
    // any other service. Because several entities share one module, the
    // token is uniquely named `<Entity>RepoLayer` (not the bare `Service`
    // that rpc-derive — one trait per module — can get away with).
    //
    // We reference the identifiers `#[vox::service]` emits from the repo
    // trait directly: `<Entity>RepoDispatcher::new(backend)` (the vox
    // Handler; architect's blanket `DynHandler` covers it) and the
    // `<snake>_service_descriptor()` free fn.
    let repo_layer = if container.emit_repo {
        let layer_token = format_ident!("{}Layer", repo_ident);
        let repo_snake = repo_ident.to_string().to_snake_case();
        let descriptor_fn = format_ident!("{}_service_descriptor", repo_snake);
        let dispatcher_ident = format_ident!("{}Dispatcher", repo_ident);
        let layer_fn = format_ident!("{}_layer", repo_snake);
        quote! {
            /// Deferred-bind Layer token for the repository service. Acts
            /// as a one-element [`architect::Layer`]; compose it with
            /// [`architect::Layer::merge`] and bind a backend at
            /// [`architect::Layer::provide`] time. Any backend that
            /// implements `#repo_ident` (the SeaORM storage, an in-memory
            /// repo, a third-party impl) is interchangeable here.
            #[cfg(feature = "vox")]
            #[derive(Debug, Default, Clone, Copy)]
            #vis struct #layer_token;

            #[cfg(feature = "vox")]
            impl ::architect::BindAny for #layer_token {
                fn descriptor(&self) -> &'static ::architect::vox::ServiceDescriptor {
                    #descriptor_fn()
                }
            }

            #[cfg(feature = "vox")]
            impl<B> ::architect::Bind<B> for #layer_token
            where
                B: #repo_ident
                    + ::core::clone::Clone
                    + ::core::marker::Send
                    + ::core::marker::Sync
                    + 'static,
            {
                fn bind_into(self, backend: &B, router: &mut ::architect::LayerRouter) {
                    use ::architect::LayerSink as _;
                    router.add_mounted(::architect::Mounted::new(
                        #descriptor_fn(),
                        #dispatcher_ident::new(backend.clone()),
                    ));
                }
            }

            #[cfg(feature = "vox")]
            impl<R> ::architect::Append<R> for #layer_token {
                type Output = ::architect::Cons<#layer_token, R>;
                fn append(self, rhs: R) -> Self::Output {
                    ::architect::Cons::new(self, rhs)
                }
            }

            #[cfg(feature = "vox")]
            impl ::architect::Descriptors for #layer_token {
                fn collect(
                    &self,
                    out: &mut ::std::vec::Vec<&'static ::architect::vox::ServiceDescriptor>,
                ) {
                    out.push(::architect::BindAny::descriptor(self));
                }
            }

            /// Immediate-bind shortcut: wrap a backend in the repo's vox
            /// dispatcher and return a `Mounted`, ready to drop into
            /// someone else's bundle via [`architect::Layer::merge`]
            /// (e.g. to override the repo impl for one service).
            #[cfg(feature = "vox")]
            #vis fn #layer_fn<B>(backend: B) -> ::architect::Mounted
            where
                B: #repo_ident
                    + ::core::clone::Clone
                    + ::core::marker::Send
                    + ::core::marker::Sync
                    + 'static,
            {
                ::architect::Mounted::new(#descriptor_fn(), #dispatcher_ident::new(backend))
            }
        }
    } else {
        quote! {}
    };

    // ── Fake-data seeding helper ──
    //
    // Emit a free `seed_fake_<entity>` function that loops, generates
    // a `<Entity>Create` via fake-rs, and feeds each one into the
    // repo. Gated on the user crate's `fake` feature, plus only
    // emitted when a repo trait exists to call into.
    let seed_fn_ident = format_ident!("seed_fake_{}", ident.to_string().to_snake_case());
    let seed_fn = if container.emit_repo {
        quote! {
            /// Generate `count` faked `#ident` instances and persist
            /// them via `repo.create`. `count` accepts a single number
            /// (`500`) or a range (`10..30` / `10..=30`) for a random
            /// quantity per call. See `architect::seed::SeedCount`.
            #[cfg(feature = "fake")]
            #vis async fn #seed_fn_ident<R>(
                repo: &R,
                count: impl ::architect::seed::SeedCount,
            ) -> ::core::result::Result<
                ::std::vec::Vec<#ident>,
                ::architect::RepoError,
            >
            where
                R: #repo_ident + ?::core::marker::Sized,
            {
                use ::fake::Fake as _;
                let n = ::architect::seed::SeedCount::pick(&count);
                let mut out = ::std::vec::Vec::with_capacity(n);
                for _ in 0..n {
                    let payload: #create_ident = ::fake::Faker.fake();
                    out.push(repo.create(payload).await?);
                }
                ::core::result::Result::Ok(out)
            }
        }
    } else {
        quote! {}
    };

    // ── Server-only emission ──
    let server_block = build_server_block(
        &ident,
        &vis,
        &container,
        &parsed,
        pk_ident,
        pk_ty,
        &create_ident,
        &update_ident,
        &list_ident,
        &repo_ident,
        &storage_ident,
        &create_fields,
        &update_fields,
    );

    Ok(quote! {
        #wire_struct
        #create_struct
        #update_struct
        #list_struct
        #repo_trait
        #repo_layer
        #seed_fn
        #server_block
    })
}

#[allow(clippy::too_many_arguments)]
fn build_server_block(
    ident: &Ident,
    vis: &syn::Visibility,
    container: &ContainerAttrs,
    parsed: &[ParsedField],
    _pk_ident: &Ident,
    pk_ty: &Type,
    create_ident: &Ident,
    update_ident: &Ident,
    list_ident: &Ident,
    repo_ident: &Ident,
    storage_ident: &Ident,
    create_fields: &[&ParsedField],
    update_fields: &[&ParsedField],
) -> TokenStream2 {
    let table_name = container
        .table_name
        .clone()
        .unwrap_or_else(|| ident.to_string().to_snake_case());
    let storage_mod = format_ident!("__{}_storage", ident.to_string().to_snake_case());
    // The repo's Layer token, emitted at parent scope in `expand`.
    let layer_token = format_ident!("{}Layer", repo_ident);

    // Model fields with sea_orm attrs.
    let model_fields = parsed.iter().map(|f| {
        let id = f.ident;
        let ty = f.ty;
        let mut sea_attrs: Vec<TokenStream2> = Vec::new();
        if f.attrs.primary_key {
            let auto = f.attrs.auto_increment.unwrap_or(false);
            if auto {
                sea_attrs.push(quote! { primary_key });
            } else {
                sea_attrs.push(quote! { primary_key, auto_increment = false });
            }
        }
        if f.attrs.json {
            sea_attrs.push(quote! { column_type = "Json" });
        }
        let sea = if sea_attrs.is_empty() {
            quote! {}
        } else {
            quote! { #[sea_orm(#(#sea_attrs),*)] }
        };
        quote! { #sea pub #id: #ty }
    });

    // From<Model> for wire.
    let from_field_assigns = parsed.iter().map(|f| {
        let id = f.ident;
        quote! { #id: m.#id }
    });

    // ActiveModel field assignments for create.
    let create_active_assigns = parsed.iter().map(|f| {
        let id = f.ident;
        if let Some(e) = &f.attrs.on_create {
            quote! { #id: ::sea_orm::Set(#e) }
        } else if f.attrs.exclude_create {
            quote! { #id: ::sea_orm::NotSet }
        } else {
            quote! { #id: ::sea_orm::Set(input.#id) }
        }
    });

    // ActiveModel field assignments for update.
    let update_active_assigns = update_fields.iter().map(|f| {
        let id = f.ident;
        quote! {
            if let ::core::option::Option::Some(v) = input.#id {
                am.#id = ::sea_orm::Set(v);
            }
        }
    });

    let touch_updated = parsed
        .iter()
        .filter(|f| f.attrs.on_update.is_some())
        .map(|f| {
            let id = f.ident;
            let e = f.attrs.on_update.as_ref().unwrap();
            quote! { am.#id = ::sea_orm::Set(#e); }
        });

    let _ = (create_fields, list_ident, repo_ident);

    // Sort match arms — emit one arm per `sortable` field, mapping the
    // wire-facing snake_case field name to its SeaORM Column variant.
    let sort_arms = parsed.iter().filter(|f| f.attrs.sortable).map(|f| {
        let id = f.ident;
        let field_name = id.to_string();
        let col_variant = format_ident!("{}", id.to_string().to_pascal_case());
        quote! {
            #field_name => {
                query = match order {
                    ::architect::SortOrder::Asc => query.order_by(Column::#col_variant, Order::Asc),
                    ::architect::SortOrder::Desc => query.order_by(Column::#col_variant, Order::Desc),
                };
            }
        }
    });

    quote! {
        #[cfg(feature = "server")]
        #[doc(hidden)]
        pub mod #storage_mod {
            // User's types (Uuid, DateTime<Utc>, etc.) come via super::*.
            // We deliberately avoid `use ::sea_orm::entity::prelude::*`
            // because it re-exports DateTime/etc and collides with the
            // user's chrono import.
            use super::*;
            use ::sea_orm::{
                ActiveModelBehavior, ActiveModelTrait, DbErr,
                DeriveEntityModel, DerivePrimaryKey, DeriveRelation,
                EntityTrait, EnumIter, IntoActiveModel, Order,
                PaginatorTrait, PrimaryKeyTrait, PrimaryKeyToColumn,
                QueryOrder, Set,
            };

            #[cfg_attr(feature = "fake", derive(::architect::fake::Dummy))]
            #[derive(Clone, Debug, PartialEq, ::sea_orm::DeriveEntityModel)]
            #[sea_orm(table_name = #table_name)]
            pub struct Model {
                #(#model_fields,)*
            }

            #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
            pub enum Relation {}

            impl ActiveModelBehavior for ActiveModel {}

            impl ::core::convert::From<Model> for #ident {
                fn from(m: Model) -> Self {
                    Self {
                        #(#from_field_assigns,)*
                    }
                }
            }

            #[derive(Clone)]
            pub struct #storage_ident<C: ::architect::storage::DbConn> {
                db: C,
            }

            impl<C: ::architect::storage::DbConn> #storage_ident<C> {
                pub fn new(db: C) -> Self { Self { db } }
            }

            fn map_err(e: ::sea_orm::DbErr) -> ::architect::RepoError {
                ::architect::RepoError::Internal(e.to_string())
            }

            impl<C: ::architect::storage::DbConn> super::#repo_ident for #storage_ident<C> {
                async fn get(&self, id: #pk_ty)
                    -> ::core::result::Result<#ident, ::architect::RepoError>
                {
                    Entity::find_by_id(id).one(&self.db).await
                        .map_err(map_err)?
                        .map(#ident::from)
                        .ok_or(::architect::RepoError::NotFound)
                }

                async fn list(
                    &self,
                    page: ::architect::Page,
                    sort: ::core::option::Option<::architect::Sort>,
                    _filter: ::core::option::Option<::architect::Filter>,
                ) -> ::core::result::Result<#list_ident, ::architect::RepoError> {
                    // Filter is intentionally ignored for now — the
                    // wire-side `Filter { raw: String }` is a placeholder
                    // for the structured predicate AST that lands when
                    // the macro grows column-typed filtering.
                    let mut query = Entity::find();
                    if let ::core::option::Option::Some(s) = sort.as_ref() {
                        let order = s.order;
                        match s.field.as_str() {
                            #(#sort_arms,)*
                            other => {
                                return ::core::result::Result::Err(
                                    ::architect::RepoError::InvalidInput(
                                        ::std::format!("unsortable field: {}", other)
                                    )
                                );
                            }
                        }
                    }
                    let size = page.size.max(1) as u64;
                    let p = query.paginate(&self.db, size);
                    let total = p.num_items().await.map_err(map_err)? as u32;
                    let items = p.fetch_page(page.index as u64).await
                        .map_err(map_err)?
                        .into_iter()
                        .map(#ident::from)
                        .collect();
                    Ok(#list_ident { items, total, page })
                }

                async fn create(&self, input: #create_ident)
                    -> ::core::result::Result<#ident, ::architect::RepoError>
                {
                    let am = ActiveModel {
                        #(#create_active_assigns,)*
                    };
                    let m = am.insert(&self.db).await.map_err(map_err)?;
                    Ok(#ident::from(m))
                }

                async fn update(&self, id: #pk_ty, input: #update_ident)
                    -> ::core::result::Result<#ident, ::architect::RepoError>
                {
                    let existing = Entity::find_by_id(id).one(&self.db).await
                        .map_err(map_err)?
                        .ok_or(::architect::RepoError::NotFound)?;
                    let mut am: ActiveModel = existing.into();
                    #(#update_active_assigns)*
                    #(#touch_updated)*
                    let m = am.update(&self.db).await.map_err(map_err)?;
                    Ok(#ident::from(m))
                }

                async fn delete(&self, id: #pk_ty)
                    -> ::core::result::Result<(), ::architect::RepoError>
                {
                    let res = Entity::delete_by_id(id).exec(&self.db).await
                        .map_err(map_err)?;
                    if res.rows_affected == 0 {
                        return ::core::result::Result::Err(::architect::RepoError::NotFound);
                    }
                    ::core::result::Result::Ok(())
                }
            }

        }

        // Only the `<Ident>RepoStorage` type leaks into the parent
        // module. The SeaORM `Model` / `Entity` / `Column` / `Relation`
        // / `ActiveModel` items stay inside `__<snake>_storage` —
        // re-exporting them by their bare names collides when multiple
        // `#[derive(Entity)]` structs share a module. Code that wants
        // to reach into them (rare; the repo trait covers normal use)
        // can name `__<snake>_storage::Entity` directly.
        #[cfg(feature = "server")]
        #vis use #storage_mod::#storage_ident;

        // The generated storage backend is the canonical single-service
        // backend: it provides exactly the repo. Declaring its `Services`
        // bundle lets `<Entity>RepoStorage::new(db).into_router()` work
        // out of the box, swappable with any other repo backend.
        #[cfg(all(feature = "server", feature = "vox"))]
        impl<C: ::architect::storage::DbConn> ::architect::Services for #storage_ident<C> {
            fn layers() -> impl ::architect::Layer<Self> {
                ::architect::layers![#layer_token]
            }
        }
    }
}

// ── `#[derive(JsonField)]` — JSON-column support ─────────────────────
//
// Companion derive for any `enum` or `struct` that appears in an
// `#[derive(Entity)]` field carrying `#[architect(json)]`. SeaORM's
// `From<T> for sea_orm::Value`, `TryGetable`, `ValueType`, and
// `Nullable` are the four traits a column type must satisfy; this
// derive emits all four (cfg-gated on the user's `server` feature)
// via a `serde_json` round-trip.
//
// Vec<T> columns can't use this directly — orphan rules forbid
// `impl From<Vec<X>> for sea_orm::Value`. Wrap in a newtype:
//
// ```ignore
// #[derive(architect::JsonField, facet::Facet, Clone, Debug, PartialEq,
//          serde::Serialize, serde::Deserialize)]
// pub struct LineItems(pub Vec<InvoiceLineItem>);
// ```
//
// Requirements on the annotated type:
// - `serde::Serialize + serde::de::DeserializeOwned` (the trip is
//   `serde_json::to_value` / `serde_json::from_value`).
// - The user's crate must depend on `serde_json` for the emitted
//   code to compile under the `server` feature.
#[proc_macro_derive(JsonField)]
pub fn derive_json_field(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;
    let (impl_g, ty_g, where_g) = input.generics.split_for_impl();
    let type_name = ident.to_string();

    quote! {
        #[cfg(feature = "server")]
        const _: () = {
            extern crate serde_json as __serde_json;

            impl #impl_g ::core::convert::From<#ident #ty_g> for ::sea_orm::Value #where_g {
                fn from(v: #ident #ty_g) -> Self {
                    ::sea_orm::Value::Json(
                        __serde_json::to_value(&v).ok().map(::std::boxed::Box::new),
                    )
                }
            }

            impl #impl_g ::sea_orm::TryGetable for #ident #ty_g #where_g {
                fn try_get_by<I: ::sea_orm::ColIdx>(
                    res: &::sea_orm::QueryResult,
                    idx: I,
                ) -> ::core::result::Result<Self, ::sea_orm::TryGetError> {
                    let v: __serde_json::Value =
                        <__serde_json::Value as ::sea_orm::TryGetable>::try_get_by(res, idx)?;
                    __serde_json::from_value(v).map_err(|e| {
                        ::sea_orm::TryGetError::DbErr(::sea_orm::DbErr::Json(e.to_string()))
                    })
                }
            }

            impl #impl_g ::sea_orm::sea_query::ValueType for #ident #ty_g #where_g {
                fn try_from(
                    v: ::sea_orm::Value,
                ) -> ::core::result::Result<Self, ::sea_orm::sea_query::ValueTypeErr> {
                    match v {
                        ::sea_orm::Value::Json(::core::option::Option::Some(b)) => {
                            __serde_json::from_value::<Self>(*b)
                                .map_err(|_| ::sea_orm::sea_query::ValueTypeErr)
                        }
                        _ => ::core::result::Result::Err(::sea_orm::sea_query::ValueTypeErr),
                    }
                }

                fn type_name() -> ::std::string::String {
                    ::std::string::String::from(#type_name)
                }

                fn array_type() -> ::sea_orm::sea_query::ArrayType {
                    ::sea_orm::sea_query::ArrayType::Json
                }

                fn column_type() -> ::sea_orm::sea_query::ColumnType {
                    ::sea_orm::sea_query::ColumnType::Json
                }
            }

            impl #impl_g ::sea_orm::sea_query::Nullable for #ident #ty_g #where_g {
                fn null() -> ::sea_orm::Value {
                    ::sea_orm::Value::Json(::core::option::Option::None)
                }
            }
        };
    }
    .into()
}
