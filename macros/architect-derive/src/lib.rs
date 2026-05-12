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
    parse_macro_input, Data, DeriveInput, Expr, Field, Fields, Ident, LitBool, LitStr, Result,
    Type,
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
                ))
            }
        },
        _ => {
            return Err(syn::Error::new_spanned(
                &ident,
                "architect::Entity requires a struct",
            ))
        }
    };

    let mut parsed: Vec<ParsedField> = Vec::with_capacity(named.len());
    for f in named.iter() {
        let attrs = parse_field_attrs(f)?;
        parsed.push(ParsedField {
            ident: f.ident.as_ref().unwrap(),
            ty: &f.ty,
            attrs,
        });
    }

    let pk = parsed
        .iter()
        .find(|f| f.attrs.primary_key)
        .ok_or_else(|| syn::Error::new_spanned(&ident, "architect::Entity needs a #[architect(primary_key)] field"))?;
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
        quote! { pub #id: #ty }
    });
    let create_struct = quote! {
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
        #[derive(Clone, Debug, PartialEq, ::architect::facet::Facet, Default)]
        #vis struct #update_ident {
            #(#update_field_defs,)*
        }
    };

    // ── List payload ──
    let list_struct = quote! {
        #[derive(Clone, Debug, PartialEq, ::architect::facet::Facet)]
        #vis struct #list_ident {
            pub items: ::std::vec::Vec<#ident>,
            pub total: u32,
            pub page: ::architect::Page,
        }
    };

    // ── Repo trait ──
    let repo_trait = if container.emit_repo {
        quote! {
            #[::vox::service]
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
        mod #storage_mod {
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

        #[cfg(feature = "server")]
        #vis use #storage_mod::{Model, Entity, Column, Relation, ActiveModel, #storage_ident};
    }
}
