//! `task glossary` — workflow-agnostic term catalog plus
//! `[[wikilink]]` resolution helper.

#![allow(clippy::needless_lifetimes)]

use crate::*;
use clap::Subcommand;
use task_core::service::{
    CreateGlossaryTermRequest, GlossaryServiceClient, GlossaryTermPatch, ResolveInTextRequest,
};
use uuid::Uuid;

#[derive(Subcommand)]
pub(crate) enum GlossaryCommands {
    /// List glossary terms (optionally filter by org/category).
    List {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show a term by uuid, slug, name, or alias.
    Show {
        term: String,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Create a glossary term.
    Create {
        #[arg(long)]
        name: String,
        #[arg(long)]
        slug: Option<String>,
        #[arg(long, default_value = "general")]
        category: String,
        #[arg(long, default_value = "")]
        body: String,
        /// Repeatable; pass `--alias foo --alias bar`.
        #[arg(long = "alias")]
        aliases: Vec<String>,
        #[arg(long)]
        organization: Option<String>,
    },
    /// Update fields on an existing term.
    Update {
        id: Uuid,
        #[arg(long)]
        body: Option<String>,
        /// Repeatable; appended via `add_alias` (idempotent).
        #[arg(long = "add-alias")]
        add_alias: Vec<String>,
        /// Repeatable list of related term ids.
        #[arg(long = "related")]
        related: Vec<Uuid>,
        #[arg(long)]
        category: Option<String>,
    },
    /// Delete a term.
    Delete { id: Uuid },
    /// Add an alias to a term.
    Alias {
        /// UUID, slug, name, or alias hit.
        term: String,
        alias: String,
        #[arg(long)]
        organization: Option<String>,
    },
    /// Resolve `[[wikilink]]`s in arbitrary text against the catalog.
    Resolve {
        #[arg(long)]
        text: String,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

pub(crate) async fn run_remote_glossary_command(
    remote: &RemoteVoxConfig,
    command: GlossaryCommands,
) -> eyre::Result<()> {
    let client = remote.glossary().await?;
    match command {
        GlossaryCommands::List {
            organization,
            category,
            json,
        } => {
            let terms = client
                .list_terms(organization, category)
                .await
                .map_err(|e| eyre::eyre!("list_terms failed: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&terms)?);
            } else if terms.is_empty() {
                println!("(no glossary terms)");
            } else {
                println!("{:<28} {:<20} SLUG", "NAME", "CATEGORY");
                for t in terms {
                    println!("{:<28} {:<20} {}", t.name, t.category, t.slug);
                }
            }
        }
        GlossaryCommands::Show {
            term,
            organization,
            json,
        } => {
            let resolved = resolve_term(&client, organization.clone(), None, &term).await?;
            if json {
                println!("{}", serde_json::to_string_pretty(&resolved)?);
            } else {
                println!("{} ({})", resolved.name, resolved.category);
                println!("slug: {}", resolved.slug);
                if !resolved.aliases.is_empty() {
                    println!("aliases: {}", resolved.aliases.join(", "));
                }
                println!();
                println!("{}", resolved.body_markdown);
            }
        }
        GlossaryCommands::Create {
            name,
            slug,
            category,
            body,
            aliases,
            organization,
        } => {
            let req = CreateGlossaryTermRequest {
                name,
                slug,
                body_markdown: body,
                aliases,
                category,
                related_term_ids: Vec::new(),
                organization,
                created_by: None,
            };
            let term = client
                .create_term(req)
                .await
                .map_err(|e| eyre::eyre!("create_term failed: {e}"))?;
            println!("Created '{}' ({}, id={}).", term.name, term.slug, term.id);
        }
        GlossaryCommands::Update {
            id,
            body,
            add_alias,
            related,
            category,
        } => {
            let patch = GlossaryTermPatch {
                name: None,
                slug: None,
                body_markdown: body,
                aliases: None, // handled separately so partial updates compose
                category,
                related_term_ids: if related.is_empty() {
                    None
                } else {
                    Some(related)
                },
            };
            let mut term = client
                .update_term(id, patch)
                .await
                .map_err(|e| eyre::eyre!("update_term failed: {e}"))?;
            for alias in add_alias {
                term = client
                    .add_alias(id, alias)
                    .await
                    .map_err(|e| eyre::eyre!("add_alias failed: {e}"))?;
            }
            println!("Updated {} ({}).", term.name, term.id);
        }
        GlossaryCommands::Delete { id } => {
            client
                .delete_term(id)
                .await
                .map_err(|e| eyre::eyre!("delete_term failed: {e}"))?;
            println!("Deleted {id}.");
        }
        GlossaryCommands::Alias {
            term,
            alias,
            organization,
        } => {
            let resolved = resolve_term(&client, organization, None, &term).await?;
            let updated = client
                .add_alias(resolved.id, alias.clone())
                .await
                .map_err(|e| eyre::eyre!("add_alias failed: {e}"))?;
            println!(
                "Added alias '{alias}' to '{}' (now: {}).",
                updated.name,
                updated.aliases.join(", ")
            );
        }
        GlossaryCommands::Resolve {
            text,
            category,
            organization,
            json,
        } => {
            let view = client
                .resolve_in_text(ResolveInTextRequest {
                    text: text.clone(),
                    organization,
                    category,
                })
                .await
                .map_err(|e| eyre::eyre!("resolve_in_text failed: {e}"))?;
            if json {
                let parsed: serde_json::Value =
                    serde_json::from_str(&view.spans_json).unwrap_or(serde_json::Value::Null);
                println!(
                    "{}",
                    serde_json::to_string_pretty(&serde_json::json!({
                        "spans": parsed,
                        "resolved_term_ids": view.resolved_term_ids,
                    }))?
                );
            } else {
                let parsed: serde_json::Value =
                    serde_json::from_str(&view.spans_json).unwrap_or(serde_json::Value::Null);
                if let Some(arr) = parsed.as_array() {
                    if arr.is_empty() {
                        println!("(no wikilinks found)");
                    } else {
                        for span in arr {
                            let slug = span
                                .get("span")
                                .and_then(|s| s.get("slug"))
                                .and_then(serde_json::Value::as_str)
                                .unwrap_or("?");
                            let target = span.get("target_id");
                            let label = span
                                .get("term_summary")
                                .and_then(|s| s.get("name"))
                                .and_then(serde_json::Value::as_str)
                                .unwrap_or(slug);
                            match target {
                                Some(serde_json::Value::String(id)) => {
                                    println!("[[{slug}]] → {label} ({id})");
                                }
                                _ => println!("[[{slug}]] → (unresolved)"),
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

/// Resolve a CLI `<term>` argument: UUID → slug → name → first alias hit.
async fn resolve_term(
    client: &GlossaryServiceClient,
    organization: Option<String>,
    category: Option<String>,
    needle: &str,
) -> eyre::Result<task_core::glossary::GlossaryTermApi> {
    if let Ok(id) = Uuid::parse_str(needle) {
        if let Some(t) = client
            .get_term(id)
            .await
            .map_err(|e| eyre::eyre!("get_term failed: {e}"))?
        {
            return Ok(t);
        }
    }
    if let Some(t) = client
        .find_term_by_slug_or_alias(organization, category, needle.to_string())
        .await
        .map_err(|e| eyre::eyre!("find_term failed: {e}"))?
    {
        return Ok(t);
    }
    Err(eyre::eyre!("no glossary term matches '{needle}'"))
}
