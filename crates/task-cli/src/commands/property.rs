//! `task prop` — Obsidian-style polymorphic property management.

#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;
use clap::{Subcommand, ValueEnum};
use task_core::service::PropertyService;

#[derive(Subcommand)]
pub(crate) enum PropertyCommands {
    /// Register a property definition.
    Define {
        name: String,
        /// Kind: text|number|checkbox|date|datetime|list|tags
        #[arg(long)]
        kind: String,
        #[arg(long)]
        description: Option<String>,
        /// JSON-encoded options blob (dropdown choices, format hints…)
        #[arg(long)]
        options: Option<String>,
    },
    /// List every registered property definition.
    ListDefinitions {
        #[arg(long)]
        json: bool,
    },
    /// Set a property on an entity. Argument is `name=value` (value parsed
    /// according to the registered kind; defaults to text).
    Set {
        reference: String,
        assignment: String,
        /// Owner kind: task (default), project, person, calendar_event,
        /// comment, location.
        #[arg(long, default_value = "task")]
        r#type: String,
    },
    /// Remove a property from an entity.
    Unset {
        reference: String,
        name: String,
        #[arg(long, default_value = "task")]
        r#type: String,
    },
    /// List all properties on an entity.
    List {
        reference: String,
        #[arg(long, default_value = "task")]
        r#type: String,
        #[arg(long)]
        json: bool,
    },
    /// Find every entity whose property matches a `name=value` assignment.
    Find {
        assignment: String,
        #[arg(long, default_value = "task")]
        r#type: String,
        #[arg(long)]
        json: bool,
    },
}

pub(crate) async fn run_remote_property_command(
    remote: &RemoteVoxConfig,
    command: PropertyCommands,
) -> eyre::Result<()> {
    let client = remote.property().await?;
    match command {
        PropertyCommands::Define {
            name,
            kind,
            description,
            options,
        } => {
            let view = client
                .define_property(
                    name.clone(),
                    kind.clone(),
                    description,
                    options.unwrap_or_default(),
                )
                .await
                .map_err(|e| eyre::eyre!("define failed: {e}"))?;
            println!(
                "Defined property '{}' (kind={}, id={}).",
                view.name, view.kind, view.id
            );
        }
        PropertyCommands::ListDefinitions { json } => {
            let defs = client
                .list_definitions()
                .await
                .map_err(|e| eyre::eyre!("list failed: {e}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&serde_json::json!(
                        defs.iter()
                            .map(|d| serde_json::json!({
                                "id": d.id,
                                "name": d.name,
                                "kind": d.kind,
                                "description": d.description,
                                "options": serde_json::from_str::<serde_json::Value>(&d.options)
                                    .unwrap_or(serde_json::Value::Null),
                                "created_at": d.created_at,
                                "updated_at": d.updated_at,
                            }))
                            .collect::<Vec<_>>()
                    ))?
                );
            } else if defs.is_empty() {
                println!("(no property definitions registered)");
            } else {
                for def in defs {
                    println!(
                        "{:<24} {:<10} {}",
                        def.name,
                        def.kind,
                        def.description.unwrap_or_default()
                    );
                }
            }
        }
        PropertyCommands::Set {
            reference,
            assignment,
            r#type,
        } => {
            let (name, raw_value) = split_assignment(&assignment)?;
            let owner_id = resolve_owner_id(remote, &r#type, &reference).await?;
            // Resolve kind via existing definition (auto-create if missing).
            let kind = lookup_definition_kind(&client, name).await;
            let json_value = encode_value(&kind, raw_value)?;
            client
                .set_property(
                    r#type,
                    owner_id,
                    name.to_string(),
                    serde_json::to_string(&json_value)?,
                )
                .await
                .map_err(|e| eyre::eyre!("set failed: {e}"))?;
            println!("Set {name} = {raw_value} on {reference}.");
        }
        PropertyCommands::Unset {
            reference,
            name,
            r#type,
        } => {
            let owner_id = resolve_owner_id(remote, &r#type, &reference).await?;
            client
                .unset_property(r#type, owner_id, name.clone())
                .await
                .map_err(|e| eyre::eyre!("unset failed: {e}"))?;
            println!("Removed {name} from {reference}.");
        }
        PropertyCommands::List {
            reference,
            r#type,
            json,
        } => {
            let owner_id = resolve_owner_id(remote, &r#type, &reference).await?;
            let raw = client
                .get_properties(r#type, owner_id)
                .await
                .map_err(|e| eyre::eyre!("get failed: {e}"))?;
            let value: serde_json::Value = serde_json::from_str(&raw)?;
            if json {
                println!("{}", serde_json::to_string_pretty(&value)?);
            } else if let Some(map) = value.as_object() {
                if map.is_empty() {
                    println!("(no properties)");
                } else {
                    for (k, v) in map {
                        println!("{k} = {v}");
                    }
                }
            } else {
                println!("{value}");
            }
        }
        PropertyCommands::Find {
            assignment,
            r#type,
            json,
        } => {
            let (name, raw_value) = split_assignment(&assignment)?;
            let kind = lookup_definition_kind(&client, name).await;
            let json_value = encode_value(&kind, raw_value)?;
            let ids = client
                .find_by_property(
                    r#type,
                    name.to_string(),
                    serde_json::to_string(&json_value)?,
                )
                .await
                .map_err(|e| eyre::eyre!("find failed: {e}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(
                        &ids.iter().map(|u| u.to_string()).collect::<Vec<_>>()
                    )?
                );
            } else if ids.is_empty() {
                println!("(no matches)");
            } else {
                for id in ids {
                    println!("{id}");
                }
            }
        }
    }
    Ok(())
}

fn split_assignment(s: &str) -> eyre::Result<(&str, &str)> {
    s.split_once('=')
        .ok_or_else(|| eyre::eyre!("expected `name=value`, got `{s}`"))
}

async fn lookup_definition_kind(
    client: &task_core::service::PropertyServiceClient,
    name: &str,
) -> String {
    match client.list_definitions().await {
        Ok(defs) => defs
            .into_iter()
            .find(|d| d.name == name)
            .map(|d| d.kind)
            .unwrap_or_else(|| "text".to_string()),
        Err(_) => "text".to_string(),
    }
}

/// Encode a CLI string value into a JSON value matching the property kind.
fn encode_value(kind: &str, raw: &str) -> eyre::Result<serde_json::Value> {
    let parsed = match kind {
        "number" => serde_json::Value::Number(
            raw.parse::<i64>()
                .ok()
                .map(serde_json::Number::from)
                .or_else(|| {
                    raw.parse::<f64>()
                        .ok()
                        .and_then(serde_json::Number::from_f64)
                })
                .ok_or_else(|| eyre::eyre!("invalid number: {raw}"))?,
        ),
        "checkbox" => serde_json::Value::Bool(matches!(
            raw.to_ascii_lowercase().as_str(),
            "1" | "true" | "yes" | "y" | "on"
        )),
        "list" | "tags" => {
            // Comma-separated convenience or JSON array passthrough.
            if raw.starts_with('[') {
                serde_json::from_str(raw)?
            } else {
                serde_json::Value::Array(
                    raw.split(',')
                        .map(|s| serde_json::Value::String(s.trim().to_string()))
                        .filter(|v| !matches!(v, serde_json::Value::String(s) if s.is_empty()))
                        .collect(),
                )
            }
        }
        // text / date / datetime / unknown → string verbatim.
        _ => serde_json::Value::String(raw.to_string()),
    };
    Ok(parsed)
}

async fn resolve_owner_id(
    remote: &RemoteVoxConfig,
    owner_type: &str,
    reference: &str,
) -> eyre::Result<uuid::Uuid> {
    // Plain UUID? short-circuit.
    if let Ok(id) = uuid::Uuid::parse_str(reference) {
        return Ok(id);
    }
    match owner_type {
        "task" => {
            let task = remote_find_task(remote, reference).await?;
            Ok(task.id)
        }
        "project" => {
            let client = remote.project_repo().await?;
            let project = remote_find_project_with_client(&client, reference).await?;
            Ok(project.id)
        }
        other => Err(eyre::eyre!(
            "name-based resolution for owner_type '{other}' is not supported; pass a UUID"
        )),
    }
}
