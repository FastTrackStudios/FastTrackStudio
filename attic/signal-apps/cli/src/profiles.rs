//! `profiles` subcommand handlers — split from cli/lib.rs.

use super::*;

pub(crate) async fn cmd_profiles_list(signal: &SignalController, as_json: bool) -> Result<()> {
    let profiles = signal.profiles().list().await?;

    if as_json {
        let arr: Vec<_> = profiles
            .iter()
            .map(|p| {
                json!({
                    "id": p.id.to_string(),
                    "name": p.name,
                    "patch_count": p.patches.len(),
                    "default_patch_id": p.default_patch_id.to_string(),
                })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&arr)?);
    } else {
        if profiles.is_empty() {
            println!("No profiles.");
            return Ok(());
        }
        println!("Profiles ({}):", profiles.len());
        for p in &profiles {
            println!("  {} — {} ({} patches)", p.id, p.name, p.patches.len());
        }
    }
    Ok(())
}

pub(crate) async fn cmd_profiles_show(signal: &SignalController, id: &str, as_json: bool) -> Result<()> {
    let profile = signal.profiles().load(id.to_string()).await?;
    match profile {
        Some(p) => {
            if as_json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&json!({
                        "id": p.id.to_string(),
                        "name": p.name,
                        "default_patch_id": p.default_patch_id.to_string(),
                        "patches": p.patches.iter().map(|patch| json!({
                            "id": patch.id.to_string(),
                            "name": patch.name,
                            "target": format!("{:?}", patch.target),
                        })).collect::<Vec<_>>(),
                    }))?
                );
            } else {
                println!("Profile: {} ({})", p.name, p.id);
                println!("  Default patch: {}", p.default_patch_id);
                for patch in &p.patches {
                    let is_default = patch.id == p.default_patch_id;
                    println!(
                        "  {} {} — {}{}",
                        if is_default { "*" } else { " " },
                        patch.id,
                        patch.name,
                        if is_default { " (default)" } else { "" },
                    );
                }
            }
        }
        None => eyre::bail!("Profile not found: {id}"),
    }
    Ok(())
}

pub(crate) async fn cmd_profiles_activate(
    signal: &SignalController,
    id: &str,
    patch: Option<&str>,
    as_json: bool,
) -> Result<()> {
    let patch_id = patch.map(|p| PatchId::from(p.to_string()));
    let graph = signal
        .profiles()
        .activate(id.to_string(), patch_id)
        .await
        .map_err(|e| eyre::eyre!("Failed to activate: {e:?}"))?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "activate",
                "profile_id": id,
                "patch_id": patch,
                "graph": format!("{graph:?}"),
                "ok": true,
            }))?
        );
    } else {
        println!(
            "activated profile {} patch {:?}",
            id,
            patch.unwrap_or("(default)")
        );
    }
    Ok(())
}

// ============================================================================
// Command Implementations — Patches
// ============================================================================

