//! `patches` subcommand handlers — split from cli/lib.rs.

use super::*;

pub(crate) async fn cmd_patches_list(
    signal: &SignalController,
    profile_id: &str,
    as_json: bool,
) -> Result<()> {
    let profile = signal
        .profiles()
        .load(profile_id.to_string())
        .await?
        .ok_or_else(|| eyre::eyre!("Profile not found: {profile_id}"))?;

    if as_json {
        let arr: Vec<_> = profile
            .patches
            .iter()
            .map(|p| {
                json!({
                    "id": p.id.to_string(),
                    "name": p.name,
                    "is_default": p.id == profile.default_patch_id,
                    "target": format!("{:?}", p.target),
                })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&arr)?);
    } else {
        if profile.patches.is_empty() {
            println!("No patches in profile \"{}\".", profile.name);
            return Ok(());
        }
        println!(
            "Patches in \"{}\" ({}):",
            profile.name,
            profile.patches.len()
        );
        for p in &profile.patches {
            let is_default = p.id == profile.default_patch_id;
            println!(
                "  {} {} — {}",
                if is_default { "*" } else { " " },
                p.id,
                p.name,
            );
        }
    }
    Ok(())
}

pub(crate) async fn cmd_patches_add(
    signal: &SignalController,
    profile_id: &str,
    name: &str,
    as_json: bool,
) -> Result<()> {
    let profile = signal
        .profiles()
        .load(profile_id.to_string())
        .await?
        .ok_or_else(|| eyre::eyre!("Profile not found: {profile_id}"))?;

    let template_target = profile
        .patches
        .first()
        .map(|p| p.target.clone())
        .ok_or_else(|| eyre::eyre!("Profile has no patches to use as template"))?;

    let patch = Patch {
        id: PatchId::from(uuid::Uuid::new_v4().to_string()),
        name: name.to_string(),
        target: template_target,
        overrides: vec![],
        metadata: Default::default(),
    };

    let result = signal
        .profiles()
        .try_add_patch(profile_id.to_string(), patch)
        .await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "add_patch",
                "profile_id": profile_id,
                "name": name,
                "patch_count": result.patches.len(),
                "ok": true,
            }))?
        );
    } else {
        println!("added patch \"{}\" to \"{}\"", name, result.name);
    }
    Ok(())
}

pub(crate) async fn cmd_patches_remove(
    signal: &SignalController,
    profile_id: &str,
    patch_id: &str,
    as_json: bool,
) -> Result<()> {
    let removed = signal
        .profiles()
        .try_remove_patch(profile_id.to_string(), patch_id.to_string())
        .await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "remove_patch",
                "profile_id": profile_id,
                "patch_id": patch_id,
                "removed_name": removed.name,
                "ok": true,
            }))?
        );
    } else {
        println!("removed patch \"{}\"", removed.name);
    }
    Ok(())
}

// ============================================================================
// Command Implementations — Browse
// ============================================================================

pub(crate) async fn cmd_browse(signal: &SignalController, query: &str, as_json: bool) -> Result<()> {
    let results = signal
        .browse(signal::tagging::BrowserQuery {
            text: Some(query.to_string()),
            ..Default::default()
        })
        .await?;

    if as_json {
        let arr: Vec<_> = results
            .iter()
            .map(|h| {
                json!({
                    "kind": format!("{:?}", h.node.kind),
                    "id": h.node.id,
                    "score": h.score,
                })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&arr)?);
    } else {
        if results.is_empty() {
            println!("No results for \"{}\".", query);
            return Ok(());
        }
        println!("Results for \"{}\" ({}):", query, results.len());
        for h in &results {
            println!(
                "  [{:?}] {} (score: {:.2})",
                h.node.kind, h.node.id, h.score
            );
        }
    }
    Ok(())
}

// ============================================================================
// Command Implementations — Songs (signal-level)
// ============================================================================

