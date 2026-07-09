//! `rigs` subcommand handlers — split from cli/lib.rs.

use super::*;

pub(crate) async fn cmd_rigs_list(signal: &SignalController, as_json: bool) -> Result<()> {
    let rigs = signal.rigs().list().await?;

    if as_json {
        let arr: Vec<_> = rigs
            .iter()
            .map(|r| {
                json!({
                    "id": r.id.to_string(),
                    "name": r.name,
                    "engine_count": r.engine_ids.len(),
                    "scene_count": r.variants.len(),
                })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&arr)?);
    } else {
        if rigs.is_empty() {
            println!("No rigs.");
            return Ok(());
        }
        println!("Rigs ({}):", rigs.len());
        for r in &rigs {
            println!(
                "  {} — {} ({} engines, {} scenes)",
                r.id,
                r.name,
                r.engine_ids.len(),
                r.variants.len()
            );
        }
    }
    Ok(())
}

pub(crate) async fn cmd_rigs_show(signal: &SignalController, id: &str, as_json: bool) -> Result<()> {
    let rig = signal.rigs().load(id.to_string()).await?;
    match rig {
        Some(r) => {
            if as_json {
                let mut engines_json = Vec::new();
                for eid in &r.engine_ids {
                    if let Some(engine) = signal.engines().load(eid.clone()).await? {
                        let mut layers_json = Vec::new();
                        for lid in &engine.layer_ids {
                            if let Some(layer) = signal.layers().load(lid.clone()).await? {
                                let snap = signal
                                    .layers()
                                    .load_variant(lid.clone(), layer.default_variant_id.clone())
                                    .await?;
                                let blocks: Vec<_> = if let Some(ref s) = snap {
                                    s.block_refs
                                        .iter()
                                        .map(|br| {
                                            json!({
                                                "collection_id": br.collection_id.to_string(),
                                            })
                                        })
                                        .collect()
                                } else {
                                    vec![]
                                };
                                layers_json.push(json!({
                                    "id": lid.to_string(),
                                    "name": layer.name,
                                    "block_refs": blocks,
                                }));
                            }
                        }
                        engines_json.push(json!({
                            "id": eid.to_string(),
                            "name": engine.name,
                            "layers": layers_json,
                        }));
                    }
                }
                println!(
                    "{}",
                    serde_json::to_string_pretty(&json!({
                        "id": r.id.to_string(),
                        "name": r.name,
                        "engines": engines_json,
                        "scenes": r.variants.iter().map(|v| json!({
                            "id": v.id.to_string(),
                            "name": v.name,
                        })).collect::<Vec<_>>(),
                    }))?
                );
            } else {
                println!("Rig: {} ({})", r.name, r.id);
                for v in &r.variants {
                    let is_default = v.id == r.default_variant_id;
                    println!("  {} Scene: {}", if is_default { "*" } else { " " }, v.name,);
                    for es in &v.engine_selections {
                        if let Some(engine) = signal.engines().load(es.engine_id.clone()).await? {
                            println!("    Engine: {} (scene: {})", engine.name, es.variant_id);
                            // Find the engine scene to get layer selections
                            if let Some(scene) =
                                engine.variants.iter().find(|s| s.id == es.variant_id)
                            {
                                for ls in &scene.layer_selections {
                                    if let Some(layer) =
                                        signal.layers().load(ls.layer_id.clone()).await?
                                    {
                                        println!(
                                            "      Layer: {} (snapshot: {})",
                                            layer.name, ls.variant_id
                                        );
                                        // Load the layer snapshot to show block refs
                                        if let Some(snap) = signal
                                            .layers()
                                            .load_variant(
                                                ls.layer_id.clone(),
                                                ls.variant_id.clone(),
                                            )
                                            .await?
                                        {
                                            for br in &snap.block_refs {
                                                let name =
                                                    lookup_preset_name(signal, &br.collection_id)
                                                        .await;
                                                println!("        Block: {}", name);
                                            }
                                        }
                                    } else {
                                        println!("      Layer: (missing: {})", ls.layer_id);
                                    }
                                }
                            }
                        } else {
                            println!("    Engine: (missing: {})", es.engine_id);
                        }
                    }
                }
            }
        }
        None => eyre::bail!("Rig not found: {id}"),
    }
    Ok(())
}

pub(crate) async fn cmd_rigs_create(signal: &SignalController, name: &str, as_json: bool) -> Result<()> {
    let rig = signal.rigs().create(name.to_string(), vec![]).await?;
    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "create_rig",
                "id": rig.id.to_string(),
                "name": rig.name,
                "ok": true,
            }))?
        );
    } else {
        println!("created rig: {} ({})", rig.name, rig.id);
    }
    Ok(())
}

pub(crate) async fn cmd_rigs_delete(signal: &SignalController, id: &str, as_json: bool) -> Result<()> {
    signal.rigs().delete(id.to_string()).await?;
    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "delete_rig",
                "id": id,
                "ok": true,
            }))?
        );
    } else {
        println!("deleted rig: {}", id);
    }
    Ok(())
}

pub(crate) async fn cmd_rigs_add_engine(
    signal: &SignalController,
    rig_id: &str,
    engine_id: &str,
    as_json: bool,
) -> Result<()> {
    let rid = signal::rig::RigId::from(rig_id.to_string());
    let eid = signal::engine::EngineId::from(engine_id.to_string());

    let mut rig = signal
        .rigs()
        .load(rid.clone())
        .await?
        .ok_or_else(|| eyre::eyre!("Rig not found: {rig_id}"))?;
    let engine = signal
        .engines()
        .load(eid.clone())
        .await?
        .ok_or_else(|| eyre::eyre!("Engine not found: {engine_id}"))?;

    rig.engine_ids.push(eid.clone());

    let selection = signal::rig::EngineSelection::new(eid, engine.default_variant_id.clone());
    for scene in &mut rig.variants {
        scene.engine_selections.push(selection.clone());
    }

    signal.rigs().save(rig).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "add_engine",
                "rig_id": rig_id,
                "engine_id": engine_id,
                "ok": true,
            }))?
        );
    } else {
        println!("added engine {} to rig {}", engine.name, rig_id);
    }
    Ok(())
}

pub(crate) async fn cmd_rigs_remove_engine(
    signal: &SignalController,
    rig_id: &str,
    engine_id: &str,
    as_json: bool,
) -> Result<()> {
    let rid = signal::rig::RigId::from(rig_id.to_string());
    let eid = signal::engine::EngineId::from(engine_id.to_string());

    let mut rig = signal
        .rigs()
        .load(rid.clone())
        .await?
        .ok_or_else(|| eyre::eyre!("Rig not found: {rig_id}"))?;

    let before = rig.engine_ids.len();
    rig.engine_ids.retain(|e| *e != eid);
    if rig.engine_ids.len() == before {
        eyre::bail!("Engine {} not found in rig {}", engine_id, rig.name);
    }

    for scene in &mut rig.variants {
        scene.engine_selections.retain(|s| s.engine_id != eid);
    }

    signal.rigs().save(rig).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "remove_engine",
                "rig_id": rig_id,
                "engine_id": engine_id,
                "ok": true,
            }))?
        );
    } else {
        println!("removed engine {} from rig {}", engine_id, rig_id);
    }
    Ok(())
}

/// Replace children within a module container by matching 1:1 by position
/// against stored raw_block state data from the rig's block presets.
///
/// Handles both Plugin and Container (sub-container) children. For plugins,
/// state_data and raw_block are transplanted. For containers, the entire node
/// is replaced wholesale (children, raw_block, and all) since REAPER's
/// `set_state_chunk` API doesn't work on container FX.
pub(crate) fn replace_by_position(
    children: &mut [daw::file::types::FxChainNode],
    block_states: &[Vec<u8>],
    replaced: &mut usize,
    skipped: &mut usize,
) {
    let mut block_idx = 0;
    for child in children.iter_mut() {
        if block_idx >= block_states.len() {
            *skipped += 1;
            continue;
        }
        let source_bytes = &block_states[block_idx];
        block_idx += 1;

        if source_bytes.is_empty() {
            *skipped += 1;
            continue;
        }

        match child {
            daw::file::types::FxChainNode::Plugin(p) => {
                if try_replace_raw_block(p, source_bytes) {
                    let display = p.custom_name.as_deref().unwrap_or(&p.name);
                    eprintln!(
                        "[state] replaced plugin '{}' ({} bytes)",
                        display,
                        source_bytes.len(),
                    );
                    *replaced += 1;
                } else {
                    *skipped += 1;
                }
            }
            daw::file::types::FxChainNode::Container(c) => {
                if try_replace_container(c, source_bytes) {
                    eprintln!(
                        "[state] replaced container '{}' ({} bytes)",
                        c.name,
                        source_bytes.len(),
                    );
                    *replaced += 1;
                } else {
                    *skipped += 1;
                }
            }
        }
    }
}

/// Parse source raw_block bytes into an FxChainNode (Plugin or Container).
pub(crate) fn parse_raw_block_bytes(source_bytes: &[u8]) -> Option<daw::file::types::FxChainNode> {
    let source_str = std::str::from_utf8(source_bytes).ok()?;
    let source_chain = daw::file::FxChain::parse(&format!(
        "<FXCHAIN\nSHOW 0\nLASTSEL 0\nDOCKED 0\n{source_str}\n>\n"
    ))
    .ok()?;
    source_chain.nodes.into_iter().next()
}

/// Parse source raw_block bytes and transplant state into a loaded plugin,
/// preserving the loaded plugin's FXID. Returns true on success.
pub(crate) fn try_replace_raw_block(plugin: &mut daw::file::types::FxPlugin, source_bytes: &[u8]) -> bool {
    if let Some(daw::file::types::FxChainNode::Plugin(source_plugin)) =
        parse_raw_block_bytes(source_bytes)
    {
        let loaded_fxid = plugin.fxid.clone();
        plugin.state_data = source_plugin.state_data.clone();
        plugin.raw_block = source_plugin.raw_block.clone();
        plugin.fxid = loaded_fxid;
        true
    } else {
        false
    }
}

/// Replace a container node's contents with data from stored raw_block bytes.
/// Preserves the loaded container's FXID but replaces children, raw_block,
/// and container_cfg from the source.
pub(crate) fn try_replace_container(
    container: &mut daw::file::types::FxContainer,
    source_bytes: &[u8],
) -> bool {
    if let Some(daw::file::types::FxChainNode::Container(source_container)) =
        parse_raw_block_bytes(source_bytes)
    {
        let loaded_fxid = container.fxid.clone();
        container.children = source_container.children;
        container.raw_block = source_container.raw_block;
        container.container_cfg = source_container.container_cfg;
        container.fxid = loaded_fxid;
        true
    } else {
        false
    }
}

pub(crate) async fn cmd_rigs_open(
    db: Option<PathBuf>,
    socket: Option<PathBuf>,
    rig_id: &str,
    own_reaper: bool,
    close_after_load: bool,
) -> Result<()> {
    let signal = connect_signal(db).await?;

    // Load rig from DB
    let rig = signal
        .rigs()
        .load(rig_id.to_string())
        .await?
        .ok_or_else(|| eyre::eyre!("Rig not found: {rig_id}"))?;

    eprintln!("Opening rig: {} ({})", rig.name, rig.id);

    // Connect to REAPER — owned or existing
    let (daw, owned) = if own_reaper {
        let (daw, pid, sock) = daw_cli::launch_and_connect("fts-signal")
            .await
            .map_err(|e| eyre::eyre!("Failed to launch REAPER: {e}"))?;
        (daw, Some((pid, sock)))
    } else {
        let daw = daw_cli::connect(socket)
            .await
            .map_err(|e| eyre::eyre!("REAPER required for rig open: {e}"))?;
        (daw, None)
    };

    let project = daw.current_project().await?;

    // ── 1. Collect all block preset state data from DB ──
    // Index by PresetId → raw_block bytes. Also keep name/source indexes for verification.
    let mut state_by_preset_id: std::collections::HashMap<String, Vec<u8>> =
        std::collections::HashMap::new();
    let mut state_by_preset_name: std::collections::HashMap<String, Vec<u8>> =
        std::collections::HashMap::new();
    let mut source_plugin_counts: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();
    let mut state_by_source_plugin: std::collections::HashMap<String, Vec<u8>> =
        std::collections::HashMap::new();
    for &bt in signal::ALL_BLOCK_TYPES {
        if let Ok(presets) = signal.block_presets().list(bt).await {
            for preset in presets {
                if let Some(data) = preset.default_snapshot().state_data() {
                    let bytes = data.to_vec();
                    state_by_preset_id.insert(preset.id().to_string(), bytes.clone());
                    state_by_preset_name.insert(preset.name().to_string(), bytes.clone());
                    for tag in preset.metadata().tags.as_slice() {
                        if let Some(source) = tag.strip_prefix("source:") {
                            *source_plugin_counts.entry(source.to_string()).or_insert(0) += 1;
                            state_by_source_plugin.insert(source.to_string(), bytes.clone());
                        }
                    }
                }
            }
        }
    }
    for (key, count) in &source_plugin_counts {
        if *count > 1 {
            state_by_source_plugin.remove(key);
        }
    }
    eprintln!(
        "[rigs open] loaded {} block presets with state data",
        state_by_preset_id.len(),
    );

    // ── 2. Resolve rig hierarchy → per-layer module/block specs ──
    use signal::ModuleBlockSource;
    use signal::plugin_block::FxRole;

    struct BlockSpec {
        #[allow(dead_code)]
        display_name: String,
        state_data: Option<Vec<u8>>,
    }
    struct ModuleSpec {
        container_name: String,
        blocks: Vec<BlockSpec>,
    }
    struct LayerSpec {
        name: String,
        modules: Vec<ModuleSpec>,
    }
    struct EngineSpec {
        name: String,
        layers: Vec<LayerSpec>,
    }

    let all_mp = signal.module_presets().list().await.unwrap_or_default();
    let default_scene = rig
        .default_variant()
        .ok_or_else(|| eyre::eyre!("Rig has no default scene"))?;

    let mut engine_specs: Vec<EngineSpec> = Vec::new();
    for engine_sel in &default_scene.engine_selections {
        let engine = signal
            .engines()
            .load(engine_sel.engine_id.to_string())
            .await?
            .ok_or_else(|| eyre::eyre!("Engine not found: {}", engine_sel.engine_id))?;
        let engine_scene = engine
            .variant(&engine_sel.variant_id)
            .or_else(|| engine.default_variant())
            .ok_or_else(|| eyre::eyre!("No scene for engine {}", engine.name))?;

        let mut layer_specs = Vec::new();
        for layer_sel in &engine_scene.layer_selections {
            let layer = signal
                .layers()
                .load(layer_sel.layer_id.to_string())
                .await?
                .ok_or_else(|| eyre::eyre!("Layer not found: {}", layer_sel.layer_id))?;
            let layer_snap = layer
                .variant(&layer_sel.variant_id)
                .or_else(|| layer.default_variant())
                .ok_or_else(|| eyre::eyre!("No snapshot for layer {}", layer.name))?;

            let mut module_specs = Vec::new();
            for module_ref in &layer_snap.module_refs {
                if let Some(mp) = all_mp.iter().find(|p| p.id() == &module_ref.collection_id) {
                    let snap = module_ref
                        .variant_id
                        .as_ref()
                        .and_then(|vid| mp.snapshot(vid))
                        .unwrap_or_else(|| mp.default_snapshot().clone());

                    let mut blocks = Vec::new();
                    for block in snap.module().blocks() {
                        let preset_data = match block.source() {
                            ModuleBlockSource::PresetDefault { preset_id, .. }
                            | ModuleBlockSource::PresetSnapshot { preset_id, .. } => {
                                state_by_preset_id.get(&preset_id.to_string()).cloned()
                            }
                            _ => None,
                        };
                        let role = FxRole::Block {
                            block_type: block.block_type(),
                            name: block.label().to_string(),
                        };
                        blocks.push(BlockSpec {
                            display_name: role.display_name(),
                            state_data: preset_data,
                        });
                    }

                    let role = FxRole::Module {
                        module_type: mp.module_type(),
                        name: mp.name().to_string(),
                    };
                    module_specs.push(ModuleSpec {
                        container_name: role.display_name(),
                        blocks,
                    });
                }
            }
            layer_specs.push(LayerSpec {
                name: layer.name.clone(),
                modules: module_specs,
            });
        }
        engine_specs.push(EngineSpec {
            name: engine.name.clone(),
            layers: layer_specs,
        });
    }

    // ── 3. Check if fast path is viable (all blocks have state_data) ──
    let total_blocks: usize = engine_specs
        .iter()
        .flat_map(|e| &e.layers)
        .flat_map(|l| &l.modules)
        .map(|m| m.blocks.len())
        .sum();
    let blocks_with_state: usize = engine_specs
        .iter()
        .flat_map(|e| &e.layers)
        .flat_map(|l| &l.modules)
        .flat_map(|m| &m.blocks)
        .filter(|b| b.state_data.is_some())
        .count();
    let fast_path = blocks_with_state == total_blocks && total_blocks > 0;

    // Track layer tracks for verification (both paths populate this).
    let mut layer_track_guids: Vec<String> = Vec::new();

    if fast_path {
        // ── 4a. FAST PATH: build FXCHAIN from raw_blocks, single set_chunk ──
        eprintln!(
            "[rigs open] fast path: building FXCHAIN from {} stored chunks",
            blocks_with_state,
        );

        use signal::plugin_block::TrackRole;

        // Create track hierarchy: [R] → [E] → [L]
        let rig_track = project
            .tracks()
            .add(
                &TrackRole::Rig {
                    name: rig.name.clone(),
                }
                .display_name(),
                None,
            )
            .await?;
        rig_track.set_folder_depth(1).await?;

        let engine_count = engine_specs.len();
        for (ei, engine) in engine_specs.iter().enumerate() {
            let engine_track = project
                .tracks()
                .add(
                    &TrackRole::Engine {
                        name: engine.name.clone(),
                    }
                    .display_name(),
                    None,
                )
                .await?;
            engine_track.set_folder_depth(1).await?;

            let layer_count = engine.layers.len();
            for (li, layer) in engine.layers.iter().enumerate() {
                let layer_track = project
                    .tracks()
                    .add(
                        &TrackRole::Layer {
                            name: layer.name.clone(),
                        }
                        .display_name(),
                        None,
                    )
                    .await?;

                // Close folders: last layer closes engine, last engine closes rig
                let is_last_layer = li == layer_count - 1;
                let is_last_engine = ei == engine_count - 1;
                if is_last_layer {
                    let close = if is_last_engine { -2 } else { -1 };
                    layer_track.set_folder_depth(close).await?;
                }

                // Build FXCHAIN for this layer
                let mut fxchain_nodes = Vec::new();
                let mut fx_count = 0usize;
                for module in &layer.modules {
                    let mut children = Vec::new();
                    for block in &module.blocks {
                        if let Some(ref data) = block.state_data {
                            if let Some(node) = parse_raw_block_bytes(data) {
                                children.push(node);
                                fx_count += 1;
                            }
                        }
                    }
                    fxchain_nodes.push(daw::file::types::FxChainNode::Container(
                        daw::file::types::FxContainer {
                            name: module.container_name.clone(),
                            bypassed: false,
                            offline: false,
                            fxid: None,
                            float_pos: None,
                            parallel: false,
                            container_cfg: None, // serial (REAPER default)
                            show: 0,
                            last_sel: 0,
                            docked: false,
                            children,
                            raw_block: String::new(),
                        },
                    ));
                }

                let fxchain = daw::file::FxChain {
                    window_rect: None,
                    show: 0,
                    last_sel: 0,
                    docked: false,
                    nodes: fxchain_nodes,
                    raw_content: String::new(),
                };

                // Inject FXCHAIN into the track chunk
                let chunk = layer_track.get_chunk().await?;
                let fxchain_text = fxchain.to_rpp_string();
                let new_chunk =
                    if let Some(existing) = daw::file::chunk_ops::extract_fxchain_block(&chunk) {
                        chunk.replace(existing, &fxchain_text)
                    } else {
                        // Insert FXCHAIN before the closing >
                        let pos = chunk
                            .rfind('>')
                            .ok_or_else(|| eyre::eyre!("Invalid track chunk: no closing >"))?;
                        format!("{}{}\n{}", &chunk[..pos], fxchain_text, &chunk[pos..])
                    };

                layer_track.set_chunk(new_chunk).await?;
                eprintln!(
                    "[rigs open] set FXCHAIN on '{}' ({} FX in {} modules)",
                    layer.name,
                    fx_count,
                    layer.modules.len(),
                );

                layer_track_guids.push(layer_track.guid().to_string());
            }
        }

        // Brief settle for REAPER to process the chunks
        tokio::time::sleep(std::time::Duration::from_millis(200)).await;
    } else {
        // ── 4b. FALLBACK: API-based loading + post-load chunk patching ──
        eprintln!(
            "[rigs open] fallback path: {}/{} blocks have state data, using API loading",
            blocks_with_state, total_blocks,
        );

        let load_result = signal
            .service()
            .load_rig_to_daw(&rig, None, &project)
            .await
            .map_err(|e| eyre::eyre!("{e}"))?;

        // Post-load: patch raw_blocks via track chunk manipulation
        for layer_result in &load_result.layer_results {
            let track = match project.tracks().by_guid(&layer_result.track_guid).await {
                Ok(Some(t)) => t,
                _ => continue,
            };

            let chunk_str = match track.get_chunk().await {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("[state] could not get track chunk: {e}");
                    continue;
                }
            };
            let fxchain_text = match daw::file::chunk_ops::extract_fxchain_block(&chunk_str) {
                Some(t) => t,
                None => {
                    eprintln!("[state] no FXCHAIN block in loaded track");
                    continue;
                }
            };
            let mut parsed = match daw::file::FxChain::parse(fxchain_text) {
                Ok(p) => p,
                Err(e) => {
                    eprintln!("[state] failed to parse loaded FXCHAIN: {e}");
                    continue;
                }
            };

            // Build per-module state lists from resolved hierarchy
            let mut module_states: std::collections::HashMap<String, Vec<Vec<u8>>> =
                std::collections::HashMap::new();
            for engine in &engine_specs {
                for layer in &engine.layers {
                    for module in &layer.modules {
                        let block_data: Vec<Vec<u8>> = module
                            .blocks
                            .iter()
                            .map(|b| b.state_data.clone().unwrap_or_default())
                            .collect();
                        module_states.insert(module.container_name.clone(), block_data);
                    }
                }
            }

            let mut replaced = 0usize;
            let mut skipped = 0usize;
            for node in parsed.nodes.iter_mut() {
                match node {
                    daw::file::types::FxChainNode::Container(c) => {
                        if let Some(block_states) = module_states.get(&c.name) {
                            replace_by_position(
                                &mut c.children,
                                block_states,
                                &mut replaced,
                                &mut skipped,
                            );
                        } else {
                            eprintln!("[state] container '{}' not in load result", c.name);
                            skipped += c.children.len();
                        }
                    }
                    daw::file::types::FxChainNode::Plugin(p) => {
                        let source = state_by_source_plugin.get(&p.name).or_else(|| {
                            p.custom_name.as_deref().and_then(|cn| {
                                cn.strip_prefix("[B] ")
                                    .and_then(|s| s.split_once(": ").map(|(_, name)| name))
                                    .and_then(|name| state_by_preset_name.get(name))
                            })
                        });
                        if let Some(source_bytes) = source {
                            if try_replace_raw_block(p, source_bytes) {
                                replaced += 1;
                                continue;
                            }
                        }
                        skipped += 1;
                    }
                }
            }

            if replaced > 0 {
                let new_fxchain = parsed.to_rpp_string();
                let new_chunk = chunk_str.replace(fxchain_text, &new_fxchain);
                if let Err(e) = track.set_chunk(new_chunk).await {
                    eprintln!("[state] failed to set track chunk: {e}");
                }
                eprintln!("[state] replaced state for {replaced} plugins ({skipped} skipped)");
            }

            layer_track_guids.push(layer_result.track_guid.clone());
        }
    }

    // ── 5. Verify FX loaded correctly by parsing track chunks ──
    let mut verify_issues: Vec<String> = Vec::new();
    let mut verified_fx = 0usize;
    for guid in &layer_track_guids {
        let track = match project.tracks().by_guid(guid).await {
            Ok(Some(t)) => t,
            _ => {
                verify_issues.push(format!("Layer track {guid} not found in REAPER"));
                continue;
            }
        };

        let chunk_str = match track.get_chunk().await {
            Ok(c) => c,
            Err(e) => {
                verify_issues.push(format!("Could not get track chunk: {e}"));
                continue;
            }
        };
        let fxchain_text = match daw::file::chunk_ops::extract_fxchain_block(&chunk_str) {
            Some(t) => t,
            None => {
                verify_issues.push("No FXCHAIN block in loaded track".to_string());
                continue;
            }
        };
        let parsed = match daw::file::FxChain::parse(fxchain_text) {
            Ok(p) => p,
            Err(e) => {
                verify_issues.push(format!("Failed to parse FXCHAIN: {e}"));
                continue;
            }
        };

        fn verify_nodes(
            nodes: &[daw::file::types::FxChainNode],
            state_by_preset: &std::collections::HashMap<String, Vec<u8>>,
            state_by_source: &std::collections::HashMap<String, Vec<u8>>,
            issues: &mut Vec<String>,
            count: &mut usize,
        ) {
            for node in nodes {
                match node {
                    daw::file::types::FxChainNode::Plugin(p) => {
                        let display = p.custom_name.as_deref().unwrap_or(&p.name);
                        let loaded_size = p.raw_block.len();

                        let source = state_by_source.get(&p.name).or_else(|| {
                            p.custom_name.as_deref().and_then(|cn| {
                                let stripped = cn
                                    .strip_prefix("[B] ")
                                    .or_else(|| cn.strip_prefix("[M] "))
                                    .unwrap_or(cn);
                                stripped
                                    .split_once(": ")
                                    .map(|(_, name)| name)
                                    .and_then(|name| state_by_preset.get(name))
                            })
                        });

                        let match_status = if let Some(source_data) = source {
                            let source_size = source_data.len();
                            if loaded_size > 0 && (loaded_size as f64 / source_size as f64) > 0.1 {
                                "ok"
                            } else if loaded_size == 0 {
                                issues.push(format!(
                                    "'{}': source has {} bytes but loaded plugin has no state",
                                    display, source_size,
                                ));
                                "EMPTY"
                            } else {
                                issues.push(format!(
                                    "'{}': raw_block size mismatch (loaded={}, source={})",
                                    display, loaded_size, source_size,
                                ));
                                "size-mismatch"
                            }
                        } else if loaded_size > 0 {
                            "ok (unmatched)"
                        } else {
                            "no-state"
                        };

                        eprintln!(
                            "[verify]   {} '{}': {} bytes [{}]",
                            if match_status.starts_with("ok") {
                                "✓"
                            } else {
                                "✗"
                            },
                            display,
                            loaded_size,
                            match_status,
                        );
                        *count += 1;
                    }
                    daw::file::types::FxChainNode::Container(c) => {
                        eprintln!(
                            "[verify] ┌ container '{}' ({} children)",
                            c.name,
                            c.children.len()
                        );
                        verify_nodes(&c.children, state_by_preset, state_by_source, issues, count);
                        eprintln!("[verify] └ end '{}'", c.name);
                    }
                }
            }
        }
        verify_nodes(
            &parsed.nodes,
            &state_by_preset_name,
            &state_by_source_plugin,
            &mut verify_issues,
            &mut verified_fx,
        );
    }

    // Teardown if requested (runs even on error)
    if close_after_load {
        if let Some((pid, sock)) = owned {
            daw_cli::teardown_owned(pid, &sock);
        }
    } else if let Some((pid, _)) = &owned {
        eprintln!("REAPER (PID {pid}) left open for inspection.");
    }

    // Print verification summary
    if verify_issues.is_empty() {
        eprintln!(
            "Rig \"{}\" loaded and verified: {} layers, {} FX confirmed in REAPER.",
            rig.name,
            layer_track_guids.len(),
            verified_fx,
        );
    } else {
        eprintln!("Rig \"{}\" loaded with verification issues:", rig.name);
        for issue in &verify_issues {
            eprintln!("  ⚠ {issue}");
        }
        return Err(eyre::eyre!(
            "{} verification issue(s) — FX may not have loaded correctly",
            verify_issues.len()
        ));
    }

    Ok(())
}

// ============================================================================
// Command Implementations — NAM
// ============================================================================

pub(crate) const DEFAULT_NAM_ROOT: &str = "~/Documents/Development/FastTrackStudio/signal-library/nam";

