//! Block capture/recall — scoped single-FX operations for the block editor.
//!
//! Captures FX state as RfxChain-compatible RPP text (not binary vst_chunk).
//! This works for ALL plugin types: VST, CLAP, JS, REAPER native, etc.

use daw_control::FxChain;
use eyre::{eyre, Result};
use signal_control::daw_bridge::{DawParameterSnapshot, DawStateChunkSnapshot};
use tracing::{debug, warn};

// Plugin tag prefixes recognized by REAPER (same list as dawfile-reaper)
const PLUGIN_TAGS: &[&str] = &["<VST ", "<AU ", "<JS ", "<CLAP ", "<VIDEO_EFFECT "];

fn is_plugin_open_tag(trimmed: &str) -> bool {
    PLUGIN_TAGS.iter().any(|tag| trimmed.starts_with(tag))
}

/// Capture a parameter snapshot for a single FX identified by GUID.
///
/// Filters the full-chain capture down to just the one FX matching `fx_guid`.
pub async fn capture_single_fx_snapshot(
    chain: &FxChain,
    fx_guid: &str,
    name: &str,
) -> Result<DawParameterSnapshot> {
    debug!("capture_single_fx_snapshot: looking for GUID '{}'", fx_guid);
    let full = signal_control::daw_bridge::capture_parameter_snapshot(chain, name).await?;

    let available: Vec<String> = full
        .fx_states
        .iter()
        .map(|s| {
            format!(
                "{}({})",
                s.plugin_name,
                &s.fx_guid[..8.min(s.fx_guid.len())]
            )
        })
        .collect();
    debug!(
        "  param snapshot has {} FX states: [{}]",
        full.fx_states.len(),
        available.join(", ")
    );

    let snap_name = full.name.clone();
    let total = full.fx_states.len();
    let matched: Vec<_> = full
        .fx_states
        .into_iter()
        .filter(|s| s.fx_guid == fx_guid)
        .collect();

    if matched.is_empty() {
        return Err(eyre!(
            "FX with GUID '{}' not found in param snapshot ({} FX captured: [{}])",
            fx_guid,
            total,
            available.join(", ")
        ));
    }

    debug!("  matched {} FX for GUID filter", matched.len());
    Ok(DawParameterSnapshot {
        name: snap_name,
        fx_states: matched,
    })
}

/// Capture the full FX chain as RfxChain-compatible RPP text.
///
/// Returns the raw `<FXCHAIN>...</FXCHAIN>` block text that can be
/// saved as an `.RfxChain` file or used with `chain.insert_chunk()`.
/// Works with ALL plugin types (VST, CLAP, JS, etc).
pub async fn capture_fx_chain_chunk(chain: &FxChain) -> Result<String> {
    debug!("capture_fx_chain_chunk: getting full chain RPP text");
    let chunk_text = chain.fx_chain_chunk_text().await?;
    debug!(
        "  got chain chunk ({} bytes, {} lines)",
        chunk_text.len(),
        chunk_text.lines().count()
    );
    Ok(chunk_text)
}

/// Represents a parsed FX entry within an FXCHAIN block.
struct FxEntry {
    /// Line index where this FX's content starts (BYPASS or plugin tag)
    start_line: usize,
    /// Line index one past the last line of this FX (exclusive)
    end_line: usize,
    /// The FXID value if found (with braces, e.g., "{GUID}")
    fxid: Option<String>,
}

/// Parse all FX entries from an FXCHAIN chunk using forward-scanning.
///
/// Uses the same approach as dawfile-reaper: scan forward for plugin opening
/// tags, depth-track to find their closing `>`, then collect trailing metadata.
fn parse_fx_entries(lines: &[&str]) -> Vec<FxEntry> {
    let mut entries = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        let trimmed = lines[i].trim();

        // Check for BYPASS line preceding a plugin block
        if trimmed.starts_with("BYPASS ") {
            // Peek ahead to see if next non-empty line is a plugin tag
            let mut peek = i + 1;
            while peek < lines.len() && lines[peek].trim().is_empty() {
                peek += 1;
            }
            if peek < lines.len() && is_plugin_open_tag(lines[peek].trim()) {
                // This BYPASS belongs to the next plugin — start the FX entry here
                let start_line = i;
                i = peek; // advance to plugin tag, will be handled below

                // Extract the plugin block with depth tracking
                let (block_end, fxid) = extract_plugin_block_and_metadata(lines, i);

                entries.push(FxEntry {
                    start_line,
                    end_line: block_end,
                    fxid,
                });
                i = block_end;
                continue;
            }
        }

        // Check for a plugin tag without preceding BYPASS
        if is_plugin_open_tag(trimmed) {
            let start_line = i;
            let (block_end, fxid) = extract_plugin_block_and_metadata(lines, i);

            entries.push(FxEntry {
                start_line,
                end_line: block_end,
                fxid,
            });
            i = block_end;
            continue;
        }

        i += 1;
    }

    entries
}

/// Extract a plugin block (depth-tracking forward scan) and its trailing metadata.
///
/// Returns (end_line_exclusive, optional_fxid).
fn extract_plugin_block_and_metadata(
    lines: &[&str],
    plugin_start: usize,
) -> (usize, Option<String>) {
    // Phase 1: Find the closing > of the plugin block via depth tracking
    let mut depth = 0i32;
    let mut i = plugin_start;

    loop {
        if i >= lines.len() {
            break;
        }
        let trimmed = lines[i].trim();
        if trimmed.starts_with('<') {
            depth += 1;
        }
        if trimmed == ">" {
            depth -= 1;
            if depth == 0 {
                i += 1; // move past the closing >
                break;
            }
        }
        i += 1;
    }

    // Phase 2: Collect trailing metadata (FLOATPOS, FXID, WAK, PRESETNAME, etc.)
    let mut fxid = None;

    while i < lines.len() {
        let trimmed = lines[i].trim();
        if trimmed.starts_with("FLOATPOS ") || trimmed.starts_with("PARM_TCP ") {
            i += 1;
        } else if trimmed.starts_with("FXID ") {
            fxid = Some(trimmed[5..].trim().to_string());
            i += 1;
        } else if trimmed.starts_with("WAK ") {
            i += 1;
        } else if trimmed.starts_with("PRESETNAME ") {
            i += 1;
        } else if trimmed.starts_with("<PARMENV ") || trimmed.starts_with("<PROGRAMENV ") {
            // Skip envelope blocks
            let mut env_depth = 0i32;
            loop {
                if i >= lines.len() {
                    break;
                }
                let t = lines[i].trim();
                if t.starts_with('<') {
                    env_depth += 1;
                }
                if t == ">" {
                    env_depth -= 1;
                    if env_depth == 0 {
                        i += 1;
                        break;
                    }
                }
                i += 1;
            }
        } else {
            // Unrecognized line — end of this FX entry
            break;
        }
    }

    (i, fxid)
}

/// Extract a single FX block from the full FXCHAIN chunk text by GUID.
///
/// Parses all FX entries forward, then finds the one whose FXID matches.
/// Returns the complete RfxChain-compatible text for that single FX:
/// BYPASS + plugin block + FLOATPOS + FXID + WAK.
pub fn extract_single_fx_from_chain(chain_chunk: &str, fx_guid: &str) -> Result<String> {
    let guid_upper = fx_guid.to_uppercase();
    let guid_with_braces = format!("{{{}}}", guid_upper);

    debug!(
        "extract_single_fx_from_chain: looking for GUID '{}' (braced: '{}')",
        fx_guid, guid_with_braces
    );

    let lines: Vec<&str> = chain_chunk.lines().collect();
    let entries = parse_fx_entries(&lines);

    debug!("  parsed {} FX entries from chain chunk", entries.len());
    for (idx, entry) in entries.iter().enumerate() {
        debug!(
            "    FX[{}] lines {}..{} FXID={:?}",
            idx, entry.start_line, entry.end_line, entry.fxid
        );
    }

    // Find the entry matching our target GUID
    let matching = entries.iter().find(|e| {
        if let Some(ref fxid) = e.fxid {
            // FXID is stored with braces: {GUID-WITH-DASHES}
            let fxid_upper = fxid.to_uppercase();
            fxid_upper == guid_with_braces
                || fxid_upper.contains(&guid_upper)
                || fxid_upper
                    .trim_matches(|c| c == '{' || c == '}')
                    .eq_ignore_ascii_case(fx_guid)
        } else {
            false
        }
    });

    let entry = matching.ok_or_else(|| {
        let all_fxids: Vec<&str> = entries.iter().filter_map(|e| e.fxid.as_deref()).collect();
        warn!(
            "GUID '{}' not found in {} FX entries. Available FXIDs: {:?}",
            fx_guid,
            entries.len(),
            all_fxids
        );
        eyre!(
            "FX GUID '{}' not found in chain chunk. Available FXIDs: {:?}",
            fx_guid,
            all_fxids
        )
    })?;

    // Extract the lines for this FX entry, stripping any leading indentation
    // to produce RfxChain-compatible output (no indentation at top level)
    let extracted_lines: Vec<&str> = lines[entry.start_line..entry.end_line]
        .iter()
        .map(|l| {
            // Strip up to 4 spaces of indentation (FXCHAIN content is typically indented 2-4 spaces)
            let stripped = l
                .strip_prefix("    ")
                .unwrap_or_else(|| l.strip_prefix("  ").unwrap_or(l));
            stripped
        })
        .collect();

    let extracted = extracted_lines.join("\n");
    debug!(
        "  extracted FX block: lines {}..{} ({} bytes, starts with '{}')",
        entry.start_line,
        entry.end_line,
        extracted.len(),
        extracted_lines.first().unwrap_or(&"<empty>")
    );

    Ok(extracted)
}

/// Capture a single FX block as RfxChain-compatible RPP text.
///
/// Gets the full chain chunk, then extracts just the block for the target FX.
pub async fn capture_single_fx_rfxchain(chain: &FxChain, fx_guid: &str) -> Result<String> {
    let chain_chunk = capture_fx_chain_chunk(chain).await?;
    extract_single_fx_from_chain(&chain_chunk, fx_guid)
}

/// Capture multiple selected FX as RfxChain-compatible RPP text.
///
/// Extracts each selected FX entry from the chain chunk and concatenates
/// them in chain order. The result is valid `.RfxChain` content that can
/// be loaded back via `chain.insert_chunk()`.
pub async fn capture_selected_fx_rfxchain(chain: &FxChain, fx_guids: &[String]) -> Result<String> {
    if fx_guids.is_empty() {
        return Err(eyre!("No FX selected for capture"));
    }

    debug!(
        "capture_selected_fx_rfxchain: capturing {} FX",
        fx_guids.len()
    );

    let chain_chunk = capture_fx_chain_chunk(chain).await?;
    let lines: Vec<&str> = chain_chunk.lines().collect();
    let entries = parse_fx_entries(&lines);

    // Build a set for fast GUID lookup (case-insensitive)
    let guid_set: std::collections::HashSet<String> =
        fx_guids.iter().map(|g| g.to_uppercase()).collect();

    // Collect matching entries in chain order (entries are already in order)
    let mut blocks = Vec::new();
    for entry in &entries {
        if let Some(ref fxid) = entry.fxid {
            let fxid_clean = fxid.trim_matches(|c| c == '{' || c == '}').to_uppercase();
            if guid_set.contains(&fxid_clean) {
                // Extract this entry's lines, stripping indentation
                let extracted_lines: Vec<&str> = lines[entry.start_line..entry.end_line]
                    .iter()
                    .map(|l| {
                        l.strip_prefix("    ")
                            .unwrap_or_else(|| l.strip_prefix("  ").unwrap_or(l))
                    })
                    .collect();
                let block = extracted_lines.join("\n");
                debug!("  included FX: FXID={} ({} bytes)", fxid, block.len());
                blocks.push(block);
            }
        }
    }

    if blocks.is_empty() {
        let available: Vec<&str> = entries.iter().filter_map(|e| e.fxid.as_deref()).collect();
        return Err(eyre!(
            "None of the {} selected GUIDs found in chain chunk. Available: {:?}",
            fx_guids.len(),
            available
        ));
    }

    debug!(
        "  captured {}/{} selected FX as RfxChain",
        blocks.len(),
        fx_guids.len()
    );

    Ok(blocks.join("\n"))
}

/// Capture the state chunk for a single FX using the old vst_chunk_encoded method.
///
/// DEPRECATED: Prefer `capture_single_fx_rfxchain` which works for all plugin types.
/// This only works for VST plugins. Kept for compatibility.
pub async fn capture_single_fx_chunk(
    chain: &FxChain,
    fx_guid: &str,
    name: &str,
) -> Result<DawStateChunkSnapshot> {
    debug!(
        "capture_single_fx_chunk (legacy vst_chunk): looking for GUID '{}'",
        fx_guid
    );
    let full = signal_control::daw_bridge::capture_state_chunks(chain, name).await?;

    let available: Vec<String> = full
        .chunks
        .iter()
        .map(|c| {
            format!(
                "{}({})",
                c.plugin_name,
                &c.fx_guid[..8.min(c.fx_guid.len())]
            )
        })
        .collect();
    debug!(
        "  chain returned {} chunks: [{}]",
        full.chunks.len(),
        available.join(", ")
    );

    let snap_name = full.name.clone();
    let total = full.chunks.len();
    let matched: Vec<_> = full
        .chunks
        .into_iter()
        .filter(|c| c.fx_guid == fx_guid)
        .collect();

    if matched.is_empty() {
        warn!(
            "capture_single_fx_chunk: GUID '{}' not found! Available: [{}]",
            fx_guid,
            available.join(", ")
        );
        return Err(eyre!(
            "FX GUID '{}' not in chunk capture ({} chunks: [{}])",
            fx_guid,
            total,
            available.join(", ")
        ));
    }

    debug!("  matched {} chunks for GUID filter", matched.len());
    Ok(DawStateChunkSnapshot {
        name: snap_name,
        chunks: matched,
    })
}
