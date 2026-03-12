//! Plugin introspection — generates structured file sets from live DAW plugins.
//!
//! Each plugin gets its own directory with separate files for different concerns:
//! - `raw.json` — Plugin identity + full parameter dump (captured once, immutable)
//! - `preset.rfxchain` — Base64-encoded FX state chunk (binary preset data)
//! - `blocks.json` — PluginBlockDef for virtual module/block layout (curated over time)
//! - `macros.json` — Per-block MacroBank configs (curated over time)
//!
//! This separation keeps git diffs clean: editing macros doesn't touch the 2000-param
//! raw dump, and re-capturing state doesn't dirty the curation files.

use std::collections::HashMap;
use std::path::Path;

use daw_control::Daw;
use daw_proto::{Fx, FxParameter, FxType};
use eyre::Result;
use macromod::MacroBank;
use serde::{Deserialize, Serialize};
use signal_proto::plugin_block::PluginBlockDef;

// ============================================================================
// Types — raw.json
// ============================================================================

/// Plugin identity and full parameter dump. Written once during introspection,
/// never modified afterward. This is the immutable reference dataset.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RawPluginData {
    /// Schema version for forward compatibility.
    pub schema_version: u32,
    /// Plugin name as reported by the DAW.
    pub plugin_name: String,
    /// Plugin vendor (if known).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub vendor: Option<String>,
    /// DAW plugin format (VST2, VST3, CLAP, JS, AU).
    pub plugin_type: String,
    /// Total parameter count.
    pub parameter_count: u32,
    /// Full parameter dump from the DAW.
    pub parameters: Vec<RawParameter>,
}

/// A single parameter as reported by the DAW.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RawParameter {
    /// 0-based parameter index.
    pub index: u32,
    /// Parameter name.
    pub name: String,
    /// Current normalized value (0.0–1.0) at capture time.
    pub default_value: f64,
    /// Formatted display string (e.g., "0.0 dB", "50%").
    pub formatted: String,
    /// Whether this parameter acts as a toggle/boolean.
    pub is_toggle: bool,
    /// Number of discrete steps (`None` = continuous).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub step_count: Option<u32>,
    /// Labels for discrete steps.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub step_labels: Vec<StepLabel>,
}

/// A single discrete step label.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StepLabel {
    pub value: f64,
    pub label: String,
}

// ============================================================================
// Types — macros.json
// ============================================================================

/// Per-block macro configurations, keyed by virtual block ID.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct MacroConfigs {
    /// Schema version for forward compatibility.
    pub schema_version: u32,
    /// Per-block macro bank configs.
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub blocks: HashMap<String, MacroBank>,
}

// ============================================================================
// Introspection result — all captured data for one plugin
// ============================================================================

/// Everything captured during introspection of a single FX.
pub struct IntrospectResult {
    /// Plugin identity + raw parameter data.
    pub raw: RawPluginData,
    /// PluginBlockDef scaffold (empty modules, to be curated later).
    pub blocks: PluginBlockDef,
    /// Base64-encoded FX state chunk (VST/CLAP binary preset data).
    pub state_chunk: Option<String>,
    /// FX info from the DAW (for slug generation).
    pub fx_info: Fx,
}

// ============================================================================
// Introspection
// ============================================================================

/// Introspect a single FX on a track.
pub async fn introspect_fx(daw: &Daw, track_arg: &str, fx_arg: &str) -> Result<IntrospectResult> {
    let (_, track_name) = daw_cli::resolve_track(daw, track_arg).await?;
    let handle = daw_cli::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = daw_cli::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;

    let fx_info = fx_handle.info().await?;
    let params = fx_handle.parameters().await?;

    // Extract this FX's RPP chunk from the chain text
    let chain_text = fx_chain.fx_chain_chunk_text().await?;
    let fx_chunks = extract_fx_chunks(&chain_text);
    let rfx_chunk = fx_chunks.get(&fx_info.guid).cloned();

    Ok(build_result(fx_info, &params, rfx_chunk))
}

/// Introspect ALL FX across all tracks.
pub async fn introspect_all(daw: &Daw) -> Result<Vec<(String, IntrospectResult)>> {
    let project = daw.current_project().await?;
    let all_tracks = project.tracks().all().await?;
    let mut results = Vec::new();

    for track_info in &all_tracks {
        let track_handle = project
            .tracks()
            .by_guid(&track_info.guid)
            .await?
            .ok_or_else(|| eyre::eyre!("Track not found: {}", track_info.name))?;

        let fx_chain = track_handle.fx_chain();
        let fx_list = fx_chain.all().await?;

        if fx_list.is_empty() {
            continue;
        }

        // Get the full chain text once per track, extract per-FX chunks
        let chain_text = fx_chain.fx_chain_chunk_text().await?;
        let fx_chunks = extract_fx_chunks(&chain_text);

        for fx_info in &fx_list {
            // Skip containers (param_count <= 1)
            if fx_info.parameter_count <= 1 {
                continue;
            }

            let fx_handle = fx_chain
                .by_index(fx_info.index)
                .await?
                .ok_or_else(|| eyre::eyre!("FX at index {} not found", fx_info.index))?;

            let params = fx_handle.parameters().await?;
            let rfx_chunk = fx_chunks.get(&fx_info.guid).cloned();
            let result = build_result(fx_info.clone(), &params, rfx_chunk);
            results.push((track_info.name.clone(), result));
        }
    }

    Ok(results)
}

fn build_result(
    fx_info: Fx,
    params: &[FxParameter],
    state_chunk: Option<String>,
) -> IntrospectResult {
    let (clean_name, vendor) = clean_plugin_name(&fx_info.plugin_name);

    let raw = RawPluginData {
        schema_version: 1,
        plugin_name: clean_name.clone(),
        vendor: vendor.clone(),
        plugin_type: fx_type_string(&fx_info.plugin_type),
        parameter_count: fx_info.parameter_count,
        parameters: params
            .iter()
            .map(|p| RawParameter {
                index: p.index,
                name: p.name.clone(),
                default_value: p.value,
                formatted: p.formatted.clone(),
                is_toggle: p.is_toggle,
                step_count: p.step_count,
                step_labels: p
                    .step_labels
                    .iter()
                    .map(|(v, l)| StepLabel {
                        value: *v,
                        label: l.clone(),
                    })
                    .collect(),
            })
            .collect(),
    };

    let mut blocks = PluginBlockDef::new(&clean_name, fx_info.parameter_count);
    if let Some(ref v) = vendor {
        blocks = blocks.with_vendor(v);
    }

    IntrospectResult {
        raw,
        blocks,
        state_chunk,
        fx_info,
    }
}

/// Strip REAPER's format prefix and vendor suffix from a plugin name.
///
/// `"VST3: Archetype John Mayer X (Neural DSP)"` → `("Archetype John Mayer X", Some("Neural DSP"))`
/// `"JS: QDelay"` → `("QDelay", None)`
/// `"ReaComp (Cockos)"` → `("ReaComp", Some("Cockos"))`
fn clean_plugin_name(raw_name: &str) -> (String, Option<String>) {
    let mut name = raw_name.to_string();

    // Strip format prefix: "VST3: ", "VST: ", "CLAP: ", "AU: ", "JS: "
    for prefix in &["VST3: ", "VST: ", "CLAP: ", "AU: ", "JS: "] {
        if let Some(rest) = name.strip_prefix(prefix) {
            name = rest.to_string();
            break;
        }
    }

    // Extract vendor from trailing parentheses: "Name (Vendor)" → ("Name", "Vendor")
    let vendor = if let Some(paren_start) = name.rfind(" (") {
        if name.ends_with(')') {
            let vendor = name[paren_start + 2..name.len() - 1].to_string();
            name = name[..paren_start].to_string();
            Some(vendor)
        } else {
            None
        }
    } else {
        None
    };

    (name.trim().to_string(), vendor)
}

fn fx_type_string(ft: &FxType) -> String {
    match ft {
        FxType::Vst2 => "VST2",
        FxType::Vst3 => "VST3",
        FxType::Au => "AU",
        FxType::Js => "JS",
        FxType::Clap => "CLAP",
        FxType::Unknown => "Unknown",
    }
    .to_string()
}

// ============================================================================
// RPP Chunk Extraction
// ============================================================================

/// Extract per-FX RPP chunks from an FXCHAIN chunk text, keyed by FXID GUID.
///
/// Each FX entry in the chain has this structure:
/// ```text
///     BYPASS 0 0 0
///     <VST "..." ...>
///       ...state data...
///     >
///     FLOATPOS 0 0 0 0
///     FXID {guid}
///     WAK 0 0
/// ```
///
/// Returns a map of GUID → complete FX chunk text (wrapped in `<FXCHAIN>` for .rfxchain import).
fn extract_fx_chunks(chain_text: &str) -> HashMap<String, String> {
    let mut chunks: HashMap<String, String> = HashMap::new();
    let lines: Vec<&str> = chain_text.lines().collect();

    // Find each FXID line, then walk backwards to find the FX entry start
    let mut i = 0;
    while i < lines.len() {
        let trimmed = lines[i].trim();

        // Look for FXID lines
        if let Some(guid) = trimmed
            .strip_prefix("FXID {")
            .and_then(|s| s.strip_suffix('}'))
        {
            let fxid_line = i;
            let guid = guid.to_string();

            // Walk backwards from FXID to find the BYPASS line that starts this FX entry.
            // The FX block (<VST/CLAP/JS>) is between BYPASS and FLOATPOS.
            let mut entry_start = fxid_line;
            let mut depth: i32 = 0;

            // First, walk back past FLOATPOS and the FX block close
            let mut j = fxid_line - 1;
            while j > 0 {
                let line = lines[j].trim();
                if line == ">" {
                    depth += 1;
                } else if line.starts_with('<') {
                    depth -= 1;
                    if depth <= 0 {
                        // Found the opening of the FX block
                        entry_start = j;
                        // Check if the line before is BYPASS
                        if j > 0 && lines[j - 1].trim().starts_with("BYPASS") {
                            entry_start = j - 1;
                        }
                        break;
                    }
                }
                j -= 1;
            }

            // Walk forward past FXID to include WAK line
            let mut entry_end = fxid_line;
            if fxid_line + 1 < lines.len() && lines[fxid_line + 1].trim().starts_with("WAK") {
                entry_end = fxid_line + 1;
            }

            // Collect the FX entry lines
            let fx_lines: Vec<&str> = lines[entry_start..=entry_end].to_vec();
            let fx_chunk = fx_lines.join("\n");

            chunks.insert(guid, fx_chunk);
        }
        i += 1;
    }

    chunks
}

// ============================================================================
// Output — per-plugin directory
// ============================================================================

const RAW_FILE: &str = "raw.json";
const BLOCKS_FILE: &str = "blocks.json";
const MACROS_FILE: &str = "macros.json";
const PRESET_FILE: &str = "preset.rfxchain";

/// Write all files for a single introspected plugin into its directory.
///
/// Only writes files that don't already exist (won't overwrite curated blocks/macros).
/// Raw data and preset are always overwritten since they come from the DAW.
pub fn write_plugin_dir(result: &IntrospectResult, dir: &Path) -> Result<()> {
    std::fs::create_dir_all(dir)?;

    // raw.json — always overwrite (fresh capture)
    let raw_path = dir.join(RAW_FILE);
    std::fs::write(&raw_path, serde_json::to_string_pretty(&result.raw)?)?;

    // preset.rfxchain — always overwrite (fresh capture)
    if let Some(ref chunk) = result.state_chunk {
        let preset_path = dir.join(PRESET_FILE);
        std::fs::write(&preset_path, chunk)?;
    }

    // blocks.json — only create if missing (don't overwrite curation work)
    let blocks_path = dir.join(BLOCKS_FILE);
    if !blocks_path.exists() {
        std::fs::write(&blocks_path, serde_json::to_string_pretty(&result.blocks)?)?;
    }

    // macros.json — only create if missing (don't overwrite curation work)
    let macros_path = dir.join(MACROS_FILE);
    if !macros_path.exists() {
        let macros = MacroConfigs {
            schema_version: 1,
            blocks: HashMap::new(),
        };
        std::fs::write(&macros_path, serde_json::to_string_pretty(&macros)?)?;
    }

    Ok(())
}

/// Print introspection result to stdout as JSON (raw data only).
pub fn print_result(result: &IntrospectResult) -> Result<()> {
    println!("{}", serde_json::to_string_pretty(&result.raw)?);
    Ok(())
}

/// Write all introspected plugins to an auto-organized directory.
///
/// Layout: `<output_dir>/<block-type>/<plugin>/{raw.json, blocks.json, macros.json, preset.rfxchain}`
///
/// Block type is inferred from the track name (e.g., track "Amps" → block type "amp").
/// Falls back to "plugin" for unrecognized track names.
pub fn write_all_to_dir(entries: &[(String, IntrospectResult)], output_dir: &Path) -> Result<()> {
    let mut seen = std::collections::HashSet::new();
    let mut written = 0u32;
    let mut skipped = 0u32;

    for (track_name, result) in entries {
        if !seen.insert(result.raw.plugin_name.clone()) {
            skipped += 1;
            continue;
        }

        let block_type = infer_block_type(track_name);
        let plugin_slug = slugify(&result.raw.plugin_name);
        let dir = output_dir.join(&block_type).join(&plugin_slug);
        write_plugin_dir(result, &dir)?;

        eprintln!(
            "  {} / {} → {}/{}/",
            track_name, result.fx_info.name, block_type, plugin_slug
        );
        written += 1;
    }

    eprintln!(
        "\nWrote {} plugin(s), skipped {} duplicate(s).",
        written, skipped
    );
    Ok(())
}

/// Infer the block type slug from a track name.
///
/// Tries to match the track name against known block types. Falls back to "plugin".
fn infer_block_type(track_name: &str) -> String {
    let lower = track_name.to_lowercase();

    // Strip common suffixes like "s", "es", "blocks" to normalize plurals
    let stripped = lower
        .trim_end_matches(" blocks")
        .trim_end_matches(" block")
        .trim();

    // Try exact match against BlockType display names and kebab-case names
    if let Some(bt) = signal_proto::BlockType::from_str(stripped) {
        return bt.as_str().to_string();
    }

    // Common track name aliases
    match stripped {
        "amps" | "amp" => "amp",
        "cabs" | "cab" | "cabinet" | "cabinets" | "ir" | "irs" => "cabinet",
        "drives" | "drive" | "overdrive" | "overdrives" | "distortion" => "drive",
        "eqs" | "eq" | "equalizer" | "equalizers" => "eq",
        "comps" | "comp" | "compressor" | "compressors" | "dynamics" => "compressor",
        "gates" | "gate" => "gate",
        "delays" | "delay" => "delay",
        "reverbs" | "reverb" | "verbs" | "verb" => "reverb",
        "mods" | "mod" | "modulation" => "modulation",
        "chorus" => "chorus",
        "flangers" | "flanger" => "flanger",
        "phasers" | "phaser" => "phaser",
        "tremolos" | "tremolo" | "trem" => "tremolo",
        "limiters" | "limiter" => "limiter",
        "boosts" | "boost" => "boost",
        "tuners" | "tuner" => "tuner",
        "filters" | "filter" | "wah" => "filter",
        "pitch" | "pitchshift" => "pitch",
        "plugins" | "plugin" | "multi" => "plugin",
        "special" | "utility" => "special",
        _ => "plugin",
    }
    .to_string()
}

// ============================================================================
// Slug helper
// ============================================================================

/// Convert a name to a filesystem-safe slug: lowercase, non-alphanumeric → `-`, collapsed.
pub fn slugify(name: &str) -> String {
    let slug: String = name
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() {
                c.to_ascii_lowercase()
            } else {
                '-'
            }
        })
        .collect();

    // Collapse consecutive dashes and trim
    let mut result = String::with_capacity(slug.len());
    let mut prev_dash = false;
    for c in slug.chars() {
        if c == '-' {
            if !prev_dash && !result.is_empty() {
                result.push('-');
            }
            prev_dash = true;
        } else {
            result.push(c);
            prev_dash = false;
        }
    }

    if result.ends_with('-') {
        result.pop();
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slugify_basic() {
        assert_eq!(slugify("Neural DSP"), "neural-dsp");
        assert_eq!(slugify("FabFilter"), "fabfilter");
        assert_eq!(slugify("Valhalla DSP"), "valhalla-dsp");
    }

    #[test]
    fn slugify_special_chars() {
        assert_eq!(slugify("Archetype: John Mayer X"), "archetype-john-mayer-x");
        assert_eq!(slugify("Pro-Q 4"), "pro-q-4");
        assert_eq!(slugify("ReaEQ (Cockos)"), "reaeq-cockos");
    }

    #[test]
    fn slugify_edge_cases() {
        assert_eq!(slugify("---Leading---"), "leading");
        assert_eq!(slugify("a"), "a");
        assert_eq!(slugify("A B  C"), "a-b-c");
    }

    #[test]
    fn clean_plugin_name_vst3() {
        let (name, vendor) = clean_plugin_name("VST3: Archetype John Mayer X (Neural DSP)");
        assert_eq!(name, "Archetype John Mayer X");
        assert_eq!(vendor.as_deref(), Some("Neural DSP"));
    }

    #[test]
    fn clean_plugin_name_vst2() {
        let (name, vendor) = clean_plugin_name("VST: ReaEQ (Cockos)");
        assert_eq!(name, "ReaEQ");
        assert_eq!(vendor.as_deref(), Some("Cockos"));
    }

    #[test]
    fn clean_plugin_name_clap() {
        let (name, vendor) = clean_plugin_name("CLAP: Pro-Q 4 (FabFilter)");
        assert_eq!(name, "Pro-Q 4");
        assert_eq!(vendor.as_deref(), Some("FabFilter"));
    }

    #[test]
    fn clean_plugin_name_js_no_vendor() {
        let (name, vendor) = clean_plugin_name("JS: QDelay");
        assert_eq!(name, "QDelay");
        assert_eq!(vendor, None);
    }

    #[test]
    fn clean_plugin_name_no_prefix() {
        let (name, vendor) = clean_plugin_name("ReaComp (Cockos)");
        assert_eq!(name, "ReaComp");
        assert_eq!(vendor.as_deref(), Some("Cockos"));
    }

    #[test]
    fn infer_block_type_from_track_names() {
        assert_eq!(infer_block_type("Amp"), "amp");
        assert_eq!(infer_block_type("EQ"), "eq");
        assert_eq!(infer_block_type("Compressor"), "compressor");
        assert_eq!(infer_block_type("Drive"), "drive");
        assert_eq!(infer_block_type("Reverb"), "reverb");
        assert_eq!(infer_block_type("Delay"), "delay");
        assert_eq!(infer_block_type("Plugin"), "plugin");
        assert_eq!(infer_block_type("Guitar Rig"), "plugin");
    }
}
