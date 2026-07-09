//! signal-cli library — reusable components for Signal CLI tools.
//!
//! Provides connection management, command implementations, and formatting
//! for querying and manipulating the Signal library (presets, rigs, profiles,
//! macros, songs, setlists).

mod daw_compat;

use std::path::{Path, PathBuf};

use crate::daw_compat::TrackHandleCompat;
use clap::Subcommand;
use crossbeam_channel::{Receiver, bounded};
use daw::file::RppSerialize;
use daw::rpc::Daw;
use eyre::Result;
use midir::{MidiInput as MidirInput, MidiInputConnection};
use serde_json::json;
use signal::SignalController;
use signal::profile::{Patch, PatchId};
use signal::traits::Collection;
use signal_sampler::{
    LibrarySpec, PlayerPatch, PreloadProfile, SamplerRig,
    engine::cache::{
        create_signal_pack, default_prepared_cache_dir, extract_signal_pack, load_sample,
        prepare_sample_cache,
    },
};

// ============================================================================
// Connection
// ============================================================================

fn expand_tilde(path: &str) -> PathBuf {
    if let Some(rest) = path.strip_prefix("~/") {
        if let Some(home) = std::env::var_os("HOME").map(PathBuf::from) {
            return home.join(rest);
        }
    }
    PathBuf::from(path)
}

pub async fn connect_signal(db: Option<PathBuf>) -> Result<SignalController> {
    let path = match db {
        Some(p) => p,
        None => utils::paths::signal_db(),
    };
    let path_str = path
        .to_str()
        .ok_or_else(|| eyre::eyre!("Invalid DB path"))?;

    eprintln!("Opening signal DB: {}", path.display());
    let controller = signal::connect_db_seeded(path_str)
        .await
        .map_err(|e| eyre::eyre!("Failed to open signal DB: {e}"))?;
    Ok(controller)
}

// ============================================================================
// CLI Definitions
// ============================================================================
mod sampler;
mod presets;
mod modules;
mod layers;
mod engines;
mod nam;
mod profiles;
mod patches;
pub(crate) use sampler::*;
pub(crate) use presets::*;
pub(crate) use modules::*;
pub(crate) use layers::*;
pub(crate) use engines::*;
pub(crate) use nam::*;
pub(crate) use profiles::*;
pub(crate) use patches::*;
mod rigs;
pub(crate) use rigs::*;

#[derive(Subcommand)]
pub enum SignalCommand {
    /// Live sampler playback and diagnostics
    #[command(subcommand)]
    Sampler(SamplerCommand),
    /// Block preset operations
    #[command(subcommand)]
    Presets(PresetsCommand),
    /// Module preset operations
    #[command(subcommand)]
    Modules(ModulesCommand),
    /// Layer operations
    #[command(subcommand)]
    Layers(LayersCommand),
    /// Engine operations
    #[command(subcommand)]
    Engines(EnginesCommand),
    /// Rig operations
    #[command(subcommand)]
    Rigs(RigsCommand),
    /// NAM model operations
    #[command(subcommand)]
    Nam(NamCommand),
    /// Profile operations
    #[command(subcommand)]
    Profiles(ProfilesCommand),
    /// Patch operations within a profile
    #[command(subcommand)]
    Patches(PatchesCommand),
    /// Macro bank operations
    #[command(subcommand)]
    Macro(MacroCommand),
    /// Search across all signal entities
    Browse {
        /// Search query
        query: String,
    },
    /// Song operations (signal-level)
    #[command(subcommand)]
    Songs(EntityCommand),
    /// Setlist operations (signal-level)
    #[command(subcommand)]
    Setlists(EntityCommand),
    /// DAW operations (connect to REAPER via socket)
    #[command(subcommand)]
    Daw(DawCommand),
    /// Load a block or module preset onto a DAW track
    Load {
        /// Type (eq, amp, drive, etc.) — matches both block types and module types
        #[arg(name = "type")]
        preset_type: String,
        /// Preset ID (block or module)
        preset_id: String,
        /// Track (index, GUID, or name)
        track: String,
        /// Snapshot ID (omit for default snapshot)
        #[arg(long)]
        snapshot: Option<String>,
    },
}

#[derive(Subcommand)]
pub enum SamplerCommand {
    /// List detected MIDI input ports
    Midi,
    /// Prepare a reusable decoded PCM cache for a sample library
    Prepare {
        /// Path to the .styx or .toml library spec
        spec: PathBuf,
        /// Root directory containing WAV/FLAC samples
        #[arg(long)]
        samples_root: PathBuf,
        /// Cache output directory. Defaults to <samples-root>/.signal-cache-v1
        #[arg(long)]
        cache_dir: Option<PathBuf>,
    },
    /// Create a high-performance .signalpack containing decoded PCM samples.
    Pack {
        /// Path to the .styx or .toml library spec
        spec: PathBuf,
        /// Root directory containing WAV/FLAC samples
        #[arg(long)]
        samples_root: PathBuf,
        /// Output .signalpack path
        #[arg(long)]
        output: Option<PathBuf>,
    },
    /// Retag every `.signalpack` under a root, deriving instrument /
    /// category / style / tags from the directory layout. Audio bodies are
    /// copied verbatim — no re-encoding — so this is fast (5,000+ packs/min).
    Retag {
        /// Library root to scan recursively for `.signalpack` files.
        #[arg(default_value = "/run/media/AudioHaven/Signal/Libraries")]
        root: PathBuf,
        /// Substrings of paths to skip (matched against the full path).
        #[arg(long)]
        skip: Vec<String>,
        /// Print derived metadata for the first 20 packs without writing.
        #[arg(long)]
        dry_run: bool,
    },
    /// Export audio files from a decoded PCM .signalpack.
    Export {
        /// Path to the .signalpack file
        pack: PathBuf,
        /// Output directory for restored files
        #[arg(long)]
        output_dir: PathBuf,
    },
    /// Inspect decoded sample duration and level statistics
    Inspect {
        /// Sample files to inspect
        files: Vec<PathBuf>,
    },
    /// Load a sample library and play it from connected MIDI input
    Play {
        /// Path to the .styx or .toml library spec
        spec: PathBuf,
        /// Root directory containing WAV samples. If omitted, loads the spec only.
        #[arg(long)]
        samples_root: Option<PathBuf>,
        /// Instrument section id. Defaults to the first section in the spec.
        #[arg(long)]
        section: Option<String>,
        /// Mic id. Defaults to the first mic in the spec.
        #[arg(long)]
        mic: Option<String>,
        /// Instrument id inside the sampler bank
        #[arg(long, default_value = "main")]
        instrument: String,
        /// MIDI channel to route, 1-16. Omit to route all channels to the instrument.
        #[arg(long)]
        channel: Option<u8>,
        /// Audio output device substring, e.g. "Yamaha TF"
        #[arg(long)]
        device: Option<String>,
        /// Audio sample rate.
        #[arg(long, default_value_t = 48_000)]
        sample_rate: u32,
        /// Fixed audio buffer size in frames. Use 0 for backend default.
        #[arg(long, default_value_t = 256)]
        buffer_size: u32,
        /// Decode the whole loaded library before listening for MIDI.
        #[arg(long)]
        preload: bool,
        /// Decoded PCM cache budget in MiB.
        #[arg(long)]
        cache_budget_mib: Option<usize>,
        /// Evict decoded samples when the configured cache budget is exceeded.
        #[arg(long)]
        enforce_cache_budget: bool,
        /// Background preload profile: fast-audition, performance, full,
        /// drum-kit, piano-center-out, orchestral-articulation.
        #[arg(long, default_value = "performance")]
        preload_profile: String,
        /// Print MIDI note/CC events while playing. Off by default to keep the
        /// live playback path low-latency.
        #[arg(long)]
        log_midi: bool,
    },
}

#[derive(Subcommand)]
pub enum DawCommand {
    /// List all tracks in the current project
    Tracks,
    /// List all installed plugins
    Plugins,
    /// List FX chain on a track (by index, GUID, or name)
    Fx {
        /// Track (index, GUID, or name)
        track: String,
    },
    /// Launch a REAPER instance
    Launch {
        /// Config ID (e.g., "fts-tracks", "fts-signal")
        #[arg(long)]
        config: Option<String>,
    },
    /// Quit a running REAPER instance (sends SIGTERM)
    Quit {
        /// PID of the REAPER instance to kill
        #[arg(long)]
        pid: Option<u32>,
    },
    /// List open project tabs
    Projects,
    /// Open a project file
    Open {
        /// Path to the .rpp project file
        path: String,
    },
    /// Close a project tab
    Close {
        /// GUID of the project to close (defaults to current)
        #[arg(long)]
        guid: Option<String>,
    },
    /// Add a new track
    AddTrack {
        /// Track name (default: "New Track")
        #[arg(long)]
        name: Option<String>,
        /// Insert at index (default: append)
        #[arg(long)]
        at: Option<u32>,
    },
    /// Remove a track
    RemoveTrack {
        /// Track name or index
        track: String,
    },
    /// Infer the signal chain structure from a track's FX chain
    Scan {
        /// Track name or index
        track: String,
    },
    /// Import a track's FX chain as a new rig preset
    Import {
        /// Track name or index
        track: String,
        /// Name for the new rig
        name: String,
    },
}

#[derive(Subcommand)]
pub enum PresetsCommand {
    /// List presets for a block type
    List {
        /// Block type (amp, drive, eq, reverb, delay, etc.)
        block_type: String,
    },
    /// Show preset detail + parameters
    Show {
        /// Block type
        #[arg(name = "type")]
        block_type: String,
        /// Preset ID
        id: String,
    },
    /// Create a new preset
    Create {
        /// Block type
        #[arg(name = "type")]
        block_type: String,
        /// Preset name
        name: String,
    },
    /// Delete a preset
    Delete {
        /// Block type
        #[arg(name = "type")]
        block_type: String,
        /// Preset ID
        id: String,
    },
    /// Import presets from vendor plugin formats
    #[command(subcommand)]
    Import(ImportCommand),
    /// Capture the current state of a live REAPER FX as a new block preset
    Capture {
        /// Block type (reverb, eq, drive, etc.)
        #[arg(long, short = 't')]
        block_type: String,
        /// Name for the new preset
        #[arg(long, short = 'n')]
        name: String,
        /// Name for the default snapshot/variation (defaults to preset name)
        #[arg(long, short = 'v')]
        variation: Option<String>,
        /// Track containing the FX to capture (index, GUID, or name)
        #[arg(long)]
        track: String,
        /// FX slot index to capture (default: 0)
        #[arg(long, default_value = "0")]
        fx: u32,
    },
    /// Re-capture a live REAPER FX over an existing block preset snapshot
    Recapture {
        /// Block type (amp, reverb, drive, etc.)
        #[arg(long, short = 't')]
        block_type: String,
        /// Preset ID to overwrite
        id: String,
        /// Snapshot ID to overwrite (default: overwrites the default snapshot)
        #[arg(long, short = 's')]
        snapshot: Option<String>,
        /// Track name or index
        #[arg(long)]
        track: String,
        /// FX index (0-based)
        #[arg(long, default_value = "0")]
        fx: u32,
    },
    /// Set a single parameter value on an existing block preset snapshot
    SetParam {
        /// Block type
        #[arg(long, short = 't')]
        block_type: String,
        /// Preset ID
        id: String,
        /// Snapshot ID (default: default snapshot)
        #[arg(long, short = 's')]
        snapshot: Option<String>,
        /// Assignment: param_name=value (e.g. "Mix=0.75")
        assignment: String,
    },
}

#[derive(Subcommand)]
pub enum ImportCommand {
    /// Import FabFilter plugin presets
    Fabfilter {
        /// Plugin name (e.g. "Pro-Q 4")
        #[arg(long)]
        plugin: Option<String>,
        /// Import all discoverable FabFilter plugins
        #[arg(long)]
        all: bool,
        /// Show what would be imported without persisting
        #[arg(long)]
        dry_run: bool,
    },
    /// Import rfxchain presets from signal-library directories
    Rfxchain {
        /// Source directory containing preset subdirectories
        #[arg(long)]
        source: PathBuf,
        /// Block type (amp, eq, reverb, etc.)
        #[arg(long)]
        block_type: String,
        /// Optional plugin name override
        #[arg(long)]
        name: Option<String>,
        /// Show what would be imported without persisting
        #[arg(long)]
        dry_run: bool,
    },
}

#[derive(Subcommand)]
pub enum ModulesCommand {
    /// List module presets
    List,
    /// Show module preset detail
    Show {
        /// Module preset ID
        id: String,
    },
    /// Create a module preset from block preset references
    Create {
        /// Module type (amp, drive, time, etc.)
        #[arg(long, short = 't')]
        module_type: String,
        /// Name for the new module preset
        #[arg(long, short = 'n')]
        name: String,
        /// Block slots as block_type:preset_id pairs (e.g. amp:abc123 reverb:def456)
        #[arg(required = true)]
        blocks: Vec<String>,
    },
    /// Add a variation (snapshot) to an existing module preset
    AddVariation {
        /// Module preset ID
        id: String,
        /// Name for the new variation
        #[arg(long, short = 'n')]
        name: String,
        /// Parameter overrides: block_id:param_name=value (e.g. "reverb_0:Mix=0.75"), repeatable
        #[arg(long = "override", short = 'o')]
        overrides: Vec<String>,
    },
    /// Edit overrides or block sources on an existing module variation
    EditVariation {
        /// Module preset ID
        id: String,
        /// Snapshot ID to update
        snapshot: String,
        /// Update overrides: block_id:param_name=value, repeatable
        #[arg(long = "override", short = 'o')]
        overrides: Vec<String>,
        /// Reassign block source: block_id:block_type:preset_id, repeatable
        #[arg(long = "block", short = 'b')]
        blocks: Vec<String>,
    },
}

/// Shared CRUD subcommands for songs, setlists.
#[derive(Subcommand)]
pub enum EntityCommand {
    /// List all
    List,
    /// Show detail
    Show {
        /// Entity ID
        id: String,
    },
    /// Create new
    Create {
        /// Name
        name: String,
    },
    /// Delete
    Delete {
        /// Entity ID
        id: String,
    },
}

#[derive(Subcommand)]
pub enum LayersCommand {
    /// List all layers
    List,
    /// Show layer detail (block refs, module refs)
    Show {
        /// Layer ID
        id: String,
    },
    /// Create a new layer
    Create {
        /// Layer name
        name: String,
        /// Engine type (guitar, bass, keys, drums, vocals)
        #[arg(long, default_value = "guitar")]
        r#type: String,
    },
    /// Delete a layer
    Delete {
        /// Layer ID
        id: String,
    },
    /// Add a block preset reference to a layer's default snapshot
    AddBlock {
        /// Layer ID
        layer_id: String,
        /// Block preset ID
        preset_id: String,
        /// Snapshot variant ID (omit for default)
        #[arg(long)]
        variant: Option<String>,
    },
    /// Remove a block preset reference from a layer's default snapshot
    RemoveBlock {
        /// Layer ID
        layer_id: String,
        /// Block preset ID to remove
        preset_id: String,
    },
}

#[derive(Subcommand)]
pub enum EnginesCommand {
    /// List all engines
    List,
    /// Show engine detail (resolves layer names)
    Show {
        /// Engine ID
        id: String,
    },
    /// Create a new engine
    Create {
        /// Engine name
        name: String,
        /// Engine type (guitar, bass, keys, drums, vocals)
        #[arg(long, default_value = "guitar")]
        r#type: String,
        /// Layer IDs to include
        #[arg(long)]
        layer: Vec<String>,
    },
    /// Delete an engine
    Delete {
        /// Engine ID
        id: String,
    },
    /// Add a layer to an engine (updates all scenes)
    AddLayer {
        /// Engine ID
        engine_id: String,
        /// Layer ID
        layer_id: String,
    },
    /// Remove a layer from an engine (updates all scenes)
    RemoveLayer {
        /// Engine ID
        engine_id: String,
        /// Layer ID
        layer_id: String,
    },
}

#[derive(Subcommand)]
pub enum RigsCommand {
    /// List all rigs
    List,
    /// Show rig detail (full hierarchy: engine -> layer -> block)
    Show {
        /// Rig ID
        id: String,
    },
    /// Create a new rig
    Create {
        /// Rig name
        name: String,
    },
    /// Delete a rig
    Delete {
        /// Rig ID
        id: String,
    },
    /// Add an engine to a rig (updates all scenes)
    AddEngine {
        /// Rig ID
        rig_id: String,
        /// Engine ID
        engine_id: String,
    },
    /// Remove an engine from a rig (updates all scenes)
    RemoveEngine {
        /// Rig ID
        rig_id: String,
        /// Engine ID
        engine_id: String,
    },
    /// Open a rig in REAPER (creates [R]/[E]/[L] track hierarchy and loads all FX)
    Open {
        /// Rig ID
        id: String,
        /// Spawn and manage a dedicated REAPER instance instead of connecting to a running one
        #[arg(long)]
        own_reaper: bool,
        /// Kill REAPER after the rig loads (only meaningful with --own-reaper; useful for testing)
        #[arg(long)]
        close_after_load: bool,
    },
}

#[derive(Subcommand)]
pub enum NamCommand {
    /// List available NAM packs
    Packs {
        /// Filter by vendor
        #[arg(long)]
        vendor: Option<String>,
        /// Filter by category (amp, drive)
        #[arg(long)]
        category: Option<String>,
    },
    /// Import NAM packs as block presets
    Import {
        /// Filter by vendor
        #[arg(long)]
        vendor: Option<String>,
        /// Filter by category (amp, drive)
        #[arg(long)]
        category: Option<String>,
        /// Show what would be imported without persisting
        #[arg(long)]
        dry_run: bool,
        /// Spawn and manage a dedicated REAPER instance instead of connecting to a running one
        #[arg(long)]
        own_reaper: bool,
    },
}

#[derive(Subcommand)]
pub enum ProfilesCommand {
    /// List all profiles
    List,
    /// Show profile detail + patches
    Show {
        /// Profile ID
        id: String,
    },
    /// Activate a profile (optionally a specific patch)
    Activate {
        /// Profile ID
        id: String,
        /// Patch ID (optional, uses default if omitted)
        patch: Option<String>,
    },
}

#[derive(Subcommand)]
pub enum PatchesCommand {
    /// List patches in a profile
    List {
        /// Profile ID
        profile_id: String,
    },
    /// Add a patch to a profile
    Add {
        /// Profile ID
        profile_id: String,
        /// Patch name
        name: String,
    },
    /// Remove a patch from a profile
    Remove {
        /// Profile ID
        profile_id: String,
        /// Patch ID
        patch_id: String,
    },
}

#[derive(Subcommand)]
pub enum MacroCommand {
    /// Show macro bank for a block preset
    Bank {
        /// Block type
        #[arg(name = "type")]
        block_type: String,
        /// Preset ID
        preset_id: String,
        /// Snapshot ID (optional)
        snapshot_id: Option<String>,
    },
    /// Set entire macro bank from JSON
    SetBank {
        /// Block type
        #[arg(name = "type")]
        block_type: String,
        /// Preset ID
        preset_id: String,
        /// JSON string (or - for stdin)
        json: String,
    },
    /// Show parameter curation
    Curation {
        /// Block type
        #[arg(name = "type")]
        block_type: String,
        /// Preset ID
        preset_id: String,
    },
    /// Set parameter curation from JSON
    SetCuration {
        /// Block type
        #[arg(name = "type")]
        block_type: String,
        /// Preset ID
        preset_id: String,
        /// JSON string (or - for stdin)
        json: String,
    },
}

// ============================================================================
// Dispatch
// ============================================================================

pub async fn run(
    db: Option<PathBuf>,
    socket: Option<PathBuf>,
    cmd: SignalCommand,
    as_json: bool,
) -> Result<()> {
    // Sampler commands are hardware/audio diagnostics and do not need the Signal DB.
    if let SignalCommand::Sampler(ref sampler_cmd) = cmd {
        return run_sampler(sampler_cmd).await;
    }

    // DAW commands get their own branch — they may or may not need the signal DB.
    if let SignalCommand::Daw(ref daw_cmd) = cmd {
        return run_daw(db, socket, daw_cmd, as_json).await;
    }

    // Load needs both signal DB and DAW connection.
    if let SignalCommand::Load {
        ref preset_type,
        ref preset_id,
        ref track,
        snapshot,
    } = cmd
    {
        return cmd_signal_load(
            db,
            socket,
            preset_type,
            preset_id,
            track,
            snapshot.as_deref(),
            as_json,
        )
        .await;
    }

    // Rigs Open needs both signal DB and DAW connection.
    if let SignalCommand::Rigs(RigsCommand::Open {
        ref id,
        own_reaper,
        close_after_load,
    }) = cmd
    {
        return cmd_rigs_open(db, socket, id, own_reaper, close_after_load).await;
    }

    // Capture needs both signal DB and DAW connection.
    if let SignalCommand::Presets(PresetsCommand::Capture {
        ref block_type,
        ref name,
        ref variation,
        ref track,
        fx,
    }) = cmd
    {
        return cmd_presets_capture(
            db,
            socket,
            block_type,
            name,
            variation.as_deref(),
            track,
            fx,
        )
        .await;
    }

    // Recapture needs both signal DB and DAW connection.
    if let SignalCommand::Presets(PresetsCommand::Recapture {
        ref block_type,
        ref id,
        ref snapshot,
        ref track,
        fx,
    }) = cmd
    {
        return cmd_presets_recapture(db, socket, block_type, id, snapshot.as_deref(), track, fx)
            .await;
    }

    let signal = connect_signal(db).await?;

    match cmd {
        SignalCommand::Presets(PresetsCommand::List { ref block_type }) => {
            cmd_presets_list(&signal, block_type, as_json).await
        }
        SignalCommand::Presets(PresetsCommand::Show {
            ref block_type,
            ref id,
        }) => cmd_presets_show(&signal, block_type, id, as_json).await,
        SignalCommand::Presets(PresetsCommand::Create {
            ref block_type,
            ref name,
        }) => cmd_presets_create(&signal, block_type, name, as_json).await,
        SignalCommand::Presets(PresetsCommand::Delete {
            ref block_type,
            ref id,
        }) => cmd_presets_delete(&signal, block_type, id, as_json).await,
        SignalCommand::Presets(PresetsCommand::Import(ref import_cmd)) => {
            cmd_presets_import(&signal, import_cmd).await
        }
        SignalCommand::Presets(PresetsCommand::SetParam {
            ref block_type,
            ref id,
            ref snapshot,
            ref assignment,
        }) => cmd_presets_set_param(&signal, block_type, id, snapshot.as_deref(), assignment).await,

        SignalCommand::Modules(ModulesCommand::List) => cmd_modules_list(&signal, as_json).await,
        SignalCommand::Modules(ModulesCommand::Show { ref id }) => {
            cmd_modules_show(&signal, id, as_json).await
        }
        SignalCommand::Modules(ModulesCommand::Create {
            ref module_type,
            ref name,
            ref blocks,
        }) => cmd_modules_create(&signal, module_type, name, blocks).await,
        SignalCommand::Modules(ModulesCommand::AddVariation {
            ref id,
            ref name,
            ref overrides,
        }) => cmd_modules_add_variation(&signal, id, name, overrides).await,
        SignalCommand::Modules(ModulesCommand::EditVariation {
            ref id,
            ref snapshot,
            ref overrides,
            ref blocks,
        }) => cmd_modules_edit_variation(&signal, id, snapshot, overrides, blocks).await,

        SignalCommand::Layers(LayersCommand::List) => cmd_layers_list(&signal, as_json).await,
        SignalCommand::Layers(LayersCommand::Show { ref id }) => {
            cmd_layers_show(&signal, id, as_json).await
        }
        SignalCommand::Layers(LayersCommand::Create {
            ref name,
            ref r#type,
        }) => cmd_layers_create(&signal, name, r#type, as_json).await,
        SignalCommand::Layers(LayersCommand::Delete { ref id }) => {
            cmd_layers_delete(&signal, id, as_json).await
        }
        SignalCommand::Layers(LayersCommand::AddBlock {
            ref layer_id,
            ref preset_id,
            ref variant,
        }) => cmd_layers_add_block(&signal, layer_id, preset_id, variant.as_deref(), as_json).await,
        SignalCommand::Layers(LayersCommand::RemoveBlock {
            ref layer_id,
            ref preset_id,
        }) => cmd_layers_remove_block(&signal, layer_id, preset_id, as_json).await,

        SignalCommand::Engines(EnginesCommand::List) => cmd_engines_list(&signal, as_json).await,
        SignalCommand::Engines(EnginesCommand::Show { ref id }) => {
            cmd_engines_show(&signal, id, as_json).await
        }
        SignalCommand::Engines(EnginesCommand::Create {
            ref name,
            ref r#type,
            ref layer,
        }) => cmd_engines_create(&signal, name, r#type, layer, as_json).await,
        SignalCommand::Engines(EnginesCommand::Delete { ref id }) => {
            cmd_engines_delete(&signal, id, as_json).await
        }
        SignalCommand::Engines(EnginesCommand::AddLayer {
            ref engine_id,
            ref layer_id,
        }) => cmd_engines_add_layer(&signal, engine_id, layer_id, as_json).await,
        SignalCommand::Engines(EnginesCommand::RemoveLayer {
            ref engine_id,
            ref layer_id,
        }) => cmd_engines_remove_layer(&signal, engine_id, layer_id, as_json).await,

        SignalCommand::Rigs(RigsCommand::List) => cmd_rigs_list(&signal, as_json).await,
        SignalCommand::Rigs(RigsCommand::Show { ref id }) => {
            cmd_rigs_show(&signal, id, as_json).await
        }
        SignalCommand::Rigs(RigsCommand::Create { ref name }) => {
            cmd_rigs_create(&signal, name, as_json).await
        }
        SignalCommand::Rigs(RigsCommand::Delete { ref id }) => {
            cmd_rigs_delete(&signal, id, as_json).await
        }
        SignalCommand::Rigs(RigsCommand::AddEngine {
            ref rig_id,
            ref engine_id,
        }) => cmd_rigs_add_engine(&signal, rig_id, engine_id, as_json).await,
        SignalCommand::Rigs(RigsCommand::RemoveEngine {
            ref rig_id,
            ref engine_id,
        }) => cmd_rigs_remove_engine(&signal, rig_id, engine_id, as_json).await,

        SignalCommand::Nam(NamCommand::Packs {
            ref vendor,
            ref category,
        }) => cmd_nam_packs(vendor.as_deref(), category.as_deref()).await,
        SignalCommand::Nam(NamCommand::Import {
            ref vendor,
            ref category,
            dry_run,
            own_reaper,
        }) => {
            if dry_run {
                cmd_nam_import_dry_run(vendor.as_deref(), category.as_deref()).await
            } else if own_reaper {
                let (daw, pid, sock) = daw_cli::launch_and_connect("fts-signal")
                    .await
                    .map_err(|e| eyre::eyre!("Failed to launch REAPER: {e}"))?;
                let result =
                    cmd_nam_import(&signal, &daw, vendor.as_deref(), category.as_deref()).await;
                daw_cli::teardown_owned(pid, &sock);
                result
            } else {
                let daw = daw_cli::connect(socket.clone())
                    .await
                    .map_err(|e| eyre::eyre!("REAPER required for nam import: {e}"))?;
                cmd_nam_import(&signal, &daw, vendor.as_deref(), category.as_deref()).await
            }
        }

        SignalCommand::Profiles(ProfilesCommand::List) => cmd_profiles_list(&signal, as_json).await,
        SignalCommand::Profiles(ProfilesCommand::Show { ref id }) => {
            cmd_profiles_show(&signal, id, as_json).await
        }
        SignalCommand::Profiles(ProfilesCommand::Activate { ref id, ref patch }) => {
            cmd_profiles_activate(&signal, id, patch.as_deref(), as_json).await
        }

        SignalCommand::Patches(PatchesCommand::List { ref profile_id }) => {
            cmd_patches_list(&signal, profile_id, as_json).await
        }
        SignalCommand::Patches(PatchesCommand::Add {
            ref profile_id,
            ref name,
        }) => cmd_patches_add(&signal, profile_id, name, as_json).await,
        SignalCommand::Patches(PatchesCommand::Remove {
            ref profile_id,
            ref patch_id,
        }) => cmd_patches_remove(&signal, profile_id, patch_id, as_json).await,

        SignalCommand::Macro(ref _macro_cmd) => {
            eyre::bail!("Macro commands not yet implemented (Phase 2)")
        }

        SignalCommand::Browse { ref query } => cmd_browse(&signal, query, as_json).await,

        SignalCommand::Songs(EntityCommand::List) => cmd_songs_list(&signal, as_json).await,
        SignalCommand::Songs(EntityCommand::Show { ref id }) => {
            cmd_songs_show(&signal, id, as_json).await
        }
        SignalCommand::Songs(EntityCommand::Create { ref name }) => {
            cmd_songs_create(&signal, name, as_json).await
        }
        SignalCommand::Songs(EntityCommand::Delete { ref id }) => {
            cmd_songs_delete(&signal, id, as_json).await
        }

        SignalCommand::Setlists(EntityCommand::List) => cmd_setlists_list(&signal, as_json).await,
        SignalCommand::Setlists(EntityCommand::Show { ref id }) => {
            cmd_setlists_show(&signal, id, as_json).await
        }
        SignalCommand::Setlists(EntityCommand::Create { ref name }) => {
            cmd_setlists_create(&signal, name, as_json).await
        }
        SignalCommand::Setlists(EntityCommand::Delete { ref id }) => {
            cmd_setlists_delete(&signal, id, as_json).await
        }

        // Handled above before signal DB connection.
        SignalCommand::Sampler(_)
        | SignalCommand::Daw(_)
        | SignalCommand::Load { .. }
        | SignalCommand::Rigs(RigsCommand::Open { .. })
        | SignalCommand::Presets(PresetsCommand::Capture { .. })
        | SignalCommand::Presets(PresetsCommand::Recapture { .. }) => unreachable!(),
    }
}

async fn cmd_songs_list(signal: &SignalController, as_json: bool) -> Result<()> {
    let songs = signal.songs().list().await?;

    if as_json {
        let arr: Vec<_> = songs
            .iter()
            .map(|s| {
                json!({
                    "id": s.id.to_string(),
                    "name": s.name,
                    "section_count": s.sections.len(),
                })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&arr)?);
    } else {
        if songs.is_empty() {
            println!("No songs.");
            return Ok(());
        }
        println!("Songs ({}):", songs.len());
        for s in &songs {
            println!("  {} — {} ({} sections)", s.id, s.name, s.sections.len());
        }
    }
    Ok(())
}

async fn cmd_songs_show(signal: &SignalController, id: &str, as_json: bool) -> Result<()> {
    let song = signal.songs().load(id.to_string()).await?;
    match song {
        Some(s) => {
            if as_json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&json!({
                        "id": s.id.to_string(),
                        "name": s.name,
                        "sections": s.sections.iter().map(|sec| json!({
                            "id": sec.id.to_string(),
                            "name": sec.name,
                            "source": format!("{:?}", sec.source),
                        })).collect::<Vec<_>>(),
                    }))?
                );
            } else {
                println!("Song: {} ({})", s.name, s.id);
                for sec in &s.sections {
                    println!("  {} — {}", sec.id, sec.name);
                }
            }
        }
        None => eyre::bail!("Song not found: {id}"),
    }
    Ok(())
}

async fn cmd_songs_create(signal: &SignalController, name: &str, as_json: bool) -> Result<()> {
    let profiles = signal.profiles().list().await?;
    let profile = profiles
        .first()
        .ok_or_else(|| eyre::eyre!("No profiles exist — create a profile first"))?;

    let song = signal
        .songs()
        .create_from_profile(name.to_string(), profile.id.clone())
        .await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "create_song",
                "id": song.id.to_string(),
                "name": song.name,
                "ok": true,
            }))?
        );
    } else {
        println!("created song: {} ({})", song.name, song.id);
    }
    Ok(())
}

async fn cmd_songs_delete(signal: &SignalController, id: &str, as_json: bool) -> Result<()> {
    signal.songs().delete(id.to_string()).await?;
    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "delete_song",
                "id": id,
                "ok": true,
            }))?
        );
    } else {
        println!("deleted song: {}", id);
    }
    Ok(())
}

// ============================================================================
// Command Implementations — Setlists (signal-level)
// ============================================================================

async fn cmd_setlists_list(signal: &SignalController, as_json: bool) -> Result<()> {
    let setlists = signal.setlists().list().await?;

    if as_json {
        let arr: Vec<_> = setlists
            .iter()
            .map(|s| {
                json!({
                    "id": s.id.to_string(),
                    "name": s.name,
                    "entry_count": s.entries.len(),
                })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&arr)?);
    } else {
        if setlists.is_empty() {
            println!("No setlists.");
            return Ok(());
        }
        println!("Setlists ({}):", setlists.len());
        for s in &setlists {
            println!("  {} — {} ({} entries)", s.id, s.name, s.entries.len());
        }
    }
    Ok(())
}

async fn cmd_setlists_show(signal: &SignalController, id: &str, as_json: bool) -> Result<()> {
    let setlist = signal.setlists().load(id.to_string()).await?;
    match setlist {
        Some(s) => {
            if as_json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&json!({
                        "id": s.id.to_string(),
                        "name": s.name,
                        "entries": s.entries.iter().map(|e| json!({
                            "id": e.id.to_string(),
                            "name": e.name,
                            "song_id": e.song_id.to_string(),
                        })).collect::<Vec<_>>(),
                    }))?
                );
            } else {
                println!("Setlist: {} ({})", s.name, s.id);
                for (i, e) in s.entries.iter().enumerate() {
                    println!("  {}. {} (song: {})", i + 1, e.name, e.song_id);
                }
            }
        }
        None => eyre::bail!("Setlist not found: {id}"),
    }
    Ok(())
}

async fn cmd_setlists_create(signal: &SignalController, name: &str, as_json: bool) -> Result<()> {
    let songs = signal.songs().list().await?;
    let song = songs
        .first()
        .ok_or_else(|| eyre::eyre!("No songs exist — create a song first"))?;

    let setlist = signal
        .setlists()
        .create(name.to_string(), song.name.clone(), song.id.clone())
        .await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "create_setlist",
                "id": setlist.id.to_string(),
                "name": setlist.name,
                "ok": true,
            }))?
        );
    } else {
        println!("created setlist: {} ({})", setlist.name, setlist.id);
    }
    Ok(())
}

async fn cmd_setlists_delete(signal: &SignalController, id: &str, as_json: bool) -> Result<()> {
    signal.setlists().delete(id.to_string()).await?;
    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "delete_setlist",
                "id": id,
                "ok": true,
            }))?
        );
    } else {
        println!("deleted setlist: {}", id);
    }
    Ok(())
}

// ============================================================================
// DAW Commands
// ============================================================================

async fn run_daw(
    db: Option<PathBuf>,
    socket: Option<PathBuf>,
    cmd: &DawCommand,
    as_json: bool,
) -> Result<()> {
    // Commands that don't need an RPC connection
    match cmd {
        DawCommand::Launch { ref config } => {
            return daw_cli::cmd_launch(config.as_deref());
        }
        DawCommand::Quit { pid } => {
            return daw_cli::cmd_quit(*pid);
        }
        // Import needs both DAW and signal DB — handle before the shared daw connection.
        DawCommand::Import {
            ref track,
            ref name,
        } => {
            return cmd_daw_import(db, socket, track, name).await;
        }
        _ => {}
    }

    let daw = daw_cli::connect(socket).await?;

    match cmd {
        DawCommand::Tracks => cmd_daw_tracks(&daw, as_json).await,
        DawCommand::Plugins => daw_cli::cmd_plugins(&daw, as_json).await,
        DawCommand::Fx { ref track } => cmd_daw_fx(&daw, track, as_json).await,
        DawCommand::Projects => daw_cli::cmd_projects(&daw, as_json).await,
        DawCommand::Open { ref path } => daw_cli::cmd_open(&daw, path, as_json).await,
        DawCommand::Close { ref guid } => daw_cli::cmd_close(&daw, guid.as_deref()).await,
        DawCommand::AddTrack { ref name, at } => {
            daw_cli::cmd_add_track(&daw, name.as_deref(), *at, as_json).await
        }
        DawCommand::RemoveTrack { ref track } => daw_cli::cmd_remove_track(&daw, track).await,
        DawCommand::Scan { ref track } => cmd_daw_scan(&daw, track, as_json).await,
        // Already handled above
        DawCommand::Launch { .. } | DawCommand::Quit { .. } | DawCommand::Import { .. } => {
            unreachable!()
        }
    }
}

async fn cmd_daw_tracks(daw: &Daw, as_json: bool) -> Result<()> {
    daw_cli::cmd_tracks(daw, as_json).await
}

async fn cmd_daw_fx(daw: &Daw, track_arg: &str, as_json: bool) -> Result<()> {
    daw_cli::cmd_fx(daw, track_arg, as_json).await
}

async fn cmd_daw_scan(daw: &Daw, track_arg: &str, as_json: bool) -> Result<()> {
    let handle = daw_cli::resolve_track_handle(daw, track_arg).await?;
    let tree = handle.fx_chain().tree().await?;
    let chain = signal::signal_daw_bridge::infer_chain_from_fx_tree(&tree);

    if as_json {
        println!("{}", serde_json::to_string_pretty(&chain)?);
    } else {
        println!("Signal chain for track \"{}\":", track_arg);
        for module in &chain.modules {
            let block_count = module.chain.blocks().len();
            println!(
                "  [{}] {} ({} block{})",
                module.module_type.as_str(),
                module.name,
                block_count,
                if block_count == 1 { "" } else { "s" }
            );
            for block in module.chain.blocks() {
                println!("    - {} ({})", block.label(), block.block_type().as_str());
            }
        }
        for block in &chain.standalone_blocks {
            println!(
                "  [{}] {} (standalone)",
                block.block_type.as_str(),
                block.name
            );
        }
    }
    Ok(())
}

async fn cmd_daw_import(
    db: Option<PathBuf>,
    socket: Option<PathBuf>,
    track_arg: &str,
    rig_name: &str,
) -> Result<()> {
    use signal::ops::rig_importer::{ImportBlock, ImportChain, ImportModule};
    use std::collections::HashMap;

    let daw = daw_cli::connect(socket).await?;
    let signal = connect_signal(db).await?;

    let handle = daw_cli::resolve_track_handle(&daw, track_arg).await?;
    let tree = handle.fx_chain().tree().await?;
    let inferred = signal::signal_daw_bridge::infer_chain_from_fx_tree(&tree);

    // Capture per-plugin state by parsing the full track RPP chunk.
    // This avoids REAPER API limitations with encoded container-child indices.
    // dawfile-reaper parses the nested container structure and gives us
    // per-plugin raw_block text matched by FXID (GUID).
    let mut state_by_guid: HashMap<String, Vec<u8>> = HashMap::new();
    match handle.get_chunk().await {
        Ok(chunk_str) => {
            if let Some(fxchain_text) = daw::file::chunk_ops::extract_fxchain_block(&chunk_str) {
                if let Ok(parsed) = daw::file::FxChain::parse(fxchain_text) {
                    fn collect_plugin_state(
                        nodes: &[daw::file::types::FxChainNode],
                        out: &mut HashMap<String, Vec<u8>>,
                    ) {
                        for node in nodes {
                            match node {
                                daw::file::types::FxChainNode::Plugin(p) => {
                                    if !p.raw_block.is_empty() {
                                        if let Some(fxid) = &p.fxid {
                                            // Store by GUID (strip braces: RPP {GUID} → tree GUID)
                                            let guid = fxid
                                                .strip_prefix('{')
                                                .and_then(|s| s.strip_suffix('}'))
                                                .unwrap_or(fxid);
                                            out.insert(
                                                guid.to_string(),
                                                p.raw_block.as_bytes().to_vec(),
                                            );
                                        } else if let Some(cn) = &p.custom_name {
                                            // Fallback for plugins without FXID (e.g. JS
                                            // inside containers): store by custom_name.
                                            // The inferred chain uses the display name as
                                            // the block ID for these plugins.
                                            out.insert(cn.clone(), p.raw_block.as_bytes().to_vec());
                                        }
                                    }
                                }
                                daw::file::types::FxChainNode::Container(c) => {
                                    // Store the entire container's raw_block keyed by its
                                    // name — sub-containers use name as their block ID in
                                    // the inferred chain. This allows `rigs open` to
                                    // restore the full container structure.
                                    if !c.raw_block.is_empty() {
                                        out.insert(c.name.clone(), c.raw_block.as_bytes().to_vec());
                                    }
                                    collect_plugin_state(&c.children, out);
                                }
                            }
                        }
                    }
                    collect_plugin_state(&parsed.nodes, &mut state_by_guid);
                    eprintln!(
                        "[import] captured state for {} plugins from track chunk",
                        state_by_guid.len()
                    );
                }
            }
        }
        Err(e) => {
            eprintln!("[import] warning: could not get track chunk for state capture: {e}");
        }
    }

    // Capture per-plugin parameters by walking the FX tree and querying
    // each plugin's parameter list via the DAW API. Keyed by GUID.
    let mut params_by_guid: HashMap<String, Vec<(String, String, f64)>> = HashMap::new();
    for node in &tree.nodes {
        collect_fx_params(&handle, node, &mut params_by_guid).await;
    }
    eprintln!(
        "[import] captured parameters for {} plugins from DAW",
        params_by_guid.len()
    );

    // Convert InferredChain → ImportChain (bridge-free input type).
    let chain = ImportChain {
        modules: inferred
            .modules
            .iter()
            .map(|m| {
                let blocks_vec = m.chain.blocks();
                ImportModule {
                    name: m.name.clone(),
                    module_type: m.module_type,
                    has_parallel_routing: !m.chain.is_serial(),
                    blocks: blocks_vec
                        .iter()
                        .enumerate()
                        .map(|(i, b)| {
                            // Look up by GUID first, then fall back to label
                            // (JS plugins inside containers may lack FXID, so
                            // collect_plugin_state stores them by custom_name).
                            let label_str = b.label();
                            let sd = state_by_guid
                                .get(b.id())
                                .or_else(|| state_by_guid.get(label_str))
                                .cloned();
                            eprintln!(
                                "[import]   block '{}' id={} state={}",
                                label_str,
                                b.id(),
                                sd.as_ref()
                                    .map_or("NONE".to_string(), |d| format!("{} bytes", d.len()))
                            );
                            ImportBlock {
                                label: label_str.to_string(),
                                block_type: b.block_type(),
                                plugin_name: m
                                    .block_plugin_names
                                    .get(i)
                                    .filter(|s| !s.is_empty())
                                    .cloned(),
                                state_data: sd,
                                parameters: params_by_guid
                                    .get(b.id())
                                    .or_else(|| params_by_guid.get(label_str))
                                    .cloned()
                                    .unwrap_or_default(),
                            }
                        })
                        .collect(),
                }
            })
            .collect(),
        standalone_blocks: inferred
            .standalone_blocks
            .iter()
            .map(|b| ImportBlock {
                label: b.name.clone(),
                block_type: b.block_type,
                plugin_name: Some(b.plugin_name.clone()),
                state_data: None, // standalone blocks don't have GUIDs in the tree
                parameters: Vec::new(),
            })
            .collect(),
    };

    println!("Importing rig \"{rig_name}\" from track \"{track_arg}\"...");
    let result = signal.import_rig_from_chain(&chain, rig_name).await?;

    println!("Created rig: {} ({})", result.rig.name, result.rig_id);
    for (name, id) in &result.module_preset_ids {
        println!("  module: {name} ({id})");
    }
    println!(
        "  {} new block preset{}, {} reused",
        result.new_block_preset_count,
        if result.new_block_preset_count == 1 {
            ""
        } else {
            "s"
        },
        result.reused_block_preset_count
    );
    println!("Run: signal rigs open {}", result.rig_id);
    Ok(())
}

/// Recursively collect plugin parameters from the FX tree by querying each
/// plugin's parameter list via the DAW API. Results are keyed by GUID.
async fn collect_fx_params(
    track: &daw::rpc::TrackHandle,
    node: &daw::service::FxNode,
    out: &mut std::collections::HashMap<String, Vec<(String, String, f64)>>,
) {
    use daw::service::FxNodeKind;
    match &node.kind {
        FxNodeKind::Plugin(fx) => {
            let guid = &fx.guid;
            match track.fx_chain().by_guid(guid).await {
                Ok(Some(fx_handle)) => match fx_handle.parameters().await {
                    Ok(params) => {
                        let mapped: Vec<(String, String, f64)> = params
                            .into_iter()
                            .map(|p| (format!("p{}", p.index), p.name, p.value))
                            .collect();
                        eprintln!(
                            "[import]   params for '{}': {} parameters",
                            fx.name,
                            mapped.len()
                        );
                        out.insert(guid.clone(), mapped);
                    }
                    Err(e) => {
                        eprintln!(
                            "[import]   warning: could not get params for '{}': {e}",
                            fx.name
                        );
                    }
                },
                _ => {
                    eprintln!(
                        "[import]   warning: could not find FX handle for '{}'",
                        fx.name
                    );
                }
            }
        }
        FxNodeKind::Container { children, .. } => {
            for child in children {
                Box::pin(collect_fx_params(track, child, out)).await;
            }
        }
    }
}

// ============================================================================
// Signal Load Command (block + module auto-detection)
// ============================================================================

async fn cmd_signal_load(
    db: Option<PathBuf>,
    socket: Option<PathBuf>,
    preset_type: &str,
    preset_id: &str,
    track_arg: &str,
    snapshot_id: Option<&str>,
    as_json: bool,
) -> Result<()> {
    let signal = connect_signal(db).await?;
    let daw = daw_cli::connect(socket).await?;
    let track_handle = daw_cli::resolve_track_handle(&daw, track_arg).await?;

    let snap_id = snapshot_id.map(|s| signal::SnapshotId::from(s.to_string()));

    // Try block type first.
    if let Some(bt) = signal::BlockType::from_str(preset_type) {
        let pid = signal::PresetId::from(preset_id.to_string());

        // Check if it's a block preset.
        let block_presets = signal.block_presets().list(bt).await?;
        if block_presets.iter().any(|p| p.id() == &pid) {
            let result = signal
                .service()
                .load_block_to_track(bt, &pid, snap_id.as_ref(), &track_handle)
                .await
                .map_err(|e| eyre::eyre!("{e}"))?;

            if as_json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&json!({
                        "action": "load",
                        "kind": "block",
                        "preset_type": preset_type,
                        "preset_id": preset_id,
                        "display_name": result.display_name,
                        "fx_guid": result.fx_guid,
                        "ok": true,
                    }))?
                );
            } else {
                println!(
                    "Loaded \"{}\" to track \"{}\" — FX GUID: {}",
                    result.display_name, track_arg, result.fx_guid,
                );
            }
            return Ok(());
        }
    }

    // Try module type.
    if let Some(mt) = signal::ModuleType::from_str(preset_type) {
        let pid = signal::ModulePresetId::from(preset_id.to_string());

        let module_presets = signal.module_presets().list().await?;
        if module_presets.iter().any(|p| p.id() == &pid) {
            let result = signal
                .service()
                .load_module_to_track(mt, &pid, 0, &track_handle)
                .await
                .map_err(|e| eyre::eyre!("{e}"))?;

            if as_json {
                let fx_list: Vec<_> = result
                    .loaded_fx
                    .iter()
                    .map(|f| {
                        json!({
                            "fx_guid": f.fx_guid,
                            "display_name": f.display_name,
                        })
                    })
                    .collect();
                println!(
                    "{}",
                    serde_json::to_string_pretty(&json!({
                        "action": "load",
                        "kind": "module",
                        "preset_type": preset_type,
                        "preset_id": preset_id,
                        "display_name": result.display_name,
                        "loaded_fx": fx_list,
                        "ok": true,
                    }))?
                );
            } else {
                println!(
                    "Loaded module \"{}\" to track \"{}\" — {} FX instances",
                    result.display_name,
                    track_arg,
                    result.loaded_fx.len(),
                );
            }
            return Ok(());
        }
    }

    Err(eyre::eyre!(
        "No block or module preset found for type \"{preset_type}\" with ID \"{preset_id}\""
    ))
}
