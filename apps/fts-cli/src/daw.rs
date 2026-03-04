use std::path::PathBuf;

use clap::{Args, Subcommand};
use daw_control::Daw;
use eyre::Result;
use serde_json::json;

use crate::introspect;

// ============================================================================
// CLI Definitions
// ============================================================================

#[derive(Subcommand)]
pub enum DawCommand {
    /// Show project info
    Info,
    /// List all tracks
    Tracks,
    /// Track operations
    #[command(subcommand)]
    Track(TrackCommand),
    /// FX chain operations
    #[command(subcommand)]
    Fx(FxCommand),
    /// List all parameters for an FX
    Params {
        /// Track name or index
        track: String,
        /// FX name or index
        fx: String,
    },
    /// Get or set a single parameter
    #[command(subcommand)]
    Param(ParamCommand),
    /// Show transport state
    Transport,
    /// Start playback
    Play,
    /// Pause playback
    Pause,
    /// Stop playback
    Stop,
    /// Toggle recording
    Record,
    /// Set tempo
    Tempo {
        /// Tempo in BPM
        bpm: f64,
    },
    /// Seek to position (seconds)
    Seek {
        /// Position in seconds
        position: f64,
    },
    /// List markers
    Markers,
    /// List regions
    Regions,
    /// Health check
    Ping,
    /// Introspect plugin(s) and generate JSON library entries
    Introspect(IntrospectArgs),
}

#[derive(Args)]
pub struct IntrospectArgs {
    /// Track name or index (required unless --all)
    #[arg(required_unless_present = "all")]
    track: Option<String>,
    /// FX name or index (required unless --all)
    #[arg(required_unless_present = "all")]
    fx: Option<String>,
    /// Block type for this plugin (e.g., amp, eq, delay, plugin). Defaults to "plugin".
    #[arg(long, short, default_value = "plugin")]
    block_type: String,
    /// Introspect ALL FX across all tracks (block type inferred from track name)
    #[arg(long, conflicts_with_all = ["track", "fx"])]
    all: bool,
    /// Output directory (creates blocks/<type>/<plugin>/ with raw.json, blocks.json, macros.json, preset.rfxchain)
    #[arg(long, short)]
    output_dir: Option<PathBuf>,
}

#[derive(Subcommand)]
pub enum TrackCommand {
    /// Show track detail
    Show {
        /// Track name or index
        track: String,
    },
    /// Mute a track
    Mute {
        /// Track name or index
        track: String,
    },
    /// Unmute a track
    Unmute {
        /// Track name or index
        track: String,
    },
    /// Solo a track
    Solo {
        /// Track name or index
        track: String,
    },
    /// Unsolo a track
    Unsolo {
        /// Track name or index
        track: String,
    },
    /// Arm a track for recording
    Arm {
        /// Track name or index
        track: String,
    },
    /// Disarm a track
    Disarm {
        /// Track name or index
        track: String,
    },
    /// Set track volume (0.0 = -inf, 1.0 = 0dB)
    Volume {
        /// Track name or index
        track: String,
        /// Volume value (0.0 to 4.0)
        value: f64,
    },
    /// Set track pan (-1.0 = L, 0.0 = C, 1.0 = R)
    Pan {
        /// Track name or index
        track: String,
        /// Pan value (-1.0 to 1.0)
        value: f64,
    },
    /// Add a new track
    Add {
        /// Track name
        name: String,
    },
    /// Remove a track
    Remove {
        /// Track name or index
        track: String,
    },
    /// Rename a track
    Rename {
        /// Track name or index
        track: String,
        /// New name
        name: String,
    },
}

#[derive(Subcommand)]
pub enum FxCommand {
    /// List FX chain for a track
    List {
        /// Track name or index
        track: String,
    },
    /// Add an FX plugin to a track
    Add {
        /// Track name or index
        track: String,
        /// Plugin name
        plugin: String,
    },
    /// Remove an FX from a track
    Remove {
        /// Track name or index
        track: String,
        /// FX name or index
        fx: String,
    },
    /// Enable an FX
    Enable {
        /// Track name or index
        track: String,
        /// FX name or index
        fx: String,
    },
    /// Bypass an FX
    Bypass {
        /// Track name or index
        track: String,
        /// FX name or index
        fx: String,
    },
}

#[derive(Subcommand)]
pub enum ParamCommand {
    /// Get a parameter value
    Get {
        /// Track name or index
        track: String,
        /// FX name or index
        fx: String,
        /// Parameter name or index
        param: String,
    },
    /// Set a parameter value
    Set {
        /// Track name or index
        track: String,
        /// FX name or index
        fx: String,
        /// Parameter name or index
        param: String,
        /// Value (0.0 to 1.0)
        value: f64,
    },
}

// ============================================================================
// Dispatch
// ============================================================================

pub async fn run(socket: Option<PathBuf>, cmd: DawCommand, as_json: bool) -> Result<()> {
    let daw = daw_cli::connect(socket).await?;

    match cmd {
        // ── Read commands (delegate to daw-cli lib) ──
        DawCommand::Info => daw_cli::cmd_info(&daw, as_json).await,
        DawCommand::Tracks => daw_cli::cmd_tracks(&daw, as_json).await,
        DawCommand::Track(TrackCommand::Show { ref track }) => {
            daw_cli::cmd_track(&daw, track, as_json).await
        }
        DawCommand::Fx(FxCommand::List { ref track }) => {
            daw_cli::cmd_fx(&daw, track, as_json).await
        }
        DawCommand::Params {
            ref track,
            ref fx,
        } => daw_cli::cmd_params(&daw, track, fx, as_json).await,
        DawCommand::Transport => daw_cli::cmd_transport(&daw, as_json).await,
        DawCommand::Markers => daw_cli::cmd_markers(&daw, as_json).await,
        DawCommand::Regions => daw_cli::cmd_regions(&daw, as_json).await,
        DawCommand::Ping => daw_cli::cmd_ping(&daw).await,

        // ── Track write commands ──
        DawCommand::Track(TrackCommand::Mute { ref track }) => {
            cmd_track_toggle(&daw, track, "mute", as_json).await
        }
        DawCommand::Track(TrackCommand::Unmute { ref track }) => {
            cmd_track_toggle(&daw, track, "unmute", as_json).await
        }
        DawCommand::Track(TrackCommand::Solo { ref track }) => {
            cmd_track_toggle(&daw, track, "solo", as_json).await
        }
        DawCommand::Track(TrackCommand::Unsolo { ref track }) => {
            cmd_track_toggle(&daw, track, "unsolo", as_json).await
        }
        DawCommand::Track(TrackCommand::Arm { ref track }) => {
            cmd_track_toggle(&daw, track, "arm", as_json).await
        }
        DawCommand::Track(TrackCommand::Disarm { ref track }) => {
            cmd_track_toggle(&daw, track, "disarm", as_json).await
        }
        DawCommand::Track(TrackCommand::Volume { ref track, value }) => {
            cmd_track_volume(&daw, track, value, as_json).await
        }
        DawCommand::Track(TrackCommand::Pan { ref track, value }) => {
            cmd_track_pan(&daw, track, value, as_json).await
        }
        DawCommand::Track(TrackCommand::Add { ref name }) => {
            cmd_track_add(&daw, name, as_json).await
        }
        DawCommand::Track(TrackCommand::Remove { ref track }) => {
            cmd_track_remove(&daw, track, as_json).await
        }
        DawCommand::Track(TrackCommand::Rename { ref track, ref name }) => {
            cmd_track_rename(&daw, track, name, as_json).await
        }

        // ── FX write commands ──
        DawCommand::Fx(FxCommand::Add {
            ref track,
            ref plugin,
        }) => cmd_fx_add(&daw, track, plugin, as_json).await,
        DawCommand::Fx(FxCommand::Remove {
            ref track,
            ref fx,
        }) => cmd_fx_remove(&daw, track, fx, as_json).await,
        DawCommand::Fx(FxCommand::Enable {
            ref track,
            ref fx,
        }) => cmd_fx_enable(&daw, track, fx, as_json).await,
        DawCommand::Fx(FxCommand::Bypass {
            ref track,
            ref fx,
        }) => cmd_fx_bypass(&daw, track, fx, as_json).await,

        // ── Param commands ──
        DawCommand::Param(ParamCommand::Get {
            ref track,
            ref fx,
            ref param,
        }) => cmd_param_get(&daw, track, fx, param, as_json).await,
        DawCommand::Param(ParamCommand::Set {
            ref track,
            ref fx,
            ref param,
            value,
        }) => cmd_param_set(&daw, track, fx, param, value, as_json).await,

        // ── Transport write commands ──
        DawCommand::Play => cmd_transport_action(&daw, "play", as_json).await,
        DawCommand::Pause => cmd_transport_action(&daw, "pause", as_json).await,
        DawCommand::Stop => cmd_transport_action(&daw, "stop", as_json).await,
        DawCommand::Record => cmd_transport_action(&daw, "record", as_json).await,
        DawCommand::Tempo { bpm } => cmd_tempo(&daw, bpm, as_json).await,
        DawCommand::Seek { position } => cmd_seek(&daw, position, as_json).await,
        DawCommand::Introspect(args) => cmd_introspect(&daw, args).await,
    }
}

// ============================================================================
// Introspect Command
// ============================================================================

async fn cmd_introspect(daw: &Daw, args: IntrospectArgs) -> Result<()> {
    if args.all {
        let entries = introspect::introspect_all(daw).await?;
        eprintln!("Introspected {} FX across all tracks.", entries.len());

        if let Some(ref dir) = args.output_dir {
            introspect::write_all_to_dir(&entries, dir)?;
        } else {
            let raw_entries: Vec<&introspect::RawPluginData> =
                entries.iter().map(|(_, r)| &r.raw).collect();
            println!("{}", serde_json::to_string_pretty(&raw_entries)?);
        }
    } else {
        let track = args.track.as_deref().unwrap();
        let fx = args.fx.as_deref().unwrap();
        let result = introspect::introspect_fx(daw, track, fx).await?;

        if let Some(ref dir) = args.output_dir {
            let block_slug = introspect::slugify(&args.block_type);
            let plugin_slug = introspect::slugify(&result.raw.plugin_name);
            let plugin_dir = dir.join(&block_slug).join(&plugin_slug);
            introspect::write_plugin_dir(&result, &plugin_dir)?;
            eprintln!("Wrote {}/", plugin_dir.display());
        } else {
            introspect::print_result(&result)?;
        }
    }

    Ok(())
}

// ============================================================================
// Track Write Commands
// ============================================================================

async fn cmd_track_toggle(daw: &Daw, track_arg: &str, action: &str, as_json: bool) -> Result<()> {
    let handle = daw_cli::resolve_track_handle(daw, track_arg).await?;
    let name = handle.info().await?.name.clone();

    match action {
        "mute" => handle.mute().await?,
        "unmute" => handle.unmute().await?,
        "solo" => handle.solo().await?,
        "unsolo" => handle.unsolo().await?,
        "arm" => handle.arm().await?,
        "disarm" => handle.disarm().await?,
        _ => unreachable!(),
    }

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": action,
                "track": name,
                "ok": true,
            }))?
        );
    } else {
        println!("{}: {}", action, name);
    }
    Ok(())
}

async fn cmd_track_volume(daw: &Daw, track_arg: &str, value: f64, as_json: bool) -> Result<()> {
    let handle = daw_cli::resolve_track_handle(daw, track_arg).await?;
    let name = handle.info().await?.name.clone();
    handle.set_volume(value).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "set_volume",
                "track": name,
                "value": value,
                "ok": true,
            }))?
        );
    } else {
        println!("volume {}: {} ({})", name, value, daw_cli::vol_to_db(value));
    }
    Ok(())
}

async fn cmd_track_pan(daw: &Daw, track_arg: &str, value: f64, as_json: bool) -> Result<()> {
    let handle = daw_cli::resolve_track_handle(daw, track_arg).await?;
    let name = handle.info().await?.name.clone();
    handle.set_pan(value).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "set_pan",
                "track": name,
                "value": value,
                "ok": true,
            }))?
        );
    } else {
        println!("pan {}: {}", name, daw_cli::pan_to_string(value));
    }
    Ok(())
}

async fn cmd_track_add(daw: &Daw, name: &str, as_json: bool) -> Result<()> {
    let project = daw.current_project().await?;
    let handle = project.tracks().add(name, None).await?;
    let info = handle.info().await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "add_track",
                "name": info.name,
                "index": info.index,
                "guid": info.guid,
                "ok": true,
            }))?
        );
    } else {
        println!("added track #{}: {}", info.index, info.name);
    }
    Ok(())
}

async fn cmd_track_remove(daw: &Daw, track_arg: &str, as_json: bool) -> Result<()> {
    let (guid, name) = daw_cli::resolve_track(daw, track_arg).await?;
    let project = daw.current_project().await?;
    project
        .tracks()
        .remove(daw_proto::TrackRef::Guid(guid))
        .await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "remove_track",
                "track": name,
                "ok": true,
            }))?
        );
    } else {
        println!("removed track: {}", name);
    }
    Ok(())
}

async fn cmd_track_rename(
    daw: &Daw,
    track_arg: &str,
    new_name: &str,
    as_json: bool,
) -> Result<()> {
    let handle = daw_cli::resolve_track_handle(daw, track_arg).await?;
    let old_name = handle.info().await?.name.clone();
    handle.rename(new_name).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "rename_track",
                "old_name": old_name,
                "new_name": new_name,
                "ok": true,
            }))?
        );
    } else {
        println!("renamed \"{}\" → \"{}\"", old_name, new_name);
    }
    Ok(())
}

// ============================================================================
// FX Write Commands
// ============================================================================

async fn cmd_fx_add(
    daw: &Daw,
    track_arg: &str,
    plugin: &str,
    as_json: bool,
) -> Result<()> {
    let handle = daw_cli::resolve_track_handle(daw, track_arg).await?;
    let track_name = handle.info().await?.name.clone();
    let fx_handle = handle.fx_chain().add(plugin).await?;
    let fx_info = fx_handle.info().await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "add_fx",
                "track": track_name,
                "fx": fx_info.name,
                "index": fx_info.index,
                "ok": true,
            }))?
        );
    } else {
        println!(
            "added FX #{} \"{}\" to \"{}\"",
            fx_info.index, fx_info.name, track_name
        );
    }
    Ok(())
}

async fn cmd_fx_remove(
    daw: &Daw,
    track_arg: &str,
    fx_arg: &str,
    as_json: bool,
) -> Result<()> {
    let (_, track_name) = daw_cli::resolve_track(daw, track_arg).await?;
    let handle = daw_cli::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = daw_cli::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;
    let fx_name = fx_handle.info().await?.name.clone();
    fx_handle.remove().await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "remove_fx",
                "track": track_name,
                "fx": fx_name,
                "ok": true,
            }))?
        );
    } else {
        println!("removed FX \"{}\" from \"{}\"", fx_name, track_name);
    }
    Ok(())
}

async fn cmd_fx_enable(
    daw: &Daw,
    track_arg: &str,
    fx_arg: &str,
    as_json: bool,
) -> Result<()> {
    let (_, track_name) = daw_cli::resolve_track(daw, track_arg).await?;
    let handle = daw_cli::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = daw_cli::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;
    let fx_name = fx_handle.info().await?.name.clone();
    fx_handle.enable().await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "enable_fx",
                "track": track_name,
                "fx": fx_name,
                "ok": true,
            }))?
        );
    } else {
        println!("enabled FX \"{}\" on \"{}\"", fx_name, track_name);
    }
    Ok(())
}

async fn cmd_fx_bypass(
    daw: &Daw,
    track_arg: &str,
    fx_arg: &str,
    as_json: bool,
) -> Result<()> {
    let (_, track_name) = daw_cli::resolve_track(daw, track_arg).await?;
    let handle = daw_cli::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = daw_cli::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;
    let fx_name = fx_handle.info().await?.name.clone();
    fx_handle.disable().await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "bypass_fx",
                "track": track_name,
                "fx": fx_name,
                "ok": true,
            }))?
        );
    } else {
        println!("bypassed FX \"{}\" on \"{}\"", fx_name, track_name);
    }
    Ok(())
}

// ============================================================================
// Param Commands
// ============================================================================

async fn resolve_param_handle(
    daw: &Daw,
    track_arg: &str,
    fx_arg: &str,
    param_arg: &str,
) -> Result<(String, String, daw_control::FxParamHandle)> {
    let (_, track_name) = daw_cli::resolve_track(daw, track_arg).await?;
    let handle = daw_cli::resolve_track_handle(daw, track_arg).await?;
    let fx_chain = handle.fx_chain();
    let fx_handle = daw_cli::resolve_fx_handle(&fx_chain, fx_arg, &track_name).await?;
    let fx_name = fx_handle.info().await?.name.clone();

    // Resolve param by index or name
    let param_handle = if let Ok(idx) = param_arg.parse::<u32>() {
        fx_handle.param(idx)
    } else {
        // Search by name
        let params = fx_handle.parameters().await?;
        let found = params
            .iter()
            .find(|p| p.name.eq_ignore_ascii_case(param_arg));
        match found {
            Some(p) => fx_handle.param(p.index),
            None => eyre::bail!("Parameter \"{}\" not found on FX \"{}\"", param_arg, fx_name),
        }
    };

    Ok((track_name, fx_name, param_handle))
}

async fn cmd_param_get(
    daw: &Daw,
    track_arg: &str,
    fx_arg: &str,
    param_arg: &str,
    as_json: bool,
) -> Result<()> {
    let (track_name, fx_name, param_handle) =
        resolve_param_handle(daw, track_arg, fx_arg, param_arg).await?;
    let info = param_handle.info().await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "track": track_name,
                "fx": fx_name,
                "param": info.name,
                "index": info.index,
                "value": info.value,
                "formatted": info.formatted,
            }))?
        );
    } else {
        println!(
            "{} / {} / {}: {} ({})",
            track_name, fx_name, info.name, info.value, info.formatted,
        );
    }
    Ok(())
}

async fn cmd_param_set(
    daw: &Daw,
    track_arg: &str,
    fx_arg: &str,
    param_arg: &str,
    value: f64,
    as_json: bool,
) -> Result<()> {
    let (track_name, fx_name, param_handle) =
        resolve_param_handle(daw, track_arg, fx_arg, param_arg).await?;
    let param_name = param_handle.info().await?.name.clone();
    param_handle.set(value).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "set_param",
                "track": track_name,
                "fx": fx_name,
                "param": param_name,
                "value": value,
                "ok": true,
            }))?
        );
    } else {
        println!(
            "set {} / {} / {} = {}",
            track_name, fx_name, param_name, value
        );
    }
    Ok(())
}

// ============================================================================
// Transport Write Commands
// ============================================================================

async fn cmd_transport_action(daw: &Daw, action: &str, as_json: bool) -> Result<()> {
    let project = daw.current_project().await?;
    let transport = project.transport();

    match action {
        "play" => transport.play().await?,
        "pause" => transport.pause().await?,
        "stop" => transport.stop().await?,
        "record" => transport.toggle_recording().await?,
        _ => unreachable!(),
    }

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": action,
                "ok": true,
            }))?
        );
    } else {
        println!("{}", action);
    }
    Ok(())
}

async fn cmd_tempo(daw: &Daw, bpm: f64, as_json: bool) -> Result<()> {
    let project = daw.current_project().await?;
    project.transport().set_tempo(bpm).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "set_tempo",
                "bpm": bpm,
                "ok": true,
            }))?
        );
    } else {
        println!("tempo: {:.1} BPM", bpm);
    }
    Ok(())
}

async fn cmd_seek(daw: &Daw, position: f64, as_json: bool) -> Result<()> {
    let project = daw.current_project().await?;
    project.transport().set_position(position).await?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&json!({
                "action": "seek",
                "position": position,
                "ok": true,
            }))?
        );
    } else {
        let mins = (position / 60.0).floor() as u32;
        let secs = position - (mins as f64 * 60.0);
        println!("seek: {}:{:06.3}", mins, secs);
    }
    Ok(())
}
