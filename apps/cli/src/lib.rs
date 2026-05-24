//! session-cli library — reusable components for Session CLI tools.
//!
//! Provides:
//! - **Offline**: `combine` — merge multiple song RPP files into a single
//!   setlist project, trimmed to marker bounds (PREROLL → POSTROLL / =START → =END).
//! - **Live**: setlist navigation, playback control (requires running REAPER session).

use std::path::PathBuf;

use clap::Subcommand;
use eyre::{Result, WrapErr};

pub mod connection;

// ============================================================================
// CLI Definitions
// ============================================================================

#[derive(Subcommand)]
pub enum SessionCommand {
    /// Combine song projects into a single setlist RPP (offline, no DAW needed)
    Combine {
        /// Path to .RPL file listing song RPP files
        input: String,
        /// Output .RPP file path (default: <input_stem> Combined.RPP)
        #[arg(short, long)]
        output: Option<String>,
        /// Gap between songs in measures (uses next song's tempo)
        #[arg(long, default_value = "2")]
        gap: u32,
        /// Trim each song to its marker bounds (PREROLL/=START/SONGSTART → POSTROLL/=END/SONGEND)
        #[arg(long, default_value = "true")]
        trim: bool,
    },
    /// Organize an RPP project into canonical FTS hierarchy (offline)
    ///
    /// Restructures tracks into: Click+Guide, Keyflow, TRACKS, Reference/Stem Split.
    /// Also organizes markers/regions into ruler lanes.
    Organize {
        /// Path to .RPP file to organize
        input: String,
        /// Output path (default: overwrite input)
        #[arg(short, long)]
        output: Option<String>,
        /// Generate Click, Count, and Guide MIDI items from regions
        #[arg(long)]
        guide: bool,
    },
    /// Show current setlist
    Setlist,
    /// List songs in the setlist
    Songs,
    /// Show song detail
    Song {
        /// Song index (0-based)
        index: usize,
    },
    /// List sections in a song
    Sections {
        /// Song index (0-based)
        song_index: usize,
    },
    /// Navigation commands
    #[command(subcommand)]
    Goto(GotoCommand),
    /// Go to next song
    Next,
    /// Go to previous song
    Previous,
    /// Go to next section
    NextSection,
    /// Go to previous section
    PrevSection,
    /// Start playback
    Play,
    /// Pause playback
    Pause,
    /// Stop playback
    Stop,
    /// Loop control
    #[command(subcommand)]
    Loop(LoopCommand),
    /// Seek to position (seconds, relative to current song)
    Seek {
        /// Position in seconds
        seconds: f64,
    },
    /// FTS session mode (Organize / Write / Produce / Record / …)
    #[command(subcommand)]
    Mode(ModeCommand),
    /// Measure RPC roundtrip latency over one persistent connection.
    Bench {
        /// Number of calls to time
        #[arg(short, long, default_value = "200")]
        count: usize,
    },
}

#[derive(Subcommand)]
pub enum ModeCommand {
    /// Print the currently active mode slug.
    Get,
    /// Switch the active mode by slug.
    Set {
        /// One of: organize, write, produce, record, edit, mix, master, live, video, minimal.
        slug: String,
    },
    /// List all known mode slugs (declaration order).
    List,
}

#[derive(Subcommand)]
pub enum GotoCommand {
    /// Navigate to a specific song
    Song {
        /// Song index (0-based)
        index: usize,
    },
    /// Navigate to a specific section in the current song
    Section {
        /// Section index (0-based)
        index: usize,
    },
}

#[derive(Subcommand)]
pub enum LoopCommand {
    /// Loop the current song
    Song,
    /// Loop the current section
    Section,
    /// Clear loop
    Clear,
}

// ============================================================================
// Dispatch
// ============================================================================

pub async fn run(socket: Option<PathBuf>, cmd: SessionCommand, as_json: bool) -> Result<()> {
    match cmd {
        SessionCommand::Combine {
            ref input,
            ref output,
            gap,
            trim,
        } => {
            // Offline command — no DAW connection needed
            cmd_combine(input, output.as_deref(), gap, trim)
        }
        SessionCommand::Organize {
            ref input,
            ref output,
            guide,
        } => cmd_organize(input, output.as_deref(), guide),
        SessionCommand::Mode(mode_cmd) => cmd_mode(socket.as_deref(), mode_cmd, as_json).await,
        SessionCommand::Bench { count } => cmd_bench(socket.as_deref(), count).await,
        SessionCommand::Play => cmd_transport(socket.as_deref(), TransportOp::Play).await,
        SessionCommand::Pause => cmd_transport(socket.as_deref(), TransportOp::Pause).await,
        SessionCommand::Stop => cmd_transport(socket.as_deref(), TransportOp::Stop).await,
        other => {
            // Other live commands not yet wired through RPC.
            let cmd_name = match other {
                SessionCommand::Combine { .. }
                | SessionCommand::Organize { .. }
                | SessionCommand::Mode(_)
                | SessionCommand::Bench { .. }
                | SessionCommand::Play
                | SessionCommand::Pause
                | SessionCommand::Stop => unreachable!(),
                SessionCommand::Setlist => "setlist",
                SessionCommand::Songs => "songs",
                SessionCommand::Song { .. } => "song",
                SessionCommand::Sections { .. } => "sections",
                SessionCommand::Goto(GotoCommand::Song { .. }) => "goto song",
                SessionCommand::Goto(GotoCommand::Section { .. }) => "goto section",
                SessionCommand::Next => "next",
                SessionCommand::Previous => "previous",
                SessionCommand::NextSection => "next-section",
                SessionCommand::PrevSection => "prev-section",
                SessionCommand::Play => "play",
                SessionCommand::Pause => "pause",
                SessionCommand::Stop => "stop",
                SessionCommand::Loop(LoopCommand::Song) => "loop song",
                SessionCommand::Loop(LoopCommand::Section) => "loop section",
                SessionCommand::Loop(LoopCommand::Clear) => "loop clear",
                SessionCommand::Seek { .. } => "seek",
            };
            let _ = (socket, as_json); // suppress unused warnings
            eyre::bail!(
                "Session command '{}' not yet implemented. \
                 Session CLI requires RPC connection to the session cell running in REAPER.",
                cmd_name
            )
        }
    }
}

// ============================================================================
// Mode commands
// ============================================================================

async fn cmd_mode(socket: Option<&std::path::Path>, cmd: ModeCommand, as_json: bool) -> Result<()> {
    use session_proto::services::SessionModeServiceClient;

    let caller = connection::connect(socket)
        .await
        .wrap_err("connect to fts-extensions socket")?;
    let client = SessionModeServiceClient::new(caller);

    match cmd {
        ModeCommand::Get => {
            let slug = client
                .current_mode()
                .await
                .wrap_err("current_mode RPC failed")?;
            print_value(&slug, as_json)
        }
        ModeCommand::Set { slug } => {
            client
                .set_mode(slug.clone())
                .await
                .wrap_err_with(|| format!("set_mode({slug}) RPC failed"))?;
            print_value(&format!("set mode = {slug}"), as_json)
        }
        ModeCommand::List => {
            let modes = client
                .list_modes()
                .await
                .wrap_err("list_modes RPC failed")?;
            if as_json {
                let json = serde_json::to_string(&modes)?;
                println!("{json}");
            } else {
                for m in modes {
                    println!("{m}");
                }
            }
            Ok(())
        }
    }
}

fn print_value(value: &str, as_json: bool) -> Result<()> {
    if as_json {
        let json = serde_json::to_string(value)?;
        println!("{json}");
    } else {
        println!("{value}");
    }
    Ok(())
}

// ============================================================================
// Combine Command
// ============================================================================

// r[impl combined.cli.combine]
/// Combine song RPP files into a single setlist project.
///
/// When `trim` is true, each song is trimmed to its marker-defined bounds:
/// - PREROLL → POSTROLL (highest priority)
/// - =START → =END
/// - SONGSTART → SONGEND
/// - First section → last section
///
/// This removes silence/content outside the performance range, producing
/// a tightly consolidated combined project.
pub fn cmd_combine(input: &str, output: Option<&str>, gap_measures: u32, trim: bool) -> Result<()> {
    use daw::file::setlist_rpp::{self, CombineOptions};
    use std::path::Path;

    let input_path = Path::new(input);
    if !input_path.exists() {
        eyre::bail!("Input file not found: {}", input);
    }

    // Determine output path
    let output_path = if let Some(out) = output {
        PathBuf::from(out)
    } else {
        let stem = input_path.file_stem().unwrap_or_default();
        let parent = input_path.parent().unwrap_or(Path::new("."));
        parent.join(format!("{} Combined.RPP", stem.to_string_lossy()))
    };

    let options = CombineOptions {
        gap_measures,
        trim_to_bounds: trim,
    };

    // Parse RPL or treat as single RPP
    let is_rpl = input_path
        .extension()
        .map_or(false, |ext| ext.eq_ignore_ascii_case("rpl"));

    let (combined_text, song_infos) = if is_rpl {
        setlist_rpp::combine_rpl(input_path, &options)?
    } else {
        setlist_rpp::combine_rpp_files(&[input_path.to_path_buf()], &options)?
    };

    std::fs::write(&output_path, &combined_text)?;

    // Print summary
    println!(
        "Combined {} songs → {}",
        song_infos.len(),
        output_path.display()
    );
    if gap_measures > 0 {
        println!("Gap: {} measure(s) between songs", gap_measures);
    }
    if trim {
        println!("Trimmed to marker bounds (PREROLL/=START/SONGSTART → POSTROLL/=END/SONGEND)");
    }
    println!();

    let mut total = 0.0;
    for (i, info) in song_infos.iter().enumerate() {
        let dur_min = (info.duration_seconds / 60.0).floor();
        let dur_sec = info.duration_seconds % 60.0;
        println!(
            "  {:>2}. {:<40} {:>6.1}s  ({:.0}:{:02.0})",
            i + 1,
            info.name,
            info.global_start_seconds,
            dur_min,
            dur_sec,
        );
        total = info.global_start_seconds + info.duration_seconds;
    }
    println!();
    println!("Total: {:.0}:{:02.0}", (total / 60.0).floor(), total % 60.0,);

    Ok(())
}

// ============================================================================
// Organize Lanes Command
// ============================================================================

// r[impl combined.cli.organize]
/// Organize an RPP project into canonical FTS hierarchy.
///
/// Restructures tracks into:
/// - Click + Guide/ (Click, Loop, Count, Guide)
/// - Keyflow/ (CHORDS, LINES, HITS)
/// - TRACKS/ (content tracks)
/// - Reference/ (Mix + Stem Split/)
///
/// Also organizes markers/regions into FTS ruler lanes.
pub fn cmd_organize(input: &str, output: Option<&str>, generate_guide: bool) -> Result<()> {
    use daw::file::setlist_rpp;
    use std::path::Path;

    let input_path = Path::new(input);
    if !input_path.exists() {
        eyre::bail!("Input file not found: {}", input);
    }

    let output_path = output
        .map(PathBuf::from)
        .unwrap_or_else(|| input_path.to_path_buf());

    // Parse the project
    let content = std::fs::read_to_string(input_path)?;
    let mut project = daw::file::parse_project_text(&content)?;

    let original_track_count = project.tracks.len();
    let source_dir = input_path.parent().unwrap_or(Path::new("."));

    // Resolve relative media paths to absolute
    for track in &mut project.tracks {
        resolve_track_paths(track, source_dir);
    }

    // Reorganize tracks into FTS hierarchy
    project.tracks = daw::file::types::organize_into_fts_hierarchy(project.tracks);

    // Generate guide MIDI items if requested
    if generate_guide {
        let (click, count, guide) = daw::file::guide_gen::generate_guide_items(&project);
        for track in &mut project.tracks {
            let lower = track.name.to_lowercase();
            match lower.as_str() {
                "click" => track.items.extend(click.clone()),
                "count" => track.items.extend(count.clone()),
                "guide" => track.items.extend(guide.clone()),
                _ => {}
            }
        }
    }

    // Organize ruler lanes
    setlist_rpp::organize_ruler_lanes(&mut project);

    // Serialize
    let organized = setlist_rpp::project_to_rpp_text(&project);
    std::fs::write(&output_path, &organized)?;

    println!(
        "Organized {} tracks into FTS hierarchy → {}",
        original_track_count,
        output_path.display()
    );
    println!("  Click + Guide, Keyflow, TRACKS, Reference/Stem Split");

    Ok(())
}

/// Resolve relative media paths in a track's items to absolute paths.
fn resolve_track_paths(track: &mut daw::file::types::track::Track, source_dir: &std::path::Path) {
    for item in &mut track.items {
        // Resolve parsed take sources
        for take in &mut item.takes {
            if let Some(ref mut source) = take.source {
                if !source.file_path.is_empty()
                    && !std::path::PathBuf::from(&source.file_path).is_absolute()
                {
                    let absolute = source_dir.join(&source.file_path);
                    source.file_path = absolute.to_string_lossy().to_string();
                }
            }
        }

        // Resolve FILE paths in raw_content
        if !item.raw_content.is_empty() {
            let mut patched = Vec::new();
            for line in item.raw_content.lines() {
                let trimmed = line.trim();
                if trimmed.starts_with("FILE ") {
                    let file_path = trimmed.trim_start_matches("FILE ").trim_matches('"');
                    if !std::path::PathBuf::from(file_path).is_absolute() {
                        let absolute = source_dir.join(file_path);
                        patched.push(format!("FILE \"{}\"", absolute.to_string_lossy()));
                        continue;
                    }
                }
                patched.push(line.to_string());
            }
            item.raw_content = patched.join("\n");
        }
    }
}

// ============================================================================
// Transport commands
// ============================================================================

async fn cmd_bench(socket: Option<&std::path::Path>, count: usize) -> Result<()> {
    use session_proto::services::SessionModeServiceClient;

    let t_connect = std::time::Instant::now();
    let caller = connection::connect(socket)
        .await
        .wrap_err("connect to fts-extensions socket")?;
    let connect_us = t_connect.elapsed().as_micros();
    let client = SessionModeServiceClient::new(caller);

    // Warmup so the JIT'd dispatch path is hot.
    let _ = client.current_mode().await;

    let mut samples = Vec::with_capacity(count);
    for _ in 0..count {
        let t0 = std::time::Instant::now();
        let _ = client
            .current_mode()
            .await
            .map_err(|e| eyre::eyre!("rpc failed: {e:?}"))?;
        samples.push(t0.elapsed().as_micros() as u64);
    }
    samples.sort_unstable();
    let min = samples[0];
    let p50 = samples[count / 2];
    let p95 = samples[(count as f64 * 0.95) as usize];
    let p99 = samples[(count as f64 * 0.99) as usize];
    let max = samples[count - 1];
    let avg: u64 = samples.iter().sum::<u64>() / count as u64;
    println!("connect+handshake: {} µs", connect_us);
    println!(
        "rpc current_mode (n={count}): min {min} p50 {p50} avg {avg} p95 {p95} p99 {p99} max {max} (µs)"
    );
    Ok(())
}

enum TransportOp {
    Play,
    Pause,
    Stop,
}

async fn cmd_transport(socket: Option<&std::path::Path>, op: TransportOp) -> Result<()> {
    use daw_proto::ProjectContext;
    use daw_proto::transport::TransportClient;

    let caller = connection::connect(socket)
        .await
        .wrap_err("connect to fts-extensions socket")?;
    let client = TransportClient::new(caller);

    let label = match op {
        TransportOp::Play => "play",
        TransportOp::Pause => "pause",
        TransportOp::Stop => "stop",
    };

    let start = std::time::Instant::now();
    let res = match op {
        TransportOp::Play => client.play(ProjectContext::Current).await,
        TransportOp::Pause => client.pause(ProjectContext::Current).await,
        TransportOp::Stop => client.stop(ProjectContext::Current).await,
    };
    let rt = start.elapsed();
    res.map_err(|e| eyre::eyre!("transport {label} rpc failed: {e:?}"))?
        .map_err(|e| eyre::eyre!("transport {label} daw error: {e:?}"))?;
    println!("{label}  rpc roundtrip {rt:?}");
    Ok(())
}
