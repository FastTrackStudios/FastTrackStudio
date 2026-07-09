//! session-cli library — reusable components for Session CLI tools.
//!
//! Provides:
//! - **Offline**: `combine` — merge multiple song RPP files into a single
//!   setlist project, trimmed to marker bounds (PREROLL → POSTROLL / =START → =END).
//! - **Live**: setlist navigation, playback control (requires running REAPER session).

use std::path::PathBuf;

use clap::{Subcommand, ValueEnum};
use eyre::{Result, WrapErr};

/// Which RPC the bench should probe. `All` is a convenience that
/// expands to the full set; CLI parsing keeps it as one variant so
/// the user can pass `-t all` without listing them.
#[derive(Copy, Clone, Debug, ValueEnum, PartialEq, Eq)]
pub enum BenchTarget {
    All,
    Mode,
    Project,
    Tempo,
    #[clap(name = "time-sig")]
    TimeSig,
    #[clap(name = "play-state")]
    PlayState,
    #[clap(name = "track-count")]
    TrackCount,
}

impl BenchTarget {
    /// Concrete targets `All` expands to (ordered for stable output).
    fn expanded() -> &'static [BenchTarget] {
        &[
            BenchTarget::Mode,
            BenchTarget::Project,
            BenchTarget::Tempo,
            BenchTarget::TimeSig,
            BenchTarget::PlayState,
            BenchTarget::TrackCount,
        ]
    }

    fn label(self) -> &'static str {
        match self {
            BenchTarget::All => "all",
            BenchTarget::Mode => "mode",
            BenchTarget::Project => "project",
            BenchTarget::Tempo => "tempo",
            BenchTarget::TimeSig => "time-sig",
            BenchTarget::PlayState => "play-state",
            BenchTarget::TrackCount => "track-count",
        }
    }
}

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
    /// Auto-organize an RPP project using dynamic-template classification (offline)
    ///
    /// Reads existing track names, classifies them via dynamic-template's
    /// monarchy sort + group rules (Drums / Guitars / Keys / Synths / Vocals / …),
    /// and writes a new RPP with tracks regrouped into folders. The source file
    /// is never overwritten — output defaults to `"<name> [FTS].rpp"` next to it.
    AutoOrganize {
        /// Path to .RPP file to organize
        input: String,
        /// Output path (default: `"<input_stem> [FTS].rpp"` next to input)
        #[arg(short, long)]
        output: Option<String>,
    },
    /// Show current setlist
    Setlist,
    /// (Re)build the setlist by scanning every open REAPER project
    /// tab for SONGSTART / SONGEND markers and section regions.
    /// Subsequent `setlist` / `songs` / `song` calls hit the cached
    /// build; rerun this after opening, closing, or editing project
    /// structure.
    BuildSetlist,
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
    /// Track-group manager (128-slot instrument-category partition)
    #[command(subcommand)]
    Groups(GroupsCommand),
    /// Song-specific recording controls (target the active song's project)
    #[command(subcommand)]
    Record(RecordCommand),
    /// Rank the current/selected take — post-recording review
    Rank {
        /// Star level: 1, 2, 3, or `down`
        level: String,
        /// Marker placement: play-pos (default), item, or mouse
        #[arg(long, default_value = "play-pos")]
        scope: String,
    },
    /// FTS session mode (Organize / Write / Produce / Record / …)
    #[command(subcommand)]
    Mode(ModeCommand),
    /// Trigger any REAPER named command by ID — works for native
    /// commands (numeric or `_REAPER_*`) and for any registered
    /// extension action including the FTS session actions
    /// (`_FTS_SESSION_BUILD_SETLIST`, `_FTS_SESSION_LOAD_DEMO_SETLIST`,
    /// …). Bypasses the broken-on-complex-types vox RPC read paths —
    /// the response is just a bool (succeeded/failed).
    Action {
        /// Command ID. Accepts the upper-snake form
        /// (`FTS_SESSION_BUILD_SETLIST`), with or without the leading
        /// underscore. Also accepts a numeric REAPER command id
        /// (e.g. 40044 for transport play/pause) as a string.
        command_id: String,
    },
    /// Set the display name of a ruler lane in the current project.
    /// Direct primitive over `Projects::set_ruler_lane_name` — useful
    /// for debugging why a lane didn't pick up its convention name
    /// (e.g. `fts session rename-lane 1 SONG`). Lane index is
    /// 1-based; REAPER will auto-create the lane if it doesn't exist
    /// yet.
    RenameLane {
        /// 1-based ruler lane index.
        index: u32,
        /// New display name. Pass an empty string to clear.
        name: String,
    },
    /// Set a numeric project-info key on the current project. Direct
    /// access to `Projects::set_project_info` — useful for poking
    /// REAPER's internal ruler-lane configuration directly. Example:
    /// `fts session set-project-info RULER_LANE_FLAGS:2 8` marks lane
    /// 2 as the default region lane.
    SetProjectInfo {
        /// Project-info key (e.g. `RULER_LANE_FLAGS:2`, `RULER_LANE_ORDER:1`).
        key: String,
        /// Numeric value.
        value: f64,
    },
    /// Measure RPC roundtrip latency over one persistent connection.
    ///
    /// Reports min / p50 / p95 / p99 / max for each target. Use
    /// `--target all` (default) to see every probe side-by-side so
    /// you can spot which RPCs are slow.
    Bench {
        /// Number of calls per target.
        #[arg(short, long, default_value = "200")]
        count: usize,
        /// Which RPC(s) to probe. Repeat or comma-separate for several;
        /// `all` runs the full set. Targets: mode, project, tempo,
        /// time-sig, play-state, track-count.
        #[arg(short, long, default_value = "all", value_delimiter = ',')]
        target: Vec<BenchTarget>,
    },
}

#[derive(Subcommand)]
pub enum ModeCommand {
    /// Print the currently active mode slug.
    Get,
    /// Switch the active mode by slug.
    Set {
        /// One of: organize, write, produce, record, edit, mix, master, live, video, scoring.
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
    /// Toggle looping for the current song
    Song,
    /// Toggle looping for the current section
    Section,
    /// Clear any active loop
    Clear,
}

#[derive(Subcommand)]
pub enum GroupsCommand {
    /// Apply the instrument-category naming scheme to the project's 128 groups
    Apply,
    /// Assign the selected tracks to the next free slot in a category's band
    Assign {
        /// Category: drums, bass, electric, acoustic, keys, synths, lead, bgv
        category: String,
    },
}

#[derive(Subcommand)]
pub enum RecordCommand {
    /// Start recording into the active song's project
    Start,
    /// Stop recording in the active song's project
    Stop,
    /// Toggle recording in the active song's project
    Toggle,
    /// Arm the selected tracks in the active song's project
    Arm,
    /// Disarm the selected tracks in the active song's project
    Disarm,
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
        SessionCommand::AutoOrganize {
            ref input,
            ref output,
        } => cmd_auto_organize(input, output.as_deref()),
        SessionCommand::Mode(mode_cmd) => cmd_mode(socket.as_deref(), mode_cmd, as_json).await,
        SessionCommand::Action { command_id } => cmd_action(socket.as_deref(), &command_id).await,
        SessionCommand::RenameLane { index, name } => {
            cmd_rename_lane(socket.as_deref(), index, &name).await
        }
        SessionCommand::SetProjectInfo { key, value } => {
            cmd_set_project_info(socket.as_deref(), &key, value).await
        }
        SessionCommand::Bench { count, target } => {
            cmd_bench(socket.as_deref(), count, target).await
        }
        SessionCommand::Play => cmd_transport(socket.as_deref(), TransportOp::Play).await,
        SessionCommand::Pause => cmd_transport(socket.as_deref(), TransportOp::Pause).await,
        SessionCommand::Stop => cmd_transport(socket.as_deref(), TransportOp::Stop).await,
        // Setlist / song views — read-only, print human-readable
        // unless --json. Connect once per call (cold-start is the
        // price for one-shot CLI; use the daemon for fast paths).
        SessionCommand::Setlist => cmd_setlist(socket.as_deref(), as_json).await,
        SessionCommand::BuildSetlist => cmd_build_setlist(socket.as_deref()).await,
        SessionCommand::Songs => cmd_songs(socket.as_deref(), as_json).await,
        SessionCommand::Song { index } => cmd_song(socket.as_deref(), index, as_json).await,
        SessionCommand::Sections { song_index } => {
            cmd_sections(socket.as_deref(), song_index, as_json).await
        }
        // Navigation — single async call, no output unless --json.
        SessionCommand::Goto(GotoCommand::Song { index }) => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::GoSong(index)).await
        }
        SessionCommand::Goto(GotoCommand::Section { index }) => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::GoSection(index)).await
        }
        SessionCommand::Next => cmd_setlist_nav(socket.as_deref(), SetlistNav::NextSong).await,
        SessionCommand::Previous => cmd_setlist_nav(socket.as_deref(), SetlistNav::PrevSong).await,
        SessionCommand::NextSection => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::NextSection).await
        }
        SessionCommand::PrevSection => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::PrevSection).await
        }
        SessionCommand::Seek { seconds } => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::Seek(seconds)).await
        }
        SessionCommand::Loop(LoopCommand::Song) => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::LoopSong).await
        }
        SessionCommand::Loop(LoopCommand::Section) => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::LoopSection).await
        }
        SessionCommand::Loop(LoopCommand::Clear) => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::LoopClear).await
        }
        SessionCommand::Record(RecordCommand::Start) => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::Record).await
        }
        SessionCommand::Record(RecordCommand::Stop) => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::StopRecording).await
        }
        SessionCommand::Record(RecordCommand::Toggle) => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::ToggleRecording).await
        }
        SessionCommand::Record(RecordCommand::Arm) => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::SetArm(true)).await
        }
        SessionCommand::Record(RecordCommand::Disarm) => {
            cmd_setlist_nav(socket.as_deref(), SetlistNav::SetArm(false)).await
        }
        SessionCommand::Rank {
            ref level,
            ref scope,
        } => cmd_rank(socket.as_deref(), level, scope).await,
        SessionCommand::Groups(cmd) => cmd_groups(socket.as_deref(), cmd).await,
    }
}

/// Track-group manager — triggers the registered REAPER actions over the
/// socket (the group manager itself runs main-thread REAPER FFI in the
/// extension). `apply` names the 128 slots; `assign <category>` adds the
/// selected tracks to the next free slot in that band.
async fn cmd_groups(socket: Option<&std::path::Path>, cmd: GroupsCommand) -> Result<()> {
    let command_id = match cmd {
        GroupsCommand::Apply => "FTS_SESSION_GROUP_APPLY_NAMING".to_string(),
        GroupsCommand::Assign { category } => {
            let suffix = match category
                .to_ascii_lowercase()
                .replace([' ', '-'], "_")
                .as_str()
            {
                "drums" => "DRUMS",
                "bass" => "BASS",
                "electric" | "electric_gtr" | "egtr" => "ELECTRIC_GTR",
                "acoustic" | "acoustic_gtr" | "agtr" => "ACOUSTIC_GTR",
                "keys" => "KEYS",
                "synths" | "synth" => "SYNTHS",
                "lead" | "lead_vocal" | "lv" => "LEAD_VOCAL",
                "bgv" | "background" | "background_vox" | "bvox" => "BACKGROUND_VOX",
                other => eyre::bail!(
                    "unknown category {other:?} (expected drums, bass, electric, acoustic, \
                     keys, synths, lead, bgv)"
                ),
            };
            format!("FTS_SESSION_GROUP_ASSIGN_{suffix}")
        }
    };
    cmd_action(socket, &command_id).await
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
        .is_some_and(|ext| ext.eq_ignore_ascii_case("rpl"));

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

pub fn cmd_auto_organize(input: &str, output: Option<&str>) -> Result<()> {
    use daw::file::types::track::{FolderSettings, FolderState, Track};
    use daw::file::{parse_project_text, types::serialize::RppSerialize};
    use daw_proto::FolderDepthChange;
    use dynamic_template::{OrganizeIntoTracks, OrganizeOptions, default_config};
    use std::collections::{HashMap, VecDeque};
    use std::path::Path;

    let input_path = Path::new(input);
    if !input_path.exists() {
        eyre::bail!("Input file not found: {}", input);
    }

    let output_path = output.map(PathBuf::from).unwrap_or_else(|| {
        let parent = input_path.parent().unwrap_or(Path::new("."));
        let stem = input_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("project");
        let ext = input_path
            .extension()
            .and_then(|s| s.to_str())
            .unwrap_or("rpp");
        parent.join(format!("{stem} [FTS].{ext}"))
    });

    if output_path == input_path {
        eyre::bail!(
            "Output path matches input ({}); refusing to overwrite",
            input
        );
    }

    let content = std::fs::read_to_string(input_path)?;
    let mut project = parse_project_text(&content)?;
    let original_count = project.tracks.len();

    let track_names: Vec<String> = project.tracks.iter().map(|t| t.name.clone()).collect();
    let config = default_config();
    // expand_items: each input becomes a leaf TrackNode so we can match it
    // back to the original Track by name. cleanup_names off — we need
    // names to round-trip exactly for the FIFO match.
    let options = OrganizeOptions {
        expand_items: true,
        cleanup_names: false,
        collapse_single_child: true,
    };
    let hierarchy = track_names
        .organize_into_tracks_with_options(&config, None, options)
        .map_err(|err| eyre::eyre!("organize failed: {err:?}"))?;

    let mut name_to_indices: HashMap<String, VecDeque<usize>> = HashMap::new();
    for (idx, track) in project.tracks.iter().enumerate() {
        name_to_indices
            .entry(track.name.clone())
            .or_default()
            .push_back(idx);
    }

    let mut originals: Vec<Option<Track>> = project.tracks.into_iter().map(Some).collect();

    let mut new_tracks: Vec<Track> = Vec::with_capacity(hierarchy.tracks.len());
    let mut matched = 0usize;
    let mut created = 0usize;
    for node in &hierarchy.tracks {
        let folder_settings = match node.folder_depth_change {
            FolderDepthChange::Normal => FolderSettings {
                folder_state: FolderState::Regular,
                indentation: 0,
            },
            FolderDepthChange::FolderStart => FolderSettings {
                folder_state: FolderState::FolderParent,
                indentation: 1,
            },
            FolderDepthChange::ClosesLevels(n) => FolderSettings {
                folder_state: FolderState::LastInFolder,
                indentation: n as i32,
            },
        };

        let track = name_to_indices
            .get_mut(&node.name)
            .and_then(|deque| deque.pop_front())
            .and_then(|idx| originals[idx].take());

        let mut track = if let Some(track) = track {
            matched += 1;
            track
        } else {
            created += 1;
            Track {
                name: node.name.clone(),
                ..Track::default()
            }
        };

        track.name = node.name.clone();
        let state_i32 = match folder_settings.folder_state {
            FolderState::Regular => 0,
            FolderState::FolderParent => 1,
            FolderState::LastInFolder => 2,
            FolderState::Unknown(v) => v,
        };
        let indent_i32 = folder_settings.indentation;
        track.folder = Some(folder_settings);
        // raw_content takes precedence in the structured serializer, so
        // we patch ISBUS in place to make the new folder structure stick
        // without trashing FX chains, sends, items, envelopes, etc.
        if !track.raw_content.is_empty() {
            track.raw_content = patch_isbus_in_raw(&track.raw_content, state_i32, indent_i32);
        }
        new_tracks.push(track);
    }

    // Append any unmatched originals at the end (shouldn't normally happen
    // — every input name should appear once in the hierarchy — but better
    // to keep tracks than silently drop them).
    let mut orphaned = 0usize;
    for track in originals.into_iter().flatten() {
        orphaned += 1;
        new_tracks.push(track);
    }

    project.tracks = new_tracks;
    let serialized = project.to_rpp_string();
    std::fs::write(&output_path, &serialized)?;

    println!(
        "Auto-organized {} tracks → {} ({} matched, {} folders created, {} orphaned)",
        original_count,
        output_path.display(),
        matched,
        created,
        orphaned
    );

    Ok(())
}

/// Replace the trailing two-field `ISBUS <state> <indent>` line inside a
/// parsed track's raw_content block, preserving leading whitespace and
/// every other line.
fn patch_isbus_in_raw(raw: &str, state: i32, indentation: i32) -> String {
    let mut out = String::with_capacity(raw.len() + 8);
    let mut patched = false;
    for line in raw.lines() {
        let trimmed = line.trim_start();
        if !patched && trimmed.starts_with("ISBUS ") {
            let prefix_len = line.len() - trimmed.len();
            out.push_str(&line[..prefix_len]);
            out.push_str(&format!("ISBUS {state} {indentation}"));
            patched = true;
        } else {
            out.push_str(line);
        }
        out.push('\n');
    }
    if !patched {
        // No ISBUS line — insert one after NAME (or at the top of the
        // block body) so the folder change is honored.
        let mut rebuilt = String::with_capacity(out.len() + 16);
        let mut inserted = false;
        for line in out.lines() {
            rebuilt.push_str(line);
            rebuilt.push('\n');
            if !inserted && line.trim_start().starts_with("NAME ") {
                let trimmed = line.trim_start();
                let prefix_len = line.len() - trimmed.len();
                rebuilt.push_str(&line[..prefix_len]);
                rebuilt.push_str(&format!("ISBUS {state} {indentation}\n"));
                inserted = true;
            }
        }
        return rebuilt;
    }
    out
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

async fn cmd_bench(
    socket: Option<&std::path::Path>,
    count: usize,
    targets: Vec<BenchTarget>,
) -> Result<()> {
    use daw_proto::project::ProjectsClient;
    use daw_proto::track::TracksClient;
    use daw_proto::transport::TransportClient;
    use session_proto::services::SessionModeServiceClient;

    let t_connect = std::time::Instant::now();
    let caller = connection::connect(socket)
        .await
        .wrap_err("connect to fts-extensions socket")?;
    let connect_us = t_connect.elapsed().as_micros();

    // Build every client we might need — cheap, each is just a wrapper
    // around the same `Caller` clone. Easier than a per-target match.
    let mode = SessionModeServiceClient::new(caller.clone());
    let projects = ProjectsClient::new(caller.clone());
    let transport = TransportClient::new(caller.clone());
    let tracks = TracksClient::new(caller.clone());

    // Expand `All` once; deduplicate so `--target mode,mode` doesn't
    // double-run.
    let mut wanted: Vec<BenchTarget> = if targets.contains(&BenchTarget::All) {
        BenchTarget::expanded().to_vec()
    } else {
        targets
    };
    wanted.sort_by_key(|t| t.label());
    wanted.dedup();

    println!("connect+handshake: {} µs", connect_us);
    println!(
        "{:<12}  {:>7}  {:>7}  {:>7}  {:>7}  {:>7}  {:>7}",
        "target", "min", "p50", "avg", "p95", "p99", "max"
    );
    println!("{}", "─".repeat(64));

    for target in wanted {
        // Warmup so first-call schema/cache costs don't poison the
        // sample. Same one-shot path the timed loop will hit.
        run_probe(target, &mode, &projects, &transport, &tracks).await?;
        let mut samples = Vec::with_capacity(count);
        for _ in 0..count {
            let t0 = std::time::Instant::now();
            run_probe(target, &mode, &projects, &transport, &tracks).await?;
            samples.push(t0.elapsed().as_micros() as u64);
        }
        samples.sort_unstable();
        let n = samples.len();
        let min = samples[0];
        let p50 = samples[n / 2];
        let p95 = samples[(n as f64 * 0.95) as usize];
        let p99 = samples[((n as f64 * 0.99) as usize).min(n - 1)];
        let max = samples[n - 1];
        let avg: u64 = samples.iter().sum::<u64>() / n as u64;
        println!(
            "{:<12}  {:>5}µs  {:>5}µs  {:>5}µs  {:>5}µs  {:>5}µs  {:>5}µs",
            target.label(),
            min,
            p50,
            avg,
            p95,
            p99,
            max
        );
    }
    println!("(n={count} per target)");
    Ok(())
}

/// Fire one RPC for the given target, swallow the value (we only care
/// about timing) and surface RPC errors. Per-call ProjectContext is
/// `Current` since the user almost always wants the active tab's
/// numbers — no benefit from a per-project parameter for a bench.
async fn run_probe(
    target: BenchTarget,
    mode: &session_proto::services::SessionModeServiceClient,
    projects: &daw_proto::project::ProjectsClient,
    transport: &daw_proto::transport::TransportClient,
    tracks: &daw_proto::track::TracksClient,
) -> Result<()> {
    use daw_proto::ProjectContext;
    match target {
        BenchTarget::All => unreachable!("expanded before reaching run_probe"),
        BenchTarget::Mode => {
            mode.current_mode()
                .await
                .map_err(|e| eyre::eyre!("mode rpc failed: {e:?}"))?;
        }
        BenchTarget::Project => {
            projects
                .current()
                .await
                .map_err(|e| eyre::eyre!("project rpc failed: {e:?}"))?;
        }
        BenchTarget::Tempo => {
            transport
                .get_tempo(ProjectContext::Current)
                .await
                .map_err(|e| eyre::eyre!("tempo rpc failed: {e:?}"))?;
        }
        BenchTarget::TimeSig => {
            transport
                .get_time_signature(ProjectContext::Current)
                .await
                .map_err(|e| eyre::eyre!("time-sig rpc failed: {e:?}"))?;
        }
        BenchTarget::PlayState => {
            transport
                .get_play_state(ProjectContext::Current)
                .await
                .map_err(|e| eyre::eyre!("play-state rpc failed: {e:?}"))?;
        }
        BenchTarget::TrackCount => {
            tracks
                .count(ProjectContext::Current)
                .await
                .map_err(|e| eyre::eyre!("track-count rpc failed: {e:?}"))?;
        }
    }
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

// ============================================================================
// Setlist / Song commands
//
// The setlist + per-song detail come from `SetlistService` mounted by
// fts-extensions. Implementation in `session::setlist_service` builds
// the setlist by enumerating REAPER project tabs and parsing each
// project's PREROLL / SONGSTART / SONGEND markers + section regions.
//
// CLI output: human-readable by default (one line per row), JSON when
// `--json` is set so other tools can pipe results.
// ============================================================================

async fn cmd_build_setlist(socket: Option<&std::path::Path>) -> Result<()> {
    let client = setlist_client(socket).await?;
    let start = std::time::Instant::now();
    client
        .build_from_open_projects()
        .await
        .map_err(rpc_err("build_from_open_projects"))?;
    let setlist = client.setlist().await.map_err(rpc_err("setlist"))?;
    println!(
        "built setlist '{}' with {} song(s) in {:?}",
        setlist.name,
        setlist.songs.len(),
        start.elapsed()
    );
    Ok(())
}

async fn cmd_setlist(socket: Option<&std::path::Path>, as_json: bool) -> Result<()> {
    let client = setlist_client(socket).await?;
    // Don't auto-build: build_from_open_projects walks every open
    // REAPER tab, hydrates per-song MIDI charts via keyflow, and can
    // take many seconds on a real session. Prompt the user to run
    // `fts session build-setlist` explicitly so they know they're
    // paying that cost.
    let setlist = match client.setlist().await {
        Ok(s) => s,
        Err(e) if is_not_found(&e) => {
            println!(
                "(no setlist cached yet — run `fts session build-setlist` to scan open projects)"
            );
            return Ok(());
        }
        Err(e) => return Err(rpc_err("setlist")(e)),
    };
    if as_json {
        print_json(&setlist)?;
    } else {
        println!("Setlist: {}", setlist.name);
        println!("  songs: {}", setlist.songs.len());
        if let Some(id) = &setlist.id {
            println!("  id:    {id}");
        }
        for (i, song) in setlist.songs.iter().enumerate() {
            let dur = song.end_seconds - song.start_seconds;
            println!(
                "  [{i:>2}] {:<32}  {:6.2}s  ({} sections)",
                song.name,
                dur.max(0.0),
                song.sections.len()
            );
        }
    }
    Ok(())
}

async fn cmd_songs(socket: Option<&std::path::Path>, as_json: bool) -> Result<()> {
    let songs = setlist_client(socket)
        .await?
        .songs()
        .await
        .map_err(rpc_err("songs"))?;
    if as_json {
        print_json(&songs)?;
    } else if songs.is_empty() {
        println!("(no songs in current setlist)");
    } else {
        for (i, s) in songs.iter().enumerate() {
            println!("[{i:>2}] {}", s.name);
        }
    }
    Ok(())
}

async fn cmd_song(socket: Option<&std::path::Path>, index: usize, as_json: bool) -> Result<()> {
    let song = setlist_client(socket)
        .await?
        .song(index)
        .await
        .map_err(rpc_err("song"))?;
    if as_json {
        print_json(&song)?;
    } else {
        println!("Song [{index}] {}", song.name);
        println!("  project_guid : {}", song.project_guid);
        println!(
            "  range        : {:.3}s → {:.3}s ({:.3}s)",
            song.start_seconds,
            song.end_seconds,
            song.end_seconds - song.start_seconds
        );
        if let Some(co) = song.count_in_seconds {
            println!("  count-in     : {co:.3}s");
        }
        if let Some(t) = song.tempo {
            println!("  tempo        : {t:.2} BPM");
        }
        if let Some(ts) = song.time_signature {
            println!("  time sig     : {}/{}", ts.numerator(), ts.denominator());
        }
        println!("  sections     : {}", song.sections.len());
        for (i, sec) in song.sections.iter().enumerate() {
            println!("    [{i:>2}] {}", sec.name);
        }
        if !song.comments.is_empty() {
            println!("  comments     : {}", song.comments.len());
        }
    }
    Ok(())
}

async fn cmd_sections(
    socket: Option<&std::path::Path>,
    song_index: usize,
    as_json: bool,
) -> Result<()> {
    let sections = setlist_client(socket)
        .await?
        .sections(song_index)
        .await
        .map_err(rpc_err("sections"))?;
    if as_json {
        print_json(&sections)?;
    } else if sections.is_empty() {
        println!("(no sections in song {song_index})");
    } else {
        for (i, sec) in sections.iter().enumerate() {
            println!("[{i:>2}] {}", sec.name);
        }
    }
    Ok(())
}

/// Discriminator for navigation-style setlist commands. They all
/// share the same shape (one async call, no output unless --json)
/// and the same dispatch — collapsing them into one match keeps the
/// adding-a-new-nav-verb cost at one variant + one arm.
enum SetlistNav {
    GoSong(usize),
    GoSection(usize),
    NextSong,
    PrevSong,
    NextSection,
    PrevSection,
    Seek(f64),
    LoopSong,
    LoopSection,
    LoopClear,
    Record,
    StopRecording,
    ToggleRecording,
    SetArm(bool),
}

async fn cmd_setlist_nav(socket: Option<&std::path::Path>, nav: SetlistNav) -> Result<()> {
    let client = setlist_client(socket).await?;
    match nav {
        SetlistNav::GoSong(i) => client.go_to_song(i).await.map_err(rpc_err("go_to_song"))?,
        SetlistNav::GoSection(i) => client
            .go_to_section(i)
            .await
            .map_err(rpc_err("go_to_section"))?,
        SetlistNav::NextSong => client.next_song().await.map_err(rpc_err("next_song"))?,
        SetlistNav::PrevSong => client
            .previous_song()
            .await
            .map_err(rpc_err("previous_song"))?,
        SetlistNav::NextSection => client
            .next_section()
            .await
            .map_err(rpc_err("next_section"))?,
        SetlistNav::PrevSection => client
            .previous_section()
            .await
            .map_err(rpc_err("previous_section"))?,
        SetlistNav::Seek(s) => client.seek_to(s).await.map_err(rpc_err("seek_to"))?,
        SetlistNav::LoopSong => client
            .toggle_song_loop()
            .await
            .map_err(rpc_err("toggle_song_loop"))?,
        SetlistNav::LoopSection => client
            .toggle_section_loop()
            .await
            .map_err(rpc_err("toggle_section_loop"))?,
        SetlistNav::LoopClear => client.clear_loop().await.map_err(rpc_err("clear_loop"))?,
        SetlistNav::Record => client.record().await.map_err(rpc_err("record"))?,
        SetlistNav::StopRecording => client
            .stop_recording()
            .await
            .map_err(rpc_err("stop_recording"))?,
        SetlistNav::ToggleRecording => client
            .toggle_recording()
            .await
            .map_err(rpc_err("toggle_recording"))?,
        SetlistNav::SetArm(armed) => client
            .set_song_record_arm(armed)
            .await
            .map_err(rpc_err("set_song_record_arm"))?,
    }
    Ok(())
}

/// Rank the current/selected take — post-recording review. Delegates to
/// `TakeRankingService` (a separate control surface from the setlist).
async fn cmd_rank(socket: Option<&std::path::Path>, level: &str, scope: &str) -> Result<()> {
    use session_proto::services::{TakeRankLevel, TakeRankScope, TakeRankingServiceClient};

    let level = match level.to_ascii_lowercase().as_str() {
        "1" | "one" => TakeRankLevel::One,
        "2" | "two" => TakeRankLevel::Two,
        "3" | "three" => TakeRankLevel::Three,
        "down" | "d" => TakeRankLevel::Down,
        other => eyre::bail!("invalid rank level {other:?} (expected 1, 2, 3, or down)"),
    };
    let scope = match scope.to_ascii_lowercase().as_str() {
        "play-pos" | "playpos" | "play" => TakeRankScope::PlayPosMinus2s,
        "item" | "item-wide" => TakeRankScope::ItemWide,
        "mouse" | "mouse-cursor" => TakeRankScope::MouseCursor,
        other => eyre::bail!("invalid rank scope {other:?} (expected play-pos, item, or mouse)"),
    };

    let caller = connection::connect(socket)
        .await
        .wrap_err("connect to fts-extensions socket")?;
    TakeRankingServiceClient::new(caller)
        .apply_rank(scope, level)
        .await
        .map_err(|e| eyre::eyre!("take_ranking.apply_rank: {e:?}"))?;
    Ok(())
}

async fn setlist_client(
    socket: Option<&std::path::Path>,
) -> Result<session_proto::services::SetlistServiceClient> {
    use session_proto::services::SetlistServiceClient;
    let caller = connection::connect(socket)
        .await
        .wrap_err("connect to fts-extensions socket")?;
    Ok(SetlistServiceClient::new(caller))
}

fn rpc_err(
    label: &'static str,
) -> impl FnOnce(vox::VoxError<session_proto::SessionServiceError>) -> eyre::Report {
    move |e| eyre::eyre!("setlist.{label}: {e:?}")
}

/// True when the server reported "no setlist cached yet". The CLI
/// uses this to decide whether to auto-build before retrying.
fn is_not_found(e: &vox::VoxError<session_proto::SessionServiceError>) -> bool {
    matches!(
        e,
        vox::VoxError::User(inner)
            if matches!(**inner, session_proto::SessionServiceError::NotFound { .. })
    )
}

fn print_json<T: facet::Facet<'static>>(value: &T) -> Result<()> {
    let json = facet_json::to_string(value).map_err(|e| eyre::eyre!("encode json: {e:?}"))?;
    println!("{json}");
    Ok(())
}

// ============================================================================
// Generic REAPER action invocation
// ============================================================================

async fn cmd_action(socket: Option<&std::path::Path>, command_id: &str) -> Result<()> {
    use daw_proto::ProjectContext;
    use daw_proto::project::ProjectsClient;

    // Normalise the command id: REAPER's named commands always start
    // with `_` (e.g. `_FTS_SESSION_BUILD_SETLIST`). Accept the bare
    // form too so users don't have to remember the leading underscore
    // when typing on the shell.
    let normalized: String =
        if command_id.starts_with('_') || command_id.chars().all(|c| c.is_ascii_digit()) {
            command_id.to_string()
        } else {
            format!("_{}", command_id)
        };

    let caller = connection::connect(socket)
        .await
        .wrap_err("connect to fts-extensions socket")?;
    let projects = ProjectsClient::new(caller);

    let start = std::time::Instant::now();
    let result = projects
        .run_command(ProjectContext::Current, normalized.clone())
        .await
        .map_err(|e| eyre::eyre!("run_command rpc failed: {e:?}"))?;
    let rt = start.elapsed();

    if result {
        println!("{normalized}  ok   ({rt:?})");
    } else {
        eyre::bail!(
            "{normalized}  failed — REAPER returned false. Command may not be \
             registered, or REAPER refused to run it (no project, wrong context, \
             etc.). Check the extension log for handler-side errors."
        );
    }
    Ok(())
}

// ============================================================================
// Ruler lane primitives — direct pokes for debugging convention drift
// ============================================================================

async fn cmd_rename_lane(socket: Option<&std::path::Path>, index: u32, name: &str) -> Result<()> {
    use daw_proto::ProjectContext;
    use daw_proto::project::ProjectsClient;

    let caller = connection::connect(socket)
        .await
        .wrap_err("connect to fts-extensions socket")?;
    let projects = ProjectsClient::new(caller);

    let start = std::time::Instant::now();
    projects
        .set_ruler_lane_name(ProjectContext::Current, index, name.to_string())
        .await
        .map_err(|e| eyre::eyre!("set_ruler_lane_name rpc failed: {e:?}"))?;
    println!("lane {index} → {:?}   ({:?})", name, start.elapsed());
    Ok(())
}

async fn cmd_set_project_info(
    socket: Option<&std::path::Path>,
    key: &str,
    value: f64,
) -> Result<()> {
    use daw_proto::ProjectContext;
    use daw_proto::project::ProjectsClient;

    let caller = connection::connect(socket)
        .await
        .wrap_err("connect to fts-extensions socket")?;
    let projects = ProjectsClient::new(caller);

    let start = std::time::Instant::now();
    projects
        .set_project_info(ProjectContext::Current, key.to_string(), value)
        .await
        .map_err(|e| eyre::eyre!("set_project_info rpc failed: {e:?}"))?;
    println!("{key} = {value}   ({:?})", start.elapsed());
    Ok(())
}
