//! tui-player — Kontakt-style ratatui browser/player for .signalpack libraries.

use std::collections::BTreeMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};

use clap::Parser;
use crossterm::event::{self, Event, KeyCode, KeyEventKind, KeyModifiers};
use crossterm::execute;
use crossterm::terminal::{
    EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
};
use eyre::{Context, Result};
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, List, ListItem, ListState, Paragraph, Wrap};
use signal_sampler::{AudioStatsSnapshot, PreloadProfile, SamplerRig};

use crossbeam_channel::{Receiver, bounded};
use midir::{MidiInput as MidirInput, MidiInputConnection};

const INSTRUMENT_ID: &str = "tui";

// ── MIDI input ──────────────────────────────────────────────────────────────

/// Raw MIDI event drained from the system, normalised to (status, data1, data2).
#[derive(Debug, Clone, Copy)]
struct MidiEvent {
    status: u8,
    data1: u8,
    data2: u8,
}

/// Holds active MIDI input connections; events arrive on `rx`.
///
/// Opens every available system MIDI input port and merges them into a
/// single channel. Drop = close all connections.
struct MidiInput {
    rx: Receiver<MidiEvent>,
    port_count: usize,
    _connections: Vec<MidiInputConnection<()>>,
}

impl MidiInput {
    fn open_all() -> Option<Self> {
        let probe = MidirInput::new("signal-tui").ok()?;
        let ports = probe.ports();
        if ports.is_empty() {
            return None;
        }
        let (tx, rx) = bounded::<MidiEvent>(512);
        let mut connections = Vec::new();
        for port in &ports {
            let port_name = probe.port_name(port).unwrap_or_else(|_| "unknown".into());
            let Ok(input) = MidirInput::new("signal-tui") else {
                continue;
            };
            let tx = tx.clone();
            match input.connect(
                port,
                "signal-tui-rx",
                move |_ts, msg, _| {
                    if msg.len() >= 3 {
                        let _ = tx.try_send(MidiEvent {
                            status: msg[0],
                            data1: msg[1],
                            data2: msg[2],
                        });
                    } else if msg.len() == 2 {
                        // Channel pressure / program change — pad to 3 with 0.
                        let _ = tx.try_send(MidiEvent {
                            status: msg[0],
                            data1: msg[1],
                            data2: 0,
                        });
                    }
                },
                (),
            ) {
                Ok(c) => connections.push(c),
                Err(e) => tracing::warn!("MIDI: failed to open \"{}\": {}", port_name, e),
            }
        }
        if connections.is_empty() {
            None
        } else {
            Some(Self {
                rx,
                port_count: connections.len(),
                _connections: connections,
            })
        }
    }

    fn drain(&self) -> impl Iterator<Item = MidiEvent> + '_ {
        self.rx.try_iter()
    }
}

fn midi_port_names() -> Vec<String> {
    let Ok(probe) = MidirInput::new("signal-tui") else {
        return Vec::new();
    };
    probe
        .ports()
        .iter()
        .map(|p| probe.port_name(p).unwrap_or_else(|_| "unknown".into()))
        .collect()
}

#[derive(Parser, Debug)]
#[command(name = "tui-player")]
struct Args {
    /// Root directory to scan for .signalpack libraries.
    /// Defaults to `/run/media/AudioHaven/Signal/Libraries` (the canonical
    /// factory-content store). User content lives separately and is wired
    /// in through the registry once the headless browser lands.
    #[arg(default_value = "/run/media/AudioHaven/Signal/Libraries")]
    root: PathBuf,
    /// Write all traces, eprintln output, and audio-engine messages here
    /// instead of to the terminal. Defaults to `/tmp/signal-tui.log`.
    #[arg(long, default_value = "/tmp/signal-tui.log")]
    log_file: PathBuf,
    /// Decoded PCM cache budget in MiB.
    #[arg(long)]
    cache_budget_mib: Option<usize>,
    /// Evict decoded samples when the configured cache budget is exceeded.
    #[arg(long)]
    enforce_cache_budget: bool,
    /// Background preload profile. Defaults to `full` so every key on
    /// the keyboard makes sound once the pack finishes preloading.
    /// `performance` (512 closest-to-C4 samples) leaves most notes silent
    /// on larger libraries.
    #[arg(long, default_value = "full")]
    preload_profile: String,
}

// ── Patch discovery ─────────────────────────────────────────────────────────
//
// Backed by `signal_browser::pack_registry`: walks the root, opens each
// `.signalpack` header (no audio decode), reads the embedded library spec
// for tags / instrument / category. The local `Patch` view is a thin wrapper
// keeping just the fields the TUI actually renders + loads from.

#[derive(Debug, Clone)]
struct Patch {
    /// What kind of file this is (drives load dispatch + icon).
    kind: signal_browser::pack_registry::EntryKind,
    /// `LibrarySpec.name` from the pack header (or filename fallback).
    name: String,
    /// Path to the file to load.
    pack: PathBuf,
    /// Folder segments relative to the root, for tree-style display.
    folder: Vec<String>,
    /// Subtitle line: `instrument · category` (omitted if neither set).
    subtitle: Option<String>,
    /// Vendor badge.
    vendor: String,
    /// Instrument label for sidebar grouping / chip filter.
    instrument: String,
    /// Category label.
    category: String,
    /// Style tokens (e.g. Stylus suite name + bpm).
    style: Vec<String>,
    /// Materialized tag set for filter chips + detail display.
    tags: signal_browser::TagSet,
}

impl Patch {
    fn from_entry(entry: &signal_browser::pack_registry::PackEntry) -> Self {
        let subtitle = match (entry.instrument.as_str(), entry.category.as_str()) {
            ("", "") => None,
            (i, "") => Some(i.to_string()),
            ("", c) => Some(c.to_string()),
            (i, c) => Some(format!("{i} · {c}")),
        };
        Self {
            kind: entry.kind,
            name: entry.name.clone(),
            pack: entry.path.clone(),
            folder: entry.folder.clone(),
            subtitle,
            vendor: entry.vendor.clone(),
            instrument: entry.instrument.clone(),
            category: entry.category.clone(),
            style: entry.style.clone(),
            tags: entry.tags.clone(),
        }
    }
}

#[derive(Debug, Default, Clone)]
struct LibraryNode {
    children: BTreeMap<String, LibraryNode>,
    patches: Vec<Patch>,
}

impl LibraryNode {
    fn insert(&mut self, segments: &[String], patch: Patch) {
        if segments.is_empty() {
            self.patches.push(patch);
            return;
        }
        let head = &segments[0];
        self.children
            .entry(head.clone())
            .or_default()
            .insert(&segments[1..], patch);
    }
}

fn scan(root: &Path) -> signal_browser::pack_registry::ScanReport {
    signal_browser::pack_registry::scan_packs_report(root)
}

/// One Miller column showing the children of a selected node.
#[derive(Debug)]
struct Column {
    title: String,
    items: Vec<Item>,
    state: ListState,
}

/// One entry in a Miller column.
#[derive(Debug, Clone)]
enum Item {
    /// A child folder. `pack_count` is the total under this folder for badge display.
    Folder { name: String, pack_count: usize },
    /// A leaf pack.
    Patch(Patch),
}

/// Build a filtered tree from `patches` and `query`.
fn filtered_tree(patches: &[Patch], query: &str) -> LibraryNode {
    let q = query.trim().to_ascii_lowercase();
    let mut tree = LibraryNode::default();
    for p in patches {
        if !q.is_empty() && !matches_query(p, &q) {
            continue;
        }
        let segments = p.folder.clone();
        tree.insert(&segments, p.clone());
    }
    tree
}

/// Build a column from a `LibraryNode` (folders first alphabetically, then
/// leaf patches). `title` is the column's header.
fn column_from_node(node: &LibraryNode, title: impl Into<String>) -> Column {
    let mut items: Vec<Item> = Vec::new();
    for (name, child) in &node.children {
        items.push(Item::Folder {
            name: name.clone(),
            pack_count: count_patches(child),
        });
    }
    for p in &node.patches {
        items.push(Item::Patch(p.clone()));
    }
    let mut state = ListState::default();
    if !items.is_empty() {
        state.select(Some(0));
    }
    Column {
        title: title.into(),
        items,
        state,
    }
}

fn count_patches(node: &LibraryNode) -> usize {
    let mut n = node.patches.len();
    for c in node.children.values() {
        n += count_patches(c);
    }
    n
}

/// Walk down `tree` along the indices in `path`, returning the resolved
/// `LibraryNode` (or the root if the path is empty / out of range).
fn descend<'a>(tree: &'a LibraryNode, columns: &[Column], path_len: usize) -> &'a LibraryNode {
    let mut node = tree;
    for col in columns.iter().take(path_len) {
        let Some(idx) = col.state.selected() else {
            return node;
        };
        let Some(item) = col.items.get(idx) else {
            return node;
        };
        let Item::Folder { name, .. } = item else {
            return node;
        };
        let Some(child) = node.children.get(name) else {
            return node;
        };
        node = child;
    }
    node
}

fn matches_query(p: &Patch, q: &str) -> bool {
    p.name.to_ascii_lowercase().contains(q)
        || p.folder.iter().any(|s| s.to_ascii_lowercase().contains(q))
        || p.instrument.to_ascii_lowercase().contains(q)
        || p.category.to_ascii_lowercase().contains(q)
        || p.vendor.to_ascii_lowercase().contains(q)
        || p.style.iter().any(|s| s.to_ascii_lowercase().contains(q))
        || p.tags
            .values()
            .any(|t| t.value.to_ascii_lowercase().contains(q))
}

fn initial_scan_error(report: &signal_browser::pack_registry::ScanReport) -> Option<String> {
    if report.loaded_count() == 0 {
        return Some(format!(
            "no Signal loadables found under {} (.signalpack/.signalengine/.signalpreset/.signalmodule/.signalblock)",
            report.root.display()
        ));
    }
    if report.has_failures() {
        return Some(format!(
            "scan loaded {} of {} loadables; {} failed, {} dirs/files skipped",
            report.loaded_count(),
            report.candidate_count,
            report.failed_loadables,
            report.skipped_walk_entries
        ));
    }
    None
}

// ── Keyboard mapping ────────────────────────────────────────────────────────

/// Tracker layout. Offset relative to current octave's C.
fn key_to_semitone(c: char) -> Option<i32> {
    Some(match c {
        // lower row — white keys, current octave
        'z' => 0,
        'x' => 2,
        'c' => 4,
        'v' => 5,
        'b' => 7,
        'n' => 9,
        'm' => 11,
        ',' => 12,
        '.' => 14,
        '/' => 16,
        // lower row — black keys (s/d empty for E, F gap)
        's' => 1,
        'd' => 3,
        'g' => 6,
        'h' => 8,
        'j' => 10,
        'l' => 13,
        ';' => 15,
        // upper row — white keys, +1 octave
        'q' => 12,
        'w' => 14,
        'e' => 16,
        'r' => 17,
        't' => 19,
        'y' => 21,
        'u' => 23,
        'i' => 24,
        'o' => 26,
        'p' => 28,
        // upper row — black keys
        '2' => 13,
        '3' => 15,
        '5' => 18,
        '6' => 20,
        '7' => 22,
        '9' => 25,
        '0' => 27,
        _ => return None,
    })
}

// ── App state ───────────────────────────────────────────────────────────────

struct App {
    #[allow(dead_code)]
    root: PathBuf,
    /// Filtered tree (rebuilt whenever `query` changes).
    tree: LibraryNode,
    /// Stack of Miller columns. `columns[0]` is the root; `columns[i]` shows
    /// the children of the selected folder in `columns[i-1]`.
    columns: Vec<Column>,
    /// Index into `columns` that the cursor is currently in.
    focus_col: usize,
    octave: i32,
    loaded: Option<Patch>,
    loaded_zones: usize,
    last_note: Option<u8>,
    error: Option<String>,
    held: std::collections::HashSet<char>,
    midi_status: String,
    log_path: PathBuf,
    /// All discovered patches; columns view a filtered subset.
    all_patches: Vec<Patch>,
    /// Diagnostics from the filesystem scan that populated `all_patches`.
    scan_report: signal_browser::pack_registry::ScanReport,
    /// Free-text filter — matches name / folder / instrument / category / tag values.
    query: String,
    /// True while the user is typing into the filter at the bottom.
    search_mode: bool,
    /// When true, fire a preview note (~C4) shortly after a pack/preset finishes
    /// loading. Useful for confirming the audio chain on selection. Toggle: F2.
    preview_on_load: bool,
    /// Set of patch file paths the user has starred. Persisted to
    /// `$XDG_CONFIG_HOME/signal/tui-favorites.txt` (or `~/.config/signal/...`).
    /// Toggle on the focused patch with `f`.
    favorites: std::collections::HashSet<PathBuf>,
    /// Resolved location of the favorites file.
    favorites_path: PathBuf,
    preview_epoch: Arc<AtomicU64>,
}

fn favorites_file() -> PathBuf {
    let base = std::env::var_os("XDG_CONFIG_HOME")
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("HOME").map(|h| PathBuf::from(h).join(".config")))
        .unwrap_or_else(|| PathBuf::from("."));
    base.join("signal").join("tui-favorites.txt")
}

fn load_favorites(path: &Path) -> std::collections::HashSet<PathBuf> {
    std::fs::read_to_string(path)
        .ok()
        .map(|s| {
            s.lines()
                .filter(|l| !l.trim().is_empty())
                .map(PathBuf::from)
                .collect()
        })
        .unwrap_or_default()
}

fn save_favorites(path: &Path, favs: &std::collections::HashSet<PathBuf>) {
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let mut sorted: Vec<&PathBuf> = favs.iter().collect();
    sorted.sort();
    let body = sorted
        .iter()
        .map(|p| p.display().to_string())
        .collect::<Vec<_>>()
        .join("\n");
    let _ = std::fs::write(path, body);
}

impl App {
    fn new(root: PathBuf) -> Self {
        let scan_report = scan(&root);
        let all_patches = scan_report
            .entries
            .iter()
            .map(Patch::from_entry)
            .collect::<Vec<_>>();
        let tree = filtered_tree(&all_patches, "");
        let root_label = root
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("(root)")
            .to_string();
        let mut app_columns = vec![column_from_node(&tree, root_label)];
        // Seed: if the root selection is a folder, open it so two columns
        // are visible from the start.
        if let Some(Item::Folder { name, .. }) = app_columns
            .first()
            .and_then(|c| c.state.selected().and_then(|i| c.items.get(i)))
            .cloned()
        {
            let child = tree.children.get(&name).cloned().unwrap_or_default();
            app_columns.push(column_from_node(&child, name));
        }
        Self {
            root,
            tree,
            columns: app_columns,
            focus_col: 0,
            octave: 60,
            loaded: None,
            loaded_zones: 0,
            last_note: None,
            error: initial_scan_error(&scan_report),
            held: std::collections::HashSet::new(),
            midi_status: String::new(),
            log_path: PathBuf::new(),
            all_patches,
            scan_report,
            query: String::new(),
            search_mode: false,
            preview_on_load: true,
            favorites: load_favorites(&favorites_file()),
            favorites_path: favorites_file(),
            preview_epoch: Arc::new(AtomicU64::new(0)),
        }
    }

    /// Rebuild the tree + reset to the root column after a filter change.
    fn refilter(&mut self) {
        self.tree = filtered_tree(&self.all_patches, &self.query);
        let root_label = self
            .root
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("(root)")
            .to_string();
        self.columns = vec![column_from_node(&self.tree, root_label)];
        self.focus_col = 0;
    }

    /// The currently selected item in the focused column, if any.
    fn current_item(&self) -> Option<&Item> {
        let col = self.columns.get(self.focus_col)?;
        let i = col.state.selected()?;
        col.items.get(i)
    }

    /// Move within the current column.
    fn move_selection(&mut self, delta: i32) {
        let Some(col) = self.columns.get_mut(self.focus_col) else {
            return;
        };
        let len = col.items.len();
        if len == 0 {
            return;
        }
        let cur = col.state.selected().unwrap_or(0) as i32;
        let next = (cur + delta).rem_euclid(len as i32) as usize;
        col.state.select(Some(next));
        // After moving, prune deeper columns since the parent selection changed,
        // then auto-open if the new selection is a folder.
        self.columns.truncate(self.focus_col + 1);
        self.auto_open_child();
    }

    /// Push a column for the currently-selected folder, if any.
    fn descend_into(&mut self) {
        let Some(item) = self.current_item().cloned() else {
            return;
        };
        let Item::Folder { name, .. } = item else {
            return;
        };
        // Truncate any old deeper columns so navigation stays consistent.
        self.columns.truncate(self.focus_col + 1);
        let parent_node = descend(&self.tree, &self.columns, self.focus_col + 1);
        let col = column_from_node(parent_node, name);
        self.columns.push(col);
        self.focus_col += 1;
    }

    /// If the focused selection is a folder, open it as the next column
    /// (without moving focus). Used after vertical moves so the user always
    /// sees what's "inside" the current row.
    fn auto_open_child(&mut self) {
        let Some(item) = self.current_item().cloned() else {
            return;
        };
        if let Item::Folder { name, .. } = item {
            let parent_node = descend(&self.tree, &self.columns, self.focus_col + 1);
            let col = column_from_node(parent_node, name);
            self.columns.push(col);
        }
    }

    /// Pop the deepest column. The cursor stays in the column it was in;
    /// if it was on the deepest one, the focus moves to its parent.
    fn ascend(&mut self) {
        if self.focus_col > 0 {
            self.focus_col -= 1;
            self.columns.truncate(self.focus_col + 1);
        }
    }

    /// Move focus right across columns. Auto-descends into a folder if
    /// nothing's open to the right yet.
    fn focus_right(&mut self) {
        if self.focus_col + 1 < self.columns.len() {
            self.focus_col += 1;
        } else {
            // Try to descend.
            self.descend_into();
        }
    }

    fn focus_left(&mut self) {
        if self.focus_col > 0 {
            self.focus_col -= 1;
        }
    }

    fn load_selected(&mut self, player: &SamplerRig) {
        let patch = match self.current_item() {
            Some(Item::Patch(p)) => p.clone(),
            Some(Item::Folder { name, .. }) => {
                self.error = Some(format!(
                    "'{name}' is a folder — press → or Enter to open, then ↓/Enter on a pack"
                ));
                return;
            }
            None => {
                self.error = Some("nothing selected".into());
                return;
            }
        };
        use signal_browser::pack_registry::EntryKind;
        tracing::info!(
            kind = patch.kind.label(),
            path = %patch.pack.display(),
            "load_selected: loading"
        );
        self.preview_epoch.fetch_add(1, Ordering::Relaxed);
        self.held.clear();
        player.all_notes_off(INSTRUMENT_ID);
        player.unload_instrument(INSTRUMENT_ID);

        let result = match patch.kind {
            EntryKind::Pack => {
                self.loaded_zones = signal_sampler::read_pack_header(&patch.pack)
                    .map(|h| h.spec.zones.len() + h.spec.grooves.len())
                    .unwrap_or(0);
                player.load_pack(INSTRUMENT_ID, &patch.pack).map(|_| ())
            }
            EntryKind::Block => {
                self.loaded_zones = 0;
                player.load_block(INSTRUMENT_ID, &patch.pack).map(|_| ())
            }
            EntryKind::Engine => {
                self.loaded_zones = 0;
                player.load_engine(INSTRUMENT_ID, &patch.pack).map(|_| ())
            }
            EntryKind::Preset => {
                self.loaded_zones = 0;
                // Presets register N sub-instruments under "tui:<engine_id>";
                // MIDI input keyed off "tui" routes via the bank's
                // note_routing populated by the preset.
                player.load_preset(INSTRUMENT_ID, &patch.pack).map(|_| ())
            }
            EntryKind::Module => Err(eyre::eyre!(
                "modules can't be loaded standalone — pick an Engine or Preset"
            )),
        };

        match result {
            Ok(()) => {
                tracing::info!(name = %patch.name, kind = patch.kind.label(), "load_selected: loaded ok");
                self.error = None;
                self.loaded = Some(patch);
                self.schedule_preview(player);
            }
            Err(e) => {
                tracing::warn!(err = %e, "load_selected: load failed");
                self.error = Some(format!("load: {e}"));
            }
        }
    }

    fn schedule_preview(&self, player: &SamplerRig) {
        if !self.preview_on_load {
            return;
        }
        let player = player.clone();
        let preview_epoch = Arc::clone(&self.preview_epoch);
        let epoch = preview_epoch.load(Ordering::Relaxed);
        std::thread::Builder::new()
            .name("signal-tui-preview".into())
            .spawn(move || {
                let start = Instant::now();
                loop {
                    if preview_epoch.load(Ordering::Relaxed) != epoch {
                        return;
                    }
                    let (loaded, total) = player.preload_progress(INSTRUMENT_ID);
                    if total == 0 || loaded > 0 || start.elapsed() >= Duration::from_secs(5) {
                        break;
                    }
                    std::thread::sleep(Duration::from_millis(50));
                }
                if preview_epoch.load(Ordering::Relaxed) != epoch {
                    return;
                }
                tracing::info!(note = 60, "preview note_on");
                player.note_on(INSTRUMENT_ID, 60, 100);
                std::thread::sleep(std::time::Duration::from_millis(900));
                if preview_epoch.load(Ordering::Relaxed) != epoch {
                    return;
                }
                player.note_off(INSTRUMENT_ID, 60);
            })
            .ok();
    }

    fn note_for_key(&self, c: char) -> Option<u8> {
        let semi = key_to_semitone(c)?;
        let n = self.octave + semi;
        if (0..=127).contains(&n) {
            Some(n as u8)
        } else {
            None
        }
    }
}

// ── Rendering ───────────────────────────────────────────────────────────────

fn ui(
    f: &mut ratatui::Frame,
    app: &mut App,
    voice_count: usize,
    preload: (usize, usize),
    audio_stats: AudioStatsSnapshot,
) {
    let area = f.area();
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Min(10),
            Constraint::Length(6),
            Constraint::Length(1),
        ])
        .split(area);

    let top = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(40),
            Constraint::Percentage(35),
            Constraint::Percentage(25),
        ])
        .split(chunks[0]);

    // Split top row: N nav columns to the left, Loaded panel on the right.
    let nav_area = Rect {
        x: top[0].x,
        y: top[0].y,
        width: top[0].width + top[1].width,
        height: top[0].height,
    };
    render_columns(f, app, nav_area);
    render_loaded(f, app, top[2], voice_count, preload, audio_stats.clone());
    render_keyboard(f, app, chunks[1]);
    render_status(f, app, chunks[2], voice_count, audio_stats);
}

fn focus_style(focused: bool) -> Style {
    if focused {
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD)
    } else {
        Style::default().fg(Color::Gray)
    }
}

/// Render the Miller-column nav. If there are too many columns to fit in
/// `area`, only the rightmost `MAX_VISIBLE_COLS` are drawn (leftmost
/// scrolled out — the user can still navigate to them with ←).
fn render_columns(f: &mut ratatui::Frame, app: &mut App, area: Rect) {
    const MAX_VISIBLE_COLS: usize = 4;
    if app.all_patches.is_empty() {
        let message = vec![
            Line::from(Span::styled(
                "No Signal loadables found",
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD),
            )),
            Line::from(format!("root: {}", app.root.display())),
            Line::from(".signalpack, .signalengine, .signalpreset, .signalmodule, .signalblock"),
            Line::from(format!(
                "scan candidates: {}  failed: {}  skipped: {}",
                app.scan_report.candidate_count,
                app.scan_report.failed_loadables,
                app.scan_report.skipped_walk_entries
            )),
        ];
        let panel = Paragraph::new(message)
            .block(Block::default().borders(Borders::ALL).title("Library"))
            .wrap(Wrap { trim: true });
        f.render_widget(panel, area);
        return;
    }
    let total = app.columns.len();
    let visible_start = total.saturating_sub(MAX_VISIBLE_COLS);
    let visible_count = total - visible_start;
    if visible_count == 0 {
        return;
    }
    let constraints: Vec<Constraint> = (0..visible_count)
        .map(|_| Constraint::Ratio(1, visible_count as u32))
        .collect();
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(constraints)
        .split(area);

    for (slot, col_idx) in (visible_start..total).enumerate() {
        let col = &mut app.columns[col_idx];
        let focused = col_idx == app.focus_col;
        let title = if visible_start > 0 && slot == 0 {
            format!("… / {}", col.title)
        } else {
            col.title.clone()
        };
        let items: Vec<ListItem> = col
            .items
            .iter()
            .map(|it| match it {
                Item::Folder { name, pack_count } => ListItem::new(Line::from(vec![
                    Span::styled("▸ ", Style::default().fg(Color::DarkGray)),
                    Span::raw(name.clone()),
                    Span::styled(
                        format!("  ({pack_count})"),
                        Style::default().fg(Color::DarkGray),
                    ),
                ])),
                Item::Patch(p) => {
                    let icon = format!("{}  ", p.kind.icon());
                    let star = if app.favorites.contains(&p.pack) {
                        "★ "
                    } else {
                        ""
                    };
                    ListItem::new(Line::from(vec![
                        Span::styled(icon, Style::default().fg(Color::Cyan)),
                        Span::styled(star, Style::default().fg(Color::Yellow)),
                        Span::raw(p.name.clone()),
                    ]))
                }
            })
            .collect();
        let list = List::new(items)
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .title(title)
                    .border_style(focus_style(focused)),
            )
            .highlight_style(
                Style::default()
                    .bg(Color::DarkGray)
                    .add_modifier(Modifier::BOLD),
            )
            .highlight_symbol("> ");
        f.render_stateful_widget(list, chunks[slot], &mut col.state);
    }
}

fn render_loaded(
    f: &mut ratatui::Frame,
    app: &App,
    area: Rect,
    voice_count: usize,
    preload: (usize, usize),
    audio_stats: AudioStatsSnapshot,
) {
    let mut lines: Vec<Line> = Vec::new();
    if let Some(p) = &app.loaded {
        lines.push(Line::from(Span::styled(
            p.name.clone(),
            Style::default()
                .fg(Color::Green)
                .add_modifier(Modifier::BOLD),
        )));
        if let Some(sub) = p.subtitle.as_ref() {
            lines.push(Line::from(Span::styled(
                sub.clone(),
                Style::default().fg(Color::Gray),
            )));
        }
        if !p.vendor.is_empty() {
            lines.push(Line::from(Span::styled(
                format!("vendor: {}", p.vendor),
                Style::default().fg(Color::DarkGray),
            )));
        }
        lines.push(Line::from(format!("{} zones", app.loaded_zones)));
        lines.push(Line::from(format!("{} voices", voice_count)));
        lines.push(Line::from(format!(
            "render: {} us last / {} us max",
            audio_stats.last_render_us, audio_stats.max_render_us
        )));
        lines.push(Line::from(format!(
            "resize events: {}",
            audio_stats.resize_events
        )));
        lines.push(Line::from(format!(
            "misses: cache {} / map {}",
            audio_stats.cache_misses, audio_stats.sample_misses
        )));
        if let Some(path) = audio_stats.recent_cache_misses.last() {
            lines.push(Line::from(Span::styled(
                format!("last cache miss: {path}"),
                Style::default().fg(Color::DarkGray),
            )));
        }
        if let Some(label) = audio_stats.recent_sample_misses.last() {
            lines.push(Line::from(Span::styled(
                format!("last map miss: {label}"),
                Style::default().fg(Color::DarkGray),
            )));
        }
        let cache_mib = audio_stats.loaded_sample_bytes as f64 / 1024.0 / 1024.0;
        if let Some(budget_bytes) = audio_stats.cache_budget_bytes {
            let budget_mib = budget_bytes as f64 / 1024.0 / 1024.0;
            let over_mib = audio_stats.cache_over_budget_bytes as f64 / 1024.0 / 1024.0;
            let cache_label = if audio_stats.cache_over_budget_bytes > 0 {
                format!("cache: {cache_mib:.1}/{budget_mib:.1} MiB, over {over_mib:.1}")
            } else {
                format!("cache: {cache_mib:.1}/{budget_mib:.1} MiB")
            };
            let cache_color = if audio_stats.cache_over_budget_bytes > 0 {
                Color::Yellow
            } else {
                Color::Gray
            };
            lines.push(Line::from(Span::styled(
                cache_label,
                Style::default().fg(cache_color),
            )));
        } else {
            lines.push(Line::from(format!("cache: {cache_mib:.1} MiB decoded")));
        }
        let (l, t) = preload;
        if t > 0 {
            let pct = if t == 0 { 100 } else { (l * 100) / t };
            let label = if l < t {
                format!("preload: {l}/{t} ({pct}%) — streaming")
            } else {
                format!("preload: {l}/{t} (ready)")
            };
            let color = if l < t { Color::Yellow } else { Color::Green };
            lines.push(Line::from(Span::styled(label, Style::default().fg(color))));
        }
        let last = app
            .last_note
            .map(|n| format!("last: {} ({})", note_name(n), n))
            .unwrap_or_else(|| "last: —".into());
        lines.push(Line::from(last));
        if !p.style.is_empty() {
            lines.push(Line::from(""));
            lines.push(Line::from(Span::styled(
                "style".to_string(),
                Style::default().fg(Color::DarkGray),
            )));
            for s in &p.style {
                lines.push(Line::from(format!("  {s}")));
            }
        }
        let tag_count = p.tags.values().count();
        if tag_count > 0 {
            lines.push(Line::from(""));
            lines.push(Line::from(Span::styled(
                "tags".to_string(),
                Style::default().fg(Color::DarkGray),
            )));
            for t in p.tags.values() {
                lines.push(Line::from(format!(
                    "  [{}] {}",
                    t.category.as_str(),
                    t.value
                )));
            }
        }
    } else {
        lines.push(Line::from(Span::styled(
            "(nothing loaded)",
            Style::default().fg(Color::DarkGray),
        )));
        lines.push(Line::from("Enter on a patch to load"));
        lines.push(Line::from(format!(
            "{} loadables scanned",
            app.scan_report.loaded_count()
        )));
        if app.scan_report.has_failures() {
            lines.push(Line::from(Span::styled(
                format!(
                    "{} failed, {} skipped",
                    app.scan_report.failed_loadables, app.scan_report.skipped_walk_entries
                ),
                Style::default().fg(Color::Yellow),
            )));
        }
    }
    if !app.midi_status.is_empty() {
        lines.push(Line::from(""));
        lines.push(Line::from(Span::styled(
            app.midi_status.clone(),
            Style::default().fg(Color::Cyan),
        )));
    }
    if !app.log_path.as_os_str().is_empty() {
        lines.push(Line::from(Span::styled(
            format!("log: {}", app.log_path.display()),
            Style::default().fg(Color::DarkGray),
        )));
    }
    let p = Paragraph::new(lines)
        .block(Block::default().borders(Borders::ALL).title("Loaded"))
        .wrap(Wrap { trim: true });
    f.render_widget(p, area);
}

fn render_keyboard(f: &mut ratatui::Frame, app: &App, area: Rect) {
    let search_line = if app.search_mode {
        Line::from(vec![
            Span::styled(
                "/ ",
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD),
            ),
            Span::styled(app.query.clone(), Style::default().fg(Color::Yellow)),
            Span::styled(
                "_",
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::REVERSED),
            ),
            Span::raw("    Enter: apply  Esc: clear"),
        ])
    } else if !app.query.is_empty() {
        Line::from(vec![
            Span::styled("filter: ", Style::default().fg(Color::DarkGray)),
            Span::styled(app.query.clone(), Style::default().fg(Color::Yellow)),
            Span::styled(
                "    /: edit  Esc: clear",
                Style::default().fg(Color::DarkGray),
            ),
        ])
    } else {
        Line::from(Span::styled(
            "/: search   Space: panic   [/]: octave   Tab: focus   Enter: load   f: ★fav   F2: preview   q/Esc: quit",
            Style::default().fg(Color::DarkGray),
        ))
    };
    let octave_label = format!(
        "octave: {} (C{})    preview-on-load: {}",
        app.octave,
        app.octave / 12 - 1,
        if app.preview_on_load { "ON" } else { "off" },
    );
    let row1 = "white: z x c v b n m , . /    upper: q w e r t y u i o p";
    let row2 = "black: s d _ g h j _ l ; _    upper#: 2 3 _ 5 6 7 _ 9 0";
    let lines = vec![
        search_line,
        Line::from(octave_label),
        Line::from(row1),
        Line::from(row2),
    ];
    let p = Paragraph::new(lines).block(Block::default().borders(Borders::ALL).title("Keyboard"));
    f.render_widget(p, area);
}

fn render_status(
    f: &mut ratatui::Frame,
    app: &App,
    area: Rect,
    voice_count: usize,
    audio_stats: AudioStatsSnapshot,
) {
    let err = app.error.as_deref().unwrap_or("");
    let last = app
        .last_note
        .map(|n| format!("{} ({})", note_name(n), n))
        .unwrap_or_else(|| "—".into());
    let cursor = match app.current_item() {
        Some(Item::Folder { name, .. }) => format!("📁 {name}"),
        Some(Item::Patch(p)) => format!("🎵 {}", p.name),
        None => "—".to_string(),
    };
    let loaded_name = app
        .loaded
        .as_ref()
        .map(|p| p.name.as_str())
        .unwrap_or("none");
    let err_part = if err.is_empty() {
        String::new()
    } else {
        format!(" | err: {err}")
    };
    let audio_part = format!(
        " | xruns:{} locks:{} resize:{} q:{}/drop:{} miss:{}/{}{}",
        audio_stats.callback_overruns,
        audio_stats.lock_misses,
        audio_stats.resize_events,
        audio_stats.pending_events,
        audio_stats.dropped_events,
        audio_stats.cache_misses,
        audio_stats.sample_misses,
        if audio_stats.cache_over_budget_bytes > 0 {
            format!(
                " cache-over:{:.1}MiB",
                audio_stats.cache_over_budget_bytes as f64 / 1024.0 / 1024.0
            )
        } else {
            String::new()
        }
    );
    let scan_part = if app.scan_report.has_failures() {
        format!(
            " | scan:{}/{} fail:{} skip:{}",
            app.scan_report.loaded_count(),
            app.scan_report.candidate_count,
            app.scan_report.failed_loadables,
            app.scan_report.skipped_walk_entries
        )
    } else {
        format!(" | scan:{}", app.scan_report.loaded_count())
    };
    let txt = format!(
        " cursor: {cursor} | loaded: {loaded_name} | voices: {voice_count} | steals:{} | last: {last} | oct: {}{scan_part}{audio_part}{err_part}",
        audio_stats.stolen_voices, app.octave
    );
    let p = Paragraph::new(txt).style(Style::default().bg(Color::DarkGray).fg(Color::White));
    f.render_widget(p, area);
}

fn note_name(n: u8) -> String {
    const NAMES: [&str; 12] = [
        "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B",
    ];
    let octave = (n as i32) / 12 - 1;
    format!("{}{}", NAMES[(n % 12) as usize], octave)
}

// ── Main loop ───────────────────────────────────────────────────────────────

struct TerminalGuard {
    terminal: Option<Terminal<CrosstermBackend<io::Stdout>>>,
}

impl TerminalGuard {
    fn enter() -> Result<Self> {
        enable_raw_mode()?;
        let mut stdout = io::stdout();
        execute!(stdout, EnterAlternateScreen)?;
        let backend = CrosstermBackend::new(stdout);
        let terminal = Terminal::new(backend)?;
        Ok(Self {
            terminal: Some(terminal),
        })
    }

    fn terminal(&mut self) -> &mut Terminal<CrosstermBackend<io::Stdout>> {
        self.terminal.as_mut().expect("terminal taken")
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        let _ = disable_raw_mode();
        let _ = execute!(io::stdout(), LeaveAlternateScreen);
        if let Some(mut t) = self.terminal.take() {
            let _ = t.show_cursor();
        }
    }
}

fn voice_count(player: &SamplerRig) -> usize {
    player.active_voices(INSTRUMENT_ID)
}

/// Redirect this process's stderr file descriptor onto an open file.
/// `eprintln!` from this binary and any C-side messages from
/// cpal/alsa/midir written to fd 2 land in the log instead of corrupting
/// the alt-screen. **Stdout is left alone** — ratatui's crossterm backend
/// draws there, and redirecting it would send the alt-screen escapes
/// into the log instead of the terminal.
#[cfg(unix)]
fn redirect_std_streams(target: &std::fs::File) {
    use std::os::fd::AsRawFd;
    let target_fd = target.as_raw_fd();
    // SAFETY: dup2 onto a valid open fd. Side effects are limited to fd 2.
    unsafe {
        libc::dup2(target_fd, libc::STDERR_FILENO);
    }
}

#[cfg(not(unix))]
fn redirect_std_streams(_target: &std::fs::File) {}

fn run() -> Result<()> {
    let args = Args::parse();

    if !args.root.is_dir() {
        eyre::bail!("not a directory: {}", args.root.display());
    }

    // Open the log file before anything writes to stderr. We redirect the
    // process's raw stderr/stdout fds onto the file so eprintln! (and any
    // C-side prints from cpal/alsa/midir) land in the log rather than
    // corrupting the alt-screen. tracing also writes here.
    let log_handle = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&args.log_file)
        .with_context(|| format!("opening log file {}", args.log_file.display()))?;
    redirect_std_streams(&log_handle);

    let log_writer = std::sync::Mutex::new(log_handle.try_clone()?);
    let _ = tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info")),
        )
        .with_writer(move || {
            log_writer
                .lock()
                .map(|g| g.try_clone().expect("clone log fd"))
                .expect("log writer poisoned")
        })
        .with_ansi(false)
        .try_init();

    let cache_budget_bytes = args
        .cache_budget_mib
        .map(|mib| mib.saturating_mul(1024 * 1024));
    let player = SamplerRig::with_device_config_and_cache_budget(
        None,
        None,
        // 1024 frames = ~23 ms latency at 44.1 kHz. Larger than the
        // default 256 (5.8 ms), but on non-RT user threads under PipeWire
        // the scheduler regularly pushes the audio callback past a 5.8 ms
        // budget, which produces audible crackling. 23 ms is still
        // comfortable for a keyboard player.
        Some(1024),
        cache_budget_bytes,
    )
    .wrap_err("opening cpal output")?;
    let preload_profile = PreloadProfile::from_name(&args.preload_profile)
        .ok_or_else(|| eyre::eyre!("unknown --preload-profile: {}", args.preload_profile))?;
    player.set_preload_profile(preload_profile);

    let midi = MidiInput::open_all();
    let mut app = App::new(args.root);
    app.log_path = args.log_file.clone();
    tracing::info!(
        root = %app.scan_report.root.display(),
        loaded = app.scan_report.loaded_count(),
        candidates = app.scan_report.candidate_count,
        failed = app.scan_report.failed_loadables,
        skipped = app.scan_report.skipped_walk_entries,
        "library scan complete"
    );
    for err in &app.scan_report.errors {
        tracing::warn!(
            path = err
                .path
                .as_ref()
                .map(|p| p.display().to_string())
                .unwrap_or_else(|| "<unknown>".to_string()),
            error = %err.message,
            "library scan skipped entry"
        );
    }
    if let Some(m) = midi.as_ref() {
        let names = midi_port_names();
        app.midi_status = format!(
            "MIDI: {} port{} ({})",
            m.port_count,
            if m.port_count == 1 { "" } else { "s" },
            names.join(", ")
        );
    } else {
        app.midi_status = "MIDI: no ports".into();
    }

    // Auto-load a sensible default patch so the user doesn't have to navigate
    // before testing audio. Picks Rhodes - LA Custom if present, else the first
    // favorite, else nothing. The TUI's preview-on-load thread (F2 toggle)
    // will fire C4 once preload completes.
    let default_patch = app
        .all_patches
        .iter()
        .find(|p| p.name.contains("Rhodes - Classic"))
        .or_else(|| {
            app.all_patches
                .iter()
                .find(|p| p.name.contains("Rhodes - LA Custom"))
        })
        .or_else(|| {
            app.all_patches
                .iter()
                .find(|p| app.favorites.contains(&p.pack))
        })
        .cloned();
    if let Some(p) = default_patch {
        tracing::info!(name = %p.name, path = %p.pack.display(), "auto-loading default patch");
        app.preview_epoch.fetch_add(1, Ordering::Relaxed);
        let result = match p.kind {
            signal_browser::pack_registry::EntryKind::Pack => {
                app.loaded_zones = signal_sampler::read_pack_header(&p.pack)
                    .map(|h| h.spec.zones.len() + h.spec.grooves.len())
                    .unwrap_or(0);
                player.load_pack(INSTRUMENT_ID, &p.pack).map(|_| ())
            }
            signal_browser::pack_registry::EntryKind::Block => {
                app.loaded_zones = 0;
                player.load_block(INSTRUMENT_ID, &p.pack).map(|_| ())
            }
            signal_browser::pack_registry::EntryKind::Engine => {
                app.loaded_zones = 0;
                player.load_engine(INSTRUMENT_ID, &p.pack).map(|_| ())
            }
            signal_browser::pack_registry::EntryKind::Preset => {
                app.loaded_zones = 0;
                player.load_preset(INSTRUMENT_ID, &p.pack).map(|_| ())
            }
            signal_browser::pack_registry::EntryKind::Module => Ok(()),
        };
        if let Err(e) = result {
            tracing::warn!(err = %e, "default load failed");
        } else {
            app.held.clear();
            app.loaded = Some(p);
            app.schedule_preview(&player);
        }
    }

    let mut guard = TerminalGuard::enter()?;
    let tick = Duration::from_millis(33);
    let mut last_draw = Instant::now() - tick;

    loop {
        // Drain MIDI events first so latency is bounded by the poll interval.
        if let Some(midi) = midi.as_ref() {
            for ev in midi.drain() {
                let kind = ev.status & 0xF0;
                match kind {
                    0x90 if ev.data2 > 0 => {
                        player.note_on(INSTRUMENT_ID, ev.data1, ev.data2);
                        app.last_note = Some(ev.data1);
                    }
                    // Note-on with vel 0 is treated as note-off per MIDI spec.
                    0x90 => player.note_off(INSTRUMENT_ID, ev.data1),
                    0x80 => player.note_off(INSTRUMENT_ID, ev.data1),
                    0xB0 => player.cc(INSTRUMENT_ID, ev.data1, ev.data2),
                    _ => {}
                }
            }
        }

        if last_draw.elapsed() >= tick {
            let vc = voice_count(&player);
            let preload = player.preload_progress(INSTRUMENT_ID);
            let audio_stats = player.audio_stats();
            if args.enforce_cache_budget && audio_stats.cache_over_budget_bytes > 0 {
                let evicted = player.evict_cache_over_budget();
                if evicted.evicted > 0 {
                    tracing::info!(
                        evicted = evicted.evicted,
                        freed_mib = evicted.bytes_freed as f64 / 1024.0 / 1024.0,
                        before_mib = evicted.bytes_before as f64 / 1024.0 / 1024.0,
                        after_mib = evicted.bytes_after as f64 / 1024.0 / 1024.0,
                        "signal-tui: enforced sampler cache budget"
                    );
                }
            }
            guard
                .terminal()
                .draw(|f| ui(f, &mut app, vc, preload, audio_stats))
                .wrap_err("draw")?;
            last_draw = Instant::now();
        }

        if !event::poll(Duration::from_millis(10))? {
            continue;
        }
        match event::read()? {
            Event::Key(k) => match k.kind {
                KeyEventKind::Press => {
                    if k.modifiers.contains(KeyModifiers::CONTROL)
                        && matches!(k.code, KeyCode::Char('c'))
                    {
                        player.all_notes_off(INSTRUMENT_ID);
                        break;
                    }
                    // Search mode swallows printable input until Enter / Esc.
                    if app.search_mode {
                        match k.code {
                            KeyCode::Esc => {
                                app.search_mode = false;
                                app.query.clear();
                                app.refilter();
                            }
                            KeyCode::Enter => {
                                app.search_mode = false;
                                app.refilter();
                            }
                            KeyCode::Backspace => {
                                app.query.pop();
                                app.refilter();
                            }
                            KeyCode::Char(c) => {
                                app.query.push(c);
                                app.refilter();
                            }
                            _ => {}
                        }
                        continue;
                    }
                    match k.code {
                        KeyCode::Esc => {
                            player.all_notes_off(INSTRUMENT_ID);
                            break;
                        }
                        KeyCode::Char('q') => {
                            player.all_notes_off(INSTRUMENT_ID);
                            break;
                        }
                        KeyCode::Char('/') => {
                            app.search_mode = true;
                            app.query.clear();
                        }
                        KeyCode::Tab | KeyCode::Right => app.focus_right(),
                        KeyCode::BackTab | KeyCode::Left => app.focus_left(),
                        KeyCode::Up => app.move_selection(-1),
                        KeyCode::Down => app.move_selection(1),
                        // Enter on a folder descends; Enter on a leaf loads.
                        KeyCode::Enter => match app.current_item() {
                            Some(Item::Folder { .. }) => app.descend_into(),
                            Some(Item::Patch(_)) => app.load_selected(&player),
                            None => {}
                        },
                        KeyCode::Backspace => app.ascend(),
                        KeyCode::F(2) => {
                            app.preview_on_load = !app.preview_on_load;
                            tracing::info!(on = app.preview_on_load, "preview toggle");
                        }
                        KeyCode::Char(' ') => {
                            app.preview_epoch.fetch_add(1, Ordering::Relaxed);
                            app.held.clear();
                            player.panic(INSTRUMENT_ID);
                            tracing::info!("panic: all sound off");
                        }
                        KeyCode::Char('f') | KeyCode::Char('F') => {
                            // Star/unstar the currently focused patch.
                            // `f` is not in the QWERTY piano map so it's safe
                            // to claim. Saves immediately so a crash doesn't
                            // lose the star.
                            if let Some(Item::Patch(p)) = app.current_item().cloned() {
                                let pth = p.pack.clone();
                                let was = app.favorites.remove(&pth);
                                if !was {
                                    app.favorites.insert(pth);
                                }
                                save_favorites(&app.favorites_path, &app.favorites);
                            }
                        }
                        KeyCode::Char('[') => {
                            app.octave = (app.octave - 12).max(0);
                        }
                        KeyCode::Char(']') => {
                            app.octave = (app.octave + 12).min(108);
                        }
                        KeyCode::Char(c) => {
                            // Auto-repeat from the OS will fire repeated Press
                            // events. Skip if already held.
                            if app.held.contains(&c) {
                                continue;
                            }
                            if let Some(note) = app.note_for_key(c) {
                                app.held.insert(c);
                                player.note_on(INSTRUMENT_ID, note, 100);
                                app.last_note = Some(note);
                            }
                        }
                        _ => {}
                    }
                }
                KeyEventKind::Release => {
                    if let KeyCode::Char(c) = k.code {
                        if app.held.remove(&c) {
                            if let Some(note) = app.note_for_key(c) {
                                player.note_off(INSTRUMENT_ID, note);
                            }
                        }
                    }
                }
                _ => {}
            },
            Event::Resize(_, _) => {
                // ratatui handles next draw; nothing to do.
            }
            _ => {}
        }
    }

    drop(guard);
    Ok(())
}

fn main() -> Result<()> {
    run()
}
