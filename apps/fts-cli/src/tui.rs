//! `fts -i` — interactive ratatui dashboard.
//!
//! One subscription to `EventBus::subscribe` multiplexes every domain
//! REAPER exposes (transport, track, marker, region, tempo_map) over
//! one Vox channel. The TUI maintains local counters / latest values
//! and a scrolling event log purely from push events — no per-domain
//! polling, no per-tick RPC. Mode + project are still polled at 1Hz
//! since they don't have push streams yet.

use std::borrow::Cow;
use std::collections::VecDeque;
use std::io;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};

use crossterm::event::{self, Event, KeyCode, KeyEventKind, KeyModifiers};
use crossterm::execute;
use crossterm::terminal::{
    EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
};
use daw_proto::event_bus::{BusFilter, DawEvent, EventBusClient};
use daw_proto::marker::{MarkerEvent, MarkersClient};
use daw_proto::project::{ProjectEvent, ProjectsClient};
use daw_proto::region::{RegionEvent, RegionsClient};
use daw_proto::tempo_map::{TempoMapClient, TempoMapEvent};
use daw_proto::track::{TrackEvent, TracksClient};
use daw_proto::transport::TransportEvent;
use daw_proto::primitives::Position;
use daw_proto::{PlayState, ProjectContext, ProjectInfo, TimeSignature};
use eyre::{Result, WrapErr, eyre};
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph};
use session_proto::services::SessionModeServiceClient;
use tokio::sync::Mutex;
use tokio::time::interval;

/// Bounded rolling window of RPC round-trip durations. Computes
/// min / avg / p95 / max for the Latency panel — keeps the
/// computation O(N log N) on the bounded sample set rather than
/// maintaining a heap; the sample count is small (last ~120 calls)
/// so the cost is negligible.
#[derive(Clone, Default)]
struct LatencyStats {
    samples: VecDeque<Duration>,
}

impl LatencyStats {
    const MAX_SAMPLES: usize = 120;

    fn push(&mut self, d: Duration) {
        if self.samples.len() == Self::MAX_SAMPLES {
            self.samples.pop_front();
        }
        self.samples.push_back(d);
    }

    fn summary(&self) -> Option<(Duration, Duration, Duration, Duration, usize)> {
        if self.samples.is_empty() {
            return None;
        }
        let mut sorted: Vec<Duration> = self.samples.iter().copied().collect();
        sorted.sort_unstable();
        let n = sorted.len();
        let min = sorted[0];
        let max = sorted[n - 1];
        let avg = sorted.iter().sum::<Duration>() / n as u32;
        let p95_idx = ((n as f64) * 0.95).ceil() as usize - 1;
        let p95 = sorted[p95_idx.min(n - 1)];
        Some((min, avg, p95, max, n))
    }
}

/// Snapshot of remote state rendered each frame.
#[derive(Default, Clone)]
struct Snapshot {
    // Polled at ~1Hz via RPC (no push stream available)
    mode: Option<String>,
    project: Option<ProjectInfo>,
    /// GUID of the currently-active project — used to filter
    /// PositionTicks (publisher emits one per open tab; we only
    /// display the active one).
    active_project_guid: Option<String>,
    // Pushed via EventBus / transport stream
    tempo: Option<f64>,
    time_signature: Option<TimeSignature>,
    play_state: Option<PlayState>,
    playhead: Option<Position>,
    edit_cursor: Option<Position>,
    is_playing: Option<bool>,
    // Counters maintained from push events (seeded once at startup)
    counts: Counts,
    // Diagnostics
    latency: LatencyStats,
    last_error: Option<String>,
    last_tick_at: Option<Instant>,
    poll_ticks: u64,
    stream_ticks: u64,
    /// Most recent push events for the live event log panel.
    event_log: VecDeque<EventLogEntry>,
}

#[derive(Clone)]
struct EventLogEntry {
    at: Instant,
    domain: &'static str,
    summary: String,
}

const EVENT_LOG_CAP: usize = 200;

/// Per-project entity counts the dashboard maintains from push events.
/// Cleared together on project switch so stale numbers from a previous
/// tab never linger.
#[derive(Default, Clone)]
struct Counts {
    tracks: Option<u32>,
    markers: Option<u32>,
    regions: Option<u32>,
    tempo_points: Option<u32>,
}

impl Counts {
    fn clear(&mut self) {
        *self = Self::default();
    }
}

pub async fn run(socket: Option<PathBuf>) -> Result<()> {
    let caller = session_cli::connection::connect(socket.as_deref())
        .await
        .wrap_err("connect to fts-extensions socket")?;

    let mode_client = SessionModeServiceClient::new(caller.clone());
    let projects_client = ProjectsClient::new(caller.clone());
    let event_bus_client = EventBusClient::new(caller.clone());
    let tracks_client = TracksClient::new(caller.clone());
    let markers_client = MarkersClient::new(caller.clone());
    let regions_client = RegionsClient::new(caller.clone());
    let tempo_map_client = TempoMapClient::new(caller.clone());

    let state = Arc::new(Mutex::new(Snapshot::default()));

    // Seed all counts in parallel so the UI shows real numbers before
    // any events have fired.
    {
        let s_state = state.clone();
        tokio::spawn(async move {
            let (t, m, r, tp) = tokio::join!(
                async { tracks_client.count(ProjectContext::Current).await.ok() },
                async { markers_client.count(ProjectContext::Current).await.ok() },
                async { regions_client.count(ProjectContext::Current).await.ok() },
                async {
                    tempo_map_client
                        .tempo_point_count(ProjectContext::Current)
                        .await
                        .ok()
                },
            );
            let mut guard = s_state.lock().await;
            if let Some(v) = t {
                guard.counts.tracks = Some(v);
            }
            if let Some(v) = m {
                guard.counts.markers = Some(v);
            }
            if let Some(v) = r {
                guard.counts.regions = Some(v);
            }
            if let Some(v) = tp {
                guard.counts.tempo_points = Some(v);
            }
        });
    }

    // ── Push: subscribe to EVERYTHING via EventBus ────────────────
    let stream_state = state.clone();
    let stream_projects = projects_client.clone();
    let stream_handle = tokio::spawn(async move {
        if let Err(e) = run_event_bus(event_bus_client, stream_projects, stream_state.clone()).await
        {
            let mut guard = stream_state.lock().await;
            guard.last_error = Some(format!("event bus: {e}"));
        }
    });

    // ── Push: subscribe to session mode changes ───────────────────
    let mode_stream_state = state.clone();
    let mode_stream_client = mode_client.clone();
    let mode_stream_handle = tokio::spawn(async move {
        if let Err(e) = run_mode_stream(mode_stream_client, mode_stream_state.clone()).await {
            let mut guard = mode_stream_state.lock().await;
            guard.last_error = Some(format!("mode stream: {e}"));
        }
    });

    // ── Poll: project info (no push stream exists yet) ────────────
    let poll_state = state.clone();
    let poll_handle = tokio::spawn(async move {
        let mut tick = interval(Duration::from_millis(1000));
        loop {
            tick.tick().await;
            collect_polled(&projects_client, &poll_state).await;
        }
    });

    let result = run_ui_loop(state.clone()).await;
    poll_handle.abort();
    stream_handle.abort();
    mode_stream_handle.abort();
    result
}

/// Open the SessionMode subscribe stream and forward each slug into
/// the snapshot. The server pushes the current mode immediately as a
/// "seed" event, then one slug per `set_mode` transition.
async fn run_mode_stream(
    client: SessionModeServiceClient,
    state: Arc<Mutex<Snapshot>>,
) -> Result<()> {
    let (tx, mut rx) = vox::channel::<String>();
    client
        .subscribe(tx)
        .await
        .map_err(|e| eyre!("mode subscribe: {e}"))?;
    loop {
        match rx.recv().await {
            Ok(Some(slug)) => {
                let mut guard = state.lock().await;
                guard.mode = Some(slug.get().to_string());
            }
            Ok(None) => return Err(eyre!("mode stream closed by server")),
            Err(e) => return Err(eyre!("mode stream recv: {e}")),
        }
    }
}

/// Subscribe to every domain via the multiplexed `EventBus` and
/// forward each event into the shared snapshot — counts get
/// incremented, transport state / tempo / position get updated, and
/// every non-position event is appended to the event log. Returns
/// only on stream error / EOF.
async fn run_event_bus(
    client: EventBusClient,
    projects: ProjectsClient,
    state: Arc<Mutex<Snapshot>>,
) -> Result<()> {
    let (tx, mut rx) = vox::channel::<DawEvent>();
    let started = Instant::now();
    client
        .subscribe(BusFilter::all(), tx)
        .await
        .map_err(|e| eyre!("event bus subscribe: {e}"))?;
    {
        let mut guard = state.lock().await;
        guard.latency.push(started.elapsed());
    }

    loop {
        match rx.recv().await {
            Ok(Some(event)) => {
                let now = Instant::now();
                // Detect CurrentChanged BEFORE moving into apply_daw_event
                // — on a project switch we kick an immediate refresh
                // RPC so the header doesn't have to wait for the 1Hz
                // poll cycle. Counts also need a re-seed since they're
                // per-project.
                let project_switched = matches!(
                    event.get(),
                    DawEvent::Project(ev)
                        if matches!(ev.event, ProjectEvent::CurrentChanged(_))
                );
                {
                    let mut guard = state.lock().await;
                    guard.stream_ticks = guard.stream_ticks.wrapping_add(1);
                    guard.last_tick_at = Some(now);
                    apply_daw_event(&mut guard, event.get(), now);
                }
                if project_switched {
                    // Off the lock — fire the refresh in the background.
                    let projects_c = projects.clone();
                    let state_c = state.clone();
                    tokio::spawn(async move {
                        if let Ok(Some(info)) = projects_c.current().await {
                            let mut guard = state_c.lock().await;
                            guard.project = Some(info);
                            // counts are per-project; clear so the next
                            // user-visible value comes from a fresh seed.
                            guard.counts.clear();
                        }
                    });
                }
            }
            Ok(None) => return Err(eyre!("event bus closed by server")),
            Err(e) => return Err(eyre!("event bus recv: {e}")),
        }
    }
}

fn apply_daw_event(snap: &mut Snapshot, event: &DawEvent, now: Instant) {
    match event {
        DawEvent::TransportPosition(tick) => {
            // Publisher emits one tick per open project tab. Drop
            // ticks for non-active tabs so the display reflects what
            // the user is actually looking at in REAPER.
            if snap
                .active_project_guid
                .as_deref()
                .is_some_and(|g| g != tick.project_guid.as_str())
            {
                return;
            }
            snap.playhead = Some(tick.playhead.clone());
            snap.edit_cursor = Some(tick.edit_cursor.clone());
            snap.is_playing = Some(tick.is_playing);
        }
        DawEvent::TransportState(ev) => {
            let summary = match ev {
                TransportEvent::PlayStateChanged { play_state, .. } => {
                    snap.play_state = Some(*play_state);
                    format!("play_state = {:?}", play_state)
                }
                TransportEvent::TempoChanged {
                    tempo,
                    time_signature,
                    ..
                } => {
                    snap.tempo = Some(tempo.bpm());
                    snap.time_signature = Some(*time_signature);
                    format!(
                        "tempo {:.2} BPM   ts {}/{}",
                        tempo.bpm(),
                        time_signature.numerator(),
                        time_signature.denominator()
                    )
                }
                TransportEvent::Snapshot { state, .. } => {
                    snap.play_state = Some(state.play_state);
                    snap.tempo = Some(state.tempo.bpm());
                    snap.time_signature = Some(state.time_signature);
                    "transport snapshot".to_string()
                }
                TransportEvent::RecordModeChanged { record_mode, .. } => {
                    format!("record_mode = {:?}", record_mode)
                }
                TransportEvent::LoopingChanged { looping, .. } => {
                    format!("looping = {}", looping)
                }
                TransportEvent::LoopRegionChanged { .. } => "loop region".to_string(),
                TransportEvent::TimeSelectionChanged { .. } => "time selection".to_string(),
                TransportEvent::PlayrateChanged { playrate, .. } => {
                    format!("playrate = {:.3}", playrate)
                }
            };
            push_log(snap, now, "transport", summary);
        }
        DawEvent::Track(env) => {
            let summary = match &env.event {
                TrackEvent::Added(t) => {
                    snap.counts.tracks = Some(snap.counts.tracks.unwrap_or(0) + 1);
                    format!("+ track {}", t.name.as_str())
                }
                TrackEvent::Removed(guid) => {
                    snap.counts.tracks = Some(snap.counts.tracks.unwrap_or(0).saturating_sub(1));
                    format!("- track {}", short(guid))
                }
                TrackEvent::Renamed { name, .. } => format!("rename → {}", name),
                TrackEvent::MuteChanged { muted, .. } => format!("mute = {}", muted),
                TrackEvent::SoloChanged { soloed, .. } => format!("solo = {}", soloed),
                TrackEvent::ArmChanged { armed, .. } => format!("arm = {}", armed),
                TrackEvent::VolumeChanged { volume, .. } => format!("vol = {:.2}", volume),
                TrackEvent::PanChanged { pan, .. } => format!("pan = {:.2}", pan),
                TrackEvent::Moved {
                    old_index,
                    new_index,
                    ..
                } => format!("moved {} → {}", old_index, new_index),
                other => format!("{:?}", other),
            };
            push_log(snap, now, "track", summary);
        }
        DawEvent::Marker(env) => {
            let summary = match &env.event {
                MarkerEvent::Added(m) => {
                    snap.counts.markers = Some(snap.counts.markers.unwrap_or(0) + 1);
                    format!("+ marker @{} {}", m.position, m.name.as_str())
                }
                MarkerEvent::Removed(id) => {
                    snap.counts.markers =
                        Some(snap.counts.markers.unwrap_or(0).saturating_sub(1));
                    format!("- marker #{}", id)
                }
                MarkerEvent::Changed(m) => format!("marker {:?} → {}", m.id, m.name.as_str()),
                MarkerEvent::MarkersChanged(list) => {
                    snap.counts.markers = Some(list.len() as u32);
                    format!("markers reload (n={})", list.len())
                }
            };
            push_log(snap, now, "marker", summary);
        }
        DawEvent::Region(env) => {
            let summary = match &env.event {
                RegionEvent::Added(r) => {
                    snap.counts.regions = Some(snap.counts.regions.unwrap_or(0) + 1);
                    format!("+ region {:?} {}", r.id, r.name.as_str())
                }
                RegionEvent::Removed(id) => {
                    snap.counts.regions =
                        Some(snap.counts.regions.unwrap_or(0).saturating_sub(1));
                    format!("- region #{}", id)
                }
                RegionEvent::Changed(r) => format!("region {:?} → {}", r.id, r.name.as_str()),
                RegionEvent::RegionsChanged(list) => {
                    snap.counts.regions = Some(list.len() as u32);
                    format!("regions reload (n={})", list.len())
                }
            };
            push_log(snap, now, "region", summary);
        }
        DawEvent::Project(env) => {
            if let ProjectEvent::CurrentChanged(guid) = &env.event {
                snap.active_project_guid = guid.clone();
                // Clear position fields so old project's playhead
                // doesn't linger for one tick during the switch.
                snap.playhead = None;
                snap.edit_cursor = None;
            }
            let summary = match &env.event {
                ProjectEvent::CurrentChanged(guid) => format!(
                    "current → {}",
                    guid.as_deref().map(short).unwrap_or("(none)")
                ),
                ProjectEvent::Opened(p) => format!("+ {}", p.name.as_str()),
                ProjectEvent::Closed(g) => format!("- {}", short(g)),
                ProjectEvent::Changed(p) => format!("Δ {}", p.name.as_str()),
                ProjectEvent::ProjectsChanged(list) => format!("reload n={}", list.len()),
            };
            push_log(snap, now, "project", summary);
        }
        DawEvent::TempoMap(env) => {
            let summary = match &env.event {
                TempoMapEvent::PointAdded(p) => {
                    snap.counts.tempo_points =
                        Some(snap.counts.tempo_points.unwrap_or(0) + 1);
                    format!("+ point {:.2} BPM", p.bpm)
                }
                TempoMapEvent::PointRemoved(i) => {
                    snap.counts.tempo_points =
                        Some(snap.counts.tempo_points.unwrap_or(0).saturating_sub(1));
                    format!("- point #{i}")
                }
                TempoMapEvent::PointChanged(p) => format!("point → {:.2} BPM", p.bpm),
                TempoMapEvent::MapChanged(list) => {
                    snap.counts.tempo_points = Some(list.len() as u32);
                    format!("map reload (n={})", list.len())
                }
            };
            push_log(snap, now, "tempo_map", summary);
        }
    }
}

fn push_log(snap: &mut Snapshot, at: Instant, domain: &'static str, summary: String) {
    if snap.event_log.len() == EVENT_LOG_CAP {
        snap.event_log.pop_front();
    }
    snap.event_log.push_back(EventLogEntry { at, domain, summary });
}

fn short(s: &str) -> &str {
    s.get(..8).unwrap_or(s)
}

/// Poll the project info — the only thing left without a push stream.
/// `ProjectEvent::CurrentChanged` fires on tab switch and triggers an
/// inline refresh too; this poll handles metadata changes (rename /
/// save / etc.) and acts as a slow heartbeat for the Latency panel.
async fn collect_polled(projects: &ProjectsClient, state: &Arc<Mutex<Snapshot>>) {
    let start = Instant::now();
    let project = projects.current().await.ok().flatten();
    let elapsed = start.elapsed();

    let mut guard = state.lock().await;
    guard.latency.push(elapsed);
    if project.is_some() {
        guard.project = project;
    }
    guard.poll_ticks = guard.poll_ticks.wrapping_add(1);
}

async fn run_ui_loop(state: Arc<Mutex<Snapshot>>) -> Result<()> {
    enable_raw_mode().wrap_err("enable raw mode")?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen).wrap_err("enter alt screen")?;

    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend).wrap_err("init terminal")?;

    let render_tick = Duration::from_millis(16);
    let mut next_render = Instant::now();
    let result = loop {
        let now = Instant::now();
        let poll_for = next_render.saturating_duration_since(now);
        if event::poll(poll_for).unwrap_or(false) {
            match event::read() {
                Ok(Event::Key(k)) if k.kind == KeyEventKind::Press => match k.code {
                    KeyCode::Char('q') | KeyCode::Esc => break Ok(()),
                    KeyCode::Char('c') if k.modifiers.contains(KeyModifiers::CONTROL) => {
                        break Ok(());
                    }
                    _ => {}
                },
                Err(e) => break Err(eyre!("terminal event read failed: {e}")),
                _ => {}
            }
        }
        if Instant::now() >= next_render {
            let snap = state.lock().await.clone();
            if let Err(e) = terminal.draw(|f| render(f, &snap)) {
                break Err(eyre!("draw failed: {e}"));
            }
            next_render += render_tick;
            let now = Instant::now();
            if next_render < now {
                next_render = now + render_tick;
            }
        }
    };

    disable_raw_mode().ok();
    execute!(terminal.backend_mut(), LeaveAlternateScreen).ok();
    terminal.show_cursor().ok();
    result
}

fn render(f: &mut ratatui::Frame, snap: &Snapshot) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3), // Session
            Constraint::Length(8), // Transport
            Constraint::Length(3), // Counts
            Constraint::Length(4), // Latency
            Constraint::Min(5),    // Events
            Constraint::Length(1), // Footer
        ])
        .split(f.area());

    // ── Session header ─────────────────────────────────────────
    let project_name: Cow<'_, str> = snap
        .project
        .as_ref()
        .map(|p| {
            if !p.name.is_empty() {
                Cow::Borrowed(p.name.as_str())
            } else if !p.path.is_empty() {
                Cow::Owned(short_path(&p.path))
            } else {
                Cow::Borrowed("(unsaved)")
            }
        })
        .unwrap_or(Cow::Borrowed("(no project)"));
    let mode = snap.mode.as_deref().unwrap_or("—");
    let header = Paragraph::new(Line::from(vec![
        Span::styled("Project ", Style::default().fg(Color::DarkGray)),
        Span::styled(
            project_name.to_string(),
            Style::default().fg(Color::White).add_modifier(Modifier::BOLD),
        ),
        Span::raw("    "),
        Span::styled("Mode ", Style::default().fg(Color::DarkGray)),
        Span::styled(
            mode.to_string(),
            Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD),
        ),
    ]))
    .block(Block::default().borders(Borders::ALL).title("Session"));
    f.render_widget(header, chunks[0]);

    // ── Transport ──────────────────────────────────────────────
    let play_label = snap
        .play_state
        .as_ref()
        .map(format_play_state)
        .unwrap_or_else(|| "—".to_string());
    let play_color = snap
        .play_state
        .as_ref()
        .map(play_state_color)
        .unwrap_or(Color::DarkGray);
    let pos_sec = snap
        .playhead
        .as_ref()
        .and_then(|p| p.time.map(|t| t.to_string()))
        .unwrap_or_else(|| "—".to_string());
    let pos_musical = snap
        .playhead
        .as_ref()
        .and_then(|p| p.musical.map(|m| m.to_string()))
        .unwrap_or_else(|| "—".to_string());
    let tempo = snap
        .tempo
        .map(|b| format!("{b:.2} BPM"))
        .unwrap_or_else(|| "—".to_string());
    let ts = snap
        .time_signature
        .map(|t| format!("{}/{}", t.numerator(), t.denominator()))
        .unwrap_or_else(|| "—".to_string());
    let transport_lines = vec![
        Line::from(vec![
            Span::styled("State    ", Style::default().fg(Color::DarkGray)),
            Span::styled(play_label, Style::default().fg(play_color)),
        ]),
        Line::from(vec![
            Span::styled("Playhead ", Style::default().fg(Color::DarkGray)),
            Span::styled(
                pos_sec,
                Style::default().fg(Color::White).add_modifier(Modifier::BOLD),
            ),
            Span::raw("    "),
            Span::styled("Musical ", Style::default().fg(Color::DarkGray)),
            Span::styled(
                pos_musical,
                Style::default().fg(Color::White).add_modifier(Modifier::BOLD),
            ),
        ]),
        {
            let edit_sec = snap
                .edit_cursor
                .as_ref()
                .and_then(|p| p.time.map(|t| t.to_string()))
                .unwrap_or_else(|| "—".to_string());
            let edit_musical = snap
                .edit_cursor
                .as_ref()
                .and_then(|p| p.musical.map(|m| m.to_string()))
                .unwrap_or_else(|| "—".to_string());
            Line::from(vec![
                Span::styled("Edit cur ", Style::default().fg(Color::DarkGray)),
                Span::styled(
                    edit_sec,
                    Style::default().fg(Color::White).add_modifier(Modifier::BOLD),
                ),
                Span::raw("    "),
                Span::styled("Musical ", Style::default().fg(Color::DarkGray)),
                Span::styled(
                    edit_musical,
                    Style::default().fg(Color::White).add_modifier(Modifier::BOLD),
                ),
            ])
        },
        {
            // Compute delta via Position's own method, then format the
            // two components from one source of truth instead of
            // open-coding the subtraction here.
            let (delta_sec, delta_musical) =
                match (snap.playhead.as_ref(), snap.edit_cursor.as_ref()) {
                    (Some(p), Some(e)) => {
                        let d = if let Some(ts) = snap.time_signature {
                            p.delta_from_with_ts(e, ts)
                        } else {
                            p.delta_from(e)
                        };
                        let sec = d.to_string();
                        let mus = snap
                            .time_signature
                            .and_then(|ts| d.musical_string(ts))
                            .unwrap_or_else(|| "—".to_string());
                        (sec, mus)
                    }
                    _ => ("—".to_string(), "—".to_string()),
                };
            Line::from(vec![
                Span::styled("Δ play−edit ", Style::default().fg(Color::DarkGray)),
                Span::styled(delta_sec, Style::default().fg(Color::Cyan)),
                Span::raw("    "),
                Span::styled("Δ musical ", Style::default().fg(Color::DarkGray)),
                Span::styled(delta_musical, Style::default().fg(Color::Cyan)),
            ])
        },
        Line::from(vec![
            Span::styled("Tempo    ", Style::default().fg(Color::DarkGray)),
            Span::raw(tempo),
            Span::raw("    "),
            Span::styled("Time sig ", Style::default().fg(Color::DarkGray)),
            Span::raw(ts),
        ]),
        Line::from(vec![
            Span::styled("Stream   ", Style::default().fg(Color::DarkGray)),
            Span::raw(format!(
                "{} ticks   age {}",
                snap.stream_ticks,
                snap.last_tick_at
                    .map(|t| format_short(t.elapsed()))
                    .unwrap_or_else(|| "—".to_string()),
            )),
        ]),
    ];
    f.render_widget(
        Paragraph::new(transport_lines)
            .block(Block::default().borders(Borders::ALL).title("Transport")),
        chunks[1],
    );

    // ── Counts (all push-maintained) ───────────────────────────
    let bold_white = Style::default().fg(Color::White).add_modifier(Modifier::BOLD);
    let dim = Style::default().fg(Color::DarkGray);
    let counts = Paragraph::new(Line::from(vec![
        Span::styled("Tracks ", dim),
        Span::styled(opt(snap.counts.tracks), bold_white),
        Span::raw("    "),
        Span::styled("Markers ", dim),
        Span::styled(opt(snap.counts.markers), bold_white),
        Span::raw("    "),
        Span::styled("Regions ", dim),
        Span::styled(opt(snap.counts.regions), bold_white),
        Span::raw("    "),
        Span::styled("Tempo pts ", dim),
        Span::styled(opt(snap.counts.tempo_points), bold_white),
    ]))
    .block(Block::default().borders(Borders::ALL).title("Counts (push)"));
    f.render_widget(counts, chunks[2]);

    // ── Latency ────────────────────────────────────────────────
    let latency_text = match snap.latency.summary() {
        Some((min, avg, p95, max, n)) => vec![
            Line::from(vec![
                Span::styled("RPC ", Style::default().fg(Color::DarkGray)),
                Span::raw(format!(
                    "min {}   avg {}   p95 {}   max {}",
                    format_short(min),
                    format_short(avg),
                    format_short(p95),
                    format_short(max),
                )),
                Span::styled(
                    format!("   (n={n})"),
                    Style::default().fg(Color::DarkGray),
                ),
            ]),
            Line::from(vec![
                Span::styled("Path ", Style::default().fg(Color::DarkGray)),
                Span::raw(
                    "Unix socket / Vox RPC. Push events (position) bypass RPC entirely."
                        .to_string(),
                ),
            ]),
        ],
        None => vec![Line::from(Span::styled(
            "no RPC samples yet",
            Style::default().fg(Color::DarkGray),
        ))],
    };
    f.render_widget(
        Paragraph::new(latency_text)
            .block(Block::default().borders(Borders::ALL).title("Latency")),
        chunks[3],
    );

    // ── Events (live log from EventBus) ────────────────────────
    let visible_rows = chunks[4].height.saturating_sub(2) as usize; // borders
    let event_lines: Vec<Line> = snap
        .event_log
        .iter()
        .rev()
        .take(visible_rows)
        .map(|e| {
            let age_ms = e.at.elapsed().as_millis();
            Line::from(vec![
                Span::styled(
                    format!("{:>5}ms ", age_ms),
                    Style::default().fg(Color::DarkGray),
                ),
                Span::styled(
                    format!("{:<10}", e.domain),
                    Style::default().fg(domain_color(e.domain)),
                ),
                Span::raw(" "),
                Span::raw(&e.summary),
            ])
        })
        .collect();
    f.render_widget(
        Paragraph::new(event_lines)
            .block(Block::default().borders(Borders::ALL).title("Events (EventBus push)")),
        chunks[4],
    );

    // ── Footer ─────────────────────────────────────────────────
    let mut footer = format!(
        "poll {}   stream {}",
        snap.poll_ticks, snap.stream_ticks
    );
    if let Some(err) = &snap.last_error {
        footer.push_str("   ");
        footer.push_str(err);
    }
    footer.push_str("    q/Esc to quit");
    f.render_widget(
        Paragraph::new(Line::from(Span::styled(
            footer,
            Style::default().fg(Color::DarkGray),
        ))),
        chunks[5],
    );
}

fn opt<T: std::fmt::Display>(v: Option<T>) -> String {
    v.map(|x| x.to_string()).unwrap_or_else(|| "—".to_string())
}

fn domain_color(d: &str) -> Color {
    match d {
        "transport" => Color::Cyan,
        "track" => Color::Green,
        "marker" => Color::Yellow,
        "region" => Color::Magenta,
        "tempo_map" => Color::LightBlue,
        "project" => Color::LightRed,
        _ => Color::White,
    }
}

fn format_play_state(s: &PlayState) -> String {
    match s {
        PlayState::Recording => "RECORDING",
        PlayState::Playing => "PLAYING",
        PlayState::Paused => "PAUSED",
        PlayState::Stopped => "STOPPED",
    }
    .to_string()
}

fn play_state_color(s: &PlayState) -> Color {
    match s {
        PlayState::Recording => Color::Red,
        PlayState::Playing => Color::Green,
        PlayState::Paused => Color::Yellow,
        PlayState::Stopped => Color::DarkGray,
    }
}

fn format_short(d: Duration) -> String {
    let us = d.as_micros();
    if us < 1_000 {
        format!("{us}µs")
    } else if us < 1_000_000 {
        format!("{:.2}ms", us as f64 / 1_000.0)
    } else {
        format!("{:.2}s", d.as_secs_f64())
    }
}

fn short_path(p: &str) -> String {
    std::path::Path::new(p)
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| p.to_string())
}
