//! Link bridge — runs the sync Link engine using daw RPC services.
//!
//! Instead of calling REAPER's C API directly (which requires being in-process),
//! this bridge implements `LinkCallbacks` by pre-fetching state via async RPC
//! before each tick, then executing queued mutations after the tick completes.
//!
//! The tick loop runs at ~30Hz on a tokio interval, matching REAPER's timer rate.

use std::cell::RefCell;

use daw::service::PlayState;
use daw::Daw;
use sync::link::{Engine, EngineConfig, LinkCallbacks, LinkState, Mode};
use tracing::info;

/// Mutations that the Link engine requested during a tick.
///
/// These are collected synchronously during `Engine::tick()` and then
/// executed as async RPC calls after the tick completes.
#[derive(Debug)]
enum Mutation {
    Play,
    Stop,
    SetPosition(f64),
    SetTempo(f64),
    SetPlayrate(f64),
    AddMarker {
        is_region: bool,
        start: f64,
        end: f64,
        name: String,
    },
    RemoveMarker(u32),
    AddTempoPoint {
        position: f64,
        bpm: f64,
    },
}

/// LinkCallbacks implementation that caches state and queues mutations.
///
/// Used inside `Engine::tick()` — all methods are synchronous. State is
/// pre-fetched before the tick; mutations are collected and executed after.
struct RpcLinkCallbacks {
    /// Pre-fetched transport/tempo state (set before each tick)
    state: LinkState,
    /// Current playrate (for nudge calculations)
    current_playrate: f64,
    /// Queued mutations (drained after each tick)
    mutations: RefCell<Vec<Mutation>>,
}

impl RpcLinkCallbacks {
    fn push(&self, m: Mutation) {
        self.mutations.borrow_mut().push(m);
    }
}

impl LinkCallbacks for RpcLinkCallbacks {
    fn capture_state(&self) -> LinkState {
        self.state.clone()
    }

    fn play(&self) {
        self.push(Mutation::Play);
    }

    fn stop(&self) {
        self.push(Mutation::Stop);
    }

    fn set_cursor_position(&self, seconds: f64) {
        self.push(Mutation::SetPosition(seconds));
    }

    fn set_tempo(&self, bpm: f64) {
        self.push(Mutation::SetTempo(bpm));
    }

    fn get_loop_range(&self) -> Option<(f64, f64)> {
        self.state.loop_range
    }

    fn nudge_playrate_up(&self) {
        // ~6% increase (one semitone)
        let new_rate = (self.current_playrate * 1.05946).min(4.0);
        self.push(Mutation::SetPlayrate(new_rate));
    }

    fn nudge_playrate_down(&self) {
        // ~6% decrease (one semitone)
        let new_rate = (self.current_playrate / 1.05946).max(0.25);
        self.push(Mutation::SetPlayrate(new_rate));
    }

    fn reset_playrate(&self) {
        self.push(Mutation::SetPlayrate(1.0));
    }

    fn go_to_region(&self, _region_id: i32) {
        // TODO: RegionService::goto_start needs a region ID mapping
        // For now, this is a no-op — quantized launch will be wired later
    }

    fn add_project_marker(&self, is_region: bool, start: f64, end: f64, name: &str) -> i32 {
        self.push(Mutation::AddMarker {
            is_region,
            start,
            end,
            name: name.to_string(),
        });
        // We can't return the actual ID synchronously since registration is async.
        // The engine uses this return value for tracking — return -1 as placeholder.
        -1
    }

    fn remove_project_marker(&self, marker_id: i32) {
        self.push(Mutation::RemoveMarker(marker_id as u32));
    }

    fn set_tempo_at_position(
        &self,
        position: f64,
        bpm: f64,
        _timesig_num: i32,
        _timesig_denom: i32,
    ) {
        self.push(Mutation::AddTempoPoint { position, bpm });
    }

    fn get_next_measure_position(&self) -> f64 {
        // Approximate: use cached state to estimate next measure boundary
        // This is close enough for the Link engine's quantized launch
        let beats_per_measure = self.state.timesig_num as f64;
        let beat = self.state.beat_position;
        let beats_until_next = beats_per_measure - (beat % beats_per_measure);
        let seconds_per_beat = 60.0 / self.state.tempo_bpm;
        self.state.position_seconds + (beats_until_next * seconds_per_beat)
    }

    fn create_midi_item(&self, _track_index: usize, _start: f64, _end: f64) {
        // TODO: Wire to ItemService when needed
    }

    fn get_project_length(&self) -> f64 {
        // Not available in cached state — return a large value
        // The engine uses this for bounds checking
        3600.0 * 4.0 // 4 hours
    }

    fn time_to_beats(&self, time_seconds: f64) -> (f64, i32) {
        // Approximate from cached tempo (accurate enough for tick-level work)
        let beats_per_second = self.state.tempo_bpm / 60.0;
        let total_beats = time_seconds * beats_per_second;
        let beats_per_measure = self.state.timesig_num as f64;
        let measures = (total_beats / beats_per_measure) as i32;
        let beat_in_measure = total_beats % beats_per_measure;
        (beat_in_measure, measures)
    }

    fn beats_to_time(&self, beat: f64, measure: i32) -> f64 {
        // Approximate from cached tempo
        let beats_per_measure = self.state.timesig_num as f64;
        let total_beats = (measure as f64 * beats_per_measure) + beat;
        let seconds_per_beat = 60.0 / self.state.tempo_bpm;
        total_beats * seconds_per_beat
    }

    fn clear_link_dummy_objects(&self) {
        // TODO: Query markers and remove any with "realink" in name
        // This is called rarely (on mode changes) — acceptable to skip for now
    }
}

/// Fetch the current transport state from REAPER via RPC and build a LinkState.
async fn fetch_link_state(
    transport: &daw::Transport,
    tempo_map: &daw::TempoMap,
) -> eyre::Result<(LinkState, f64)> {
    let state = transport.get_state().await?;

    let is_playing = matches!(state.play_state, PlayState::Playing | PlayState::Recording);

    let position = state
        .playhead_position
        .time
        .map(|t| t.as_seconds())
        .unwrap_or(0.0);

    let beat_position = state
        .playhead_position
        .musical
        .map(|m| m.beat as f64 + (m.subdivision as f64 / 1000.0))
        .unwrap_or(0.0);

    // Approximate QN from position and tempo (accurate enough for Link sync)
    let qn = position * (state.tempo.bpm / 60.0);

    let (timesig_num, timesig_denom) = tempo_map
        .time_signature_at(position)
        .await
        .unwrap_or((4, 4));

    let loop_range = state
        .loop_region
        .map(|lr| (lr.start_seconds, lr.end_seconds));

    // Compute loop range in beats (approximate)
    let loop_range_beats = if let Some((start, end)) = loop_range {
        let beats_per_second = state.tempo.bpm / 60.0;
        Some((start * beats_per_second, end * beats_per_second))
    } else {
        None
    };

    let link_state = LinkState {
        is_playing,
        position_seconds: position,
        beat_position,
        qn_position: qn,
        tempo_bpm: state.tempo.bpm,
        timesig_num,
        timesig_denom,
        output_latency: 0.0, // Not available via RPC — acceptable for non-audio-thread sync
        is_looping: state.looping,
        playrate: state.playrate,
        loop_range,
        loop_range_beats,
    };

    Ok((link_state, state.playrate))
}

/// Execute queued mutations from the Link engine tick.
async fn execute_mutations(
    transport: &daw::Transport,
    tempo_map: &daw::TempoMap,
    markers: &daw::Markers,
    regions: &daw::Regions,
    mutations: Vec<Mutation>,
) {
    for m in mutations {
        let result: eyre::Result<()> = async {
            match m {
                Mutation::Play => transport.play().await?,
                Mutation::Stop => transport.stop().await?,
                Mutation::SetPosition(s) => transport.set_position(s).await?,
                Mutation::SetTempo(bpm) => transport.set_tempo(bpm).await?,
                Mutation::SetPlayrate(rate) => transport.set_playrate(rate).await?,
                Mutation::AddMarker {
                    is_region,
                    start,
                    end,
                    name,
                } => {
                    if is_region {
                        regions.add(start, end, &name).await?;
                    } else {
                        markers.add(start, &name).await?;
                    }
                }
                Mutation::RemoveMarker(id) => {
                    markers.remove(id).await?;
                }
                Mutation::AddTempoPoint { position, bpm } => {
                    tempo_map.add_point(position, bpm).await?;
                }
            }
            Ok(())
        }
        .await;

        if let Err(e) = result {
            tracing::warn!("Link mutation failed: {e}");
        }
    }
}

/// Run the Link engine tick loop.
///
/// This is the main sync loop that drives Ableton Link from the sync-extension.
/// Runs at ~30Hz, matching REAPER's timer callback rate.
pub async fn run_link_engine(daw: &Daw) -> eyre::Result<()> {
    let project = daw.current_project().await?;
    let transport = project.transport();
    let tempo_map = project.tempo_map();
    let markers = project.markers();
    let regions = project.regions();

    // Get initial tempo
    let initial_tempo = transport.get_tempo().await.unwrap_or(120.0);

    let config = EngineConfig {
        mode: Mode::Puppet,
        push_local_tempo: true,
        start_stop_sync: true,
        phase_tolerance: None,
    };

    let mut engine = Engine::new(initial_tempo, config);
    info!("Link engine initialized (tempo={initial_tempo:.1} BPM, mode=Puppet)");

    // Write initial link mode to ExtState
    daw.ext_state()
        .set("FTS_SYNC", "link_mode", "puppet", false)
        .await?;

    let mut interval = tokio::time::interval(std::time::Duration::from_millis(33));
    let mut ext_check_counter: u64 = 0;

    loop {
        interval.tick().await;

        // Check ExtState for mode changes (~1Hz, not every tick)
        ext_check_counter += 1;
        if ext_check_counter % 30 == 0 {
            if let Ok(Some(mode_str)) = daw.ext_state().get("FTS_SYNC", "link_mode").await {
                let new_mode = match mode_str.as_str() {
                    "puppet" => Some(Mode::Puppet),
                    "master" => Some(Mode::Master),
                    "off" => Some(Mode::Off),
                    _ => None,
                };
                if let Some(mode) = new_mode {
                    if mode != engine.mode() {
                        engine.set_mode(mode);
                        let enabled = mode != Mode::Off;
                        engine.set_enabled(enabled);
                        info!("Link mode changed to {mode:?}");
                    }
                }
            }
        }

        // Skip tick if Link is off
        if engine.mode() == Mode::Off {
            continue;
        }

        // Fetch state via RPC
        let (link_state, playrate) = match fetch_link_state(&transport, &tempo_map).await {
            Ok(s) => s,
            Err(e) => {
                tracing::warn!("Failed to fetch state: {e}");
                continue;
            }
        };

        // Build callbacks with pre-fetched state
        let callbacks = RpcLinkCallbacks {
            state: link_state,
            current_playrate: playrate,
            mutations: RefCell::new(Vec::new()),
        };

        // Run engine tick (synchronous — uses cached state, queues mutations)
        engine.tick(&callbacks);

        // Execute any queued mutations
        let mutations = callbacks.mutations.into_inner();
        if !mutations.is_empty() {
            execute_mutations(&transport, &tempo_map, &markers, &regions, mutations).await;
        }

        // Periodic status logging (~5s)
        if ext_check_counter % 150 == 0 {
            info!(
                "Link: mode={:?} peers={} offset={:.4}s",
                engine.mode(),
                engine.num_peers(),
                engine.timeline_offset()
            );
        }
    }
}
