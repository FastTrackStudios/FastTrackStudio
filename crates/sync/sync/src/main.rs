//! fts-sync — Ableton Link transport sync as an SHM guest extension.
//!
//! Connects to daw-bridge via shared memory and runs the Link sync engine,
//! providing DAW-to-DAW transport synchronization without being loaded into
//! REAPER's process. Hot-reloadable: rebuild, restart, REAPER stays running.
//!
//! # Usage
//!
//! ```sh
//! # Start REAPER with daw-bridge loaded, then:
//! cargo run -p sync --features guest --bin fts-sync
//!
//! # Or with custom bootstrap socket:
//! FTS_SHM_BOOTSTRAP_SOCK=/tmp/fts-daw-12345.bootstrap.sock cargo run -p sync --features guest --bin fts-sync
//! ```

use daw_extension_runtime::GuestOptions;
use eyre::{Result, eyre};
use std::sync::Arc;
use sync::link::{Engine, EngineConfig, LinkCallbacks, LinkState, Mode};
use tracing::info;

fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into()),
        )
        .init();

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;

    rt.block_on(run())
}

async fn run() -> Result<()> {
    let pid = std::process::id();
    info!("[fts-sync:{pid}] Starting sync extension");

    // Connect to daw-bridge via SHM
    let daw = daw_extension_runtime::connect(GuestOptions {
        role: "fts-sync",
        ..Default::default()
    })
    .await?;

    info!("[fts-sync:{pid}] Connected to daw-bridge");

    // Get initial tempo from the current project
    let project = daw.current_project().await.map_err(|e| eyre!("{e}"))?;
    let transport = project.transport();
    let initial_tempo = transport.get_tempo().await.map_err(|e| eyre!("{e}"))?;

    info!("[fts-sync:{pid}] Initial tempo: {initial_tempo:.1} BPM");

    // Create the Link engine
    let config = EngineConfig {
        mode: Mode::Off, // Start disabled — enabled via ExtState or API
        push_local_tempo: true,
        start_stop_sync: true,
        phase_tolerance: None,
    };

    let mut engine = Engine::new(initial_tempo, config);
    let daw = Arc::new(daw);

    info!("[fts-sync:{pid}] Link engine created, entering tick loop");

    // Check for ExtState-based mode control and tick the engine at ~30Hz
    let mut interval = tokio::time::interval(std::time::Duration::from_millis(33));
    let mut ext_check_counter: u64 = 0;

    loop {
        interval.tick().await;

        // Check ExtState for mode changes (~1Hz)
        ext_check_counter += 1;
        if ext_check_counter % 30 == 0 {
            let ext_state = daw.ext_state();
            if let Ok(Some(mode_str)) = ext_state.get("FTS_SYNC", "link_mode").await {
                match mode_str.as_str() {
                    "puppet" if engine.mode() != Mode::Puppet => {
                        engine.set_enabled(true);
                        engine.set_mode(Mode::Puppet);
                        info!("[fts-sync:{pid}] Link enabled, mode=Puppet");
                    }
                    "master" if engine.mode() != Mode::Master => {
                        engine.set_enabled(true);
                        engine.set_mode(Mode::Master);
                        info!("[fts-sync:{pid}] Link enabled, mode=Master");
                    }
                    "off" if engine.mode() != Mode::Off => {
                        engine.set_mode(Mode::Off);
                        engine.set_enabled(false);
                        info!("[fts-sync:{pid}] Link disabled");
                    }
                    _ => {}
                }
            }
        }

        // Tick the engine if active
        if engine.mode() != Mode::Off {
            let callbacks = DawLinkCallbacks {
                daw: daw.clone(),
                rt: tokio::runtime::Handle::current(),
            };
            engine.tick(&callbacks);
        }
    }
}

/// Implements `LinkCallbacks` using the `Daw` handle over SHM RPC.
///
/// Since `LinkCallbacks` methods are sync but `Daw` is async, we use
/// `block_on` to bridge the gap. This is fine because we're running
/// on a single-threaded tokio runtime in the tick loop.
struct DawLinkCallbacks {
    daw: Arc<daw::Daw>,
    rt: tokio::runtime::Handle,
}

impl DawLinkCallbacks {
    /// Run an async operation synchronously within the tick.
    fn block_on<F: std::future::Future>(&self, f: F) -> F::Output {
        self.rt.block_on(f)
    }
}

impl LinkCallbacks for DawLinkCallbacks {
    fn capture_state(&self) -> LinkState {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let transport = project.transport();
            let state = transport.get_state().await.unwrap();

            let is_playing = state.play_state == daw::PlayState::Playing;
            let position = state
                .playhead_position
                .time
                .map(|t| t.as_seconds())
                .unwrap_or(0.0);

            let tempo_bpm = state.tempo.bpm;
            let beats_per_second = tempo_bpm / 60.0;
            let beat_position = position * beats_per_second;
            let qn_position = beat_position; // quarter notes ≈ beats in 4/4

            // Get output latency
            let audio_engine = self.daw.audio_engine();
            let output_latency = audio_engine.output_latency_seconds().await.unwrap_or(0.0);

            LinkState {
                is_playing,
                position_seconds: position,
                beat_position,
                qn_position,
                tempo_bpm,
                timesig_num: state.time_signature.numerator as i32,
                timesig_denom: state.time_signature.denominator as i32,
                output_latency,
                is_looping: state.looping,
                playrate: state.playrate,
                loop_range: state
                    .loop_region
                    .as_ref()
                    .map(|lr| (lr.start_seconds, lr.end_seconds)),
                loop_range_beats: state.loop_region.as_ref().map(|lr| {
                    (
                        lr.start_seconds * beats_per_second,
                        lr.end_seconds * beats_per_second,
                    )
                }),
            }
        })
    }

    fn play(&self) {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let _ = project.transport().play().await;
        });
    }

    fn stop(&self) {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let _ = project.transport().stop().await;
        });
    }

    fn set_cursor_position(&self, seconds: f64) {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let _ = project.transport().set_position(seconds).await;
        });
    }

    fn set_tempo(&self, bpm: f64) {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let _ = project.transport().set_tempo(bpm).await;
        });
    }

    fn get_loop_range(&self) -> Option<(f64, f64)> {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let state = project.transport().get_state().await.unwrap();
            state
                .loop_region
                .map(|lr| (lr.start_seconds, lr.end_seconds))
        })
    }

    fn nudge_playrate_up(&self) {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            // REAPER action 40524: "Transport: Increase playrate by ~6%"
            let _ = project.run_command("40524").await;
        });
    }

    fn nudge_playrate_down(&self) {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            // REAPER action 40525: "Transport: Decrease playrate by ~6%"
            let _ = project.run_command("40525").await;
        });
    }

    fn reset_playrate(&self) {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            // REAPER action 40521: "Transport: Set playrate to 1.0"
            let _ = project.run_command("40521").await;
        });
    }

    fn go_to_region(&self, region_id: i32) {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let _ = project.regions().goto_start(region_id as u32).await;
        });
    }

    fn add_project_marker(&self, is_region: bool, start: f64, end: f64, name: &str) -> i32 {
        let name = name.to_string();
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            if is_region {
                project.regions().add(start, end, &name).await.unwrap_or(0) as i32
            } else {
                project.markers().add(start, &name).await.unwrap_or(0) as i32
            }
        })
    }

    fn remove_project_marker(&self, marker_id: i32) {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let _ = project.markers().remove(marker_id as u32).await;
        });
    }

    fn set_tempo_at_position(
        &self,
        position: f64,
        bpm: f64,
        _timesig_num: i32,
        _timesig_denom: i32,
    ) {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let _ = project.tempo_map().add_point(position, bpm).await;
        });
    }

    fn get_next_measure_position(&self) -> f64 {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let state = project.transport().get_state().await.unwrap();
            let pos = state
                .playhead_position
                .time
                .map(|t| t.as_seconds())
                .unwrap_or(0.0);
            let tempo = state.tempo.bpm;
            let timesig_num = state.time_signature.numerator as f64;
            // Approximate: beats per measure / (tempo/60) = seconds per measure
            let secs_per_measure = timesig_num / (tempo / 60.0);
            let current_measure = (pos / secs_per_measure).floor();
            (current_measure + 1.0) * secs_per_measure
        })
    }

    fn create_midi_item(&self, _track_index: usize, _start: f64, _end: f64) {
        // TODO: Implement via MidiService when needed for quantized launch
        tracing::warn!("create_midi_item not yet implemented in SHM guest");
    }

    fn get_project_length(&self) -> f64 {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let state = project.transport().get_state().await.unwrap();
            let pos = state
                .playhead_position
                .time
                .map(|t| t.as_seconds())
                .unwrap_or(0.0);
            // Approximate from position if playing, or use a large default
            pos.max(300.0) // fallback to 5 min
        })
    }

    fn time_to_beats(&self, time_seconds: f64) -> (f64, i32) {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let state = project.transport().get_state().await.unwrap();
            let bps = state.tempo.bpm / 60.0;
            let timesig_num = state.time_signature.numerator as f64;
            let total_beats = time_seconds * bps;
            let measure = (total_beats / timesig_num).floor() as i32;
            let beat_in_measure = total_beats - (measure as f64 * timesig_num);
            (beat_in_measure, measure)
        })
    }

    fn beats_to_time(&self, beat: f64, measure: i32) -> f64 {
        self.block_on(async {
            let project = self.daw.current_project().await.unwrap();
            let state = project.transport().get_state().await.unwrap();
            let bps = state.tempo.bpm / 60.0;
            let timesig_num = state.time_signature.numerator as f64;
            let total_beats = (measure as f64 * timesig_num) + beat;
            total_beats / bps
        })
    }

    fn clear_link_dummy_objects(&self) {
        // TODO: Implement marker cleanup for quantized launch
        tracing::debug!("clear_link_dummy_objects not yet implemented in SHM guest");
    }
}
