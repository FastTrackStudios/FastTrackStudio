//! `Diagnostics` impl for the REAPER backend.
//!
//! Runs the entire latency probe loop inside one architect-dispatched
//! main-thread closure. No per-sample RPC marshaling, no IPC, no
//! tokio scheduling. The only remaining cost is the csurf callback +
//! tokio `broadcast` send/recv.

use daw_proto::ProjectContext;
use daw_proto::diagnostics::{AudioSyncSnapshot, Diagnostics};
use daw_proto::track::{TrackEvent, TrackStreamEvent};
use std::time::{Duration, Instant};
use tokio::sync::broadcast::error::TryRecvError;

fn audio_snapshot_to_wire(s: daw_audio_sync::AudioSnapshot) -> AudioSyncSnapshot {
    AudioSyncSnapshot {
        sequence: s.sequence,
        host_micros: s.host_micros,
        playhead_seconds: s.playhead_seconds,
        sample_rate: s.sample_rate,
        buffer_len: s.buffer_len,
        is_playing: s.is_playing,
    }
}

impl Diagnostics for crate::Reaper {
    fn hub_publish_latency_us(&self, project: ProjectContext, samples: u32) -> Vec<u64> {
        // Project resolution is just to keep the call shape close
        // to other diagnostic methods; the probe itself doesn't
        // touch REAPER state.
        let _ = project; // probe doesn't touch project state
        let project_guid = String::new();
        let guid = "probe-guid".to_string();

        let mut rx = crate::event_hub::hub().subscribe_tracks();
        while rx.try_recv().is_ok() {}

        let mut results = Vec::with_capacity(samples as usize);
        for i in 0..samples {
            let target = 0.2 + (i as f64) * 0.05;
            let event = TrackStreamEvent {
                project_guid: project_guid.clone(),
                event: TrackEvent::VolumeChanged {
                    guid: guid.clone(),
                    volume: target,
                },
            };
            let t0 = Instant::now();
            crate::event_hub::hub().publish_track(event);

            let deadline = t0 + std::time::Duration::from_millis(10);
            loop {
                match rx.try_recv() {
                    Ok(envelope) => {
                        if let TrackEvent::VolumeChanged { guid: g, volume } = &envelope.event
                            && g == &guid
                            && (*volume - target).abs() < 1e-9
                        {
                            results.push(t0.elapsed().as_micros() as u64);
                            break;
                        }
                    }
                    Err(TryRecvError::Empty) => {
                        if Instant::now() > deadline {
                            results.push(t0.elapsed().as_micros() as u64);
                            break;
                        }
                        std::hint::spin_loop();
                    }
                    Err(TryRecvError::Lagged(_)) => continue,
                    Err(TryRecvError::Closed) => return results,
                }
            }
        }

        results
    }

    fn audio_sync_snapshot(&self) -> Option<AudioSyncSnapshot> {
        daw_audio_sync::global_snapshot().map(audio_snapshot_to_wire)
    }

    fn audio_sync_observe(&self, count: u32, interval_us: u64) -> Vec<AudioSyncSnapshot> {
        // Runs on the main thread (architect dispatcher). Spin-sleep
        // between samples — `interval_us` is the poll cadence (caller
        // should set it to ~1/4 of the expected buffer period so we
        // catch every distinct sequence). Total budget is generous:
        // 8× nominal so OS scheduler hiccups don't truncate the
        // window prematurely. Holds the main thread for the duration
        // — keep `count` reasonable (< 50).
        let mut out = Vec::with_capacity(count as usize);
        let interval = Duration::from_micros(interval_us.max(50));
        let mut last_seq = 0u64;
        let deadline = Instant::now() + interval * count.max(1) * 8;
        while out.len() < count as usize && Instant::now() < deadline {
            if let Some(snap) = daw_audio_sync::global_snapshot()
                && snap.sequence != last_seq
            {
                last_seq = snap.sequence;
                out.push(audio_snapshot_to_wire(snap));
            }
            std::thread::sleep(interval);
        }
        out
    }
}
