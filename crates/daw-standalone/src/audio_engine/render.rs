//! Backend-agnostic project renderer.
//!
//! `ProjectRenderer` takes a [`Standalone`] backend + project GUID
//! and produces a stereo master buffer for a given sample range.
//! It walks the routing graph each block (cheap because we snapshot
//! the state once up-front per render call) and honors:
//!
//! - Item playback: position, length, fade in/out (linear), volume,
//!   take start_offset, take play_rate (linear-interp resampling),
//!   take volume, item mute / track mute / solo
//! - Track sends → destination track bus (volume + pan)
//! - `parent_send_enabled` (REAPER `B_MAINSEND`) → sum to master
//! - Track volume + pan applied on the pre-send signal
//!
//! Out of scope for v0:
//! - Hardware outputs (currently sum to master same as parent send)
//! - Pre-FX vs Post-FX send positions (always treated as post-fader)
//! - Channel mappings beyond stereo (channel count is honored on the
//!   bus but down-mixed to stereo at the master)
//! - Folder bus summing (folders treated as containers; their child
//!   tracks already send to master if parent_send is on)
//!
//! WASM-compatible: pure math + heap, no threads, no cpal. The cpal
//! mixer in `mixer.rs` wraps this on native; an AudioWorklet shim can
//! wrap it on the web.

use std::sync::Arc;

use daw_proto::RouteType;

use super::decoder::DecodedAudio;
use crate::sync::{ProjectState, Standalone};

/// Stereo render buffer (interleaved L/R/L/R/...).
#[derive(Debug, Clone)]
pub struct StereoBuffer {
    pub samples: Vec<f32>,
    pub frames: usize,
    pub sample_rate: u32,
}

impl StereoBuffer {
    pub fn zeroed(frames: usize, sample_rate: u32) -> Self {
        Self {
            samples: vec![0.0; frames * 2],
            frames,
            sample_rate,
        }
    }

    pub fn fill(&mut self, v: f32) {
        for s in self.samples.iter_mut() {
            *s = v;
        }
    }
}

/// Snapshot of routing-relevant track data, copied once per render
/// call so we don't hold the project lock through the inner loops.
struct TrackSnapshot {
    guid: String,
    name: String,
    volume: f64,
    pan: f64,
    muted: bool,
    soloed: bool,
    parent_send: bool,
    sends: Vec<SendSnapshot>,
    items: Vec<ItemSnapshot>,
}

struct SendSnapshot {
    dest_guid: String,
    volume: f64,
    pan: f64,
    muted: bool,
}

struct ItemSnapshot {
    take_guid: Option<String>,
    audio: Option<Arc<DecodedAudio>>,
    position_seconds: f64,
    length_seconds: f64,
    fade_in_seconds: f64,
    fade_out_seconds: f64,
    muted: bool,
    item_volume: f64,
    take_volume: f64,
    play_rate: f64,
    start_offset_seconds: f64,
}

/// Render a stereo master block.
pub struct ProjectRenderer<'a> {
    daw: &'a Standalone,
    project_guid: &'a str,
    sample_rate: u32,
}

impl<'a> ProjectRenderer<'a> {
    pub fn new(daw: &'a Standalone, project_guid: &'a str, sample_rate: u32) -> Self {
        Self {
            daw,
            project_guid,
            sample_rate,
        }
    }

    /// Render `frames` stereo frames starting at `start_frame` (in
    /// output-rate samples). Returns a fresh `StereoBuffer`.
    pub fn render_block(&self, start_frame: u64, frames: usize) -> StereoBuffer {
        let mut master = StereoBuffer::zeroed(frames, self.sample_rate);
        if frames == 0 {
            return master;
        }

        let snapshot = match self.snapshot_tracks() {
            Some(s) => s,
            None => return master,
        };
        let any_soloed = snapshot.iter().any(|t| t.soloed);

        // Allocate per-track stereo buses keyed by guid.
        let mut buses: std::collections::HashMap<String, StereoBuffer> =
            std::collections::HashMap::with_capacity(snapshot.len());
        for t in &snapshot {
            buses.insert(
                t.guid.clone(),
                StereoBuffer::zeroed(frames, self.sample_rate),
            );
        }

        let start_seconds = start_frame as f64 / self.sample_rate as f64;
        let end_seconds = start_seconds + (frames as f64 / self.sample_rate as f64);

        // 1) Item playback into per-track buses.
        for t in &snapshot {
            if t.muted || (any_soloed && !t.soloed) {
                continue;
            }
            let bus = buses.get_mut(&t.guid).expect("bus pre-allocated");
            for item in &t.items {
                if item.muted {
                    continue;
                }
                let Some(audio) = &item.audio else { continue };
                mix_item_into_bus(
                    bus,
                    audio,
                    item,
                    start_seconds,
                    end_seconds,
                    self.sample_rate,
                );
            }
        }

        // 2) Apply track gain + pan in place (pre-send, post-fader).
        for t in &snapshot {
            if t.muted || (any_soloed && !t.soloed) {
                continue;
            }
            if let Some(bus) = buses.get_mut(&t.guid) {
                apply_volume_pan(bus, t.volume as f32, t.pan as f32);
            }
        }

        // 3) Sends — additive into destination buses. Iterate by index
        // so we can borrow source + dest disjointly via swap_remove
        // trick (or just clone the source bus, which is what we do).
        for t in &snapshot {
            if t.muted || (any_soloed && !t.soloed) {
                continue;
            }
            // Clone source bus once; sends read from this snapshot.
            let src = match buses.get(&t.guid) {
                Some(b) => b.samples.clone(),
                None => continue,
            };
            for s in &t.sends {
                if s.muted {
                    continue;
                }
                if let Some(dest_bus) = buses.get_mut(&s.dest_guid) {
                    add_with_volume_pan(dest_bus, &src, s.volume as f32, s.pan as f32);
                }
            }
        }

        // 4) Sum to master: tracks with parent_send_enabled go through.
        for t in &snapshot {
            if t.muted || (any_soloed && !t.soloed) {
                continue;
            }
            if !t.parent_send {
                continue;
            }
            let bus = match buses.get(&t.guid) {
                Some(b) => b,
                None => continue,
            };
            for (m, s) in master.samples.iter_mut().zip(bus.samples.iter()) {
                *m += *s;
            }
        }

        master
    }

    fn snapshot_tracks(&self) -> Option<Vec<TrackSnapshot>> {
        self.daw.read_project(self.project_guid, |p| {
            let mut tracks = Vec::with_capacity(p.tracks.len());
            for t in &p.tracks {
                tracks.push(snapshot_track(p, t));
            }
            tracks
        })
    }
}

fn snapshot_track(p: &ProjectState, t: &daw_proto::Track) -> TrackSnapshot {
    let parent_send = p
        .track_ext
        .get(&t.guid)
        .map(|e| e.parent_send_enabled)
        .unwrap_or(true);

    // Sends: only the Send variant matters (Receives mirror; HW out
    // for v0 also routes to master via parent_send equivalent).
    let sends: Vec<SendSnapshot> = p
        .sends
        .get(&t.guid)
        .map(|v| {
            v.iter()
                .filter(|r| r.route_type == RouteType::Send)
                .filter_map(|r| {
                    let dest_guid = r.dest_track_guid.clone()?;
                    Some(SendSnapshot {
                        dest_guid,
                        volume: r.volume,
                        pan: r.pan,
                        muted: r.muted,
                    })
                })
                .collect()
        })
        .unwrap_or_default();

    // Items on this track.
    let mut items = Vec::new();
    if let Some(item_guids) = p.items_by_track.get(&t.guid) {
        for ig in item_guids {
            let Some(ie) = p.items.get(ig) else { continue };
            let item = &ie.item;
            // Active take.
            let take_guid_opt = p
                .takes
                .get(ig)
                .and_then(|tl| tl.takes.get(tl.active_idx as usize).map(|t| t.guid.clone()));
            let audio = take_guid_opt
                .as_ref()
                .and_then(|tg| p.audio_sources.get(tg).cloned());
            let (take_volume, play_rate, start_offset) = p
                .takes
                .get(ig)
                .and_then(|tl| tl.takes.get(tl.active_idx as usize))
                .map(|tk| (tk.volume, tk.play_rate, tk.start_offset.as_seconds()))
                .unwrap_or((1.0, 1.0, 0.0));
            items.push(ItemSnapshot {
                take_guid: take_guid_opt,
                audio,
                position_seconds: item.position.as_seconds(),
                length_seconds: item.length.as_seconds(),
                fade_in_seconds: item.fade_in_length.as_seconds(),
                fade_out_seconds: item.fade_out_length.as_seconds(),
                muted: item.muted,
                item_volume: item.volume,
                take_volume,
                play_rate: if play_rate.abs() < 1e-9 {
                    1.0
                } else {
                    play_rate
                },
                start_offset_seconds: start_offset,
            });
        }
    }

    TrackSnapshot {
        guid: t.guid.clone(),
        name: t.name.clone(),
        volume: t.volume,
        pan: t.pan,
        muted: t.muted,
        soloed: t.soloed,
        parent_send,
        sends,
        items,
    }
}

fn mix_item_into_bus(
    bus: &mut StereoBuffer,
    audio: &DecodedAudio,
    item: &ItemSnapshot,
    block_start_seconds: f64,
    block_end_seconds: f64,
    output_rate: u32,
) {
    let item_start = item.position_seconds;
    let item_end = item_start + item.length_seconds;

    // Quick out: item doesn't overlap this block.
    if item_end <= block_start_seconds || item_start >= block_end_seconds {
        return;
    }

    let gain = (item.item_volume * item.take_volume) as f32;
    if gain == 0.0 {
        return;
    }

    let fade_in = item.fade_in_seconds.max(0.0);
    let fade_out = item.fade_out_seconds.max(0.0);

    let audio_channels = audio.channels.max(1) as usize;
    let audio_rate = audio.sample_rate.max(1) as f64;
    let output_rate_f = output_rate as f64;

    for frame in 0..bus.frames {
        let block_time = block_start_seconds + (frame as f64 / output_rate_f);
        if block_time < item_start || block_time >= item_end {
            continue;
        }
        // Time within the item.
        let item_time = block_time - item_start;
        // Source time, advancing play_rate × wall-time.
        let source_time = item.start_offset_seconds + item_time * item.play_rate;
        if source_time < 0.0 {
            continue;
        }
        let source_frame_f = source_time * audio_rate;
        let source_len = audio.frame_count() as f64;
        if source_frame_f >= source_len {
            continue;
        }

        // Linear interpolation between two source frames.
        let i0 = source_frame_f.floor() as usize;
        let frac = (source_frame_f - i0 as f64) as f32;
        let i1 = (i0 + 1).min(audio.frame_count().saturating_sub(1));
        let (l, r) = sample_stereo_interp(&audio.samples, audio_channels, i0, i1, frac);

        // Fade in / out shape (linear for v0).
        let mut env = 1.0f32;
        if fade_in > 0.0 && item_time < fade_in {
            env *= (item_time / fade_in) as f32;
        }
        let time_until_end = item_end - block_time;
        if fade_out > 0.0 && time_until_end < fade_out {
            env *= (time_until_end / fade_out).max(0.0) as f32;
        }
        let g = gain * env;

        bus.samples[frame * 2] += l * g;
        bus.samples[frame * 2 + 1] += r * g;
    }
}

fn sample_stereo_interp(
    samples: &[f32],
    channels: usize,
    i0: usize,
    i1: usize,
    frac: f32,
) -> (f32, f32) {
    let off0 = i0 * channels;
    let off1 = i1 * channels;
    let l = match channels {
        0 => 0.0,
        1 => {
            // Mono → both channels.
            let s0 = samples.get(off0).copied().unwrap_or(0.0);
            let s1 = samples.get(off1).copied().unwrap_or(0.0);
            s0 + (s1 - s0) * frac
        }
        _ => {
            let s0 = samples.get(off0).copied().unwrap_or(0.0);
            let s1 = samples.get(off1).copied().unwrap_or(0.0);
            s0 + (s1 - s0) * frac
        }
    };
    let r = match channels {
        0 | 1 => l,
        _ => {
            let s0 = samples.get(off0 + 1).copied().unwrap_or(0.0);
            let s1 = samples.get(off1 + 1).copied().unwrap_or(0.0);
            s0 + (s1 - s0) * frac
        }
    };
    (l, r)
}

fn apply_volume_pan(bus: &mut StereoBuffer, volume: f32, pan: f32) {
    // Constant-power pan; clamps pan to [-1, 1].
    let p = pan.clamp(-1.0, 1.0);
    let lg = ((1.0 - p) * 0.5).sqrt();
    let rg = ((1.0 + p) * 0.5).sqrt();
    for i in 0..bus.frames {
        let l = bus.samples[i * 2];
        let r = bus.samples[i * 2 + 1];
        bus.samples[i * 2] = l * lg * volume;
        bus.samples[i * 2 + 1] = r * rg * volume;
    }
}

fn add_with_volume_pan(dest: &mut StereoBuffer, src: &[f32], volume: f32, pan: f32) {
    let p = pan.clamp(-1.0, 1.0);
    let lg = ((1.0 - p) * 0.5).sqrt() * volume;
    let rg = ((1.0 + p) * 0.5).sqrt() * volume;
    for i in 0..dest.frames {
        dest.samples[i * 2] += src[i * 2] * lg;
        dest.samples[i * 2 + 1] += src[i * 2 + 1] * rg;
    }
}
