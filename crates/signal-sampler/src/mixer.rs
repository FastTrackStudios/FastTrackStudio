//! Drum-kit mixer overlay for a [`PresetRuntime`](crate::runtime::PresetRuntime).
//!
//! Models the send-based routing of a real drum console:
//!
//! - **Close mics** (e.g. `In 1`, `In 2`) of each piece go **direct** to the
//!   master — one [`DirectChannel`] each (fader + mute + solo + meter).
//! - **Overhead / Room** mics of each piece do NOT go out the close track;
//!   they are **sent** (with a per-piece level) to a shared bus track — one
//!   [`Send`] each (level + mute + solo + meter).
//! - Each shared **bus** (`Overhead`, `Room Close`, …) sums every piece's send
//!   and goes to master — one [`Bus`] each (fader + mute + solo + meter).
//! - A single **master** fader + mute scales the whole kit, with a master meter.
//!
//! The render loop lives on `PresetRuntime` (which owns the engines' per-mic
//! scratch buffers); this module holds the routing config, the bus + master
//! accumulators, and the lock-free peak meters.

use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};

/// Per-block peak-meter decay (peak-hold). Applied once per audio block so the
/// UI sees a smooth fall even when it polls slowly.
const METER_DECAY: f32 = 0.85;

fn db_to_lin(db: f32) -> f32 {
    10f32.powf(db / 20.0)
}

/// A drum mic id routes to a shared **bus** (overhead / room) rather than
/// straight out a close-mic channel when its name looks like an overhead or
/// room/ambient mic. Everything else is a direct close mic.
pub fn mic_is_bus(id: &str) -> bool {
    let l = id.to_ascii_lowercase();
    l == "oh"
        || l.contains("overhead")
        || l.starts_with("room")
        || l.contains("ambien")
        || l.contains("ambc")
}

/// Lock-free peak meters: the audio thread stores per-block peaks; the UI reads
/// them via a cloned `Arc` without touching the bank lock. Values are linear
/// peak amplitude (~0..1+); the UI converts to dB.
#[derive(Debug)]
pub struct MixerMeters {
    channels: Vec<AtomicU32>,
    sends: Vec<AtomicU32>,
    buses: Vec<AtomicU32>,
    master: AtomicU32,
}

impl MixerMeters {
    fn new(channels: usize, sends: usize, buses: usize) -> Self {
        let mk = |n| (0..n).map(|_| AtomicU32::new(0)).collect();
        Self {
            channels: mk(channels),
            sends: mk(sends),
            buses: mk(buses),
            master: AtomicU32::new(0),
        }
    }

    fn store_peak(slot: &AtomicU32, block_peak: f32) {
        let prev = f32::from_bits(slot.load(Ordering::Relaxed));
        let held = (prev * METER_DECAY).max(block_peak);
        slot.store(held.to_bits(), Ordering::Relaxed);
    }

    fn read(slot: &AtomicU32) -> f32 {
        f32::from_bits(slot.load(Ordering::Relaxed))
    }

    /// Current peak (linear) of direct channel `i`.
    pub fn channel_peak(&self, i: usize) -> f32 {
        self.channels.get(i).map(Self::read).unwrap_or(0.0)
    }
    /// Current peak (linear) of send `i`.
    pub fn send_peak(&self, i: usize) -> f32 {
        self.sends.get(i).map(Self::read).unwrap_or(0.0)
    }
    /// Current peak (linear) of bus `i`.
    pub fn bus_peak(&self, i: usize) -> f32 {
        self.buses.get(i).map(Self::read).unwrap_or(0.0)
    }
    /// Current peak (linear) of the master output.
    pub fn master_peak(&self) -> f32 {
        Self::read(&self.master)
    }

    // Peak setters used by the audio render (via the `meters` field, so they
    // don't borrow the whole mixer while its scratch buffers are borrowed mut).
    pub(crate) fn set_channel_peak(&self, i: usize, peak: f32) {
        if let Some(s) = self.channels.get(i) {
            Self::store_peak(s, peak);
        }
    }
    pub(crate) fn set_send_peak(&self, i: usize, peak: f32) {
        if let Some(s) = self.sends.get(i) {
            Self::store_peak(s, peak);
        }
    }
    pub(crate) fn set_bus_peak(&self, i: usize, peak: f32) {
        if let Some(s) = self.buses.get(i) {
            Self::store_peak(s, peak);
        }
    }
    pub(crate) fn set_master_peak(&self, peak: f32) {
        Self::store_peak(&self.master, peak);
    }
}

/// A close-mic channel that goes direct to master.
#[derive(Debug)]
pub struct DirectChannel {
    pub engine_idx: usize,
    pub mic_idx: usize,
    pub engine_label: String,
    pub mic_label: String,
    pub gain_db: f32,
    pub gain_lin: f32,
    pub muted: bool,
    pub soloed: bool,
}

/// A bus-mic send from one piece into a shared bus.
#[derive(Debug)]
pub struct Send {
    pub engine_idx: usize,
    pub mic_idx: usize,
    pub bus_idx: usize,
    pub engine_label: String,
    pub mic_label: String,
    pub level_db: f32,
    pub level_lin: f32,
    pub muted: bool,
    pub soloed: bool,
}

/// A shared bus track (sum of all pieces' sends of one mic id) → master.
#[derive(Debug)]
pub struct Bus {
    pub id: String,
    pub label: String,
    pub gain_db: f32,
    pub gain_lin: f32,
    pub muted: bool,
    pub soloed: bool,
    /// Interleaved-stereo accumulator, re-sized to the current block.
    pub(crate) acc: Vec<f32>,
}

/// Routing config + bus/master accumulators + meters for a drum preset.
#[derive(Debug)]
pub struct DrumMixer {
    pub channels: Vec<DirectChannel>,
    pub sends: Vec<Send>,
    pub buses: Vec<Bus>,
    pub master_gain_db: f32,
    pub master_gain_lin: f32,
    pub master_muted: bool,
    pub meters: Arc<MixerMeters>,
    /// Scratch the whole kit mix is summed into (so master gain + meter apply
    /// to this preset's output alone, then it's added to the host buffer).
    pub(crate) master_scratch: Vec<f32>,
}

impl DrumMixer {
    /// Build a mixer from each engine's `(label, mic_ids)`. Close mics become
    /// direct channels; overhead/room mics become sends into shared buses
    /// (one bus per distinct bus-mic id, in first-seen order).
    pub fn build(engine_mics: &[(String, Vec<String>)]) -> Self {
        let mut channels = Vec::new();
        let mut sends = Vec::new();
        let mut buses: Vec<Bus> = Vec::new();
        let mut bus_idx_of: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();

        for (engine_idx, (engine_label, mics)) in engine_mics.iter().enumerate() {
            for (mic_idx, mic_id) in mics.iter().enumerate() {
                if mic_is_bus(mic_id) {
                    let bus_idx = *bus_idx_of.entry(mic_id.clone()).or_insert_with(|| {
                        buses.push(Bus {
                            id: mic_id.clone(),
                            label: mic_id.clone(),
                            gain_db: 0.0,
                            gain_lin: 1.0,
                            muted: false,
                            soloed: false,
                            acc: Vec::new(),
                        });
                        buses.len() - 1
                    });
                    sends.push(Send {
                        engine_idx,
                        mic_idx,
                        bus_idx,
                        engine_label: engine_label.clone(),
                        mic_label: mic_id.clone(),
                        level_db: 0.0,
                        level_lin: 1.0,
                        muted: false,
                        soloed: false,
                    });
                } else {
                    channels.push(DirectChannel {
                        engine_idx,
                        mic_idx,
                        engine_label: engine_label.clone(),
                        mic_label: mic_id.clone(),
                        gain_db: 0.0,
                        gain_lin: 1.0,
                        muted: false,
                        soloed: false,
                    });
                }
            }
        }

        let meters = Arc::new(MixerMeters::new(channels.len(), sends.len(), buses.len()));
        Self {
            channels,
            sends,
            buses,
            master_gain_db: 0.0,
            master_gain_lin: 1.0,
            master_muted: false,
            meters,
            master_scratch: Vec::new(),
        }
    }

    /// True if there is anything to route (avoids engaging on empty presets).
    pub fn is_empty(&self) -> bool {
        self.channels.is_empty() && self.sends.is_empty()
    }

    /// Any channel / send / bus currently soloed → solo-in-place is active.
    pub(crate) fn any_solo(&self) -> bool {
        self.channels.iter().any(|c| c.soloed)
            || self.sends.iter().any(|s| s.soloed)
            || self.buses.iter().any(|b| b.soloed)
    }

    pub fn set_channel_gain_db(&mut self, i: usize, db: f32) {
        if let Some(ch) = self.channels.get_mut(i) {
            ch.gain_db = db;
            ch.gain_lin = db_to_lin(db);
        }
    }
    pub fn set_channel_mute(&mut self, i: usize, muted: bool) {
        if let Some(ch) = self.channels.get_mut(i) {
            ch.muted = muted;
        }
    }
    pub fn set_channel_solo(&mut self, i: usize, soloed: bool) {
        if let Some(ch) = self.channels.get_mut(i) {
            ch.soloed = soloed;
        }
    }
    pub fn set_send_level_db(&mut self, i: usize, db: f32) {
        if let Some(s) = self.sends.get_mut(i) {
            s.level_db = db;
            s.level_lin = db_to_lin(db);
        }
    }
    pub fn set_send_mute(&mut self, i: usize, muted: bool) {
        if let Some(s) = self.sends.get_mut(i) {
            s.muted = muted;
        }
    }
    pub fn set_send_solo(&mut self, i: usize, soloed: bool) {
        if let Some(s) = self.sends.get_mut(i) {
            s.soloed = soloed;
        }
    }
    pub fn set_bus_gain_db(&mut self, i: usize, db: f32) {
        if let Some(b) = self.buses.get_mut(i) {
            b.gain_db = db;
            b.gain_lin = db_to_lin(db);
        }
    }
    pub fn set_bus_mute(&mut self, i: usize, muted: bool) {
        if let Some(b) = self.buses.get_mut(i) {
            b.muted = muted;
        }
    }
    pub fn set_bus_solo(&mut self, i: usize, soloed: bool) {
        if let Some(b) = self.buses.get_mut(i) {
            b.soloed = soloed;
        }
    }
    pub fn set_master_gain_db(&mut self, db: f32) {
        self.master_gain_db = db;
        self.master_gain_lin = db_to_lin(db);
    }
    pub fn set_master_mute(&mut self, muted: bool) {
        self.master_muted = muted;
    }
}

// ── Cloneable layout snapshot for the UI ────────────────────────────────────

/// A flat, cloneable description of the mixer for rendering. Meter indices line
/// up with [`MixerMeters`] (`channel_idx`/`send_idx`/`bus_idx`); setters on
/// the player take the same indices.
#[derive(Clone, Debug, Default)]
pub struct MixerLayout {
    pub engines: Vec<EngineStrip>,
    pub buses: Vec<BusStrip>,
    pub master_gain_db: f32,
    pub master_muted: bool,
}

#[derive(Clone, Debug)]
pub struct EngineStrip {
    pub engine_idx: usize,
    pub label: String,
    pub channels: Vec<ChannelStrip>,
    pub sends: Vec<SendStrip>,
}

#[derive(Clone, Debug)]
pub struct ChannelStrip {
    pub channel_idx: usize,
    pub mic_label: String,
    pub gain_db: f32,
    pub muted: bool,
    pub soloed: bool,
}

#[derive(Clone, Debug)]
pub struct SendStrip {
    pub send_idx: usize,
    pub mic_label: String,
    pub bus_idx: usize,
    pub bus_label: String,
    pub level_db: f32,
    pub muted: bool,
    pub soloed: bool,
}

#[derive(Clone, Debug)]
pub struct BusStrip {
    pub bus_idx: usize,
    pub label: String,
    pub gain_db: f32,
    pub muted: bool,
    pub soloed: bool,
}

impl DrumMixer {
    /// Snapshot the current structure + fader/mute/solo state, grouped by engine.
    pub fn layout(&self) -> MixerLayout {
        let mut engines: Vec<EngineStrip> = Vec::new();
        let mut idx_of_engine: std::collections::HashMap<usize, usize> =
            std::collections::HashMap::new();
        let engine_slot = |engines: &mut Vec<EngineStrip>,
                           idx_of: &mut std::collections::HashMap<usize, usize>,
                           engine_idx: usize,
                           label: &str|
         -> usize {
            *idx_of.entry(engine_idx).or_insert_with(|| {
                engines.push(EngineStrip {
                    engine_idx,
                    label: label.to_string(),
                    channels: Vec::new(),
                    sends: Vec::new(),
                });
                engines.len() - 1
            })
        };

        for (channel_idx, ch) in self.channels.iter().enumerate() {
            let slot = engine_slot(
                &mut engines,
                &mut idx_of_engine,
                ch.engine_idx,
                &ch.engine_label,
            );
            engines[slot].channels.push(ChannelStrip {
                channel_idx,
                mic_label: ch.mic_label.clone(),
                gain_db: ch.gain_db,
                muted: ch.muted,
                soloed: ch.soloed,
            });
        }
        for (send_idx, s) in self.sends.iter().enumerate() {
            let slot = engine_slot(
                &mut engines,
                &mut idx_of_engine,
                s.engine_idx,
                &s.engine_label,
            );
            let bus_label = self
                .buses
                .get(s.bus_idx)
                .map(|b| b.label.clone())
                .unwrap_or_default();
            engines[slot].sends.push(SendStrip {
                send_idx,
                mic_label: s.mic_label.clone(),
                bus_idx: s.bus_idx,
                bus_label,
                level_db: s.level_db,
                muted: s.muted,
                soloed: s.soloed,
            });
        }

        let buses = self
            .buses
            .iter()
            .enumerate()
            .map(|(bus_idx, b)| BusStrip {
                bus_idx,
                label: b.label.clone(),
                gain_db: b.gain_db,
                muted: b.muted,
                soloed: b.soloed,
            })
            .collect();

        MixerLayout {
            engines,
            buses,
            master_gain_db: self.master_gain_db,
            master_muted: self.master_muted,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mm2_like() -> Vec<(String, Vec<String>)> {
        let mics = |v: &[&str]| v.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        vec![
            (
                "kick".into(),
                mics(&["In 1", "In 2", "Overhead", "Room Close", "Room Far"]),
            ),
            (
                "snare".into(),
                mics(&["In 1", "In 2", "Overhead", "Room Close", "Room Far"]),
            ),
        ]
    }

    #[test]
    fn classifies_close_vs_bus() {
        assert!(!mic_is_bus("In 1"));
        assert!(!mic_is_bus("Top"));
        assert!(mic_is_bus("Overhead"));
        assert!(mic_is_bus("OH"));
        assert!(mic_is_bus("Room Close"));
        assert!(mic_is_bus("Room Far"));
        assert!(mic_is_bus("Room Mono"));
    }

    #[test]
    fn builds_direct_channels_sends_and_shared_buses() {
        let m = DrumMixer::build(&mm2_like());
        assert_eq!(m.channels.len(), 4);
        assert_eq!(m.sends.len(), 6);
        assert_eq!(m.buses.len(), 3);
        assert_eq!(m.buses[0].label, "Overhead");
        let oh_sends: Vec<_> = m
            .sends
            .iter()
            .filter(|s| s.mic_label == "Overhead")
            .collect();
        assert_eq!(oh_sends.len(), 2);
        assert_eq!(oh_sends[0].bus_idx, oh_sends[1].bus_idx);
    }

    #[test]
    fn layout_groups_by_engine() {
        let m = DrumMixer::build(&mm2_like());
        let layout = m.layout();
        assert_eq!(layout.engines.len(), 2);
        assert_eq!(layout.engines[0].label, "kick");
        assert_eq!(layout.engines[0].channels.len(), 2);
        assert_eq!(layout.engines[0].sends.len(), 3);
        assert_eq!(layout.buses.len(), 3);
    }

    #[test]
    fn setters_update_gain_mute_solo_master() {
        let mut m = DrumMixer::build(&mm2_like());
        m.set_channel_gain_db(0, -6.0);
        assert!((m.channels[0].gain_lin - db_to_lin(-6.0)).abs() < 1e-6);
        m.set_send_mute(0, true);
        assert!(m.sends[0].muted);
        m.set_bus_solo(0, true);
        assert!(m.buses[0].soloed);
        assert!(m.any_solo());
        m.set_bus_solo(0, false);
        assert!(!m.any_solo());
        m.set_master_gain_db(3.0);
        assert!((m.master_gain_lin - db_to_lin(3.0)).abs() < 1e-6);
    }
}
