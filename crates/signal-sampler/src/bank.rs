//! SamplerBank — holds N named SampleEngine instances and mixes them.

use std::collections::HashMap;
use std::path::Path;

use sc_player::{PlayerPatch, SampleEngine};

use crate::InstrumentId;

/// Holds multiple [`SampleEngine`] instances and mixes them into one buffer.
///
/// Each instrument is identified by a user-chosen string key (e.g. `"strings_1v"`).
/// Instruments can be loaded, unloaded, and muted independently.
pub struct SamplerBank {
    engines: HashMap<InstrumentId, InstrumentSlot>,
    /// MIDI channel → instrument ID routing (channel 1–16, 1-based index).
    midi_channels: HashMap<u8, InstrumentId>,
    sample_rate: u32,
}

struct InstrumentSlot {
    engine: SampleEngine,
    muted: bool,
}

impl SamplerBank {
    pub fn new(sample_rate: u32) -> Self {
        Self {
            engines: HashMap::new(),
            midi_channels: HashMap::new(),
            sample_rate,
        }
    }

    /// Load a sample library from `spec_path` + optional `samples_root` WAV directory.
    ///
    /// If `samples_root` is `None`, the bank loads the spec only (useful for
    /// testing MIDI routing without actual audio).
    pub fn load_instrument(
        &mut self,
        id: impl Into<InstrumentId>,
        spec_path: &Path,
        samples_root: Option<&Path>,
        section: impl Into<String>,
        mic: impl Into<String>,
    ) -> eyre::Result<()> {
        let id = id.into();
        let patch = match samples_root {
            Some(root) => PlayerPatch::load(spec_path, root)?,
            None => {
                let spec = sc_player::LibrarySpec::from_file(spec_path)?;
                PlayerPatch::from_spec(spec)
            }
        };
        let engine = SampleEngine::new(patch, self.sample_rate, section, mic);
        tracing::info!("signal-sampler: loaded instrument {id:?}");
        self.engines.insert(id, InstrumentSlot { engine, muted: false });
        Ok(())
    }

    /// Remove an instrument from the bank.
    pub fn unload_instrument(&mut self, id: &str) {
        self.engines.remove(id);
        self.midi_channels.retain(|_, v| v != id);
    }

    /// Route a MIDI channel (1–16) to an instrument.
    pub fn set_midi_channel(&mut self, id: impl Into<InstrumentId>, channel: u8) {
        self.midi_channels.insert(channel, id.into());
    }

    /// Mute or unmute an instrument (still processes MIDI, just silent in mix).
    pub fn set_muted(&mut self, id: &str, muted: bool) {
        if let Some(slot) = self.engines.get_mut(id) {
            slot.muted = muted;
        }
    }

    // ── Direct MIDI ──────────────────────────────────────────────────────────

    pub fn note_on(&mut self, id: &str, note: u8, velocity: u8) {
        if let Some(slot) = self.engines.get_mut(id) {
            slot.engine.note_on(note, velocity);
        }
    }

    pub fn note_off(&mut self, id: &str, note: u8) {
        if let Some(slot) = self.engines.get_mut(id) {
            slot.engine.note_off(note);
        }
    }

    pub fn cc(&mut self, id: &str, controller: u8, value: u8) {
        if let Some(slot) = self.engines.get_mut(id) {
            slot.engine.cc(controller, value);
        }
    }

    // ── Channel-routed MIDI ──────────────────────────────────────────────────

    /// Dispatch a raw MIDI message to the instrument assigned to `channel` (1–16).
    ///
    /// Silently ignored if no instrument is mapped to that channel.
    pub fn midi_message(&mut self, channel: u8, status: u8, data1: u8, data2: u8) {
        let id = match self.midi_channels.get(&channel) {
            Some(id) => id.clone(),
            None => return,
        };
        let kind = status & 0xF0;
        match kind {
            0x80 => self.note_off(&id, data1),
            0x90 => {
                if data2 == 0 {
                    self.note_off(&id, data1);
                } else {
                    self.note_on(&id, data1, data2);
                }
            }
            0xB0 => self.cc(&id, data1, data2),
            _ => {}
        }
    }

    // ── Render ───────────────────────────────────────────────────────────────

    /// Mix all un-muted instruments into `output` (interleaved stereo, +=).
    pub fn render(&mut self, output: &mut [f32]) {
        for slot in self.engines.values_mut() {
            if !slot.muted {
                slot.engine.render(output);
            }
        }
    }

    /// Number of instruments currently loaded.
    pub fn len(&self) -> usize {
        self.engines.len()
    }

    pub fn is_empty(&self) -> bool {
        self.engines.is_empty()
    }

    /// IDs of all loaded instruments.
    pub fn instrument_ids(&self) -> Vec<&str> {
        self.engines.keys().map(|s| s.as_str()).collect()
    }
}
