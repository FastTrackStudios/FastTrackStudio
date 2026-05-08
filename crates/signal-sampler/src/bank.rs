//! SamplerBank — holds N named SampleEngine instances and mixes them.

use std::collections::HashMap;
use std::path::Path;

use crate::block::{BlockSpec, SamplerBlock};
use crate::engine::cache::{PreloadStats, SampleCache};

use crate::InstrumentId;
use std::path::PathBuf;

/// Pending preload work attached to a freshly-registered engine. Returned
/// by `register_pack` so callers can either spawn a per-pack preload
/// thread (single load) or pool the work into a coordinator (engine
/// presets that span multiple packs).
struct PendingPreload {
    cache: SampleCache,
    paths: Vec<PathBuf>,
}

/// One slot inside an [`EnginePreset`] — a single MIDI note triggers a
/// single `.signalpack`. Optional `priority` overrides the auto-derived
/// drum-kit priority order; lower numbers preload first.
#[derive(Debug, Clone, facet::Facet)]
pub struct EngineSlot {
    pub note: u8,
    /// Path to the `.signalpack`, relative to the preset file's directory
    /// (or absolute).
    pub pack: String,
    /// Optional override for the auto-derived drum-priority order.
    /// `0` = highest priority (decoded first).
    #[facet(default)]
    pub priority: Option<u8>,
}

/// Multi-pack instrument preset (e.g. "MM2 Standard Drum Kit").
///
/// Parsed from a `.engine.styx` file. Each slot maps a single MIDI note
/// to a `.signalpack`; loading the preset registers each pack as its own
/// engine in the bank and routes the corresponding MIDI note to it.
#[derive(Debug, Clone, facet::Facet)]
pub struct EnginePreset {
    pub name: String,
    #[facet(default)]
    pub description: String,
    pub slots: Vec<EngineSlot>,
}

impl EnginePreset {
    pub fn from_file(path: &Path) -> Result<Self, crate::SamplerError> {
        let text = std::fs::read_to_string(path)?;
        facet_styx::from_str(&text).map_err(|e| crate::SamplerError::SpecParse(e.to_string()))
    }
}

/// Map an instrument tag to a drum-kit preload priority. Lower values are
/// loaded first. Anything not in the table falls into the "other" bucket
/// at the end.
pub fn drum_priority(instrument: &str) -> u8 {
    match instrument {
        "kick" => 0,
        "snare" => 1,
        "hi-hat" => 2,
        "ride" => 3,
        "tom" => 4,
        "crash" => 5,
        "splash" => 6,
        "china" => 7,
        "stack" => 8,
        "cymbal" => 9,
        "effects" => 10,
        _ => 11,
    }
}

/// Holds multiple [`SampleEngine`] instances and mixes them into one buffer.
///
/// Each instrument is identified by a user-chosen string key (e.g. `"strings_1v"`).
/// Instruments can be loaded, unloaded, and muted independently.
pub struct SamplerBank {
    engines: HashMap<InstrumentId, InstrumentSlot>,
    /// MIDI channel → instrument ID routing (channel 1–16, 1-based index).
    midi_channels: HashMap<u8, InstrumentId>,
    /// Per-note routing populated by engine presets (drum kits etc.).
    /// When set, `note_on(_, note, _)` dispatches to the routed sub-instrument
    /// instead of the id passed in. Falls through to the given id when there
    /// is no route, so single-instrument playback (no preset loaded) still
    /// works the same way.
    note_routing: HashMap<u8, InstrumentId>,
    sample_rate: u32,
}

struct InstrumentSlot {
    block: SamplerBlock,
    muted: bool,
}

impl SamplerBank {
    pub fn new(sample_rate: u32) -> Self {
        Self {
            engines: HashMap::new(),
            midi_channels: HashMap::new(),
            note_routing: HashMap::new(),
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
            Some(root) => crate::PlayerPatch::load(spec_path, root)?,
            None => {
                let spec = crate::LibrarySpec::from_file(spec_path)?;
                crate::PlayerPatch::from_spec(spec)
            }
        };
        let engine = crate::SampleEngine::new(patch, self.sample_rate, section, mic);
        let block = SamplerBlock::from_engine(
            spec_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("block")
                .to_string(),
            engine,
            crate::block::BlockParams::default(),
        );
        tracing::info!("signal-sampler: loaded instrument {id:?}");
        self.engines.insert(
            id,
            InstrumentSlot {
                block,
                muted: false,
            },
        );
        Ok(())
    }

    /// Load an instrument directly from a `.signalpack`.
    ///
    /// Section/mic default to the first entries declared in the pack's
    /// embedded spec, or empty strings when absent. The pack supplies all
    /// audio — no on-disk source files required.
    pub fn load_pack(&mut self, id: impl Into<InstrumentId>, pack_path: &Path) -> eyre::Result<()> {
        let id = id.into();
        let cache = self.register_pack(id.clone(), pack_path)?;
        // Single-pack load: spawn a dedicated preload thread immediately
        // (engine-preset loads use a coordinator to order packs by drum
        // priority and don't go through this path).
        let pack_label = pack_path.display().to_string();
        std::thread::Builder::new()
            .name(format!("signal-preload:{}", pack_label))
            .spawn(move || {
                let start = std::time::Instant::now();
                tracing::info!(
                    pack = %pack_label,
                    paths = cache.paths.len(),
                    "background preload starting",
                );
                let stats = cache.cache.preload(cache.paths.iter().map(|p| p.as_path()));
                tracing::info!(
                    pack = %pack_label,
                    loaded = stats.loaded,
                    failed = stats.failed,
                    bytes = stats.bytes,
                    elapsed_ms = start.elapsed().as_millis() as u64,
                    "background preload complete",
                );
            })
            .expect("spawn signal-preload thread");
        tracing::info!(
            id = ?id,
            pack = %pack_path.display(),
            "signal-sampler: loaded pack (preload streaming in background)",
        );
        Ok(())
    }

    /// Build a `SampleEngine` from a `.signalpack`, install it under `id`,
    /// and return a handle the caller can use to drive a (possibly shared)
    /// background preloader. Does **not** spawn a preload thread on its
    /// own — that's the caller's job.
    fn register_pack(
        &mut self,
        id: InstrumentId,
        pack_path: &Path,
    ) -> eyre::Result<PendingPreload> {
        let block = SamplerBlock::from_pack(pack_path, self.sample_rate)?;
        let cache = block.cache_handle();
        let paths = block.sample_paths_centered(60);
        self.engines.insert(
            id,
            InstrumentSlot {
                block,
                muted: false,
            },
        );
        Ok(PendingPreload { cache, paths })
    }

    /// Register a `.signalblock` file. Resolves the referenced `.signalpack`
    /// and applies the spec's params (gain, pan, transpose, …).
    pub fn load_block(
        &mut self,
        id: impl Into<InstrumentId>,
        block_path: &Path,
    ) -> eyre::Result<()> {
        let id = id.into();
        let spec = BlockSpec::from_file(block_path)?;
        let dir = block_path.parent().unwrap_or(Path::new(""));
        let block = SamplerBlock::from_spec(spec, dir, self.sample_rate)?;
        let cache = block.cache_handle();
        let paths = block.sample_paths_centered(60);
        let label = block_path.display().to_string();
        self.engines.insert(
            id.clone(),
            InstrumentSlot {
                block,
                muted: false,
            },
        );
        std::thread::Builder::new()
            .name(format!("signal-preload:{}", label))
            .spawn(move || {
                let stats = cache.preload(paths.iter().map(|p| p.as_path()));
                tracing::info!(
                    block = %label,
                    loaded = stats.loaded,
                    failed = stats.failed,
                    "block preload complete",
                );
            })
            .expect("spawn signal-preload thread");
        tracing::info!(id = ?id, block = %block_path.display(), "loaded block");
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

    /// Load a multi-pack preset (drum kit, multi-mic instrument, …).
    ///
    /// Each [`EngineSlot`] becomes its own engine in the bank under
    /// `id_prefix:<note>`. The bank's note routing table is updated so
    /// `note_on(any_id, note, vel)` dispatches to the right slot.
    /// One coordinator thread streams the preload across packs in
    /// **drum priority order** (kick → snare → hats → toms → ride → …),
    /// so the most-played pieces are audible first.
    pub fn load_engine_preset(
        &mut self,
        id_prefix: &str,
        preset: &EnginePreset,
        preset_dir: &Path,
    ) -> eyre::Result<Vec<InstrumentId>> {
        let mut slot_work: Vec<(u8, PendingPreload, u8)> = Vec::new();
        let mut slot_ids: Vec<InstrumentId> = Vec::new();
        for slot in &preset.slots {
            let pack_buf = PathBuf::from(&slot.pack);
            let pack_path = if pack_buf.is_absolute() {
                pack_buf
            } else {
                preset_dir.join(pack_buf)
            };
            let id: InstrumentId = format!("{id_prefix}:{}", slot.note);
            let pending = self.register_pack(id.clone(), &pack_path)?;
            // Auto-derived priority from the pack's `instrument` tag —
            // overridable by an explicit `priority` on the slot.
            let auto_prio = self
                .engines
                .get(&id)
                .map(|s| drum_priority(&s.block.patch().spec.instrument))
                .unwrap_or(11);
            let prio = slot.priority.unwrap_or(auto_prio);
            slot_work.push((slot.note, pending, prio));
            self.note_routing.insert(slot.note, id.clone());
            slot_ids.push(id);
        }
        // Order packs by priority — kick first, snare next, …
        slot_work.sort_by_key(|(_, _, p)| *p);

        let preset_label = preset.name.clone();
        let total_paths: usize = slot_work.iter().map(|(_, p, _)| p.paths.len()).sum();
        std::thread::Builder::new()
            .name(format!("signal-preload-kit:{}", preset_label))
            .spawn(move || {
                let start = std::time::Instant::now();
                tracing::info!(
                    preset = %preset_label,
                    slots = slot_work.len(),
                    paths = total_paths,
                    "kit preload starting (priority order)",
                );
                let mut total_loaded = 0;
                for (note, pending, prio) in &slot_work {
                    let pack_start = std::time::Instant::now();
                    let stats = pending
                        .cache
                        .preload(pending.paths.iter().map(|p| p.as_path()));
                    total_loaded += stats.loaded;
                    tracing::info!(
                        preset = %preset_label,
                        note = note,
                        priority = prio,
                        loaded = stats.loaded,
                        elapsed_ms = pack_start.elapsed().as_millis() as u64,
                        "kit slot ready",
                    );
                }
                tracing::info!(
                    preset = %preset_label,
                    loaded = total_loaded,
                    total = total_paths,
                    elapsed_ms = start.elapsed().as_millis() as u64,
                    "kit preload complete",
                );
            })
            .expect("spawn signal-preload-kit thread");

        tracing::info!(
            preset = %preset.name,
            slots = preset.slots.len(),
            "loaded engine preset",
        );
        Ok(slot_ids)
    }

    /// `(loaded, total)` sample counts for a given instrument, or
    /// `(0, 0)` if the id isn't loaded. Lock-free and cheap; safe to call
    /// per UI frame.
    pub fn preload_progress(&self, id: &str) -> (usize, usize) {
        match self.engines.get(id) {
            Some(slot) => (
                slot.block.loaded_sample_count(),
                slot.block.total_sample_count(),
            ),
            None => (0, 0),
        }
    }

    /// Decode all samples for a loaded instrument into RAM.
    pub fn preload_instrument(&mut self, id: &str) -> eyre::Result<PreloadStats> {
        let slot = self
            .engines
            .get_mut(id)
            .ok_or_else(|| eyre::eyre!("instrument not loaded: {id}"))?;
        Ok(slot.block.preload_samples())
    }

    // ── Direct MIDI ──────────────────────────────────────────────────────────

    pub fn note_on(&mut self, id: &str, note: u8, velocity: u8) {
        let routed = self.note_routing.get(&note).cloned();
        let target = routed.as_deref().unwrap_or(id);
        if let Some(slot) = self.engines.get_mut(target) {
            slot.block.note_on(note, velocity);
        }
    }

    pub fn note_off(&mut self, id: &str, note: u8) {
        let routed = self.note_routing.get(&note).cloned();
        let target = routed.as_deref().unwrap_or(id);
        if let Some(slot) = self.engines.get_mut(target) {
            slot.block.note_off(note);
        }
    }

    pub fn note_off_with_velocity(&mut self, id: &str, note: u8, velocity: u8) {
        let routed = self.note_routing.get(&note).cloned();
        let target = routed.as_deref().unwrap_or(id);
        if let Some(slot) = self.engines.get_mut(target) {
            slot.block.note_off_with_velocity(note, velocity);
        }
    }

    pub fn cc(&mut self, id: &str, controller: u8, value: u8) {
        if let Some(slot) = self.engines.get_mut(id) {
            slot.block.cc(controller, value);
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
            0x80 => self.note_off_with_velocity(&id, data1, data2),
            0x90 => {
                if data2 == 0 {
                    self.note_off_with_velocity(&id, data1, data2);
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
                slot.block.render(output);
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
