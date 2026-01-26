//! MockRig - In-memory rig service for testing and web demos
//!
//! This implementation provides a fully functional rig service that doesn't
//! require REAPER. Useful for:
//! - Unit testing business logic
//! - Integration testing without DAW
//! - Web demos and previews
//! - Development/prototyping

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};

use roam::{Context, Tx};
use uuid::Uuid;

use super::core::{
    BaseTone, Block, BlockOverride, BlockParameter, BlockType, Genre, GlobalBlock, GlobalBlockConfig,
    InstrumentType, Layer, Patch, PatchAssignment, PatchCategory, PatchVariation, PerformanceSong,
    PerformanceSetlist, Preset, PresetBlock, PresetCategory, PresetTag, Profile, Rig, Scene,
    SceneTransition, Section, SectionConfig, SectionRouting, Snapshot, TagCategory, TagRegistry, Tags,
};
use super::service::{
    GlobalBlockInfo, PatchInfo, PresetInfo, ProfileInfo, RigCommand, RigEvent, RigInfo, RigService,
    SceneInfo, SectionInfo, SetlistInfo, SongInfo,
};

// Platform-agnostic broadcast channel
#[cfg(not(target_arch = "wasm32"))]
use tokio::sync::broadcast;

#[cfg(target_arch = "wasm32")]
mod wasm_broadcast {
    use std::sync::{Arc, Mutex};

    pub struct Sender<T: Clone> {
        subscribers: Arc<Mutex<Vec<async_channel::Sender<T>>>>,
    }

    impl<T: Clone> Clone for Sender<T> {
        fn clone(&self) -> Self {
            Self {
                subscribers: Arc::clone(&self.subscribers),
            }
        }
    }

    pub struct Receiver<T> {
        inner: async_channel::Receiver<T>,
    }

    impl<T: Clone> Sender<T> {
        pub fn send(&self, value: T) -> Result<usize, ()> {
            let mut subs = self.subscribers.lock().unwrap();
            subs.retain(|tx| !tx.is_closed());
            let count = subs.len();
            for tx in subs.iter() {
                let _ = tx.try_send(value.clone());
            }
            Ok(count)
        }

        pub fn subscribe(&self) -> Receiver<T> {
            let (tx, rx) = async_channel::bounded(64);
            self.subscribers.lock().unwrap().push(tx);
            Receiver { inner: rx }
        }
    }

    impl<T> Receiver<T> {
        pub async fn recv(&mut self) -> Result<T, RecvError> {
            self.inner.recv().await.map_err(|_| RecvError)
        }
    }

    pub struct RecvError;

    pub fn channel<T: Clone>(_capacity: usize) -> (Sender<T>, Receiver<T>) {
        let sender = Sender {
            subscribers: Arc::new(Mutex::new(Vec::new())),
        };
        let receiver = sender.subscribe();
        (sender, receiver)
    }
}

#[cfg(target_arch = "wasm32")]
use wasm_broadcast as broadcast;

// region:    --- MockRig

/// Mock implementation of RigService for testing and web demos.
pub struct MockRig {
    /// Available profiles
    profiles: Arc<RwLock<Vec<Profile>>>,
    /// Available rigs
    rigs: Arc<RwLock<Vec<Rig>>>,
    /// Available patches (shared across profiles)
    patches: Arc<RwLock<Vec<Patch>>>,
    /// Currently loaded profile
    current_profile: Arc<RwLock<Option<Profile>>>,
    /// Currently loaded rig
    current_rig: Arc<RwLock<Option<Rig>>>,
    /// Currently loaded preset
    current_preset: Arc<RwLock<Option<Preset>>>,
    /// Current snapshot ID
    current_snapshot_id: Arc<RwLock<Option<Uuid>>>,
    /// Available setlists
    setlists: Arc<RwLock<Vec<PerformanceSetlist>>>,
    /// Current setlist
    current_setlist: Arc<RwLock<Option<PerformanceSetlist>>>,
    /// Current song index
    current_song_index: Arc<RwLock<usize>>,
    /// Current scene index
    current_scene_index: Arc<RwLock<usize>>,
    /// Broadcast channel for RigEvent
    event_tx: broadcast::Sender<RigEvent>,
    /// Preloaded preset IDs
    preloaded: Arc<RwLock<Vec<Uuid>>>,
}

impl MockRig {
    /// Create a new empty MockRig
    pub fn new() -> Self {
        let (event_tx, _rx) = broadcast::channel(64);
        Self {
            profiles: Arc::new(RwLock::new(Vec::new())),
            rigs: Arc::new(RwLock::new(Vec::new())),
            patches: Arc::new(RwLock::new(Vec::new())),
            current_profile: Arc::new(RwLock::new(None)),
            current_rig: Arc::new(RwLock::new(None)),
            current_preset: Arc::new(RwLock::new(None)),
            current_snapshot_id: Arc::new(RwLock::new(None)),
            setlists: Arc::new(RwLock::new(Vec::new())),
            current_setlist: Arc::new(RwLock::new(None)),
            current_song_index: Arc::new(RwLock::new(0)),
            current_scene_index: Arc::new(RwLock::new(0)),
            event_tx,
            preloaded: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Create a MockRig with sample guitar data
    pub fn with_sample_guitar_data() -> Self {
        let mock = Self::new();

        // Create a guitar rig
        let mut rig = Rig::new("Guitar Rig", InstrumentType::Guitar);

        // Guitar rigs are primarily block-based (no sections/engines needed)
        // Sections are more for Keys rigs with multiple sound engines

        // Add blocks for guitar signal chain
        // 1. Drive/Overdrive block
        let drive_plugin = super::core::PluginId::vst3("drive-uid", "Tube Screamer");
        let drive_block = GlobalBlock::new(Block::new("Drive", drive_plugin), 0);
        let drive_id = drive_block.block.id;
        rig.add_global_block(drive_block);

        // 2. Amp block
        let amp_plugin = super::core::PluginId::vst3("amp-uid", "Marshall JCM800");
        let amp_block = GlobalBlock::new(Block::new("Amp", amp_plugin), 1);
        let amp_id = amp_block.block.id;
        rig.add_global_block(amp_block);

        // 3. Cab/IR block
        let cab_plugin = super::core::PluginId::vst3("cab-uid", "4x12 Vintage 30");
        let cab_block = GlobalBlock::new(Block::new("Cab", cab_plugin), 2);
        let cab_id = cab_block.block.id;
        rig.add_global_block(cab_block);

        // 4. Delay block
        let delay_plugin = super::core::PluginId::vst3("delay-uid", "Tape Delay");
        let delay_block = GlobalBlock::new(Block::new("Delay", delay_plugin), 3);
        let delay_id = delay_block.block.id;
        rig.add_global_block(delay_block);

        // 5. Reverb block
        let reverb_plugin = super::core::PluginId::vst3("reverb-uid", "Hall Reverb");
        let reverb_block = GlobalBlock::new(Block::new("Reverb", reverb_plugin), 4);
        let reverb_id = reverb_block.block.id;
        rig.add_global_block(reverb_block);

        let rig_id = rig.id;

        // For presets, we'll use placeholder section IDs since guitar rigs
        // don't really use sections - but the data model requires them
        let amp_section_id = Uuid::new_v4();
        let effects_section_id = Uuid::new_v4();

        // Create patches
        let mut patches = Vec::new();

        // Clean patch
        let mut clean_patch = Patch::new("Clean", PatchCategory::clean());
        clean_patch.add_variation(PatchVariation::soft());
        clean_patch.add_variation(PatchVariation::normal());
        clean_patch.add_variation(PatchVariation::bright());
        let clean_id = clean_patch.id;
        let clean_soft_id = clean_patch.variations[0].id;
        let clean_normal_id = clean_patch.variations[1].id;
        patches.push(clean_patch);

        // Crunch patch
        let mut crunch_patch = Patch::new("Crunch", PatchCategory::crunch());
        crunch_patch.add_variation(PatchVariation::soft());
        crunch_patch.add_variation(PatchVariation::normal());
        crunch_patch.add_variation(PatchVariation::hard());
        let crunch_id = crunch_patch.id;
        patches.push(crunch_patch);

        // Drive patch
        let mut drive_patch = Patch::new("Drive", PatchCategory::drive());
        drive_patch.add_variation(PatchVariation::normal());
        drive_patch.add_variation(PatchVariation::hard());
        let drive_id = drive_patch.id;
        let drive_normal_id = drive_patch.variations[0].id;
        let drive_hard_id = drive_patch.variations[1].id;
        patches.push(drive_patch);

        // Lead patch
        let mut lead_patch = Patch::new("Lead", PatchCategory::lead());
        lead_patch.add_variation(PatchVariation::normal());
        lead_patch.add_variation(PatchVariation::bright());
        let lead_id = lead_patch.id;
        patches.push(lead_patch);

        // Create profile with presets
        let mut profile = Profile::new("FTS-Guitar", rig_id);

        // Create a tag registry to get consistent tag IDs
        let tag_registry = TagRegistry::with_defaults();

        // ========== DEFAULT GUITAR RIG PRESET ==========
        // Standard signal chain: Input Gain -> Comp -> EQ -> Boost -> Drive 1 -> Drive 2
        //                        -> Delay -> Reverb -> Amp -> Cab -> EQ -> Reverb -> Delay

        let mut default_rig = Preset::new("Default Guitar", PresetCategory::generic(BaseTone::Clean));
        default_rig.rating = 5;
        default_rig.notes = Some("Full-featured guitar signal chain with pre/post effects".to_string());

        // 1. Input Gain - trim input level
        let input_gain = PresetBlock::new("Input Gain", BlockType::Input, 0)
            .with_params([
                BlockParameter::new("gain", "Gain", 0.5).with_range(-12.0, 12.0).with_unit("dB"),
                BlockParameter::new("phase", "Phase", 0.0).with_range(0.0, 1.0),
            ]);
        let input_gain_id = input_gain.id;
        default_rig.add_block(input_gain);

        // 2. Compressor - tame dynamics
        let compressor = PresetBlock::compressor("Comp", 1)
            .with_params([
                BlockParameter::new("threshold", "Threshold", 0.4).with_range(-40.0, 0.0).with_unit("dB"),
                BlockParameter::new("ratio", "Ratio", 0.3).with_range(1.0, 20.0).with_unit(":1"),
                BlockParameter::new("attack", "Attack", 0.2).with_range(0.1, 100.0).with_unit("ms"),
                BlockParameter::new("release", "Release", 0.5).with_range(10.0, 1000.0).with_unit("ms"),
                BlockParameter::new("makeup", "Makeup", 0.5).with_range(0.0, 20.0).with_unit("dB"),
            ]);
        let compressor_id = compressor.id;
        default_rig.add_block(compressor);

        // 3. EQ (Pre) - shape input tone
        let pre_eq = PresetBlock::eq("Pre EQ", 2)
            .with_params([
                BlockParameter::new("low", "Low", 0.5).with_range(-12.0, 12.0).with_unit("dB"),
                BlockParameter::new("mid", "Mid", 0.5).with_range(-12.0, 12.0).with_unit("dB"),
                BlockParameter::new("high", "High", 0.5).with_range(-12.0, 12.0).with_unit("dB"),
                BlockParameter::new("low_freq", "Low Freq", 0.3).with_range(60.0, 400.0).with_unit("Hz"),
                BlockParameter::new("high_freq", "High Freq", 0.6).with_range(2000.0, 8000.0).with_unit("Hz"),
            ]);
        let pre_eq_id = pre_eq.id;
        default_rig.add_block(pre_eq);

        // 4. Boost - clean volume boost for solos
        let boost = PresetBlock::drive("Boost", 3)
            .bypassed(true)
            .with_params([
                BlockParameter::new("boost", "Boost", 0.5).with_range(0.0, 20.0).with_unit("dB"),
                BlockParameter::new("bright", "Bright", 0.4).with_unit("%"),
            ]);
        let boost_id = boost.id;
        default_rig.add_block(boost);

        // 5. Drive 1 - first gain stage (light overdrive)
        let drive1 = PresetBlock::drive("Drive 1", 4)
            .bypassed(true)
            .with_params([
                BlockParameter::gain(0.3),
                BlockParameter::tone(0.5),
                BlockParameter::level(0.6),
            ]);
        let drive1_id = drive1.id;
        default_rig.add_block(drive1);

        // 6. Drive 2 - second gain stage (heavier distortion)
        let drive2 = PresetBlock::drive("Drive 2", 5)
            .bypassed(true)
            .with_params([
                BlockParameter::gain(0.5),
                BlockParameter::tone(0.55),
                BlockParameter::level(0.55),
            ]);
        let drive2_id = drive2.id;
        default_rig.add_block(drive2);

        // 7. Pre-Amp Delay - for rhythmic/ambient effects before amp
        let pre_delay = PresetBlock::delay("Pre Delay", 6)
            .bypassed(true)
            .with_params([
                BlockParameter::time_ms("time", "Time", 0.4, 1500.0),
                BlockParameter::new("feedback", "Feedback", 0.3).with_unit("%"),
                BlockParameter::mix(0.25),
                BlockParameter::new("sync", "Tempo Sync", 0.0),
            ]);
        let pre_delay_id = pre_delay.id;
        default_rig.add_block(pre_delay);

        // 8. Pre-Amp Reverb - small room/spring before amp
        let pre_reverb = PresetBlock::reverb("Pre Reverb", 7)
            .bypassed(true)
            .with_params([
                BlockParameter::decay(0.3),
                BlockParameter::new("predelay", "Pre-Delay", 0.1).with_range(0.0, 100.0).with_unit("ms"),
                BlockParameter::mix(0.15),
            ]);
        let pre_reverb_id = pre_reverb.id;
        default_rig.add_block(pre_reverb);

        // 9. Amp - main amplifier
        let amp = PresetBlock::amp("Amp", 8)
            .with_params([
                BlockParameter::gain(0.4),
                BlockParameter::new("bass", "Bass", 0.5).with_unit("%"),
                BlockParameter::new("mid", "Mid", 0.5).with_unit("%"),
                BlockParameter::new("treble", "Treble", 0.5).with_unit("%"),
                BlockParameter::new("presence", "Presence", 0.5).with_unit("%"),
                BlockParameter::new("master", "Master", 0.6).with_unit("%"),
            ]);
        let amp_id = amp.id;
        default_rig.add_block(amp);

        // 10. Cabinet - speaker cabinet/IR
        let cabinet = PresetBlock::cabinet("Cabinet", 9)
            .with_params([
                BlockParameter::new("mic", "Mic Position", 0.5).with_unit("%"),
                BlockParameter::new("distance", "Distance", 0.3).with_unit("%"),
                BlockParameter::new("room", "Room", 0.2).with_unit("%"),
                BlockParameter::level(0.0),
            ]);
        let cabinet_id = cabinet.id;
        default_rig.add_block(cabinet);

        // 11. Post EQ - final tone shaping
        let post_eq = PresetBlock::eq("Post EQ", 10)
            .with_params([
                BlockParameter::new("low", "Low", 0.5).with_range(-12.0, 12.0).with_unit("dB"),
                BlockParameter::new("mid", "Mid", 0.5).with_range(-12.0, 12.0).with_unit("dB"),
                BlockParameter::new("high", "High", 0.5).with_range(-12.0, 12.0).with_unit("dB"),
                BlockParameter::new("presence", "Presence", 0.5).with_range(-12.0, 12.0).with_unit("dB"),
            ]);
        let post_eq_id = post_eq.id;
        default_rig.add_block(post_eq);

        // 12. Post Reverb - main room/hall reverb
        let post_reverb = PresetBlock::reverb("Post Reverb", 11)
            .with_params([
                BlockParameter::decay(0.5),
                BlockParameter::new("predelay", "Pre-Delay", 0.15).with_range(0.0, 200.0).with_unit("ms"),
                BlockParameter::new("damping", "Damping", 0.4).with_unit("%"),
                BlockParameter::mix(0.25),
            ]);
        let post_reverb_id = post_reverb.id;
        default_rig.add_block(post_reverb);

        // 13. Post Delay - main delay for leads
        let post_delay = PresetBlock::delay("Post Delay", 12)
            .with_params([
                BlockParameter::time_ms("time", "Time", 0.5, 2000.0),
                BlockParameter::new("feedback", "Feedback", 0.35).with_unit("%"),
                BlockParameter::new("modulation", "Mod", 0.2).with_unit("%"),
                BlockParameter::mix(0.3),
            ]);
        let post_delay_id = post_delay.id;
        default_rig.add_block(post_delay);

        // Default Guitar snapshots
        let mut default_clean = Snapshot::new_with_auto_tags("Clean", &tag_registry);
        default_clean.add_block_override(BlockOverride::new(compressor_id).with_param("threshold", 0.35).with_param("ratio", 0.25));
        default_clean.add_block_override(BlockOverride::new(amp_id).with_param("gain", 0.25).with_param("master", 0.55));
        default_clean.add_block_override(BlockOverride::new(post_reverb_id).with_param("mix", 0.2).with_param("decay", 0.4));
        default_rig.add_snapshot(default_clean);

        let mut default_crunch = Snapshot::new_with_auto_tags("Crunch", &tag_registry);
        default_crunch.add_block_override(BlockOverride::new(amp_id).with_param("gain", 0.5).with_param("mid", 0.55));
        default_crunch.add_block_override(BlockOverride::new(post_reverb_id).with_param("mix", 0.2));
        default_rig.add_snapshot(default_crunch);

        let mut default_drive = Snapshot::new_with_auto_tags("Drive", &tag_registry);
        default_drive.add_block_override(BlockOverride::new(drive1_id).with_bypass(false).with_param("gain", 0.4).with_param("level", 0.6));
        default_drive.add_block_override(BlockOverride::new(amp_id).with_param("gain", 0.55).with_param("mid", 0.55));
        default_rig.add_snapshot(default_drive);

        let mut default_high_gain = Snapshot::new_with_auto_tags("High Gain", &tag_registry);
        default_high_gain.add_block_override(BlockOverride::new(drive1_id).with_bypass(false).with_param("gain", 0.5));
        default_high_gain.add_block_override(BlockOverride::new(drive2_id).with_bypass(false).with_param("gain", 0.55));
        default_high_gain.add_block_override(BlockOverride::new(amp_id).with_param("gain", 0.65).with_param("mid", 0.6));
        default_rig.add_snapshot(default_high_gain);

        let mut default_lead = Snapshot::new_with_auto_tags("Lead", &tag_registry);
        default_lead.add_block_override(BlockOverride::new(boost_id).with_bypass(false).with_param("boost", 0.6));
        default_lead.add_block_override(BlockOverride::new(drive1_id).with_bypass(false).with_param("gain", 0.45).with_param("level", 0.65));
        default_lead.add_block_override(BlockOverride::new(amp_id).with_param("gain", 0.55).with_param("mid", 0.6).with_param("presence", 0.55));
        default_lead.add_block_override(BlockOverride::new(post_delay_id).with_param("mix", 0.35).with_param("feedback", 0.4));
        default_lead.add_block_override(BlockOverride::new(post_reverb_id).with_param("mix", 0.3));
        default_rig.add_snapshot(default_lead);

        let mut default_solo = Snapshot::new_with_auto_tags("Solo", &tag_registry);
        default_solo.add_block_override(BlockOverride::new(boost_id).with_bypass(false).with_param("boost", 0.7));
        default_solo.add_block_override(BlockOverride::new(drive1_id).with_bypass(false).with_param("gain", 0.5).with_param("level", 0.7));
        default_solo.add_block_override(BlockOverride::new(drive2_id).with_bypass(false).with_param("gain", 0.4));
        default_solo.add_block_override(BlockOverride::new(amp_id).with_param("gain", 0.6).with_param("mid", 0.65).with_param("presence", 0.6));
        default_solo.add_block_override(BlockOverride::new(post_delay_id).with_param("mix", 0.4).with_param("feedback", 0.45));
        default_solo.add_block_override(BlockOverride::new(post_reverb_id).with_param("mix", 0.35).with_param("decay", 0.6));
        default_rig.add_snapshot(default_solo);

        let default_rig_id = default_rig.id;
        let default_clean_id = default_rig.snapshots[0].id;
        let default_crunch_id = default_rig.snapshots[1].id;
        let default_drive_id = default_rig.snapshots[2].id;
        let _default_high_gain_id = default_rig.snapshots[3].id;
        let default_lead_id = default_rig.snapshots[4].id;
        let _default_solo_id = default_rig.snapshots[5].id;
        profile.add_preset(default_rig);

        // Helper to find tag ID by name
        let find_tag = |name: &str| -> Uuid {
            tag_registry
                .find_by_name(name)
                .map(|t| t.id)
                .unwrap_or_else(Uuid::new_v4)
        };

        // Common tag IDs
        let tag_clean = find_tag("Clean");
        let tag_crunch = find_tag("Crunch");
        let tag_drive = find_tag("Drive");
        let tag_lead = find_tag("Lead");
        let tag_blues = find_tag("Blues");
        let tag_rock = find_tag("Rock");
        let tag_metal = find_tag("Metal");
        let tag_worship = find_tag("Worship");
        let tag_jazz = find_tag("Jazz");
        let tag_country = find_tag("Country");
        let tag_warm = find_tag("Warm");
        let tag_bright = find_tag("Bright");
        let tag_aggressive = find_tag("Aggressive");
        let tag_smooth = find_tag("Smooth");
        let tag_vintage = find_tag("Vintage");
        let tag_modern = find_tag("Modern");

        // ========== WORSHIP PRESET ==========
        // Ambient worship tones with lots of delay/reverb

        let mut worship = Preset::new("Worship", PresetCategory::genre(BaseTone::Clean, Genre::Worship));
        worship.tags = Tags::from_ids([tag_worship, tag_warm, tag_smooth]);
        worship.rating = 5;
        worship.add_section_config(SectionConfig::new(amp_section_id, true));
        worship.add_section_config(SectionConfig::new(effects_section_id, true));
        worship.add_patch_assignment(PatchAssignment::new(amp_section_id, 0, clean_id));
        worship.add_global_block_config(GlobalBlockConfig::new(reverb_id, true));
        worship.add_global_block_config(GlobalBlockConfig::new(delay_id, true));

        // Worship-specific blocks: Compressor -> Light Drive -> Amp -> Mod -> Delay -> Reverb
        let worship_comp = PresetBlock::compressor("Comp", 0)
            .with_params([
                BlockParameter::new("threshold", "Threshold", 0.4).with_range(-40.0, 0.0).with_unit("dB"),
                BlockParameter::new("ratio", "Ratio", 0.3).with_range(1.0, 20.0).with_unit(":1"),
                BlockParameter::new("attack", "Attack", 0.2).with_range(0.1, 100.0).with_unit("ms"),
                BlockParameter::new("release", "Release", 0.5).with_range(10.0, 1000.0).with_unit("ms"),
            ]);
        let worship_comp_id = worship_comp.id;
        worship.add_block(worship_comp);

        let worship_drive = PresetBlock::drive("Klon", 1)
            .with_params([
                BlockParameter::gain(0.2),
                BlockParameter::tone(0.5),
                BlockParameter::level(0.6),
            ]);
        let worship_drive_id = worship_drive.id;
        worship.add_block(worship_drive);

        let worship_amp = PresetBlock::amp("Clean Amp", 2)
            .with_params([
                BlockParameter::gain(0.3),
                BlockParameter::new("bass", "Bass", 0.5).with_unit("%"),
                BlockParameter::new("mid", "Mid", 0.5).with_unit("%"),
                BlockParameter::new("treble", "Treble", 0.6).with_unit("%"),
                BlockParameter::new("presence", "Presence", 0.4).with_unit("%"),
                BlockParameter::level(0.7),
            ]);
        let worship_amp_id = worship_amp.id;
        worship.add_block(worship_amp);

        let worship_mod = PresetBlock::modulation("Chorus", 3)
            .with_params([
                BlockParameter::new("rate", "Rate", 0.3).with_range(0.1, 10.0).with_unit("Hz"),
                BlockParameter::new("depth", "Depth", 0.4).with_unit("%"),
                BlockParameter::mix(0.3),
            ]);
        let worship_mod_id = worship_mod.id;
        worship.add_block(worship_mod);

        let worship_delay = PresetBlock::delay("Dotted 8th", 4)
            .with_params([
                BlockParameter::time_ms("time", "Time", 0.5, 2000.0),
                BlockParameter::new("feedback", "Feedback", 0.4).with_unit("%"),
                BlockParameter::mix(0.35),
            ]);
        let worship_delay_id = worship_delay.id;
        worship.add_block(worship_delay);

        let worship_reverb = PresetBlock::reverb("Hall", 5)
            .with_params([
                BlockParameter::decay(0.7),
                BlockParameter::new("predelay", "Pre-Delay", 0.2).with_range(0.0, 200.0).with_unit("ms"),
                BlockParameter::mix(0.45),
            ]);
        let worship_reverb_id = worship_reverb.id;
        worship.add_block(worship_reverb);

        // Snapshots with different block parameter values
        let mut clean_snapshot = Snapshot::new_with_auto_tags("Clean", &tag_registry);
        clean_snapshot.add_variation_assignment(super::core::PatchVariationAssignment::new(
            amp_section_id, 0, Some(clean_soft_id),
        ));
        // Clean: low drive, medium reverb
        clean_snapshot.add_block_override(BlockOverride::new(worship_drive_id).with_bypass(true));
        clean_snapshot.add_block_override(BlockOverride::new(worship_reverb_id).with_param("mix", 0.35));
        worship.add_snapshot(clean_snapshot);

        let mut shimmer_snapshot = Snapshot::new_with_auto_tags("Shimmer", &tag_registry);
        shimmer_snapshot.add_variation_assignment(super::core::PatchVariationAssignment::new(
            amp_section_id, 0, Some(clean_normal_id),
        ));
        // Shimmer: more reverb, more delay, chorus on
        shimmer_snapshot.add_block_override(BlockOverride::new(worship_drive_id).with_bypass(true));
        shimmer_snapshot.add_block_override(BlockOverride::new(worship_mod_id).with_param("mix", 0.5));
        shimmer_snapshot.add_block_override(BlockOverride::new(worship_delay_id).with_param("mix", 0.45).with_param("feedback", 0.5));
        shimmer_snapshot.add_block_override(BlockOverride::new(worship_reverb_id).with_param("mix", 0.6).with_param("decay", 0.85));
        worship.add_snapshot(shimmer_snapshot);

        let mut crunch_snapshot = Snapshot::new_with_auto_tags("Crunch", &tag_registry);
        crunch_snapshot.add_variation_assignment(super::core::PatchVariationAssignment::new(
            amp_section_id, 0, Some(clean_normal_id),
        ));
        // Crunch: drive on with low gain
        crunch_snapshot.add_block_override(BlockOverride::new(worship_drive_id).with_param("gain", 0.35).with_param("level", 0.65));
        crunch_snapshot.add_block_override(BlockOverride::new(worship_amp_id).with_param("gain", 0.45));
        worship.add_snapshot(crunch_snapshot);

        let mut lead_snapshot = Snapshot::new_with_auto_tags("Lead", &tag_registry);
        lead_snapshot.add_variation_assignment(super::core::PatchVariationAssignment::new(
            amp_section_id, 0, Some(drive_normal_id),
        ));
        // Lead: drive on higher, boost delay
        lead_snapshot.add_block_override(BlockOverride::new(worship_drive_id).with_param("gain", 0.5).with_param("level", 0.7));
        lead_snapshot.add_block_override(BlockOverride::new(worship_amp_id).with_param("gain", 0.55).with_param("mid", 0.6));
        lead_snapshot.add_block_override(BlockOverride::new(worship_delay_id).with_param("mix", 0.4));
        worship.add_snapshot(lead_snapshot);

        let worship_id = worship.id;
        let worship_clean_id = worship.snapshots[0].id;
        let worship_shimmer_id = worship.snapshots[1].id;
        profile.add_preset(worship);

        // ========== BLUES PRESET ==========
        // Classic blues tones: Tube Screamer -> Fender-style amp -> spring reverb

        let mut blues = Preset::new("Blues", PresetCategory::genre(BaseTone::Crunch, Genre::Blues));
        blues.tags = Tags::from_ids([tag_blues, tag_warm, tag_vintage]);
        blues.rating = 5;
        blues.add_section_config(SectionConfig::new(amp_section_id, true));
        blues.add_section_config(SectionConfig::new(effects_section_id, true));
        blues.add_patch_assignment(PatchAssignment::new(amp_section_id, 0, crunch_id));
        blues.add_global_block_config(GlobalBlockConfig::new(reverb_id, true));
        blues.add_global_block_config(GlobalBlockConfig::new(delay_id, false));

        // Blues blocks: Wah -> TS-style Drive -> Fender Amp -> Spring Reverb
        let blues_wah = PresetBlock::new("Wah", BlockType::Custom, 0)
            .bypassed(true)
            .with_params([
                BlockParameter::new("position", "Position", 0.5).with_unit("%"),
                BlockParameter::new("range", "Range", 0.7).with_unit("%"),
            ]);
        let blues_wah_id = blues_wah.id;
        blues.add_block(blues_wah);

        let blues_ts = PresetBlock::drive("Tube Screamer", 1)
            .with_params([
                BlockParameter::new("drive", "Drive", 0.4).with_unit("%"),
                BlockParameter::tone(0.55),
                BlockParameter::level(0.6),
            ]);
        let blues_ts_id = blues_ts.id;
        blues.add_block(blues_ts);

        let blues_amp = PresetBlock::amp("Deluxe Reverb", 2)
            .with_params([
                BlockParameter::new("volume", "Volume", 0.5).with_unit("%"),
                BlockParameter::new("bass", "Bass", 0.4).with_unit("%"),
                BlockParameter::new("treble", "Treble", 0.6).with_unit("%"),
                BlockParameter::new("reverb", "Reverb", 0.4).with_unit("%"),
                BlockParameter::new("vibrato", "Vibrato", 0.0).with_unit("%"),
            ]);
        let blues_amp_id = blues_amp.id;
        blues.add_block(blues_amp);

        let blues_reverb = PresetBlock::reverb("Spring", 3)
            .with_params([
                BlockParameter::decay(0.5),
                BlockParameter::new("dwell", "Dwell", 0.4).with_unit("%"),
                BlockParameter::mix(0.25),
            ]);
        let blues_reverb_id = blues_reverb.id;
        blues.add_block(blues_reverb);

        // Blues snapshots with overrides
        let mut blues_clean = Snapshot::new_with_auto_tags("Clean", &tag_registry);
        blues_clean.add_block_override(BlockOverride::new(blues_ts_id).with_bypass(true));
        blues_clean.add_block_override(BlockOverride::new(blues_amp_id).with_param("volume", 0.35));
        blues.add_snapshot(blues_clean);

        let mut blues_crunch = Snapshot::new_with_auto_tags("Crunch", &tag_registry);
        blues_crunch.add_block_override(BlockOverride::new(blues_ts_id).with_param("drive", 0.35).with_param("level", 0.55));
        blues_crunch.add_block_override(BlockOverride::new(blues_amp_id).with_param("volume", 0.55));
        blues.add_snapshot(blues_crunch);

        let mut blues_lead = Snapshot::new_with_auto_tags("Lead", &tag_registry);
        blues_lead.add_block_override(BlockOverride::new(blues_ts_id).with_param("drive", 0.5).with_param("level", 0.65));
        blues_lead.add_block_override(BlockOverride::new(blues_amp_id).with_param("volume", 0.6).with_param("treble", 0.65));
        blues_lead.add_block_override(BlockOverride::new(blues_reverb_id).with_param("mix", 0.3));
        blues.add_snapshot(blues_lead);

        let mut blues_solo = Snapshot::new_with_auto_tags("Solo", &tag_registry);
        blues_solo.add_block_override(BlockOverride::new(blues_ts_id).with_param("drive", 0.6).with_param("level", 0.75));
        blues_solo.add_block_override(BlockOverride::new(blues_amp_id).with_param("volume", 0.7).with_param("bass", 0.5).with_param("treble", 0.7));
        blues_solo.add_block_override(BlockOverride::new(blues_reverb_id).with_param("mix", 0.35));
        blues.add_snapshot(blues_solo);

        let blues_id = blues.id;
        let blues_clean_id = blues.snapshots[0].id;
        let blues_crunch_id = blues.snapshots[1].id;
        let blues_lead_id = blues.snapshots[2].id;
        let blues_solo_id = blues.snapshots[3].id;
        profile.add_preset(blues);

        // ========== ARCHETYPE PRESETS (Artist-specific) ==========

        // John Mayer preset
        let mut john_mayer = Preset::new("John Mayer", PresetCategory::genre(BaseTone::Clean, Genre::Blues));
        john_mayer.tags = Tags::from_ids([tag_blues, tag_warm, tag_smooth, tag_vintage]);
        john_mayer.rating = 5;
        john_mayer.notes = Some("John Mayer signature tones - Gravity, Slow Dancing, etc.".to_string());
        john_mayer.add_snapshot(Snapshot::new_with_auto_tags("Clean", &tag_registry));
        john_mayer.add_snapshot(Snapshot::new_with_auto_tags("Gravity", &tag_registry));
        john_mayer.add_snapshot(Snapshot::new_with_auto_tags("Slow Dancing", &tag_registry));
        john_mayer.add_snapshot(Snapshot::new_with_auto_tags("Lead", &tag_registry));
        profile.add_preset(john_mayer);

        // SRV preset
        let mut srv = Preset::new("SRV", PresetCategory::genre(BaseTone::Crunch, Genre::Blues));
        srv.tags = Tags::from_ids([tag_blues, tag_aggressive, tag_vintage]);
        srv.rating = 5;
        srv.notes = Some("Stevie Ray Vaughan Texas blues tones".to_string());
        srv.add_snapshot(Snapshot::new_with_auto_tags("Clean", &tag_registry));
        srv.add_snapshot(Snapshot::new_with_auto_tags("Texas Flood", &tag_registry));
        srv.add_snapshot(Snapshot::new_with_auto_tags("Pride & Joy", &tag_registry));
        srv.add_snapshot(Snapshot::new_with_auto_tags("Scuttle Buttin", &tag_registry));
        profile.add_preset(srv);

        // BB King preset
        let mut bb_king = Preset::new("BB King", PresetCategory::genre(BaseTone::Clean, Genre::Blues));
        bb_king.tags = Tags::from_ids([tag_blues, tag_warm, tag_smooth, tag_vintage]);
        bb_king.rating = 4;
        bb_king.add_snapshot(Snapshot::new_with_auto_tags("Sweet", &tag_registry));
        bb_king.add_snapshot(Snapshot::new_with_auto_tags("Lucille", &tag_registry));
        bb_king.add_snapshot(Snapshot::new_with_auto_tags("Solo", &tag_registry));
        profile.add_preset(bb_king);

        // ========== ROCK PRESET ==========
        // Marshall-style rock: OD -> Marshall Plexi -> Cab -> Slapback Delay

        let mut rock = Preset::new("Rock", PresetCategory::genre(BaseTone::Drive, Genre::Rock));
        rock.tags = Tags::from_ids([tag_rock, tag_aggressive]);
        rock.rating = 5;
        rock.add_section_config(SectionConfig::new(amp_section_id, true));
        rock.add_section_config(SectionConfig::new(effects_section_id, true));
        rock.add_patch_assignment(PatchAssignment::new(amp_section_id, 0, drive_id));
        rock.add_global_block_config(GlobalBlockConfig::new(reverb_id, false));
        rock.add_global_block_config(GlobalBlockConfig::new(delay_id, true));

        // Rock blocks: Gate -> OD808 -> Marshall -> Cab -> Delay
        let rock_gate = PresetBlock::new("Gate", BlockType::Gate, 0)
            .with_params([
                BlockParameter::new("threshold", "Threshold", 0.3).with_range(-80.0, 0.0).with_unit("dB"),
                BlockParameter::new("release", "Release", 0.4).with_range(10.0, 500.0).with_unit("ms"),
            ]);
        let rock_gate_id = rock_gate.id;
        rock.add_block(rock_gate);

        let rock_od = PresetBlock::drive("OD808", 1)
            .with_params([
                BlockParameter::new("overdrive", "Overdrive", 0.5).with_unit("%"),
                BlockParameter::tone(0.55),
                BlockParameter::level(0.65),
            ]);
        let rock_od_id = rock_od.id;
        rock.add_block(rock_od);

        let rock_amp = PresetBlock::amp("Plexi 100W", 2)
            .with_params([
                BlockParameter::new("presence", "Presence", 0.5).with_unit("%"),
                BlockParameter::new("bass", "Bass", 0.45).with_unit("%"),
                BlockParameter::new("mid", "Middle", 0.6).with_unit("%"),
                BlockParameter::new("treble", "Treble", 0.55).with_unit("%"),
                BlockParameter::new("master", "Master", 0.6).with_unit("%"),
                BlockParameter::gain(0.55),
            ]);
        let rock_amp_id = rock_amp.id;
        rock.add_block(rock_amp);

        let rock_cab = PresetBlock::cabinet("4x12 Greenback", 3)
            .with_params([
                BlockParameter::new("mic", "Mic Position", 0.5).with_unit("%"),
                BlockParameter::new("distance", "Distance", 0.3).with_unit("%"),
                BlockParameter::level(0.0),
            ]);
        let _rock_cab_id = rock_cab.id;
        rock.add_block(rock_cab);

        let rock_delay = PresetBlock::delay("Slapback", 4)
            .with_params([
                BlockParameter::time_ms("time", "Time", 0.15, 500.0),
                BlockParameter::new("feedback", "Feedback", 0.2).with_unit("%"),
                BlockParameter::mix(0.2),
            ]);
        let rock_delay_id = rock_delay.id;
        rock.add_block(rock_delay);

        // Rock snapshots
        let mut rock_clean = Snapshot::new_with_auto_tags("Clean", &tag_registry);
        rock_clean.add_block_override(BlockOverride::new(rock_od_id).with_bypass(true));
        rock_clean.add_block_override(BlockOverride::new(rock_amp_id).with_param("gain", 0.25).with_param("master", 0.5));
        rock.add_snapshot(rock_clean);

        let mut rock_crunch = Snapshot::new_with_auto_tags("Crunch", &tag_registry);
        rock_crunch.add_block_override(BlockOverride::new(rock_od_id).with_bypass(true));
        rock_crunch.add_block_override(BlockOverride::new(rock_amp_id).with_param("gain", 0.5).with_param("master", 0.55));
        rock.add_snapshot(rock_crunch);

        let mut rock_drive = Snapshot::new_with_auto_tags("Drive", &tag_registry);
        rock_drive.add_block_override(BlockOverride::new(rock_od_id).with_param("overdrive", 0.4).with_param("level", 0.6));
        rock_drive.add_block_override(BlockOverride::new(rock_amp_id).with_param("gain", 0.6));
        rock.add_snapshot(rock_drive);

        let mut rock_lead = Snapshot::new_with_auto_tags("Lead", &tag_registry);
        rock_lead.add_block_override(BlockOverride::new(rock_od_id).with_param("overdrive", 0.55).with_param("level", 0.7));
        rock_lead.add_block_override(BlockOverride::new(rock_amp_id).with_param("gain", 0.65).with_param("mid", 0.65));
        rock_lead.add_block_override(BlockOverride::new(rock_delay_id).with_param("mix", 0.25));
        rock.add_snapshot(rock_lead);

        let mut rock_solo = Snapshot::new_with_auto_tags("Solo", &tag_registry);
        rock_solo.add_block_override(BlockOverride::new(rock_od_id).with_param("overdrive", 0.65).with_param("level", 0.8));
        rock_solo.add_block_override(BlockOverride::new(rock_amp_id).with_param("gain", 0.7).with_param("mid", 0.7).with_param("treble", 0.6));
        rock_solo.add_block_override(BlockOverride::new(rock_delay_id).with_param("mix", 0.3).with_param("feedback", 0.3));
        rock.add_snapshot(rock_solo);

        let rock_id = rock.id;
        let rock_clean_id = rock.snapshots[0].id;
        let rock_crunch_id = rock.snapshots[1].id;
        let rock_drive_id = rock.snapshots[2].id;
        let rock_lead_id = rock.snapshots[3].id;
        let rock_solo_id = rock.snapshots[4].id;
        profile.add_preset(rock);

        // Classic Rock 70s preset
        let mut classic_rock = Preset::new("Classic Rock 70s", PresetCategory::genre(BaseTone::Drive, Genre::Rock));
        classic_rock.tags = Tags::from_ids([tag_rock, tag_warm, tag_vintage]);
        classic_rock.rating = 4;
        classic_rock.add_snapshot(Snapshot::new_with_auto_tags("Rhythm", &tag_registry));
        classic_rock.add_snapshot(Snapshot::new_with_auto_tags("Power Chords", &tag_registry));
        classic_rock.add_snapshot(Snapshot::new_with_auto_tags("Solo", &tag_registry));
        profile.add_preset(classic_rock);

        // Modern Rock preset
        let mut modern_rock = Preset::new("Modern Rock", PresetCategory::genre(BaseTone::Drive, Genre::Rock));
        modern_rock.tags = Tags::from_ids([tag_rock, tag_aggressive, tag_modern]);
        modern_rock.rating = 4;
        modern_rock.add_snapshot(Snapshot::new_with_auto_tags("Clean", &tag_registry));
        modern_rock.add_snapshot(Snapshot::new_with_auto_tags("Crunch", &tag_registry));
        modern_rock.add_snapshot(Snapshot::new_with_auto_tags("Heavy", &tag_registry));
        modern_rock.add_snapshot(Snapshot::new_with_auto_tags("Lead", &tag_registry));
        profile.add_preset(modern_rock);

        // ========== METAL PRESET ==========
        // High-gain modern metal: Gate -> Boost -> 5150 -> Cab -> EQ

        let mut metal = Preset::new("Metal", PresetCategory::genre(BaseTone::Drive, Genre::Metal));
        metal.tags = Tags::from_ids([tag_metal, tag_aggressive, tag_modern]);
        metal.rating = 5;
        metal.add_section_config(SectionConfig::new(amp_section_id, true));
        metal.add_patch_assignment(PatchAssignment::new(amp_section_id, 0, drive_id));
        metal.add_global_block_config(GlobalBlockConfig::new(reverb_id, false));
        metal.add_global_block_config(GlobalBlockConfig::new(delay_id, false));

        // Metal blocks: Gate -> Boost -> High-Gain Amp -> Cab -> EQ
        let metal_gate = PresetBlock::new("Noise Gate", BlockType::Gate, 0)
            .with_params([
                BlockParameter::new("threshold", "Threshold", 0.5).with_range(-80.0, 0.0).with_unit("dB"),
                BlockParameter::new("release", "Release", 0.3).with_range(5.0, 200.0).with_unit("ms"),
            ]);
        let metal_gate_id = metal_gate.id;
        metal.add_block(metal_gate);

        let metal_boost = PresetBlock::drive("Tight Boost", 1)
            .with_params([
                BlockParameter::new("boost", "Boost", 0.6).with_range(0.0, 20.0).with_unit("dB"),
                BlockParameter::new("tight", "Tight", 0.7).with_unit("%"),
            ]);
        let metal_boost_id = metal_boost.id;
        metal.add_block(metal_boost);

        let metal_amp = PresetBlock::amp("5150 III", 2)
            .with_params([
                BlockParameter::gain(0.75),
                BlockParameter::new("low", "Low", 0.5).with_unit("%"),
                BlockParameter::new("mid", "Mid", 0.55).with_unit("%"),
                BlockParameter::new("high", "High", 0.6).with_unit("%"),
                BlockParameter::new("presence", "Presence", 0.55).with_unit("%"),
                BlockParameter::new("resonance", "Resonance", 0.45).with_unit("%"),
                BlockParameter::new("master", "Master", 0.5).with_unit("%"),
            ]);
        let metal_amp_id = metal_amp.id;
        metal.add_block(metal_amp);

        let metal_cab = PresetBlock::cabinet("Mesa 4x12 V30", 3)
            .with_params([
                BlockParameter::new("mic", "Mic", 0.6).with_unit("%"),
                BlockParameter::new("room", "Room", 0.15).with_unit("%"),
            ]);
        let _metal_cab_id = metal_cab.id;
        metal.add_block(metal_cab);

        let metal_eq = PresetBlock::eq("Graphic EQ", 4)
            .with_params([
                BlockParameter::new("100hz", "100Hz", 0.45).with_range(-12.0, 12.0).with_unit("dB"),
                BlockParameter::new("400hz", "400Hz", 0.4).with_range(-12.0, 12.0).with_unit("dB"),
                BlockParameter::new("800hz", "800Hz", 0.55).with_range(-12.0, 12.0).with_unit("dB"),
                BlockParameter::new("2khz", "2kHz", 0.55).with_range(-12.0, 12.0).with_unit("dB"),
                BlockParameter::new("4khz", "4kHz", 0.5).with_range(-12.0, 12.0).with_unit("dB"),
            ]);
        let metal_eq_id = metal_eq.id;
        metal.add_block(metal_eq);

        // Metal snapshots
        let mut metal_clean = Snapshot::new_with_auto_tags("Clean", &tag_registry);
        metal_clean.add_block_override(BlockOverride::new(metal_boost_id).with_bypass(true));
        metal_clean.add_block_override(BlockOverride::new(metal_amp_id).with_param("gain", 0.2).with_param("master", 0.4));
        metal.add_snapshot(metal_clean);

        let mut metal_rhythm = Snapshot::new_with_auto_tags("Rhythm", &tag_registry);
        metal_rhythm.add_block_override(BlockOverride::new(metal_boost_id).with_param("boost", 0.5).with_param("tight", 0.75));
        metal_rhythm.add_block_override(BlockOverride::new(metal_amp_id).with_param("gain", 0.7).with_param("mid", 0.5));
        metal.add_snapshot(metal_rhythm);

        let mut metal_chug = Snapshot::new_with_auto_tags("Chug", &tag_registry);
        metal_chug.add_block_override(BlockOverride::new(metal_gate_id).with_param("threshold", 0.6).with_param("release", 0.2));
        metal_chug.add_block_override(BlockOverride::new(metal_boost_id).with_param("boost", 0.65).with_param("tight", 0.85));
        metal_chug.add_block_override(BlockOverride::new(metal_amp_id).with_param("gain", 0.75).with_param("low", 0.55).with_param("mid", 0.45));
        metal.add_snapshot(metal_chug);

        let mut metal_lead = Snapshot::new_with_auto_tags("Lead", &tag_registry);
        metal_lead.add_block_override(BlockOverride::new(metal_boost_id).with_param("boost", 0.55).with_param("tight", 0.6));
        metal_lead.add_block_override(BlockOverride::new(metal_amp_id).with_param("gain", 0.72).with_param("mid", 0.6).with_param("high", 0.65));
        metal_lead.add_block_override(BlockOverride::new(metal_eq_id).with_param("2khz", 0.6));
        metal.add_snapshot(metal_lead);

        let mut metal_solo = Snapshot::new_with_auto_tags("Solo", &tag_registry);
        metal_solo.add_block_override(BlockOverride::new(metal_boost_id).with_param("boost", 0.7).with_param("tight", 0.55));
        metal_solo.add_block_override(BlockOverride::new(metal_amp_id).with_param("gain", 0.78).with_param("mid", 0.65).with_param("master", 0.55));
        metal_solo.add_block_override(BlockOverride::new(metal_eq_id).with_param("2khz", 0.65).with_param("4khz", 0.55));
        metal.add_snapshot(metal_solo);

        profile.add_preset(metal);

        // Thrash preset
        let mut thrash = Preset::new("Thrash", PresetCategory::genre(BaseTone::Drive, Genre::Metal));
        thrash.tags = Tags::from_ids([tag_metal, tag_aggressive, tag_modern]);
        thrash.rating = 4;
        thrash.add_snapshot(Snapshot::new_with_auto_tags("Rhythm", &tag_registry));
        thrash.add_snapshot(Snapshot::new_with_auto_tags("Fast Picking", &tag_registry));
        thrash.add_snapshot(Snapshot::new_with_auto_tags("Lead", &tag_registry));
        profile.add_preset(thrash);

        // Djent preset
        let mut djent = Preset::new("Djent", PresetCategory::genre(BaseTone::Drive, Genre::Metal));
        djent.tags = Tags::from_ids([tag_metal, tag_aggressive, tag_modern, tag_bright]);
        djent.rating = 4;
        djent.notes = Some("8-string djent tone with tight low end".to_string());
        djent.add_snapshot(Snapshot::new_with_auto_tags("Rhythm", &tag_registry));
        djent.add_snapshot(Snapshot::new_with_auto_tags("Polyrhythm", &tag_registry));
        djent.add_snapshot(Snapshot::new_with_auto_tags("Clean Ambient", &tag_registry));
        djent.add_snapshot(Snapshot::new_with_auto_tags("Lead", &tag_registry));
        profile.add_preset(djent);

        // ========== JAZZ PRESET ==========

        let mut jazz = Preset::new("Jazz", PresetCategory::genre(BaseTone::Clean, Genre::Jazz));
        jazz.tags = Tags::from_ids([tag_jazz, tag_warm, tag_smooth]);
        jazz.rating = 5;
        jazz.add_snapshot(Snapshot::new_with_auto_tags("Clean", &tag_registry));
        jazz.add_snapshot(Snapshot::new_with_auto_tags("Warm", &tag_registry));
        jazz.add_snapshot(Snapshot::new_with_auto_tags("Comp", &tag_registry));
        jazz.add_snapshot(Snapshot::new_with_auto_tags("Solo", &tag_registry));
        profile.add_preset(jazz);

        // Fusion preset
        let mut fusion = Preset::new("Fusion", PresetCategory::genre(BaseTone::Lead, Genre::Jazz));
        fusion.tags = Tags::from_ids([tag_jazz, tag_smooth, tag_warm]);
        fusion.rating = 4;
        fusion.add_snapshot(Snapshot::new_with_auto_tags("Rhythm", &tag_registry));
        fusion.add_snapshot(Snapshot::new_with_auto_tags("Legato", &tag_registry));
        fusion.add_snapshot(Snapshot::new_with_auto_tags("Solo", &tag_registry));
        profile.add_preset(fusion);

        // ========== COUNTRY PRESET ==========

        let mut country = Preset::new("Country", PresetCategory::genre(BaseTone::Clean, Genre::Country));
        country.tags = Tags::from_ids([tag_country, tag_bright, tag_vintage]);
        country.rating = 5;
        country.add_snapshot(Snapshot::new_with_auto_tags("Clean", &tag_registry));
        country.add_snapshot(Snapshot::new_with_auto_tags("Twang", &tag_registry));
        country.add_snapshot(Snapshot::new_with_auto_tags("Chicken Pickin", &tag_registry));
        country.add_snapshot(Snapshot::new_with_auto_tags("Crunch", &tag_registry));
        country.add_snapshot(Snapshot::new_with_auto_tags("Lead", &tag_registry));
        profile.add_preset(country);

        // Outlaw Country preset
        let mut outlaw = Preset::new("Outlaw Country", PresetCategory::genre(BaseTone::Crunch, Genre::Country));
        outlaw.tags = Tags::from_ids([tag_country, tag_warm, tag_vintage]);
        outlaw.rating = 4;
        outlaw.add_snapshot(Snapshot::new_with_auto_tags("Rhythm", &tag_registry));
        outlaw.add_snapshot(Snapshot::new_with_auto_tags("Drive", &tag_registry));
        outlaw.add_snapshot(Snapshot::new_with_auto_tags("Solo", &tag_registry));
        profile.add_preset(outlaw);

        // ========== SONGS ==========

        // Song 1: Cryin' - Mateus Asato (Blues ballad with emotional builds)
        let mut cryin = PerformanceSong::new("Cryin' - Mateus Asato");
        cryin.add_scene(Scene::new("Intro", blues_id).with_snapshot(blues_clean_id));
        cryin.add_scene(Scene::new("Verse 1", blues_id).with_snapshot(blues_clean_id));
        cryin.add_scene(Scene::new("Chorus 1", blues_id).with_snapshot(blues_crunch_id));
        cryin.add_scene(Scene::new("Verse 2", blues_id).with_snapshot(blues_crunch_id));
        cryin.add_scene(Scene::new("Chorus 2", blues_id).with_snapshot(blues_lead_id));
        cryin.add_scene(Scene::new("Solo", blues_id).with_snapshot(blues_solo_id));
        cryin.add_scene(Scene::new("Outro", blues_id).with_snapshot(blues_clean_id));

        // Song 2: Thriller - Dirty Loops (Funky, dynamic)
        let mut thriller = PerformanceSong::new("Thriller - Dirty Loops");
        thriller.add_scene(Scene::new("Intro", default_rig_id).with_snapshot(default_clean_id));
        thriller.add_scene(Scene::new("Verse 1", default_rig_id).with_snapshot(default_crunch_id));
        thriller.add_scene(Scene::new("Pre-Chorus", default_rig_id).with_snapshot(default_drive_id));
        thriller.add_scene(Scene::new("Chorus", rock_id).with_snapshot(rock_drive_id));
        thriller.add_scene(Scene::new("Verse 2", default_rig_id).with_snapshot(default_crunch_id));
        thriller.add_scene(Scene::new("Bridge", worship_id).with_snapshot(worship_shimmer_id));
        thriller.add_scene(Scene::new("Solo", default_rig_id).with_snapshot(default_lead_id));
        thriller.add_scene(Scene::new("Outro", rock_id).with_snapshot(rock_drive_id));

        // Song 3: Movin' Out - Sammy Rae & The Friends (Energetic rock/soul)
        let mut movin_out = PerformanceSong::new("Movin' Out - Sammy Rae & The Friends");
        movin_out.add_scene(Scene::new("Intro", rock_id).with_snapshot(rock_clean_id));
        movin_out.add_scene(Scene::new("Verse 1", rock_id).with_snapshot(rock_crunch_id));
        movin_out.add_scene(Scene::new("Chorus 1", rock_id).with_snapshot(rock_drive_id));
        movin_out.add_scene(Scene::new("Verse 2", rock_id).with_snapshot(rock_crunch_id));
        movin_out.add_scene(Scene::new("Chorus 2", rock_id).with_snapshot(rock_drive_id));
        movin_out.add_scene(Scene::new("Bridge", default_rig_id).with_snapshot(default_lead_id));
        movin_out.add_scene(Scene::new("Solo", rock_id).with_snapshot(rock_solo_id));
        movin_out.add_scene(Scene::new("Outro", rock_id).with_snapshot(rock_drive_id));

        // Song 4: Bennie and the Jets - Elton John (Classic rock, piano-driven)
        let mut bennie = PerformanceSong::new("Bennie and the Jets - Elton John");
        bennie.add_scene(Scene::new("Intro", default_rig_id).with_snapshot(default_clean_id));
        bennie.add_scene(Scene::new("Verse 1", default_rig_id).with_snapshot(default_clean_id));
        bennie.add_scene(Scene::new("Chorus 1", default_rig_id).with_snapshot(default_crunch_id));
        bennie.add_scene(Scene::new("Verse 2", default_rig_id).with_snapshot(default_crunch_id));
        bennie.add_scene(Scene::new("Chorus 2", rock_id).with_snapshot(rock_drive_id));
        bennie.add_scene(Scene::new("Solo", rock_id).with_snapshot(rock_lead_id));
        bennie.add_scene(Scene::new("Outro", default_rig_id).with_snapshot(default_crunch_id));

        // Song 5: Girl Goodbye - Toto (Classic rock)
        let mut girl_goodbye = PerformanceSong::new("Girl Goodbye - Toto");
        girl_goodbye.add_scene(Scene::new("Intro", rock_id).with_snapshot(rock_clean_id));
        girl_goodbye.add_scene(Scene::new("Verse 1", rock_id).with_snapshot(rock_crunch_id));
        girl_goodbye.add_scene(Scene::new("Pre-Chorus", rock_id).with_snapshot(rock_drive_id));
        girl_goodbye.add_scene(Scene::new("Chorus", rock_id).with_snapshot(rock_drive_id));
        girl_goodbye.add_scene(Scene::new("Verse 2", rock_id).with_snapshot(rock_crunch_id));
        girl_goodbye.add_scene(Scene::new("Bridge", default_rig_id).with_snapshot(default_lead_id));
        girl_goodbye.add_scene(Scene::new("Guitar Solo", rock_id).with_snapshot(rock_solo_id));
        girl_goodbye.add_scene(Scene::new("Outro", rock_id).with_snapshot(rock_drive_id));

        // ========== SETLISTS ==========

        // Main setlist: Just Friends Spring 2026
        let mut main_setlist = PerformanceSetlist::new("Just Friends Spring 2026");
        main_setlist.add_song(cryin.clone());
        main_setlist.add_song(thriller.clone());
        main_setlist.add_song(movin_out.clone());
        main_setlist.add_song(bennie.clone());
        main_setlist.add_song(girl_goodbye.clone());

        // All songs setlist (for browsing all available songs)
        let mut all_setlist = PerformanceSetlist::new("All");
        all_setlist.add_song(cryin);
        all_setlist.add_song(thriller);
        all_setlist.add_song(movin_out);
        all_setlist.add_song(bennie);
        all_setlist.add_song(girl_goodbye);

        // Store everything
        *mock.rigs.write().unwrap() = vec![rig.clone()];
        *mock.patches.write().unwrap() = patches;
        *mock.profiles.write().unwrap() = vec![profile.clone()];
        *mock.current_rig.write().unwrap() = Some(rig);
        *mock.current_profile.write().unwrap() = Some(profile.clone());
        *mock.current_preset.write().unwrap() = profile.presets.first().cloned();
        *mock.setlists.write().unwrap() = vec![main_setlist.clone(), all_setlist];
        *mock.current_setlist.write().unwrap() = Some(main_setlist);

        mock
    }

    /// Get the current profile
    pub fn profile(&self) -> Option<Profile> {
        self.current_profile.read().unwrap().clone()
    }

    /// Get the current rig
    pub fn rig(&self) -> Option<Rig> {
        self.current_rig.read().unwrap().clone()
    }

    /// Get the current preset
    pub fn preset(&self) -> Option<Preset> {
        self.current_preset.read().unwrap().clone()
    }

    /// Emit an event
    fn emit(&self, event: RigEvent) {
        let _ = self.event_tx.send(event);
    }

    /// Execute a command synchronously (avoids recursive async issues)
    fn execute_command(&self, cmd: RigCommand) {
        match cmd {
            RigCommand::LoadProfile { profile_id } => {
                let profiles = self.profiles.read().unwrap();
                if let Some(profile) = profiles.iter().find(|p| p.id == profile_id) {
                    *self.current_profile.write().unwrap() = Some(profile.clone());
                    self.emit(RigEvent::ProfileLoaded {
                        profile: ProfileInfo::from_profile(profile),
                    });
                }
            }
            RigCommand::LoadRig { rig_id } => {
                let rigs = self.rigs.read().unwrap();
                if let Some(rig) = rigs.iter().find(|r| r.id == rig_id) {
                    *self.current_rig.write().unwrap() = Some(rig.clone());
                    self.emit(RigEvent::RigLoaded {
                        rig: RigInfo::from_rig(rig),
                    });
                }
            }
            RigCommand::LoadPreset { preset_id } => {
                let profile = self.current_profile.read().unwrap();
                if let Some(preset) = profile.as_ref().and_then(|p| p.get_preset(preset_id)) {
                    *self.current_preset.write().unwrap() = Some(preset.clone());
                    *self.current_snapshot_id.write().unwrap() = preset.default_snapshot_id;
                    self.emit(RigEvent::PresetLoaded {
                        preset: PresetInfo::from_preset(preset),
                    });
                }
            }
            RigCommand::LoadPresetWithSnapshot { preset_id, snapshot_id } => {
                let profile = self.current_profile.read().unwrap();
                if let Some(preset) = profile.as_ref().and_then(|p| p.get_preset(preset_id)) {
                    *self.current_preset.write().unwrap() = Some(preset.clone());
                    *self.current_snapshot_id.write().unwrap() = Some(snapshot_id);
                    self.emit(RigEvent::PresetLoaded {
                        preset: PresetInfo::from_preset(preset),
                    });
                    self.emit(RigEvent::SnapshotActivated { preset_id, snapshot_id });
                }
            }
            RigCommand::GoToScene { scene_index } => {
                self.go_to_scene(scene_index);
            }
            RigCommand::NextScene => {
                let setlist = self.current_setlist.read().unwrap();
                let song_idx = *self.current_song_index.read().unwrap();
                let current_scene = *self.current_scene_index.read().unwrap();

                if let Some(song) = setlist.as_ref().and_then(|s| s.songs.get(song_idx)) {
                    if current_scene + 1 < song.scenes.len() {
                        drop(setlist);
                        self.go_to_scene(current_scene + 1);
                    }
                }
            }
            RigCommand::PreviousScene => {
                let current_scene = *self.current_scene_index.read().unwrap();
                if current_scene > 0 {
                    self.go_to_scene(current_scene - 1);
                }
            }
            RigCommand::GoToSong { song_index } => {
                self.go_to_song(song_index);
            }
            RigCommand::NextSong => {
                let song_idx = *self.current_song_index.read().unwrap();
                let should_advance = {
                    let setlist = self.current_setlist.read().unwrap();
                    setlist
                        .as_ref()
                        .map(|s| song_idx + 1 < s.songs.len())
                        .unwrap_or(false)
                };
                if should_advance {
                    self.go_to_song(song_idx + 1);
                }
            }
            RigCommand::PreviousSong => {
                let song_idx = *self.current_song_index.read().unwrap();
                if song_idx > 0 {
                    self.go_to_song(song_idx - 1);
                }
            }
            RigCommand::LoadSetlist { setlist_id } => {
                let setlists = self.setlists.read().unwrap();
                if let Some(setlist) = setlists.iter().find(|s| s.id == setlist_id) {
                    *self.current_setlist.write().unwrap() = Some(setlist.clone());
                    *self.current_song_index.write().unwrap() = 0;
                    *self.current_scene_index.write().unwrap() = 0;
                    self.emit(RigEvent::SetlistLoaded {
                        setlist: SetlistInfo::from_setlist(setlist),
                    });
                    // Also trigger song/scene changed events
                    self.emit(RigEvent::SongChanged { song_index: 0 });
                    self.emit(RigEvent::SceneChanged {
                        song_index: 0,
                        scene_index: 0,
                    });
                }
            }
            RigCommand::PreloadPreset { preset_id } => {
                // Simulate preloading (in real impl, this would load plugin state)
                let mut preloaded = self.preloaded.write().unwrap();
                if !preloaded.contains(&preset_id) {
                    preloaded.push(preset_id);
                }
                self.emit(RigEvent::PreloadCompleted { preset_id });
            }
            RigCommand::PreloadSong { song_index } => {
                // Simulate preloading all presets for a song
                let setlist = self.current_setlist.read().unwrap();
                if let Some(song) = setlist.as_ref().and_then(|s| s.songs.get(song_index)) {
                    let preset_ids: Vec<Uuid> = song.scenes.iter().map(|s| s.preset_id).collect();
                    drop(setlist);
                    for preset_id in preset_ids {
                        let mut preloaded = self.preloaded.write().unwrap();
                        if !preloaded.contains(&preset_id) {
                            preloaded.push(preset_id);
                            drop(preloaded);
                            self.emit(RigEvent::PreloadCompleted { preset_id });
                        }
                    }
                }
            }
            RigCommand::SetSectionEnabled { section_id, enabled } => {
                // Update the section in the current rig
                let mut rig = self.current_rig.write().unwrap();
                let should_emit = if let Some(rig) = rig.as_mut() {
                    if let Some(section) = rig.sections.iter_mut().find(|s| s.id == section_id) {
                        section.enabled = enabled;
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };
                drop(rig);
                if should_emit {
                    self.emit(RigEvent::SectionChanged { section_id });
                }
            }
            RigCommand::SetBlockBypassed { block_id, bypassed } => {
                // Update the global block in the current rig
                let mut rig = self.current_rig.write().unwrap();
                let should_emit = if let Some(rig) = rig.as_mut() {
                    if let Some(block) = rig.global_blocks.iter_mut().find(|b| b.block.id == block_id) {
                        block.block.bypassed = bypassed;
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };
                drop(rig);
                if should_emit {
                    self.emit(RigEvent::BlockChanged { block_id });
                }
            }
            // Other commands would be implemented similarly
            _ => {
                tracing::warn!("MockRig: unhandled command {:?}", cmd);
            }
        }
    }

    /// Go to a specific scene (helper to avoid code duplication)
    fn go_to_scene(&self, scene_index: usize) {
        let setlist = self.current_setlist.read().unwrap();
        let song_idx = *self.current_song_index.read().unwrap();

        if let Some(song) = setlist.as_ref().and_then(|s| s.songs.get(song_idx)) {
            if scene_index < song.scenes.len() {
                *self.current_scene_index.write().unwrap() = scene_index;

                // Load the scene's preset
                let scene = &song.scenes[scene_index];
                let profile = self.current_profile.read().unwrap();
                if let Some(preset) = profile.as_ref().and_then(|p| p.get_preset(scene.preset_id)) {
                    *self.current_preset.write().unwrap() = Some(preset.clone());
                    *self.current_snapshot_id.write().unwrap() = scene.snapshot_id;
                }

                self.emit(RigEvent::SceneChanged {
                    song_index: song_idx,
                    scene_index,
                });
            }
        }
    }

    /// Go to a specific song (helper to avoid code duplication)
    fn go_to_song(&self, song_index: usize) {
        let setlist = self.current_setlist.read().unwrap();
        if let Some(setlist_ref) = setlist.as_ref() {
            if song_index < setlist_ref.songs.len() {
                *self.current_song_index.write().unwrap() = song_index;
                *self.current_scene_index.write().unwrap() = 0;

                self.emit(RigEvent::SongChanged { song_index });

                // Load first scene
                if let Some(song) = setlist_ref.songs.get(song_index) {
                    if let Some(scene) = song.scenes.first() {
                        let profile = self.current_profile.read().unwrap();
                        if let Some(preset) = profile.as_ref().and_then(|p| p.get_preset(scene.preset_id)) {
                            *self.current_preset.write().unwrap() = Some(preset.clone());
                            *self.current_snapshot_id.write().unwrap() = scene.snapshot_id;
                        }
                    }
                }

                self.emit(RigEvent::SceneChanged {
                    song_index,
                    scene_index: 0,
                });
            }
        }
    }
}

impl Default for MockRig {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for MockRig {
    fn eq(&self, _other: &Self) -> bool {
        true // Services are typically singletons
    }
}

impl Clone for MockRig {
    fn clone(&self) -> Self {
        Self {
            profiles: Arc::clone(&self.profiles),
            rigs: Arc::clone(&self.rigs),
            patches: Arc::clone(&self.patches),
            current_profile: Arc::clone(&self.current_profile),
            current_rig: Arc::clone(&self.current_rig),
            current_preset: Arc::clone(&self.current_preset),
            current_snapshot_id: Arc::clone(&self.current_snapshot_id),
            setlists: Arc::clone(&self.setlists),
            current_setlist: Arc::clone(&self.current_setlist),
            current_song_index: Arc::clone(&self.current_song_index),
            current_scene_index: Arc::clone(&self.current_scene_index),
            event_tx: self.event_tx.clone(),
            preloaded: Arc::clone(&self.preloaded),
        }
    }
}

// endregion: --- MockRig

// region:    --- RigService Implementation

impl RigService for MockRig {
    async fn get_available_profiles(&self, _cx: &Context) -> Vec<ProfileInfo> {
        self.profiles
            .read()
            .unwrap()
            .iter()
            .map(ProfileInfo::from_profile)
            .collect()
    }

    async fn get_current_profile(&self, _cx: &Context) -> Option<ProfileInfo> {
        self.current_profile
            .read()
            .unwrap()
            .as_ref()
            .map(ProfileInfo::from_profile)
    }

    async fn get_current_rig(&self, _cx: &Context) -> Option<RigInfo> {
        self.current_rig
            .read()
            .unwrap()
            .as_ref()
            .map(RigInfo::from_rig)
    }

    async fn get_sections(&self, _cx: &Context) -> Vec<SectionInfo> {
        self.current_rig
            .read()
            .unwrap()
            .as_ref()
            .map(|r| r.sections.iter().map(SectionInfo::from_section).collect())
            .unwrap_or_default()
    }

    async fn get_global_blocks(&self, _cx: &Context) -> Vec<GlobalBlockInfo> {
        self.current_rig
            .read()
            .unwrap()
            .as_ref()
            .map(|r| {
                r.global_blocks
                    .iter()
                    .map(GlobalBlockInfo::from_global_block)
                    .collect()
            })
            .unwrap_or_default()
    }

    async fn get_available_presets(&self, _cx: &Context) -> Vec<PresetInfo> {
        self.current_profile
            .read()
            .unwrap()
            .as_ref()
            .map(|p| p.presets.iter().map(PresetInfo::from_preset).collect())
            .unwrap_or_default()
    }

    async fn get_current_preset(&self, _cx: &Context) -> Option<PresetInfo> {
        self.current_preset
            .read()
            .unwrap()
            .as_ref()
            .map(PresetInfo::from_preset)
    }

    async fn get_presets_by_category(&self, _cx: &Context, category: PresetCategory) -> Vec<PresetInfo> {
        self.current_profile
            .read()
            .unwrap()
            .as_ref()
            .map(|p| {
                p.presets
                    .iter()
                    .filter(|preset| {
                        // Check exact match or if preset is in the fallback chain
                        preset.category == category || category.matches(&preset.category)
                    })
                    .map(PresetInfo::from_preset)
                    .collect()
            })
            .unwrap_or_default()
    }

    async fn get_patches(&self, _cx: &Context, _section_id: Uuid) -> Vec<PatchInfo> {
        // For now, return all patches (could filter by section compatibility)
        self.patches
            .read()
            .unwrap()
            .iter()
            .map(PatchInfo::from_patch)
            .collect()
    }

    async fn get_available_setlists(&self, _cx: &Context) -> Vec<SetlistInfo> {
        self.setlists
            .read()
            .unwrap()
            .iter()
            .map(SetlistInfo::from_setlist)
            .collect()
    }

    async fn get_current_setlist(&self, _cx: &Context) -> Option<SetlistInfo> {
        self.current_setlist
            .read()
            .unwrap()
            .as_ref()
            .map(SetlistInfo::from_setlist)
    }

    async fn get_setlist_songs(&self, _cx: &Context) -> Vec<SongInfo> {
        let setlist = self.current_setlist.read().unwrap();
        let current_scene_idx = *self.current_scene_index.read().unwrap();

        setlist
            .as_ref()
            .map(|s| {
                s.songs
                    .iter()
                    .enumerate()
                    .map(|(idx, song)| SongInfo::from_song(idx, song, current_scene_idx))
                    .collect()
            })
            .unwrap_or_default()
    }

    async fn get_current_song(&self, _cx: &Context) -> Option<SongInfo> {
        let setlist = self.current_setlist.read().unwrap();
        let song_idx = *self.current_song_index.read().unwrap();
        let scene_idx = *self.current_scene_index.read().unwrap();

        setlist
            .as_ref()
            .and_then(|s| s.songs.get(song_idx))
            .map(|song| SongInfo::from_song(song_idx, song, scene_idx))
    }

    async fn get_current_scene(&self, _cx: &Context) -> Option<SceneInfo> {
        let setlist = self.current_setlist.read().unwrap();
        let song_idx = *self.current_song_index.read().unwrap();
        let scene_idx = *self.current_scene_index.read().unwrap();
        let profile = self.current_profile.read().unwrap();

        setlist
            .as_ref()
            .and_then(|s| s.songs.get(song_idx))
            .and_then(|song| song.scenes.get(scene_idx))
            .map(|scene| {
                let preset_name = profile
                    .as_ref()
                    .and_then(|p| p.get_preset(scene.preset_id))
                    .map(|p| p.name.clone())
                    .unwrap_or_default();

                let snapshot_name = scene.snapshot_id.and_then(|sid| {
                    profile
                        .as_ref()
                        .and_then(|p| p.get_preset(scene.preset_id))
                        .and_then(|preset| preset.get_snapshot(sid))
                        .map(|s| s.name.clone())
                });

                SceneInfo::from_scene(scene_idx, scene, preset_name, snapshot_name)
            })
    }

    async fn execute(&self, _cx: &Context, cmd: RigCommand) {
        tracing::debug!("MockRig::execute({:?})", cmd);
        self.execute_command(cmd);
    }

    async fn subscribe(&self, _cx: &Context, events: Tx<RigEvent>) {
        let mut rx = self.event_tx.subscribe();
        roam::session::runtime::spawn(async move {
            loop {
                match rx.recv().await {
                    Ok(event) => {
                        if events.send(&event).await.is_err() {
                            break;
                        }
                    }
                    Err(_) => break,
                }
            }
        });
    }

    async fn subscribe_parameters(
        &self,
        _cx: &Context,
        _block_ids: Vec<Uuid>,
        _values: Tx<(Uuid, u32, f64)>,
    ) {
        // Parameter subscription would be implemented similarly to event subscription
        // For now, this is a placeholder
    }
}

// endregion: --- RigService Implementation

// region:    --- Tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_mock_is_empty() {
        let mock = MockRig::new();
        assert!(mock.profile().is_none());
        assert!(mock.rig().is_none());
        assert!(mock.preset().is_none());
    }

    #[test]
    fn test_with_sample_data() {
        let mock = MockRig::with_sample_guitar_data();
        let profile = mock.profile().expect("should have profile");
        assert_eq!(profile.name, "FTS-Guitar");
        // Default Guitar + Genre presets: Worship, Blues, Rock, Metal, Jazz, Country
        // Archetype/variant presets: John Mayer, SRV, BB King, Classic Rock, Modern Rock, Thrash, Djent, Fusion, Outlaw Country
        assert_eq!(profile.presets.len(), 16);

        // The default preset should be "Default Guitar" with 13 blocks
        let default_preset = &profile.presets[0];
        assert_eq!(default_preset.name, "Default Guitar");
        assert_eq!(default_preset.blocks.len(), 13);

        let rig = mock.rig().expect("should have rig");
        assert_eq!(rig.name, "Guitar Rig");
        assert_eq!(rig.global_blocks.len(), 5); // Drive, Amp, Cab, Delay, Reverb
    }
}

// endregion: --- Tests
