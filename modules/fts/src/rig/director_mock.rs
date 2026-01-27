//! Mock implementation of DirectorService for testing and UI development
//!
//! Provides sample rigs for the full band:
//! - 4 Vocalists
//! - 2 Guitar rigs
//! - 1 Bass rig
//! - 2 Keyboard rigs
//! - 1 Drum rig (for live sample replacement)

use roam::{Context, Tx};
use std::sync::{Arc, RwLock};
use uuid::Uuid;

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

use super::core::{
    Block, DirectorInfo, InstrumentType, Rig, Module, ModuleType, PluginId, RigDirector,
    Role, RoleAssignment,
};
use super::director_service::{
    DirectorCommand, DirectorEvent, DirectorService, ModuleInfo, InputAssignmentInfo, RigViewInfo,
    RoleInfo, SendLevelInfo,
};

/// Mock implementation of the DirectorService.
pub struct MockDirector {
    /// The director being managed
    director: RwLock<RigDirector>,
    /// Broadcast channel for events
    event_tx: broadcast::Sender<DirectorEvent>,
}

impl MockDirector {
    /// Create a new mock director with full band rigs
    pub fn new() -> Arc<Self> {
        let director = Self::create_full_band_director();
        let (event_tx, _) = broadcast::channel(100);

        Arc::new(Self {
            director: RwLock::new(director),
            event_tx,
        })
    }

    /// Create a full band director with all instrument types
    fn create_full_band_director() -> RigDirector {
        let mut director = RigDirector::new("Full Band");

        // === Create roles ===
        // Vocal roles
        let lead_role = Role::lead();
        let bg1_role = Role::background(1);
        let bg2_role = Role::background(2);
        let bg3_role = Role::background(3);

        // Custom vocal roles (for song-specific assignments)
        let sarah_role = Role::custom("Sarah").with_color(0xFF6B6BFF);
        let dave_role = Role::custom("Dave").with_color(0x4ECDC4FF);
        let emma_role = Role::custom("Emma").with_color(0x95E1D3FF);
        let eli_role = Role::custom("Eli").with_color(0xF7DC6FFF);

        // Guitar roles
        let lead_gtr_role = Role::custom("Lead Guitar").with_color(0x1ABC9CFF);
        let rhythm_gtr_role = Role::custom("Rhythm Guitar").with_color(0x16A085FF);

        // Other roles
        let bass_role = Role::custom("Bass").with_color(0x9B59B6FF);
        let keys1_role = Role::custom("Keys 1").with_color(0x3498DBFF);
        let keys2_role = Role::custom("Keys 2").with_color(0x2980B9FF);
        let drums_role = Role::custom("Drums").with_color(0xF39C12FF);

        // Store role IDs before adding
        let lead_id = lead_role.id;
        let bg1_id = bg1_role.id;
        let bg2_id = bg2_role.id;
        let bg3_id = bg3_role.id;
        let lead_gtr_id = lead_gtr_role.id;
        let rhythm_gtr_id = rhythm_gtr_role.id;
        let bass_id = bass_role.id;
        let keys1_id = keys1_role.id;
        let keys2_id = keys2_role.id;
        let drums_id = drums_role.id;

        // Add all roles
        director.add_role(lead_role);
        director.add_role(bg1_role);
        director.add_role(bg2_role);
        director.add_role(bg3_role);
        director.add_role(sarah_role);
        director.add_role(dave_role);
        director.add_role(emma_role);
        director.add_role(eli_role);
        director.add_role(lead_gtr_role);
        director.add_role(rhythm_gtr_role);
        director.add_role(bass_role);
        director.add_role(keys1_role);
        director.add_role(keys2_role);
        director.add_role(drums_role);

        // === Create Vocal Rigs (4) ===
        let mut lead_vox = Self::create_vocal_rig("Lead Vocal", 0);
        let mut bg1_vox = Self::create_vocal_rig("BG Vocal 1", 1);
        let mut bg2_vox = Self::create_vocal_rig("BG Vocal 2", 2);
        let mut bg3_vox = Self::create_vocal_rig("BG Vocal 3", 3);

        let lead_vox_id = lead_vox.id;
        let bg1_vox_id = bg1_vox.id;
        let bg2_vox_id = bg2_vox.id;
        let bg3_vox_id = bg3_vox.id;

        lead_vox.current_role_id = Some(lead_id);
        bg1_vox.current_role_id = Some(bg1_id);
        bg2_vox.current_role_id = Some(bg2_id);
        bg3_vox.current_role_id = Some(bg3_id);

        director.add_rig(lead_vox);
        director.add_rig(bg1_vox);
        director.add_rig(bg2_vox);
        director.add_rig(bg3_vox);

        // === Create Guitar Rigs (2) ===
        let mut gtr1 = Self::create_guitar_rig("Lead Guitar", 4);
        let mut gtr2 = Self::create_guitar_rig("Rhythm Guitar", 5);

        let gtr1_id = gtr1.id;
        let gtr2_id = gtr2.id;

        gtr1.current_role_id = Some(lead_gtr_id);
        gtr2.current_role_id = Some(rhythm_gtr_id);

        director.add_rig(gtr1);
        director.add_rig(gtr2);

        // === Create Bass Rig (1) ===
        let mut bass = Self::create_bass_rig("Bass", 6);
        let bass_rig_id = bass.id;
        bass.current_role_id = Some(bass_id);
        director.add_rig(bass);

        // === Create Keyboard Rigs (2) ===
        let mut keys1 = Self::create_keyboard_rig("Keys 1", 7);
        let mut keys2 = Self::create_keyboard_rig("Keys 2", 8);

        let keys1_rig_id = keys1.id;
        let keys2_rig_id = keys2.id;

        keys1.current_role_id = Some(keys1_id);
        keys2.current_role_id = Some(keys2_id);

        director.add_rig(keys1);
        director.add_rig(keys2);

        // === Create Drum Rig (1) ===
        let mut drums = Self::create_drum_rig("Drums", 9);
        let drums_rig_id = drums.id;
        drums.current_role_id = Some(drums_id);
        director.add_rig(drums);

        // === Create global role assignments ===
        // Vocals
        director.add_assignment(RoleAssignment::global(lead_id, lead_vox_id));
        director.add_assignment(RoleAssignment::global(bg1_id, bg1_vox_id));
        director.add_assignment(RoleAssignment::global(bg2_id, bg2_vox_id));
        director.add_assignment(RoleAssignment::global(bg3_id, bg3_vox_id));

        // Guitars
        director.add_assignment(RoleAssignment::global(lead_gtr_id, gtr1_id));
        director.add_assignment(RoleAssignment::global(rhythm_gtr_id, gtr2_id));

        // Bass
        director.add_assignment(RoleAssignment::global(bass_id, bass_rig_id));

        // Keys
        director.add_assignment(RoleAssignment::global(keys1_id, keys1_rig_id));
        director.add_assignment(RoleAssignment::global(keys2_id, keys2_rig_id));

        // Drums
        director.add_assignment(RoleAssignment::global(drums_id, drums_rig_id));

        // Create shared sends engine
        director.sends_engine = Self::create_sends_engine();

        director
    }

    // === Vocal Rig Creation ===

    /// Create a vocal rig with standard engine chain
    fn create_vocal_rig(name: &str, input_index: u8) -> Rig {
        let mut rig = Rig::vocal(name).with_group("Vocals");
        rig.set_input(input_index);

        // Replace default engines with detailed ones
        rig.modules.clear();
        rig.add_module(Self::create_rescue_engine());
        rig.add_module(Self::create_correction_engine());
        rig.add_module(Self::create_tonal_engine());
        rig.add_module(Self::create_modulation_engine());

        rig
    }

    fn create_rescue_engine() -> Module {
        let mut engine = Module::new("Rescue", ModuleType::Rescue);

        let gate = Block::new("Gate", PluginId::vst3("gate-001", "Noise Gate"));
        let gate_id = engine.add_block(gate, 0);
        engine.add_macro("Gate", gate_id, 0, "0.3", 0.3);

        let deesser = Block::new("De-Esser", PluginId::vst3("deesser-001", "De-Esser"));
        let deesser_id = engine.add_block(deesser, 1);
        engine.add_macro("DeEs", deesser_id, 0, "0.5", 0.5);

        let eq = Block::new("Rescue EQ", PluginId::vst3("eq-001", "Parametric EQ"));
        engine.add_block(eq, 2);

        let comp = Block::new("Compressor", PluginId::vst3("comp-001", "Compressor"));
        engine.add_block(comp, 3);

        engine
    }

    fn create_correction_engine() -> Module {
        let mut engine = Module::new("Correction", ModuleType::Correction);

        let tuner = Block::new("Tuner", PluginId::vst3("tuner-001", "Auto-Tune"));
        let tuner_id = engine.add_block(tuner, 0);
        engine.add_macro("Tune", tuner_id, 0, "On", 1.0);

        engine
    }

    fn create_tonal_engine() -> Module {
        let mut engine = Module::new("Tonal", ModuleType::Tonal);

        let comp = Block::new("Style Comp", PluginId::vst3("comp-002", "LA-2A"));
        let comp_id = engine.add_block(comp, 0);
        engine.add_macro("Drive", comp_id, 0, "4", 0.4);

        let eq = Block::new("Tonal EQ", PluginId::vst3("eq-002", "Pultec EQ"));
        let eq_id = engine.add_block(eq, 1);
        engine.add_macro("EQ", eq_id, 0, "Warm", 0.6);

        let sat = Block::new("Saturator", PluginId::vst3("sat-001", "Tape Saturator"));
        engine.add_block(sat, 2);

        engine
    }

    fn create_modulation_engine() -> Module {
        let mut engine = Module::new("Modulation", ModuleType::Modulation);

        let chorus = Block::new("Chorus", PluginId::vst3("chorus-001", "Chorus"));
        let chorus_id = engine.add_block(chorus, 0);
        engine.add_macro("Chor", chorus_id, 0, "Off", 0.0);

        let flanger = Block::new("Flanger", PluginId::vst3("flanger-001", "Flanger"));
        let flanger_id = engine.add_block(flanger, 1);
        engine.add_macro("Flng", flanger_id, 0, "Off", 0.0);

        engine
    }

    // === Guitar Rig Creation ===

    fn create_guitar_rig(name: &str, input_index: u8) -> Rig {
        let mut rig = Rig::guitar(name).with_group("Guitars");
        rig.set_input(input_index);

        // Replace default engines with detailed ones
        rig.modules.clear();
        rig.add_module(Self::create_amp_engine());
        rig.add_module(Self::create_cabinet_engine());
        rig.add_module(Self::create_guitar_modulation_engine());

        rig
    }

    fn create_amp_engine() -> Module {
        let mut engine = Module::new("Amp", ModuleType::Amp);

        let amp = Block::new("Amp Sim", PluginId::vst3("amp-001", "Neural Amp"));
        let amp_id = engine.add_block(amp, 0);
        engine.add_macro("Gain", amp_id, 0, "6", 0.6);
        engine.add_macro("Tone", amp_id, 1, "5", 0.5);

        engine
    }

    fn create_cabinet_engine() -> Module {
        let mut engine = Module::new("Cabinet", ModuleType::Cabinet);

        let ir = Block::new("IR Loader", PluginId::vst3("ir-001", "Cab IR"));
        let ir_id = engine.add_block(ir, 0);
        engine.add_macro("IR", ir_id, 0, "4x12", 1.0);

        engine
    }

    fn create_guitar_modulation_engine() -> Module {
        let mut engine = Module::new("Modulation", ModuleType::Modulation);

        let delay = Block::new("Delay", PluginId::vst3("delay-002", "Tape Delay"));
        let delay_id = engine.add_block(delay, 0);
        engine.add_macro("Dly", delay_id, 0, "30%", 0.3);

        let reverb = Block::new("Reverb", PluginId::vst3("verb-002", "Spring Reverb"));
        let reverb_id = engine.add_block(reverb, 1);
        engine.add_macro("Verb", reverb_id, 0, "25%", 0.25);

        engine
    }

    // === Bass Rig Creation ===

    fn create_bass_rig(name: &str, input_index: u8) -> Rig {
        let mut rig = Rig::bass(name).with_group("Bass");
        rig.set_input(input_index);

        // Replace default engines with detailed ones
        rig.modules.clear();
        rig.add_module(Self::create_bass_amp_engine());
        rig.add_module(Self::create_bass_cabinet_engine());
        rig.add_module(Self::create_bass_compression_engine());

        rig
    }

    fn create_bass_amp_engine() -> Module {
        let mut engine = Module::new("Amp", ModuleType::Amp);

        let amp = Block::new("Bass Amp", PluginId::vst3("bass-amp-001", "SVT"));
        let amp_id = engine.add_block(amp, 0);
        engine.add_macro("Drive", amp_id, 0, "4", 0.4);
        engine.add_macro("Low", amp_id, 1, "6", 0.6);

        engine
    }

    fn create_bass_cabinet_engine() -> Module {
        let mut engine = Module::new("Cabinet", ModuleType::Cabinet);

        let ir = Block::new("IR Loader", PluginId::vst3("bass-ir-001", "8x10 IR"));
        let ir_id = engine.add_block(ir, 0);
        engine.add_macro("IR", ir_id, 0, "8x10", 1.0);

        engine
    }

    fn create_bass_compression_engine() -> Module {
        let mut engine = Module::new("Compression", ModuleType::Compression);

        let comp = Block::new("Compressor", PluginId::vst3("bass-comp-001", "FET Comp"));
        let comp_id = engine.add_block(comp, 0);
        engine.add_macro("Comp", comp_id, 0, "4:1", 0.5);
        engine.add_macro("Atk", comp_id, 1, "10ms", 0.3);

        engine
    }

    // === Keyboard Rig Creation ===

    fn create_keyboard_rig(name: &str, input_index: u8) -> Rig {
        let mut rig = Rig::keyboard(name).with_group("Keys");
        rig.set_input(input_index);

        // Replace default engines with detailed ones
        rig.modules.clear();
        rig.add_module(Self::create_keys_tonal_engine());
        rig.add_module(Self::create_keys_modulation_engine());

        rig
    }

    fn create_keys_tonal_engine() -> Module {
        let mut engine = Module::new("Tonal", ModuleType::Tonal);

        let eq = Block::new("EQ", PluginId::vst3("keys-eq-001", "Parametric EQ"));
        let eq_id = engine.add_block(eq, 0);
        engine.add_macro("High", eq_id, 0, "+2dB", 0.6);

        let comp = Block::new("Compressor", PluginId::vst3("keys-comp-001", "Opto Comp"));
        let comp_id = engine.add_block(comp, 1);
        engine.add_macro("Comp", comp_id, 0, "2:1", 0.3);

        engine
    }

    fn create_keys_modulation_engine() -> Module {
        let mut engine = Module::new("Modulation", ModuleType::Modulation);

        let chorus = Block::new("Chorus", PluginId::vst3("keys-chorus-001", "Dimension"));
        let chorus_id = engine.add_block(chorus, 0);
        engine.add_macro("Chor", chorus_id, 0, "II", 0.5);

        let tremolo = Block::new("Tremolo", PluginId::vst3("keys-trem-001", "Tremolo"));
        let tremolo_id = engine.add_block(tremolo, 1);
        engine.add_macro("Trem", tremolo_id, 0, "Off", 0.0);

        engine
    }

    // === Drum Rig Creation ===

    fn create_drum_rig(name: &str, input_index: u8) -> Rig {
        let mut rig = Rig::drums(name).with_group("Drums");
        rig.set_input(input_index);

        // Replace default engines with detailed ones
        rig.modules.clear();
        rig.add_module(Self::create_transient_engine());
        rig.add_module(Self::create_drums_tonal_engine());

        rig
    }

    fn create_transient_engine() -> Module {
        let mut engine = Module::new("Transient", ModuleType::Transient);

        let transient = Block::new("Transient", PluginId::vst3("trans-001", "Transient Designer"));
        let trans_id = engine.add_block(transient, 0);
        engine.add_macro("Atk", trans_id, 0, "+3dB", 0.65);
        engine.add_macro("Sus", trans_id, 1, "-2dB", 0.4);

        engine
    }

    fn create_drums_tonal_engine() -> Module {
        let mut engine = Module::new("Tonal", ModuleType::Tonal);

        let eq = Block::new("EQ", PluginId::vst3("drum-eq-001", "API EQ"));
        let eq_id = engine.add_block(eq, 0);
        engine.add_macro("Punch", eq_id, 0, "+4dB", 0.7);

        let comp = Block::new("Compressor", PluginId::vst3("drum-comp-001", "SSL Comp"));
        let comp_id = engine.add_block(comp, 1);
        engine.add_macro("Crush", comp_id, 0, "20%", 0.2);

        engine
    }

    // === Shared Sends Module ===

    fn create_sends_engine() -> Module {
        let mut engine = Module::new("Sends", ModuleType::Sends);

        let verb = Block::new("Verb", PluginId::vst3("verb-001", "Plate Reverb"));
        let verb_id = engine.add_block(verb, 0);
        engine.add_macro("Verb", verb_id, 0, "35%", 0.35);

        let delay = Block::new("Delay", PluginId::vst3("delay-001", "Tape Delay"));
        let delay_id = engine.add_block(delay, 1);
        engine.add_macro("Dly", delay_id, 0, "20%", 0.2);

        let special = Block::new("Special", PluginId::vst3("special-001", "Multi-FX"));
        engine.add_block(special, 2);

        engine
    }

    // === Event handling ===

    fn emit(&self, event: DirectorEvent) {
        let _ = self.event_tx.send(event);
    }

    fn build_rig_view_info(&self, rig: &Rig, role: Option<&Role>) -> RigViewInfo {
        let director = self.director.read().unwrap();

        let status = if !rig.enabled {
            super::core::RigStatus::Muted
        } else {
            super::core::RigStatus::Idle
        };

        RigViewInfo {
            rig_id: rig.id,
            rig_name: rig.name.clone(),
            instrument_type: rig.instrument_type,
            assigned_role: role.map(RoleInfo::from_role),
            input: rig
                .input_assignment
                .as_ref()
                .map(InputAssignmentInfo::from_input),
            status,
            engines: rig.modules.iter().map(ModuleInfo::from_engine).collect(),
            send_levels: rig
                .send_levels
                .iter()
                .filter_map(|s| {
                    director
                        .sends_engine
                        .blocks
                        .iter()
                        .find(|b| b.block.id == s.sends_block_id)
                        .map(|b| SendLevelInfo {
                            sends_block_id: s.sends_block_id,
                            sends_block_name: b.block.name.clone(),
                            level: s.level,
                            pre_fader: s.pre_fader,
                            enabled: s.enabled,
                        })
                })
                .collect(),
            output_level: rig.output_level,
            pan: rig.pan,
            enabled: rig.enabled,
            group: rig.group.clone(),
        }
    }
}

impl Default for MockDirector {
    fn default() -> Self {
        let (event_tx, _) = broadcast::channel(100);
        Self {
            director: RwLock::new(Self::create_full_band_director()),
            event_tx,
        }
    }
}

impl DirectorService for MockDirector {
    async fn get_director(&self, _ctx: &Context) -> Option<DirectorInfo> {
        let director = self.director.read().unwrap();
        Some(director.info())
    }

    async fn get_rig_views(&self, _ctx: &Context) -> Vec<RigViewInfo> {
        let director = self.director.read().unwrap();

        director
            .rigs
            .iter()
            .map(|rig| {
                let role = rig
                    .current_role_id
                    .and_then(|rid| director.get_role(rid));
                self.build_rig_view_info(rig, role)
            })
            .collect()
    }

    async fn get_rig_view(&self, _ctx: &Context, rig_id: Uuid) -> Option<RigViewInfo> {
        let director = self.director.read().unwrap();

        director.get_rig(rig_id).map(|rig| {
            let role = rig
                .current_role_id
                .and_then(|rid| director.get_role(rid));
            self.build_rig_view_info(rig, role)
        })
    }

    async fn get_sends_engine(&self, _ctx: &Context) -> Option<ModuleInfo> {
        let director = self.director.read().unwrap();
        Some(ModuleInfo::from_engine(&director.sends_engine))
    }

    async fn get_roles(&self, _ctx: &Context) -> Vec<RoleInfo> {
        let director = self.director.read().unwrap();
        director.roles.iter().map(RoleInfo::from_role).collect()
    }

    async fn get_role_assignments(&self, _ctx: &Context) -> Vec<RoleAssignment> {
        let director = self.director.read().unwrap();
        director.role_assignments.clone()
    }

    async fn execute(&self, _ctx: &Context, cmd: DirectorCommand) {
        match cmd {
            DirectorCommand::AssignRoleGlobal { role_id, rig_id } => {
                let mut director = self.director.write().unwrap();
                director.assign_role_global(role_id, rig_id);
                drop(director);
                self.emit(DirectorEvent::RoleAssignmentChanged {
                    rig_id,
                    role_id: Some(role_id),
                });
            }

            DirectorCommand::AssignRoleForSong {
                role_id,
                rig_id,
                song_id,
            } => {
                let mut director = self.director.write().unwrap();
                director
                    .role_assignments
                    .push(RoleAssignment::for_song(role_id, rig_id, song_id));
                drop(director);
                self.emit(DirectorEvent::RoleAssignmentChanged {
                    rig_id,
                    role_id: Some(role_id),
                });
            }

            DirectorCommand::ClearRoleAssignment { rig_id } => {
                let mut director = self.director.write().unwrap();
                director
                    .role_assignments
                    .retain(|a| a.rig_id != rig_id);
                if let Some(rig) = director.get_rig_mut(rig_id) {
                    rig.current_role_id = None;
                }
                drop(director);
                self.emit(DirectorEvent::RoleAssignmentChanged {
                    rig_id,
                    role_id: None,
                });
            }

            DirectorCommand::SetRigEnabled { rig_id, enabled } => {
                let mut director = self.director.write().unwrap();
                if let Some(rig) = director.get_rig_mut(rig_id) {
                    rig.enabled = enabled;
                }
                drop(director);
                self.emit(DirectorEvent::RigChanged { rig_id });
            }

            DirectorCommand::SetRigLevel { rig_id, level } => {
                let mut director = self.director.write().unwrap();
                if let Some(rig) = director.get_rig_mut(rig_id) {
                    rig.output_level = level.clamp(0.0, 1.0);
                }
                drop(director);
                self.emit(DirectorEvent::RigChanged { rig_id });
            }

            DirectorCommand::SetRigPan { rig_id, pan } => {
                let mut director = self.director.write().unwrap();
                if let Some(rig) = director.get_rig_mut(rig_id) {
                    rig.pan = pan.clamp(-1.0, 1.0);
                }
                drop(director);
                self.emit(DirectorEvent::RigChanged { rig_id });
            }

            DirectorCommand::SetModuleEnabled {
                rig_id,
                module_type,
                enabled,
            } => {
                let mut director = self.director.write().unwrap();
                if let Some(rig) = director.get_rig_mut(rig_id) {
                    if let Some(engine) = rig.get_module_mut(module_type) {
                        engine.enabled = enabled;
                    }
                }
                drop(director);
                self.emit(DirectorEvent::ModuleChanged {
                    rig_id,
                    module_type,
                });
            }

            DirectorCommand::SetModuleLevel {
                rig_id,
                module_type,
                level,
            } => {
                let mut director = self.director.write().unwrap();
                if let Some(rig) = director.get_rig_mut(rig_id) {
                    if let Some(engine) = rig.get_module_mut(module_type) {
                        engine.level = level.clamp(0.0, 1.0);
                    }
                }
                drop(director);
                self.emit(DirectorEvent::ModuleChanged {
                    rig_id,
                    module_type,
                });
            }

            DirectorCommand::SetMacroValue {
                rig_id,
                macro_id,
                value,
            } => {
                let mut director = self.director.write().unwrap();
                if let Some(rig) = director.get_rig_mut(rig_id) {
                    for engine in &mut rig.modules {
                        if let Some(m) = engine.macros.iter_mut().find(|m| m.id == macro_id) {
                            m.normalized = value.clamp(0.0, 1.0);
                            m.display_value = format!("{:.0}%", value * 100.0);
                            break;
                        }
                    }
                }
                drop(director);
                self.emit(DirectorEvent::MacroChanged {
                    rig_id,
                    macro_id,
                    value,
                });
            }

            DirectorCommand::SetSendLevel {
                rig_id,
                sends_block_id,
                level,
            } => {
                let mut director = self.director.write().unwrap();
                if let Some(rig) = director.get_rig_mut(rig_id) {
                    rig.set_send_level(sends_block_id, level);
                }
                drop(director);
                self.emit(DirectorEvent::SendLevelChanged {
                    rig_id,
                    sends_block_id,
                    level,
                });
            }

            DirectorCommand::SetSendEnabled {
                rig_id,
                sends_block_id,
                enabled,
            } => {
                let mut director = self.director.write().unwrap();
                if let Some(rig) = director.get_rig_mut(rig_id) {
                    if let Some(send) = rig
                        .send_levels
                        .iter_mut()
                        .find(|s| s.sends_block_id == sends_block_id)
                    {
                        send.enabled = enabled;
                    }
                }
                drop(director);
                self.emit(DirectorEvent::RigChanged { rig_id });
            }

            DirectorCommand::ReassignInput { rig_id, input_index } => {
                let mut director = self.director.write().unwrap();
                if let Some(rig) = director.get_rig_mut(rig_id) {
                    rig.set_input(input_index);
                }
                drop(director);
                self.emit(DirectorEvent::InputReassigned {
                    rig_id,
                    input_index,
                });
            }

            DirectorCommand::SetInputGain { rig_id, gain_db } => {
                let mut director = self.director.write().unwrap();
                if let Some(rig) = director.get_rig_mut(rig_id) {
                    if let Some(input) = &mut rig.input_assignment {
                        input.gain_db = gain_db;
                    }
                }
                drop(director);
                self.emit(DirectorEvent::RigChanged { rig_id });
            }

            DirectorCommand::SetPhantomPower { rig_id, enabled } => {
                let mut director = self.director.write().unwrap();
                if let Some(rig) = director.get_rig_mut(rig_id) {
                    if let Some(input) = &mut rig.input_assignment {
                        input.phantom_power = enabled;
                    }
                }
                drop(director);
                self.emit(DirectorEvent::RigChanged { rig_id });
            }

            DirectorCommand::SetSendsBlockEnabled { block_id, enabled } => {
                let mut director = self.director.write().unwrap();
                if let Some(block) = director
                    .sends_engine
                    .blocks
                    .iter_mut()
                    .find(|b| b.block.id == block_id)
                {
                    block.block.bypassed = !enabled;
                }
                drop(director);
                self.emit(DirectorEvent::SendsModuleChanged);
            }

            DirectorCommand::SetSendsParameter {
                block_id,
                param_index,
                value,
            } => {
                let mut director = self.director.write().unwrap();
                if let Some(block) = director
                    .sends_engine
                    .blocks
                    .iter_mut()
                    .find(|b| b.block.id == block_id)
                {
                    block.block.set_parameter(param_index, value);
                }
                drop(director);
                self.emit(DirectorEvent::SendsModuleChanged);
            }
        }
    }

    async fn subscribe(&self, _ctx: &Context, events: Tx<DirectorEvent>) {
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
}
