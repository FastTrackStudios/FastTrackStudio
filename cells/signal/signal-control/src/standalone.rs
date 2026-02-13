//! StandaloneRigControlService — DB-backed implementation of RigControlService.
//!
//! Queries SQLite via signal-storage repos and returns the same Info types
//! as MockRigControlService. Swappable via enum dispatch in SignalControl.

use std::sync::{Arc, RwLock};

use roam::{Context, Tx};
use uuid::Uuid;

use signal::id::{PatchId, ProfileId, RigId, RigPresetId, SongId};
use signal::module::ModuleType;
use signal::normalized::Rating;
use signal::rig::InstrumentType;
use signal::service::{
    EngineStateInfo, PatchInfo, PreloadStatusInfo, PresetSceneInfo, ProfileInfo, RigControlCommand,
    RigControlEvent, RigInfo, RigPresetInfo, SectionInfo, SetlistInfo, SlotStateInfo, SongInfo,
};
use signal::RigControlService;
use signal_storage::DatabaseConnection;

/// DB-backed RigControlService implementation.
///
/// Loads profiles, songs, and presets from SQLite filtered by instrument type.
/// Engine-related methods return sensible defaults (no real audio engine).
pub struct StandaloneRigControlService {
    db: DatabaseConnection,
    instrument_type: String,

    // Navigation state (mirrors MockRigControlService)
    current_profile_index: RwLock<usize>,
    current_song_index: RwLock<usize>,
    current_section_index: RwLock<usize>,
    current_patch: RwLock<Option<PatchInfo>>,
    current_preset: RwLock<Option<RigPresetInfo>>,

    // Event broadcasting
    event_subscriber: RwLock<Option<Arc<Tx<RigControlEvent>>>>,

    // Cached data (loaded from DB on construction, refreshable)
    cached_profiles: RwLock<Vec<ProfileInfo>>,
    cached_songs: RwLock<Vec<SongInfo>>,
    cached_presets: RwLock<Vec<RigPresetInfo>>,
}

impl StandaloneRigControlService {
    /// Create a new standalone service backed by the given database.
    ///
    /// Loads all data for `instrument_type` into caches immediately.
    pub async fn new(db: DatabaseConnection, instrument_type: &str) -> eyre::Result<Self> {
        let svc = Self {
            db,
            instrument_type: instrument_type.to_string(),
            current_profile_index: RwLock::new(0),
            current_song_index: RwLock::new(0),
            current_section_index: RwLock::new(0),
            current_patch: RwLock::new(None),
            current_preset: RwLock::new(None),
            event_subscriber: RwLock::new(None),
            cached_profiles: RwLock::new(Vec::new()),
            cached_songs: RwLock::new(Vec::new()),
            cached_presets: RwLock::new(Vec::new()),
        };
        svc.refresh_caches().await?;
        Ok(svc)
    }

    pub fn db(&self) -> &DatabaseConnection {
        &self.db
    }

    pub fn instrument_type(&self) -> &str {
        &self.instrument_type
    }

    /// Reload all cached data from the database.
    ///
    /// Call after CRUD operations to pick up changes.
    pub async fn refresh_caches(&self) -> eyre::Result<()> {
        self.refresh_presets_cache().await?;
        self.refresh_profiles_cache().await?;
        self.refresh_songs_cache().await?;
        Ok(())
    }

    // ── Private cache refresh methods ────────────────────────────────

    async fn refresh_presets_cache(&self) -> eyre::Result<()> {
        let db_presets =
            signal_storage::preset_repo::list_presets_by_type(&self.db, &self.instrument_type)
                .await?;

        let mut infos = Vec::with_capacity(db_presets.len());
        for p in &db_presets {
            let category = p.category_parsed();
            let rating = Rating::default();
            let scenes = signal_storage::preset_repo::list_preset_snapshots(&self.db, p.id)
                .await
                .unwrap_or_default()
                .into_iter()
                .map(|s| PresetSceneInfo {
                    id: s.id,
                    name: s.name,
                    is_default: s.is_default,
                })
                .collect();
            infos.push(RigPresetInfo {
                id: RigPresetId::from_uuid(p.id),
                name: p.name.clone(),
                category,
                rating,
                scenes,
            });
        }
        *self.cached_presets.write().unwrap() = infos;
        Ok(())
    }

    async fn refresh_profiles_cache(&self) -> eyre::Result<()> {
        let db_profiles =
            signal_storage::profile_repo::list_profiles_by_type(&self.db, &self.instrument_type)
                .await?;

        let mut infos = Vec::with_capacity(db_profiles.len());
        for prof in &db_profiles {
            let templates = signal_storage::profile_repo::list_scene_templates(&self.db, prof.id)
                .await
                .unwrap_or_default();
            let patches: Vec<PatchInfo> = templates
                .iter()
                .enumerate()
                .map(|(i, t)| PatchInfo {
                    id: PatchId::from_uuid(t.id),
                    name: t.name.clone(),
                    index: i,
                })
                .collect();
            infos.push(ProfileInfo {
                id: ProfileId::from_uuid(prof.id),
                name: prof.name.clone(),
                patch_count: patches.len(),
                patches,
            });
        }
        *self.cached_profiles.write().unwrap() = infos;
        Ok(())
    }

    async fn refresh_songs_cache(&self) -> eyre::Result<()> {
        let db_songs =
            signal_storage::song_repo::list_songs_by_type(&self.db, &self.instrument_type).await?;

        let mut infos = Vec::with_capacity(db_songs.len());
        for (idx, song) in db_songs.iter().enumerate() {
            let sections = signal_storage::song_repo::list_song_scenes(&self.db, song.id)
                .await
                .unwrap_or_default();
            let section_names: Vec<String> = sections.iter().map(|s| s.name.clone()).collect();
            let default_section_index = sections.iter().position(|s| s.is_default);
            infos.push(SongInfo {
                id: SongId::from_uuid(song.id),
                index: idx,
                name: song.name.clone(),
                artist: song.artist.clone(),
                section_count: sections.len(),
                section_names,
                default_section_index,
                current_section_index: None,
            });
        }
        *self.cached_songs.write().unwrap() = infos;
        Ok(())
    }

    // ── Event broadcasting (same pattern as MockRigControlService) ───

    pub fn broadcast_event(&self, event: RigControlEvent) {
        let tx_opt = self.event_subscriber.read().unwrap().clone();
        if let Some(tx) = tx_opt {
            #[cfg(not(target_arch = "wasm32"))]
            {
                tokio::spawn(async move {
                    let _ = tx.send(&event).await;
                });
            }
            #[cfg(target_arch = "wasm32")]
            {
                wasm_bindgen_futures::spawn_local(async move {
                    let _ = tx.send(&event).await;
                });
            }
        }
    }

    /// Parse instrument_type string to the InstrumentType enum.
    fn instrument_type_enum(&self) -> InstrumentType {
        match self.instrument_type.as_str() {
            "guitar" => InstrumentType::Guitar,
            "bass" => InstrumentType::Bass,
            "keys" => InstrumentType::Keys,
            "vocals" => InstrumentType::Vocals,
            "drums" => InstrumentType::Drums,
            "synth" | "synth_bass" => InstrumentType::Synth,
            other => InstrumentType::Custom(other.to_string()),
        }
    }
}

// ── RigControlService trait implementation ───────────────────────────

impl RigControlService for StandaloneRigControlService {
    // ── Engine State (no real engine) ────────────────────────────────

    async fn get_engine_state(&self, _cx: &Context) -> EngineStateInfo {
        EngineStateInfo::uninitialized()
    }

    async fn get_slot_state(
        &self,
        _cx: &Context,
        _module_type: ModuleType,
    ) -> Option<SlotStateInfo> {
        None
    }

    async fn get_all_slot_states(&self, _cx: &Context) -> Vec<SlotStateInfo> {
        Vec::new()
    }

    async fn check_preload_status(&self, _cx: &Context, _handle: u64) -> Option<PreloadStatusInfo> {
        None
    }

    // ── Profile/Song/Preset Queries ──────────────────────────────────

    async fn get_available_profiles(&self, _cx: &Context) -> Vec<ProfileInfo> {
        self.cached_profiles.read().unwrap().clone()
    }

    async fn get_current_profile(&self, _cx: &Context) -> Option<ProfileInfo> {
        let idx = *self.current_profile_index.read().unwrap();
        self.cached_profiles.read().unwrap().get(idx).cloned()
    }

    async fn get_current_patch(&self, _cx: &Context) -> Option<PatchInfo> {
        self.current_patch.read().unwrap().clone()
    }

    async fn get_current_rig(&self, _cx: &Context) -> Option<RigInfo> {
        Some(RigInfo {
            id: RigId::from_uuid(Uuid::nil()),
            name: format!("{} Standalone", self.instrument_type),
            instrument_type: self.instrument_type_enum(),
            engine_count: 0,
        })
    }

    async fn get_available_setlists(&self, _cx: &Context) -> Vec<SetlistInfo> {
        let songs = self.cached_songs.read().unwrap();
        let song_names: Vec<String> = songs.iter().map(|s| s.name.clone()).collect();
        vec![SetlistInfo {
            name: "Setlist".to_string(),
            song_count: songs.len(),
            song_names,
        }]
    }

    async fn get_current_setlist(&self, _cx: &Context) -> Option<SetlistInfo> {
        let songs = self.cached_songs.read().unwrap();
        let song_names: Vec<String> = songs.iter().map(|s| s.name.clone()).collect();
        Some(SetlistInfo {
            name: "Setlist".to_string(),
            song_count: songs.len(),
            song_names,
        })
    }

    async fn get_setlist_songs(&self, _cx: &Context) -> Vec<SongInfo> {
        self.cached_songs.read().unwrap().clone()
    }

    async fn get_current_song(&self, _cx: &Context) -> Option<SongInfo> {
        let idx = *self.current_song_index.read().unwrap();
        self.cached_songs.read().unwrap().get(idx).cloned()
    }

    async fn get_current_section(&self, _cx: &Context) -> Option<SectionInfo> {
        let song_idx = *self.current_song_index.read().unwrap();
        let section_idx = *self.current_section_index.read().unwrap();
        let songs = self.cached_songs.read().unwrap();
        let song = songs.get(song_idx)?;
        let name = song.section_names.get(section_idx)?.clone();
        Some(SectionInfo {
            index: section_idx,
            name,
            has_overrides: false,
        })
    }

    async fn get_available_presets(&self, _cx: &Context) -> Vec<RigPresetInfo> {
        self.cached_presets.read().unwrap().clone()
    }

    // ── Commands ─────────────────────────────────────────────────────

    async fn execute(&self, _cx: &Context, cmd: RigControlCommand) {
        match cmd {
            RigControlCommand::LoadSongSection {
                song_index,
                section_index,
            } => {
                let songs = self.cached_songs.read().unwrap();
                if song_index < songs.len() && section_index < songs[song_index].section_names.len()
                {
                    drop(songs);
                    *self.current_song_index.write().unwrap() = song_index;
                    *self.current_section_index.write().unwrap() = section_index;
                    self.broadcast_event(RigControlEvent::SongChanged { song_index });
                    self.broadcast_event(RigControlEvent::SectionChanged { section_index });
                }
            }
            RigControlCommand::LoadProfile { profile_id } => {
                let profiles = self.cached_profiles.read().unwrap();
                if let Some(index) = profiles.iter().position(|p| p.id == profile_id) {
                    let profile_info = profiles[index].clone();
                    drop(profiles);
                    *self.current_profile_index.write().unwrap() = index;
                    self.broadcast_event(RigControlEvent::ProfileLoaded {
                        profile: profile_info.clone(),
                    });

                    // Auto-select first patch so a profile always has an active patch
                    if let Some(first_patch) = profile_info.patches.first() {
                        let patch_info = first_patch.clone();
                        *self.current_patch.write().unwrap() = Some(patch_info.clone());
                        self.broadcast_event(RigControlEvent::PatchLoaded { patch: patch_info });
                    }
                }
            }
            RigControlCommand::LoadPatch {
                profile_id,
                patch_index,
            } => {
                let profiles = self.cached_profiles.read().unwrap();
                if let Some(profile) = profiles.iter().find(|p| p.id == profile_id) {
                    if let Some(patch) = profile.patches.get(patch_index) {
                        let patch_info = patch.clone();
                        drop(profiles);
                        *self.current_patch.write().unwrap() = Some(patch_info.clone());
                        self.broadcast_event(RigControlEvent::PatchLoaded { patch: patch_info });
                    }
                }
            }
            RigControlCommand::NextSong => {
                let songs_len = self.cached_songs.read().unwrap().len();
                let mut song_index = self.current_song_index.write().unwrap();
                *song_index = (*song_index + 1).min(songs_len.saturating_sub(1));
                *self.current_section_index.write().unwrap() = 0;
                let index = *song_index;
                drop(song_index);
                self.broadcast_event(RigControlEvent::SongChanged { song_index: index });
                self.broadcast_event(RigControlEvent::SectionChanged { section_index: 0 });
            }
            RigControlCommand::PreviousSong => {
                let mut song_index = self.current_song_index.write().unwrap();
                *song_index = song_index.saturating_sub(1);
                *self.current_section_index.write().unwrap() = 0;
                let index = *song_index;
                drop(song_index);
                self.broadcast_event(RigControlEvent::SongChanged { song_index: index });
                self.broadcast_event(RigControlEvent::SectionChanged { section_index: 0 });
            }
            RigControlCommand::NextSection => {
                let song_idx = *self.current_song_index.read().unwrap();
                let songs = self.cached_songs.read().unwrap();
                let max_section = songs
                    .get(song_idx)
                    .map(|s| s.section_names.len().saturating_sub(1))
                    .unwrap_or(0);
                drop(songs);
                let mut section_index = self.current_section_index.write().unwrap();
                *section_index = (*section_index + 1).min(max_section);
                let index = *section_index;
                drop(section_index);
                self.broadcast_event(RigControlEvent::SectionChanged {
                    section_index: index,
                });
            }
            RigControlCommand::PreviousSection => {
                let mut section_index = self.current_section_index.write().unwrap();
                *section_index = section_index.saturating_sub(1);
                let index = *section_index;
                drop(section_index);
                self.broadcast_event(RigControlEvent::SectionChanged {
                    section_index: index,
                });
            }
            RigControlCommand::SelectPreset { preset_id } => {
                let presets = self.cached_presets.read().unwrap();
                if let Some(preset) = presets.iter().find(|p| p.id == preset_id) {
                    let preset = preset.clone();
                    drop(presets);
                    *self.current_preset.write().unwrap() = Some(preset.clone());
                    self.broadcast_event(RigControlEvent::PresetSelected { preset });
                }
            }
            // Engine commands are no-ops in standalone mode
            _ => {
                tracing::debug!("StandaloneRigControlService: ignoring engine command {cmd:?}");
            }
        }
    }

    // ── Subscriptions ────────────────────────────────────────────────

    async fn subscribe(&self, _cx: &Context, events: Tx<RigControlEvent>) {
        *self.event_subscriber.write().unwrap() = Some(Arc::new(events));
    }

    async fn subscribe_slots(
        &self,
        _cx: &Context,
        _module_types: Vec<ModuleType>,
        _states: Tx<SlotStateInfo>,
    ) {
        // No slot broadcasting in standalone mode
    }
}
