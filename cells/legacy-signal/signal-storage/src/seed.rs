//! Database seeding — populates the DB with default data on first launch.
//!
//! Seed data is defined declaratively using `SeedPreset`, `SeedProfile`,
//! and `SeedSong` structs. Each instrument type has its own `seed_*_defaults()`
//! function that builds the full data set per the user spec.
//!
//! Module presets (for the signal chain) still come from
//! [`MockRigControlService::with_guitar_defaults()`] — the mock service is
//! the source of truth for Klon / Deluxe Reverb etc.

use chrono::Utc;
use sea_orm::{ActiveModelTrait, DatabaseConnection, Set};
use std::collections::HashMap;
use uuid::Uuid;

use signal::service::MockRigControlService;
use signal_proto::defaults::blocks::dummy;
use signal_proto::defaults::modules::dummy as module_dummy;
use signal_proto::preset::ModulePreset;

use crate::entities::{
    module_preset_entity, module_snapshot, performance_song, preset as preset_entity,
    profile as profile_entity, scene_template, snapshot as snapshot_entity, song_scene,
};
use crate::error::StorageResult;
use crate::facet_bridge;

// ── Declarative Seed Types ───────────────────────────────────────────────

/// A rig preset with its child scenes.
struct SeedPreset {
    name: &'static str,
    description: Option<&'static str>,
    category: serde_json::Value,
    tags: &'static [&'static str],
    instrument: &'static str,
    scenes: &'static [SeedScene],
}

/// A scene (snapshot) within a preset.
#[allow(dead_code)]
struct SeedScene {
    name: &'static str,
    is_default: bool,
    /// Scene-level tags (stored for future tag inheritance queries).
    tags: &'static [&'static str],
}

/// A profile with its patches.
struct SeedProfileDef {
    name: &'static str,
    tags: &'static [&'static str],
    instrument: &'static str,
    patches: &'static [SeedPatch],
}

/// A patch (scene template) within a profile.
struct SeedPatch {
    name: &'static str,
    /// "PresetName / SceneName" — used to look up IDs from the seed presets.
    preset_scene: &'static str,
    /// Whether this is the default patch for the profile (for future UI use).
    #[allow(dead_code)]
    is_default: bool,
}

/// A song with sections.
struct SeedSongDef {
    name: &'static str,
    artist: Option<&'static str>,
    tags: &'static [&'static str],
    instrument: &'static str,
    sections: &'static [SeedSection],
}

/// A section within a song.
struct SeedSection {
    name: &'static str,
    /// "PresetName / SceneName" — used to look up IDs from the seed presets.
    preset_scene: &'static str,
    /// Whether this is the default section for the song.
    is_default: bool,
}

/// Tracks generated IDs for linking presets → profiles → songs.
struct SeedIds {
    /// "PresetName" → preset UUID
    presets: HashMap<&'static str, Uuid>,
    /// "PresetName / SceneName" → snapshot UUID
    scenes: HashMap<String, Uuid>,
}

impl SeedIds {
    fn new() -> Self {
        Self {
            presets: HashMap::new(),
            scenes: HashMap::new(),
        }
    }

    fn preset_id(&self, name: &str) -> Uuid {
        self.presets.get(name).copied().unwrap_or(Uuid::nil())
    }

    fn scene_id(&self, key: &str) -> Option<Uuid> {
        self.scenes.get(key).copied()
    }
}

// ── Top-level Entry Point ────────────────────────────────────────────────

/// Seed the database with defaults if it's empty.
///
/// Uses incremental seeding: each category is checked independently so
/// new seed data (e.g. dummy block/module presets) gets added even if
/// the DB was created before those features existed.
///
/// Returns `true` if any seeding was performed, `false` if fully up-to-date.
pub async fn seed_if_empty(db: &DatabaseConnection) -> StorageResult<bool> {
    let existing_module_presets = crate::module_repo::list_module_presets(db, None).await?;
    let existing_block_presets = crate::block_repo::list_block_presets(db, None).await?;
    let mut seeded = false;

    // Full seed: no module presets means fresh DB
    if existing_module_presets.is_empty() {
        seed_from_defaults(db).await?;
        seeded = true;
    } else {
        tracing::debug!(
            "Core data already seeded ({} module presets)",
            existing_module_presets.len()
        );

        // Incremental: seed dummy block presets if missing
        if existing_block_presets.is_empty() {
            let dummy_blocks = dummy::all_dummy_blocks();
            seed_dummy_block_presets(db, &dummy_blocks).await?;
            tracing::info!(
                "Incremental seed: added {} dummy block presets",
                dummy_blocks.len()
            );
            seeded = true;
        }

        // Incremental: seed dummy module presets if missing.
        let has_dummy_modules = existing_module_presets
            .iter()
            .any(|p| p.name.starts_with("Dummy "));
        if !has_dummy_modules {
            let dummy_modules = module_dummy::all_dummy_module_presets();
            seed_dummy_module_presets(db, &dummy_modules).await?;
            tracing::info!(
                "Incremental seed: added {} dummy module presets",
                dummy_modules.len()
            );
            seeded = true;
        }
    }

    if !seeded {
        tracing::debug!("Database fully seeded, nothing to do");
    }
    Ok(seeded)
}

/// Seed all core data.
///
/// Module presets (Klon, Deluxe Reverb) come from the mock service.
/// Rig presets, profiles, and songs are seeded directly to DB per the spec.
async fn seed_from_defaults(db: &DatabaseConnection) -> StorageResult<()> {
    // ── Module presets from mock service ──────────────────────────
    let service = MockRigControlService::with_guitar_defaults();
    let data = service.data();
    let module_presets: Vec<_> = data.store.module_presets().values().cloned().collect();
    seed_module_presets(db, &module_presets).await?;

    // ── Dummy block + module presets ─────────────────────────────
    let dummy_blocks = dummy::all_dummy_blocks();
    seed_dummy_block_presets(db, &dummy_blocks).await?;
    let dummy_modules = module_dummy::all_dummy_module_presets();
    seed_dummy_module_presets(db, &dummy_modules).await?;

    // ── Instrument-specific seed data ────────────────────────────
    seed_guitar_defaults(db).await?;
    seed_bass_defaults(db).await?;

    tracing::info!(
        "Seeded DB: {} module presets, {} dummy block presets, {} dummy module presets, guitar + bass rig data",
        module_presets.len(),
        dummy_blocks.len(),
        dummy_modules.len(),
    );
    Ok(())
}

// ── Generic Seed Helpers ─────────────────────────────────────────────────

/// Insert rig presets and their scenes into the DB, returning tracked IDs.
async fn seed_rig_presets(
    db: &DatabaseConnection,
    presets: &[SeedPreset],
) -> StorageResult<SeedIds> {
    let mut ids = SeedIds::new();
    let now = Utc::now().fixed_offset();

    for preset in presets {
        let preset_id = Uuid::new_v4();
        ids.presets.insert(preset.name, preset_id);

        let tags_json = serde_json::json!(preset.tags);

        preset_entity::ActiveModel {
            id: Set(preset_id),
            name: Set(preset.name.to_string()),
            description: Set(preset.description.map(String::from)),
            author_id: Set(None),
            category: Set(preset.category.clone()),
            tags: Set(tags_json),
            data: Set(serde_json::json!({})),
            is_public: Set(false),
            instrument_type: Set(preset.instrument.to_string()),
            is_deleted: Set(false),
            is_favorite: Set(false),
            is_template: Set(true),
            version: Set(1),
            created_at: Set(now),
            updated_at: Set(now),
        }
        .insert(db)
        .await?;

        // If no scenes defined, create a single "Default" scene
        let scenes: Vec<SeedScene> = if preset.scenes.is_empty() {
            vec![SeedScene {
                name: "Default",
                is_default: true,
                tags: &[],
            }]
        } else {
            // Can't move out of the slice — just use it directly below
            Vec::new() // unused, see branch below
        };

        let scenes_to_insert = if preset.scenes.is_empty() {
            &scenes[..]
        } else {
            preset.scenes
        };

        for scene in scenes_to_insert {
            let scene_id = Uuid::new_v4();
            let key = format!("{} / {}", preset.name, scene.name);
            ids.scenes.insert(key, scene_id);

            snapshot_entity::ActiveModel {
                id: Set(scene_id),
                preset_id: Set(preset_id),
                name: Set(scene.name.to_string()),
                data: Set(serde_json::json!({})),
                is_default: Set(scene.is_default),
                created_at: Set(now),
                updated_at: Set(now),
            }
            .insert(db)
            .await?;
        }
    }

    Ok(ids)
}

/// Insert profiles with patches into the DB.
async fn seed_rig_profiles(
    db: &DatabaseConnection,
    profiles: &[SeedProfileDef],
    ids: &SeedIds,
) -> StorageResult<()> {
    let now = Utc::now().fixed_offset();

    for profile in profiles {
        let profile_id = Uuid::new_v4();
        let tags_json = serde_json::json!(profile.tags);

        profile_entity::ActiveModel {
            id: Set(profile_id),
            name: Set(profile.name.to_string()),
            rig_id: Set(Uuid::nil()),
            description: Set(None),
            tags: Set(tags_json),
            metadata: Set(serde_json::json!({"instrument": profile.instrument})),
            instrument_type: Set(profile.instrument.to_string()),
            default_scene_template_id: Set(None),
            is_template: Set(true),
            is_deleted: Set(false),
            created_at: Set(now),
            updated_at: Set(now),
        }
        .insert(db)
        .await?;

        for (i, patch) in profile.patches.iter().enumerate() {
            let (preset_uuid, snapshot_uuid) = resolve_preset_scene(patch.preset_scene, ids);

            scene_template::ActiveModel {
                id: Set(Uuid::new_v4()),
                profile_id: Set(profile_id),
                name: Set(patch.name.to_string()),
                preset_id: Set(preset_uuid),
                snapshot_id: Set(snapshot_uuid),
                module_overrides: Set(serde_json::json!({})),
                block_overrides: Set(serde_json::json!([])),
                parameter_state: Set(serde_json::json!({})),
                sort_order: Set(i as i32),
                tags: Set(serde_json::json!([])),
                created_at: Set(now),
                updated_at: Set(now),
            }
            .insert(db)
            .await?;
        }
    }
    Ok(())
}

/// Insert songs with sections into the DB.
async fn seed_rig_songs(
    db: &DatabaseConnection,
    songs: &[SeedSongDef],
    ids: &SeedIds,
) -> StorageResult<()> {
    let now = Utc::now().fixed_offset();

    for song in songs {
        let song_id = Uuid::new_v4();
        let tags_json = serde_json::json!(song.tags);

        performance_song::ActiveModel {
            id: Set(song_id),
            name: Set(song.name.to_string()),
            artist: Set(song.artist.map(String::from)),
            auto_advance: Set(false),
            linked_song_id: Set(None),
            instrument_type: Set(song.instrument.to_string()),
            module_overrides: Set(serde_json::json!([])),
            tags: Set(tags_json),
            is_template: Set(true),
            is_deleted: Set(false),
            created_at: Set(now),
            updated_at: Set(now),
        }
        .insert(db)
        .await?;

        for (i, section) in song.sections.iter().enumerate() {
            let (preset_uuid, snapshot_uuid) = resolve_preset_scene(section.preset_scene, ids);

            song_scene::ActiveModel {
                id: Set(Uuid::new_v4()),
                song_id: Set(song_id),
                name: Set(section.name.to_string()),
                preset_id: Set(preset_uuid),
                snapshot_id: Set(snapshot_uuid),
                transition: Set(serde_json::json!({})),
                midi_triggers: Set(serde_json::json!([])),
                module_overrides: Set(serde_json::json!({})),
                block_overrides: Set(serde_json::json!([])),
                sort_order: Set(i as i32),
                is_default: Set(section.is_default),
                tags: Set(serde_json::json!([])),
                created_at: Set(now),
                updated_at: Set(now),
            }
            .insert(db)
            .await?;
        }
    }
    Ok(())
}

/// Parse "PresetName / SceneName" into (preset_uuid, Option<snapshot_uuid>).
fn resolve_preset_scene(key: &str, ids: &SeedIds) -> (Uuid, Option<Uuid>) {
    let preset_name = key.split(" / ").next().unwrap_or(key);
    let preset_uuid = ids.preset_id(preset_name);
    let snapshot_uuid = ids.scene_id(key);
    (preset_uuid, snapshot_uuid)
}

// ── Guitar Seed Data ─────────────────────────────────────────────────────

/// Seed guitar rig: 3 presets, 3 profiles, 3 songs per user spec.
async fn seed_guitar_defaults(db: &DatabaseConnection) -> StorageResult<()> {
    use signal_proto::category::{BaseTone, PresetCategory};

    let clean_cat = facet_bridge::to_json_value(&PresetCategory::Generic {
        base_tone: BaseTone::Clean,
    })?;
    let drive_cat = facet_bridge::to_json_value(&PresetCategory::Generic {
        base_tone: BaseTone::Drive,
    })?;

    // ── Rig Presets ──────────────────────────────────────────────
    let presets = [
        SeedPreset {
            name: "Vox AC30 / Dream Ruby",
            description: Some("Vintage AC30 tone with Dream Ruby reverb"),
            category: clean_cat.clone(),
            tags: &["clean", "ambient", "vintage"],
            instrument: "guitar",
            scenes: &[SeedScene {
                name: "Default",
                is_default: true,
                tags: &[],
            }],
        },
        SeedPreset {
            name: "Clean Jazz Chime",
            description: Some("Shimmering clean jazz tone with multiple voicings"),
            category: clean_cat,
            tags: &["clean", "jazz", "chimey"],
            instrument: "guitar",
            scenes: &[
                SeedScene {
                    name: "Dry",
                    is_default: true,
                    tags: &["dry"],
                },
                SeedScene {
                    name: "Bright",
                    is_default: false,
                    tags: &["bright"],
                },
                SeedScene {
                    name: "Dark",
                    is_default: false,
                    tags: &["dark"],
                },
            ],
        },
        SeedPreset {
            name: "Heavy 5150",
            description: Some("High-gain 5150 for metal and hard rock"),
            category: drive_cat,
            tags: &["metal", "5150"],
            instrument: "guitar",
            scenes: &[
                SeedScene {
                    name: "Chug",
                    is_default: true,
                    tags: &["chug"],
                },
                SeedScene {
                    name: "Extreme",
                    is_default: false,
                    tags: &["high gain"],
                },
                SeedScene {
                    name: "Nothin to Gain'",
                    is_default: false,
                    tags: &["high gain"],
                },
            ],
        },
    ];

    let ids = seed_rig_presets(db, &presets).await?;

    // ── Profiles ─────────────────────────────────────────────────
    let profiles = [
        SeedProfileDef {
            name: "Worship",
            tags: &["guitar", "worship"],
            instrument: "guitar",
            patches: &[
                SeedPatch {
                    name: "Clean",
                    preset_scene: "Vox AC30 / Dream Ruby / Default",
                    is_default: true,
                },
                SeedPatch {
                    name: "Crunch",
                    preset_scene: "Clean Jazz Chime / Dry",
                    is_default: false,
                },
                SeedPatch {
                    name: "Drive",
                    preset_scene: "Heavy 5150 / Chug",
                    is_default: false,
                },
                SeedPatch {
                    name: "Lead",
                    preset_scene: "Heavy 5150 / Extreme",
                    is_default: false,
                },
                SeedPatch {
                    name: "Ambient",
                    preset_scene: "Vox AC30 / Dream Ruby / Default",
                    is_default: false,
                },
                SeedPatch {
                    name: "Tremolo",
                    preset_scene: "Clean Jazz Chime / Bright",
                    is_default: false,
                },
                SeedPatch {
                    name: "Delay",
                    preset_scene: "Clean Jazz Chime / Dark",
                    is_default: false,
                },
                SeedPatch {
                    name: "Solo",
                    preset_scene: "Heavy 5150 / Extreme",
                    is_default: false,
                },
            ],
        },
        SeedProfileDef {
            name: "Blues",
            tags: &["guitar", "blues"],
            instrument: "guitar",
            patches: &[
                SeedPatch {
                    name: "Clean",
                    preset_scene: "Clean Jazz Chime / Bright",
                    is_default: false,
                },
                SeedPatch {
                    name: "Crunch",
                    preset_scene: "Clean Jazz Chime / Dry",
                    is_default: true,
                },
                SeedPatch {
                    name: "Drive",
                    preset_scene: "Heavy 5150 / Chug",
                    is_default: false,
                },
                SeedPatch {
                    name: "Lead",
                    preset_scene: "Heavy 5150 / Extreme",
                    is_default: false,
                },
                SeedPatch {
                    name: "Funk",
                    preset_scene: "Clean Jazz Chime / Bright",
                    is_default: false,
                },
                SeedPatch {
                    name: "Q-Tron",
                    preset_scene: "Clean Jazz Chime / Dark",
                    is_default: false,
                },
                SeedPatch {
                    name: "Roomy",
                    preset_scene: "Vox AC30 / Dream Ruby / Default",
                    is_default: false,
                },
                SeedPatch {
                    name: "Solo",
                    preset_scene: "Heavy 5150 / Extreme",
                    is_default: false,
                },
            ],
        },
        SeedProfileDef {
            name: "Rock",
            tags: &["guitar", "rock"],
            instrument: "guitar",
            patches: &[
                SeedPatch {
                    name: "Clean",
                    preset_scene: "Clean Jazz Chime / Bright",
                    is_default: false,
                },
                SeedPatch {
                    name: "Crunch",
                    preset_scene: "Clean Jazz Chime / Dry",
                    is_default: false,
                },
                SeedPatch {
                    name: "Drive",
                    preset_scene: "Heavy 5150 / Chug",
                    is_default: true,
                },
                SeedPatch {
                    name: "Lead",
                    preset_scene: "Heavy 5150 / Extreme",
                    is_default: false,
                },
                SeedPatch {
                    name: "Ambient",
                    preset_scene: "Vox AC30 / Dream Ruby / Default",
                    is_default: false,
                },
                SeedPatch {
                    name: "Phaser",
                    preset_scene: "Clean Jazz Chime / Bright",
                    is_default: false,
                },
                SeedPatch {
                    name: "DLY Lead",
                    preset_scene: "Heavy 5150 / Nothin to Gain'",
                    is_default: false,
                },
                SeedPatch {
                    name: "Solo",
                    preset_scene: "Heavy 5150 / Extreme",
                    is_default: false,
                },
            ],
        },
    ];

    seed_rig_profiles(db, &profiles, &ids).await?;

    // ── Songs ────────────────────────────────────────────────────
    // Each song's sections reference preset/scenes from one of the
    // profiles above, so we can verify the full chain works:
    //   Song Section → Preset/Scene ← Profile Patch
    let songs = [
        // Guitar Song 1 — 3 sections, drawn from Worship profile patches
        SeedSongDef {
            name: "Guitar Song 1",
            artist: None,
            tags: &["guitar", "worship"],
            instrument: "guitar",
            sections: &[
                SeedSection {
                    name: "Intro (Clean)",
                    preset_scene: "Vox AC30 / Dream Ruby / Default", // Worship → Clean
                    is_default: true,
                },
                SeedSection {
                    name: "Verse (Crunch)",
                    preset_scene: "Clean Jazz Chime / Dry", // Worship → Crunch
                    is_default: false,
                },
                SeedSection {
                    name: "Chorus (Lead)",
                    preset_scene: "Heavy 5150 / Extreme", // Worship → Lead
                    is_default: false,
                },
            ],
        },
        // Guitar Song 2 — 5 sections, drawn from Blues profile patches
        SeedSongDef {
            name: "Guitar Song 2",
            artist: None,
            tags: &["guitar", "blues"],
            instrument: "guitar",
            sections: &[
                SeedSection {
                    name: "Intro (Clean)",
                    preset_scene: "Clean Jazz Chime / Bright", // Blues → Clean
                    is_default: false,
                },
                SeedSection {
                    name: "Verse (Crunch)",
                    preset_scene: "Clean Jazz Chime / Dry", // Blues → Crunch
                    is_default: true,
                },
                SeedSection {
                    name: "Chorus (Drive)",
                    preset_scene: "Heavy 5150 / Chug", // Blues → Drive
                    is_default: false,
                },
                SeedSection {
                    name: "Bridge (Funk)",
                    preset_scene: "Clean Jazz Chime / Bright", // Blues → Funk
                    is_default: false,
                },
                SeedSection {
                    name: "Outro (Solo)",
                    preset_scene: "Heavy 5150 / Extreme", // Blues → Solo
                    is_default: false,
                },
            ],
        },
        // Guitar Song 3 — 4 sections, drawn from Rock profile patches
        SeedSongDef {
            name: "Guitar Song 3",
            artist: None,
            tags: &["guitar", "rock"],
            instrument: "guitar",
            sections: &[
                SeedSection {
                    name: "Intro (Clean)",
                    preset_scene: "Clean Jazz Chime / Bright", // Rock → Clean
                    is_default: false,
                },
                SeedSection {
                    name: "Verse (Drive)",
                    preset_scene: "Heavy 5150 / Chug", // Rock → Drive
                    is_default: true,
                },
                SeedSection {
                    name: "Bridge (Ambient)",
                    preset_scene: "Vox AC30 / Dream Ruby / Default", // Rock → Ambient
                    is_default: false,
                },
                SeedSection {
                    name: "Solo (DLY Lead)",
                    preset_scene: "Heavy 5150 / Nothin to Gain'", // Rock → DLY Lead
                    is_default: false,
                },
            ],
        },
    ];

    seed_rig_songs(db, &songs, &ids).await?;

    tracing::info!(
        "Seeded guitar rig: {} presets, {} profiles, {} songs",
        presets.len(),
        profiles.len(),
        songs.len(),
    );
    Ok(())
}

// ── Bass Seed Data ───────────────────────────────────────────────────────

/// Seed bass rig: 3 presets, 1 profile, 1 song per user spec.
async fn seed_bass_defaults(db: &DatabaseConnection) -> StorageResult<()> {
    use signal_proto::category::{BaseTone, PresetCategory};

    let clean_cat = facet_bridge::to_json_value(&PresetCategory::Generic {
        base_tone: BaseTone::Clean,
    })?;
    let drive_cat = facet_bridge::to_json_value(&PresetCategory::Generic {
        base_tone: BaseTone::Drive,
    })?;
    let ambient_cat = facet_bridge::to_json_value(&PresetCategory::Generic {
        base_tone: BaseTone::Clean,
    })?;

    // ── Rig Presets ──────────────────────────────────────────────
    let presets = [
        SeedPreset {
            name: "Parallax",
            description: Some("Bass rig preset: Parallax"),
            category: clean_cat,
            tags: &["bass", "clean", "deep"],
            instrument: "bass",
            scenes: &[
                SeedScene {
                    name: "Clean",
                    is_default: true,
                    tags: &["clean"],
                },
                SeedScene {
                    name: "Drive",
                    is_default: false,
                    tags: &["drive"],
                },
                SeedScene {
                    name: "DI",
                    is_default: false,
                    tags: &["di"],
                },
                SeedScene {
                    name: "Sledgehammer",
                    is_default: false,
                    tags: &["aggressive"],
                },
            ],
        },
        SeedPreset {
            name: "Amped",
            description: Some("Bass rig preset: Amped"),
            category: drive_cat,
            tags: &["bass", "drive"],
            instrument: "bass",
            scenes: &[
                SeedScene {
                    name: "Clean",
                    is_default: true,
                    tags: &["clean"],
                },
                SeedScene {
                    name: "Crunch",
                    is_default: false,
                    tags: &["crunch"],
                },
                SeedScene {
                    name: "Distort",
                    is_default: false,
                    tags: &["distort"],
                },
            ],
        },
        SeedPreset {
            name: "Ambient",
            description: Some("Bass rig preset: Ambient"),
            category: ambient_cat,
            tags: &["bass", "ambient"],
            instrument: "bass",
            scenes: &[
                SeedScene {
                    name: "Vibey",
                    is_default: true,
                    tags: &["vibey"],
                },
                SeedScene {
                    name: "Flanger",
                    is_default: false,
                    tags: &["flanger"],
                },
                SeedScene {
                    name: "Chorus",
                    is_default: false,
                    tags: &["chorus"],
                },
            ],
        },
    ];

    let ids = seed_rig_presets(db, &presets).await?;

    // ── Profile: All-Around Live Rig ─────────────────────────────
    let profiles = [SeedProfileDef {
        name: "All-Around Live Rig",
        tags: &["bass", "live"],
        instrument: "bass",
        patches: &[
            SeedPatch {
                name: "Clean",
                preset_scene: "Parallax / Clean",
                is_default: true,
            },
            SeedPatch {
                name: "Crunch",
                preset_scene: "Amped / Crunch",
                is_default: false,
            },
            SeedPatch {
                name: "Drive",
                preset_scene: "Parallax / Drive",
                is_default: false,
            },
            SeedPatch {
                name: "Fuzz Octave",
                preset_scene: "Amped / Distort",
                is_default: false,
            },
            SeedPatch {
                name: "Ambient",
                preset_scene: "Ambient / Vibey",
                is_default: false,
            },
            SeedPatch {
                name: "Sub",
                preset_scene: "Parallax / DI",
                is_default: false,
            },
            SeedPatch {
                name: "Chorus Drive",
                preset_scene: "Ambient / Chorus",
                is_default: false,
            },
            SeedPatch {
                name: "Fuzz",
                preset_scene: "Amped / Distort",
                is_default: false,
            },
            SeedPatch {
                name: "DI",
                preset_scene: "Parallax / DI",
                is_default: false,
            },
        ],
    }];

    seed_rig_profiles(db, &profiles, &ids).await?;

    // ── Song: Bass Song ──────────────────────────────────────────
    let songs = [SeedSongDef {
        name: "Bass Song",
        artist: None,
        tags: &["bass"],
        instrument: "bass",
        sections: &[
            SeedSection {
                name: "Intro",
                preset_scene: "Parallax / Clean",
                is_default: true,
            },
            SeedSection {
                name: "Verse",
                preset_scene: "Parallax / Drive",
                is_default: false,
            },
            SeedSection {
                name: "Chorus",
                preset_scene: "Amped / Crunch",
                is_default: false,
            },
            SeedSection {
                name: "Bridge",
                preset_scene: "Ambient / Vibey",
                is_default: false,
            },
        ],
    }];

    seed_rig_songs(db, &songs, &ids).await?;

    tracing::info!(
        "Seeded bass rig: {} presets, {} profiles, {} songs",
        presets.len(),
        profiles.len(),
        songs.len(),
    );
    Ok(())
}

// ── Module Presets ────────────────────────────────────────────────────────

async fn seed_module_presets(
    db: &DatabaseConnection,
    presets: &[ModulePreset],
) -> StorageResult<()> {
    for mp in presets {
        let now = Utc::now().fixed_offset();
        let snapshot_json = facet_bridge::to_json_value(&mp.snapshot)?;
        let tags_json = facet_bridge::to_json_value(&mp.metadata.tags)?;

        module_preset_entity::ActiveModel {
            id: Set(mp.id.into()),
            name: Set(mp.metadata.name.clone()),
            module_type: Set(mp.module_type.display_name().to_string()),
            description: Set(mp.metadata.description.clone()),
            blocks: Set(snapshot_json.clone()),
            macros: Set(serde_json::json!([])),
            tags: Set(tags_json),
            is_template: Set(true),
            is_deleted: Set(false),
            created_at: Set(now),
            updated_at: Set(now),
        }
        .insert(db)
        .await?;

        seed_module_snapshot(db, mp.id.into(), &mp.snapshot, true).await?;
    }
    Ok(())
}

async fn seed_module_snapshot(
    db: &DatabaseConnection,
    module_preset_id: Uuid,
    snapshot: &signal_proto::snapshot::ModuleSnapshot,
    is_default: bool,
) -> StorageResult<()> {
    let now = Utc::now().fixed_offset();
    let block_refs_json = facet_bridge::to_json_value(&snapshot.block_snapshots)?;
    let tags_json = facet_bridge::to_json_value(&snapshot.tags)?;

    module_snapshot::ActiveModel {
        id: Set(snapshot.id.into()),
        module_preset_id: Set(module_preset_id),
        name: Set(snapshot.name.clone()),
        block_overrides: Set(block_refs_json),
        is_default: Set(is_default),
        tags: Set(tags_json),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;
    Ok(())
}

// ── Dummy Block Presets + Snapshots ──────────────────────────────────────

async fn seed_dummy_block_presets(
    db: &DatabaseConnection,
    blocks: &[signal_proto::block::Block],
) -> StorageResult<()> {
    for block in blocks {
        let plugin_id_json = facet_bridge::to_json_value(&block.plugin_id)?;
        let tags_json = serde_json::json!([]);
        let block_type_str = block.block_type.display_name();

        let preset_id = crate::block_repo::create_block_preset(
            db,
            &block.name,
            block_type_str,
            Some(plugin_id_json),
            None,
            block.description.as_deref(),
            tags_json,
        )
        .await?;

        for (i, name) in dummy::DUMMY_SNAPSHOT_NAMES.iter().enumerate() {
            let is_default = i == 0;
            let params = serde_json::json!({});
            crate::block_repo::create_block_snapshot(db, preset_id, name, params, None, is_default)
                .await?;
        }
    }
    Ok(())
}

// ── Dummy Module Presets + Snapshots ────────────────────────────────────

async fn seed_dummy_module_presets(
    db: &DatabaseConnection,
    modules: &[module_dummy::DummyModulePreset],
) -> StorageResult<()> {
    for module in modules {
        let module_type_str = module.module_type.display_name();
        let tags_json = serde_json::json!([]);

        let preset_id = crate::module_repo::create_module_preset(
            db,
            &module.name,
            module_type_str,
            Some(&module.description),
            serde_json::json!([]),
            serde_json::json!([]),
            tags_json,
        )
        .await?;

        for (i, name) in module_dummy::DUMMY_SNAPSHOT_NAMES.iter().enumerate() {
            let is_default = i == 0;
            let overrides = serde_json::json!([]);
            let snap_tags = serde_json::json!([]);
            crate::module_repo::create_module_snapshot(
                db, preset_id, name, overrides, is_default, snap_tags,
            )
            .await?;
        }
    }
    Ok(())
}

// ── Tests ────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;

    async fn test_db() -> DatabaseConnection {
        let db = Database::connect("sqlite::memory:").await.unwrap();
        crate::Migrator::up(&db, None).await.unwrap();
        db
    }

    #[tokio::test]
    async fn seed_defaults_inserts_all_data() {
        let db = test_db().await;

        let seeded = seed_if_empty(&db).await.unwrap();
        assert!(seeded, "should seed on empty DB");

        // ── Module presets (2 from guitar mock + 19 dummy) ───────
        let module_presets = crate::module_repo::list_module_presets(&db, None)
            .await
            .unwrap();
        assert!(
            module_presets.len() >= 2,
            "expected at least 2 module presets, got {}",
            module_presets.len()
        );

        let first_mp = &module_presets[0];
        let snapshots = crate::module_repo::list_module_snapshots(&db, first_mp.id)
            .await
            .unwrap();
        assert!(
            !snapshots.is_empty(),
            "first module preset should have snapshots"
        );

        // ── Rig presets (3 guitar + 3 bass = 6) ─────────────────
        let presets = crate::preset_repo::list_presets(&db, true).await.unwrap();
        assert_eq!(
            presets.len(),
            6,
            "expected 6 presets (3 guitar + 3 bass), got {}",
            presets.len()
        );

        // Verify guitar presets exist by name
        let guitar_preset_names: Vec<&str> = presets
            .iter()
            .filter(|p| {
                p.name == "Vox AC30 / Dream Ruby"
                    || p.name == "Clean Jazz Chime"
                    || p.name == "Heavy 5150"
            })
            .map(|p| p.name.as_str())
            .collect();
        assert_eq!(guitar_preset_names.len(), 3, "expected 3 guitar presets");

        // Verify Clean Jazz Chime has 3 scenes
        let jazz_chime = presets
            .iter()
            .find(|p| p.name == "Clean Jazz Chime")
            .unwrap();
        let jazz_snaps = crate::preset_repo::list_preset_snapshots(&db, jazz_chime.id)
            .await
            .unwrap();
        assert_eq!(jazz_snaps.len(), 3, "Clean Jazz Chime should have 3 scenes");
        // First scene (Dry) should be default
        assert!(jazz_snaps.iter().any(|s| s.name == "Dry" && s.is_default));

        // Verify AC30 has 1 "Default" scene
        let ac30 = presets
            .iter()
            .find(|p| p.name == "Vox AC30 / Dream Ruby")
            .unwrap();
        let ac30_snaps = crate::preset_repo::list_preset_snapshots(&db, ac30.id)
            .await
            .unwrap();
        assert_eq!(ac30_snaps.len(), 1, "AC30 should have 1 scene");
        assert_eq!(ac30_snaps[0].name, "Default");
        assert!(ac30_snaps[0].is_default);

        // Verify bass presets exist
        let bass_names: Vec<&str> = presets
            .iter()
            .filter(|p| p.name == "Parallax" || p.name == "Amped" || p.name == "Ambient")
            .map(|p| p.name.as_str())
            .collect();
        assert_eq!(bass_names.len(), 3, "expected 3 bass presets");

        // Verify Parallax has 4 scenes with first default
        let parallax = presets.iter().find(|p| p.name == "Parallax").unwrap();
        let par_snaps = crate::preset_repo::list_preset_snapshots(&db, parallax.id)
            .await
            .unwrap();
        assert_eq!(par_snaps.len(), 4, "Parallax should have 4 scenes");
        assert!(par_snaps.iter().any(|s| s.name == "Clean" && s.is_default));

        // ── Profiles (3 guitar + 1 bass = 4) ────────────────────
        let profiles = crate::profile_repo::list_profiles(&db).await.unwrap();
        assert_eq!(profiles.len(), 4, "expected 4 profiles (3 guitar + 1 bass)");

        // Guitar profiles have 8 patches each
        for name in &["Worship", "Blues", "Rock"] {
            let prof = profiles.iter().find(|p| p.name == *name).unwrap();
            let patches = crate::profile_repo::list_scene_templates(&db, prof.id)
                .await
                .unwrap();
            assert_eq!(patches.len(), 8, "{name} profile should have 8 patches");
        }

        // Bass profile has 9 patches
        let bass_prof = profiles
            .iter()
            .find(|p| p.name == "All-Around Live Rig")
            .unwrap();
        let bass_patches = crate::profile_repo::list_scene_templates(&db, bass_prof.id)
            .await
            .unwrap();
        assert_eq!(
            bass_patches.len(),
            9,
            "All-Around Live Rig should have 9 patches"
        );

        // ── Songs (3 guitar + 1 bass = 4) ────────────────────────
        let songs = crate::song_repo::list_songs(&db).await.unwrap();
        assert_eq!(songs.len(), 4, "expected 4 songs (3 guitar + 1 bass)");

        // Guitar Song 1 — 3 sections from Worship patches
        let song1 = songs.iter().find(|s| s.name == "Guitar Song 1").unwrap();
        assert_eq!(song1.artist, None);
        let song1_sections = crate::song_repo::list_song_scenes(&db, song1.id)
            .await
            .unwrap();
        assert_eq!(
            song1_sections.len(),
            3,
            "Guitar Song 1 should have 3 sections"
        );

        // Guitar Song 2 — 5 sections from Blues patches
        let song2 = songs.iter().find(|s| s.name == "Guitar Song 2").unwrap();
        let song2_sections = crate::song_repo::list_song_scenes(&db, song2.id)
            .await
            .unwrap();
        assert_eq!(
            song2_sections.len(),
            5,
            "Guitar Song 2 should have 5 sections"
        );

        // Guitar Song 3 — 4 sections from Rock patches
        let song3 = songs.iter().find(|s| s.name == "Guitar Song 3").unwrap();
        let song3_sections = crate::song_repo::list_song_scenes(&db, song3.id)
            .await
            .unwrap();
        assert_eq!(
            song3_sections.len(),
            4,
            "Guitar Song 3 should have 4 sections"
        );

        // Bass song
        let bass_song = songs.iter().find(|s| s.name == "Bass Song").unwrap();
        let bass_sections = crate::song_repo::list_song_scenes(&db, bass_song.id)
            .await
            .unwrap();
        assert_eq!(bass_sections.len(), 4, "Bass Song should have 4 sections");

        // ── Dummy block presets (33) ─────────────────────────────
        let block_presets = crate::block_repo::list_block_presets(&db, None)
            .await
            .unwrap();
        assert_eq!(block_presets.len(), 33, "expected 33 dummy block presets");

        for bp in &block_presets {
            let snaps = crate::block_repo::list_block_snapshots(&db, bp.id)
                .await
                .unwrap();
            assert_eq!(
                snaps.len(),
                4,
                "block preset '{}' should have 4 snapshots",
                bp.name
            );
            assert!(snaps[0].is_default, "first snapshot should be default");
        }

        // ── Dummy module presets (19) ────────────────────────────
        let dummy_module_presets: Vec<_> = module_presets
            .iter()
            .filter(|p| p.name.starts_with("Dummy "))
            .collect();
        assert_eq!(
            dummy_module_presets.len(),
            19,
            "expected 19 dummy module presets"
        );

        for dmp in &dummy_module_presets {
            let snaps = crate::module_repo::list_module_snapshots(&db, dmp.id)
                .await
                .unwrap();
            assert_eq!(
                snaps.len(),
                4,
                "dummy module preset '{}' should have 4 snapshots",
                dmp.name
            );
            assert!(
                snaps[0].is_default,
                "first module snapshot should be default"
            );
        }
    }

    #[tokio::test]
    async fn seed_is_idempotent() {
        let db = test_db().await;

        let first = seed_if_empty(&db).await.unwrap();
        assert!(first, "first seed should insert data");

        let second = seed_if_empty(&db).await.unwrap();
        assert!(!second, "second seed should be a no-op");

        // Counts should be unchanged
        let profiles = crate::profile_repo::list_profiles(&db).await.unwrap();
        assert_eq!(
            profiles.len(),
            4,
            "should still have 4 profiles (3 guitar + 1 bass)"
        );
    }
}
