//! `RigDirector` — multi-rig coordination and role assignment.
//!
//! The [`RigDirector`] manages multiple rigs, assigns roles (Lead, Background,
//! Custom) to rigs, and resolves which rig fills each role based on context
//! (global, per-song, per-scene) with priority ordering.

use std::fmt;

use facet::Facet;
use uuid::Uuid;

use crate::id::{ModuleId, PresetId, RigId, RoleId, SceneId, SongId};
use crate::module::{ModuleMacro, ModuleType};
use crate::normalized::NormalizedF64;
use crate::rig::{InstrumentType, Rig};

// ─────────────────────────────────────────────────────────────────────────────
// RoleType
// ─────────────────────────────────────────────────────────────────────────────

/// The category of a role within the director.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum RoleType {
    /// Primary / lead instrument.
    Lead = 0,
    /// Background or harmony part, with optional voice number.
    Background(Option<u8>) = 1,
    /// User-defined role.
    Custom = 2,
}

impl fmt::Display for RoleType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lead => f.write_str("Lead"),
            Self::Background(None) => f.write_str("Background"),
            Self::Background(Some(n)) => write!(f, "Background {n}"),
            Self::Custom => f.write_str("Custom"),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Role
// ─────────────────────────────────────────────────────────────────────────────

/// A named role that can be assigned to a rig.
#[derive(Debug, Clone, Facet)]
pub struct Role {
    pub id: RoleId,
    pub name: String,
    pub role_type: RoleType,
    pub default_preset_id: Option<PresetId>,
    pub color: Option<u32>,
    pub abbreviation: Option<String>,
}

impl Role {
    /// Create a new role with the given name and type.
    pub fn new(name: impl Into<String>, role_type: RoleType) -> Self {
        Self {
            id: RoleId::new(),
            name: name.into(),
            role_type,
            default_preset_id: None,
            color: None,
            abbreviation: None,
        }
    }

    /// Create a Lead role.
    pub fn lead(name: impl Into<String>) -> Self {
        Self::new(name, RoleType::Lead)
    }

    /// Create a Background role with an optional voice number.
    pub fn background(name: impl Into<String>, voice: Option<u8>) -> Self {
        Self::new(name, RoleType::Background(voice))
    }

    /// Create a Custom role.
    pub fn custom(name: impl Into<String>) -> Self {
        Self::new(name, RoleType::Custom)
    }

    /// Set the default preset for this role (builder pattern).
    #[must_use]
    pub fn with_preset(mut self, preset_id: PresetId) -> Self {
        self.default_preset_id = Some(preset_id);
        self
    }

    /// Set the color for this role (builder pattern).
    #[must_use]
    pub fn with_color(mut self, color: u32) -> Self {
        self.color = Some(color);
        self
    }

    /// Set the abbreviation for this role (builder pattern).
    #[must_use]
    pub fn with_abbreviation(mut self, abbrev: impl Into<String>) -> Self {
        self.abbreviation = Some(abbrev.into());
        self
    }

    /// Get the display abbreviation, falling back to the first 3 characters of the name.
    pub fn display_abbrev(&self) -> &str {
        self.abbreviation
            .as_deref()
            .unwrap_or_else(|| {
                let end = self.name.char_indices().nth(3).map_or(self.name.len(), |(i, _)| i);
                &self.name[..end]
            })
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// AssignmentContext
// ─────────────────────────────────────────────────────────────────────────────

/// The context in which a role assignment applies.
///
/// More specific contexts take priority over broader ones:
/// Scene > Song > Global.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum AssignmentContext {
    /// Applies everywhere unless overridden.
    Global = 0,
    /// Applies to a specific song.
    Song { song_id: SongId } = 1,
    /// Applies to a specific scene within a song.
    Scene { song_id: SongId, scene_id: SceneId } = 2,
}

impl AssignmentContext {
    /// Specificity priority (higher = more specific).
    pub fn priority(&self) -> u8 {
        match self {
            Self::Global => 0,
            Self::Song { .. } => 1,
            Self::Scene { .. } => 2,
        }
    }

    /// Whether this context applies to the given song and optional scene.
    pub fn applies_to(&self, song_id: SongId, scene_id: Option<SceneId>) -> bool {
        match self {
            Self::Global => true,
            Self::Song { song_id: sid } => *sid == song_id,
            Self::Scene {
                song_id: sid,
                scene_id: scid,
            } => *sid == song_id && scene_id == Some(*scid),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// RoleAssignment
// ─────────────────────────────────────────────────────────────────────────────

/// Assigns a rig to a role within a specific context.
#[derive(Debug, Clone, Facet)]
pub struct RoleAssignment {
    pub id: Uuid,
    pub role_id: RoleId,
    pub rig_id: RigId,
    pub context: AssignmentContext,
    pub priority: u8,
}

impl RoleAssignment {
    /// Create a global assignment.
    pub fn global(role_id: RoleId, rig_id: RigId) -> Self {
        Self {
            id: Uuid::new_v4(),
            role_id,
            rig_id,
            context: AssignmentContext::Global,
            priority: 0,
        }
    }

    /// Create a song-scoped assignment.
    pub fn for_song(role_id: RoleId, rig_id: RigId, song_id: SongId) -> Self {
        Self {
            id: Uuid::new_v4(),
            role_id,
            rig_id,
            context: AssignmentContext::Song { song_id },
            priority: 1,
        }
    }

    /// Create a scene-scoped assignment.
    pub fn for_scene(
        role_id: RoleId,
        rig_id: RigId,
        song_id: SongId,
        scene_id: SceneId,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            role_id,
            rig_id,
            context: AssignmentContext::Scene { song_id, scene_id },
            priority: 2,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// RigStatus
// ─────────────────────────────────────────────────────────────────────────────

/// Runtime status of a rig within the director.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum RigStatus {
    Active,
    Idle,
    Muted,
    Warning,
    Offline,
}

impl fmt::Display for RigStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Active => f.write_str("Active"),
            Self::Idle => f.write_str("Idle"),
            Self::Muted => f.write_str("Muted"),
            Self::Warning => f.write_str("Warning"),
            Self::Offline => f.write_str("Offline"),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ModuleState
// ─────────────────────────────────────────────────────────────────────────────

/// Snapshot of a module's runtime state.
#[derive(Debug, Clone, Facet)]
pub struct ModuleState {
    pub module_id: ModuleId,
    pub module_type: ModuleType,
    pub enabled: bool,
    pub level: NormalizedF64,
    pub macros: Vec<ModuleMacro>,
}

// ─────────────────────────────────────────────────────────────────────────────
// DirectorInfo
// ─────────────────────────────────────────────────────────────────────────────

/// Lightweight summary of the director's state.
#[derive(Debug, Clone, Facet)]
pub struct DirectorInfo {
    pub id: Uuid,
    pub name: String,
    pub rig_count: usize,
    pub active_rig_count: usize,
}

// ─────────────────────────────────────────────────────────────────────────────
// RigDirector
// ─────────────────────────────────────────────────────────────────────────────

/// Coordinates multiple rigs, role assignments, and a shared sends engine.
///
/// The director is the top-level container that manages which rig fills
/// which role at any given point in the performance (globally, per-song,
/// or per-scene).
#[derive(Debug, Clone, Facet)]
pub struct RigDirector {
    pub id: Uuid,
    pub name: String,
    pub rigs: Vec<Rig>,
    pub sends_engine: crate::module::Module,
    pub roles: Vec<Role>,
    pub role_assignments: Vec<RoleAssignment>,
}

impl RigDirector {
    /// Create a new director with a sends engine module.
    pub fn new(name: impl Into<String>, sends_engine: crate::module::Module) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            rigs: Vec::new(),
            sends_engine,
            roles: Vec::new(),
            role_assignments: Vec::new(),
        }
    }

    /// Add a rig to the director.
    pub fn add_rig(&mut self, rig: Rig) {
        self.rigs.push(rig);
    }

    /// Add a role to the director.
    pub fn add_role(&mut self, role: Role) {
        self.roles.push(role);
    }

    /// Add a role assignment.
    pub fn add_assignment(&mut self, assignment: RoleAssignment) {
        self.role_assignments.push(assignment);
    }

    /// Get a rig by ID.
    pub fn get_rig(&self, id: RigId) -> Option<&Rig> {
        self.rigs.iter().find(|r| r.id == id)
    }

    /// Get a mutable rig by ID.
    pub fn get_rig_mut(&mut self, id: RigId) -> Option<&mut Rig> {
        self.rigs.iter_mut().find(|r| r.id == id)
    }

    /// Get all rigs of a given instrument type.
    pub fn rigs_by_type(&self, instrument_type: &InstrumentType) -> Vec<&Rig> {
        self.rigs
            .iter()
            .filter(|r| &r.instrument_type == instrument_type)
            .collect()
    }

    /// Get a role by ID.
    pub fn get_role(&self, id: RoleId) -> Option<&Role> {
        self.roles.iter().find(|r| r.id == id)
    }

    /// Resolve which rig fills a role for the given song and optional scene.
    ///
    /// Returns the rig ID of the most specific matching assignment.
    /// Scene assignments beat song assignments beat global assignments.
    pub fn resolve_role(
        &self,
        role_id: RoleId,
        song_id: SongId,
        scene_id: Option<SceneId>,
    ) -> Option<RigId> {
        self.role_assignments
            .iter()
            .filter(|a| a.role_id == role_id && a.context.applies_to(song_id, scene_id))
            .max_by_key(|a| a.context.priority())
            .map(|a| a.rig_id)
    }

    /// Get a lightweight summary of the director.
    pub fn info(&self) -> DirectorInfo {
        DirectorInfo {
            id: self.id,
            name: self.name.clone(),
            rig_count: self.rigs.len(),
            active_rig_count: self.rigs.len(), // All rigs are considered active by default
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::module::Module;

    fn test_sends_engine() -> Module {
        Module::new("Sends Engine", ModuleType::Sends)
    }

    fn test_rig(name: &str, instrument: InstrumentType) -> Rig {
        Rig::new(name, instrument)
    }

    // ── Role creation ────────────────────────────────────────────────────

    #[test]
    fn role_lead() {
        let role = Role::lead("Lead Vocal");
        assert_eq!(role.name, "Lead Vocal");
        assert_eq!(role.role_type, RoleType::Lead);
        assert!(role.default_preset_id.is_none());
        assert!(role.color.is_none());
        assert!(role.abbreviation.is_none());
    }

    #[test]
    fn role_background() {
        let role = Role::background("BG Vocal 1", Some(1));
        assert_eq!(role.name, "BG Vocal 1");
        assert_eq!(role.role_type, RoleType::Background(Some(1)));

        let role_no_voice = Role::background("Background", None);
        assert_eq!(role_no_voice.role_type, RoleType::Background(None));
    }

    #[test]
    fn role_custom() {
        let role = Role::custom("Special FX")
            .with_color(0xFF_00_00)
            .with_abbreviation("SFX");
        assert_eq!(role.role_type, RoleType::Custom);
        assert_eq!(role.color, Some(0xFF_00_00));
        assert_eq!(role.display_abbrev(), "SFX");
    }

    #[test]
    fn role_display_abbrev_fallback() {
        let role = Role::lead("Melody");
        // No abbreviation set — falls back to first 3 chars
        assert_eq!(role.display_abbrev(), "Mel");
    }

    #[test]
    fn role_display_abbrev_short_name() {
        let role = Role::lead("FX");
        // Name shorter than 3 chars — returns full name
        assert_eq!(role.display_abbrev(), "FX");
    }

    #[test]
    fn role_with_preset() {
        let preset_id = PresetId::new();
        let role = Role::lead("Lead").with_preset(preset_id);
        assert_eq!(role.default_preset_id, Some(preset_id));
    }

    // ── Role type display ────────────────────────────────────────────────

    #[test]
    fn role_type_display() {
        assert_eq!(format!("{}", RoleType::Lead), "Lead");
        assert_eq!(format!("{}", RoleType::Background(None)), "Background");
        assert_eq!(format!("{}", RoleType::Background(Some(2))), "Background 2");
        assert_eq!(format!("{}", RoleType::Custom), "Custom");
    }

    // ── Assignment contexts ──────────────────────────────────────────────

    #[test]
    fn assignment_context_priority() {
        assert_eq!(AssignmentContext::Global.priority(), 0);
        assert!(
            AssignmentContext::Song {
                song_id: SongId::new()
            }
            .priority()
                > AssignmentContext::Global.priority()
        );
        assert!(
            AssignmentContext::Scene {
                song_id: SongId::new(),
                scene_id: SceneId::new()
            }
            .priority()
                > AssignmentContext::Song {
                    song_id: SongId::new()
                }
                .priority()
        );
    }

    #[test]
    fn assignment_context_applies_to() {
        let song_id = SongId::new();
        let scene_id = SceneId::new();
        let other_song = SongId::new();

        // Global applies to everything
        assert!(AssignmentContext::Global.applies_to(song_id, None));
        assert!(AssignmentContext::Global.applies_to(song_id, Some(scene_id)));

        // Song applies only to matching song
        let song_ctx = AssignmentContext::Song { song_id };
        assert!(song_ctx.applies_to(song_id, None));
        assert!(song_ctx.applies_to(song_id, Some(scene_id)));
        assert!(!song_ctx.applies_to(other_song, None));

        // Scene applies only to matching song + scene
        let scene_ctx = AssignmentContext::Scene { song_id, scene_id };
        assert!(scene_ctx.applies_to(song_id, Some(scene_id)));
        assert!(!scene_ctx.applies_to(song_id, None));
        assert!(!scene_ctx.applies_to(song_id, Some(SceneId::new())));
        assert!(!scene_ctx.applies_to(other_song, Some(scene_id)));
    }

    // ── Resolve role priority ────────────────────────────────────────────

    #[test]
    fn resolve_role_scene_beats_song_beats_global() {
        let mut director = RigDirector::new("Test", test_sends_engine());

        let rig_global = test_rig("Global Rig", InstrumentType::Guitar);
        let rig_song = test_rig("Song Rig", InstrumentType::Guitar);
        let rig_scene = test_rig("Scene Rig", InstrumentType::Guitar);

        let global_id = rig_global.id;
        let song_id_rig = rig_song.id;
        let scene_id_rig = rig_scene.id;

        director.add_rig(rig_global);
        director.add_rig(rig_song);
        director.add_rig(rig_scene);

        let role = Role::lead("Lead");
        let role_id = role.id;
        director.add_role(role);

        let song_id = SongId::new();
        let scene_id = SceneId::new();

        director.add_assignment(RoleAssignment::global(role_id, global_id));
        director.add_assignment(RoleAssignment::for_song(role_id, song_id_rig, song_id));
        director.add_assignment(RoleAssignment::for_scene(
            role_id,
            scene_id_rig,
            song_id,
            scene_id,
        ));

        // With scene context — scene assignment wins
        let resolved = director.resolve_role(role_id, song_id, Some(scene_id));
        assert_eq!(resolved, Some(scene_id_rig));

        // With song context only — song assignment wins
        let resolved = director.resolve_role(role_id, song_id, None);
        assert_eq!(resolved, Some(song_id_rig));

        // Different song — only global applies
        let other_song = SongId::new();
        let resolved = director.resolve_role(role_id, other_song, None);
        assert_eq!(resolved, Some(global_id));

        // Unknown role — no match
        let resolved = director.resolve_role(RoleId::new(), song_id, None);
        assert!(resolved.is_none());
    }

    // ── Director rig management ──────────────────────────────────────────

    #[test]
    fn director_rig_management() {
        let mut director = RigDirector::new("My Setup", test_sends_engine());
        assert_eq!(director.rigs.len(), 0);

        let rig = test_rig("Guitar", InstrumentType::Guitar);
        let rig_id = rig.id;
        director.add_rig(rig);

        assert_eq!(director.rigs.len(), 1);
        assert!(director.get_rig(rig_id).is_some());
        assert!(director.get_rig(RigId::new()).is_none());

        // Mutable access
        let r = director.get_rig_mut(rig_id).unwrap();
        r.name = "Electric Guitar".into();
        assert_eq!(director.get_rig(rig_id).unwrap().name, "Electric Guitar");
    }

    #[test]
    fn director_rigs_by_type() {
        let mut director = RigDirector::new("Band", test_sends_engine());
        director.add_rig(test_rig("Guitar 1", InstrumentType::Guitar));
        director.add_rig(test_rig("Guitar 2", InstrumentType::Guitar));
        director.add_rig(test_rig("Bass", InstrumentType::Bass));

        let guitars = director.rigs_by_type(&InstrumentType::Guitar);
        assert_eq!(guitars.len(), 2);

        let basses = director.rigs_by_type(&InstrumentType::Bass);
        assert_eq!(basses.len(), 1);

        let drums = director.rigs_by_type(&InstrumentType::Drums);
        assert!(drums.is_empty());
    }

    #[test]
    fn director_info() {
        let mut director = RigDirector::new("Live Rig", test_sends_engine());
        director.add_rig(test_rig("Guitar", InstrumentType::Guitar));
        director.add_rig(test_rig("Keys", InstrumentType::Keys));

        let info = director.info();
        assert_eq!(info.name, "Live Rig");
        assert_eq!(info.rig_count, 2);
        assert_eq!(info.active_rig_count, 2);
    }

    // ── RigStatus display ────────────────────────────────────────────────

    #[test]
    fn rig_status_display() {
        assert_eq!(format!("{}", RigStatus::Active), "Active");
        assert_eq!(format!("{}", RigStatus::Idle), "Idle");
        assert_eq!(format!("{}", RigStatus::Muted), "Muted");
        assert_eq!(format!("{}", RigStatus::Warning), "Warning");
        assert_eq!(format!("{}", RigStatus::Offline), "Offline");
    }

}
