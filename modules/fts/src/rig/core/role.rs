//! Role - Semantic assignment for vocal rigs
//!
//! Roles provide semantic meaning to vocal rigs (Lead, Background, or custom
//! names like "Sarah", "Dave"). Roles can be reassigned between rigs without
//! changing the underlying processing.

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// A semantic role that can be assigned to a vocal rig.
///
/// Roles stay with their semantic meaning while rigs/inputs can be reassigned.
/// For example, if Sarah is on input 3 instead of input 4 tonight, just reassign
/// the "Sarah" role to the rig on input 3.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct Role {
    /// Unique identifier
    pub id: Uuid,
    /// Display name (e.g., "Lead", "BG 1", "Sarah")
    pub name: String,
    /// Type of role
    pub role_type: RoleType,
    /// Default engine preset to load when this role is assigned
    pub default_preset_id: Option<Uuid>,
    /// Display color (RGBA)
    pub color: Option<u32>,
    /// Short abbreviation for compact display (e.g., "LD", "BG1", "SAR")
    pub abbreviation: Option<String>,
}

/// Type of vocal role.
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Facet)]
pub enum RoleType {
    /// Lead vocalist
    Lead = 0,
    /// Background vocalist (with optional index)
    Background(Option<u8>) = 1,
    /// Custom named role (e.g., "Sarah", "Dave")
    Custom = 2,
}

/// Assignment of a role to a rig in a specific context.
///
/// Role assignments can be global (default) or scoped to a specific song/scene.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct RoleAssignment {
    /// Unique identifier
    pub id: Uuid,
    /// The role being assigned
    pub role_id: Uuid,
    /// The rig receiving the assignment
    pub rig_id: Uuid,
    /// Context in which this assignment applies
    pub context: AssignmentContext,
    /// Priority for conflict resolution (higher = takes precedence)
    pub priority: u8,
}

/// Context in which a role assignment applies.
///
/// Resolution priority: Scene > Song > Global
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Facet)]
pub enum AssignmentContext {
    /// Default assignment, applies everywhere unless overridden
    Global = 0,
    /// Assignment for a specific song
    Song { song_id: Uuid } = 1,
    /// Assignment for a specific scene within a song
    Scene { song_id: Uuid, scene_id: Uuid } = 2,
}

impl Role {
    /// Create a new role
    pub fn new(name: impl Into<String>, role_type: RoleType) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            role_type,
            default_preset_id: None,
            color: None,
            abbreviation: None,
        }
    }

    /// Create a Lead role
    pub fn lead() -> Self {
        let mut role = Self::new("Lead", RoleType::Lead);
        role.abbreviation = Some("LD".to_string());
        role.color = Some(0xFF6B6BFF); // Coral red
        role
    }

    /// Create a Background role with index
    pub fn background(index: u8) -> Self {
        let mut role = Self::new(format!("BG {index}"), RoleType::Background(Some(index)));
        role.abbreviation = Some(format!("BG{index}"));
        role.color = Some(0x4ECDC4FF); // Teal
        role
    }

    /// Create a custom named role
    pub fn custom(name: impl Into<String>) -> Self {
        let name = name.into();
        let abbrev = name.chars().take(3).collect::<String>().to_uppercase();
        let mut role = Self::new(&name, RoleType::Custom);
        role.abbreviation = Some(abbrev);
        role.color = Some(0x95E1D3FF); // Mint green
        role
    }

    /// Set default preset
    pub fn with_preset(mut self, preset_id: Uuid) -> Self {
        self.default_preset_id = Some(preset_id);
        self
    }

    /// Set color
    pub fn with_color(mut self, color: u32) -> Self {
        self.color = Some(color);
        self
    }

    /// Set abbreviation
    pub fn with_abbreviation(mut self, abbrev: impl Into<String>) -> Self {
        self.abbreviation = Some(abbrev.into());
        self
    }

    /// Get display abbreviation
    pub fn display_abbrev(&self) -> String {
        self.abbreviation.clone().unwrap_or_else(|| {
            self.name.chars().take(2).collect::<String>().to_uppercase()
        })
    }
}

impl RoleAssignment {
    /// Create a global role assignment
    pub fn global(role_id: Uuid, rig_id: Uuid) -> Self {
        Self {
            id: Uuid::new_v4(),
            role_id,
            rig_id,
            context: AssignmentContext::Global,
            priority: 0,
        }
    }

    /// Create a song-specific role assignment
    pub fn for_song(role_id: Uuid, rig_id: Uuid, song_id: Uuid) -> Self {
        Self {
            id: Uuid::new_v4(),
            role_id,
            rig_id,
            context: AssignmentContext::Song { song_id },
            priority: 1,
        }
    }

    /// Create a scene-specific role assignment
    pub fn for_scene(role_id: Uuid, rig_id: Uuid, song_id: Uuid, scene_id: Uuid) -> Self {
        Self {
            id: Uuid::new_v4(),
            role_id,
            rig_id,
            context: AssignmentContext::Scene { song_id, scene_id },
            priority: 2,
        }
    }
}

impl AssignmentContext {
    /// Get the priority level for this context (higher = more specific)
    pub fn priority(&self) -> u8 {
        match self {
            AssignmentContext::Global => 0,
            AssignmentContext::Song { .. } => 1,
            AssignmentContext::Scene { .. } => 2,
        }
    }

    /// Check if this context applies to a given song/scene
    pub fn applies_to(&self, song_id: Option<Uuid>, scene_id: Option<Uuid>) -> bool {
        match self {
            AssignmentContext::Global => true,
            AssignmentContext::Song { song_id: ctx_song } => {
                song_id.is_some_and(|s| s == *ctx_song)
            }
            AssignmentContext::Scene {
                song_id: ctx_song,
                scene_id: ctx_scene,
            } => {
                song_id.is_some_and(|s| s == *ctx_song)
                    && scene_id.is_some_and(|sc| sc == *ctx_scene)
            }
        }
    }
}

impl std::fmt::Display for RoleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RoleType::Lead => write!(f, "Lead"),
            RoleType::Background(Some(idx)) => write!(f, "Background {idx}"),
            RoleType::Background(None) => write!(f, "Background"),
            RoleType::Custom => write!(f, "Custom"),
        }
    }
}
