//! Guitar profile templates — Worship, Blues, Rock, Funk.
//!
//! Each profile contains 4 patches representing common tones for that genre.
//! Patches use placeholder scene references since the seed data defines
//! structure, not runtime state.

use crate::id::RigSceneId;
use crate::profile::{Patch, Profile};
use crate::scene::ScopedSceneRef;
use crate::version::VersionedRef;

/// Create a placeholder rig scene reference for seed data patches.
fn placeholder_scene() -> ScopedSceneRef {
    ScopedSceneRef::Rig(VersionedRef::new(RigSceneId::new(), 1))
}

// ─────────────────────────────────────────────────────────────────────────────
// Worship Profile
// ─────────────────────────────────────────────────────────────────────────────

/// Worship guitar profile — clean ambient pads, shimmer delays, light drive.
///
/// Designed for modern worship settings (Hillsong, Bethel, Elevation style):
/// - Ambient Clean: dotted-eighth delay, large reverb, chorus
/// - Soft Drive: light Klon into clean amp, shimmer reverb
/// - Lead Shimmer: moderate drive, octave-up shimmer, long delay
/// - Swells: volume pedal swells into heavy reverb and delay
pub fn guitar_worship_profile() -> Profile {
    let mut profile = Profile::new("Worship");
    profile.add_patch(Patch::new("Ambient Clean", placeholder_scene()));
    profile.add_patch(Patch::new("Soft Drive", placeholder_scene()));
    profile.add_patch(Patch::new("Lead Shimmer", placeholder_scene()));
    profile.add_patch(Patch::new("Swells", placeholder_scene()));
    profile
}

// ─────────────────────────────────────────────────────────────────────────────
// Blues Profile
// ─────────────────────────────────────────────────────────────────────────────

/// Blues guitar profile — warm breakup, Klon into Fender, touch-sensitive.
///
/// Classic and modern blues tones:
/// - Blues Clean: Fender clean, light compression, spring reverb
/// - Blues Crunch: Klon at low gain into pushed Fender, touch-responsive
/// - Blues Lead: Klon + TS stacked, neck pickup tone
/// - BB King: Midrange-forward, sustain compression, vibrato
pub fn guitar_blues_profile() -> Profile {
    let mut profile = Profile::new("Blues");
    profile.add_patch(Patch::new("Blues Clean", placeholder_scene()));
    profile.add_patch(Patch::new("Blues Crunch", placeholder_scene()));
    profile.add_patch(Patch::new("Blues Lead", placeholder_scene()));
    profile.add_patch(Patch::new("BB King", placeholder_scene()));
    profile
}

// ─────────────────────────────────────────────────────────────────────────────
// Rock Profile
// ─────────────────────────────────────────────────────────────────────────────

/// Rock guitar profile — medium gain, dual amp, tight time effects.
///
/// Covers classic to modern rock territory:
/// - Rhythm Clean: bright clean, light chorus, short reverb
/// - Crunch Rhythm: medium gain dual amp, tight delay
/// - Rock Lead: stacked drives, longer delay, moderate reverb
/// - Power Chords: high gain, tight low end, minimal effects
pub fn guitar_rock_profile() -> Profile {
    let mut profile = Profile::new("Rock");
    profile.add_patch(Patch::new("Rhythm Clean", placeholder_scene()));
    profile.add_patch(Patch::new("Crunch Rhythm", placeholder_scene()));
    profile.add_patch(Patch::new("Rock Lead", placeholder_scene()));
    profile.add_patch(Patch::new("Power Chords", placeholder_scene()));
    profile
}

// ─────────────────────────────────────────────────────────────────────────────
// Funk Profile
// ─────────────────────────────────────────────────────────────────────────────

/// Funk guitar profile — wah, envelope filter, tight comp, clean-to-crunch.
///
/// Tight, percussive funk guitar tones:
/// - Funk Clean: tight compressor, bright EQ, spring reverb
/// - Wah Funk: wah pedal, light compression, dry signal
/// - Envelope Groove: envelope filter (auto-wah), tight dynamics
/// - Funky Lead: light crunch, phaser, short delay
pub fn guitar_funk_profile() -> Profile {
    let mut profile = Profile::new("Funk");
    profile.add_patch(Patch::new("Funk Clean", placeholder_scene()));
    profile.add_patch(Patch::new("Wah Funk", placeholder_scene()));
    profile.add_patch(Patch::new("Envelope Groove", placeholder_scene()));
    profile.add_patch(Patch::new("Funky Lead", placeholder_scene()));
    profile
}

/// All four guitar profiles.
pub fn all_guitar_profiles() -> Vec<Profile> {
    vec![
        guitar_worship_profile(),
        guitar_blues_profile(),
        guitar_rock_profile(),
        guitar_funk_profile(),
    ]
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn worship_profile_has_4_patches() {
        let p = guitar_worship_profile();
        assert_eq!(p.name, "Worship");
        assert_eq!(p.patches.len(), 4);
    }

    #[test]
    fn blues_profile_has_4_patches() {
        let p = guitar_blues_profile();
        assert_eq!(p.name, "Blues");
        assert_eq!(p.patches.len(), 4);
    }

    #[test]
    fn rock_profile_has_4_patches() {
        let p = guitar_rock_profile();
        assert_eq!(p.name, "Rock");
        assert_eq!(p.patches.len(), 4);
    }

    #[test]
    fn funk_profile_has_4_patches() {
        let p = guitar_funk_profile();
        assert_eq!(p.name, "Funk");
        assert_eq!(p.patches.len(), 4);
    }

    #[test]
    fn all_profiles_returns_4() {
        assert_eq!(all_guitar_profiles().len(), 4);
    }

    #[test]
    fn all_patch_names_unique() {
        let profiles = all_guitar_profiles();
        let mut names: Vec<&str> = profiles
            .iter()
            .flat_map(|p| p.patches.iter().map(|patch| patch.name.as_str()))
            .collect();
        let total = names.len();
        names.sort();
        names.dedup();
        assert_eq!(names.len(), total, "duplicate patch names found");
    }

    #[test]
    fn all_profile_names_unique() {
        let profiles = all_guitar_profiles();
        let mut names: Vec<&str> = profiles.iter().map(|p| p.name.as_str()).collect();
        let total = names.len();
        names.sort();
        names.dedup();
        assert_eq!(names.len(), total, "duplicate profile names found");
    }
}
