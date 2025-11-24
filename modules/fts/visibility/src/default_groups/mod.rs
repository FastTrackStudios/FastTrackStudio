//! Default sorting groups for visibility manager
//!
//! These groups are hardcoded and can be used as the foundation for visibility management.
//! Groups are organized into modules by category for better organization.

pub mod drums;
pub mod bass;
pub mod guitars;
pub mod keys;
pub mod synths;
pub mod orchestra;
pub mod vocals;

use super::group::SortingGroup;
use super::Group;

/// Re-export all group types
pub use drums::Drums;
pub use bass::Bass;
pub use guitars::{GtrElectric, GtrAcoustic};
pub use keys::Keys;
pub use synths::Synths;
pub use orchestra::Orchestra;
pub use vocals::{Vocals, VBgvs, VChoir};

// ============================================================================
// Helper Functions
// ============================================================================

/// Create all default sorting groups
pub fn create_default_groups() -> Vec<Group> {
    vec![
        Drums::build(),
        Bass::build(),
        GtrElectric::build(),
        GtrAcoustic::build(),
        Keys::build(),
        Synths::build(),
        Orchestra::build(),
        Vocals::build(),
        VBgvs::build(),
        VChoir::build(),
    ]
}

