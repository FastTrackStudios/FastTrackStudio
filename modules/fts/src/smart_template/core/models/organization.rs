//! Organization modes for track hierarchies

use serde::{Deserialize, Serialize};

/// Mode for organizing track hierarchies
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum OrganizationMode {
    /// Group by performer first, then arrangement
    /// Example: Cody -> Strum -> [Sources]
    #[default]
    ByPerformer,
    
    /// Group by arrangement first, then performer
    /// Example: Strum (Cody) -> [Sources]
    ByArrangement,
}

