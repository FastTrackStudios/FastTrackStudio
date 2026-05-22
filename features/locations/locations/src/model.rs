//! `Location` — typed view of one place page.
//!
//! Locations live as markdown files in a `vault::Vault` under
//! `locations/<slug>.md` (or anywhere — discovered via the
//! `type: location` frontmatter discriminator). Other features
//! (notably `inventory`) reference locations by `id` so renames
//! don't break links.
//!
//! `kind` is free-form (`studio`, `room`, `storage`, `venue`,
//! `home`, ...) so user conventions round-trip; the [`Kind`]
//! enum names the canonical set without forcing it.
//!
//! Unknown frontmatter keys round-trip on the source
//! `VaultPage`, not on `Location` — keep the typed surface
//! narrow and reach for raw frontmatter when the user needs it.

use chrono::{DateTime, Utc};
use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Location {
    /// Vault-relative path of the markdown file backing this
    /// location (e.g. `locations/home-studio.md`). Populated by
    /// the scanner; not serialized into frontmatter.
    #[serde(skip)]
    pub path: String,

    /// Stable identifier — survives renames + moves. Generated
    /// at create-time; persisted in frontmatter.
    pub id: Uuid,

    pub name: String,

    /// Free-form. Canonical set in [`Kind`].
    #[serde(default = "default_kind")]
    pub kind: String,

    /// Parent location, if this is nested inside another (e.g.
    /// "Control Room" inside "Home Studio"). `None` for top-level.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub parent_id: Option<Uuid>,

    /// Optional postal address (used by venue/studio kinds).
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub address: Option<String>,

    /// Free-form tags — `"primary"`, `"rental"`, `"climate-controlled"`.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub tags: Vec<String>,

    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateCreated"
    )]
    pub date_created: Option<DateTime<Utc>>,

    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateModified"
    )]
    pub date_modified: Option<DateTime<Utc>>,

    /// Markdown body after the frontmatter close fence.
    /// Free-form notes — directions, gear quirks, access codes.
    #[serde(skip)]
    pub details: String,
}

fn default_kind() -> String {
    Kind::Other.as_str().to_string()
}

/// Canonical `kind` values. Parsing accepts any string —
/// unknown kinds round-trip as the raw string on `Location`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Kind {
    Studio,
    Room,
    Storage,
    Venue,
    Home,
    Other,
}

impl Kind {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Studio => "studio",
            Self::Room => "room",
            Self::Storage => "storage",
            Self::Venue => "venue",
            Self::Home => "home",
            Self::Other => "other",
        }
    }

    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "studio" => Some(Self::Studio),
            "room" => Some(Self::Room),
            "storage" | "store" => Some(Self::Storage),
            "venue" => Some(Self::Venue),
            "home" => Some(Self::Home),
            "other" => Some(Self::Other),
            _ => None,
        }
    }
}
