//! Role-based download bundles and download portal.
//!
//! Two access modes:
//!
//! 1. **Direct role link** — a URL that goes straight to one role's files
//!    (`/portal/campus-jax-2026/orchestra-violin`)
//!
//! 2. **Portal link** — a single URL for the whole event with a role selector
//!    (`/portal/campus-jax-2026`)
//!    Shows all roles, lets you pick yours, highlights your downloads,
//!    but lets you browse other roles' files too.
//!
//! File storage is on Nextcloud (WebDAV). The portal is served by task-server.

use chrono::NaiveDate;
use facet::Facet;

// ── Download Portal ──────────────────────────────────────────────────────────

/// The top-level download portal for an event/project.
/// Stored as `downloads/portal.md`.
///
/// One portal link covers the whole event. Anyone with the link
/// sees the role selector and can browse all public bundles.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct DownloadPortal {
    pub title: String,
    /// Parent event or project title.
    pub event: String,
    /// URL slug for the portal (e.g. "campus-jax-2026").
    pub slug: String,
    /// Optional portal-wide password.
    pub password: Option<String>,
    /// Expiration date for the portal.
    pub expires: Option<NaiveDate>,
    /// Whether the portal is live.
    pub published: bool,
    /// Branding / header message shown on the portal page.
    #[facet(default)]
    pub message: String,
    /// Visibility setting for cross-role browsing.
    pub visibility: PortalVisibility,
    /// All bundles available through this portal.
    #[facet(default)]
    pub bundles: Vec<DownloadBundle>,
}

/// Controls what visitors can see beyond their own role.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum PortalVisibility {
    /// Can see file names and categories of other roles, but only
    /// download their own. Default — lets a violinist peek at the
    /// woodwind part list and request access.
    #[default]
    BrowseAll,
    /// Can see AND download files from any role.
    DownloadAll,
    /// Can only see their own role's files. Other roles are hidden.
    OwnRoleOnly,
}

// ── Download Bundle ──────────────────────────────────────────────────────────

/// A curated set of files for a specific role or group.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct DownloadBundle {
    /// Bundle identifier (e.g. "orchestra-violin-1", "band-drums", "crew-audio").
    pub id: String,
    /// Human-readable name (e.g. "Violin 1", "Drum Kit", "Audio Crew").
    pub name: String,
    /// Icon/emoji for the role selector (e.g. "🎻", "🥁", "🎤", "🎧").
    pub icon: Option<String>,
    /// Which role(s) this bundle is for.
    #[facet(default)]
    pub roles: Vec<String>,
    /// Which performance/act this belongs to (empty = event-wide).
    pub performance: Option<String>,
    /// Group for organizing in the role selector (e.g. "Strings", "Rhythm", "Crew").
    pub group: Option<String>,

    // ── Content ──────────────────────────────────────────────────
    /// Explicitly listed files.
    #[facet(default)]
    pub files: Vec<BundleFile>,

    /// Auto-include rules: glob patterns to collect files.
    #[facet(default)]
    pub include_rules: Vec<IncludeRule>,

    /// Shared files that appear in every bundle (schedule, venue info, etc.).
    /// These are defined once at the portal level and inherited.
    #[facet(default)]
    pub include_shared: bool,

    // ── Direct link ──────────────────────────────────────────────
    /// Direct share URL for this specific role (skips role selector).
    pub direct_url: Option<String>,
    /// Nextcloud share token for the generated bundle folder.
    pub share_token: Option<String>,

    // ── Tracking ─────────────────────────────────────────────────
    /// Who this bundle was sent to.
    #[facet(default)]
    pub recipients: Vec<Recipient>,

    #[facet(default)]
    pub notes: String,
}

/// A file included in a download bundle.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct BundleFile {
    /// Source path relative to project root.
    pub source: String,
    /// Display path within the bundle (can rename/reorganize).
    /// If empty, uses the source filename.
    pub dest: Option<String>,
    /// Category for organizing within the bundle (e.g. "Sheet Music", "Click Tracks").
    pub category: Option<String>,
    /// File size in bytes (populated when bundle is generated).
    pub size_bytes: Option<u64>,
    /// MIME type (populated when bundle is generated).
    pub mime_type: Option<String>,
}

/// Rule for auto-including files from the project.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct IncludeRule {
    /// Glob pattern relative to project root (e.g. "scores/violin-1/*.pdf").
    pub pattern: String,
    /// Category to file them under in the bundle.
    pub category: Option<String>,
}

/// Shared files that appear in every bundle (schedule, venue info, etc.).
/// Defined at the portal level.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct SharedFiles {
    #[facet(default)]
    pub files: Vec<BundleFile>,
    #[facet(default)]
    pub include_rules: Vec<IncludeRule>,
}

/// A person who received a bundle or portal link.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Recipient {
    pub name: String,
    pub email: Option<String>,
    pub phone: Option<String>,
    pub sent_date: Option<NaiveDate>,
    /// Whether they've accessed the portal/link.
    pub accessed: bool,
    /// Whether they've downloaded files.
    pub downloaded: bool,
    /// Which bundle they were sent (empty = portal link).
    pub bundle_id: Option<String>,
}
