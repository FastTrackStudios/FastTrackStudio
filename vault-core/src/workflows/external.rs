//! External service integrations — link tasks and projects to external systems.
//!
//! References are stored as YAML frontmatter fields. The external service
//! is the source of truth for its domain (finances, invoices, code).
//! We store the reference ID and sync status.

use chrono::NaiveDate;
use facet::Facet;

/// A reference to an external service resource.
/// Can be embedded in any task, project, or deliverable.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ExternalRef {
    /// Service identifier: "firefly-iii", "invoice-ninja", "github", "reaper", etc.
    pub service: String,
    /// Resource ID in the external service.
    pub ref_id: String,
    /// Human-readable label.
    pub label: Option<String>,
    /// URL to the resource in the external service.
    pub url: Option<String>,
    /// Last time we synced with the external service.
    pub last_sync: Option<NaiveDate>,
}

// ── Firefly III (budgeting / accounting) ─────────────────────────────────────

/// Reference to a Firefly III transaction or budget.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct FireflyRef {
    /// Firefly III instance base URL.
    pub instance: Option<String>,
    /// Transaction ID.
    pub transaction_id: Option<String>,
    /// Budget ID.
    pub budget_id: Option<String>,
    /// Category name.
    pub category: Option<String>,
    /// Account name.
    pub account: Option<String>,
}

// ── Invoice Ninja (invoicing / billing) ──────────────────────────────────────

/// Reference to an Invoice Ninja invoice or quote.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct InvoiceNinjaRef {
    /// Invoice Ninja instance base URL.
    pub instance: Option<String>,
    /// Invoice ID.
    pub invoice_id: Option<String>,
    /// Quote ID.
    pub quote_id: Option<String>,
    /// Client ID.
    pub client_id: Option<String>,
    /// Invoice status.
    pub status: InvoiceStatus,
    /// Amount.
    pub amount: Option<f64>,
    /// Currency code (e.g. "USD").
    pub currency: Option<String>,
    /// Due date.
    pub due: Option<NaiveDate>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum InvoiceStatus {
    #[default]
    Draft,
    Sent,
    Viewed,
    Partial,
    Paid,
    Overdue,
    Cancelled,
}

// ── GitHub (code / issues) ───────────────────────────────────────────────────

/// Reference to a GitHub issue, PR, or release.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct GitHubRef {
    /// Repository slug (e.g. "FastTrackStudios/task").
    pub repo: String,
    /// Issue number.
    pub issue: Option<u32>,
    /// Pull request number.
    pub pr: Option<u32>,
    /// Release tag.
    pub release: Option<String>,
}

// ── DAW / REAPER ─────────────────────────────────────────────────────────────

/// Reference to a DAW session file.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct DawSessionRef {
    /// DAW identifier: "reaper", "logic", "ableton", "protools", etc.
    pub daw: String,
    /// Path to the session file (relative to project folder).
    pub session_file: String,
    /// Sample rate of the session.
    pub sample_rate: Option<u32>,
    /// Tempo (BPM).
    pub tempo: Option<f64>,
    /// Time signature (e.g. "4/4").
    pub time_signature: Option<String>,
    /// Track count.
    pub track_count: Option<u32>,
}

// ── Client Portal ────────────────────────────────────────────────────────────

/// Configuration for a client-facing view of a project.
/// Controls what the client can see and access.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ClientPortal {
    pub enabled: bool,
    pub client_name: String,
    pub client_email: Option<String>,
    /// Which output versions are visible to the client.
    #[facet(default)]
    pub shared_outputs: Vec<String>,
    /// Whether the client can leave feedback/comments.
    pub allow_feedback: bool,
    /// Whether the client can approve outputs.
    pub allow_approval: bool,
    /// Nextcloud share link (auto-generated).
    pub share_url: Option<String>,
}
