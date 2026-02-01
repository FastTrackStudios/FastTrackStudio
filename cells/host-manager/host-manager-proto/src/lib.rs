//! Host Manager Protocol
//!
//! Types for identifying and managing connections to multiple hosts.
//! This is used by desktop apps and other clients that need to connect
//! to multiple DAW hosts and route commands appropriately.

use facet::Facet;

/// Host identity sent via virtual connection metadata.
///
/// When a client connects to a host, the host opens a virtual connection
/// back with this identity in the metadata. This allows the client to
/// identify and route commands to the appropriate host.
///
/// # Hierarchy
///
/// Hosts are organized in a hierarchy:
/// - **Purpose**: The type of service (e.g., "guitar", "keyboard", "tracks")
/// - **Instance**: A specific instance of that purpose (e.g., "Guitar 1", "Guitar 2")
/// - **Host**: A specific machine/process (for redundancy within an instance)
///
/// # Examples
///
/// Single instance (no redundancy):
/// ```ignore
/// HostIdentity {
///     id: "host-123",
///     name: "Studio REAPER",
///     purpose: "tracks",
///     instance: None,  // Uses purpose as instance name
///     tags: vec![],
/// }
/// ```
///
/// Multiple instances with redundancy:
/// ```ignore
/// // Guitar 1 - Primary
/// HostIdentity {
///     purpose: "guitar",
///     instance: Some("Guitar 1"),
///     ..
/// }
/// // Guitar 1 - Backup (same instance, different host)
/// HostIdentity {
///     purpose: "guitar",
///     instance: Some("Guitar 1"),
///     ..
/// }
/// // Guitar 2 - Primary (different instance)
/// HostIdentity {
///     purpose: "guitar",
///     instance: Some("Guitar 2"),
///     ..
/// }
/// ```
#[derive(Debug, Clone, Facet)]
pub struct HostIdentity {
    /// Unique host ID (generated or from config).
    pub id: String,
    /// Human-readable name (e.g., "Studio REAPER").
    pub name: String,
    /// Purpose tag (e.g., "tracks", "guitar", "keyboard").
    /// This categorizes what the host is used for.
    pub purpose: String,
    /// Instance identifier for multiple instances of the same purpose.
    ///
    /// Examples: "Guitar 1", "Guitar 2", "Main", "Backup"
    /// If not set, the purpose is used as the instance name.
    pub instance: Option<String>,
    /// Optional additional tags for filtering.
    pub tags: Vec<String>,
}

impl HostIdentity {
    /// Create a new host identity.
    pub fn new(id: impl Into<String>, name: impl Into<String>, purpose: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            purpose: purpose.into(),
            instance: None,
            tags: vec![],
        }
    }

    /// Set the instance name.
    pub fn with_instance(mut self, instance: impl Into<String>) -> Self {
        self.instance = Some(instance.into());
        self
    }

    /// Add tags.
    pub fn with_tags(mut self, tags: Vec<String>) -> Self {
        self.tags = tags;
        self
    }

    /// Get the instance name, falling back to purpose if not set.
    pub fn instance_name(&self) -> &str {
        self.instance.as_deref().unwrap_or(&self.purpose)
    }

    /// Get a unique key for this purpose+instance combination.
    ///
    /// This is used for grouping redundant hosts. Hosts with the same
    /// instance key are considered part of the same redundancy group.
    ///
    /// # Format
    /// - With instance: `"purpose:instance"` (e.g., `"guitar:Guitar 1"`)
    /// - Without instance: `"purpose"` (e.g., `"guitar"`)
    pub fn instance_key(&self) -> String {
        match &self.instance {
            Some(inst) => format!("{}:{}", self.purpose, inst),
            None => self.purpose.clone(),
        }
    }
}

/// Metadata key for host identity in virtual connection requests.
pub const HOST_IDENTITY_KEY: &str = "host-identity";
