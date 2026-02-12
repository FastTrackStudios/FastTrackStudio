//! Preset import/export bundle types.
//!
//! [`PresetBundle`] is the wire format for sharing presets between instances.
//! It wraps one or more [`Preset`](crate::preset::Preset)s with format version
//! and metadata. The actual serialization (via `facet-json`) lives in
//! [`signal-storage`] — this module only defines the domain types.


use crate::preset::Preset;

// ─────────────────────────────────────────────────────────────────────────────
// BundleMetadata
// ─────────────────────────────────────────────────────────────────────────────

/// Metadata attached to an exported preset bundle.
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct BundleMetadata {
    /// ISO-8601 timestamp of when the bundle was exported.
    pub exported_at: String,
    /// Optional name of the user/system that exported.
    pub exported_by: Option<String>,
    /// Optional description of the bundle contents.
    pub description: Option<String>,
}

impl BundleMetadata {
    /// Create metadata with just a timestamp.
    pub fn new(exported_at: impl Into<String>) -> Self {
        Self {
            exported_at: exported_at.into(),
            exported_by: None,
            description: None,
        }
    }

    /// Set the exporter name.
    #[must_use]
    pub fn with_exported_by(mut self, name: impl Into<String>) -> Self {
        self.exported_by = Some(name.into());
        self
    }

    /// Set the bundle description.
    #[must_use]
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// PresetBundle
// ─────────────────────────────────────────────────────────────────────────────

/// Current bundle format version. Increment on breaking changes.
pub const BUNDLE_FORMAT_VERSION: u32 = 1;

/// A portable bundle of one or more presets for import/export.
///
/// The `version` field enables forward compatibility — importers can
/// reject or migrate bundles from newer/older formats.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct PresetBundle {
    /// Format version for forward compatibility.
    pub version: u32,
    /// The presets in this bundle.
    pub presets: Vec<Preset>,
    /// Export metadata.
    pub metadata: BundleMetadata,
}

impl PresetBundle {
    /// Create a bundle containing a single preset.
    pub fn single(preset: Preset, metadata: BundleMetadata) -> Self {
        Self {
            version: BUNDLE_FORMAT_VERSION,
            presets: vec![preset],
            metadata,
        }
    }

    /// Create a bundle containing multiple presets.
    pub fn multiple(presets: Vec<Preset>, metadata: BundleMetadata) -> Self {
        Self {
            version: BUNDLE_FORMAT_VERSION,
            presets,
            metadata,
        }
    }

    /// Whether this bundle contains exactly one preset.
    pub fn is_single(&self) -> bool {
        self.presets.len() == 1
    }

    /// Number of presets in the bundle.
    pub fn count(&self) -> usize {
        self.presets.len()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ExportError / ImportError
// ─────────────────────────────────────────────────────────────────────────────

/// Error during preset export.
#[derive(Debug, Clone, PartialEq)]
pub enum ExportError {
    /// Serialization failed.
    Serialization(String),
    /// I/O error writing to file.
    Io(String),
}

impl std::fmt::Display for ExportError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Serialization(msg) => write!(f, "export serialization error: {msg}"),
            Self::Io(msg) => write!(f, "export I/O error: {msg}"),
        }
    }
}

impl std::error::Error for ExportError {}

/// Error during preset import.
#[derive(Debug, Clone, PartialEq)]
pub enum ImportError {
    /// Deserialization failed.
    Deserialization(String),
    /// I/O error reading from file.
    Io(String),
    /// Bundle version is incompatible.
    IncompatibleVersion { found: u32, expected: u32 },
    /// Bundle contains no presets.
    EmptyBundle,
}

impl std::fmt::Display for ImportError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Deserialization(msg) => write!(f, "import deserialization error: {msg}"),
            Self::Io(msg) => write!(f, "import I/O error: {msg}"),
            Self::IncompatibleVersion { found, expected } => {
                write!(
                    f,
                    "incompatible bundle version: found {found}, expected {expected}"
                )
            }
            Self::EmptyBundle => write!(f, "bundle contains no presets"),
        }
    }
}

impl std::error::Error for ImportError {}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::category::{BaseTone, PresetCategory};

    fn test_preset(name: &str) -> Preset {
        Preset::new(
            name,
            PresetCategory::Generic {
                base_tone: BaseTone::Clean,
            },
        )
    }

    #[test]
    fn single_bundle() {
        let preset = test_preset("My Clean");
        let bundle =
            PresetBundle::single(preset.clone(), BundleMetadata::new("2025-01-01T00:00:00Z"));

        assert!(bundle.is_single());
        assert_eq!(bundle.count(), 1);
        assert_eq!(bundle.version, BUNDLE_FORMAT_VERSION);
        assert_eq!(bundle.presets[0].name, "My Clean");
    }

    #[test]
    fn multiple_bundle() {
        let presets = vec![
            test_preset("Clean"),
            test_preset("Crunch"),
            test_preset("Lead"),
        ];
        let bundle = PresetBundle::multiple(
            presets,
            BundleMetadata::new("2025-06-15T12:00:00Z")
                .with_exported_by("Test User")
                .with_description("My favorite presets"),
        );

        assert!(!bundle.is_single());
        assert_eq!(bundle.count(), 3);
        assert_eq!(bundle.metadata.exported_by.as_deref(), Some("Test User"));
        assert_eq!(
            bundle.metadata.description.as_deref(),
            Some("My favorite presets")
        );
    }

    #[test]
    fn error_display() {
        let err = ExportError::Serialization("bad data".into());
        assert_eq!(err.to_string(), "export serialization error: bad data");

        let err = ImportError::IncompatibleVersion {
            found: 99,
            expected: 1,
        };
        assert_eq!(
            err.to_string(),
            "incompatible bundle version: found 99, expected 1"
        );

        let err = ImportError::EmptyBundle;
        assert_eq!(err.to_string(), "bundle contains no presets");
    }
}
