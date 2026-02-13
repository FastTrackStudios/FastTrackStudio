//! Generic override path/op model.
//!
//! Overrides let higher-level variants (Layer, Engine, Rig, Song sections)
//! modify lower-level parameters without changing the referenced variant directly.

use facet::Facet;
use serde::{Deserialize, Serialize};

use crate::ParameterValue;

// ─── Override target path ───────────────────────────────────────

/// Identifies which parameter to override within the hierarchy.
///
/// Paths are dot-separated: `"module.drive.block.od.param.gain"`.
/// The controller resolves these against the live hierarchy.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub struct OverridePath(String);

impl OverridePath {
    pub fn new(path: impl Into<String>) -> Self {
        Self(path.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Split path into segments for resolution.
    pub fn segments(&self) -> impl Iterator<Item = &str> {
        self.0.split('.')
    }
}

impl From<&str> for OverridePath {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

// ─── Override operation ─────────────────────────────────────────

/// What to do at the override target.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub enum OverrideOp {
    /// Set parameter to an absolute value.
    Set(ParameterValue),
    /// Bypass a block or module.
    Bypass(bool),
}

// ─── Override entry ─────────────────────────────────────────────

/// A single override: path + operation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Override {
    pub path: OverridePath,
    pub op: OverrideOp,
}

impl Override {
    pub fn set(path: impl Into<OverridePath>, value: f32) -> Self {
        Self {
            path: path.into(),
            op: OverrideOp::Set(ParameterValue::new(value)),
        }
    }

    pub fn bypass(path: impl Into<OverridePath>, bypassed: bool) -> Self {
        Self {
            path: path.into(),
            op: OverrideOp::Bypass(bypassed),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_override_path_segments() {
        let path = OverridePath::new("module.drive.block.od.param.gain");
        let segs: Vec<_> = path.segments().collect();
        assert_eq!(segs, ["module", "drive", "block", "od", "param", "gain"]);
    }

    #[test]
    fn test_override_set() {
        let ov = Override::set("module.eq.param.freq", 0.75);
        assert_eq!(ov.path.as_str(), "module.eq.param.freq");
        match &ov.op {
            OverrideOp::Set(v) => assert!((v.get() - 0.75).abs() < f32::EPSILON),
            _ => panic!("expected Set"),
        }
    }
}
