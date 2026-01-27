//! Canonical Parameter Specification System
//!
//! This module provides a single source of truth for parameter metadata that drives:
//! 1. UI rendering (generic knobs and specialized FX widgets)
//! 2. Plugin parameter automation (nih-plug, VST3, CLAP)
//! 3. Value conversion between normalized (0.0-1.0) and real-world spaces (dB, ms, Hz)
//! 4. Preset serialization and snapshot overrides
//!
//! The key design principle: **Define once, use everywhere**. A parameter's metadata
//! (format, range, scaling) is defined in `ParamSpec` and used identically for UI,
//! plugins, and serialization.

use serde::{Deserialize, Serialize};
use facet::Facet;

// ─────────────────────────────────────────────────────────────────────────────
// Skew factor defaults
// ─────────────────────────────────────────────────────────────────────────────

fn default_skew() -> f64 {
    1.0 // Linear scaling
}

fn default_log_skew() -> f64 {
    0.2 // Logarithmic-like scaling (~1/5)
}

// ─────────────────────────────────────────────────────────────────────────────
// ParamFormat: Value range and conversion rules
// ─────────────────────────────────────────────────────────────────────────────

/// How to format and scale a parameter value.
///
/// This enum carries enough information to:
/// - Convert normalized (0.0-1.0) ↔ real-world values
/// - Format values for display (e.g., "-18.0 dB", "250 ms")
/// - Parse user input back to real-world values
/// - Support perceptual scaling (skew) for plugin automation
///
/// **Design principle**: Mirrors nih-plug's FloatRange/IntRange so that when we
/// build audio DSP plugins, `ParamFormat` can directly generate plugin parameter
/// definitions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum ParamFormat {
    /// 0.0-1.0 displayed as 0%-100%
    Percent,

    /// Decibel scale. Normalized 0.0 = min dB, 1.0 = max dB.
    /// Supports linear or skewed mapping for perceptual scaling.
    ///
    /// **Skew factor**:
    /// - 1.0 = linear (proportional)
    /// - <1.0 = more resolution at low end (e.g., -60..0 dB uses more travel for -40..-10)
    /// - >1.0 = more resolution at high end (rarely used)
    ///
    /// **Why skew**: A compressor threshold knob should spend most of its travel in
    /// the -40 to -10 dB range where it matters perceptually. Skew makes this happen.
    Decibels {
        min: f64,
        max: f64,
        #[serde(default = "default_skew")]
        skew_factor: f64,
    },

    /// Frequency in Hz. Normalized 0.0 = min Hz, 1.0 = max Hz.
    /// Default skew gives logarithmic scaling (perceptually uniform).
    ///
    /// **Why logarithmic**: Humans perceive frequency logarithmically. An octave
    /// (2x frequency) sounds like the same interval whether it's 100→200 Hz or
    /// 5000→10000 Hz. Logarithmic scaling makes the UI match perception.
    Frequency {
        min: f64,
        max: f64,
        #[serde(default = "default_log_skew")]
        skew_factor: f64,
    },

    /// Time in milliseconds.
    Milliseconds {
        min: f64,
        max: f64,
        #[serde(default = "default_skew")]
        skew_factor: f64,
    },

    /// Time in seconds.
    Seconds {
        min: f64,
        max: f64,
        #[serde(default = "default_skew")]
        skew_factor: f64,
    },

    /// Arbitrary float range with display precision.
    Float {
        min: f64,
        max: f64,
        precision: u8,
        #[serde(default = "default_skew")]
        skew_factor: f64,
    },

    /// Integer range (quantized to nearest integer).
    Integer { min: i32, max: i32 },

    /// Boolean toggle (normalized: 0.0 = false, >0.5 = true).
    Toggle,

    /// Enum with named options. Normalized value quantized to option index.
    /// - 0.0 = options[0]
    /// - 1.0 = options[len-1]
    /// - Quantized to nearest index
    Enum { options: Vec<String> },
}

impl ParamFormat {
    /// Convert a normalized value (0.0-1.0) to real-world value.
    ///
    /// This is the key function for feeding values to FX widgets, plugin parameters,
    /// and display strings.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let format = ParamFormat::Decibels { min: -60.0, max: 0.0, skew_factor: 1.0 };
    /// assert_eq!(format.denormalize(0.0), -60.0);
    /// assert_eq!(format.denormalize(1.0), 0.0);
    /// assert_eq!(format.denormalize(0.5), -30.0); // linear
    ///
    /// let format = ParamFormat::Frequency { min: 20.0, max: 20000.0, skew_factor: 0.2 };
    /// assert!(format.denormalize(0.5) > 20.0);
    /// assert!(format.denormalize(0.5) < 20000.0);
    /// ```
    pub fn denormalize(&self, normalized: f64) -> f64 {
        let clamped = normalized.clamp(0.0, 1.0);

        match self {
            ParamFormat::Percent => clamped * 100.0,

            ParamFormat::Decibels {
                min,
                max,
                skew_factor,
            } => Self::apply_skew(*min, *max, clamped, *skew_factor),

            ParamFormat::Frequency {
                min,
                max,
                skew_factor,
            } => Self::apply_skew(*min, *max, clamped, *skew_factor),

            ParamFormat::Milliseconds {
                min,
                max,
                skew_factor,
            } => Self::apply_skew(*min, *max, clamped, *skew_factor),

            ParamFormat::Seconds {
                min,
                max,
                skew_factor,
            } => Self::apply_skew(*min, *max, clamped, *skew_factor),

            ParamFormat::Float {
                min,
                max,
                skew_factor,
                ..
            } => Self::apply_skew(*min, *max, clamped, *skew_factor),

            ParamFormat::Integer { min, max } => {
                let range = (*max - *min) as f64;
                *min as f64 + (clamped * range)
            }

            ParamFormat::Toggle => if clamped > 0.5 { 1.0 } else { 0.0 },

            ParamFormat::Enum { options } => {
                let index = (clamped * (options.len() - 1).max(0) as f64).round() as usize;
                index as f64
            }
        }
    }

    /// Convert a real-world value to normalized (0.0-1.0).
    ///
    /// This is used when:
    /// - Sending FX widget changes back to storage/plugins
    /// - Deserializing preset values
    /// - Converting user input to normalized space
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let format = ParamFormat::Decibels { min: -60.0, max: 0.0, skew_factor: 1.0 };
    /// assert_eq!(format.normalize(-60.0), 0.0);
    /// assert_eq!(format.normalize(0.0), 1.0);
    /// assert_eq!(format.normalize(-30.0), 0.5); // linear
    /// ```
    pub fn normalize(&self, real_value: f64) -> f64 {
        match self {
            ParamFormat::Percent => (real_value / 100.0).clamp(0.0, 1.0),

            ParamFormat::Decibels {
                min,
                max,
                skew_factor,
            } => Self::unapply_skew(*min, *max, real_value, *skew_factor),

            ParamFormat::Frequency {
                min,
                max,
                skew_factor,
            } => Self::unapply_skew(*min, *max, real_value, *skew_factor),

            ParamFormat::Milliseconds {
                min,
                max,
                skew_factor,
            } => Self::unapply_skew(*min, *max, real_value, *skew_factor),

            ParamFormat::Seconds {
                min,
                max,
                skew_factor,
            } => Self::unapply_skew(*min, *max, real_value, *skew_factor),

            ParamFormat::Float {
                min,
                max,
                skew_factor,
                ..
            } => Self::unapply_skew(*min, *max, real_value, *skew_factor),

            ParamFormat::Integer { min, max } => {
                let range = *max - *min;
                if range == 0 {
                    0.0
                } else {
                    ((real_value as i32 - min).clamp(0, *max - *min) as f64
                        / (*max - *min) as f64)
                        .clamp(0.0, 1.0)
                }
            }

            ParamFormat::Toggle => if real_value > 0.5 { 1.0 } else { 0.0 },

            ParamFormat::Enum { options } => {
                let index = real_value as usize;
                if options.is_empty() {
                    0.0
                } else {
                    (index as f64 / (options.len() - 1).max(1) as f64).clamp(0.0, 1.0)
                }
            }
        }
    }

    /// Format a real-world value as a display string.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// assert_eq!(
    ///     ParamFormat::Decibels { min: -60.0, max: 0.0, skew_factor: 1.0 }
    ///         .format_value(-18.0),
    ///     "-18.0 dB"
    /// );
    /// assert_eq!(
    ///     ParamFormat::Milliseconds { min: 1.0, max: 2000.0, skew_factor: 1.0 }
    ///         .format_value(250.0),
    ///     "250 ms"
    /// );
    /// ```
    pub fn format_value(&self, real_value: f64) -> String {
        match self {
            ParamFormat::Percent => format!("{:.1}%", real_value),
            ParamFormat::Decibels { .. } => format!("{:.1} dB", real_value),
            ParamFormat::Frequency { .. } => {
                if real_value >= 1000.0 {
                    format!("{:.2} kHz", real_value / 1000.0)
                } else {
                    format!("{:.0} Hz", real_value)
                }
            }
            ParamFormat::Milliseconds { .. } => format!("{:.1} ms", real_value),
            ParamFormat::Seconds { .. } => format!("{:.2} s", real_value),
            ParamFormat::Float { precision, .. } => {
                format!("{:.prec$}", real_value, prec = *precision as usize)
            }
            ParamFormat::Integer { .. } => format!("{}", real_value as i32),
            ParamFormat::Toggle => {
                if real_value > 0.5 {
                    "On".to_string()
                } else {
                    "Off".to_string()
                }
            }
            ParamFormat::Enum { options } => {
                let index = real_value as usize;
                options
                    .get(index)
                    .cloned()
                    .unwrap_or_else(|| format!("Option {}", index))
            }
        }
    }

    /// Parse a display string back to a real-world value.
    ///
    /// This is lenient - it tries to extract the numeric value and ignore units.
    pub fn parse_value(&self, text: &str) -> Option<f64> {
        let trimmed = text.trim();

        // Extract number from string like "-18.0 dB" → "-18.0"
        let number_str = trimmed
            .split_whitespace()
            .next()
            .unwrap_or(trimmed);

        match self {
            ParamFormat::Percent => {
                let num = number_str.trim_end_matches('%').parse::<f64>().ok()?;
                Some(num.clamp(0.0, 100.0))
            }
            ParamFormat::Decibels { min, max, .. } => {
                let num = number_str.parse::<f64>().ok()?;
                Some(num.clamp(*min, *max))
            }
            ParamFormat::Frequency { min, max, .. } => {
                let num_str = number_str.trim_end_matches("kHz").trim_end_matches("Hz");
                let num = num_str.parse::<f64>().ok()?;
                let value = if trimmed.contains("kHz") { num * 1000.0 } else { num };
                Some(value.clamp(*min, *max))
            }
            ParamFormat::Milliseconds { min, max, .. } => {
                let num = number_str.trim_end_matches("ms").parse::<f64>().ok()?;
                Some(num.clamp(*min, *max))
            }
            ParamFormat::Seconds { min, max, .. } => {
                let num = number_str.trim_end_matches('s').parse::<f64>().ok()?;
                Some(num.clamp(*min, *max))
            }
            ParamFormat::Float { min, max, .. } => {
                let num = number_str.parse::<f64>().ok()?;
                Some(num.clamp(*min, *max))
            }
            ParamFormat::Integer { min, max } => {
                let num = number_str.parse::<i32>().ok()?;
                Some((num.clamp(*min, *max)) as f64)
            }
            ParamFormat::Toggle => {
                let lower = trimmed.to_lowercase();
                Some(if lower == "on" || lower == "true" || lower == "1" {
                    1.0
                } else {
                    0.0
                })
            }
            ParamFormat::Enum { options } => {
                // Try to match option name
                for (index, opt) in options.iter().enumerate() {
                    if opt.eq_ignore_ascii_case(trimmed) {
                        return Some(index as f64);
                    }
                }
                // Fall back to parsing as number
                number_str.parse::<usize>().ok().map(|i| i as f64)
            }
        }
    }

    /// Helper: Apply skew transformation (normalized → real-world)
    fn apply_skew(min: f64, max: f64, normalized: f64, skew: f64) -> f64 {
        if skew == 1.0 {
            // Linear: min + (max - min) * normalized
            min + (max - min) * normalized
        } else if skew < 1.0 {
            // Inverse skew: compress high end, expand low end
            // Formula: min + (max - min) * pow(normalized, 1/skew)
            min + (max - min) * normalized.powf(1.0 / skew)
        } else {
            // Positive skew: compress low end, expand high end
            // Formula: min + (max - min) * pow(normalized, skew)
            min + (max - min) * normalized.powf(skew)
        }
    }

    /// Helper: Unapply skew transformation (real-world → normalized)
    fn unapply_skew(min: f64, max: f64, real_value: f64, skew: f64) -> f64 {
        let range = max - min;
        if range == 0.0 {
            return 0.0;
        }

        let normalized_linear = (real_value - min) / range;
        let clamped = normalized_linear.clamp(0.0, 1.0);

        if skew == 1.0 {
            clamped
        } else if skew < 1.0 {
            // Inverse of: pow(normalized, 1/skew) → pow(result, skew)
            clamped.powf(skew)
        } else {
            // Inverse of: pow(normalized, skew) → pow(result, 1/skew)
            clamped.powf(1.0 / skew)
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ParamSpec: Complete parameter specification
// ─────────────────────────────────────────────────────────────────────────────

/// Canonical parameter specification — SINGLE source of truth.
///
/// This struct carries everything needed to:
/// 1. Render in any UI widget (generic knobs or specialized FX)
/// 2. Map to/from a nih-plug plugin parameter
/// 3. Serialize for presets/snapshots/RPC
/// 4. Convert between normalized and real-world values
///
/// Design principle: **Define once, use everywhere**. A parameter is defined here
/// and used identically for UI, plugins, and serialization.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ParamSpec {
    /// Stable string identifier (e.g., "threshold", "ratio", "attack").
    ///
    /// Used for:
    /// - Preset storage (so presets survive parameter reordering)
    /// - Snapshot overrides
    /// - FX widget field mapping (must match widget struct field names)
    pub id: String,

    /// Human-readable name (e.g., "Threshold", "Attack Time").
    pub name: String,

    /// Short name for compact displays (e.g., "Thrs", "Atk").
    /// Shown in Mini/Compact LODs.
    pub short_name: String,

    /// Value format, range, and conversion rules.
    pub format: ParamFormat,

    /// Default value in normalized space (0.0-1.0).
    pub default: f64,

    /// LOD (Level of Detail) priority.
    /// Lower = shown in more compact views.
    /// Used to decide which parameters to hide when space is limited.
    pub priority: u8,

    /// Plugin parameter index — bridges named params to nih-plug/VST3/CLAP indices.
    /// None for UI-only params (e.g., computed display values, labels).
    ///
    /// When building a nih-plug plugin, parameters are ordered by this index.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub plugin_index: Option<u32>,

    /// Group for organizing params in the UI (e.g., "Dynamics", "Filter", "Mix").
    /// Used by nih-plug's `#[nested(group = "...")]` attribute.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub group: Option<String>,
}

// ─────────────────────────────────────────────────────────────────────────────
// ResolvedParam: Parameter with current value
// ─────────────────────────────────────────────────────────────────────────────

/// A parameter with its spec + current value + override status.
///
/// This is what the UI renders and what FX mappers consume. It combines:
/// - `ParamSpec`: static metadata (format, range, name)
/// - `normalized`: current value in 0.0-1.0 space
/// - `is_overridden`: whether this comes from a snapshot override
#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedParam {
    /// The parameter's metadata and conversion rules.
    pub spec: ParamSpec,

    /// Current value in NORMALIZED space (0.0-1.0).
    /// Use `real_value()` to get the denormalized value for FX widgets.
    pub normalized: f64,

    /// Whether this value comes from a snapshot override.
    /// UI can use this to highlight the parameter or show an indicator.
    pub is_overridden: bool,
}

impl ResolvedParam {
    /// Get the real-world value (e.g., -18.0 dB, 250.0 ms).
    ///
    /// This is what FX widgets expect. Internally uses `ParamFormat::denormalize()`.
    #[inline]
    pub fn real_value(&self) -> f64 {
        self.spec.format.denormalize(self.normalized)
    }

    /// Get the display string (e.g., "-18.0 dB").
    ///
    /// Convenience method combining denormalize + format_value.
    pub fn display(&self) -> String {
        self.spec.format.format_value(self.real_value())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_percent_format() {
        let fmt = ParamFormat::Percent;
        assert_eq!(fmt.denormalize(0.0), 0.0);
        assert_eq!(fmt.denormalize(0.5), 50.0);
        assert_eq!(fmt.denormalize(1.0), 100.0);

        assert_eq!(fmt.normalize(0.0), 0.0);
        assert_eq!(fmt.normalize(50.0), 0.5);
        assert_eq!(fmt.normalize(100.0), 1.0);
    }

    #[test]
    fn test_db_linear() {
        let fmt = ParamFormat::Decibels {
            min: -60.0,
            max: 0.0,
            skew_factor: 1.0,
        };
        assert_eq!(fmt.denormalize(0.0), -60.0);
        assert_eq!(fmt.denormalize(0.5), -30.0);
        assert_eq!(fmt.denormalize(1.0), 0.0);

        assert_eq!(fmt.normalize(-60.0), 0.0);
        assert!((fmt.normalize(-30.0) - 0.5).abs() < 0.001);
        assert_eq!(fmt.normalize(0.0), 1.0);
    }

    #[test]
    fn test_enum_format() {
        let fmt = ParamFormat::Enum {
            options: vec!["Off".into(), "Low".into(), "High".into()],
        };
        assert_eq!(fmt.denormalize(0.0), 0.0); // Off
        assert_eq!(fmt.denormalize(0.5), 1.0); // Low
        assert_eq!(fmt.denormalize(1.0), 2.0); // High

        assert_eq!(fmt.normalize(0.0), 0.0);
        assert!(fmt.normalize(1.0) > 0.4 && fmt.normalize(1.0) < 0.7); // Mid
        assert_eq!(fmt.normalize(2.0), 1.0);
    }

    #[test]
    fn test_format_value() {
        let fmt = ParamFormat::Decibels {
            min: -60.0,
            max: 0.0,
            skew_factor: 1.0,
        };
        assert_eq!(fmt.format_value(-18.0), "-18.0 dB");

        let fmt = ParamFormat::Milliseconds {
            min: 1.0,
            max: 2000.0,
            skew_factor: 1.0,
        };
        assert_eq!(fmt.format_value(250.0), "250.0 ms");
    }

    #[test]
    fn test_parse_value() {
        let fmt = ParamFormat::Decibels {
            min: -60.0,
            max: 0.0,
            skew_factor: 1.0,
        };
        assert_eq!(fmt.parse_value("-18.0 dB"), Some(-18.0));
        assert_eq!(fmt.parse_value("-18.0"), Some(-18.0));

        let fmt = ParamFormat::Milliseconds {
            min: 1.0,
            max: 2000.0,
            skew_factor: 1.0,
        };
        assert_eq!(fmt.parse_value("250 ms"), Some(250.0));
    }
}
