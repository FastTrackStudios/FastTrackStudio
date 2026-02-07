//! Parameter system — format-aware normalization, denormalization, and display.
//!
//! Parameters describe how a DSP control maps between a normalized [0.0, 1.0]
//! range and its real-world value (dB, Hz, ms, etc.). The [`ParamFormat`] enum
//! encodes the mapping math, while [`ParamSpec`] bundles format with metadata.

use facet::Facet;

use crate::normalized::NormalizedF64;

// ─────────────────────────────────────────────────────────────────────────────
// ParamFormat
// ─────────────────────────────────────────────────────────────────────────────

/// Describes the real-world format and range of a parameter.
///
/// Each variant knows how to convert between a normalized `[0.0, 1.0]` value
/// and the corresponding real-world value (dB, Hz, ms, etc.).
///
/// Skew factors control the curve shape:
/// - `1.0` = linear
/// - `< 1.0` = more resolution at the low end (e.g. frequency)
/// - `> 1.0` = more resolution at the high end
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(u8)]
pub enum ParamFormat {
    /// 0–100% linear mapping.
    Percent,

    /// Decibel range with optional skew.
    Decibels {
        min: f64,
        max: f64,
        skew_factor: f64,
    },

    /// Frequency range (Hz) with logarithmic-style skew.
    Frequency {
        min: f64,
        max: f64,
        skew_factor: f64,
    },

    /// Millisecond range with optional skew.
    Milliseconds {
        min: f64,
        max: f64,
        skew_factor: f64,
    },

    /// Second range with optional skew.
    Seconds {
        min: f64,
        max: f64,
        skew_factor: f64,
    },

    /// Generic float range with configurable precision and skew.
    Float {
        min: f64,
        max: f64,
        precision: u8,
        skew_factor: f64,
    },

    /// Integer range (linear).
    Integer {
        min: i32,
        max: i32,
    },

    /// Boolean on/off toggle.
    Toggle,

    /// Discrete set of named options.
    Enum {
        options: Vec<String>,
    },
}

impl ParamFormat {
    /// Apply skew curve: maps linear [0, 1] to skewed [0, 1].
    fn apply_skew(normalized: f64, skew_factor: f64) -> f64 {
        if (skew_factor - 1.0).abs() < f64::EPSILON {
            normalized
        } else {
            normalized.powf(skew_factor)
        }
    }

    /// Invert skew curve: maps skewed [0, 1] back to linear [0, 1].
    fn unapply_skew(skewed: f64, skew_factor: f64) -> f64 {
        if (skew_factor - 1.0).abs() < f64::EPSILON {
            skewed
        } else {
            skewed.powf(1.0 / skew_factor)
        }
    }

    /// Convert a normalized `[0.0, 1.0]` value to its real-world representation.
    pub fn denormalize(&self, normalized: f64) -> f64 {
        let n = normalized.clamp(0.0, 1.0);

        match self {
            Self::Percent => n * 100.0,

            Self::Decibels { min, max, skew_factor }
            | Self::Frequency { min, max, skew_factor }
            | Self::Milliseconds { min, max, skew_factor }
            | Self::Seconds { min, max, skew_factor }
            | Self::Float { min, max, skew_factor, .. } => {
                let skewed = Self::apply_skew(n, *skew_factor);
                min + skewed * (max - min)
            }

            Self::Integer { min, max } => {
                let range = f64::from(*max - *min);
                (f64::from(*min) + n * range).round()
            }

            Self::Toggle => {
                if n >= 0.5 { 1.0 } else { 0.0 }
            }

            Self::Enum { options } => {
                if options.is_empty() {
                    return 0.0;
                }
                #[allow(clippy::cast_precision_loss)]
                let max_index = (options.len() - 1) as f64;
                (n * max_index).round()
            }
        }
    }

    /// Convert a real-world value back to normalized `[0.0, 1.0]`.
    pub fn normalize(&self, real_value: f64) -> f64 {
        match self {
            Self::Percent => (real_value / 100.0).clamp(0.0, 1.0),

            Self::Decibels { min, max, skew_factor }
            | Self::Frequency { min, max, skew_factor }
            | Self::Milliseconds { min, max, skew_factor }
            | Self::Seconds { min, max, skew_factor }
            | Self::Float { min, max, skew_factor, .. } => {
                let range = max - min;
                if range.abs() < f64::EPSILON {
                    return 0.0;
                }
                let linear = ((real_value - min) / range).clamp(0.0, 1.0);
                Self::unapply_skew(linear, *skew_factor)
            }

            Self::Integer { min, max } => {
                let range = f64::from(*max - *min);
                if range.abs() < f64::EPSILON {
                    return 0.0;
                }
                ((real_value - f64::from(*min)) / range).clamp(0.0, 1.0)
            }

            Self::Toggle => {
                if real_value >= 0.5 { 1.0 } else { 0.0 }
            }

            Self::Enum { options } => {
                if options.len() <= 1 {
                    return 0.0;
                }
                #[allow(clippy::cast_precision_loss)]
                let max_index = (options.len() - 1) as f64;
                (real_value / max_index).clamp(0.0, 1.0)
            }
        }
    }

    /// Format a real-world value as a human-readable string with units.
    pub fn format_value(&self, real_value: f64) -> String {
        match self {
            Self::Percent => format!("{real_value:.1}%"),
            Self::Decibels { .. } => format!("{real_value:.1} dB"),
            Self::Frequency { .. } => {
                if real_value >= 1000.0 {
                    format!("{:.2} kHz", real_value / 1000.0)
                } else {
                    format!("{real_value:.1} Hz")
                }
            }
            Self::Milliseconds { .. } => format!("{real_value:.1} ms"),
            Self::Seconds { .. } => format!("{real_value:.2} s"),
            Self::Float { precision, .. } => {
                format!("{:.prec$}", real_value, prec = *precision as usize)
            }
            #[allow(clippy::cast_possible_truncation)]
            Self::Integer { .. } => {
                let int_value = real_value as i32;
                format!("{int_value}")
            }
            Self::Toggle => {
                if real_value >= 0.5 { "On".to_string() } else { "Off".to_string() }
            }
            Self::Enum { options } => {
                #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                let index = real_value.round() as usize;
                options
                    .get(index)
                    .cloned()
                    .unwrap_or_else(|| format!("?{index}"))
            }
        }
    }

    /// Parse a human-readable string back to a real-world value.
    ///
    /// Returns `None` if the text cannot be parsed for this format.
    pub fn parse_value(&self, text: &str) -> Option<f64> {
        let trimmed = text.trim();
        match self {
            Self::Percent => {
                let s = trimmed.strip_suffix('%').unwrap_or(trimmed).trim();
                s.parse::<f64>().ok()
            }
            Self::Decibels { .. } => {
                let s = trimmed
                    .strip_suffix("dB")
                    .unwrap_or(trimmed)
                    .trim();
                s.parse::<f64>().ok()
            }
            Self::Frequency { .. } => {
                if let Some(s) = trimmed.strip_suffix("kHz") {
                    s.trim().parse::<f64>().ok().map(|v| v * 1000.0)
                } else {
                    let s = trimmed.strip_suffix("Hz").unwrap_or(trimmed).trim();
                    s.parse::<f64>().ok()
                }
            }
            Self::Milliseconds { .. } => {
                let s = trimmed.strip_suffix("ms").unwrap_or(trimmed).trim();
                s.parse::<f64>().ok()
            }
            Self::Seconds { .. } => {
                let s = trimmed.strip_suffix('s').unwrap_or(trimmed).trim();
                s.parse::<f64>().ok()
            }
            Self::Float { .. } => trimmed.parse::<f64>().ok(),
            Self::Integer { .. } => trimmed.parse::<i32>().ok().map(f64::from),
            Self::Toggle => match trimmed.to_lowercase().as_str() {
                "on" | "true" | "1" | "yes" => Some(1.0),
                "off" | "false" | "0" | "no" => Some(0.0),
                _ => None,
            },
            Self::Enum { options } => options
                .iter()
                .position(|o| o.eq_ignore_ascii_case(trimmed))
                .map(|i| {
                    #[allow(clippy::cast_precision_loss)]
                    let v = i as f64;
                    v
                }),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ParamSpec
// ─────────────────────────────────────────────────────────────────────────────

/// Full specification for a parameter: identity, format, default, and metadata.
///
/// This is the "blueprint" for a parameter — it does not hold a current value,
/// only describes what values are valid and how to display them.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ParamSpec {
    /// Unique identifier (e.g. `"drive"`, `"eq_low_freq"`).
    pub id: String,
    /// Human-readable name (e.g. `"Drive"`, `"Low Frequency"`).
    pub name: String,
    /// Abbreviated name for compact UI (e.g. `"Drv"`, `"Lo F"`).
    pub short_name: String,
    /// Value format and range.
    pub format: ParamFormat,
    /// Default normalized value in [0.0, 1.0].
    pub default: f64,
    /// Display priority (lower = more prominent). 0 = primary.
    pub priority: u8,
    /// Optional plugin parameter index (for mapping to plugin APIs).
    pub plugin_index: Option<u32>,
    /// Optional grouping label (e.g. `"EQ"`, `"Dynamics"`).
    pub group: Option<String>,
}

// ─────────────────────────────────────────────────────────────────────────────
// ParameterValue
// ─────────────────────────────────────────────────────────────────────────────

/// A parameter index paired with its current normalized value.
///
/// Lightweight value type used in [`Block`](crate::block::Block) parameter lists
/// and real-time parameter change messages.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ParameterValue {
    /// Index into the parent's parameter spec list.
    pub index: u32,
    /// Current value, normalized to [0.0, 1.0].
    pub value: NormalizedF64,
}

impl ParameterValue {
    /// Create a new parameter value, clamping `value` to [0.0, 1.0].
    pub fn new(index: u32, value: f64) -> Self {
        Self {
            index,
            value: NormalizedF64::new(value),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // Denormalize / normalize roundtrips

    #[test]
    fn percent_roundtrip() {
        let fmt = ParamFormat::Percent;
        let real = fmt.denormalize(0.75);
        assert!((real - 75.0).abs() < f64::EPSILON);
        let back = fmt.normalize(real);
        assert!((back - 0.75).abs() < 1e-10);
    }

    #[test]
    fn decibels_linear_roundtrip() {
        let fmt = ParamFormat::Decibels { min: -60.0, max: 0.0, skew_factor: 1.0 };
        let real = fmt.denormalize(0.5);
        assert!((real - (-30.0)).abs() < f64::EPSILON);
        let back = fmt.normalize(real);
        assert!((back - 0.5).abs() < 1e-10);
    }

    #[test]
    fn decibels_skewed_roundtrip() {
        let fmt = ParamFormat::Decibels { min: -60.0, max: 0.0, skew_factor: 0.5 };
        for n in [0.0, 0.25, 0.5, 0.75, 1.0] {
            let real = fmt.denormalize(n);
            let back = fmt.normalize(real);
            assert!((back - n).abs() < 1e-10, "failed at n={n}: back={back}");
        }
    }

    #[test]
    fn frequency_roundtrip() {
        let fmt = ParamFormat::Frequency { min: 20.0, max: 20000.0, skew_factor: 0.2 };
        for n in [0.0, 0.25, 0.5, 0.75, 1.0] {
            let real = fmt.denormalize(n);
            let back = fmt.normalize(real);
            assert!((back - n).abs() < 1e-10, "failed at n={n}: real={real}, back={back}");
        }
    }

    #[test]
    fn milliseconds_roundtrip() {
        let fmt = ParamFormat::Milliseconds { min: 0.0, max: 1000.0, skew_factor: 1.0 };
        let real = fmt.denormalize(0.3);
        assert!((real - 300.0).abs() < f64::EPSILON);
        let back = fmt.normalize(real);
        assert!((back - 0.3).abs() < 1e-10);
    }

    #[test]
    fn seconds_roundtrip() {
        let fmt = ParamFormat::Seconds { min: 0.0, max: 10.0, skew_factor: 1.0 };
        let real = fmt.denormalize(0.5);
        assert!((real - 5.0).abs() < f64::EPSILON);
        let back = fmt.normalize(real);
        assert!((back - 0.5).abs() < 1e-10);
    }

    #[test]
    fn float_roundtrip() {
        let fmt = ParamFormat::Float { min: -1.0, max: 1.0, precision: 2, skew_factor: 1.0 };
        let real = fmt.denormalize(0.75);
        assert!((real - 0.5).abs() < f64::EPSILON);
        let back = fmt.normalize(real);
        assert!((back - 0.75).abs() < 1e-10);
    }

    #[test]
    fn integer_roundtrip() {
        let fmt = ParamFormat::Integer { min: 0, max: 10 };
        let real = fmt.denormalize(0.5);
        assert_eq!(real as i32, 5);
        let back = fmt.normalize(5.0);
        assert!((back - 0.5).abs() < 1e-10);
    }

    #[test]
    fn toggle_roundtrip() {
        let fmt = ParamFormat::Toggle;
        assert_eq!(fmt.denormalize(0.0), 0.0);
        assert_eq!(fmt.denormalize(0.49), 0.0);
        assert_eq!(fmt.denormalize(0.5), 1.0);
        assert_eq!(fmt.denormalize(1.0), 1.0);
        assert_eq!(fmt.normalize(0.0), 0.0);
        assert_eq!(fmt.normalize(1.0), 1.0);
    }

    #[test]
    fn enum_roundtrip() {
        let fmt = ParamFormat::Enum {
            options: vec!["Low".into(), "Mid".into(), "High".into()],
        };
        assert_eq!(fmt.denormalize(0.0) as usize, 0);
        assert_eq!(fmt.denormalize(0.5) as usize, 1);
        assert_eq!(fmt.denormalize(1.0) as usize, 2);
        let back = fmt.normalize(1.0);
        assert!((back - 0.5).abs() < 1e-10);
    }

    // format_value output

    #[test]
    fn format_percent() {
        let fmt = ParamFormat::Percent;
        assert_eq!(fmt.format_value(75.0), "75.0%");
    }

    #[test]
    fn format_decibels() {
        let fmt = ParamFormat::Decibels { min: -60.0, max: 0.0, skew_factor: 1.0 };
        assert_eq!(fmt.format_value(-30.0), "-30.0 dB");
    }

    #[test]
    fn format_frequency_hz() {
        let fmt = ParamFormat::Frequency { min: 20.0, max: 20000.0, skew_factor: 0.2 };
        assert_eq!(fmt.format_value(440.0), "440.0 Hz");
    }

    #[test]
    fn format_frequency_khz() {
        let fmt = ParamFormat::Frequency { min: 20.0, max: 20000.0, skew_factor: 0.2 };
        assert_eq!(fmt.format_value(5000.0), "5.00 kHz");
    }

    #[test]
    fn format_milliseconds() {
        let fmt = ParamFormat::Milliseconds { min: 0.0, max: 1000.0, skew_factor: 1.0 };
        assert_eq!(fmt.format_value(250.5), "250.5 ms");
    }

    #[test]
    fn format_seconds() {
        let fmt = ParamFormat::Seconds { min: 0.0, max: 10.0, skew_factor: 1.0 };
        assert_eq!(fmt.format_value(3.5), "3.50 s");
    }

    #[test]
    fn format_float() {
        let fmt = ParamFormat::Float { min: 0.0, max: 1.0, precision: 3, skew_factor: 1.0 };
        assert_eq!(fmt.format_value(0.123_456), "0.123");
    }

    #[test]
    fn format_integer() {
        let fmt = ParamFormat::Integer { min: 0, max: 127 };
        assert_eq!(fmt.format_value(42.0), "42");
    }

    #[test]
    fn format_toggle() {
        let fmt = ParamFormat::Toggle;
        assert_eq!(fmt.format_value(1.0), "On");
        assert_eq!(fmt.format_value(0.0), "Off");
    }

    #[test]
    fn format_enum_variant() {
        let fmt = ParamFormat::Enum {
            options: vec!["Low".into(), "Mid".into(), "High".into()],
        };
        assert_eq!(fmt.format_value(1.0), "Mid");
        assert_eq!(fmt.format_value(2.0), "High");
    }

    #[test]
    fn format_enum_out_of_bounds() {
        let fmt = ParamFormat::Enum {
            options: vec!["A".into(), "B".into()],
        };
        assert_eq!(fmt.format_value(5.0), "?5");
    }

    // parse_value

    #[test]
    fn parse_percent() {
        let fmt = ParamFormat::Percent;
        assert_eq!(fmt.parse_value("75%"), Some(75.0));
        assert_eq!(fmt.parse_value("75"), Some(75.0));
    }

    #[test]
    fn parse_decibels() {
        let fmt = ParamFormat::Decibels { min: -60.0, max: 0.0, skew_factor: 1.0 };
        assert_eq!(fmt.parse_value("-30 dB"), Some(-30.0));
        assert_eq!(fmt.parse_value("-30"), Some(-30.0));
    }

    #[test]
    fn parse_frequency() {
        let fmt = ParamFormat::Frequency { min: 20.0, max: 20000.0, skew_factor: 0.2 };
        assert_eq!(fmt.parse_value("440 Hz"), Some(440.0));
        assert_eq!(fmt.parse_value("5 kHz"), Some(5000.0));
    }

    #[test]
    fn parse_toggle() {
        let fmt = ParamFormat::Toggle;
        assert_eq!(fmt.parse_value("On"), Some(1.0));
        assert_eq!(fmt.parse_value("off"), Some(0.0));
        assert_eq!(fmt.parse_value("true"), Some(1.0));
        assert_eq!(fmt.parse_value("maybe"), None);
    }

    #[test]
    fn parse_enum_variant() {
        let fmt = ParamFormat::Enum {
            options: vec!["Low".into(), "Mid".into(), "High".into()],
        };
        assert_eq!(fmt.parse_value("Mid"), Some(1.0));
        assert_eq!(fmt.parse_value("high"), Some(2.0));
        assert_eq!(fmt.parse_value("Unknown"), None);
    }

    // ParameterValue clamping

    #[test]
    fn parameter_value_clamps_above_one() {
        let pv = ParameterValue::new(0, 1.5);
        assert_eq!(pv.value.get(), 1.0);
    }

    #[test]
    fn parameter_value_clamps_below_zero() {
        let pv = ParameterValue::new(0, -0.5);
        assert_eq!(pv.value.get(), 0.0);
    }

    #[test]
    fn parameter_value_preserves_valid() {
        let pv = ParameterValue::new(3, 0.42);
        assert_eq!(pv.index, 3);
        assert!((pv.value.get() - 0.42).abs() < f64::EPSILON);
    }

    // Edge cases

    #[test]
    fn denormalize_clamps_input() {
        let fmt = ParamFormat::Percent;
        assert_eq!(fmt.denormalize(-0.5), 0.0);
        assert_eq!(fmt.denormalize(1.5), 100.0);
    }

    #[test]
    fn normalize_zero_range() {
        let fmt = ParamFormat::Decibels { min: 0.0, max: 0.0, skew_factor: 1.0 };
        assert_eq!(fmt.normalize(0.0), 0.0);
    }

    #[test]
    fn enum_empty_options() {
        let fmt = ParamFormat::Enum { options: vec![] };
        assert_eq!(fmt.denormalize(0.5), 0.0);
        assert_eq!(fmt.normalize(0.0), 0.0);
    }
}
