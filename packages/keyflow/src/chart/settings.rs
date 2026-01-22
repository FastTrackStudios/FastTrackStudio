//! Chart Settings
//!
//! Configuration options for chart parsing and display

use crate::chord::PushPullBase;
use facet::Facet;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Chart configuration settings
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ChartSettings {
    /// Internal settings storage
    settings: HashMap<ChartSetting, SettingValue>,
    /// Default push/pull base (standard, triplet, or tuplet)
    #[serde(default)]
    pub push_mode: PushPullBase,
}

/// Available chart settings
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum ChartSetting {
    /// Automatically group repeated phrases into 4-bar units with repeat signs
    SmartRepeats,
    /// Default push/pull mode (standard, triplet, or tuplet number)
    PushMode,
    /// Automatically fill whole/half notes with quarter note slashes
    /// When enabled (default), a whole note chord becomes 4 quarter slashes,
    /// a half note becomes 2 quarter slashes. This is standard for master rhythm charts.
    AutoRhythmSlashes,
    /// Whether push/pull notation alters the rhythm display
    /// When enabled (default), pushed chords create triplet/syncopated notation.
    /// When disabled, pushed chords show apostrophe markers on chord symbols instead.
    PushAltersRhythm,
}

/// Setting value types
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum SettingValue {
    Bool(bool),
    String(String),
    Number(i32),
}

impl ChartSettings {
    /// Create new default settings
    pub fn new() -> Self {
        let mut settings = HashMap::new();

        // Set defaults
        settings.insert(ChartSetting::SmartRepeats, SettingValue::Bool(false));
        settings.insert(ChartSetting::AutoRhythmSlashes, SettingValue::Bool(true)); // ON by default
        settings.insert(ChartSetting::PushAltersRhythm, SettingValue::Bool(true)); // ON by default

        Self {
            settings,
            push_mode: PushPullBase::Standard,
        }
    }

    /// Parse a setting line (e.g., "/SMART_REPEATS=true")
    pub fn parse_setting_line(&mut self, line: &str) -> Result<(), String> {
        // Remove leading slash and trim
        let line = line.trim().trim_start_matches('/').trim();

        // Split by '='
        let parts: Vec<&str> = line.splitn(2, '=').collect();
        if parts.len() != 2 {
            return Err(format!(
                "Invalid setting format: '{}'. Expected /SETTING=value",
                line
            ));
        }

        let key = parts[0].trim().to_uppercase();
        let value = parts[1].trim();

        match key.as_str() {
            "SMART_REPEATS" => {
                let bool_value = Self::parse_bool(value)?;
                self.set(ChartSetting::SmartRepeats, SettingValue::Bool(bool_value));
                Ok(())
            }
            "PUSH" => {
                self.push_mode = Self::parse_push_mode(value)?;
                Ok(())
            }
            "AUTO_RHYTHM_SLASHES" | "AUTORHYTHMSLASHES" | "AUTO_SLASHES" => {
                let bool_value = Self::parse_bool(value)?;
                self.set(
                    ChartSetting::AutoRhythmSlashes,
                    SettingValue::Bool(bool_value),
                );
                Ok(())
            }
            "PUSH_ALTERS_RHYTHM" | "PUSHALTERSRHYTHM" => {
                let bool_value = Self::parse_bool(value)?;
                self.set(
                    ChartSetting::PushAltersRhythm,
                    SettingValue::Bool(bool_value),
                );
                Ok(())
            }
            _ => Err(format!("Unknown setting: '{}'", key)),
        }
    }

    /// Parse push mode value: "standard", "triplet", or a number for tuplet
    fn parse_push_mode(value: &str) -> Result<PushPullBase, String> {
        let value_lower = value.to_lowercase();
        match value_lower.as_str() {
            "standard" | "normal" | "binary" => Ok(PushPullBase::Standard),
            "triplet" | "3" => Ok(PushPullBase::Triplet),
            _ => {
                // Try to parse as a tuplet number (5, 7, 9, etc.)
                if let Ok(n) = value.parse::<u8>() {
                    if n >= 3 {
                        Ok(PushPullBase::Tuplet(n))
                    } else {
                        Err(format!(
                            "Invalid tuplet value: '{}'. Must be 3 or greater",
                            value
                        ))
                    }
                } else {
                    Err(format!(
                        "Invalid push mode: '{}'. Expected 'standard', 'triplet', or a number",
                        value
                    ))
                }
            }
        }
    }

    /// Parse a boolean value from string
    fn parse_bool(value: &str) -> Result<bool, String> {
        match value.to_lowercase().as_str() {
            "true" | "1" | "yes" | "on" => Ok(true),
            "false" | "0" | "no" | "off" => Ok(false),
            _ => Err(format!(
                "Invalid boolean value: '{}'. Expected true/false",
                value
            )),
        }
    }

    /// Set a setting value
    pub fn set(&mut self, setting: ChartSetting, value: SettingValue) {
        self.settings.insert(setting, value);
    }

    /// Get a setting value
    pub fn get(&self, setting: ChartSetting) -> Option<&SettingValue> {
        self.settings.get(&setting)
    }

    /// Get a boolean setting (with default fallback)
    pub fn get_bool(&self, setting: ChartSetting) -> bool {
        match self.settings.get(&setting) {
            Some(SettingValue::Bool(b)) => *b,
            _ => false,
        }
    }

    /// Get a string setting (with default fallback)
    pub fn get_string(&self, setting: ChartSetting) -> Option<String> {
        match self.settings.get(&setting) {
            Some(SettingValue::String(s)) => Some(s.clone()),
            _ => None,
        }
    }

    /// Get a number setting (with default fallback)
    pub fn get_number(&self, setting: ChartSetting) -> Option<i32> {
        match self.settings.get(&setting) {
            Some(SettingValue::Number(n)) => Some(*n),
            _ => None,
        }
    }

    /// Check if smart repeats is enabled
    pub fn smart_repeats(&self) -> bool {
        self.get_bool(ChartSetting::SmartRepeats)
    }

    /// Check if auto rhythm slashes is enabled (default: true)
    ///
    /// When enabled, whole notes and half notes in rhythm charts are automatically
    /// expanded to quarter note slashes. For example:
    /// - A whole note chord becomes 4 quarter slashes
    /// - A half note chord becomes 2 quarter slashes
    ///
    /// This is standard notation for master rhythm charts.
    pub fn auto_rhythm_slashes(&self) -> bool {
        // Default to true if not explicitly set
        match self.settings.get(&ChartSetting::AutoRhythmSlashes) {
            Some(SettingValue::Bool(b)) => *b,
            _ => true, // Default ON
        }
    }

    /// Check if push alters rhythm is enabled (default: true)
    ///
    /// When enabled, pushed chords create triplet/syncopated rhythm notation
    /// showing exactly when the chord should be played.
    ///
    /// When disabled, pushed chords show simple apostrophe markers on the
    /// chord symbols (`'C` for push, `C'` for pull) in a contrasting color.
    /// The rhythm notation remains on-beat for simpler reading.
    pub fn push_alters_rhythm(&self) -> bool {
        // Default to true if not explicitly set
        match self.settings.get(&ChartSetting::PushAltersRhythm) {
            Some(SettingValue::Bool(b)) => *b,
            _ => true, // Default ON
        }
    }
}

impl Default for ChartSettings {
    fn default() -> Self {
        Self::new()
    }
}

impl ChartSetting {
    /// Get the display name for this setting
    pub fn name(&self) -> &'static str {
        match self {
            ChartSetting::SmartRepeats => "SMART_REPEATS",
            ChartSetting::PushMode => "PUSH",
            ChartSetting::AutoRhythmSlashes => "AUTO_RHYTHM_SLASHES",
            ChartSetting::PushAltersRhythm => "PUSH_ALTERS_RHYTHM",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_settings() {
        let settings = ChartSettings::new();
        assert!(!settings.smart_repeats());
    }

    #[test]
    fn test_parse_smart_repeats_true() {
        let mut settings = ChartSettings::new();
        settings.parse_setting_line("/SMART_REPEATS=true").unwrap();
        assert!(settings.smart_repeats());
    }

    #[test]
    fn test_parse_smart_repeats_false() {
        let mut settings = ChartSettings::new();
        settings.parse_setting_line("/SMART_REPEATS=false").unwrap();
        assert!(!settings.smart_repeats());
    }

    #[test]
    fn test_parse_bool_variations() {
        let mut settings = ChartSettings::new();

        // Test various true values
        settings.parse_setting_line("/SMART_REPEATS=1").unwrap();
        assert!(settings.smart_repeats());

        settings.parse_setting_line("/SMART_REPEATS=yes").unwrap();
        assert!(settings.smart_repeats());

        settings.parse_setting_line("/SMART_REPEATS=on").unwrap();
        assert!(settings.smart_repeats());

        // Test various false values
        settings.parse_setting_line("/SMART_REPEATS=0").unwrap();
        assert!(!settings.smart_repeats());

        settings.parse_setting_line("/SMART_REPEATS=no").unwrap();
        assert!(!settings.smart_repeats());

        settings.parse_setting_line("/SMART_REPEATS=off").unwrap();
        assert!(!settings.smart_repeats());
    }

    #[test]
    fn test_parse_invalid_setting() {
        let mut settings = ChartSettings::new();
        let result = settings.parse_setting_line("/UNKNOWN_SETTING=true");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_invalid_format() {
        let mut settings = ChartSettings::new();
        let result = settings.parse_setting_line("/SMART_REPEATS");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_invalid_bool() {
        let mut settings = ChartSettings::new();
        let result = settings.parse_setting_line("/SMART_REPEATS=maybe");
        assert!(result.is_err());
    }

    #[test]
    fn test_case_insensitive_setting_name() {
        let mut settings = ChartSettings::new();
        settings.parse_setting_line("/smart_repeats=true").unwrap();
        assert!(settings.smart_repeats());
    }

    #[test]
    fn test_whitespace_handling() {
        let mut settings = ChartSettings::new();
        settings
            .parse_setting_line("  /  SMART_REPEATS  =  true  ")
            .unwrap();
        assert!(settings.smart_repeats());
    }
}
