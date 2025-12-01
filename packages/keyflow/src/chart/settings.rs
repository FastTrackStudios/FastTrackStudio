//! Chart Settings
//!
//! Configuration options for chart parsing and display

use std::collections::HashMap;
use serde::{Deserialize, Serialize};

/// Chart configuration settings
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ChartSettings {
    /// Internal settings storage
    settings: HashMap<ChartSetting, SettingValue>,
}

/// Available chart settings
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ChartSetting {
    /// Automatically group repeated phrases into 4-bar units with repeat signs
    SmartRepeats,
}

/// Setting value types
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

        Self { settings }
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
            _ => Err(format!("Unknown setting: '{}'", key)),
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
