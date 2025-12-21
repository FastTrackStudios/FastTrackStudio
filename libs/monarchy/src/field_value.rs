//! Field value descriptors for metadata fields
//!
//! Allows each metadata field value (e.g., "Out", "In", "Hi Hat", "Ride") to have
//! its own patterns and negative patterns, making them act more like separate groups.

use serde::{Deserialize, Serialize};

/// Helper trait to convert various input types into a Vec<String> for patterns
pub trait IntoPatterns {
    fn into_patterns(self) -> Vec<String>;
}

// Implementation for single &str
impl IntoPatterns for &str {
    fn into_patterns(self) -> Vec<String> {
        vec![self.to_string()]
    }
}

// Implementation for String
impl IntoPatterns for String {
    fn into_patterns(self) -> Vec<String> {
        vec![self]
    }
}

// Implementation for Vec<String>
impl IntoPatterns for Vec<String> {
    fn into_patterns(self) -> Vec<String> {
        self
    }
}

// Implementation for Vec<&str>
impl IntoPatterns for Vec<&str> {
    fn into_patterns(self) -> Vec<String> {
        self.into_iter().map(|s| s.to_string()).collect()
    }
}

// Implementation for arrays [&str; N]
impl<const N: usize> IntoPatterns for [&str; N] {
    fn into_patterns(self) -> Vec<String> {
        self.into_iter().map(|s| s.to_string()).collect()
    }
}

// Implementation for slices &[&str]
impl IntoPatterns for &[&str] {
    fn into_patterns(self) -> Vec<String> {
        self.iter().map(|s| (*s).to_string()).collect()
    }
}

/// Describes a specific value for a metadata field with its own patterns
/// 
/// This allows values like "Out", "Hi Hat", "Ride" to have their own matching rules
/// instead of just using simple string matching.
/// 
/// # Example
/// 
/// ```rust
/// FieldValueDescriptor::new("Out")
///     .patterns(["out", "ouut"])
///     .exclude(["outside", "outdoor"])
/// ```
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct FieldValueDescriptor {
    /// The value name (e.g., "Out", "Hi Hat", "Ride")
    pub value: String,
    
    /// Patterns that match this value (any of these will match)
    pub patterns: Vec<String>,
    
    /// Negative patterns to exclude (none of these should match)
    pub negative_patterns: Vec<String>,
}

impl FieldValueDescriptor {
    /// Create a new field value descriptor
    pub fn new(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            patterns: Vec::new(),
            negative_patterns: Vec::new(),
        }
    }
    
    /// Add patterns to match this value
    pub fn patterns<P>(mut self, patterns: P) -> Self
    where
        P: Into<Vec<String>>,
    {
        self.patterns = patterns.into();
        self
    }
    
    /// Add negative patterns to exclude
    pub fn exclude<P>(mut self, patterns: P) -> Self
    where
        P: Into<Vec<String>>,
    {
        self.negative_patterns = patterns.into();
        self
    }
    
    /// Check if a string matches this value descriptor
    pub fn matches(&self, text: &str) -> bool {
        let text_lower = text.to_lowercase();
        
        // Check negative patterns first
        for pattern in &self.negative_patterns {
            let pattern_lower = pattern.to_lowercase();
            if Self::contains_word(&text_lower, &pattern_lower) {
                return false;
            }
        }
        
        // If no patterns specified, match the value name directly as a word
        if self.patterns.is_empty() {
            return Self::contains_word(&text_lower, &self.value.to_lowercase());
        }
        
        // Check positive patterns
        for pattern in &self.patterns {
            let pattern_lower = pattern.to_lowercase();
            if Self::contains_word(&text_lower, &pattern_lower) {
                return true;
            }
        }
        
        false
    }
    
    /// Check if text contains pattern as a whole word (not just as a substring)
    /// A word is defined as a sequence of alphanumeric characters
    /// The pattern must be surrounded by non-alphanumeric characters or at the start/end of the string
    fn contains_word(text: &str, pattern: &str) -> bool {
        if pattern.is_empty() {
            return false;
        }

        // Find all occurrences of the pattern
        let mut start = 0;
        while let Some(pos) = text[start..].find(pattern) {
            let actual_pos = start + pos;
            let before_pos = if actual_pos > 0 { actual_pos - 1 } else { 0 };
            let after_pos = actual_pos + pattern.len();

            // Check if character before pattern is non-alphanumeric (or at start)
            let before_is_word_char = if actual_pos > 0 {
                text.chars().nth(before_pos).map_or(false, |c| c.is_alphanumeric())
            } else {
                false // At start, so no character before
            };

            // Check if character after pattern is non-alphanumeric (or at end)
            let after_is_word_char = if after_pos < text.len() {
                text.chars().nth(after_pos).map_or(false, |c| c.is_alphanumeric())
            } else {
                false // At end, so no character after
            };

            // Pattern is a whole word if it's not surrounded by word characters
            if !before_is_word_char && !after_is_word_char {
                return true;
            }

            // Continue searching from after this occurrence
            start = actual_pos + 1;
        }

        false
    }
}

/// Builder for field value descriptors
pub struct FieldValueDescriptorBuilder {
    descriptor: FieldValueDescriptor,
}

impl FieldValueDescriptorBuilder {
    /// Create a new builder
    pub fn new(value: impl Into<String>) -> Self {
        Self {
            descriptor: FieldValueDescriptor::new(value),
        }
    }
    
    /// Add patterns to match this value
    pub fn patterns<P>(mut self, patterns: P) -> Self
    where
        P: IntoPatterns,
    {
        self.descriptor.patterns = patterns.into_patterns();
        self
    }
    
    /// Add negative patterns to exclude
    pub fn exclude<P>(mut self, patterns: P) -> Self
    where
        P: IntoPatterns,
    {
        self.descriptor.negative_patterns = patterns.into_patterns();
        self
    }
    
    /// Build the descriptor
    pub fn build(self) -> FieldValueDescriptor {
        self.descriptor
    }
}

impl FieldValueDescriptor {
    /// Start building a new field value descriptor
    pub fn builder(value: impl Into<String>) -> FieldValueDescriptorBuilder {
        FieldValueDescriptorBuilder::new(value)
    }
}

