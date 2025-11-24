//! Multi-mic descriptor definitions
//!
//! Multi-mic descriptors (like "In", "Out", "Trig", "Sub", "Ambient") can have
//! their own patterns and negative patterns for matching.

/// A multi-mic descriptor with its own patterns
/// 
/// For example, the "In" descriptor might have patterns like ["Inside", "Internal", "Beater"]
/// to match various ways of expressing "inside mic" in track names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultiMicDescriptor {
    /// The descriptor name (e.g., "In", "Out", "Trig", "Sub", "Ambient")
    pub name: String,
    
    /// Patterns that match this descriptor (e.g., ["Inside", "Internal"] for "In")
    pub patterns: Vec<String>,
    
    /// Negative patterns to exclude (patterns that should NOT match this descriptor)
    pub negative_patterns: Vec<String>,
}

impl MultiMicDescriptor {
    /// Create a new multi-mic descriptor
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            patterns: Vec::new(),
            negative_patterns: Vec::new(),
        }
    }
    
    /// Add a pattern to this descriptor (combinator style)
    pub fn with_pattern(mut self, pattern: impl Into<String>) -> Self {
        self.patterns.push(pattern.into());
        self
    }
    
    /// Add multiple patterns to this descriptor (combinator style)
    pub fn with_patterns(mut self, patterns: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.patterns.extend(patterns.into_iter().map(|p| p.into()));
        self
    }
    
    /// Add a negative pattern to this descriptor (combinator style)
    pub fn with_negative_pattern(mut self, pattern: impl Into<String>) -> Self {
        self.negative_patterns.push(pattern.into());
        self
    }
    
    /// Add multiple negative patterns to this descriptor (combinator style)
    pub fn with_negative_patterns(mut self, patterns: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.negative_patterns.extend(patterns.into_iter().map(|p| p.into()));
        self
    }
    
    // Mutating methods (for backward compatibility)
    
    /// Add a pattern to this descriptor
    pub fn add_pattern(&mut self, pattern: impl Into<String>) {
        self.patterns.push(pattern.into());
    }
    
    /// Add a negative pattern to this descriptor
    pub fn add_negative_pattern(&mut self, pattern: impl Into<String>) {
        self.negative_patterns.push(pattern.into());
    }
    
    /// Set all patterns for this descriptor
    pub fn set_patterns(&mut self, patterns: Vec<String>) {
        self.patterns = patterns;
    }
    
    /// Set all negative patterns for this descriptor
    pub fn set_negative_patterns(&mut self, patterns: Vec<String>) {
        self.negative_patterns = patterns;
    }
    
    /// Check if this descriptor matches a given string
    pub fn matches(&self, text: &str) -> bool {
        let text_lower = text.to_lowercase();
        
        // Check negative patterns first (if any match, exclude)
        for neg_pattern in &self.negative_patterns {
            if text_lower.contains(&neg_pattern.to_lowercase()) {
                return false;
            }
        }
        
        // Check if the descriptor name itself matches
        if text_lower.contains(&self.name.to_lowercase()) {
            return true;
        }
        
        // Check if any pattern matches
        for pattern in &self.patterns {
            if text_lower.contains(&pattern.to_lowercase()) {
                return true;
            }
        }
        
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_multi_mic_descriptor_matches() {
        let mut in_desc = MultiMicDescriptor::new("In");
        in_desc.set_patterns(vec![
            "Inside".to_string(),
            "Internal".to_string(),
            "Beater".to_string(),
        ]);
        
        // Should match the descriptor name itself
        assert!(in_desc.matches("Kick In"));
        assert!(in_desc.matches("K IN"));
        
        // Should match patterns
        assert!(in_desc.matches("Kick Inside"));
        assert!(in_desc.matches("K Internal"));
        assert!(in_desc.matches("Beater"));
        
        // Should not match unrelated text
        assert!(!in_desc.matches("Kick Out"));
        assert!(!in_desc.matches("External"));
    }

    #[test]
    fn test_multi_mic_descriptor_negative_patterns() {
        let mut in_desc = MultiMicDescriptor::new("In");
        in_desc.set_patterns(vec!["Inside".to_string()]);
        in_desc.set_negative_patterns(vec!["Input".to_string()]);
        
        // Should match normal pattern
        assert!(in_desc.matches("Kick Inside"));
        
        // Should not match if negative pattern is present
        assert!(!in_desc.matches("Kick Input"));
        assert!(!in_desc.matches("Input Inside")); // Negative pattern takes precedence
    }
}

