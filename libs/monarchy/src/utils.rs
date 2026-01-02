//! Utility functions shared across the monarchy crate

/// Check if text contains pattern as a whole word (not just as a substring)
/// 
/// A word is defined as a sequence of alphanumeric characters.
/// The pattern must be surrounded by non-alphanumeric characters or be at the start/end of the string.
/// 
/// # Examples
/// 
/// ```
/// use monarchy::utils::contains_word;
/// 
/// assert!(contains_word("kick_in", "kick"));
/// assert!(contains_word("Kick In", "kick"));
/// assert!(contains_word("kick", "kick"));
/// assert!(!contains_word("kickstart", "kick"));
/// assert!(!contains_word("sidekick", "kick"));
/// ```
pub fn contains_word(text: &str, pattern: &str) -> bool {
    if pattern.is_empty() {
        return false;
    }

    let text_lower = text.to_lowercase();
    let pattern_lower = pattern.to_lowercase();
    
    // Find all occurrences of the pattern
    let mut start = 0;
    while let Some(pos) = text_lower[start..].find(&pattern_lower) {
        let actual_pos = start + pos;
        let after_pos = actual_pos + pattern_lower.len();

        // Check if character before pattern is non-alphanumeric (or at start)
        let before_is_word_char = if actual_pos > 0 {
            text.chars().nth(actual_pos - 1).map_or(false, |c| c.is_alphanumeric())
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_contains_word_basic() {
        assert!(contains_word("kick", "kick"));
        assert!(contains_word("Kick", "kick"));
        assert!(contains_word("KICK", "kick"));
    }

    #[test]
    fn test_contains_word_with_separators() {
        assert!(contains_word("kick_in", "kick"));
        assert!(contains_word("kick-in", "kick"));
        assert!(contains_word("kick in", "kick"));
        assert!(contains_word("Kick In", "in"));
    }

    #[test]
    fn test_contains_word_at_boundaries() {
        assert!(contains_word("kick", "kick"));
        assert!(contains_word("the kick drum", "kick"));
        assert!(contains_word("kick drum", "kick"));
        assert!(contains_word("the kick", "kick"));
    }

    #[test]
    fn test_contains_word_not_substring() {
        assert!(!contains_word("kickstart", "kick"));
        assert!(!contains_word("sidekick", "kick"));
        assert!(!contains_word("kicking", "kick"));
    }

    #[test]
    fn test_contains_word_empty_pattern() {
        assert!(!contains_word("kick", ""));
    }

    #[test]
    fn test_contains_word_pattern_longer_than_text() {
        assert!(!contains_word("ki", "kick"));
    }
}
