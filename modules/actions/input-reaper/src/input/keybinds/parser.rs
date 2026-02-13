//! Key Sequence Parser
//!
//! Parses key sequence notation into structured representations.
//!
//! ## Supported Formats
//!
//! - Simple keys: `a`, `1`, `space`, `tab`, `esc`, `enter`
//! - Modifiers: `<C-x>` (Ctrl), `<S-x>` (Shift), `<A-x>` (Alt/Option), `<M-x>` (Meta/Cmd)
//! - Combinations: `<C-S-a>` (Ctrl+Shift+A)
//! - Sequences: `gg` (press g twice), `gc` (g then c)
//! - Special: `<leader>` (configurable prefix key)

use facet::Facet;
use std::fmt;

/// Modifier keys
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Facet)]
pub struct Modifiers {
    pub ctrl: bool,
    pub shift: bool,
    pub alt: bool,
    pub meta: bool,
}

impl Modifiers {
    pub const NONE: Modifiers = Modifiers {
        ctrl: false,
        shift: false,
        alt: false,
        meta: false,
    };

    pub const CTRL: Modifiers = Modifiers {
        ctrl: true,
        shift: false,
        alt: false,
        meta: false,
    };

    pub const SHIFT: Modifiers = Modifiers {
        ctrl: false,
        shift: true,
        alt: false,
        meta: false,
    };

    pub const ALT: Modifiers = Modifiers {
        ctrl: false,
        shift: false,
        alt: true,
        meta: false,
    };

    pub const META: Modifiers = Modifiers {
        ctrl: false,
        shift: false,
        alt: false,
        meta: true,
    };

    /// Check if no modifiers are pressed
    pub fn is_empty(&self) -> bool {
        !self.ctrl && !self.shift && !self.alt && !self.meta
    }

    /// Create from individual modifier states
    pub fn new(ctrl: bool, shift: bool, alt: bool, meta: bool) -> Self {
        Self {
            ctrl,
            shift,
            alt,
            meta,
        }
    }
}

impl fmt::Display for Modifiers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();
        if self.ctrl {
            parts.push("C");
        }
        if self.shift {
            parts.push("S");
        }
        if self.alt {
            parts.push("A");
        }
        if self.meta {
            parts.push("M");
        }
        write!(f, "{}", parts.join("-"))
    }
}

/// A single parsed key with modifiers
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParsedKey {
    /// The base key (lowercase, or special name like "space", "tab")
    pub key: String,
    /// Active modifiers
    pub modifiers: Modifiers,
}

impl ParsedKey {
    pub fn new(key: impl Into<String>, modifiers: Modifiers) -> Self {
        Self {
            key: key.into(),
            modifiers,
        }
    }

    /// Create a simple key with no modifiers
    pub fn simple(key: impl Into<String>) -> Self {
        Self {
            key: key.into(),
            modifiers: Modifiers::NONE,
        }
    }
}

impl fmt::Display for ParsedKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.modifiers.is_empty() {
            // Simple key
            write!(f, "{}", self.key)
        } else {
            // Key with modifiers
            write!(f, "<{}-{}>", self.modifiers, self.key)
        }
    }
}

/// A sequence of keys (for multi-key bindings like "gg")
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct KeySequence {
    pub keys: Vec<ParsedKey>,
}

impl KeySequence {
    pub fn new(keys: Vec<ParsedKey>) -> Self {
        Self { keys }
    }

    /// Parse a key sequence string into a KeySequence
    pub fn parse(input: &str) -> Result<Self, ParseError> {
        let mut keys = Vec::new();
        let mut chars = input.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '<' {
                // Parse bracketed key like <C-S-a> or <space>
                let mut bracket_content = String::new();
                let mut found_close = false;

                for c in chars.by_ref() {
                    if c == '>' {
                        found_close = true;
                        break;
                    }
                    bracket_content.push(c);
                }

                if !found_close {
                    return Err(ParseError::UnclosedBracket);
                }

                keys.push(parse_bracketed_key(&bracket_content)?);
            } else {
                // Simple single character key
                keys.push(ParsedKey::simple(c.to_lowercase().to_string()));
            }
        }

        if keys.is_empty() {
            return Err(ParseError::EmptySequence);
        }

        Ok(Self { keys })
    }

    /// Check if this sequence is a single key
    pub fn is_single(&self) -> bool {
        self.keys.len() == 1
    }

    /// Get the single key if this is a single-key sequence
    pub fn single(&self) -> Option<&ParsedKey> {
        if self.is_single() {
            self.keys.first()
        } else {
            None
        }
    }

    /// Check if this sequence starts with another sequence (prefix matching)
    pub fn starts_with(&self, prefix: &KeySequence) -> bool {
        if prefix.keys.len() > self.keys.len() {
            return false;
        }
        self.keys[..prefix.keys.len()] == prefix.keys
    }
}

impl fmt::Display for KeySequence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for key in &self.keys {
            write!(f, "{key}")?;
        }
        Ok(())
    }
}

/// Parse error for key sequences
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    UnclosedBracket,
    EmptySequence,
    InvalidModifier(String),
    EmptyKey,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnclosedBracket => write!(f, "Unclosed bracket in key sequence"),
            ParseError::EmptySequence => write!(f, "Empty key sequence"),
            ParseError::InvalidModifier(m) => write!(f, "Invalid modifier: {m}"),
            ParseError::EmptyKey => write!(f, "Empty key in sequence"),
        }
    }
}

impl std::error::Error for ParseError {}

/// Parse bracketed content like "C-S-a" or "space"
fn parse_bracketed_key(content: &str) -> Result<ParsedKey, ParseError> {
    // Check for special keys without modifiers
    let lower = content.to_lowercase();
    if let Some(special) = parse_special_key(&lower) {
        return Ok(ParsedKey::simple(special));
    }

    // Parse modifiers and key
    let parts: Vec<&str> = content.split('-').collect();

    if parts.is_empty() {
        return Err(ParseError::EmptyKey);
    }

    let mut modifiers = Modifiers::NONE;
    let mut key_part = None;

    for (i, part) in parts.iter().enumerate() {
        let upper = part.to_uppercase();

        // Check if this is a modifier
        match upper.as_str() {
            "C" | "CTRL" | "CONTROL" => modifiers.ctrl = true,
            "S" | "SHIFT" => modifiers.shift = true,
            "A" | "ALT" | "OPT" | "OPTION" => modifiers.alt = true,
            "M" | "META" | "CMD" | "COMMAND" | "WIN" | "SUPER" => modifiers.meta = true,
            _ => {
                // Not a modifier - must be the key
                if i == parts.len() - 1 {
                    // Last part is the key
                    let lower_part = part.to_lowercase();
                    key_part = Some(parse_special_key(&lower_part).unwrap_or(lower_part));
                } else {
                    // Unknown modifier in middle
                    return Err(ParseError::InvalidModifier(part.to_string()));
                }
            }
        }
    }

    let key = key_part.ok_or(ParseError::EmptyKey)?;
    Ok(ParsedKey::new(key, modifiers))
}

/// Parse special key names
fn parse_special_key(name: &str) -> Option<String> {
    match name {
        "space" | "spc" => Some("space".to_string()),
        "tab" => Some("tab".to_string()),
        "enter" | "return" | "ret" | "cr" => Some("enter".to_string()),
        "esc" | "escape" => Some("esc".to_string()),
        "backspace" | "bs" => Some("backspace".to_string()),
        "delete" | "del" => Some("delete".to_string()),
        "up" => Some("up".to_string()),
        "down" => Some("down".to_string()),
        "left" => Some("left".to_string()),
        "right" => Some("right".to_string()),
        "home" => Some("home".to_string()),
        "end" => Some("end".to_string()),
        "pageup" | "pgup" => Some("pageup".to_string()),
        "pagedown" | "pgdn" => Some("pagedown".to_string()),
        "insert" | "ins" => Some("insert".to_string()),
        "f1" | "f2" | "f3" | "f4" | "f5" | "f6" | "f7" | "f8" | "f9" | "f10" | "f11" | "f12" => {
            Some(name.to_string())
        }
        "leader" => Some("leader".to_string()),
        "plus" => Some("+".to_string()),
        "minus" => Some("-".to_string()),
        _ => None,
    }
}

/// Format a key event into a normalized key sequence string
pub fn format_key_event(key: &str, modifiers: Modifiers) -> String {
    let parsed = ParsedKey::new(key.to_lowercase(), modifiers);
    parsed.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_key() {
        let seq = KeySequence::parse("a").unwrap();
        assert_eq!(seq.keys.len(), 1);
        assert_eq!(seq.keys[0].key, "a");
        assert!(seq.keys[0].modifiers.is_empty());
    }

    #[test]
    fn test_parse_sequence() {
        let seq = KeySequence::parse("gg").unwrap();
        assert_eq!(seq.keys.len(), 2);
        assert_eq!(seq.keys[0].key, "g");
        assert_eq!(seq.keys[1].key, "g");
    }

    #[test]
    fn test_parse_ctrl_key() {
        let seq = KeySequence::parse("<C-s>").unwrap();
        assert_eq!(seq.keys.len(), 1);
        assert_eq!(seq.keys[0].key, "s");
        assert!(seq.keys[0].modifiers.ctrl);
        assert!(!seq.keys[0].modifiers.shift);
    }

    #[test]
    fn test_parse_shift_key() {
        let seq = KeySequence::parse("<S-g>").unwrap();
        assert_eq!(seq.keys.len(), 1);
        assert_eq!(seq.keys[0].key, "g");
        assert!(seq.keys[0].modifiers.shift);
    }

    #[test]
    fn test_parse_multi_modifier() {
        let seq = KeySequence::parse("<C-S-a>").unwrap();
        assert_eq!(seq.keys.len(), 1);
        assert_eq!(seq.keys[0].key, "a");
        assert!(seq.keys[0].modifiers.ctrl);
        assert!(seq.keys[0].modifiers.shift);
    }

    #[test]
    fn test_parse_special_key() {
        let seq = KeySequence::parse("<space>").unwrap();
        assert_eq!(seq.keys.len(), 1);
        assert_eq!(seq.keys[0].key, "space");
    }

    #[test]
    fn test_parse_ctrl_special() {
        let seq = KeySequence::parse("<C-space>").unwrap();
        assert_eq!(seq.keys.len(), 1);
        assert_eq!(seq.keys[0].key, "space");
        assert!(seq.keys[0].modifiers.ctrl);
    }

    #[test]
    fn test_parse_mixed_sequence() {
        let seq = KeySequence::parse("g<C-a>").unwrap();
        assert_eq!(seq.keys.len(), 2);
        assert_eq!(seq.keys[0].key, "g");
        assert!(seq.keys[0].modifiers.is_empty());
        assert_eq!(seq.keys[1].key, "a");
        assert!(seq.keys[1].modifiers.ctrl);
    }

    #[test]
    fn test_display_simple() {
        let seq = KeySequence::parse("abc").unwrap();
        assert_eq!(seq.to_string(), "abc");
    }

    #[test]
    fn test_display_with_modifiers() {
        let seq = KeySequence::parse("<C-S-a>").unwrap();
        assert_eq!(seq.to_string(), "<C-S-a>");
    }
}
