//! Which-Key style key sequence resolution
//!
//! A trie-based key sequence accumulator that intercepts prefix keys and
//! shows available continuations. When a complete sequence is matched,
//! the associated action is fired.
//!
//! ## Flow
//!
//! 1. User presses `v` (a registered prefix) → `PartialMatch` with children
//! 2. User presses `d` (a leaf under `v`) → `FullMatch` with action
//! 3. Or user presses `esc` → `Cancelled`
//! 4. Or timeout expires → auto-reset

use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};
use std::time::Instant;
use tracing::info;

// ---------------------------------------------------------------------------
// Data types
// ---------------------------------------------------------------------------

/// A node in the which-key trie.
#[derive(Debug, Clone)]
pub enum WhichKeyNode {
    /// Intermediate node — pressing this key reveals more options.
    Branch {
        key: String,
        label: String,
        children: HashMap<String, WhichKeyNode>,
    },
    /// Terminal node — pressing this key fires the action.
    Leaf {
        key: String,
        label: String,
        action: String,
    },
}

/// Declarative entry for building a which-key tree.
#[derive(Debug, Clone)]
pub enum WhichKeyEntry {
    Leaf {
        key: String,
        label: String,
        action: String,
    },
    Branch {
        key: String,
        label: String,
        children: Vec<WhichKeyEntry>,
    },
}

impl WhichKeyEntry {
    pub fn leaf(key: &str, label: &str, action: &str) -> Self {
        Self::Leaf {
            key: key.into(),
            label: label.into(),
            action: action.into(),
        }
    }

    pub fn branch(key: &str, label: &str, children: Vec<WhichKeyEntry>) -> Self {
        Self::Branch {
            key: key.into(),
            label: label.into(),
            children,
        }
    }

    /// Convert this entry into a `WhichKeyNode`.
    fn into_node(self) -> (String, WhichKeyNode) {
        match self {
            WhichKeyEntry::Leaf { key, label, action } => {
                (key.clone(), WhichKeyNode::Leaf { key, label, action })
            }
            WhichKeyEntry::Branch {
                key,
                label,
                children,
            } => {
                let child_map: HashMap<String, WhichKeyNode> =
                    children.into_iter().map(|e| e.into_node()).collect();
                (
                    key.clone(),
                    WhichKeyNode::Branch {
                        key,
                        label,
                        children: child_map,
                    },
                )
            }
        }
    }
}

/// Result of feeding a key into the accumulator.
#[derive(Debug)]
pub enum WhichKeyResult {
    /// Complete sequence matched — fire this action.
    FullMatch {
        action: String,
        label: String,
        sequence: String,
    },
    /// Partial match — show these continuations.
    /// Each tuple is (key, label, is_branch).
    PartialMatch {
        continuations: Vec<(String, String, bool)>,
        sequence: String,
    },
    /// This key is not a which-key prefix — fall through to normal resolution.
    NotAPrefix,
    /// Invalid continuation key broke the sequence — reset occurred.
    Broken { sequence: String },
    /// User pressed Escape to cancel mid-sequence.
    Cancelled,
}

// ---------------------------------------------------------------------------
// Trie
// ---------------------------------------------------------------------------

/// The complete which-key trie (all prefix trees merged).
#[derive(Debug, Clone, Default)]
pub struct WhichKeyTrie {
    /// Top-level prefix keys → their branch nodes.
    roots: HashMap<String, WhichKeyNode>,
}

impl WhichKeyTrie {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a prefix tree from declarative entries.
    pub fn add_tree(&mut self, prefix: &str, label: &str, entries: Vec<WhichKeyEntry>) {
        let children: HashMap<String, WhichKeyNode> =
            entries.into_iter().map(|e| e.into_node()).collect();
        self.roots.insert(
            prefix.to_string(),
            WhichKeyNode::Branch {
                key: prefix.to_string(),
                label: label.to_string(),
                children,
            },
        );
    }

    /// Remove all registered trees.
    pub fn clear(&mut self) {
        self.roots.clear();
    }

    /// Check if a key is a registered root prefix.
    pub fn is_prefix(&self, key: &str) -> bool {
        self.roots.contains_key(key)
    }

    /// Walk the trie along a sequence of keys.
    /// Returns the node at the end of the path, or None if the path is invalid.
    fn walk(&self, keys: &[String]) -> Option<&WhichKeyNode> {
        if keys.is_empty() {
            return None;
        }

        let mut current = self.roots.get(&keys[0])?;

        for key in &keys[1..] {
            match current {
                WhichKeyNode::Branch { children, .. } => {
                    current = children.get(key)?;
                }
                WhichKeyNode::Leaf { .. } => {
                    // Can't walk further into a leaf.
                    return None;
                }
            }
        }

        Some(current)
    }
}

/// Extract continuations (immediate children) from a node.
fn node_continuations(node: &WhichKeyNode) -> Vec<(String, String, bool)> {
    match node {
        WhichKeyNode::Branch { children, .. } => {
            let mut result: Vec<(String, String, bool)> = children
                .values()
                .map(|child| match child {
                    WhichKeyNode::Branch { key, label, .. } => (key.clone(), label.clone(), true),
                    WhichKeyNode::Leaf { key, label, .. } => (key.clone(), label.clone(), false),
                })
                .collect();
            // Sort alphabetically by key for consistent display.
            result.sort_by(|a, b| a.0.cmp(&b.0));
            result
        }
        WhichKeyNode::Leaf { .. } => Vec::new(),
    }
}

// ---------------------------------------------------------------------------
// State accumulator
// ---------------------------------------------------------------------------

/// Accumulator state for which-key sequences.
struct WhichKeyState {
    /// Current accumulated key sequence (e.g., ["v"], ["f", "e"]).
    current_sequence: Vec<String>,
    /// When the last key was pressed (for timeout).
    last_keypress: Option<Instant>,
    /// The trie to resolve against.
    trie: WhichKeyTrie,
}

impl WhichKeyState {
    fn new() -> Self {
        Self {
            current_sequence: Vec::new(),
            last_keypress: None,
            trie: WhichKeyTrie::new(),
        }
    }

    fn is_active(&self) -> bool {
        !self.current_sequence.is_empty()
    }

    fn reset(&mut self) {
        self.current_sequence.clear();
        self.last_keypress = None;
    }

    fn sequence_string(&self) -> String {
        self.current_sequence.join("")
    }

    fn feed_key(&mut self, key: &str) -> WhichKeyResult {
        // Keys with modifier brackets break any pending sequence.
        // Exception: bare shift (uppercase letters like "V") is fine.
        if key.starts_with('<') {
            if self.is_active() {
                let seq = self.sequence_string();
                self.reset();
                return WhichKeyResult::Broken { sequence: seq };
            }
            return WhichKeyResult::NotAPrefix;
        }

        // Escape cancels a pending sequence.
        if key == "esc" {
            if self.is_active() {
                self.reset();
                return WhichKeyResult::Cancelled;
            }
            return WhichKeyResult::NotAPrefix;
        }

        // Starting a new sequence?
        if !self.is_active() {
            if !self.trie.is_prefix(key) {
                return WhichKeyResult::NotAPrefix;
            }
            // Enter prefix mode.
            self.current_sequence.push(key.to_string());
            self.last_keypress = Some(Instant::now());
            let node = self.trie.walk(&self.current_sequence).unwrap();
            let continuations = node_continuations(node);
            return WhichKeyResult::PartialMatch {
                continuations,
                sequence: self.sequence_string(),
            };
        }

        // Mid-sequence: try to advance.
        self.current_sequence.push(key.to_string());
        self.last_keypress = Some(Instant::now());

        match self.trie.walk(&self.current_sequence) {
            Some(WhichKeyNode::Leaf { label, action, .. }) => {
                let seq = self.sequence_string();
                let label = label.clone();
                let action = action.clone();
                self.reset();
                WhichKeyResult::FullMatch {
                    action,
                    label,
                    sequence: seq,
                }
            }
            Some(node @ WhichKeyNode::Branch { .. }) => {
                let continuations = node_continuations(node);
                WhichKeyResult::PartialMatch {
                    continuations,
                    sequence: self.sequence_string(),
                }
            }
            None => {
                // Invalid key — remove it and report broken.
                self.current_sequence.pop();
                let seq = self.sequence_string();
                self.reset();
                WhichKeyResult::Broken { sequence: seq }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Global state + public API
// ---------------------------------------------------------------------------

fn get_state() -> &'static Mutex<WhichKeyState> {
    static STATE: OnceLock<Mutex<WhichKeyState>> = OnceLock::new();
    STATE.get_or_init(|| Mutex::new(WhichKeyState::new()))
}

/// Feed a key into the which-key accumulator.
pub fn feed_key(key: &str) -> WhichKeyResult {
    let mut state = get_state().lock().unwrap();
    state.feed_key(key)
}

/// Check for timeout and reset if the sequence has been idle too long.
/// Returns `true` if a timeout occurred (and the state was reset).
pub fn check_timeout(timeout_ms: u64) -> bool {
    let mut state = get_state().lock().unwrap();
    if !state.is_active() {
        return false;
    }
    if let Some(last) = state.last_keypress {
        if last.elapsed().as_millis() as u64 > timeout_ms {
            let seq = state.sequence_string();
            state.reset();
            info!(sequence = %seq, "Which-Key: sequence timed out");
            return true;
        }
    }
    false
}

/// Register a which-key prefix tree.
pub fn register_tree(prefix: &str, label: &str, entries: Vec<WhichKeyEntry>) {
    let mut state = get_state().lock().unwrap();
    state.trie.add_tree(prefix, label, entries);
    info!(prefix = %prefix, label = %label, "Which-Key: registered tree");
}

/// Remove all which-key trees (call when switching presets).
pub fn clear_trees() {
    let mut state = get_state().lock().unwrap();
    state.trie.clear();
    state.reset();
    info!("Which-Key: cleared all trees");
}

/// Check if the which-key accumulator is currently mid-sequence.
pub fn is_active() -> bool {
    let state = get_state().lock().unwrap();
    state.is_active()
}

/// Get the label for a root prefix key (e.g., "v" → "Visibility Manager").
pub fn root_label(key: &str) -> Option<String> {
    let state = get_state().lock().unwrap();
    state.trie.roots.get(key).map(|node| match node {
        WhichKeyNode::Branch { label, .. } | WhichKeyNode::Leaf { label, .. } => label.clone(),
    })
}

/// Return all registered root prefix keys with their labels, sorted by key.
/// Each tuple is `(key, label, is_branch)` — always `true` for roots.
pub fn all_roots() -> Vec<(String, String, bool)> {
    let state = get_state().lock().unwrap();
    let mut roots: Vec<(String, String, bool)> = state
        .trie
        .roots
        .iter()
        .map(|(key, node)| {
            let label = match node {
                WhichKeyNode::Branch { label, .. } | WhichKeyNode::Leaf { label, .. } => {
                    label.clone()
                }
            };
            let is_branch = matches!(node, WhichKeyNode::Branch { .. });
            (key.clone(), label, is_branch)
        })
        .collect();
    roots.sort_by(|a, b| a.0.cmp(&b.0));
    roots
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_trie() -> WhichKeyTrie {
        let mut trie = WhichKeyTrie::new();
        trie.add_tree(
            "v",
            "Visibility",
            vec![
                WhichKeyEntry::leaf("d", "Drums", "vis:drums"),
                WhichKeyEntry::leaf("b", "Bass", "vis:bass"),
            ],
        );
        trie.add_tree(
            "f",
            "FX Manager",
            vec![
                WhichKeyEntry::branch(
                    "e",
                    "EQ/Rescue",
                    vec![
                        WhichKeyEntry::leaf("r", "Rescue EQ", "fx:rescue_eq"),
                        WhichKeyEntry::leaf("q", "Relational EQ", "fx:relational_eq"),
                    ],
                ),
                WhichKeyEntry::leaf("r", "Reverb", "fx:reverb"),
            ],
        );
        trie
    }

    #[test]
    fn test_single_level_full_match() {
        let mut state = WhichKeyState {
            current_sequence: Vec::new(),
            last_keypress: None,
            trie: test_trie(),
        };

        // Press "v" — partial match
        let result = state.feed_key("v");
        assert!(matches!(result, WhichKeyResult::PartialMatch { .. }));

        // Press "d" — full match
        let result = state.feed_key("d");
        match result {
            WhichKeyResult::FullMatch {
                action,
                label,
                sequence,
            } => {
                assert_eq!(action, "vis:drums");
                assert_eq!(label, "Drums");
                assert_eq!(sequence, "vd");
            }
            _ => panic!("Expected FullMatch, got {:?}", result),
        }

        // State should be reset
        assert!(!state.is_active());
    }

    #[test]
    fn test_nested_full_match() {
        let mut state = WhichKeyState {
            current_sequence: Vec::new(),
            last_keypress: None,
            trie: test_trie(),
        };

        // Press "f" — partial match
        let result = state.feed_key("f");
        assert!(matches!(result, WhichKeyResult::PartialMatch { .. }));

        // Press "e" — still partial (branch)
        let result = state.feed_key("e");
        assert!(matches!(result, WhichKeyResult::PartialMatch { .. }));

        // Press "r" — full match
        let result = state.feed_key("r");
        match result {
            WhichKeyResult::FullMatch {
                action, sequence, ..
            } => {
                assert_eq!(action, "fx:rescue_eq");
                assert_eq!(sequence, "fer");
            }
            _ => panic!("Expected FullMatch, got {:?}", result),
        }
    }

    #[test]
    fn test_not_a_prefix() {
        let mut state = WhichKeyState {
            current_sequence: Vec::new(),
            last_keypress: None,
            trie: test_trie(),
        };

        let result = state.feed_key("x");
        assert!(matches!(result, WhichKeyResult::NotAPrefix));
    }

    #[test]
    fn test_broken_sequence() {
        let mut state = WhichKeyState {
            current_sequence: Vec::new(),
            last_keypress: None,
            trie: test_trie(),
        };

        state.feed_key("v"); // enter prefix
        let result = state.feed_key("z"); // invalid continuation
        match result {
            WhichKeyResult::Broken { sequence } => assert_eq!(sequence, "v"),
            _ => panic!("Expected Broken, got {:?}", result),
        }
        assert!(!state.is_active());
    }

    #[test]
    fn test_escape_cancels() {
        let mut state = WhichKeyState {
            current_sequence: Vec::new(),
            last_keypress: None,
            trie: test_trie(),
        };

        state.feed_key("v");
        let result = state.feed_key("esc");
        assert!(matches!(result, WhichKeyResult::Cancelled));
        assert!(!state.is_active());
    }

    #[test]
    fn test_modifier_breaks_sequence() {
        let mut state = WhichKeyState {
            current_sequence: Vec::new(),
            last_keypress: None,
            trie: test_trie(),
        };

        state.feed_key("v");
        let result = state.feed_key("<C-s>");
        match result {
            WhichKeyResult::Broken { sequence } => assert_eq!(sequence, "v"),
            _ => panic!("Expected Broken, got {:?}", result),
        }
    }

    #[test]
    fn test_modifier_not_prefix() {
        let mut state = WhichKeyState {
            current_sequence: Vec::new(),
            last_keypress: None,
            trie: test_trie(),
        };

        let result = state.feed_key("<C-v>");
        assert!(matches!(result, WhichKeyResult::NotAPrefix));
    }
}
