//! Trie (prefix tree) for multi-key sequence lookup.
//!
//! Each node in the trie represents a position in a key sequence.
//! Leaf nodes carry the action to execute; intermediate nodes indicate
//! that the sequence is a prefix of one or more bindings.

use std::collections::HashMap;

use crate::command::{ActionId, InputCommand};
use crate::key::KeyChord;

// region: --- KeyTrie

/// A trie node for key sequence dispatch.
///
/// The trie maps sequences of `KeyChord`s to `InputCommand`s.
/// Intermediate nodes (with children but no binding) represent
/// prefixes of longer sequences.
#[derive(Debug, Clone)]
pub struct KeyTrie {
    children: HashMap<KeyChord, KeyTrie>,
    binding: Option<InputCommand>,
}

/// Result of looking up a key sequence in the trie.
#[derive(Debug)]
pub enum TrieLookup {
    /// Exact match: the sequence maps to a command.
    Match(InputCommand),
    /// Prefix match: the sequence is a prefix of one or more bindings.
    Prefix,
    /// No match: the sequence doesn't match any binding or prefix.
    Miss,
}

impl KeyTrie {
    /// Create an empty trie node.
    pub fn new() -> Self {
        Self {
            children: HashMap::new(),
            binding: None,
        }
    }

    /// Insert a key sequence → command binding.
    ///
    /// If the sequence is empty, sets the binding on this node.
    /// Otherwise, recurses into the appropriate child.
    pub fn insert(&mut self, sequence: &[KeyChord], command: InputCommand) {
        if sequence.is_empty() {
            self.binding = Some(command);
            return;
        }

        let child = self
            .children
            .entry(sequence[0].clone())
            .or_default();
        child.insert(&sequence[1..], command);
    }

    /// Look up a key sequence in the trie.
    pub fn lookup(&self, sequence: &[KeyChord]) -> TrieLookup {
        if sequence.is_empty() {
            return if self.binding.is_some() {
                TrieLookup::Match(self.binding.clone().unwrap())
            } else if !self.children.is_empty() {
                TrieLookup::Prefix
            } else {
                TrieLookup::Miss
            };
        }

        match self.children.get(&sequence[0]) {
            Some(child) => child.lookup(&sequence[1..]),
            None => TrieLookup::Miss,
        }
    }

    /// Bind a single-key action (convenience method).
    pub fn bind(&mut self, chord: KeyChord, action: ActionId) {
        self.insert(&[chord], InputCommand::Action(action));
    }

    /// Bind a key sequence to an action (convenience method).
    pub fn bind_sequence(&mut self, chords: Vec<KeyChord>, action: ActionId) {
        self.insert(&chords, InputCommand::Action(action));
    }

    /// Bind a key chord to a mode switch command.
    pub fn bind_mode_switch(&mut self, chord: KeyChord, mode_id: crate::mode::ModeId) {
        self.insert(&[chord], InputCommand::SwitchMode(mode_id));
    }

    /// Bind a key chord to a push-mode command.
    pub fn bind_push_mode(&mut self, chord: KeyChord, mode_id: crate::mode::ModeId) {
        self.insert(&[chord], InputCommand::PushMode(mode_id));
    }
}

impl Default for KeyTrie {
    fn default() -> Self {
        Self::new()
    }
}

// endregion: --- KeyTrie

// region: --- Tests

#[cfg(test)]
mod tests {
    use super::*;
    use crate::key::{KeyCode, Modifiers};

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    fn chord(ch: char) -> KeyChord {
        KeyChord::plain(KeyCode::Character(ch.to_string()))
    }

    #[test]
    fn test_trie_single_key_lookup() -> Result<()> {
        // -- Setup
        let mut trie = KeyTrie::new();
        trie.bind(chord('j'), ActionId::new("cursor.down"));

        // -- Exec & Check
        assert!(matches!(trie.lookup(&[chord('j')]), TrieLookup::Match(_)));
        assert!(matches!(trie.lookup(&[chord('k')]), TrieLookup::Miss));

        Ok(())
    }

    #[test]
    fn test_trie_multi_key_sequence() -> Result<()> {
        // -- Setup
        let mut trie = KeyTrie::new();
        trie.bind_sequence(vec![chord('g'), chord('g')], ActionId::new("cursor.top"));

        // -- Exec & Check
        assert!(matches!(trie.lookup(&[chord('g')]), TrieLookup::Prefix));
        assert!(matches!(
            trie.lookup(&[chord('g'), chord('g')]),
            TrieLookup::Match(_)
        ));
        assert!(matches!(
            trie.lookup(&[chord('g'), chord('j')]),
            TrieLookup::Miss
        ));

        Ok(())
    }

    #[test]
    fn test_trie_with_modifiers() -> Result<()> {
        // -- Setup
        let mut trie = KeyTrie::new();
        let ctrl_s = KeyChord::new(
            KeyCode::Character("s".into()),
            Modifiers {
                ctrl: true,
                ..Modifiers::NONE
            },
        );
        trie.bind(ctrl_s.clone(), ActionId::new("file.save"));

        // -- Exec & Check
        assert!(matches!(trie.lookup(&[ctrl_s]), TrieLookup::Match(_)));
        // Plain 's' should miss
        assert!(matches!(trie.lookup(&[chord('s')]), TrieLookup::Miss));

        Ok(())
    }

    #[test]
    fn test_trie_mode_switch_binding() -> Result<()> {
        // -- Setup
        use crate::mode::ModeId;
        let mut trie = KeyTrie::new();
        trie.bind_mode_switch(chord('i'), ModeId::insert());

        // -- Exec & Check
        match trie.lookup(&[chord('i')]) {
            TrieLookup::Match(InputCommand::SwitchMode(mode)) => {
                assert_eq!(mode, ModeId::insert());
            }
            other => panic!("expected SwitchMode, got {:?}", other),
        }

        Ok(())
    }
}

// endregion: --- Tests
