//! Core input processing state machine.
//!
//! The `InputProcessor` owns the mode stack, per-mode keymaps (tries),
//! a sequence accumulator, and timeout configuration. It is the main
//! entry point for translating raw input events into `InputCommand`s.

use std::collections::HashMap;
use std::time::Duration;

use crate::command::InputCommand;
use crate::context::ActionContext;
use crate::event::InputEvent;
use crate::key::{KeyChord, KeyCode};
use crate::mode::{ModeDefinition, ModeId, ModeStack};
use crate::trie::{KeyTrie, TrieLookup};

// region: --- SequenceState

/// Tracks the in-progress key sequence for multi-key bindings.
#[derive(Debug)]
pub struct SequenceState {
    /// Accumulated key chords so far.
    keys: Vec<KeyChord>,
}

impl SequenceState {
    fn new() -> Self {
        Self { keys: Vec::new() }
    }

    /// Push a key chord onto the sequence.
    fn push(&mut self, chord: KeyChord) {
        self.keys.push(chord);
    }

    /// Clear the accumulated sequence.
    fn clear(&mut self) {
        self.keys.clear();
    }

    /// Whether there is a pending (non-empty) sequence.
    fn is_pending(&self) -> bool {
        !self.keys.is_empty()
    }

    /// Get the accumulated keys as a slice.
    fn keys(&self) -> &[KeyChord] {
        &self.keys
    }

    /// Build a display string for the pending keys (e.g., "g" while waiting for second key).
    fn display(&self) -> String {
        self.keys
            .iter()
            .map(chord_display)
            .collect::<Vec<_>>()
            .join("")
    }
}

/// Format a key chord for display in the pending indicator.
fn chord_display(chord: &KeyChord) -> String {
    let mut s = String::new();
    if chord.modifiers.ctrl {
        s.push_str("C-");
    }
    if chord.modifiers.alt {
        s.push_str("A-");
    }
    if chord.modifiers.shift {
        s.push_str("S-");
    }
    if chord.modifiers.meta {
        s.push_str("M-");
    }
    match &chord.key {
        KeyCode::Character(c) => s.push_str(c),
        KeyCode::Escape => s.push_str("Esc"),
        KeyCode::Enter => s.push_str("Enter"),
        KeyCode::Tab => s.push_str("Tab"),
        KeyCode::Backspace => s.push_str("BS"),
        KeyCode::Delete => s.push_str("Del"),
        KeyCode::ArrowUp => s.push_str("Up"),
        KeyCode::ArrowDown => s.push_str("Down"),
        KeyCode::ArrowLeft => s.push_str("Left"),
        KeyCode::ArrowRight => s.push_str("Right"),
        KeyCode::F(n) => {
            s.push('F');
            s.push_str(&n.to_string());
        }
    }
    s
}

// endregion: --- SequenceState

// region: --- InputProcessor

/// The core input processing state machine.
///
/// Owns the mode stack, per-mode keymaps, sequence accumulator, and timeout
/// configuration. Translates raw `InputEvent`s into `InputCommand`s.
pub struct InputProcessor {
    modes: ModeStack,
    keymaps: HashMap<ModeId, KeyTrie>,
    sequence: SequenceState,
    timeout: Duration,
}

impl InputProcessor {
    /// Create a new processor starting in Normal mode with empty keymaps.
    pub fn new() -> Self {
        Self {
            modes: ModeStack::new(ModeId::normal()),
            keymaps: HashMap::new(),
            sequence: SequenceState::new(),
            timeout: Duration::from_millis(1000),
        }
    }

    /// Register a mode definition on the mode stack.
    pub fn add_mode(&mut self, def: ModeDefinition) {
        self.modes.add_mode(def);
    }

    /// Set (or replace) the keymap trie for a given mode.
    pub fn set_keymap(&mut self, mode: ModeId, trie: KeyTrie) {
        self.keymaps.insert(mode, trie);
    }

    /// Set the timeout duration for pending sequences.
    pub fn set_timeout(&mut self, timeout: Duration) {
        self.timeout = timeout;
    }

    /// Process an input event, returning zero or more commands.
    ///
    /// This is the main entry point. It handles:
    /// - Key events: trie lookup via the current mode's keymap
    /// - Insert mode passthrough: unmatched character keys → `InsertText`
    /// - Escape: cancel pending sequence, pop sub-mode, or switch to Normal
    /// - Non-key events: forwarded as `Unhandled`
    pub fn process(&mut self, event: InputEvent, _ctx: &ActionContext) -> Vec<InputCommand> {
        match event {
            InputEvent::Key(ref key_event) => {
                let chord = KeyChord::new(key_event.key.clone(), key_event.modifiers);
                self.process_key(chord, event)
            }
            // Non-key events are forwarded as unhandled
            _ => vec![InputCommand::Unhandled(event)],
        }
    }

    /// Handle the timeout expiring on a pending sequence.
    ///
    /// If keys are pending, clears the sequence and returns `Unhandled`
    /// for the first key (or the original event that started the sequence).
    pub fn timeout_expired(&mut self) -> Vec<InputCommand> {
        if !self.sequence.is_pending() {
            return Vec::new();
        }

        // Timeout: treat the pending keys as unmatched
        let keys = std::mem::take(&mut self.sequence.keys);
        if let Some(first) = keys.into_iter().next() {
            vec![InputCommand::Unhandled(InputEvent::Key(
                crate::event::KeyEvent {
                    key: first.key,
                    modifiers: first.modifiers,
                },
            ))]
        } else {
            Vec::new()
        }
    }

    /// The current active mode.
    pub fn current_mode(&self) -> &ModeId {
        self.modes.current()
    }

    /// A display string for any pending key sequence (e.g., "g" while waiting).
    pub fn pending_display(&self) -> Option<String> {
        if self.sequence.is_pending() {
            Some(self.sequence.display())
        } else {
            None
        }
    }

    /// Whether the processor has a pending sequence that needs a timeout.
    pub fn needs_timeout(&self) -> bool {
        self.sequence.is_pending()
    }

    /// The configured timeout duration.
    pub fn timeout_duration(&self) -> Duration {
        self.timeout
    }

    /// Access the mode stack (for inspection/testing).
    pub fn mode_stack(&self) -> &ModeStack {
        &self.modes
    }

    // region: --- Private

    /// Core key processing logic.
    fn process_key(&mut self, chord: KeyChord, original_event: InputEvent) -> Vec<InputCommand> {
        // Handle Escape specially: cancel pending, pop sub-mode, or switch to Normal
        if chord.key == KeyCode::Escape && chord.modifiers == crate::key::Modifiers::NONE {
            return self.handle_escape();
        }

        // Accumulate the chord into the sequence
        self.sequence.push(chord);

        // Look up the accumulated sequence in the current mode's trie
        let current_mode = self.modes.current().clone();
        let lookup = self
            .keymaps
            .get(&current_mode)
            .map(|trie| trie.lookup(self.sequence.keys()))
            .unwrap_or(TrieLookup::Miss);

        match lookup {
            TrieLookup::Match(command) => {
                self.sequence.clear();
                self.execute_command(command)
            }
            TrieLookup::Prefix => {
                // Sequence is a valid prefix — signal pending state
                let display = self.sequence.display();
                vec![InputCommand::Pending { display }]
            }
            TrieLookup::Miss => {
                self.sequence.clear();
                self.handle_unmatched(original_event, &current_mode)
            }
        }
    }

    /// Handle the Escape key.
    fn handle_escape(&mut self) -> Vec<InputCommand> {
        // If a sequence is pending, cancel it
        if self.sequence.is_pending() {
            self.sequence.clear();
            return Vec::new();
        }

        // If in a sub-mode, pop it
        if self.modes.depth() > 1 {
            return self.modes.pop();
        }

        // If not in Normal mode, switch to Normal
        if self.modes.current() != &ModeId::normal() {
            return self.modes.switch_base(ModeId::normal());
        }

        // Already in Normal with no pending — no-op
        Vec::new()
    }

    /// Handle an unmatched key in the current mode.
    fn handle_unmatched(
        &self,
        original_event: InputEvent,
        mode: &ModeId,
    ) -> Vec<InputCommand> {
        // In a passthrough-text mode (e.g., Insert), unmatched character keys
        // produce InsertText commands
        if let Some(def) = self.modes.definition(mode)
            && def.passthrough_text
            && let InputEvent::Key(ref key_event) = original_event
            && let KeyCode::Character(ref ch) = key_event.key
            && key_event.modifiers == crate::key::Modifiers::NONE
        {
            return vec![InputCommand::InsertText(ch.clone())];
        }

        vec![InputCommand::Unhandled(original_event)]
    }

    /// Execute a matched command, handling mode transitions.
    fn execute_command(&mut self, command: InputCommand) -> Vec<InputCommand> {
        match command {
            InputCommand::SwitchMode(ref mode_id) => {
                let mut cmds = self.modes.switch_base(mode_id.clone());
                cmds.insert(0, command);
                cmds
            }
            InputCommand::PushMode(ref mode_id) => {
                let mut cmds = self.modes.push(mode_id.clone());
                cmds.insert(0, command);
                cmds
            }
            InputCommand::PopMode => {
                let mut cmds = self.modes.pop();
                cmds.insert(0, command);
                cmds
            }
            _ => vec![command],
        }
    }

    // endregion: --- Private
}

impl Default for InputProcessor {
    fn default() -> Self {
        Self::new()
    }
}

// endregion: --- InputProcessor

// region: --- Tests

#[cfg(test)]
mod tests {
    use super::*;
    use crate::command::ActionId;
    use crate::event::KeyEvent;
    use crate::key::{KeyCode, Modifiers};

    // -- Setup & Fixtures

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    fn key_event(key: KeyCode) -> InputEvent {
        InputEvent::Key(KeyEvent {
            key,
            modifiers: Modifiers::NONE,
        })
    }

    fn char_event(ch: char) -> InputEvent {
        key_event(KeyCode::Character(ch.to_string()))
    }

    fn chord(ch: char) -> KeyChord {
        KeyChord::plain(KeyCode::Character(ch.to_string()))
    }

    fn make_processor() -> InputProcessor {
        let mut proc = InputProcessor::new();

        // Register modes
        proc.add_mode(
            ModeDefinition::new(ModeId::normal(), "NORMAL")
                .with_on_enter(vec![ActionId::new("mode.normal.enter")])
                .with_on_exit(vec![ActionId::new("mode.normal.exit")]),
        );
        proc.add_mode(
            ModeDefinition::new(ModeId::insert(), "INSERT")
                .with_passthrough_text(true)
                .with_on_enter(vec![ActionId::new("mode.insert.enter")])
                .with_on_exit(vec![ActionId::new("mode.insert.exit")]),
        );
        proc.add_mode(
            ModeDefinition::new(ModeId::visual(), "VISUAL")
                .with_on_enter(vec![ActionId::new("mode.visual.enter")])
                .with_on_exit(vec![ActionId::new("mode.visual.exit")]),
        );

        // Normal mode keymap
        let mut normal_trie = KeyTrie::new();
        normal_trie.bind(chord('j'), ActionId::new("cursor.down"));
        normal_trie.bind(chord('k'), ActionId::new("cursor.up"));
        normal_trie.bind_mode_switch(chord('i'), ModeId::insert());
        normal_trie.bind_mode_switch(chord('v'), ModeId::visual());
        normal_trie.bind_sequence(vec![chord('g'), chord('g')], ActionId::new("cursor.top"));
        normal_trie.bind_sequence(vec![chord('g'), chord('e')], ActionId::new("cursor.end"));
        proc.set_keymap(ModeId::normal(), normal_trie);

        // Insert mode keymap (mostly empty — passthrough handles text)
        let insert_trie = KeyTrie::new();
        proc.set_keymap(ModeId::insert(), insert_trie);

        proc
    }

    fn extract_action_ids(commands: &[InputCommand]) -> Vec<&str> {
        commands
            .iter()
            .filter_map(|cmd| match cmd {
                InputCommand::Action(id) => Some(id.as_str()),
                _ => None,
            })
            .collect()
    }

    // -- Tests

    #[test]
    fn test_single_key_action_normal_mode() -> Result<()> {
        // -- Setup
        let mut proc = make_processor();
        let ctx = ActionContext::new();

        // -- Exec
        let commands = proc.process(char_event('j'), &ctx);

        // -- Check
        assert_eq!(commands.len(), 1);
        let ids = extract_action_ids(&commands);
        assert_eq!(ids, vec!["cursor.down"]);
        assert!(!proc.needs_timeout());

        Ok(())
    }

    #[test]
    fn test_two_key_sequence_with_pending() -> Result<()> {
        // -- Setup
        let mut proc = make_processor();
        let ctx = ActionContext::new();

        // -- Exec: first key of sequence
        let commands = proc.process(char_event('g'), &ctx);

        // -- Check: should be Pending
        assert_eq!(commands.len(), 1);
        assert!(matches!(&commands[0], InputCommand::Pending { display } if display == "g"));
        assert!(proc.needs_timeout());
        assert_eq!(proc.pending_display(), Some("g".to_string()));

        // -- Exec: second key completes the sequence
        let commands = proc.process(char_event('g'), &ctx);

        // -- Check: should be the action
        let ids = extract_action_ids(&commands);
        assert_eq!(ids, vec!["cursor.top"]);
        assert!(!proc.needs_timeout());
        assert_eq!(proc.pending_display(), None);

        Ok(())
    }

    #[test]
    fn test_mode_switch_via_keybinding() -> Result<()> {
        // -- Setup
        let mut proc = make_processor();
        let ctx = ActionContext::new();
        assert_eq!(proc.current_mode(), &ModeId::normal());

        // -- Exec: press 'i' to enter insert mode
        let commands = proc.process(char_event('i'), &ctx);

        // -- Check: SwitchMode + on_exit normal + on_enter insert
        assert_eq!(proc.current_mode(), &ModeId::insert());
        // Commands: SwitchMode(insert), Action(mode.normal.exit), Action(mode.insert.enter)
        assert!(matches!(&commands[0], InputCommand::SwitchMode(m) if *m == ModeId::insert()));
        let action_ids = extract_action_ids(&commands);
        assert_eq!(action_ids, vec!["mode.normal.exit", "mode.insert.enter"]);

        Ok(())
    }

    #[test]
    fn test_insert_mode_passthrough() -> Result<()> {
        // -- Setup
        let mut proc = make_processor();
        let ctx = ActionContext::new();

        // Switch to insert mode first
        proc.process(char_event('i'), &ctx);
        assert_eq!(proc.current_mode(), &ModeId::insert());

        // -- Exec: type characters in insert mode
        let commands = proc.process(char_event('h'), &ctx);

        // -- Check: should produce InsertText
        assert_eq!(commands.len(), 1);
        assert!(matches!(&commands[0], InputCommand::InsertText(s) if s == "h"));

        let commands = proc.process(char_event('e'), &ctx);
        assert!(matches!(&commands[0], InputCommand::InsertText(s) if s == "e"));

        Ok(())
    }

    #[test]
    fn test_escape_cancels_pending_and_returns_to_normal() -> Result<()> {
        // -- Setup
        let mut proc = make_processor();
        let ctx = ActionContext::new();

        // Start a sequence
        let commands = proc.process(char_event('g'), &ctx);
        assert!(matches!(&commands[0], InputCommand::Pending { .. }));
        assert!(proc.needs_timeout());

        // -- Exec: press Escape to cancel
        let commands = proc.process(key_event(KeyCode::Escape), &ctx);

        // -- Check: pending cancelled, no commands emitted, still in normal
        assert!(commands.is_empty());
        assert!(!proc.needs_timeout());
        assert_eq!(proc.current_mode(), &ModeId::normal());

        Ok(())
    }

    #[test]
    fn test_escape_exits_insert_to_normal() -> Result<()> {
        // -- Setup
        let mut proc = make_processor();
        let ctx = ActionContext::new();

        // Enter insert mode
        proc.process(char_event('i'), &ctx);
        assert_eq!(proc.current_mode(), &ModeId::insert());

        // -- Exec: press Escape
        let commands = proc.process(key_event(KeyCode::Escape), &ctx);

        // -- Check: should switch back to normal
        assert_eq!(proc.current_mode(), &ModeId::normal());
        let action_ids = extract_action_ids(&commands);
        assert_eq!(action_ids, vec!["mode.insert.exit", "mode.normal.enter"]);

        Ok(())
    }

    #[test]
    fn test_new_starts_in_normal_mode() -> Result<()> {
        // -- Exec
        let proc = InputProcessor::new();

        // -- Check
        assert_eq!(proc.current_mode(), &ModeId::normal());
        assert!(!proc.needs_timeout());
        assert_eq!(proc.pending_display(), None);

        Ok(())
    }

    #[test]
    fn test_unmatched_key_in_normal_mode() -> Result<()> {
        // -- Setup
        let mut proc = make_processor();
        let ctx = ActionContext::new();

        // -- Exec: press a key with no binding
        let commands = proc.process(char_event('z'), &ctx);

        // -- Check: should be Unhandled
        assert_eq!(commands.len(), 1);
        assert!(matches!(&commands[0], InputCommand::Unhandled(_)));

        Ok(())
    }

    #[test]
    fn test_timeout_expired_clears_pending() -> Result<()> {
        // -- Setup
        let mut proc = make_processor();
        let ctx = ActionContext::new();

        // Start a pending sequence
        proc.process(char_event('g'), &ctx);
        assert!(proc.needs_timeout());

        // -- Exec: timeout expires
        let commands = proc.timeout_expired();

        // -- Check: pending cleared, first key forwarded as unhandled
        assert!(!proc.needs_timeout());
        assert_eq!(commands.len(), 1);
        assert!(matches!(&commands[0], InputCommand::Unhandled(_)));

        Ok(())
    }

    #[test]
    fn test_sequence_miss_second_key() -> Result<()> {
        // -- Setup
        let mut proc = make_processor();
        let ctx = ActionContext::new();

        // Start 'g' sequence
        let commands = proc.process(char_event('g'), &ctx);
        assert!(matches!(&commands[0], InputCommand::Pending { .. }));

        // -- Exec: press 'z' which doesn't complete any 'g' sequence
        let commands = proc.process(char_event('z'), &ctx);

        // -- Check: sequence cleared, unhandled (since gz is not bound)
        assert!(!proc.needs_timeout());
        assert_eq!(commands.len(), 1);
        assert!(matches!(&commands[0], InputCommand::Unhandled(_)));

        Ok(())
    }

    #[test]
    fn test_non_key_events_forwarded_as_unhandled() -> Result<()> {
        // -- Setup
        let mut proc = make_processor();
        let ctx = ActionContext::new();

        // -- Exec
        let commands = proc.process(InputEvent::FocusGained, &ctx);

        // -- Check
        assert_eq!(commands.len(), 1);
        assert!(matches!(&commands[0], InputCommand::Unhandled(InputEvent::FocusGained)));

        Ok(())
    }
}

// endregion: --- Tests
