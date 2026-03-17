//! Bridge between input-reaper keybind definitions and the `input` crate.
//!
//! Converts:
//! - `KeybindPreset` / `KeybindOverride` → `KeymapConfig`
//! - `WhichKeyEntry` trees → keymap entries in the trie
//! - REAPER VK codes → `InputEvent`
//! - input-reaper key notation (`<C-s>`, `gg`) → input crate notation (`Ctrl+s`, `g g`)

use std::collections::HashMap;

use enumflags2::BitFlags;
use input::config::KeymapConfig;
use input::event::{InputEvent, KeyEvent};
use input::key::{KeyChord, KeyCode, Modifiers};
use reaper_medium::{AcceleratorBehavior, AcceleratorKeyCode};

use super::which_key::WhichKeyEntry;
use super::{KeybindContext, KeybindOverride, KeybindPreset};

// ---------------------------------------------------------------------------
// VK Code → InputEvent
// ---------------------------------------------------------------------------

/// Convert REAPER's VK code + AcceleratorBehavior into an `InputEvent`.
///
/// Returns `None` for pure modifier-only keypresses (Shift, Ctrl, Alt, Cmd).
pub fn vk_to_input_event(
    key: AcceleratorKeyCode,
    behavior: &BitFlags<AcceleratorBehavior>,
) -> Option<InputEvent> {
    let key_code = key.get() as u32;

    // Skip pure modifier keys — they don't produce input events.
    match key_code {
        16 | 160 | 161 => return None, // VK_SHIFT, VK_LSHIFT, VK_RSHIFT
        17 | 162 | 163 => return None, // VK_CONTROL, VK_LCONTROL, VK_RCONTROL
        18 | 164 | 165 => return None, // VK_MENU (Alt), VK_LMENU, VK_RMENU
        91 | 92 => return None,        // VK_LWIN, VK_RWIN
        _ => {}
    }

    let ctrl = behavior.contains(AcceleratorBehavior::Control);
    let alt = behavior.contains(AcceleratorBehavior::Alt);
    let mut shift = behavior.contains(AcceleratorBehavior::Shift);

    // On macOS: Command (⌘) is reported as ctrl, map it to meta.
    #[cfg(target_os = "macos")]
    let modifiers = {
        let meta = ctrl; // Command = reported as ctrl on macOS
        let ctrl_key = false; // No real ctrl on macOS (it's mapped to meta)
        Modifiers {
            ctrl: ctrl_key,
            alt,
            shift,
            meta,
        }
    };
    #[cfg(not(target_os = "macos"))]
    let modifiers = Modifiers {
        ctrl,
        alt,
        shift,
        meta: false,
    };

    // Convert VK code to KeyCode.
    //
    // On macOS, SWELL can emit ASCII codes for punctuation in modified chords.
    // Normalize shifted ASCII symbols to their base key and infer shift.
    #[cfg(target_os = "macos")]
    let key_code_result = {
        if let Some((base, inferred_shift)) = mac_ascii_normalize(key_code) {
            if inferred_shift {
                shift = true;
            }
            Some(KeyCode::Character(base.to_string()))
        } else {
            vk_to_key_code(key_code)
        }
    };
    #[cfg(not(target_os = "macos"))]
    let key_code_result = vk_to_key_code(key_code);

    let key = key_code_result?;

    // Rebuild modifiers with possibly updated shift.
    #[cfg(target_os = "macos")]
    let modifiers = Modifiers { shift, ..modifiers };

    Some(InputEvent::Key(KeyEvent { key, modifiers }))
}

/// Map a Windows VK code to an `input::KeyCode`.
fn vk_to_key_code(key_code: u32) -> Option<KeyCode> {
    match key_code {
        // Letters (A-Z) → lowercase character
        65..=90 => {
            let c = char::from_u32(key_code + 32)?;
            Some(KeyCode::Character(c.to_string()))
        }
        // Numbers (0-9)
        48..=57 => {
            let c = char::from_u32(key_code)?;
            Some(KeyCode::Character(c.to_string()))
        }
        // Special keys
        8 => Some(KeyCode::Backspace),
        9 => Some(KeyCode::Tab),
        13 => Some(KeyCode::Enter),
        27 => Some(KeyCode::Escape),
        32 => Some(KeyCode::Character(" ".to_string())), // Space
        // Arrow keys
        0x25 => Some(KeyCode::ArrowLeft),
        0x26 => Some(KeyCode::ArrowUp),
        0x27 => Some(KeyCode::ArrowRight),
        0x28 => Some(KeyCode::ArrowDown),
        // Function keys (F1-F12)
        0x70..=0x7B => Some(KeyCode::F((key_code - 0x70 + 1) as u8)),
        // Navigation
        0x21 => Some(KeyCode::Character("pageup".to_string())),
        0x22 => Some(KeyCode::Character("pagedown".to_string())),
        0x23 => Some(KeyCode::Character("end".to_string())),
        0x24 => Some(KeyCode::Character("home".to_string())),
        0x2D => Some(KeyCode::Character("insert".to_string())),
        0x2E => Some(KeyCode::Delete),
        // OEM keys (US layout)
        0xBA => Some(KeyCode::Character(";".to_string())),
        0xBB => Some(KeyCode::Character("=".to_string())),
        0xBC => Some(KeyCode::Character(",".to_string())),
        0xBD => Some(KeyCode::Character("-".to_string())),
        0xBE => Some(KeyCode::Character(".".to_string())),
        0xBF => Some(KeyCode::Character("/".to_string())),
        0xC0 => Some(KeyCode::Character("`".to_string())),
        0xDB => Some(KeyCode::Character("[".to_string())),
        0xDC => Some(KeyCode::Character("\\".to_string())),
        0xDD => Some(KeyCode::Character("]".to_string())),
        0xDE => Some(KeyCode::Character("'".to_string())),
        _ => None, // Unknown VK code
    }
}

/// macOS SWELL ASCII normalization.
///
/// When modifiers are held on macOS, SWELL sometimes sends the ASCII code
/// of the shifted symbol instead of the base VK code. This maps shifted
/// ASCII back to the base key and flags whether shift was inferred.
#[cfg(target_os = "macos")]
fn mac_ascii_normalize(key_code: u32) -> Option<(&'static str, bool)> {
    match key_code {
        44 => Some((",", false)),
        46 => Some((".", false)),
        47 => Some(("/", false)),
        59 => Some((";", false)),
        39 => Some(("'", false)),
        45 => Some(("-", false)),
        61 => Some(("=", false)),
        91 => Some(("[", false)),
        93 => Some(("]", false)),
        92 => Some(("\\", false)),
        96 => Some(("`", false)),
        // Shifted symbols → base key + shift=true
        60 => Some((",", true)),   // '<'
        62 => Some((".", true)),   // '>'
        63 => Some(("/", true)),   // '?'
        58 => Some((";", true)),   // ':'
        34 => Some(("'", true)),   // '"'
        95 => Some(("-", true)),   // '_'
        43 => Some(("=", true)),   // '+'
        123 => Some(("[", true)),  // '{'
        125 => Some(("]", true)),  // '}'
        124 => Some(("\\", true)), // '|'
        126 => Some(("`", true)),  // '~'
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Preset → KeymapConfig
// ---------------------------------------------------------------------------

/// Convert a `KeybindPreset` and its which-key trees into a `KeymapConfig`.
///
/// All keybinds go into the "normal" mode keymap (REAPER doesn't use modal
/// editing). Context-specific bindings (`KeybindContext::Main`, `Midi`, etc.)
/// are placed in `keymap_context` with `WhenExpr` conditions.
pub fn preset_to_keymap_config(
    preset: &KeybindPreset,
    trees: &[(String, String, Vec<WhichKeyEntry>)],
) -> KeymapConfig {
    let mut global_bindings: HashMap<String, String> = HashMap::new();
    let mut context_bindings: HashMap<KeybindContext, HashMap<String, String>> = HashMap::new();

    // Convert keybindings
    for binding in &preset.bindings {
        let input_seq = translate_sequence(&binding.keys);
        let ctx = binding.effective_context();

        if ctx == KeybindContext::Global {
            global_bindings.insert(input_seq, binding.action.clone());
        } else {
            context_bindings
                .entry(ctx)
                .or_default()
                .insert(input_seq, binding.action.clone());
        }
    }

    // Convert which-key trees into flat key sequences
    let tree_entries = which_key_trees_to_keymap_entries(trees);
    for (seq, action) in tree_entries {
        global_bindings.insert(seq, action);
    }

    // Build keymap
    let mut keymap: HashMap<String, HashMap<String, String>> = HashMap::new();
    keymap.insert("normal".to_string(), global_bindings);

    // Build keymap_context
    let mut keymap_context: HashMap<String, Vec<input::config::ContextLayerConfig>> =
        HashMap::new();
    for (ctx, bindings) in context_bindings {
        let when = context_to_when_expr(ctx);
        let layer = input::config::ContextLayerConfig { when, bindings };
        keymap_context
            .entry("normal".to_string())
            .or_default()
            .push(layer);
    }

    // Build scroll bindings from wheel bindings
    let scroll = convert_wheel_bindings(preset);

    KeymapConfig {
        modes: HashMap::new(), // Use defaults (normal mode only)
        keymap,
        keymap_context,
        mouse: HashMap::new(), // Mouse modifiers handled separately by REAPER's native system
        scroll,
    }
}

/// Convert an override layer into a `KeymapConfig` for merging.
pub fn override_to_keymap_config(override_layer: &KeybindOverride) -> KeymapConfig {
    // Build a temporary preset to reuse the conversion logic
    let pseudo_preset = KeybindPreset {
        name: override_layer.name.clone(),
        description: override_layer.description.clone(),
        version: "1.0.0".to_string(),
        bindings: override_layer.bindings.clone(),
        wheel_bindings: override_layer.wheel_bindings.clone(),
        mouse_modifiers: Vec::new(),
    };
    preset_to_keymap_config(&pseudo_preset, &[])
}

// ---------------------------------------------------------------------------
// Which-Key Trees → Keymap Entries
// ---------------------------------------------------------------------------

/// Convert `WhichKeyEntry` trees into flat keymap entries.
///
/// Each tree prefix + leaf path becomes a space-separated key sequence.
/// For example: tree prefix="v", leaf key="d" → sequence "v d", action="..."
fn which_key_trees_to_keymap_entries(
    trees: &[(String, String, Vec<WhichKeyEntry>)],
) -> HashMap<String, String> {
    let mut entries = HashMap::new();

    for (prefix, _label, children) in trees {
        flatten_which_key_entries(&mut entries, prefix, children);
    }

    entries
}

/// Recursively flatten which-key entries into space-separated key sequences.
fn flatten_which_key_entries(
    out: &mut HashMap<String, String>,
    prefix: &str,
    entries: &[WhichKeyEntry],
) {
    for entry in entries {
        match entry {
            WhichKeyEntry::Leaf { key, action, .. } => {
                let seq = format!("{} {}", prefix, key);
                out.insert(seq, action.clone());
            }
            WhichKeyEntry::Branch { key, children, .. } => {
                let new_prefix = format!("{} {}", prefix, key);
                flatten_which_key_entries(out, &new_prefix, children);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Key Notation Translation
// ---------------------------------------------------------------------------

/// Convert an input-reaper key sequence string to input crate notation.
///
/// Handles multi-character sequences like `gg`, bracketed keys like `<C-s>`,
/// and mixed sequences like `g<C-a>`.
///
/// # Examples
/// - `"a"` → `"a"`
/// - `"gg"` → `"g g"`
/// - `"<C-s>"` → `"Ctrl+s"`
/// - `"<S-Tab>"` → `"Shift+Tab"`
/// - `"<M-w>"` → `"Meta+w"`
/// - `"<C-S-a>"` → `"Ctrl+Shift+a"`
/// - `"g<C-a>"` → `"g Ctrl+a"`
pub fn translate_sequence(reaper_seq: &str) -> String {
    let mut chords = Vec::new();
    let mut chars = reaper_seq.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '<' {
            // Parse bracketed key like <C-S-a> or <space>
            let mut bracket_content = String::new();
            for ch in chars.by_ref() {
                if ch == '>' {
                    break;
                }
                bracket_content.push(ch);
            }
            chords.push(translate_bracketed(&bracket_content));
        } else {
            // Simple single character key
            chords.push(c.to_lowercase().to_string());
        }
    }

    chords.join(" ")
}

/// Translate a bracketed key expression (without angle brackets) to input crate notation.
///
/// `"C-S-a"` → `"Ctrl+Shift+a"`
/// `"space"` → `"Space"`
/// `"M-w"` → `"Meta+w"`
fn translate_bracketed(content: &str) -> String {
    // Check if it's a special key without modifiers
    let lower = content.to_lowercase();
    if let Some(special) = translate_special_key(&lower) {
        return special;
    }

    // Parse modifiers and key
    let parts: Vec<&str> = content.split('-').collect();
    let mut modifiers = Vec::new();
    let mut key_part = None;

    for (i, part) in parts.iter().enumerate() {
        let upper = part.to_uppercase();
        match upper.as_str() {
            "C" | "CTRL" | "CONTROL" => modifiers.push("Ctrl"),
            "S" | "SHIFT" => modifiers.push("Shift"),
            "A" | "ALT" | "OPT" | "OPTION" => modifiers.push("Alt"),
            "M" | "META" | "CMD" | "COMMAND" | "WIN" | "SUPER" => modifiers.push("Meta"),
            _ => {
                if i == parts.len() - 1 {
                    // Last part is the key
                    let lower_part = part.to_lowercase();
                    key_part = Some(translate_special_key(&lower_part).unwrap_or(lower_part));
                }
            }
        }
    }

    let key = key_part.unwrap_or_default();

    if modifiers.is_empty() {
        key
    } else {
        modifiers.push(&key);
        modifiers.join("+")
    }
}

/// Translate input-reaper special key names to input crate names.
fn translate_special_key(name: &str) -> Option<String> {
    match name {
        "space" | "spc" => Some("Space".to_string()),
        "tab" => Some("Tab".to_string()),
        "enter" | "return" | "ret" | "cr" => Some("Enter".to_string()),
        "esc" | "escape" => Some("Escape".to_string()),
        "backspace" | "bs" => Some("Backspace".to_string()),
        "delete" | "del" => Some("Delete".to_string()),
        "up" => Some("Up".to_string()),
        "down" => Some("Down".to_string()),
        "left" => Some("Left".to_string()),
        "right" => Some("Right".to_string()),
        "home" => Some("home".to_string()),
        "end" => Some("end".to_string()),
        "pageup" | "pgup" => Some("pageup".to_string()),
        "pagedown" | "pgdn" => Some("pagedown".to_string()),
        "insert" | "ins" => Some("insert".to_string()),
        "f1" | "f2" | "f3" | "f4" | "f5" | "f6" | "f7" | "f8" | "f9" | "f10" | "f11" | "f12" => {
            Some(name.to_string())
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Context → WhenExpr string
// ---------------------------------------------------------------------------

/// Convert a `KeybindContext` to a when-expression string for `keymap_context`.
fn context_to_when_expr(ctx: KeybindContext) -> String {
    match ctx {
        KeybindContext::Global => "true".to_string(),
        KeybindContext::Main => "context:main".to_string(),
        KeybindContext::Midi => "context:midi".to_string(),
        KeybindContext::MidiInline => "context:midi_inline".to_string(),
        KeybindContext::MediaExplorer => "context:media_explorer".to_string(),
    }
}

// ---------------------------------------------------------------------------
// Wheel bindings → scroll config
// ---------------------------------------------------------------------------

/// Convert wheel bindings from a preset into scroll config entries.
fn convert_wheel_bindings(preset: &KeybindPreset) -> HashMap<String, HashMap<String, String>> {
    if preset.wheel_bindings.is_empty() {
        return HashMap::new();
    }

    let mut scroll_map: HashMap<String, String> = HashMap::new();

    for wheel in &preset.wheel_bindings {
        // Build the scroll pattern string
        let axis = if wheel.horizontal {
            "ScrollX"
        } else {
            "Scroll"
        };

        // Parse modifiers from the wheel binding's modifier string
        let mods = parse_reaper_modifier_string(&wheel.modifiers);
        let pattern = if mods.is_empty() {
            axis.to_string()
        } else {
            format!("{}+{}", mods, axis)
        };

        scroll_map.insert(pattern, wheel.action.clone());
    }

    let mut result = HashMap::new();
    if !scroll_map.is_empty() {
        result.insert("normal".to_string(), scroll_map);
    }
    result
}

/// Parse input-reaper modifier string (e.g., `"<C->"`, `"<C-S->"`, `""`) into
/// input crate modifier prefix (e.g., `"Ctrl"`, `"Ctrl+Shift"`, `""`).
fn parse_reaper_modifier_string(mods: &str) -> String {
    let trimmed = mods.trim();
    if trimmed.is_empty() || trimmed == "<>" {
        return String::new();
    }

    // Strip angle brackets
    let inner = trimmed
        .strip_prefix('<')
        .and_then(|s| s.strip_suffix('>'))
        .unwrap_or(trimmed);

    // Split by '-' and collect modifier names
    let mut parts = Vec::new();
    for part in inner.split('-') {
        match part.to_uppercase().as_str() {
            "C" | "CTRL" => parts.push("Ctrl"),
            "S" | "SHIFT" => parts.push("Shift"),
            "A" | "ALT" => parts.push("Alt"),
            "M" | "META" | "CMD" => parts.push("Meta"),
            "" => {} // trailing hyphen
            _ => {}  // unknown, skip
        }
    }

    parts.join("+")
}

// ---------------------------------------------------------------------------
// Introspection helpers (for which-key overlay)
// ---------------------------------------------------------------------------

/// Get the continuations (child entries) at a given key sequence prefix
/// from the InputProcessor's keytrie.
///
/// Returns `(key_display, label, is_branch)` tuples.
pub fn trie_continuations_at(
    trie: &input::trie::KeyTrie,
    prefix: &[KeyChord],
) -> Vec<(String, String, bool)> {
    use input::trie::KeyTrie;

    // Walk to the node at the prefix path
    let node = walk_trie(trie, prefix);

    match node {
        Some(KeyTrie::Node(node)) => {
            let mut result: Vec<(String, String, bool)> = node
                .children
                .iter()
                .map(|(chord, child)| {
                    let key_display = chord_to_display(chord);
                    match child {
                        KeyTrie::Node(n) => (key_display, n.name.clone(), true),
                        KeyTrie::Leaf(_) => (key_display, String::new(), false),
                    }
                })
                .collect();
            result.sort_by(|a, b| a.0.cmp(&b.0));
            result
        }
        _ => Vec::new(),
    }
}

/// Walk a KeyTrie along a sequence of chords.
fn walk_trie<'a>(
    trie: &'a input::trie::KeyTrie,
    path: &[KeyChord],
) -> Option<&'a input::trie::KeyTrie> {
    use input::trie::KeyTrie;

    if path.is_empty() {
        return Some(trie);
    }

    let mut current = trie;
    for chord in path {
        match current {
            KeyTrie::Node(node) => {
                current = node.children.get(chord)?;
            }
            KeyTrie::Leaf(_) => return None,
        }
    }
    Some(current)
}

/// Format a KeyChord for display in the which-key overlay.
fn chord_to_display(chord: &KeyChord) -> String {
    let mut s = String::new();
    if chord.modifiers.ctrl {
        s.push_str("C-");
    }
    if chord.modifiers.meta {
        s.push_str("M-");
    }
    if chord.modifiers.shift {
        s.push_str("S-");
    }
    if chord.modifiers.alt {
        s.push_str("A-");
    }
    match &chord.key {
        KeyCode::Character(c) if c == " " => s.push_str("SPC"),
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::input::keybinds::Keybind;

    #[test]
    fn test_translate_simple_key() {
        assert_eq!(translate_sequence("a"), "a");
    }

    #[test]
    fn test_translate_multi_key_sequence() {
        assert_eq!(translate_sequence("gg"), "g g");
    }

    #[test]
    fn test_translate_ctrl_key() {
        assert_eq!(translate_sequence("<C-s>"), "Ctrl+s");
    }

    #[test]
    fn test_translate_shift_tab() {
        assert_eq!(translate_sequence("<S-Tab>"), "Shift+Tab");
    }

    #[test]
    fn test_translate_meta_key() {
        assert_eq!(translate_sequence("<M-w>"), "Meta+w");
    }

    #[test]
    fn test_translate_multi_modifier() {
        assert_eq!(translate_sequence("<C-S-a>"), "Ctrl+Shift+a");
    }

    #[test]
    fn test_translate_mixed_sequence() {
        assert_eq!(translate_sequence("g<C-a>"), "g Ctrl+a");
    }

    #[test]
    fn test_translate_space() {
        assert_eq!(translate_sequence("<space>"), "Space");
    }

    #[test]
    fn test_translate_escape() {
        assert_eq!(translate_sequence("<esc>"), "Escape");
    }

    #[test]
    fn test_which_key_trees_to_entries() {
        let trees = vec![(
            "v".to_string(),
            "Visibility".to_string(),
            vec![
                WhichKeyEntry::leaf("d", "Drums", "vis:drums"),
                WhichKeyEntry::branch(
                    "e",
                    "EQ",
                    vec![WhichKeyEntry::leaf("r", "Rescue", "fx:rescue")],
                ),
            ],
        )];

        let entries = which_key_trees_to_keymap_entries(&trees);

        assert_eq!(entries.get("v d"), Some(&"vis:drums".to_string()));
        assert_eq!(entries.get("v e r"), Some(&"fx:rescue".to_string()));
        assert_eq!(entries.get("v"), None); // Prefix, not a leaf
    }

    #[test]
    fn test_parse_reaper_modifier_string() {
        assert_eq!(parse_reaper_modifier_string(""), "");
        assert_eq!(parse_reaper_modifier_string("<C->"), "Ctrl");
        assert_eq!(parse_reaper_modifier_string("<C-S->"), "Ctrl+Shift");
        assert_eq!(parse_reaper_modifier_string("<M->"), "Meta");
    }

    #[test]
    fn test_preset_to_keymap_config_basic() {
        let preset = KeybindPreset::new("test", "Test").with_bindings(vec![
            Keybind::new("a", "action_a"),
            Keybind::new("<C-s>", "save"),
            Keybind::new("gg", "goto_top"),
        ]);

        let config = preset_to_keymap_config(&preset, &[]);

        let normal = config.keymap.get("normal").unwrap();
        assert_eq!(normal.get("a"), Some(&"action_a".to_string()));
        assert_eq!(normal.get("Ctrl+s"), Some(&"save".to_string()));
        assert_eq!(normal.get("g g"), Some(&"goto_top".to_string()));
    }

    #[test]
    fn test_preset_with_context_bindings() {
        let preset = KeybindPreset::new("test", "Test").with_bindings(vec![
            Keybind::new("j", "nav_down").with_context(KeybindContext::Main),
            Keybind::new("k", "nav_up"),
        ]);

        let config = preset_to_keymap_config(&preset, &[]);

        // Global binding in keymap
        let normal = config.keymap.get("normal").unwrap();
        assert_eq!(normal.get("k"), Some(&"nav_up".to_string()));
        assert!(!normal.contains_key("j")); // Main-only, not in global

        // Context binding in keymap_context
        let layers = config.keymap_context.get("normal").unwrap();
        assert_eq!(layers.len(), 1);
        assert_eq!(layers[0].when, "context:main");
        assert_eq!(layers[0].bindings.get("j"), Some(&"nav_down".to_string()));
    }
}
