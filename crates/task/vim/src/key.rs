//! Adapter that lets us drive `keybindings::ModalMachine` with
//! Dioxus keyboard events.
//!
//! v1 keeps the alphabet small: printable ASCII chars + the few
//! named keys our current bindings need (Escape, Enter, Tab,
//! Backspace, arrow keys). Modifier-aware bindings (Ctrl-, Alt-,
//! Cmd-) follow once we wire in our first Ctrl-mapped command.

use std::convert::Infallible;

use keybindings::InputKey;

/// A single keystroke from Dioxus's keyboard event, normalized for
/// the modal machine.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DioxusKey {
    /// A printable character — the canonical channel for vim
    /// command keys (`h`, `j`, `i`, `o`, `:`, etc.).
    Char(char),
    Escape,
    Enter,
    Tab,
    Backspace,
    ArrowLeft,
    ArrowRight,
    ArrowUp,
    ArrowDown,
}

impl DioxusKey {
    /// Construct from Dioxus's `Key` enum + a flag for shift.
    /// Returns `None` for keys we don't yet route through the
    /// modal machine (function keys, dead keys, …) — those fall
    /// through to the editor as before.
    #[must_use]
    pub fn from_dioxus(key: &dioxus::prelude::Key, shift: bool) -> Option<Self> {
        use dioxus::prelude::Key;
        match key {
            Key::Character(s) => {
                let c = s.chars().next()?;
                if s.chars().count() != 1 {
                    return None;
                }
                // If shift is held with a letter, the browser
                // already gives us the upper-cased character via
                // `Key::Character`. We honor that as-is.
                let _ = shift;
                Some(Self::Char(c))
            }
            Key::Escape => Some(Self::Escape),
            Key::Enter => Some(Self::Enter),
            Key::Tab => Some(Self::Tab),
            Key::Backspace => Some(Self::Backspace),
            Key::ArrowLeft => Some(Self::ArrowLeft),
            Key::ArrowRight => Some(Self::ArrowRight),
            Key::ArrowUp => Some(Self::ArrowUp),
            Key::ArrowDown => Some(Self::ArrowDown),
            _ => None,
        }
    }
}

impl InputKey for DioxusKey {
    type Error = Infallible;

    fn decompose(&mut self) -> Option<Self> {
        // Dioxus key events are already atomic; nothing to split.
        None
    }

    fn from_macro_str(s: &str) -> Result<Vec<Self>, Self::Error> {
        // Minimal v1 parser — only used for tests / future macro
        // playback. Recognizes:
        //   - Plain characters
        //   - `<Esc>`, `<CR>`, `<Tab>`, `<BS>`,
        //     `<Left>`, `<Right>`, `<Up>`, `<Down>`
        let mut out = Vec::new();
        let mut chars = s.chars().peekable();
        while let Some(c) = chars.next() {
            if c != '<' {
                out.push(Self::Char(c));
                continue;
            }
            let mut name = String::new();
            for ch in chars.by_ref() {
                if ch == '>' {
                    break;
                }
                name.push(ch);
            }
            let key = match name.to_ascii_lowercase().as_str() {
                "esc" => Self::Escape,
                "cr" | "enter" => Self::Enter,
                "tab" => Self::Tab,
                "bs" | "backspace" => Self::Backspace,
                "left" => Self::ArrowLeft,
                "right" => Self::ArrowRight,
                "up" => Self::ArrowUp,
                "down" => Self::ArrowDown,
                _ => continue,
            };
            out.push(key);
        }
        Ok(out)
    }

    fn get_char(&self) -> Option<char> {
        match self {
            Self::Char(c) => Some(*c),
            _ => None,
        }
    }
}
