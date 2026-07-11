//! Interactive QWERTY keyboard map for the /input page.
//!
//! Renders an ANSI-style keyboard where every key that carries at least
//! one binding (in the active profile + category selection) lights up.
//! The keyboard's own Ctrl / Shift / Alt / Meta keys are toggles: with a
//! modifier chord active, the highlight switches to the keys bound under
//! that chord (Ctrl toggled → `S` lights up because `<C-s>` exists).
//! Hovering a lit key shows its bindings; clicking it filters the
//! binding list below to that key.
//!
//! The component's input is a plain `Vec<(category, KeybindDef)>` so the
//! data source stays swappable — today the embedded profile snapshot,
//! later a live `InputConfigService` feed.

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::prelude::*;
use input_config_proto::KeybindDef;

use super::input_tutorial::pretty_keys;

// ---------------------------------------------------------------------------
// Chord parsing
// ---------------------------------------------------------------------------

/// A modifier chord state (also the toggle state of the on-screen keyboard).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default, Debug)]
pub struct Mods {
    pub ctrl: bool,
    pub shift: bool,
    pub alt: bool,
    pub meta: bool,
}

impl Mods {
    fn is_empty(&self) -> bool {
        !(self.ctrl || self.shift || self.alt || self.meta)
    }

    fn label(&self) -> String {
        let mut parts = Vec::new();
        if self.ctrl {
            parts.push("Ctrl");
        }
        if self.shift {
            parts.push("Shift");
        }
        if self.alt {
            parts.push("Alt");
        }
        if self.meta {
            parts.push("Meta");
        }
        parts.join(" + ")
    }
}

/// An active "clicked a key" filter: modifier chord + physical key id.
#[derive(Clone, PartialEq, Debug)]
pub struct KeyFilter {
    pub mods: Mods,
    pub key: String,
}

impl KeyFilter {
    /// Human label for the filter chip, e.g. `Ctrl + S`.
    pub fn label(&self) -> String {
        let mods = self.mods.label();
        let key = display_key_name(&self.key);
        if mods.is_empty() { key } else { format!("{mods} + {key}") }
    }
}

/// Does a binding's FIRST chord land on this filter's (mods, key)?
pub fn binding_matches(keys: &str, filter: &KeyFilter) -> bool {
    first_chord(keys).is_some_and(|(m, k)| m == filter.mods && k == filter.key)
}

/// Parse the first chord of a key-sequence string (`"g g"`, `"<C-S-space>"`,
/// `"r"`, `"?"`) into (modifier set, physical key id). Shifted symbols and
/// uppercase letters normalize to their physical key + implied Shift
/// (`"?"` → Shift + `/`, `"<plus>"` → Shift + `=`).
pub fn first_chord(keys: &str) -> Option<(Mods, String)> {
    let token = keys.split_whitespace().next()?;
    let inner = token
        .strip_prefix('<')
        .and_then(|t| t.strip_suffix('>'))
        .unwrap_or(token);

    let mut mods = Mods::default();
    let mut rest = inner;
    while let Some((m, tail)) = rest.split_once('-') {
        match m {
            "C" => mods.ctrl = true,
            "S" => mods.shift = true,
            "A" => mods.alt = true,
            // `D` is the config alias for the platform/command key.
            "M" | "D" => mods.meta = true,
            _ => break,
        }
        rest = tail;
    }

    // Bare-modifier chords like `<C->` (wheel bindings) have no tail key.
    if rest.is_empty() {
        return None;
    }

    let (key, implied_shift) = normalize_key(rest)?;
    if implied_shift {
        mods.shift = true;
    }
    Some((mods, key))
}

/// Normalize a chord tail to a physical key id + implied-Shift flag.
fn normalize_key(name: &str) -> Option<(String, bool)> {
    // Single characters first (case matters: `N` → Shift + n).
    let mut chars = name.chars();
    if let (Some(c), None) = (chars.next(), chars.next()) {
        return Some(normalize_char(c));
    }

    let id = match name.to_ascii_lowercase().as_str() {
        "space" => "space",
        "enter" | "return" => "enter",
        "esc" | "escape" => "esc",
        "tab" => "tab",
        "backspace" | "bs" => "backspace",
        "delete" | "del" => "delete",
        "minus" => "-",
        "plus" => return Some(("=".to_string(), true)),
        "up" => "up",
        "down" => "down",
        "left" => "left",
        "right" => "right",
        other => return Some((other.to_string(), false)),
    };
    Some((id.to_string(), false))
}

/// Map a single character to (base key id, implied shift).
fn normalize_char(c: char) -> (String, bool) {
    const SHIFTED: &[(char, char)] = &[
        ('!', '1'),
        ('@', '2'),
        ('#', '3'),
        ('$', '4'),
        ('%', '5'),
        ('^', '6'),
        ('&', '7'),
        ('*', '8'),
        ('(', '9'),
        (')', '0'),
        ('_', '-'),
        ('+', '='),
        ('{', '['),
        ('}', ']'),
        ('|', '\\'),
        (':', ';'),
        ('"', '\''),
        ('<', ','),
        ('>', '.'),
        ('?', '/'),
        ('~', '`'),
    ];
    if let Some(&(_, base)) = SHIFTED.iter().find(|(s, _)| *s == c) {
        return (base.to_string(), true);
    }
    if c.is_ascii_uppercase() {
        return (c.to_ascii_lowercase().to_string(), true);
    }
    (c.to_string(), false)
}

/// Pretty display name for a physical key id (`"space"` → `Space`).
fn display_key_name(id: &str) -> String {
    match id {
        "space" => "Space".to_string(),
        "enter" => "Enter".to_string(),
        "esc" => "Esc".to_string(),
        "tab" => "Tab".to_string(),
        "backspace" => "Backspace".to_string(),
        "delete" => "Delete".to_string(),
        "up" => "Up".to_string(),
        "down" => "Down".to_string(),
        "left" => "Left".to_string(),
        "right" => "Right".to_string(),
        k if k.len() == 1 => k.to_uppercase(),
        k => k.to_string(),
    }
}

// ---------------------------------------------------------------------------
// Layout
// ---------------------------------------------------------------------------

/// One physical key: id used for binding lookup, printed legend, the
/// shifted legend (if any), and width in keyboard units (1u = a letter key).
struct KeySpec {
    id: &'static str,
    label: &'static str,
    shifted: Option<&'static str>,
    units: f32,
}

const fn key(id: &'static str, label: &'static str, shifted: Option<&'static str>, units: f32) -> KeySpec {
    KeySpec { id, label, shifted, units }
}

/// Modifier key ids — rendered as toggles, not binding targets.
const MOD_IDS: &[&str] = &["ctrl", "shift", "alt", "meta"];

/// ANSI-ish main block, 15u per row. Esc sits in the backtick position
/// (60%-style) since the profiles bind `<escape>` but not backtick.
fn layout() -> Vec<Vec<KeySpec>> {
    vec![
        vec![
            key("esc", "Esc", None, 1.0),
            key("1", "1", Some("!"), 1.0),
            key("2", "2", Some("@"), 1.0),
            key("3", "3", Some("#"), 1.0),
            key("4", "4", Some("$"), 1.0),
            key("5", "5", Some("%"), 1.0),
            key("6", "6", Some("^"), 1.0),
            key("7", "7", Some("&"), 1.0),
            key("8", "8", Some("*"), 1.0),
            key("9", "9", Some("("), 1.0),
            key("0", "0", Some(")"), 1.0),
            key("-", "-", Some("_"), 1.0),
            key("=", "=", Some("+"), 1.0),
            key("backspace", "Backspace", None, 2.0),
        ],
        vec![
            key("tab", "Tab", None, 1.5),
            key("q", "Q", None, 1.0),
            key("w", "W", None, 1.0),
            key("e", "E", None, 1.0),
            key("r", "R", None, 1.0),
            key("t", "T", None, 1.0),
            key("y", "Y", None, 1.0),
            key("u", "U", None, 1.0),
            key("i", "I", None, 1.0),
            key("o", "O", None, 1.0),
            key("p", "P", None, 1.0),
            key("[", "[", Some("{"), 1.0),
            key("]", "]", Some("}"), 1.0),
            key("\\", "\\", Some("|"), 1.5),
        ],
        vec![
            key("caps", "Caps", None, 1.75),
            key("a", "A", None, 1.0),
            key("s", "S", None, 1.0),
            key("d", "D", None, 1.0),
            key("f", "F", None, 1.0),
            key("g", "G", None, 1.0),
            key("h", "H", None, 1.0),
            key("j", "J", None, 1.0),
            key("k", "K", None, 1.0),
            key("l", "L", None, 1.0),
            key(";", ";", Some(":"), 1.0),
            key("'", "'", Some("\""), 1.0),
            key("enter", "Enter", None, 2.25),
        ],
        vec![
            key("shift", "Shift", None, 2.25),
            key("z", "Z", None, 1.0),
            key("x", "X", None, 1.0),
            key("c", "C", None, 1.0),
            key("v", "V", None, 1.0),
            key("b", "B", None, 1.0),
            key("n", "N", None, 1.0),
            key("m", "M", None, 1.0),
            key(",", ",", Some("<"), 1.0),
            key(".", ".", Some(">"), 1.0),
            key("/", "/", Some("?"), 1.0),
            key("shift", "Shift", None, 2.75),
        ],
        vec![
            key("ctrl", "Ctrl", None, 1.25),
            key("meta", "Meta", None, 1.25),
            key("alt", "Alt", None, 1.25),
            key("space", "", None, 6.25),
            key("alt", "Alt", None, 1.25),
            key("meta", "Meta", None, 1.25),
            key("ctrl", "Ctrl", None, 2.5),
        ],
    ]
}

// ---------------------------------------------------------------------------
// Component
// ---------------------------------------------------------------------------

/// One binding entry fed to the keyboard: (category title, binding).
pub type KeyboardBinding = (String, KeybindDef);

/// The interactive keyboard map. `filter` is shared with the page so that
/// clicking a key filters the binding list rendered below the keyboard.
#[component]
pub fn KeyboardMap(bindings: Vec<KeyboardBinding>, filter: Signal<Option<KeyFilter>>) -> Element {
    let mut mods = use_signal(Mods::default);
    let mut hovered = use_signal(|| None::<&'static str>);

    // (mods, key id) → indices into `bindings`, keyed on FIRST chord.
    let mut lookup: HashMap<(Mods, String), Vec<usize>> = HashMap::new();
    for (i, (_, b)) in bindings.iter().enumerate() {
        if let Some(chord) = first_chord(&b.keys) {
            lookup.entry(chord).or_default().push(i);
        }
    }

    let m = mods();
    let hov = hovered();
    let active_filter = filter();

    // Render one non-modifier key (main block or arrow pad).
    let render_key = |id: &'static str,
                      label: &'static str,
                      shifted: Option<&'static str>,
                      grow: Option<f32>| {
        let binds = lookup.get(&(m, id.to_string())).cloned().unwrap_or_default();
        let count = binds.len();
        let is_hovered = hov == Some(id);
        let is_filtered = active_filter
            .as_ref()
            .is_some_and(|f| f.mods == m && f.key == id);

        let base = "relative w-full h-9 sm:h-10 rounded-md border text-[0.65rem] sm:text-xs font-medium flex flex-col items-center justify-center transition-colors select-none";
        let tone = if is_filtered {
            "bg-primary text-primary-foreground border-primary shadow-lg shadow-primary/30"
        } else if count >= 3 {
            "bg-primary/60 border-primary/70 text-foreground cursor-pointer hover:bg-primary/70"
        } else if count == 2 {
            "bg-primary/40 border-primary/60 text-foreground cursor-pointer hover:bg-primary/55"
        } else if count == 1 {
            "bg-primary/20 border-primary/40 text-foreground cursor-pointer hover:bg-primary/35"
        } else {
            "bg-muted/20 border-border/40 text-muted-foreground/40"
        };

        // With Shift toggled, show the shifted legend as the primary one.
        let (main_label, sub_label) = if m.shift && shifted.is_some() {
            (shifted.unwrap(), Some(label))
        } else {
            (label, shifted)
        };

        let style = grow
            .map(|u| format!("flex: {u} 1 0%; min-width: 0;"))
            .unwrap_or_default();

        rsx! {
            div {
                class: "relative",
                style: "{style}",
                button {
                    class: "{base} {tone}",
                    onmouseenter: move |_| hovered.set(Some(id)),
                    onmouseleave: move |_| hovered.set(None),
                    onclick: move |_| {
                        if count == 0 {
                            return;
                        }
                        let next = KeyFilter { mods: m, key: id.to_string() };
                        if filter().as_ref() == Some(&next) {
                            filter.set(None);
                        } else {
                            filter.set(Some(next));
                        }
                    },
                    span { "{main_label}" }
                    if let Some(sub) = sub_label {
                        span { class: "text-[0.5rem] leading-none opacity-50", "{sub}" }
                    }
                    if count > 1 {
                        span {
                            class: "absolute top-0.5 right-1 text-[0.5rem] leading-none opacity-70",
                            "{count}"
                        }
                    }
                }
                if is_hovered && count > 0 {
                    KeyTooltip {
                        entries: binds
                            .iter()
                            .map(|&i| bindings[i].clone())
                            .collect::<Vec<_>>(),
                    }
                }
            }
        }
    };

    // Render a modifier key as a toggle.
    let render_mod = |id: &'static str, label: &'static str, units: f32| {
        let active = match id {
            "ctrl" => m.ctrl,
            "shift" => m.shift,
            "alt" => m.alt,
            _ => m.meta,
        };
        let tone = if active {
            "bg-accent text-foreground border-primary/60 ring-1 ring-primary/40"
        } else {
            "bg-muted/30 border-border/50 text-muted-foreground hover:text-foreground hover:bg-accent/40 cursor-pointer"
        };
        rsx! {
            button {
                class: "w-full h-9 sm:h-10 rounded-md border text-[0.65rem] sm:text-xs font-medium flex items-center justify-center transition-colors select-none cursor-pointer {tone}",
                style: "flex: {units} 1 0%; min-width: 0;",
                onclick: move |_| {
                    let mut next = mods();
                    match id {
                        "ctrl" => next.ctrl = !next.ctrl,
                        "shift" => next.shift = !next.shift,
                        "alt" => next.alt = !next.alt,
                        _ => next.meta = !next.meta,
                    }
                    mods.set(next);
                },
                "{label}"
            }
        }
    };

    let chord_label = m.label();

    rsx! {
        div { class: "rounded-xl border border-border/60 bg-card/40 p-3 sm:p-4 mb-8",

            // Header: modifier chips + hint / clear.
            div { class: "flex flex-wrap items-center gap-2 mb-3",
                span { class: "text-xs text-muted-foreground mr-1", "Chord:" }
                for (mid, mlabel) in [("ctrl", "Ctrl"), ("shift", "Shift"), ("alt", "Alt"), ("meta", "Meta")] {
                    {
                        let active = match mid {
                            "ctrl" => m.ctrl,
                            "shift" => m.shift,
                            "alt" => m.alt,
                            _ => m.meta,
                        };
                        rsx! {
                            button {
                                key: "{mid}",
                                class: if active {
                                    "px-2.5 py-1 rounded-full text-xs font-medium bg-primary/30 text-foreground border border-primary/50"
                                } else {
                                    "px-2.5 py-1 rounded-full text-xs font-medium text-muted-foreground border border-border/50 hover:text-foreground hover:bg-accent/40"
                                },
                                onclick: move |_| {
                                    let mut next = mods();
                                    match mid {
                                        "ctrl" => next.ctrl = !next.ctrl,
                                        "shift" => next.shift = !next.shift,
                                        "alt" => next.alt = !next.alt,
                                        _ => next.meta = !next.meta,
                                    }
                                    mods.set(next);
                                },
                                "{mlabel}"
                            }
                        }
                    }
                }
                span { class: "text-xs text-muted-foreground/70 ml-auto hidden sm:inline",
                    if chord_label.is_empty() {
                        "Toggle a modifier to preview chords · click a lit key to filter"
                    } else {
                        "Showing bindings under {chord_label} · click a lit key to filter"
                    }
                }
            }

            div { class: "flex gap-3 items-end",

                // Main block
                div { class: "flex-1 min-w-0 flex flex-col gap-1",
                    for (r, row) in layout().iter().enumerate() {
                        div { key: "{r}", class: "flex gap-1",
                            for k in row.iter() {
                                if MOD_IDS.contains(&k.id) {
                                    {render_mod(k.id, k.label, k.units)}
                                } else {
                                    {render_key(k.id, k.label, k.shifted, Some(k.units))}
                                }
                            }
                        }
                    }
                }

                // Arrow pad — <up>/<down>/<left>/<right> are real bindings.
                div { class: "hidden md:flex flex-col gap-1 shrink-0 w-32",
                    div { class: "flex gap-1",
                        div { class: "flex-1" }
                        div { class: "flex-1", {render_key("up", "\u{2191}", None, None)} }
                        div { class: "flex-1" }
                    }
                    div { class: "flex gap-1",
                        div { class: "flex-1", {render_key("left", "\u{2190}", None, None)} }
                        div { class: "flex-1", {render_key("down", "\u{2193}", None, None)} }
                        div { class: "flex-1", {render_key("right", "\u{2192}", None, None)} }
                    }
                }
            }
        }
    }
}

/// Tooltip listing the bindings that live on a hovered key.
#[component]
fn KeyTooltip(entries: Vec<KeyboardBinding>) -> Element {
    rsx! {
        div {
            class: "absolute z-50 top-full left-1/2 -translate-x-1/2 mt-1.5 w-72 max-w-[80vw] rounded-lg border border-border/70 bg-popover shadow-xl p-2 pointer-events-none",
            for (i, (category, b)) in entries.iter().enumerate() {
                div {
                    key: "{i}",
                    class: "flex items-start gap-2 px-1.5 py-1",
                    div { class: "shrink-0 flex items-center gap-0.5 flex-wrap pt-0.5",
                        for (ci, chord) in pretty_keys(&b.keys).iter().enumerate() {
                            if ci > 0 {
                                span { class: "text-muted-foreground/60 text-[0.6rem]", "then" }
                            }
                            span { class: "inline-flex items-center gap-0.5",
                                for (kii, kk) in chord.iter().enumerate() {
                                    if kii > 0 {
                                        span { class: "text-muted-foreground/60 text-[0.6rem]", "+" }
                                    }
                                    Kbd { "{kk}" }
                                }
                            }
                        }
                    }
                    div { class: "min-w-0 flex-1",
                        div { class: "text-xs text-foreground leading-snug",
                            {b.desc.clone().unwrap_or_default()}
                        }
                        div { class: "text-[0.65rem] text-muted-foreground", "{category}" }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn chord(keys: &str) -> (Mods, String) {
        first_chord(keys).expect("chord should parse")
    }

    #[test]
    fn first_chord_parses_plain_and_modified_keys() {
        assert_eq!(chord("r"), (Mods::default(), "r".to_string()));
        assert_eq!(
            chord("<C-s>"),
            (Mods { ctrl: true, ..Default::default() }, "s".to_string())
        );
        assert_eq!(
            chord("<C-S-space>"),
            (Mods { ctrl: true, shift: true, ..Default::default() }, "space".to_string())
        );
        // Only the FIRST chord of a sequence counts.
        assert_eq!(chord("g g"), (Mods::default(), "g".to_string()));
        // `D` is the platform/command modifier alias.
        assert_eq!(
            chord("<D-1>"),
            (Mods { meta: true, ..Default::default() }, "1".to_string())
        );
        assert_eq!(
            chord("<M-z>"),
            (Mods { meta: true, ..Default::default() }, "z".to_string())
        );
    }

    #[test]
    fn first_chord_normalizes_shifted_symbols() {
        // `?` is Shift + `/` on the physical board.
        assert_eq!(
            chord("?"),
            (Mods { shift: true, ..Default::default() }, "/".to_string())
        );
        assert_eq!(
            chord(":"),
            (Mods { shift: true, ..Default::default() }, ";".to_string())
        );
        assert_eq!(
            chord("<plus>"),
            (Mods { shift: true, ..Default::default() }, "=".to_string())
        );
        assert_eq!(chord("<minus>"), (Mods::default(), "-".to_string()));
        assert_eq!(chord(","), (Mods::default(), ",".to_string()));
        // `-` alone must not be eaten by the modifier splitter.
        assert_eq!(chord("-"), (Mods::default(), "-".to_string()));
    }

    #[test]
    fn bare_modifier_chords_do_not_map_to_a_key() {
        assert_eq!(first_chord("<C->"), None);
    }

    #[test]
    fn filter_matching() {
        let f = KeyFilter {
            mods: Mods { ctrl: true, ..Default::default() },
            key: "s".to_string(),
        };
        assert!(binding_matches("<C-s>", &f));
        assert!(!binding_matches("s", &f));
        assert!(!binding_matches("<C-S-s>", &f));
        assert_eq!(f.label(), "Ctrl + S");
    }
}
