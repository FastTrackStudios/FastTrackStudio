//! /input — the keyboard-configuration tutorial.
//!
//! Renders the canonical keybind profiles (embedded from
//! features/reaper/reaper-input/config/config at build time) grouped by
//! category — Transport, Navigation, Editing, … — exactly the sections
//! the styx files are organized into. Each binding shows its key
//! sequence (pretty-printed as key caps), what it does, and where it
//! applies.
//!
//! This is the static seed of the input platform: the same
//! `input-config-proto` types will later be fed from
//! `InputConfigService` (your live rig / hub profiles) instead of the
//! embedded snapshot — the rendering stays identical.

use dioxus::prelude::*;
use architect_ui::prelude::*;
use input_config_proto::{
    KeybindContext, KeybindDef, ProfileConfig, SectionConfig, WheelBindDef, kebab_to_title,
};

pub(crate) mod embedded {
    include!(concat!(env!("OUT_DIR"), "/input_profiles.rs"));
}
use embedded::{EmbeddedProfile, PROFILES};

use super::colors::category_color;
use super::keyboard::{KeyFilter, KeyboardMap, Mods, binding_matches, first_chord};
use super::modes::{Mode, load_modes};

/// A parsed category (one section file) of the active profile.
struct Category {
    id: String,
    title: String,
    config: SectionConfig,
}

fn load_categories(profile: &EmbeddedProfile) -> (String, Vec<Category>) {
    let meta: Option<ProfileConfig> = facet_styx::from_str(profile.profile_styx).ok();
    let display_name = meta
        .as_ref()
        .map(|m| m.name.clone())
        .unwrap_or_else(|| kebab_to_title(profile.id));

    // profile.styx's `sections` list carries the intended order; sections
    // not listed there (or unparseable) fall to the end alphabetically.
    let order: Vec<String> = meta
        .map(|m| {
            m.sections
                .iter()
                .map(|s| s.trim_end_matches(".styx").to_string())
                .collect()
        })
        .unwrap_or_default();

    let mut categories: Vec<Category> = profile
        .sections
        .iter()
        .filter_map(|s| {
            let config: SectionConfig = facet_styx::from_str(s.styx).ok()?;
            let empty = config.bindings().is_empty()
                && config.wheel().is_empty()
                && config.which_key().is_empty();
            (!empty).then(|| Category {
                id: s.id.to_string(),
                title: kebab_to_title(s.id),
                config,
            })
        })
        .collect();
    categories.sort_by_key(|c| {
        (
            order.iter().position(|o| *o == c.id).unwrap_or(usize::MAX),
            c.id.clone(),
        )
    });
    (display_name, categories)
}

/// Split a key-sequence string (`"g g"`, `"<C-S-space>"`, `"r"`) into
/// display tokens, one per pressed chord.
pub(crate) fn pretty_keys(keys: &str) -> Vec<Vec<String>> {
    keys.split_whitespace().map(pretty_chord).collect()
}

/// `"<C-S-space>"` → ["Ctrl", "Shift", "Space"]; `"r"` → ["R"].
pub(crate) fn pretty_chord(token: &str) -> Vec<String> {
    let inner = token
        .strip_prefix('<')
        .and_then(|t| t.strip_suffix('>'))
        .unwrap_or(token);

    let mut parts = Vec::new();
    let mut rest = inner;
    while let Some((m, tail)) = rest.split_once('-') {
        if !matches!(m, "C" | "S" | "A" | "M") {
            break;
        }
        parts.push(
            match m {
                "C" => "Ctrl",
                "S" => "Shift",
                "A" => "Alt",
                _ => "Meta",
            }
            .to_string(),
        );
        rest = tail;
    }

    // Bare-modifier strings like `<C->` (wheel bindings) have no tail key.
    if rest.is_empty() {
        return parts;
    }

    let key = match rest {
        "space" => "Space".to_string(),
        "enter" | "return" => "Enter".to_string(),
        "esc" | "escape" => "Esc".to_string(),
        "tab" => "Tab".to_string(),
        "backspace" => "Backspace".to_string(),
        "delete" | "del" => "Delete".to_string(),
        "up" | "down" | "left" | "right" => {
            let mut s = rest.to_string();
            s[..1].make_ascii_uppercase();
            format!("{s} Arrow")
        }
        k if k.len() == 1 => k.to_uppercase(),
        k => k.to_string(),
    };
    parts.push(key);
    parts
}

fn context_label(ctx: Option<KeybindContext>) -> Option<&'static str> {
    match ctx.unwrap_or_default() {
        KeybindContext::Global => None,
        KeybindContext::Main => Some("Arrange"),
        KeybindContext::Midi => Some("MIDI Editor"),
        KeybindContext::MidiInline => Some("Inline MIDI"),
        KeybindContext::MediaExplorer => Some("Media Explorer"),
        KeybindContext::Custom(_) => None,
    }
}

/// The only profile rendered for now. The other profiles stay embedded
/// and every code path still handles them — flip this back to a picker
/// by iterating `PROFILES` again.
const ACTIVE_PROFILE: &str = "fasttrackstudio";

/// The /input tutorial page. `initial_category` preselects a category
/// sidebar entry (used by guide deep-links like `/input?category=transport`).
#[component]
pub fn InputTutorial(#[props(default)] initial_category: String) -> Element {
    let mut active_category = use_signal(|| initial_category.clone());
    let mut key_filter = use_signal(|| None::<KeyFilter>);
    let mut active_mode = use_signal(|| None::<String>);

    let profile = PROFILES
        .iter()
        .find(|p| p.id == ACTIVE_PROFILE)
        .or(PROFILES.first());
    let Some(profile) = profile else {
        return rsx! { div { class: "p-8", "No keybind profiles embedded." } };
    };
    let (display_name, categories) = load_categories(profile);

    // Modes/workflows are shared across profiles — they layer overlays
    // over whatever profile is active.
    let modes = load_modes();
    let active_mode_id = active_mode();
    let mode: Option<&Mode> = active_mode_id
        .as_ref()
        .and_then(|id| modes.iter().find(|m| &m.id == id));

    // Fall back to "All" when the selected category doesn't exist in this
    // profile (e.g. a stale deep-link query).
    let mut current = active_category();
    if !current.is_empty() && !categories.iter().any(|c| c.id == current) {
        current = String::new();
    }
    let shown: Vec<&Category> = if current.is_empty() {
        categories.iter().collect()
    } else {
        categories.iter().filter(|c| c.id == current).collect()
    };

    // Everything the keyboard map highlights: the bindings of the active
    // selection, tagged with their category id (drives the key colors),
    // plus the active mode's layered bindings tagged with the mode id.
    let mut keyboard_bindings: Vec<(String, KeybindDef)> = shown
        .iter()
        .flat_map(|c| {
            c.config
                .bindings()
                .iter()
                .map(|b| (c.id.clone(), b.clone()))
        })
        .collect();
    if let Some(m) = mode {
        keyboard_bindings.extend(m.bindings.iter().map(|b| (m.id.clone(), b.clone())));
    }
    let filter = key_filter();

    // First chords of the WHOLE base profile — a mode binding landing on
    // one of these overrides it while the mode is active.
    let base_chords: std::collections::HashSet<(Mods, String)> = categories
        .iter()
        .flat_map(|c| c.config.bindings().iter())
        .filter_map(|b| first_chord(&b.keys))
        .collect();
    // First chords the active mode claims — base bindings on these keys
    // are shadowed while the mode is on.
    let mode_overrides: Option<ModeOverrides> = mode.map(|m| ModeOverrides {
        name: m.name.clone(),
        color: m.color(),
        chords: m
            .bindings
            .iter()
            .filter_map(|b| first_chord(&b.keys))
            .collect(),
    });

    rsx! {
        div { class: "max-w-7xl mx-auto px-4 lg:px-8 py-10",

            // Header
            div { class: "mb-8",
                h1 { class: "text-3xl font-bold tracking-tight", "Input & Shortcuts" }
                p { class: "mt-2 text-muted-foreground max-w-3xl",
                    "How {display_name} is played: every shortcut, organized by what "
                    "you're doing — transport, navigation, editing, and more. These are "
                    "the canonical profiles that ship with FastTrackStudio; soon this "
                    "page will render "
                    span { class: "text-foreground font-medium", "your" }
                    " configuration and let you share your own workflows."
                }
            }

            // Mode / workflow selector. Modes (mode-*) are the modal
            // states — one active at a time; the rest are toggleable
            // workflows. Selecting one composes its overlay bindings
            // over the base profile everywhere below.
            div { class: "flex flex-wrap items-center gap-x-2 gap-y-1.5 mb-2",
                span { class: "text-sm text-muted-foreground mr-1", "Mode:" }
                button {
                    class: if active_mode_id.is_none() {
                        "px-2.5 py-1 rounded-full text-xs font-medium bg-accent/70 text-foreground"
                    } else {
                        "px-2.5 py-1 rounded-full text-xs font-medium text-muted-foreground border border-border/50 hover:text-foreground hover:bg-accent/40 transition-colors"
                    },
                    onclick: move |_| {
                        active_mode.set(None);
                        key_filter.set(None);
                    },
                    "None"
                }
                for m in modes.iter().filter(|m| m.is_modal) {
                    ModeChip {
                        key: "{m.id}",
                        id: m.id.clone(),
                        name: m.name.clone(),
                        color: m.color(),
                        active: active_mode_id.as_deref() == Some(m.id.as_str()),
                        on_select: move |id: Option<String>| {
                            active_mode.set(id);
                            key_filter.set(None);
                        },
                    }
                }
            }
            div { class: "flex flex-wrap items-center gap-x-2 gap-y-1.5 mb-6",
                span { class: "text-sm text-muted-foreground mr-1", "Workflows:" }
                for m in modes.iter().filter(|m| !m.is_modal) {
                    ModeChip {
                        key: "{m.id}",
                        id: m.id.clone(),
                        name: m.name.clone(),
                        color: m.color(),
                        active: active_mode_id.as_deref() == Some(m.id.as_str()),
                        on_select: move |id: Option<String>| {
                            active_mode.set(id);
                            key_filter.set(None);
                        },
                    }
                }
            }

            // Interactive keyboard map — highlights the keys bound in the
            // active profile + category selection, colored per category;
            // mode-layered keys glow in the mode's color.
            KeyboardMap {
                bindings: keyboard_bindings,
                filter: key_filter,
                on_select_category: move |id: String| {
                    active_category.set(id);
                    key_filter.set(None);
                },
                mode_id: active_mode_id.clone(),
            }

            // Active key filter chip (set by clicking a key above).
            if let Some(f) = filter.clone() {
                div { class: "flex items-center gap-2 mb-6 -mt-4",
                    span { class: "text-sm text-muted-foreground", "Filtered by" }
                    button {
                        class: "inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full text-xs font-medium bg-primary/20 border border-primary/40 text-foreground hover:bg-primary/30 transition-colors",
                        onclick: move |_| key_filter.set(None),
                        "{f.label()}"
                        span { class: "text-muted-foreground", "\u{00D7}" }
                    }
                }
            }

            div { class: "flex gap-8 items-start",

                // Category sidebar
                nav { class: "hidden lg:block w-52 shrink-0 sticky top-24",
                    div { class: "text-xs uppercase tracking-wider text-muted-foreground mb-2", "Categories" }
                    button {
                        class: if current.is_empty() {
                            "block w-full text-left px-2 py-1 rounded text-sm bg-accent/60 text-foreground"
                        } else {
                            "block w-full text-left px-2 py-1 rounded text-sm text-muted-foreground hover:text-foreground hover:bg-accent/30"
                        },
                        onclick: move |_| {
                            active_category.set(String::new());
                            key_filter.set(None);
                        },
                        "All"
                    }
                    for c in categories.iter() {
                        button {
                            key: "{c.id}",
                            class: if current == c.id {
                                "flex w-full items-center text-left px-2 py-1 rounded text-sm bg-accent/60 text-foreground"
                            } else {
                                "flex w-full items-center text-left px-2 py-1 rounded text-sm text-muted-foreground hover:text-foreground hover:bg-accent/30"
                            },
                            onclick: {
                                let id = c.id.clone();
                                move |_| {
                                    active_category.set(id.clone());
                                    key_filter.set(None);
                                }
                            },
                            span {
                                class: "inline-block w-2 h-2 rounded-full mr-1.5 shrink-0",
                                style: "background-color: {category_color(&c.id)};",
                            }
                            "{c.title}"
                            span { class: "ml-1 text-xs text-muted-foreground/70",
                                "{c.config.bindings().len() + c.config.wheel().len()}"
                            }
                        }
                    }
                }

                // Category sections
                div { class: "flex-1 min-w-0 space-y-10",
                    // Active mode first: what it adds/changes + settings.
                    if let Some(m) = mode {
                        ModeSection {
                            mode: m.clone(),
                            filter: filter.clone(),
                            base_chords: base_chords.iter().cloned().collect::<Vec<_>>(),
                        }
                    }
                    for c in shown {
                        CategorySection {
                            key: "{profile.id}/{c.id}",
                            category_id: c.id.clone(),
                            title: c.title.clone(),
                            config: c.config.clone(),
                            filter: filter.clone(),
                            mode_overrides: mode_overrides.clone(),
                        }
                    }
                }
            }
        }
    }
}

/// What the active mode shadows: base bindings whose first chord matches
/// one of `chords` get an "overridden" tag while the mode is on.
#[derive(Clone, PartialEq)]
struct ModeOverrides {
    name: String,
    color: &'static str,
    chords: Vec<(Mods, String)>,
}

/// One selectable mode/workflow chip.
#[component]
fn ModeChip(
    id: String,
    name: String,
    color: &'static str,
    active: bool,
    on_select: EventHandler<Option<String>>,
) -> Element {
    let style = if active {
        format!("color: {color}; background-color: {color}26; border-color: {color}80;")
    } else {
        String::new()
    };
    let class = if active {
        "inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full text-xs font-medium border transition-colors"
    } else {
        "inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full text-xs font-medium border border-border/50 text-muted-foreground hover:text-foreground hover:bg-accent/40 transition-colors"
    };
    rsx! {
        button {
            class: "{class}",
            style: "{style}",
            onclick: move |_| {
                // Clicking the active chip deselects (back to no mode).
                on_select.call((!active).then(|| id.clone()));
            },
            span {
                class: "inline-block w-1.5 h-1.5 rounded-full",
                style: "background-color: {color};",
            }
            "{name}"
        }
    }
}

/// The active mode's own section: the bindings it layers over the base
/// profile (tagged "overrides" where they shadow a base key) and the
/// REAPER settings it flips while active.
#[component]
fn ModeSection(mode: Mode, #[props(default)] filter: Option<KeyFilter>, base_chords: Vec<(Mods, String)>) -> Element {
    let color = mode.color();

    let rows: Vec<(KeybindDef, bool)> = mode
        .bindings
        .iter()
        .filter(|b| filter.as_ref().is_none_or(|f| binding_matches(&b.keys, f)))
        .map(|b| {
            let overrides = first_chord(&b.keys)
                .is_some_and(|c| base_chords.contains(&c));
            (b.clone(), overrides)
        })
        .collect();

    if rows.is_empty() && filter.is_some() {
        return rsx! {};
    }

    rsx! {
        section { id: "mode-{mode.id}",
            h2 { class: "text-xl font-semibold mb-1 flex items-center gap-2",
                span {
                    class: "inline-block w-2.5 h-2.5 rounded-full shrink-0",
                    style: "background-color: {color};",
                }
                span { style: "color: {color};", "Mode: {mode.name}" }
                span { class: "text-xs font-normal text-muted-foreground",
                    "adds/changes these"
                }
            }
            if !mode.description.is_empty() {
                p { class: "text-sm text-muted-foreground mb-3", "{mode.description}" }
            }

            // Settings flipped while the mode is active.
            if !mode.settings.is_empty() || mode.armed_action.is_some() {
                div {
                    class: "rounded-lg border px-4 py-3 mb-3 text-sm",
                    style: "border-color: {color}40; background-color: {color}0d;",
                    div { class: "text-xs uppercase tracking-wider mb-1.5", style: "color: {color};",
                        "While in this mode"
                    }
                    for (i, s) in mode.settings.iter().enumerate() {
                        div { key: "{i}", class: "flex items-center gap-2 py-0.5",
                            span {
                                class: "shrink-0 text-[0.65rem] px-1.5 py-px rounded-full font-medium",
                                style: "color: {color}; background-color: {color}1a;",
                                if s.enabled { "on" } else { "off" }
                            }
                            span { class: "text-muted-foreground",
                                {s.desc.clone().unwrap_or_else(|| format!("Command {}", s.command))}
                            }
                        }
                    }
                    if let Some(a) = mode.armed_action.as_ref() {
                        div { class: "flex items-center gap-2 py-0.5",
                            span {
                                class: "shrink-0 text-[0.65rem] px-1.5 py-px rounded-full font-medium",
                                style: "color: {color}; background-color: {color}1a;",
                                "armed"
                            }
                            span { class: "text-muted-foreground",
                                {a.name.clone().unwrap_or_else(|| a.command.clone())}
                            }
                        }
                    }
                }
            }

            div {
                class: "rounded-xl border overflow-hidden",
                style: "border-color: {color}40;",
                for (i, (b, overrides)) in rows.iter().enumerate() {
                    BindingRow {
                        key: "{i}",
                        binding: b.clone(),
                        zebra: i % 2 == 1,
                        tag: overrides.then(|| ("overrides".to_string(), color.to_string())),
                    }
                }
            }
        }
    }
}

#[component]
fn CategorySection(
    category_id: String,
    title: String,
    config: SectionConfig,
    #[props(default)] filter: Option<KeyFilter>,
    #[props(default)] mode_overrides: Option<ModeOverrides>,
) -> Element {
    // With a key filter active, show only the bindings whose first chord
    // lands on the filtered key (wheel bindings are keyboard-less — hide).
    let bindings: Vec<KeybindDef> = config
        .bindings()
        .iter()
        .filter(|b| {
            filter
                .as_ref()
                .is_none_or(|f| binding_matches(&b.keys, f))
        })
        .cloned()
        .collect();
    let show_wheel = filter.is_none();

    if bindings.is_empty() && (!show_wheel || config.wheel().is_empty()) {
        return rsx! {};
    }

    let accent = category_color(&category_id);

    rsx! {
        section { id: "{category_id}",
            h2 { class: "text-xl font-semibold mb-3 flex items-center gap-2",
                span {
                    class: "inline-block w-2.5 h-2.5 rounded-full shrink-0",
                    style: "background-color: {accent};",
                }
                "{title}"
                span { class: "text-xs font-normal text-muted-foreground",
                    "{bindings.len()} shortcuts"
                }
            }
            div { class: "rounded-xl border border-border/60 bg-card/40 overflow-hidden",
                for (i, b) in bindings.iter().enumerate() {
                    BindingRow {
                        key: "{i}",
                        binding: b.clone(),
                        zebra: i % 2 == 1,
                        tag: mode_overrides.as_ref().and_then(|mo| {
                            first_chord(&b.keys)
                                .is_some_and(|c| mo.chords.contains(&c))
                                .then(|| (format!("overridden in {}", mo.name), mo.color.to_string()))
                        }),
                    }
                }
                if show_wheel {
                    for (i, w) in config.wheel().iter().enumerate() {
                        WheelRow { key: "w{i}", bind: w.clone(), zebra: (bindings.len() + i) % 2 == 1 }
                    }
                }
            }
        }
    }
}

#[component]
fn BindingRow(
    binding: KeybindDef,
    zebra: bool,
    #[props(default)] tag: Option<(String, String)>,
) -> Element {
    let chords = pretty_keys(&binding.keys);
    let desc = binding.desc.clone().unwrap_or_default();
    let ctx = context_label(binding.context.clone());

    rsx! {
        div {
            class: if zebra {
                "flex items-center gap-4 px-4 py-2 bg-muted/20"
            } else {
                "flex items-center gap-4 px-4 py-2"
            },
            div { class: "w-56 shrink-0 flex items-center gap-1.5 flex-wrap",
                for (ci, chord) in chords.iter().enumerate() {
                    if ci > 0 {
                        span { class: "text-muted-foreground/60 text-xs", "then" }
                    }
                    span { class: "inline-flex items-center gap-0.5",
                        for (ki, k) in chord.iter().enumerate() {
                            if ki > 0 {
                                span { class: "text-muted-foreground/60 text-xs", "+" }
                            }
                            Kbd { "{k}" }
                        }
                    }
                }
            }
            div { class: "flex-1 text-sm",
                "{desc}"
                if let Some(m) = binding.mnemonic.as_ref() {
                    span { class: "ml-2 text-xs italic text-muted-foreground/80",
                        "“{m}”"
                    }
                }
                if let Some(w) = binding.why.as_ref() {
                    details { class: "mt-0.5",
                        summary { class: "text-xs text-muted-foreground/70 cursor-pointer select-none hover:text-foreground",
                            "why this key?"
                        }
                        p { class: "text-xs text-muted-foreground mt-1 max-w-xl", "{w}" }
                    }
                }
            }
            if let Some((label, color)) = tag.as_ref() {
                span {
                    class: "shrink-0 text-[0.65rem] px-2 py-0.5 rounded-full font-medium",
                    style: "color: {color}; background-color: {color}1a; border: 1px solid {color}40;",
                    "{label}"
                }
            }
            if let Some(ctx) = ctx {
                span { class: "shrink-0 text-xs px-2 py-0.5 rounded-full bg-accent/40 text-muted-foreground",
                    "{ctx}"
                }
            }
        }
    }
}

#[component]
fn WheelRow(bind: WheelBindDef, zebra: bool) -> Element {
    let mut chord = pretty_chord(&bind.modifiers);
    // A bare-modifier string like "<C->" leaves an empty tail; drop it.
    chord.retain(|k| !k.is_empty());
    let wheel = if bind.horizontal.unwrap_or(false) {
        "Wheel ↔"
    } else {
        "Wheel ↕"
    };
    let desc = bind.desc.clone().unwrap_or_default();
    let ctx = context_label(bind.context.clone());

    rsx! {
        div {
            class: if zebra {
                "flex items-center gap-4 px-4 py-2 bg-muted/20"
            } else {
                "flex items-center gap-4 px-4 py-2"
            },
            div { class: "w-56 shrink-0 flex items-center gap-0.5",
                for k in chord.iter().filter(|k| k.as_str() != wheel) {
                    Kbd { "{k}" }
                    span { class: "text-muted-foreground/60 text-xs", "+" }
                }
                Kbd { "{wheel}" }
            }
            div { class: "flex-1 text-sm", "{desc}" }
            if let Some(ctx) = ctx {
                span { class: "shrink-0 text-xs px-2 py-0.5 rounded-full bg-accent/40 text-muted-foreground",
                    "{ctx}"
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_embedded_profiles_parse() {
        assert!(!PROFILES.is_empty(), "no profiles embedded");
        for profile in PROFILES {
            let (name, categories) = load_categories(profile);
            assert!(!name.is_empty());
            assert!(
                !categories.is_empty(),
                "profile {} produced no categories — styx parse regression?",
                profile.id
            );
            let bindings: usize = categories.iter().map(|c| c.config.bindings().len()).sum();
            assert!(bindings > 0, "profile {} has no bindings", profile.id);
        }
    }

    #[test]
    fn chord_pretty_printing() {
        assert_eq!(pretty_keys("<C-s>"), vec![vec!["Ctrl".to_string(), "S".into()]]);
        assert_eq!(
            pretty_keys("<C-S-space>"),
            vec![vec!["Ctrl".to_string(), "Shift".into(), "Space".into()]]
        );
        assert_eq!(pretty_keys("g g"), vec![vec!["G".to_string()], vec!["G".to_string()]]);
        assert_eq!(pretty_chord("<C->"), vec!["Ctrl".to_string()]);
    }
}
