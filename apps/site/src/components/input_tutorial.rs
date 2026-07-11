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
use fts_ui::prelude::*;
use input_config_proto::{
    KeybindContext, KeybindDef, ProfileConfig, SectionConfig, WheelBindDef, kebab_to_title,
};

pub(crate) mod embedded {
    include!(concat!(env!("OUT_DIR"), "/input_profiles.rs"));
}
use embedded::{EmbeddedProfile, PROFILES};

use super::keyboard::{KeyFilter, KeyboardMap, binding_matches};

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
    }
}

/// The /input tutorial page. `initial_category` preselects a category
/// sidebar entry (used by guide deep-links like `/input?category=transport`).
#[component]
pub fn InputTutorial(#[props(default)] initial_category: String) -> Element {
    let mut active_profile = use_signal(|| "fasttrackstudio".to_string());
    let mut active_category = use_signal(|| initial_category.clone());
    let mut key_filter = use_signal(|| None::<KeyFilter>);

    let profile = PROFILES
        .iter()
        .find(|p| p.id == active_profile())
        .or(PROFILES.first());
    let Some(profile) = profile else {
        return rsx! { div { class: "p-8", "No keybind profiles embedded." } };
    };
    let (display_name, categories) = load_categories(profile);

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
    // selection, tagged with their category title.
    let keyboard_bindings: Vec<(String, KeybindDef)> = shown
        .iter()
        .flat_map(|c| {
            c.config
                .bindings()
                .iter()
                .map(|b| (c.title.clone(), b.clone()))
        })
        .collect();
    let filter = key_filter();

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

            // Profile picker
            div { class: "flex flex-wrap items-center gap-2 mb-6",
                span { class: "text-sm text-muted-foreground mr-1", "Profile:" }
                for p in PROFILES.iter() {
                    button {
                        key: "{p.id}",
                        class: if p.id == profile.id {
                            "px-3 py-1.5 rounded-lg text-sm font-medium bg-accent/70 text-foreground"
                        } else {
                            "px-3 py-1.5 rounded-lg text-sm font-medium text-muted-foreground hover:text-foreground hover:bg-accent/40 transition-colors"
                        },
                        onclick: {
                            let id = p.id.to_string();
                            move |_| {
                                active_profile.set(id.clone());
                                active_category.set(String::new());
                                key_filter.set(None);
                            }
                        },
                        {kebab_to_title(p.id)}
                    }
                }
            }

            // Interactive keyboard map — highlights the keys bound in the
            // active profile + category selection.
            KeyboardMap { bindings: keyboard_bindings, filter: key_filter }

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
                                "block w-full text-left px-2 py-1 rounded text-sm bg-accent/60 text-foreground"
                            } else {
                                "block w-full text-left px-2 py-1 rounded text-sm text-muted-foreground hover:text-foreground hover:bg-accent/30"
                            },
                            onclick: {
                                let id = c.id.clone();
                                move |_| {
                                    active_category.set(id.clone());
                                    key_filter.set(None);
                                }
                            },
                            "{c.title}"
                            span { class: "ml-1 text-xs text-muted-foreground/70",
                                "{c.config.bindings().len() + c.config.wheel().len()}"
                            }
                        }
                    }
                }

                // Category sections
                div { class: "flex-1 min-w-0 space-y-10",
                    for c in shown {
                        CategorySection {
                            key: "{profile.id}/{c.id}",
                            category_id: c.id.clone(),
                            title: c.title.clone(),
                            config: c.config.clone(),
                            filter: filter.clone(),
                        }
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

    rsx! {
        section { id: "{category_id}",
            h2 { class: "text-xl font-semibold mb-3 flex items-center gap-2",
                "{title}"
                span { class: "text-xs font-normal text-muted-foreground",
                    "{bindings.len()} shortcuts"
                }
            }
            div { class: "rounded-xl border border-border/60 bg-card/40 overflow-hidden",
                for (i, b) in bindings.iter().enumerate() {
                    BindingRow { key: "{i}", binding: b.clone(), zebra: i % 2 == 1 }
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
fn BindingRow(binding: KeybindDef, zebra: bool) -> Element {
    let chords = pretty_keys(&binding.keys);
    let desc = binding.desc.clone().unwrap_or_default();
    let ctx = context_label(binding.context);

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
            div { class: "flex-1 text-sm", "{desc}" }
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
    let ctx = context_label(bind.context);

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
