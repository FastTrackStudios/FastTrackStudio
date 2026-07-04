//! Two-tier theming for Task: organization → project.
//!
//! Tier 1 — **organization theme**. The `App` root re-renders an
//! `fts_ui::ThemeProvider` keyed on the active org. The preset name is
//! resolved from `OrgThemeOverrides` first (user-picked, in-memory),
//! then falls back to the org's static `theme_preset` field in
//! `crate::data`.
//!
//! Tier 2 — **project override**. When a project route wants to deviate
//! from the org theme it wraps its content in `ProjectThemeScope`,
//! which renders an `fts_ui::ThemeScope` if the project has an entry in
//! `ProjectThemeOverrides`. If not, it returns children unchanged so
//! the org theme shows through.
//!
//! Both override stores are in-memory `Signal<HashMap<...>>` provided
//! at `App` root via `use_context_provider`. v1 has no persistence —
//! a page refresh resets to the org's static default.
//!
//! FUTURE: persist `ProjectThemeOverrides` to localStorage or to a
//! per-project setting on the Project entity; persist
//! `OrgThemeOverrides` per-user.

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::prelude::*;
use uuid::Uuid;

/// User-picked preset for an organization. Keyed by `Organization::id`
/// (which is currently `&'static str` in `crate::data`). Missing entry
/// means "use the org's static default".
///
/// `mode` carries the current light/dark choice. It's intentionally
/// global (not per-org) — users want one consistent mode across the app
/// even when they switch organizations. The popover's `ThemeSwitcher`
/// writes to this through `OrgSwitcher`'s bridge effect; the App-level
/// effect reads it and sets `theme_state.mode` so `ThemeProvider`
/// re-renders the CSS variables for the right palette.
#[derive(Clone, Copy)]
pub struct OrgThemeOverrides {
    pub map: Signal<HashMap<String, String>>,
    pub mode: Signal<ThemeMode>,
}

/// Per-project preset name override. `None` (i.e. missing entry) means
/// "inherit the org theme".
#[derive(Clone, Copy)]
pub struct ProjectThemeOverrides {
    pub map: Signal<HashMap<Uuid, String>>,
}

/// Read the per-project override context. Call from inside the project
/// route or any of its descendants.
#[must_use]
pub fn use_project_theme_overrides() -> ProjectThemeOverrides {
    use_context::<ProjectThemeOverrides>()
}

/// Read the per-org override context.
#[must_use]
pub fn use_org_theme_overrides() -> OrgThemeOverrides {
    use_context::<OrgThemeOverrides>()
}

/// Build a `ThemeState` for a preset name + mode. Falls back to the
/// fts-ui default preset if the name doesn't match anything.
/// The Obsidian dark palette (Cody, 2026-07-03) — kept in lockstep
/// with the static sheet in `apps/web/fts-theme.css`. The runtime
/// `ThemeProvider` writes tokens onto `.fts-theme-root`, which beats
/// the stylesheet's `:root` block for everything inside the app — so
/// the palette must be applied HERE, not only in the CSS file.
/// NOTE: bare token keys — `ThemeStyle::css_variables` prepends the
/// `--` itself.
const OBSIDIAN_DARK: &[(&str, &str)] = &[
    ("background", "#1e1e1e"),
    ("foreground", "#dadada"),
    ("card", "#161616"),
    ("card-foreground", "#dadada"),
    ("popover", "#1e1e1e"),
    ("popover-foreground", "#dadada"),
    ("primary", "#7f6df2"),
    ("primary-foreground", "#fbfbfb"),
    ("sidebar", "#161616"),
];

pub fn state_from_preset_name(name: &str, mode: ThemeMode) -> ThemeState {
    let preset = theme_preset(name).unwrap_or_else(default_theme_preset);
    // Org/project presets keep their own colors; only the default
    // preset gets the Obsidian skin.
    let is_default = preset.name == "default";
    let mut state = ThemeState::new(preset, mode);
    if is_default {
        for (key, value) in OBSIDIAN_DARK {
            state.set_token(ThemeMode::Dark, *key, *value);
        }
    }
    state
}

/// Wraps `children` in a `ThemeScope` when the project has an override,
/// otherwise just renders `children`. `ThemeScope` reads the parent
/// `ThemeContext` to inherit the current mode (light/dark), so the
/// override only swaps the color tokens.
#[component]
pub fn ProjectThemeScope(project_id: Uuid, children: Element) -> Element {
    let overrides = use_project_theme_overrides();
    let name_opt = overrides.map.read().get(&project_id).cloned();

    match name_opt {
        Some(name) => {
            let preset = theme_preset(&name).unwrap_or_else(default_theme_preset);
            rsx! {
                ThemeScope { styles: preset.styles, {children} }
            }
        }
        None => rsx! { {children} },
    }
}
