//! Top-level `App` component — platform launchers mount this.
//!
//! Wires the theme provider + per-org overrides, then mounts the
//! router. Per-route layout lives in
//! [`crate::shell::app_shell::AppShell`].

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::prelude::*;
use uuid::Uuid;

use crate::data::{Organization, organizations};
use crate::orgs::{OrgMeta, OrgSelection, fetch_orgs};
use crate::routes::Route;
use crate::theming::{OrgThemeOverrides, ProjectThemeOverrides, state_from_preset_name};

#[component]
pub fn App() -> Element {
    let orgs = organizations();
    let initial_org = orgs[0].clone();
    let active_org: Signal<Organization> = use_context_provider(move || Signal::new(initial_org));

    // Multi-org data selection: which org(s) the data views load from.
    // Defaults to `All` (every hosted org); the org switcher drives it.
    let _org_selection: Signal<OrgSelection> =
        use_context_provider(|| Signal::new(OrgSelection::All));
    // Hosted org list, discovered from the server's well-known endpoint
    // and published for the switcher + data fetchers.
    let mut org_list: Signal<Vec<OrgMeta>> = use_context_provider(|| Signal::new(Vec::new()));
    let orgs_res = use_resource(|| async move { fetch_orgs().await });
    use_effect(move || {
        if let Some(Ok(list)) = &*orgs_res.read_unchecked() {
            if *org_list.peek() != *list {
                org_list.set(list.clone());
            }
        }
    });

    let org_overrides: OrgThemeOverrides = use_context_provider(|| OrgThemeOverrides {
        map: Signal::new(HashMap::<String, String>::new()),
        mode: Signal::new(ThemeMode::Dark),
    });

    let _project_overrides: ProjectThemeOverrides =
        use_context_provider(|| ProjectThemeOverrides {
            map: Signal::new(HashMap::<Uuid, String>::new()),
        });

    let mut theme_state = use_signal(|| {
        let org = active_org.read().clone();
        state_from_preset_name(org.theme_preset, ThemeMode::Dark)
    });

    use_effect(move || {
        let org = active_org.read().clone();
        let resolved_name: String = org_overrides
            .map
            .read()
            .get(org.id)
            .cloned()
            .unwrap_or_else(|| org.theme_preset.to_string());
        let preset = theme_preset(&resolved_name).unwrap_or_else(default_theme_preset);
        theme_state.write().set_preset(preset);
    });

    use_effect(move || {
        let mode = *org_overrides.mode.read();
        if theme_state.peek().mode != mode {
            theme_state.write().set_mode(mode);
        }
    });

    rsx! {
        ThemeProvider { state: theme_state,
            div { class: "min-h-screen bg-background text-foreground",
                Router::<Route> {}
            }
        }
    }
}
