//! Top-level `App` component — platform launchers mount this.
//!
//! Wires the theme provider + per-org overrides, then mounts the
//! router. Per-route layout lives in
//! [`crate::shell::app_shell::AppShell`].

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::prelude::*;
use uuid::Uuid;

use crate::orgs::{OrgMeta, OrgSelection, fetch_orgs, home_slug};
use crate::routes::Route;
use crate::theming::{OrgThemeOverrides, ProjectThemeOverrides, state_from_preset_name};

#[component]
pub fn App() -> Element {
    // App-wide architect registries (notifications + reactivity) and
    // the shared per-org connection: one `Connection<vox::Caller>` for
    // the active org (the selected one, or home under "All"),
    // re-established when the switcher moves. Atom-pattern hooks build
    // typed clients from this caller; multi-org fan-out keeps using
    // `vox_clients::caller_for` per slug (same cached sockets).
    //
    // Note: the connect closure reads the selection + org list
    // signals synchronously — that's what re-triggers it (see
    // `use_connect_reactive`'s dependency contract).
    // Multi-org data selection: which org(s) the data views load from.
    // Defaults to `All` (every hosted org); the org switcher drives it.
    let org_selection: Signal<OrgSelection> =
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

    // Theme follows the active org (the selected one, or home under
    // "All"), keyed by slug in the overrides map. Until discovery
    // resolves, the default preset holds.
    let theme_org_slug = use_memo(move || match &*org_selection.read() {
        OrgSelection::One(slug) => slug.clone(),
        OrgSelection::All => home_slug(&org_list.read()),
    });

    let mut theme_state = use_signal(|| state_from_preset_name("", ThemeMode::Dark));

    use_effect(move || {
        let slug = theme_org_slug();
        let resolved_name: String = org_overrides.map.read().get(&slug).cloned().unwrap_or_default();
        let preset = theme_preset(&resolved_name).unwrap_or_else(default_theme_preset);
        theme_state.write().set_preset(preset);
    });

    use_effect(move || {
        let mode = *org_overrides.mode.read();
        if theme_state.peek().mode != mode {
            theme_state.write().set_mode(mode);
        }
    });

    // Supervised: the established caller is watched for death
    // (`Caller::closed()` resolves when the org socket drops — server
    // restart, network blip) and the connection re-establishes under
    // exponential backoff with full jitter (floor 250ms, cap 10s). The
    // `Connection::generation` bump on each re-establish is what tells
    // hooks/caches downstream to invalidate; `caller_for`'s root cache
    // additionally self-validates via `is_connected()` (see
    // `vox_clients`), so the reconnect lands on a fresh socket.
    let _conn: architect::Connection<vox_core::Caller> = architect::use_app_supervised(
        move || {
            let slug = match &*org_selection.read() {
                OrgSelection::One(slug) => slug.clone(),
                OrgSelection::All => home_slug(&org_list.read()),
            };
            async move {
                if slug.is_empty() {
                    // Discovery hasn't resolved yet — stay Connecting;
                    // the org-list signal write re-runs this closure.
                    return Err("awaiting org discovery".to_owned());
                }
                crate::vox_clients::caller_for(&slug).await
            }
        },
        |caller: vox_core::Caller| async move { caller.closed().await },
    );

    // The per-feature optimistic stores (architect-atom `Store`s) every
    // route page's `use_<entity>_list` / `use_<entity>_mutations` hooks
    // read. Provided after `use_app_supervised` so mutations find the
    // notifications + reactivity registries it installed above.
    crate::stores::provide_stores();

    // Web auth: the active-account context + boot restore (validate
    // the persisted session, or auto sign-in as Guest). Needs the
    // org-list signal above (auth talks to the home org's endpoint);
    // must precede the router so pages/presence can read the account.
    crate::auth::provide_auth();

    // Org-wide presence: join the active org's `DocPresence` channel
    // over the shared connection above (the hook reads the
    // `Connection<vox::Caller>` context, so this must come AFTER
    // `use_app_reactive`) and provide the identity/status contexts the
    // publisher, roster, and picker consume. The publisher itself
    // lives in the shell (`PresencePublisher`) — it needs the router.
    crate::presence::provide_org_presence();

    rsx! {
        ThemeProvider { state: theme_state,
            div { class: "min-h-screen bg-background text-foreground",
                Router::<Route> {}
            }
        }
    }
}
