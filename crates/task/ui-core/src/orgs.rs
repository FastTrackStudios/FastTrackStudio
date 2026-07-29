//! Multi-org selection model.
//!
//! The server hosts several orgs (`/.well-known/task-server.json`
//! lists them). The UI lets you view **all** of them at once (the
//! default) or scope to a single org. The selection is held in a
//! `Signal<OrgSelection>` context; data fetchers resolve it to a list
//! of slugs via [`selected_slugs`] and fan out per org.
//!
//! Every feature UI crate needs this to scope its own fetches, so it
//! lives here rather than in the shell. **Discovery** (the well-known
//! fetch, which is `window.fetch` on wasm and `reqwest` on native)
//! stays in the shell's `orgs` module.

/// App-root context: the last org-discovery error (a native `fetch_orgs`
/// failure), surfaced in the Servers UI so a stuck "org discovery hasn't
/// resolved yet" shows *why* — a connect timeout, TLS error, 404, etc. —
/// instead of the app silently sitting on an empty org list. `None` = no
/// error / discovery succeeded.
#[derive(Clone, Copy)]
pub struct DiscoveryError(pub dioxus::prelude::Signal<Option<String>>);

/// One hosted org, as surfaced by the server's well-known endpoint.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct OrgMeta {
    pub slug: String,
    pub name: String,
    pub is_home: bool,
    /// Org's stable UUID (`org.toml` `id`). `None` for older servers
    /// that don't surface it. Needed by org-scoped services like the
    /// timer that key on `org_id`.
    pub id: Option<uuid::Uuid>,
    /// Plugin ids this org has turned off (`org.toml`
    /// `disabled_plugins`, relayed through the well-known doc). Empty
    /// for older servers — everything on. Resolve through
    /// [`plugin_set_for`] / [`active_plugin_set`] rather than reading
    /// raw (resolution handles unknown ids + always-on core).
    pub disabled_plugins: Vec<String>,
}

/// The effective plugin set of the org with `slug` (unknown slug or a
/// pre-plugin server = everything on; core is always on either way).
#[must_use]
pub fn plugin_set_for(orgs: &[OrgMeta], slug: &str) -> task_plugin::PluginSet {
    let disabled = orgs
        .iter()
        .find(|o| o.slug == slug)
        .map(|o| task_plugin::PluginChoice::Disabled(o.disabled_plugins.clone()));
    task_plugin::PluginSet::resolve(disabled.as_ref())
}

/// The plugin set the shell gates nav / widgets / routes on: the
/// ACTIVE org's (see [`active_slug`] — the selected org, or home under
/// "All"). Before discovery resolves this is "everything on", so
/// nothing flickers off while the org list loads.
#[must_use]
pub fn active_plugin_set(sel: &OrgSelection, orgs: &[OrgMeta]) -> task_plugin::PluginSet {
    plugin_set_for(orgs, &active_slug(sel, orgs))
}

/// What the org switcher is pointed at. `All` aggregates every hosted
/// org; `One` scopes to a single slug. Defaults to `All`.
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub enum OrgSelection {
    #[default]
    All,
    One(String),
}

impl OrgSelection {
    /// Display label for the switcher trigger.
    #[must_use]
    pub fn label(&self, orgs: &[OrgMeta]) -> String {
        match self {
            Self::All => "All organizations".to_string(),
            Self::One(slug) => orgs
                .iter()
                .find(|o| &o.slug == slug)
                .map_or_else(|| slug.clone(), |o| o.name.clone()),
        }
    }
}

/// Resolve a selection to the concrete slugs to fetch from. `All`
/// expands to every hosted org (home org first for stable ordering);
/// `One` is just that slug.
#[must_use]
pub fn selected_slugs(sel: &OrgSelection, orgs: &[OrgMeta]) -> Vec<String> {
    match sel {
        OrgSelection::One(slug) => vec![slug.clone()],
        OrgSelection::All => {
            let mut slugs: Vec<String> = orgs.iter().map(|o| o.slug.clone()).collect();
            slugs.sort_by_key(|s| orgs.iter().find(|o| &o.slug == s).map(|o| !o.is_home));
            slugs
        }
    }
}

/// Where a newly-created record should land: the selected org, or the
/// home org (falling back to the first hosted org) when viewing All.
#[must_use]
pub fn create_target(sel: &OrgSelection, orgs: &[OrgMeta]) -> String {
    match sel {
        OrgSelection::One(slug) => slug.clone(),
        OrgSelection::All => orgs
            .iter()
            .find(|o| o.is_home)
            .or_else(|| orgs.first())
            .map(|o| o.slug.clone())
            .unwrap_or_default(),
    }
}

/// The home org's slug (falls back to the first hosted org, then to
/// an empty string before discovery resolves).
#[must_use]
pub fn home_slug(orgs: &[OrgMeta]) -> String {
    orgs.iter()
        .find(|o| o.is_home)
        .or_else(|| orgs.first())
        .map(|o| o.slug.clone())
        .unwrap_or_default()
}

/// The single org that org-scoped surfaces (the vault page, the note
/// palette/omni-picker) should read from. When the switcher is scoped to
/// `One`, that org; otherwise the home org. `All` has no single vault to
/// show, so it falls back to home — pick a specific org in the switcher
/// to browse or search its vault. This is what lets the vault/palette
/// follow the org switcher instead of being pinned to the home org.
#[must_use]
pub fn active_slug(sel: &OrgSelection, orgs: &[OrgMeta]) -> String {
    match sel {
        OrgSelection::One(slug) => slug.clone(),
        OrgSelection::All => home_slug(orgs),
    }
}

/// HTTP(S) base derived from the configured vox WebSocket URL —
/// `ws://host/…` → `http://host`, `wss://` → `https://`.
#[must_use]
pub fn http_base() -> String {
    let v = crate::vox_session::vox_url();
    let v = v.trim_end_matches("/vox").trim_end_matches('/');
    if let Some(rest) = v.strip_prefix("wss://") {
        format!("https://{rest}")
    } else if let Some(rest) = v.strip_prefix("ws://") {
        format!("http://{rest}")
    } else {
        v.to_string()
    }
}
