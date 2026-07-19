//! Multi-org selection model + discovery.
//!
//! The server hosts several orgs (`/.well-known/task-server.json`
//! lists them). The UI lets you view **all** of them at once (the
//! default) or scope to a single org. The selection is held in a
//! `Signal<OrgSelection>` context; data fetchers resolve it to a list
//! of slugs via [`selected_slugs`] and fan out per org.

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

// ── discovery ───────────────────────────────────────────────────────

#[derive(serde::Deserialize)]
struct WellKnown {
    orgs: Vec<RawOrg>,
}

#[derive(serde::Deserialize)]
struct RawOrg {
    slug: String,
    display_name: String,
    is_home: bool,
    #[serde(default)]
    id: Option<uuid::Uuid>,
}

fn parse_orgs(body: &str) -> Result<Vec<OrgMeta>, String> {
    let wk: WellKnown = serde_json::from_str(body).map_err(|e| format!("parse well-known: {e}"))?;
    Ok(wk
        .orgs
        .into_iter()
        .map(|o| OrgMeta {
            slug: o.slug,
            name: o.display_name,
            is_home: o.is_home,
            id: o.id,
        })
        .collect())
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

/// Fetch the hosted org list from `/.well-known/task-server.json`.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_orgs() -> Result<Vec<OrgMeta>, String> {
    use wasm_bindgen::JsCast;
    use wasm_bindgen_futures::JsFuture;

    let base = http_base();
    if base.is_empty() {
        return Err("no server URL configured".to_owned());
    }
    let url = format!("{base}/.well-known/task-server.json");
    let win = web_sys::window().ok_or("no window")?;
    let resp_val = JsFuture::from(win.fetch_with_str(&url))
        .await
        .map_err(|e| format!("fetch orgs: {e:?}"))?;
    let resp: web_sys::Response = resp_val
        .dyn_into()
        .map_err(|_| "fetch returned a non-Response".to_owned())?;
    let text_promise = resp.text().map_err(|e| format!("orgs body: {e:?}"))?;
    let text_val = JsFuture::from(text_promise)
        .await
        .map_err(|e| format!("orgs body await: {e:?}"))?;
    let text = text_val.as_string().ok_or("orgs body not a string")?;
    parse_orgs(&text)
}

/// Fetch the hosted org list from `/.well-known/task-server.json`.
///
/// Native (desktop/mobile, incl. iOS) has no `window.fetch`; use
/// `reqwest` (rustls — works in the iOS sandbox) over the same
/// [`http_base`]-derived URL. This is what makes an installed app with
/// no `TASK_VOX_URL` env connect: the user-selected server drives
/// [`http_base`], discovery resolves the org slug, and the vox dial can
/// proceed (`vox_clients::org_ws_url` needs a real slug).
#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_orgs() -> Result<Vec<OrgMeta>, String> {
    let base = http_base();
    if base.is_empty() {
        return Err("no server URL configured".to_owned());
    }
    let url = format!("{base}/.well-known/task-server.json");
    let body = reqwest::get(&url)
        .await
        .map_err(|e| format!("fetch orgs `{url}`: {e}"))?
        .error_for_status()
        .map_err(|e| format!("fetch orgs `{url}`: {e}"))?
        .text()
        .await
        .map_err(|e| format!("orgs body `{url}`: {e}"))?;
    parse_orgs(&body)
}
