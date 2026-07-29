//! Org **discovery** — the `/.well-known/task-server.json` fetch.
//!
//! The selection model itself ([`OrgMeta`], [`OrgSelection`],
//! [`selected_slugs`] and friends) lives in [`task_ui_core::orgs`] so
//! feature UI crates can scope their own fetches without depending on
//! this shell; it is re-exported here, so every `crate::orgs::…` path
//! still resolves.
//!
//! Discovery stays here because it is the one platform-specific piece:
//! `window.fetch` on wasm, `reqwest` (rustls — works in the iOS
//! sandbox) on native, with a Sentry breadcrumb on failure.

pub use task_ui_core::orgs::*;

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
    /// Plugin deny-list from the org's manifest. Absent on servers
    /// predating the plugin toggle — everything on.
    #[serde(default)]
    disabled_plugins: Vec<String>,
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
            disabled_plugins: o.disabled_plugins,
        })
        .collect())
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
    // Explicit connect + total timeouts: on device a stalled DNS/TLS/connect
    // would otherwise leave the resource pending forever ("org discovery
    // hasn't resolved yet" with no way to tell why). Fail fast + loud instead.
    let client = reqwest::Client::builder()
        .connect_timeout(std::time::Duration::from_secs(8))
        .timeout(std::time::Duration::from_secs(20))
        .user_agent("task-mobile")
        .build()
        .map_err(|e| format!("http client: {e}"))?;
    let result = async {
        let body = client
            .get(&url)
            .send()
            .await
            .map_err(|e| format!("fetch orgs `{url}`: {e}"))?
            .error_for_status()
            .map_err(|e| format!("fetch orgs `{url}`: {e}"))?
            .text()
            .await
            .map_err(|e| format!("orgs body `{url}`: {e}"))?;
        parse_orgs(&body)
    }
    .await;
    match &result {
        Ok(orgs) => tracing::info!(url, count = orgs.len(), "org discovery ok"),
        Err(e) => {
            tracing::warn!(url, error = %e, "org discovery failed");
            // Belt-and-suspenders: capture directly so this failure
            // reaches Sentry even if the client's tracing subscriber was
            // superseded by dioxus's own subscriber init.
            #[cfg(not(target_arch = "wasm32"))]
            sentry::capture_message(
                &format!("org discovery failed: {e}"),
                sentry::Level::Warning,
            );
        }
    }
    result
}
