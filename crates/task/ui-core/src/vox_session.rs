//! Vox endpoint configuration.
//!
//! The connection machinery itself lives in [`crate::vox_clients`]
//! (one cached connection root per org, typed clients as views over
//! its caller) and the app root's `Connection<Caller>`
//! (`architect::use_app_reactive`). This module only answers "what
//! base URL do we dial" — compile-time on wasm, env on native.

/// Compile-time default for the wasm build (overridden by
/// `TASK_VOX_URL_WEB` at build time). Matches the dev server's bind.
pub const DEFAULT_VOX_URL: &str = "ws://127.0.0.1:18080/vox";

use std::sync::{LazyLock, RwLock};

/// The user-selected active server (from the multi-server registry).
/// When set, it overrides the env/same-origin default in [`vox_url`].
/// Held in a process-global so the plain (non-component) `vox_url` +
/// establish paths can read it; the app root keeps it in sync with the
/// active the app's server registry via [`set_active_server`].
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ActiveServer {
    /// vox base URL — `ws(s)://host[:port]` (a trailing `/vox` is fine;
    /// [`crate::vox_clients`] normalizes it).
    pub url: String,
    /// The session token issued when signing into this server, if any.
    pub token: Option<String>,
}

static ACTIVE: LazyLock<RwLock<Option<ActiveServer>>> = LazyLock::new(|| RwLock::new(None));

/// Set (or clear) the active server. Called from the app root whenever
/// the active registry entry changes.
pub fn set_active_server(server: Option<ActiveServer>) {
    if let Ok(mut w) = ACTIVE.write() {
        *w = server.filter(|s| !s.url.trim().is_empty());
    }
}

/// The active server, if one is selected.
#[must_use]
pub fn active_server() -> Option<ActiveServer> {
    ACTIVE.read().ok().and_then(|r| r.clone())
}

/// The configured vox base URL.
///
/// Wasm resolution order:
/// 1. `TASK_VOX_URL_WEB` baked at build time (dev harnesses, the
///    multiplayer suite — explicit always wins);
/// 2. **same-origin at runtime** — `ws(s)://<location.host>/vox` —
///    when the page is served from a real host. This is what makes
///    one deployed image work behind any hostname: the deployment's
///    ingress routes `/vox` + `/org` + `/.well-known` to the server
///    on the same origin as the static bundle;
/// 3. [`DEFAULT_VOX_URL`] when the page itself is on
///    localhost/127.0.0.1 (a bare `dx serve` without the env —
///    same-origin would dial the static server's own port).
///
/// Native: `TASK_VOX_URL` at runtime, empty when unset.
#[must_use]
pub fn vox_url() -> String {
    // A user-selected server (multi-server registry) always wins.
    if let Some(server) = active_server() {
        return server.url;
    }
    #[cfg(target_arch = "wasm32")]
    {
        if let Some(baked) = option_env!("TASK_VOX_URL_WEB") {
            return baked.to_string();
        }
        if let Some(url) = same_origin_vox_url() {
            return url;
        }
        DEFAULT_VOX_URL.to_string()
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        std::env::var("TASK_VOX_URL").unwrap_or_default()
    }
}

/// Derive `ws(s)://<host>/vox` from the page's own origin. `None` only
/// when there's no window (tests, workers) or a non-http(s) origin.
///
/// The FastTrackStudio / single-binary deploy serves the app AND vox from
/// one origin (the server, or an ingress routing `/vox`+`/org`+`/.well-known`
/// to it), so the page's own host is always the vox host — including on
/// localhost and LAN/Tailscale hosts. The separate-static-server dev flow
/// bakes `TASK_VOX_URL_WEB` (checked before this in `vox_url`), so it is
/// unaffected.
#[cfg(target_arch = "wasm32")]
fn same_origin_vox_url() -> Option<String> {
    let location = web_sys::window()?.location();
    let host = location.host().ok()?; // host[:port]
    let protocol = location.protocol().ok()?;
    if protocol != "http:" && protocol != "https:" {
        return None;
    }
    let scheme = match protocol.as_str() {
        "https:" => "wss",
        _ => "ws",
    };
    Some(format!("{scheme}://{host}/vox"))
}
