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

/// Derive `ws(s)://<host>/vox` from the page's own origin. `None` on
/// localhost (dev — the static server isn't the vox server) or when
/// there's no window (tests, workers).
#[cfg(target_arch = "wasm32")]
fn same_origin_vox_url() -> Option<String> {
    let location = web_sys::window()?.location();
    let host = location.host().ok()?; // host[:port]
    let hostname = location.hostname().ok()?;
    if hostname == "localhost" || hostname == "127.0.0.1" || hostname == "[::1]" {
        return None;
    }
    let scheme = match location.protocol().ok()?.as_str() {
        "https:" => "wss",
        _ => "ws",
    };
    Some(format!("{scheme}://{host}/vox"))
}
