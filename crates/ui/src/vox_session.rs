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

/// The configured vox base URL. Wasm: baked at build time
/// (`TASK_VOX_URL_WEB`, falling back to [`DEFAULT_VOX_URL`]). Native:
/// `TASK_VOX_URL` at runtime, empty when unset.
#[must_use]
pub fn vox_url() -> String {
    #[cfg(target_arch = "wasm32")]
    {
        option_env!("TASK_VOX_URL_WEB")
            .unwrap_or(DEFAULT_VOX_URL)
            .to_string()
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        std::env::var("TASK_VOX_URL").unwrap_or_default()
    }
}
