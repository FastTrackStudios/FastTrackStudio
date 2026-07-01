//! Per-account UI preferences — localStorage stopgap.
//!
//! Keys are `task.prefs.<email>.<name>` so each account keeps its own
//! defaults (switching accounts switches your setup, not just your
//! avatar). FUTURE(plans/relevancy-and-inbox.md): replace with a
//! per-user prefs entity on the server so the CLI and every device
//! share the same personalization.

// Used by the wasm storage fns + the key-shape test — same cfg dance
// as `auth::token_key`.
#[cfg(any(target_arch = "wasm32", test))]
fn key(email: &str, name: &str) -> String {
    format!("task.prefs.{email}.{name}")
}

#[cfg(target_arch = "wasm32")]
fn storage() -> Option<web_sys::Storage> {
    web_sys::window().and_then(|w| w.local_storage().ok().flatten())
}

/// Read a boolean pref for `email`, falling back to `default` when
/// unset (or on native, which has no store yet).
#[must_use]
pub fn load_bool(email: &str, name: &str, default: bool) -> bool {
    #[cfg(target_arch = "wasm32")]
    {
        if let Some(v) = storage().and_then(|s| s.get_item(&key(email, name)).ok().flatten()) {
            return v == "true";
        }
    }
    #[cfg(not(target_arch = "wasm32"))]
    let _ = (email, name);
    default
}

pub fn save_bool(email: &str, name: &str, value: bool) {
    #[cfg(target_arch = "wasm32")]
    if let Some(s) = storage() {
        let _ = s.set_item(&key(email, name), if value { "true" } else { "false" });
    }
    #[cfg(not(target_arch = "wasm32"))]
    let _ = (email, name, value);
}

#[cfg(test)]
mod tests {
    use super::key;

    #[test]
    fn keys_are_account_scoped() {
        assert_eq!(
            key("cody@fasttrackstudios.com", "tasks.relevant"),
            "task.prefs.cody@fasttrackstudios.com.tasks.relevant"
        );
    }
}
