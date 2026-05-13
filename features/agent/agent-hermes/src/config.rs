//! Configuration for the Hermes integration plugin.
//!
//! `HermesConfig` is constructed from the server's `IntegrationSettings`
//! row (the sealed-at-rest config table). The server decrypts the
//! token before passing it in here — this struct holds plaintext and
//! must never be serialized to disk or wire.

use url::Url;

/// Runtime config for [`crate::HermesIntegration`].
///
/// Constructed from the server's `IntegrationSettings` after
/// decrypting the sealed session token. Held only in-memory; never
/// serialised, never logged.
#[derive(Clone, Debug)]
pub struct HermesConfig {
    /// Dashboard base, e.g. `https://hermes.starcommand.live` or
    /// `http://localhost:9119` (recommended via SSH tunnel — Hermes'
    /// dashboard enforces an `Invalid Host header` trusted-hosts check
    /// against externally-fronted hostnames).
    pub base_url: Url,

    /// Dashboard session token (constant-time HMAC verified server-side).
    /// Fetched once from Hermes' UI; stored sealed at rest, decrypted
    /// only in this struct.
    pub session_token: String,

    /// Optional default board slug. When set we pass `?board=…` to
    /// `/dispatch` and on the WS subscription so we only see events
    /// for that board.
    pub default_board: Option<String>,

    /// Default profile / assignee for dispatched tasks (e.g.
    /// "engineer"). Used when [`agent_proto::integration::DispatchRequest::agent_kind`]
    /// doesn't carry a `hermes-profile:` prefix.
    pub default_profile: String,

    /// Shared secret for inbound webhooks (the HMAC key Hermes uses to
    /// sign POST bodies it sends to `/webhooks/hermes/…`). Optional;
    /// when `None` the webhook route accepts only the WS event loop.
    pub webhook_secret: Option<String>,
}

impl Default for HermesConfig {
    fn default() -> Self {
        // Localhost via SSH tunnel is the recommended deployment per
        // the Hermes integration spec.
        Self {
            base_url: Url::parse("http://localhost:9119").expect("static URL"),
            session_token: String::new(),
            default_board: None,
            default_profile: "engineer".to_string(),
            webhook_secret: None,
        }
    }
}

impl HermesConfig {
    /// Build the WebSocket URL for the event stream.
    ///
    /// Replaces `http`/`https` with `ws`/`wss` and joins
    /// `/api/plugins/kanban/events`, then appends the auth token plus
    /// optional `since` cursor and `board` filter.
    pub fn ws_url(&self, since: Option<i64>) -> Url {
        let mut url = self.base_url.clone();
        // Replace scheme http→ws / https→wss. Url::set_scheme refuses
        // some transitions silently, so do it via string surgery on
        // the serialised form for robustness.
        let current_scheme = url.scheme().to_string();
        let new_scheme = match current_scheme.as_str() {
            "http" => "ws",
            "https" => "wss",
            _ => current_scheme.as_str(), // ws://, wss:// — leave alone
        };
        if current_scheme != new_scheme {
            // The Url crate refuses set_scheme for non-special<->special
            // transitions, but http<->ws and https<->wss are allowed.
            let _ = url.set_scheme(new_scheme);
        }
        url.set_path("/api/plugins/kanban/events");
        {
            let mut q = url.query_pairs_mut();
            q.append_pair("token", &self.session_token);
            if let Some(s) = since {
                q.append_pair("since", &s.to_string());
            }
            if let Some(board) = &self.default_board {
                q.append_pair("board", board);
            }
        }
        url
    }

    /// Join `base_url` with a path that must begin with `/`.
    ///
    /// Panics in debug builds if `path` doesn't start with `/`;
    /// returns a best-effort joined URL in release builds.
    pub fn api_url(&self, path: &str) -> Url {
        debug_assert!(path.starts_with('/'), "api_url path must start with /");
        let mut url = self.base_url.clone();
        url.set_path(path);
        url
    }
}
