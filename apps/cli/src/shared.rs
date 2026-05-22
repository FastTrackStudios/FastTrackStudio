//! Minimal cross-cutting helpers for the vertical-slice CLI.
//!
//! Holds [`RemoteVoxConfig`] — endpoint URL resolution for the
//! `task doctor` probe. The previous `LiveSession` /
//! `ServerRegistry` machinery (synced local `CrdtDoc` over
//! `WorkspaceSync`, per-server token registry) was ripped along
//! with the Loro entity layer. The endpoint-resolution logic
//! stays so future commands that hit a remote vox surface (e.g.
//! `AuthService::sign_in`) don't have to re-derive URL shaping.

#[derive(Debug, Clone)]
pub(crate) struct RemoteVoxConfig {
    pub(crate) display_url: String,
}

impl RemoteVoxConfig {
    pub(crate) fn from_args(
        server: String,
        session_token: Option<String>,
        organization_id: Option<String>,
    ) -> eyre::Result<Self> {
        let base = normalize_vox_url(&server);
        let mut display_url = base;
        if let Some(_token) = session_token.as_deref().filter(|s| !s.is_empty()) {
            append_query_param(&mut display_url, "token", "<redacted>");
        }
        if let Some(org) = organization_id.as_deref().filter(|s| !s.is_empty()) {
            append_query_param(&mut display_url, "organization_id", org);
        }
        Ok(Self { display_url })
    }
}

fn normalize_vox_url(server: &str) -> String {
    let trimmed = server.trim().trim_end_matches('/');
    if trimmed.starts_with("ws://") || trimmed.starts_with("wss://") {
        trimmed.to_string()
    } else if let Some(rest) = trimmed.strip_prefix("https://") {
        format!("wss://{}/vox", rest.trim_end_matches("/vox"))
    } else if let Some(rest) = trimmed.strip_prefix("http://") {
        format!("ws://{}/vox", rest.trim_end_matches("/vox"))
    } else {
        format!("ws://{}/vox", trimmed.trim_end_matches("/vox"))
    }
}

fn append_query_param(url: &mut String, key: &str, value: &str) {
    let separator = if url.contains('?') { '&' } else { '?' };
    url.push(separator);
    url.push_str(key);
    url.push('=');
    url.push_str(&percent_encode_query_value(value));
}

fn percent_encode_query_value(value: &str) -> String {
    let mut out = String::new();
    for byte in value.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' => {
                out.push(byte as char);
            }
            _ => out.push_str(&format!("%{byte:02X}")),
        }
    }
    out
}
