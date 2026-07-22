//! Share links — the dev-preview slice of `plans/collaboration-sharing.md`.
//!
//! [`ShareStore`] persists an org's links in `<org>/shares.json` (a flat,
//! human-inspectable file — the sqlite move comes with grants/audiences);
//! [`ShareServiceImpl`] serves the link CRUD on the ORG lane; the HTTP
//! landing route (`GET /org/{slug}/share/{token}`, wired in `lib.rs`)
//! resolves a token — 404 unknown, 410 disabled — and hands the visitor a
//! minimal page that opens the shared note in the app.
//!
//! Every setting is retroactive by construction: the landing route reads
//! the store on every hit, so disabling a link kills it immediately.

use std::path::PathBuf;
use std::sync::Mutex;

use share_proto::{ShareError, ShareLinkInfo, ShareService};

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct StoredLink {
    pub token: String,
    pub label: String,
    pub note_path: String,
    pub capability: String,
    #[serde(default)]
    pub disabled: bool,
    pub created_at: String,
}

/// JSON-file-backed link store, one per org.
pub struct ShareStore {
    path: PathBuf,
    links: Mutex<Vec<StoredLink>>,
}

impl ShareStore {
    /// Open (or start empty at) `<org>/shares.json`.
    pub fn open(org_dir: &std::path::Path) -> Self {
        let path = org_dir.join("shares.json");
        let links = std::fs::read_to_string(&path)
            .ok()
            .and_then(|s| serde_json::from_str(&s).ok())
            .unwrap_or_default();
        Self {
            path,
            links: Mutex::new(links),
        }
    }

    fn save(&self, links: &[StoredLink]) -> Result<(), ShareError> {
        let json = serde_json::to_string_pretty(links)
            .map_err(|e| ShareError::Storage(e.to_string()))?;
        std::fs::write(&self.path, json).map_err(|e| ShareError::Storage(e.to_string()))
    }

    pub fn resolve(&self, token: &str) -> Option<StoredLink> {
        self.links
            .lock()
            .expect("share store poisoned")
            .iter()
            .find(|l| l.token == token)
            .cloned()
    }

    fn with_links<R>(&self, f: impl FnOnce(&mut Vec<StoredLink>) -> R) -> (R, Vec<StoredLink>) {
        let mut guard = self.links.lock().expect("share store poisoned");
        let r = f(&mut guard);
        (r, guard.clone())
    }
}

/// The org-lane share service.
#[derive(Clone)]
pub struct ShareServiceImpl {
    store: std::sync::Arc<ShareStore>,
    slug: String,
    /// Public base URL links are composed against
    /// (`TASK_SHARE_PUBLIC_BASE`, default derived from the bind address).
    public_base: String,
}

impl ShareServiceImpl {
    pub fn new(store: std::sync::Arc<ShareStore>, slug: String, public_base: String) -> Self {
        Self {
            store,
            slug,
            public_base,
        }
    }

    fn info(&self, l: &StoredLink) -> ShareLinkInfo {
        ShareLinkInfo {
            token: l.token.clone(),
            label: l.label.clone(),
            note_path: l.note_path.clone(),
            capability: l.capability.clone(),
            disabled: l.disabled,
            url: format!(
                "{}/org/{}/share/{}",
                self.public_base.trim_end_matches('/'),
                self.slug,
                l.token
            ),
            created_at: l.created_at.clone(),
        }
    }
}

impl ShareService for ShareServiceImpl {
    async fn create_link(
        &self,
        note_path: String,
        label: String,
        capability: String,
    ) -> Result<ShareLinkInfo, ShareError> {
        if note_path.is_empty() {
            return Err(ShareError::Invalid("empty note path".into()));
        }
        let capability = match capability.as_str() {
            "view" | "comment" => capability,
            other => {
                return Err(ShareError::Invalid(format!(
                    "capability must be view|comment, got {other}"
                )));
            }
        };
        // 32 hex chars of UUIDv4 randomness — unguessable, URL-safe.
        let token = uuid::Uuid::new_v4().simple().to_string();
        let link = StoredLink {
            token,
            label: if label.is_empty() {
                "share link".into()
            } else {
                label
            },
            note_path,
            capability,
            disabled: false,
            created_at: chrono::Utc::now().to_rfc3339(),
        };
        let (info, links) = self.store.with_links(|links| {
            links.insert(0, link.clone());
            self.info(&link)
        });
        self.store.save(&links)?;
        Ok(info)
    }

    async fn list_links(&self) -> Result<Vec<ShareLinkInfo>, ShareError> {
        let (out, _) = self
            .store
            .with_links(|links| links.iter().map(|l| self.info(l)).collect());
        Ok(out)
    }

    async fn links_for_note(&self, note_path: String) -> Result<Vec<ShareLinkInfo>, ShareError> {
        let (out, _) = self.store.with_links(|links| {
            links
                .iter()
                .filter(|l| l.note_path == note_path)
                .map(|l| self.info(l))
                .collect()
        });
        Ok(out)
    }

    async fn set_link_disabled(&self, token: String, disabled: bool) -> Result<(), ShareError> {
        let (found, links) = self.store.with_links(|links| {
            match links.iter_mut().find(|l| l.token == token) {
                Some(l) => {
                    l.disabled = disabled;
                    true
                }
                None => false,
            }
        });
        if !found {
            return Err(ShareError::NotFound);
        }
        self.store.save(&links)
    }

    async fn delete_link(&self, token: String) -> Result<(), ShareError> {
        let (found, links) = self.store.with_links(|links| {
            let before = links.len();
            links.retain(|l| l.token != token);
            links.len() != before
        });
        if !found {
            return Err(ShareError::NotFound);
        }
        self.store.save(&links)
    }
}

/// The share landing page: token-checked on EVERY hit (revocation is
/// immediate), then a minimal page that opens the shared note in the app.
/// `app_origin` = where the web app lives (`TASK_SHARE_APP_ORIGIN`; empty =
/// same origin, the embedded-app deployment).
pub fn landing_html(link: &StoredLink, app_origin: &str) -> String {
    let note = &link.note_path;
    let label = &link.label;
    let cap = &link.capability;
    let open = format!(
        "{}/vault?path={}",
        app_origin.trim_end_matches('/'),
        urlencoding_encode(note)
    );
    format!(
        r#"<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="robots" content="noindex">
<title>{label}</title>
<style>
 body{{font-family:system-ui,sans-serif;background:#0b0d10;color:#e6e8eb;display:flex;min-height:100vh;align-items:center;justify-content:center;margin:0}}
 .card{{background:#14171c;border:1px solid #262b33;border-radius:12px;padding:2.5rem;max-width:26rem;text-align:center}}
 h1{{font-size:1.15rem;margin:0 0 .4rem}}
 p{{color:#9aa3af;font-size:.9rem;margin:.25rem 0 1.4rem}}
 a.btn{{display:inline-block;background:#6d5ef2;color:#fff;text-decoration:none;border-radius:8px;padding:.65rem 1.4rem;font-weight:600}}
 .cap{{display:inline-block;border:1px solid #333a45;border-radius:99px;padding:.1rem .6rem;font-size:.72rem;color:#9aa3af;margin-bottom:1rem;text-transform:uppercase;letter-spacing:.08em}}
</style></head><body><div class="card">
<h1>{label}</h1>
<div class="cap">{cap} access</div>
<p>You've been invited to <strong>{note}</strong>.</p>
<a class="btn" href="{open}">Open</a>
</div></body></html>"#,
    )
}

/// Gone page for a disabled link.
pub fn disabled_html() -> &'static str {
    r#"<!doctype html><html><head><meta charset="utf-8"><title>Link disabled</title>
<style>body{font-family:system-ui,sans-serif;background:#0b0d10;color:#9aa3af;display:flex;min-height:100vh;align-items:center;justify-content:center}</style>
</head><body><p>This share link has been disabled by its owner.</p></body></html>"#
}

/// Percent-encode a path for a query value (kept tiny to avoid a dep).
fn urlencoding_encode(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for b in s.bytes() {
        match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' | b'/' => {
                out.push(b as char)
            }
            b' ' => out.push_str("%20"),
            _ => out.push_str(&format!("%{b:02X}")),
        }
    }
    out
}
