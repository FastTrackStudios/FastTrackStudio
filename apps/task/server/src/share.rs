//! Share links — notes, Root slices, and Named Versions (issue #271).
//!
//! [`ShareStore`] persists an org's links in `<org>/shares.json` (a flat,
//! human-inspectable file — the sqlite move comes with grants/audiences)
//! and the per-link access log in `<org>/shares-access.jsonl`;
//! [`ShareServiceImpl`] serves link CRUD on the ORG lane; the HTTP
//! routes here (`GET /org/{slug}/share/{token}[/…]`, wired in `lib.rs`)
//! resolve a token on EVERY hit — 404 unknown, 410 disabled/expired,
//! password re-checked — so every setting is retroactive by
//! construction.
//!
//! ## The Files targets (issue #271)
//!
//! A **Slice** link serves exactly its `(root, subpath)` subtree: the
//! link's URLs carry paths *relative to the slice*, so nothing outside
//! it is even addressable (AC 1). A **Named Version** link resolves the
//! curated entity to its exact change and serves *that* commit's tree
//! (AC 2), whatever the live tree looks like today. View-only links
//! stream proxy renditions and never original bytes; the `download`
//! capability gates originals and every download writes a receipt into
//! the access log (AC 3/4).

use std::path::PathBuf;
use std::sync::Mutex;

use axum::extract::{Path as AxPath, Query, State};
use axum::http::{StatusCode, header};
use axum::response::{Html, IntoResponse, Response};
use files::FilesService as _;
use sha2::{Digest, Sha256};
use share_proto::{
    NewShareLink, ShareAccess, ShareCapabilities, ShareError, ShareLinkInfo, ShareService,
    ShareTarget,
};

use crate::AppState;

// ── storage ─────────────────────────────────────────────────────────

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct StoredLink {
    pub token: String,
    pub label: String,
    /// `None` only on rows written before issue #271 — those were all
    /// note links; [`StoredLink::target`] reconstructs them.
    #[serde(default)]
    pub target: Option<ShareTarget>,
    /// Legacy pre-#271 fields, kept so an existing `shares.json` loads.
    #[serde(default)]
    pub note_path: String,
    #[serde(default)]
    pub capability: String,
    #[serde(default)]
    pub capabilities: Option<ShareCapabilities>,
    /// SHA-256 hex of the link password; `None` = open.
    #[serde(default)]
    pub password_sha256: Option<String>,
    /// Unix seconds after which the link stops resolving; 0 = never.
    #[serde(default)]
    pub expires_unix: i64,
    #[serde(default)]
    pub disabled: bool,
    pub created_at: String,
}

impl StoredLink {
    pub fn target(&self) -> ShareTarget {
        self.target.clone().unwrap_or_else(|| ShareTarget::Note {
            path: self.note_path.clone(),
        })
    }

    pub fn capabilities(&self) -> ShareCapabilities {
        self.capabilities.unwrap_or(ShareCapabilities {
            comment: self.capability == "comment",
            download: false,
        })
    }

    pub fn expired(&self, now_unix: i64) -> bool {
        self.expires_unix > 0 && now_unix > self.expires_unix
    }

    /// Check a presented password. An unprotected link accepts anything;
    /// a protected one requires the exact hash match.
    pub fn password_ok(&self, presented: Option<&str>) -> bool {
        match &self.password_sha256 {
            None => true,
            Some(hash) => presented.is_some_and(|pw| sha256_hex(pw) == *hash),
        }
    }
}

fn sha256_hex(s: &str) -> String {
    let mut h = Sha256::new();
    h.update(s.as_bytes());
    let out = h.finalize();
    let mut hex = String::with_capacity(64);
    for b in out {
        hex.push_str(&format!("{b:02x}"));
    }
    hex
}

/// On-disk shape of `shares.json`. Loads the pre-#271 shape (a bare
/// array of links) transparently.
#[derive(Default, serde::Serialize, serde::Deserialize)]
struct StoreFile {
    #[serde(default)]
    links: Vec<StoredLink>,
    /// The org kill switch: while on, no new link mints.
    #[serde(default)]
    sharing_disabled: bool,
}

/// JSON-file-backed link store, one per org, plus the append-only
/// access log beside it.
pub struct ShareStore {
    path: PathBuf,
    log_path: PathBuf,
    state: Mutex<StoreFile>,
}

impl ShareStore {
    /// Open (or start empty at) `<org>/shares.json`.
    pub fn open(org_dir: &std::path::Path) -> Self {
        let path = org_dir.join("shares.json");
        let log_path = org_dir.join("shares-access.jsonl");
        let state = std::fs::read_to_string(&path)
            .ok()
            .and_then(|s| {
                serde_json::from_str::<StoreFile>(&s).ok().or_else(|| {
                    serde_json::from_str::<Vec<StoredLink>>(&s)
                        .ok()
                        .map(|links| StoreFile {
                            links,
                            sharing_disabled: false,
                        })
                })
            })
            .unwrap_or_default();
        Self {
            path,
            log_path,
            state: Mutex::new(state),
        }
    }

    fn save(&self, file: &StoreFile) -> Result<(), ShareError> {
        let json =
            serde_json::to_string_pretty(file).map_err(|e| ShareError::Storage(e.to_string()))?;
        std::fs::write(&self.path, json).map_err(|e| ShareError::Storage(e.to_string()))
    }

    pub fn resolve(&self, token: &str) -> Option<StoredLink> {
        self.state
            .lock()
            .expect("share store poisoned")
            .links
            .iter()
            .find(|l| l.token == token)
            .cloned()
    }

    pub fn sharing_disabled(&self) -> bool {
        self.state
            .lock()
            .expect("share store poisoned")
            .sharing_disabled
    }

    fn with_state<R>(&self, f: impl FnOnce(&mut StoreFile) -> R) -> (R, StoreFile) {
        let mut guard = self.state.lock().expect("share store poisoned");
        let r = f(&mut guard);
        let snapshot = StoreFile {
            links: guard.links.clone(),
            sharing_disabled: guard.sharing_disabled,
        };
        (r, snapshot)
    }

    /// Append one access row — landing views, browses, rendition
    /// streams, and download receipts (issue #271 AC 4). Best-effort:
    /// a failed log line must not fail the serve.
    pub fn log_access(&self, token: &str, kind: &str, path: &str) {
        #[derive(serde::Serialize)]
        struct Row<'a> {
            at: String,
            token: &'a str,
            kind: &'a str,
            path: &'a str,
        }
        let row = Row {
            at: chrono::Utc::now().to_rfc3339(),
            token,
            kind,
            path,
        };
        let Ok(line) = serde_json::to_string(&row) else {
            return;
        };
        use std::io::Write as _;
        let _ = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.log_path)
            .and_then(|mut f| writeln!(f, "{line}"));
    }

    /// One link's access rows, newest first (capped — the log is
    /// append-only and unbounded on disk).
    pub fn read_access(&self, token: &str) -> Vec<ShareAccess> {
        #[derive(serde::Deserialize)]
        struct Row {
            at: String,
            token: String,
            kind: String,
            #[serde(default)]
            path: String,
        }
        let Ok(body) = std::fs::read_to_string(&self.log_path) else {
            return Vec::new();
        };
        let mut out: Vec<ShareAccess> = body
            .lines()
            .filter_map(|l| serde_json::from_str::<Row>(l).ok())
            .filter(|r| r.token == token)
            .map(|r| ShareAccess {
                at: r.at,
                kind: r.kind,
                path: r.path,
            })
            .collect();
        out.reverse();
        out.truncate(500);
        out
    }
}

// ── the org-lane service ────────────────────────────────────────────

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
            target: l.target(),
            capabilities: l.capabilities(),
            password_protected: l.password_sha256.is_some(),
            expires_unix: l.expires_unix,
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

fn validate_target(target: &ShareTarget) -> Result<(), ShareError> {
    match target {
        ShareTarget::Note { path } if path.is_empty() => {
            Err(ShareError::Invalid("empty note path".into()))
        }
        ShareTarget::Slice { subpath, .. }
            if subpath.starts_with('/') || subpath.split('/').any(|s| s == "..") =>
        {
            Err(ShareError::Invalid(format!("bad slice subpath: {subpath}")))
        }
        _ => Ok(()),
    }
}

/// Apply mint/edit options onto a link. `existing` distinguishes the
/// update contract (None keeps, sentinel clears) from a fresh mint.
fn apply_options(link: &mut StoredLink, options: NewShareLink, fresh: bool) {
    if !options.label.is_empty() || fresh {
        link.label = if options.label.is_empty() {
            "share link".into()
        } else {
            options.label
        };
    }
    link.capabilities = Some(options.capabilities);
    match options.password.as_deref() {
        Some("") => link.password_sha256 = None,
        Some(pw) => link.password_sha256 = Some(sha256_hex(pw)),
        None if fresh => link.password_sha256 = None,
        None => {}
    }
    match options.expires_unix {
        Some(0) => link.expires_unix = 0,
        Some(t) => link.expires_unix = t,
        None if fresh => link.expires_unix = 0,
        None => {}
    }
}

impl ShareService for ShareServiceImpl {
    async fn create_link(
        &self,
        target: ShareTarget,
        options: NewShareLink,
    ) -> Result<ShareLinkInfo, ShareError> {
        // The org kill switch (issue #271): minting refuses; existing
        // links keep resolving until individually revoked.
        if self.store.sharing_disabled() {
            return Err(ShareError::SharingDisabled);
        }
        validate_target(&target)?;
        // 32 hex chars of UUIDv4 randomness — unguessable, URL-safe.
        let token = uuid::Uuid::new_v4().simple().to_string();
        let mut link = StoredLink {
            token,
            label: String::new(),
            target: Some(target),
            note_path: String::new(),
            capability: String::new(),
            capabilities: None,
            password_sha256: None,
            expires_unix: 0,
            disabled: false,
            created_at: chrono::Utc::now().to_rfc3339(),
        };
        apply_options(&mut link, options, true);
        let (info, file) = self.store.with_state(|s| {
            s.links.insert(0, link.clone());
            self.info(&link)
        });
        self.store.save(&file)?;
        Ok(info)
    }

    async fn update_link(
        &self,
        token: String,
        options: NewShareLink,
    ) -> Result<ShareLinkInfo, ShareError> {
        let (info, file) = self.store.with_state(|s| {
            s.links.iter_mut().find(|l| l.token == token).map(|l| {
                apply_options(l, options, false);
                self.info(l)
            })
        });
        let Some(info) = info else {
            return Err(ShareError::NotFound);
        };
        self.store.save(&file)?;
        Ok(info)
    }

    async fn list_links(&self) -> Result<Vec<ShareLinkInfo>, ShareError> {
        let (out, _) = self
            .store
            .with_state(|s| s.links.iter().map(|l| self.info(l)).collect());
        Ok(out)
    }

    async fn links_for_target(
        &self,
        target: ShareTarget,
    ) -> Result<Vec<ShareLinkInfo>, ShareError> {
        let (out, _) = self.store.with_state(|s| {
            s.links
                .iter()
                .filter(|l| l.target() == target)
                .map(|l| self.info(l))
                .collect()
        });
        Ok(out)
    }

    async fn set_link_disabled(&self, token: String, disabled: bool) -> Result<(), ShareError> {
        let (found, file) =
            self.store
                .with_state(|s| match s.links.iter_mut().find(|l| l.token == token) {
                    Some(l) => {
                        l.disabled = disabled;
                        true
                    }
                    None => false,
                });
        if !found {
            return Err(ShareError::NotFound);
        }
        self.store.save(&file)
    }

    async fn delete_link(&self, token: String) -> Result<(), ShareError> {
        let (found, file) = self.store.with_state(|s| {
            let before = s.links.len();
            s.links.retain(|l| l.token != token);
            s.links.len() != before
        });
        if !found {
            return Err(ShareError::NotFound);
        }
        self.store.save(&file)
    }

    async fn access_log(&self, token: String) -> Result<Vec<ShareAccess>, ShareError> {
        if self.store.resolve(&token).is_none() {
            return Err(ShareError::NotFound);
        }
        Ok(self.store.read_access(&token))
    }

    async fn set_sharing_disabled(&self, disabled: bool) -> Result<(), ShareError> {
        let ((), file) = self.store.with_state(|s| s.sharing_disabled = disabled);
        self.store.save(&file)
    }

    async fn sharing_disabled(&self) -> Result<bool, ShareError> {
        Ok(self.store.sharing_disabled())
    }
}

// ── HTTP: token-gated serving ───────────────────────────────────────
//
// These routes are the link's whole surface. They are token-gated, not
// session-gated, on purpose: the token IS the grant, checked fresh on
// every hit, and the scope is enforced by construction — a slice
// link's URLs carry paths relative to the slice, so nothing outside it
// is addressable, signed in or not (AC 1).

/// Query string every share route accepts: the link password.
#[derive(serde::Deserialize, Default)]
pub struct ShareQuery {
    #[serde(default)]
    pub pw: Option<String>,
}

/// The gate every share hit passes: resolve, disabled/expired → 410,
/// password → form (absent) or 401 (wrong). `Ok` carries the link.
/// (The refusal Response is boxed — clippy's `result_large_err`; every
/// caller immediately unboxes into its return.)
fn gate(
    state: &AppState,
    slug: &str,
    token: &str,
    pw: Option<&str>,
) -> Result<(crate::OrgAppState, StoredLink), Box<Response>> {
    let Some(org) = state.org(slug) else {
        return Err(Box::new(
            (StatusCode::NOT_FOUND, "no such org").into_response(),
        ));
    };
    let Some(link) = org.shares.resolve(token) else {
        return Err(Box::new(
            (StatusCode::NOT_FOUND, "no such share link").into_response(),
        ));
    };
    if link.disabled {
        return Err(Box::new(
            (StatusCode::GONE, Html(disabled_html())).into_response(),
        ));
    }
    if link.expired(chrono::Utc::now().timestamp()) {
        return Err(Box::new(
            (StatusCode::GONE, Html(expired_html())).into_response(),
        ));
    }
    if !link.password_ok(pw) {
        return Err(Box::new(match pw {
            // Wrong password ≠ no password: the former is a refusal,
            // the latter is the form.
            Some(_) => (
                StatusCode::UNAUTHORIZED,
                Html(password_html(&link.label, true)),
            )
                .into_response(),
            None => (StatusCode::OK, Html(password_html(&link.label, false))).into_response(),
        }));
    }
    Ok((org, link))
}

/// Reject a slice-relative path that tries to escape.
fn clean_rel(rel: &str) -> Result<String, Box<Response>> {
    let rel = rel.trim_matches('/');
    if rel.split('/').any(|s| s == "..") {
        return Err(Box::new(StatusCode::NOT_FOUND.into_response()));
    }
    Ok(rel.to_string())
}

/// Join the slice subpath with a link-relative path.
fn join_scope(subpath: &str, rel: &str) -> String {
    match (subpath.is_empty(), rel.is_empty()) {
        (true, _) => rel.to_string(),
        (false, true) => subpath.to_string(),
        (false, false) => format!("{subpath}/{rel}"),
    }
}

/// What a Files share resolves to: the root and (for a Named Version)
/// the exact commit to serve.
struct FilesScope {
    root_id: uuid::Uuid,
    subpath: String,
    /// `None` = the checkpoint head (slice links follow the live root).
    at: Option<String>,
}

/// Resolve a link's Files scope; `None` for note links.
async fn files_scope(
    org: &crate::OrgAppState,
    link: &StoredLink,
) -> Result<Option<FilesScope>, Response> {
    match link.target() {
        ShareTarget::Note { .. } => Ok(None),
        ShareTarget::Slice { root_id, subpath } => Ok(Some(FilesScope {
            root_id,
            subpath,
            at: None,
        })),
        ShareTarget::NamedVersion { id } => {
            // Resolve the curated entity to its exact change — the
            // whole point of a Named Version link (AC 2).
            let version = org.files.resolve_named_version(id).await.map_err(|e| {
                (StatusCode::NOT_FOUND, format!("named version: {e}")).into_response()
            })?;
            let named = org
                .files
                .list_named_versions(None)
                .await
                .ok()
                .and_then(|all| all.into_iter().find(|v| v.id == id));
            let Some(named) = named else {
                return Err((StatusCode::NOT_FOUND, "named version entity gone").into_response());
            };
            Ok(Some(FilesScope {
                root_id: named.root_id,
                subpath: String::new(),
                at: Some(version.commit_id),
            }))
        }
    }
}

fn pw_suffix(q: &ShareQuery) -> String {
    match &q.pw {
        Some(pw) if !pw.is_empty() => format!("?pw={}", urlencoding_encode(pw)),
        _ => String::new(),
    }
}

/// `GET /org/{slug}/share/{token}` — the landing: note links keep their
/// open-in-app card; Files links render the scoped listing directly.
pub async fn share_landing_handler(
    State(state): State<AppState>,
    AxPath((slug, token)): AxPath<(String, String)>,
    Query(q): Query<ShareQuery>,
) -> Response {
    let (org, link) = match gate(&state, &slug, &token, q.pw.as_deref()) {
        Ok(v) => v,
        Err(resp) => return *resp,
    };
    org.shares.log_access(&token, "view", "");
    match files_scope(&org, &link).await {
        Err(resp) => resp,
        Ok(None) => {
            let app_origin = std::env::var("TASK_SHARE_APP_ORIGIN").unwrap_or_default();
            Html(landing_html(&link, &app_origin)).into_response()
        }
        Ok(Some(scope)) => render_browse(&org, &slug, &token, &link, &scope, "", &q).await,
    }
}

/// `GET /org/{slug}/share/{token}/b/{*rel}` — browse inside the scope.
pub async fn share_browse_handler(
    State(state): State<AppState>,
    AxPath((slug, token, rel)): AxPath<(String, String, String)>,
    Query(q): Query<ShareQuery>,
) -> Response {
    let (org, link) = match gate(&state, &slug, &token, q.pw.as_deref()) {
        Ok(v) => v,
        Err(resp) => return *resp,
    };
    let rel = match clean_rel(&rel) {
        Ok(r) => r,
        Err(resp) => return *resp,
    };
    let scope = match files_scope(&org, &link).await {
        Ok(Some(s)) => s,
        Ok(None) => return (StatusCode::NOT_FOUND, "not a files link").into_response(),
        Err(resp) => return resp,
    };
    org.shares.log_access(&token, "browse", &rel);
    render_browse(&org, &slug, &token, &link, &scope, &rel, &q).await
}

/// `GET /org/{slug}/share/{token}/rendition/{kind}/{*rel}` — stream a
/// derived rendition. This is the ONLY media a view-only link serves:
/// originals need the `download` capability (AC 3).
pub async fn share_rendition_handler(
    State(state): State<AppState>,
    AxPath((slug, token, kind, rel)): AxPath<(String, String, String, String)>,
    Query(q): Query<ShareQuery>,
    headers: axum::http::HeaderMap,
) -> Response {
    let (org, link) = match gate(&state, &slug, &token, q.pw.as_deref()) {
        Ok(v) => v,
        Err(resp) => return *resp,
    };
    let rel = match clean_rel(&rel) {
        Ok(r) => r,
        Err(resp) => return *resp,
    };
    let scope = match files_scope(&org, &link).await {
        Ok(Some(s)) => s,
        Ok(None) => return (StatusCode::NOT_FOUND, "not a files link").into_response(),
        Err(resp) => return resp,
    };
    let Some(wire_kind) = rendition_kind_from_tag(&kind) else {
        return (StatusCode::NOT_FOUND, "unknown rendition kind").into_response();
    };
    let full = join_scope(&scope.subpath, &rel);
    let rendition = match &scope.at {
        Some(commit) => {
            org.files
                .rendition_at(scope.root_id, full, commit.clone(), wire_kind)
                .await
        }
        None => org.files.rendition(scope.root_id, full, wire_kind).await,
    };
    let info = match rendition {
        Ok(info) => info,
        Err(e) => return (StatusCode::NOT_FOUND, format!("rendition: {e}")).into_response(),
    };
    org.shares.log_access(&token, "rendition", &rel);
    let total = info.len;
    let range = headers
        .get(header::RANGE)
        .and_then(|v| v.to_str().ok())
        .and_then(|s| crate::parse_byte_range(s, total));
    crate::rendition_stream_response(&org, scope.root_id, &info.file_id, &info.mime, total, range)
}

/// `GET /org/{slug}/share/{token}/download/{*rel}` — original bytes,
/// gated by the `download` capability, receipted in the access log.
pub async fn share_download_handler(
    State(state): State<AppState>,
    AxPath((slug, token, rel)): AxPath<(String, String, String)>,
    Query(q): Query<ShareQuery>,
) -> Response {
    let (org, link) = match gate(&state, &slug, &token, q.pw.as_deref()) {
        Ok(v) => v,
        Err(resp) => return *resp,
    };
    if !link.capabilities().download {
        return (
            StatusCode::FORBIDDEN,
            "this link is view-only — downloads are not enabled",
        )
            .into_response();
    }
    let rel = match clean_rel(&rel) {
        Ok(r) => r,
        Err(resp) => return *resp,
    };
    let scope = match files_scope(&org, &link).await {
        Ok(Some(s)) => s,
        Ok(None) => return (StatusCode::NOT_FOUND, "not a files link").into_response(),
        Err(resp) => return resp,
    };
    let full = join_scope(&scope.subpath, &rel);
    let len = match org
        .files
        .source_len_at(scope.root_id, full.clone(), scope.at.clone())
        .await
    {
        Ok(n) => n,
        Err(e) => return (StatusCode::NOT_FOUND, format!("download: {e}")).into_response(),
    };
    // The receipt (AC 4) — written before the stream starts, so an
    // aborted transfer still shows who pulled the trigger.
    org.shares.log_access(&token, "download", &rel);
    let filename = rel.rsplit('/').next().unwrap_or("download").to_string();
    let (mut writer, reader) = tokio::io::duplex(64 * 1024);
    let files = org.files.clone();
    let (root_id, at) = (scope.root_id, scope.at.clone());
    tokio::spawn(async move {
        if let Err(e) = files.read_source_at(root_id, full, at, &mut writer).await {
            tracing::warn!(?e, "share download: read failed mid-stream");
        }
    });
    let body = axum::body::Body::from_stream(tokio_util::io::ReaderStream::new(reader));
    (
        StatusCode::OK,
        [
            (header::CONTENT_TYPE, "application/octet-stream".to_string()),
            (header::CONTENT_LENGTH, len.to_string()),
            (
                header::CONTENT_DISPOSITION,
                format!("attachment; filename=\"{}\"", filename.replace('"', "")),
            ),
        ],
        body,
    )
        .into_response()
}

fn rendition_kind_from_tag(tag: &str) -> Option<files_proto::RenditionKind> {
    use files_proto::RenditionKind as K;
    Some(match tag {
        "proxy-1080" => K::Proxy1080,
        "proxy-720" => K::Proxy720,
        "audio-aac" => K::Audio,
        "peaks" => K::Peaks,
        "filmstrip" => K::Filmstrip,
        _ => return None,
    })
}

/// Render a directory listing (or bounce to the file page) at `rel`
/// inside the scope.
async fn render_browse(
    org: &crate::OrgAppState,
    slug: &str,
    token: &str,
    link: &StoredLink,
    scope: &FilesScope,
    rel: &str,
    q: &ShareQuery,
) -> Response {
    let full = join_scope(&scope.subpath, rel);
    let listing = match &scope.at {
        Some(commit) => {
            org.files
                .browse_at(scope.root_id, commit.clone(), full.clone())
                .await
        }
        None => org.files.browse(scope.root_id, full.clone()).await,
    };
    let base = format!("/org/{slug}/share/{token}");
    let pw = pw_suffix(q);
    match listing {
        Ok(entries) => Html(browse_html(link, &base, rel, &entries, &pw)).into_response(),
        // Not a directory (or not present as one): render the file page.
        Err(_) => Html(file_html(link, &base, rel, &pw)).into_response(),
    }
}

// ── HTML ────────────────────────────────────────────────────────────

const PAGE_CSS: &str = r"
 body{font-family:system-ui,sans-serif;background:#0b0d10;color:#e6e8eb;margin:0;padding:2rem;display:flex;justify-content:center}
 .card{background:#14171c;border:1px solid #262b33;border-radius:12px;padding:2rem;max-width:44rem;width:100%}
 h1{font-size:1.1rem;margin:0 0 .3rem}
 p{color:#9aa3af;font-size:.9rem}
 a{color:#a5b4fc;text-decoration:none}
 a:hover{text-decoration:underline}
 ul{list-style:none;padding:0;margin:.8rem 0}
 li{padding:.45rem .2rem;border-bottom:1px solid #1d222a;display:flex;gap:.6rem;align-items:baseline}
 .cap{display:inline-block;border:1px solid #333a45;border-radius:99px;padding:.1rem .6rem;font-size:.72rem;color:#9aa3af;margin:0 .25rem .8rem 0;text-transform:uppercase;letter-spacing:.08em}
 .crumb{font-size:.85rem;color:#9aa3af;margin-bottom:.6rem}
 video{width:100%;border-radius:8px;background:#000}
 .btn{display:inline-block;background:#6d5ef2;color:#fff;border-radius:8px;padding:.5rem 1.1rem;font-weight:600;margin-top:1rem}
 input{background:#0b0d10;border:1px solid #333a45;border-radius:8px;color:#e6e8eb;padding:.5rem .8rem}
";

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

fn page(title: &str, body: &str) -> String {
    format!(
        r#"<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="robots" content="noindex">
<title>{title}</title><style>{PAGE_CSS}</style></head>
<body><div class="card">{body}</div></body></html>"#,
        title = html_escape(title),
    )
}

fn caps_chips(link: &StoredLink) -> String {
    let caps = link.capabilities();
    let mut out = String::from(r#"<span class="cap">view</span>"#);
    if caps.comment {
        out.push_str(r#"<span class="cap">comment</span>"#);
    }
    if caps.download {
        out.push_str(r#"<span class="cap">download</span>"#);
    }
    out
}

fn browse_html(
    link: &StoredLink,
    base: &str,
    rel: &str,
    entries: &[files_proto::BrowseEntry],
    pw: &str,
) -> String {
    let label = html_escape(&link.label);
    let mut body = format!("<h1>{label}</h1>{}", caps_chips(link));
    if !rel.is_empty() {
        let parent = match rel.rsplit_once('/') {
            Some((p, _)) => format!("{base}/b/{p}{pw}"),
            None => format!("{base}{pw}"),
        };
        body.push_str(&format!(
            r#"<div class="crumb"><a href="{parent}">&larr; up</a> &nbsp; /{rel}</div>"#,
            rel = html_escape(rel),
        ));
    }
    body.push_str("<ul>");
    for e in entries {
        let child = if rel.is_empty() {
            e.name.clone()
        } else {
            format!("{rel}/{}", e.name)
        };
        let icon = if e.is_dir { "&#128193;" } else { "&#128196;" };
        body.push_str(&format!(
            r#"<li>{icon} <a href="{base}/b/{child}{pw}">{name}</a></li>"#,
            child = urlencoding_encode(&child),
            name = html_escape(&e.name),
        ));
    }
    if entries.is_empty() {
        body.push_str("<li>empty</li>");
    }
    body.push_str("</ul>");
    page(&link.label, &body)
}

/// Video extensions the share file page embeds a proxy player for —
/// mirror of the app's review-player list.
fn is_video_name(name: &str) -> bool {
    let ext = name.rsplit('.').next().unwrap_or_default().to_lowercase();
    matches!(
        ext.as_str(),
        "mov" | "mp4" | "m4v" | "mkv" | "webm" | "avi" | "mxf" | "mts"
    )
}

fn file_html(link: &StoredLink, base: &str, rel: &str, pw: &str) -> String {
    let label = html_escape(&link.label);
    let name = rel.rsplit('/').next().unwrap_or(rel);
    let mut body = format!(
        "<h1>{label}</h1>{}<div class=\"crumb\">/{rel}</div>",
        caps_chips(link),
        rel = html_escape(rel),
    );
    if is_video_name(name) {
        body.push_str(&format!(
            r#"<video controls preload="metadata" src="{base}/rendition/proxy-720/{rel}{pw}"></video>"#,
            rel = urlencoding_encode(rel),
        ));
    } else {
        body.push_str(&format!("<p>{}</p>", html_escape(name)));
    }
    if link.capabilities().download {
        body.push_str(&format!(
            r#"<a class="btn" href="{base}/download/{rel}{pw}">Download original</a>"#,
            rel = urlencoding_encode(rel),
        ));
    } else {
        body.push_str(r#"<p>View-only link — originals aren't downloadable.</p>"#);
    }
    page(&link.label, &body)
}

fn password_html(label: &str, wrong: bool) -> String {
    let note = if wrong {
        r#"<p style="color:#f87171">Wrong password.</p>"#
    } else {
        "<p>This link is password-protected.</p>"
    };
    let body = format!(
        r#"<h1>{label}</h1>{note}
<form method="get"><input type="password" name="pw" placeholder="Password" autofocus>
<button class="btn" type="submit">Open</button></form>"#,
        label = html_escape(label),
    );
    page(label, &body)
}

/// The share landing page for a NOTE link: token-checked on EVERY hit
/// (revocation is immediate), then a minimal page that opens the shared
/// note in the app. `app_origin` = where the web app lives
/// (`TASK_SHARE_APP_ORIGIN`; empty = same origin).
pub fn landing_html(link: &StoredLink, app_origin: &str) -> String {
    let note = match link.target() {
        ShareTarget::Note { path } => path,
        _ => String::new(),
    };
    let label = html_escape(&link.label);
    let open = format!(
        "{}/vault?path={}&share=1",
        app_origin.trim_end_matches('/'),
        urlencoding_encode(&note)
    );
    let body = format!(
        r#"<h1>{label}</h1>{caps}
<p>You've been invited to <strong>{note}</strong>.</p>
<a class="btn" href="{open}">Open</a>"#,
        caps = caps_chips(link),
        note = html_escape(&note),
    );
    page(&link.label, &body)
}

/// Gone page for a disabled link.
pub fn disabled_html() -> &'static str {
    r#"<!doctype html><html><head><meta charset="utf-8"><title>Link disabled</title>
<style>body{font-family:system-ui,sans-serif;background:#0b0d10;color:#9aa3af;display:flex;min-height:100vh;align-items:center;justify-content:center}</style>
</head><body><p>This share link has been disabled by its owner.</p></body></html>"#
}

/// Gone page for an expired link.
pub fn expired_html() -> &'static str {
    r#"<!doctype html><html><head><meta charset="utf-8"><title>Link expired</title>
<style>body{font-family:system-ui,sans-serif;background:#0b0d10;color:#9aa3af;display:flex;min-height:100vh;align-items:center;justify-content:center}</style>
</head><body><p>This share link has expired.</p></body></html>"#
}

/// Percent-encode a path for a URL (kept tiny to avoid a dep).
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
