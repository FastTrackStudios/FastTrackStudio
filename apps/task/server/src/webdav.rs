//! `/org/{slug}/dav` — the HTTP mount point for the Files WebDAV bridge
//! (issue #274). Authentication and org scoping live here; the protocol
//! and the filesystem view live in `files-webdav`.
//!
//! ## Why auth is here and not in the bridge
//!
//! Task's identity normally rides the vox WS upgrade. WebDAV clients
//! speak plain HTTP and cannot, so this route re-uses the shape
//! `/media` established for the same reason: authenticate the request
//! itself, then hand an already-scoped call to the feature. Three
//! credentials are accepted, all of them *existing* tokens — this
//! ticket adds no new credential type:
//!
//! 1. `Authorization: Bearer <session token>` — native clients and
//!    scripts, which own their requests and can set headers.
//! 2. `Authorization: Basic <user:secret>` — what Finder and Explorer
//!    actually send. The password is tried first as a session token
//!    (paste what `task` already holds, no password on disk in a
//!    keychain entry), then as an email/password sign-in, which is what
//!    a human typing into the OS mount dialog will do.
//! 3. `?token=` — a signed `BlobToken` media grant covering this org's
//!    dav space, for the odd tool that can only carry a URL.
//!
//! Whatever the route, the session must be valid **in this org's own
//! auth store**, which is the same "any member of this org" boundary
//! `permits.rs` puts on the `FilesService` RPC verbs. Per-root
//! narrowing beyond that is the bridge's `WebdavPolicy` (hide a root)
//! until the Files permission model lands with slices.
//!
//! ## Why this enforces unconditionally
//!
//! `/media` ships its check behind `TASK_ENFORCE_MEDIA_TOKEN` because
//! turning it on hot would have blacked out every `<audio>` tag on an
//! already-deployed bundle. This route is new: it has no existing
//! clients to break, and an unauthenticated WebDAV mount would hand an
//! org's entire project tree to anyone who can reach the server. It
//! fails closed from its first deploy, and a `401` carries the
//! `WWW-Authenticate: Basic` challenge that makes an OS client prompt
//! for credentials instead of silently failing to mount.

use axum::extract::State;
use axum::response::{IntoResponse as _, Response};
use base64::Engine as _;
use axum::http::{HeaderMap, Request, StatusCode, header};

use crate::AppState;

/// The realm an OS mount dialog shows. Kept stable — macOS keys its
/// keychain entries on it.
const REALM: &str = "Task Files";

/// Query string of [`webdav_handler`] — the signed grant, for callers
/// that can only carry a URL.
#[derive(serde::Deserialize, Default)]
pub struct DavQuery {
    #[serde(default)]
    pub token: Option<String>,
}

fn challenge() -> Response {
    (
        StatusCode::UNAUTHORIZED,
        [(
            header::WWW_AUTHENTICATE,
            format!("Basic realm=\"{REALM}\", charset=\"UTF-8\""),
        )],
        "Files WebDAV requires a Task session",
    )
        .into_response()
}

/// Split an `Authorization: Basic` header into its user and secret.
fn basic(headers: &HeaderMap) -> Option<(String, String)> {
    let raw = headers
        .get(header::AUTHORIZATION)?
        .to_str()
        .ok()?
        .strip_prefix("Basic ")?;
    let decoded = base64::engine::general_purpose::STANDARD
        .decode(raw.trim())
        .ok()?;
    let text = String::from_utf8(decoded).ok()?;
    let (user, secret) = text.split_once(':')?;
    Some((user.to_owned(), secret.to_owned()))
}

/// Is `token` a live session in `slug`'s own auth store?
async fn session_ok(state: &AppState, slug: &str, token: &str) -> bool {
    if token.is_empty() {
        return false;
    }
    match state.org(slug) {
        Some(org) => org
            .auth
            .auth
            .current_session(architect_auth::CurrentSession {
                token: token.to_owned(),
            })
            .await
            .is_ok(),
        None => false,
    }
}

/// `None` = authenticated, proceed; `Some(response)` = refuse with it.
///
/// Always records how the decision was reached (`dav.auth_via`), the
/// same wide-event discipline `/media` uses, so a mount that will not
/// authenticate is diagnosable from traces rather than from a user's
/// description of a Finder dialog.
async fn authorize(
    state: &AppState,
    slug: &str,
    path: &str,
    token: Option<&str>,
    headers: &HeaderMap,
) -> Option<Response> {
    use task_telemetry::wide;

    wide::set("org.slug", slug.to_owned());
    wide::set("dav.path", path.to_owned());

    if state.org(slug).is_none() {
        wide::set("dav.auth_via", "unknown-org");
        // Same answer as a bad credential: whether an org exists here
        // is not something an unauthenticated caller gets to probe.
        return Some(challenge());
    }

    // 1. Bearer session — native clients.
    if let Some(bearer) = crate::watch_bridge::bearer(headers) {
        let ok = session_ok(state, slug, &bearer).await;
        wide::set("dav.auth_via", if ok { "bearer" } else { "bearer-invalid" });
        if ok {
            return None;
        }
        return Some(challenge());
    }

    // 2. HTTP Basic — what Finder and Explorer send.
    if let Some((user, secret)) = basic(headers) {
        if session_ok(state, slug, &secret).await {
            wide::set("dav.auth_via", "basic-session-token");
            return None;
        }
        let signed_in = match state.org(slug) {
            Some(org) => org
                .auth
                .auth
                .sign_in_email_password(architect_auth::SignInEmailPassword {
                    email: user,
                    password: secret,
                    ip_address: None,
                    user_agent: Some(format!("webdav/{REALM}")),
                })
                .await
                .is_ok(),
            None => false,
        };
        wide::set(
            "dav.auth_via",
            if signed_in {
                "basic-password"
            } else {
                "basic-invalid"
            },
        );
        return if signed_in { None } else { Some(challenge()) };
    }

    // 3. Signed URL grant.
    if let Some(token) = token.filter(|t| !t.is_empty()) {
        let now = chrono::Utc::now().timestamp();
        let ok = matches!(
            crate::attachments::signed_url::BlobToken::verify(token, &state.keypair, now),
            Ok(tok) if tok.allows_media(slug, path)
        );
        wide::set(
            "dav.auth_via",
            if ok { "signed-token" } else { "token-invalid" },
        );
        return if ok { None } else { Some(challenge()) };
    }

    wide::set("dav.auth_via", "absent");
    Some(challenge())
}

/// `ANY /org/{slug}/dav` and `ANY /org/{slug}/dav/{*path}` — mount an
/// org's File Roots from a file manager. See the module doc.
pub async fn webdav_handler(
    State(state): State<AppState>,
    axum::extract::Path(params): axum::extract::Path<
        std::collections::HashMap<String, String>,
    >,
    axum::extract::Query(q): axum::extract::Query<DavQuery>,
    req: Request<axum::body::Body>,
) -> Response {
    let Some(slug) = params.get("slug").cloned() else {
        return StatusCode::NOT_FOUND.into_response();
    };
    let mount = format!("/org/{slug}/dav");
    // The path *within* the mount, for the auth trail and for the
    // signed-grant scope check.
    let rel = req
        .uri()
        .path()
        .strip_prefix(&mount)
        .unwrap_or_default()
        .trim_start_matches('/')
        .to_owned();

    if let Some(refusal) = authorize(
        &state,
        &slug,
        &rel,
        q.token.as_deref(),
        req.headers(),
    )
    .await
    {
        return refusal;
    }

    let Some(org) = state.org(&slug) else {
        return StatusCode::NOT_FOUND.into_response();
    };
    org.files_webdav
        .handle(&mount, req)
        .await
        .map(axum::body::Body::new)
}
