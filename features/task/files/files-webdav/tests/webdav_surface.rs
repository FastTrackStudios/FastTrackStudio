//! The WebDAV bridge (issue #274) driven the way a file manager drives
//! it: real `http::Request`s in, real responses out.
//!
//! The spec's Testing Decisions put the proving ground at "the external
//! behaviour seam, never store internals". For the RPC surface that is
//! an in-process memory link; for a *protocol* bridge the equivalent
//! seam is the protocol itself — `OPTIONS` / `PROPFIND` / `PUT` / `GET`
//! / `MKCOL` / `MOVE` / `DELETE` / `LOCK` against
//! [`WebdavBridge::handle`], with the File Root underneath created
//! through the ordinary `FilesService` calls. Nothing below reaches
//! into `LiveTreeFs`, the registry, or the version store.
//!
//! Each test is named for the acceptance criterion it proves.

use bytes::Bytes;
use files::{FilesBackend, FilesService as _, RootFlavor};
use files_webdav::WebdavBridge;
use http::{Method, Request, Response, StatusCode};
use http_body_util::{BodyExt as _, Full};

const MOUNT: &str = "/org/acme/dav";

/// A `FilesBackend` over a temp org files area, plus a bridge on it.
struct Harness {
    _dir: tempfile::TempDir,
    data_dir: std::path::PathBuf,
    backend: FilesBackend,
    bridge: WebdavBridge,
}

impl Harness {
    fn new() -> Self {
        let dir = tempfile::tempdir().expect("data tempdir");
        let data_dir = dir.path().to_path_buf();
        let backend = FilesBackend::new(&data_dir).expect("backend");
        let bridge = WebdavBridge::new(backend.clone()).expect("bridge");
        Self {
            _dir: dir,
            data_dir,
            backend,
            bridge,
        }
    }

    /// Stage a folder under the org's files area and turn it into a
    /// File Root — the ordinary route, marker file and version store
    /// included.
    async fn root(&self, name: &str) -> files::FileRootInfo {
        let dir = self.data_dir.join(name);
        std::fs::create_dir_all(&dir).expect("stage root dir");
        self.backend
            .create_root(
                dir.to_str().unwrap().to_string(),
                name.to_string(),
                RootFlavor::Media,
            )
            .await
            .expect("create_root")
    }

    async fn send(&self, method: &str, path: &str, body: &[u8]) -> (StatusCode, String) {
        self.send_with(method, path, body, &[]).await
    }

    async fn send_with(
        &self,
        method: &str,
        path: &str,
        body: &[u8],
        headers: &[(&str, &str)],
    ) -> (StatusCode, String) {
        let res = self.raw(method, path, body, headers).await;
        let status = res.status();
        let bytes = res
            .into_body()
            .collect()
            .await
            .expect("collect response body")
            .to_bytes();
        (status, String::from_utf8_lossy(&bytes).into_owned())
    }

    async fn raw(
        &self,
        method: &str,
        path: &str,
        body: &[u8],
        headers: &[(&str, &str)],
    ) -> Response<files_webdav::Body> {
        let mut builder = Request::builder()
            .method(Method::from_bytes(method.as_bytes()).expect("method"))
            .uri(path);
        for (k, v) in headers {
            builder = builder.header(*k, *v);
        }
        let req = builder
            .body(Full::new(Bytes::copy_from_slice(body)))
            .expect("request builds");
        self.bridge.handle(MOUNT, req).await
    }

    /// `PROPFIND` with the depth a file manager uses when it opens a
    /// folder.
    async fn propfind_depth1(&self, path: &str) -> (StatusCode, String) {
        self.send_with("PROPFIND", path, b"", &[("Depth", "1")])
            .await
    }
}

/// Acceptance criterion 1, half one: what a file manager does at mount
/// time — `OPTIONS` on the mount URL must advertise WebDAV class 2 (no
/// class 2, no read-write mount on macOS or Windows) and offer the
/// write verbs, and `PROPFIND` must list the org's roots as folders.
#[tokio::test(flavor = "multi_thread")]
async fn a_file_manager_can_mount_and_see_the_roots() {
    let h = Harness::new();
    h.root("El Artisa").await;
    h.root("Dr Jaramillo").await;

    let res = h.raw("OPTIONS", MOUNT, b"", &[]).await;
    assert_eq!(res.status(), StatusCode::OK);
    let dav = res
        .headers()
        .get("DAV")
        .expect("OPTIONS advertises a DAV header")
        .to_str()
        .unwrap()
        .to_string();
    assert!(dav.contains('2'), "WebDAV class 2 (locking): {dav}");
    let allow = res
        .headers()
        .get("Allow")
        .expect("OPTIONS advertises Allow")
        .to_str()
        .unwrap()
        .to_string();
    for verb in ["PROPFIND", "LOCK", "UNLOCK"] {
        assert!(allow.contains(verb), "{verb} missing from Allow: {allow}");
    }

    let (status, body) = h.propfind_depth1(&format!("{MOUNT}/")).await;
    assert_eq!(status, StatusCode::MULTI_STATUS);
    assert!(body.contains("El%20Artisa"), "{body}");
    assert!(body.contains("Dr%20Jaramillo"), "{body}");
}

/// Acceptance criterion 1, half two: a root mounts **read-write** —
/// the full round of verbs a file manager issues when a user drags a
/// file in, renames it, makes a folder and deletes something, plus the
/// `LOCK`/`UNLOCK` pair that keeps an OS client's mount happy.
#[tokio::test(flavor = "multi_thread")]
async fn a_root_mounts_read_write() {
    let h = Harness::new();
    h.root("Mix Session").await;
    let root_url = format!("{MOUNT}/Mix%20Session");

    // PUT a file, then read it back byte for byte.
    let (status, _) = h
        .send("PUT", &format!("{root_url}/mix.wav"), b"take one")
        .await;
    assert_eq!(status, StatusCode::CREATED);
    let (status, body) = h.send("GET", &format!("{root_url}/mix.wav"), b"").await;
    assert_eq!(status, StatusCode::OK);
    assert_eq!(body, "take one");

    // Overwriting an existing file is the DAW-save case.
    let (status, _) = h
        .send("PUT", &format!("{root_url}/mix.wav"), b"take two, final")
        .await;
    assert_eq!(status, StatusCode::NO_CONTENT);

    // MKCOL + a file inside it, then PROPFIND the new collection.
    let (status, _) = h.send("MKCOL", &format!("{root_url}/stems"), b"").await;
    assert_eq!(status, StatusCode::CREATED);
    let (status, _) = h
        .send("PUT", &format!("{root_url}/stems/kick.wav"), b"boom")
        .await;
    assert_eq!(status, StatusCode::CREATED);
    let (status, body) = h.propfind_depth1(&format!("{root_url}/stems/")).await;
    assert_eq!(status, StatusCode::MULTI_STATUS);
    assert!(body.contains("kick.wav"), "{body}");

    // MOVE (rename) and DELETE.
    let (status, _) = h
        .send_with(
            "MOVE",
            &format!("{root_url}/stems/kick.wav"),
            b"",
            &[("Destination", &format!("{root_url}/stems/kik.wav"))],
        )
        .await;
    assert_eq!(status, StatusCode::CREATED);
    let (status, _) = h
        .send("GET", &format!("{root_url}/stems/kik.wav"), b"")
        .await;
    assert_eq!(status, StatusCode::OK);
    let (status, _) = h
        .send("DELETE", &format!("{root_url}/stems/kik.wav"), b"")
        .await;
    assert_eq!(status, StatusCode::NO_CONTENT);
    let (status, _) = h
        .send("GET", &format!("{root_url}/stems/kik.wav"), b"")
        .await;
    assert_eq!(status, StatusCode::NOT_FOUND);

    // LOCK / UNLOCK: the exchange an OS client performs before it will
    // treat the mount as writable.
    let lock_body = br#"<?xml version="1.0" encoding="utf-8" ?>
        <D:lockinfo xmlns:D="DAV:">
          <D:lockscope><D:exclusive/></D:lockscope>
          <D:locktype><D:write/></D:locktype>
          <D:owner><D:href>finder</D:href></D:owner>
        </D:lockinfo>"#;
    let res = h
        .raw("LOCK", &format!("{root_url}/mix.wav"), lock_body, &[])
        .await;
    assert_eq!(res.status(), StatusCode::OK);
    let token = res
        .headers()
        .get("Lock-Token")
        .expect("LOCK returns a Lock-Token")
        .to_str()
        .unwrap()
        .to_string();
    let (status, _) = h
        .send_with(
            "UNLOCK",
            &format!("{root_url}/mix.wav"),
            b"",
            &[("Lock-Token", &token)],
        )
        .await;
    assert_eq!(status, StatusCode::NO_CONTENT);
}

/// Acceptance criterion 2: "writes through the bridge enter the cadence
/// pipeline like any other write". The bridge has no privileged write
/// path — a `PUT` lands in the live tree, so the ordinary
/// scan-certified Session checkpoint picks it up and it appears in the
/// file's version chain, exactly as a write over NFS would.
#[tokio::test(flavor = "multi_thread")]
async fn writes_through_the_bridge_enter_the_cadence_pipeline() {
    let h = Harness::new();
    let root = h.root("Tracking Day").await;
    let root_url = format!("{MOUNT}/Tracking%20Day");

    h.send("PUT", &format!("{root_url}/session.rpp"), b"reaper project")
        .await;
    let cp1 = h
        .backend
        .checkpoint_now(root.id, Some("after the drop".into()))
        .await
        .expect("checkpoint_now");
    assert!(
        cp1.changed_paths.contains(&"session.rpp".to_string()),
        "the checkpoint scan saw the WebDAV write: {:?}",
        cp1.changed_paths
    );

    // A second save through the bridge extends the same chain — one
    // history, whichever surface wrote.
    h.send(
        "PUT",
        &format!("{root_url}/session.rpp"),
        b"reaper project v2",
    )
    .await;
    let cp2 = h
        .backend
        .checkpoint_now(root.id, None)
        .await
        .expect("checkpoint_now");
    assert_eq!(cp2.changed_paths, vec!["session.rpp".to_string()]);

    let chain = h
        .backend
        .chain(root.id, "session.rpp".to_string())
        .await
        .expect("chain");
    assert_eq!(chain.len(), 2, "two saved states: {chain:?}");
    assert_eq!(chain[0].commit_id, cp2.commit_id, "newest first");
    assert_eq!(chain[1].commit_id, cp1.commit_id);
}

/// Acceptance criterion 3: "only current heads are visible; version
/// history is not exposed". The version store lives *inside* the root,
/// so the proof is that it neither appears in a listing nor can be
/// reached, read, or deleted by name — and that the mount has no
/// version-addressed URL space at all.
#[tokio::test(flavor = "multi_thread")]
async fn version_history_is_not_exposed() {
    let h = Harness::new();
    let root = h.root("Private History").await;
    let root_url = format!("{MOUNT}/Private%20History");

    h.send("PUT", &format!("{root_url}/mix.wav"), b"take one")
        .await;
    h.backend
        .checkpoint_now(root.id, None)
        .await
        .expect("checkpoint_now");
    // The store really is there on disk — otherwise this test proves
    // nothing about hiding it.
    assert!(
        std::path::Path::new(&root.path).join(".fts-files").is_dir(),
        "the version store exists inside the live tree"
    );

    let (status, body) = h.propfind_depth1(&format!("{root_url}/")).await;
    assert_eq!(status, StatusCode::MULTI_STATUS);
    assert!(body.contains("mix.wav"), "the live tree is visible: {body}");
    assert!(
        !body.contains(".fts-files"),
        "version store leaked into a listing: {body}"
    );
    assert!(
        !body.contains(".fts-root.json"),
        "root marker leaked into a listing: {body}"
    );

    // Asking for it by name is a 404, not a 403 — it is not part of
    // this tree as far as WebDAV is concerned.
    for path in [".fts-files", ".fts-root.json", ".fts-files/store"] {
        let (status, _) = h.send("GET", &format!("{root_url}/{path}"), b"").await;
        assert_eq!(status, StatusCode::NOT_FOUND, "GET {path}");
        let (status, _) = h.propfind_depth1(&format!("{root_url}/{path}")).await;
        assert_eq!(status, StatusCode::NOT_FOUND, "PROPFIND {path}");
    }

    // And it cannot be destroyed or written through the mount.
    let (status, _) = h
        .send("DELETE", &format!("{root_url}/.fts-files"), b"")
        .await;
    assert_eq!(status, StatusCode::NOT_FOUND);
    // A `PUT` at a hidden name is refused as 409 rather than 404 —
    // dav-server reads "the target's parent chain does not resolve" as
    // a conflict. Either way the write does not happen, which is the
    // property that matters, so assert the refusal and the disk.
    let marker = std::path::Path::new(&root.path).join(".fts-root.json");
    let before = std::fs::read(&marker).expect("marker exists");
    let (status, _) = h
        .send("PUT", &format!("{root_url}/.fts-root.json"), b"{}")
        .await;
    assert!(status.is_client_error(), "PUT at a hidden name: {status}");
    assert_eq!(std::fs::read(&marker).unwrap(), before, "marker untouched");
    assert!(
        std::path::Path::new(&root.path).join(".fts-files").is_dir(),
        "the version store survived every attempt at it"
    );
}

/// Acceptance criterion 4, the per-root half: "a per-root policy can
/// hide a root from WebDAV". A hidden root must be indistinguishable
/// from one that does not exist — no listing entry, no reachable path
/// — while staying perfectly usable over the RPC surface.
#[tokio::test(flavor = "multi_thread")]
async fn a_hidden_root_is_unreachable_over_webdav() {
    let h = Harness::new();
    let public = h.root("Shared Project").await;
    let secret = h.root("Client Masters").await;

    let (_, body) = h.propfind_depth1(&format!("{MOUNT}/")).await;
    assert!(body.contains("Client%20Masters"), "{body}");

    h.bridge
        .policy()
        .set_hidden(secret.id, true)
        .expect("hide the root");

    let (status, body) = h.propfind_depth1(&format!("{MOUNT}/")).await;
    assert_eq!(status, StatusCode::MULTI_STATUS);
    assert!(body.contains("Shared%20Project"), "{body}");
    assert!(
        !body.contains("Client%20Masters"),
        "a hidden root must not be listed: {body}"
    );

    // Not reachable by name, nor by the uuid escape hatch.
    for seg in ["Client%20Masters", &secret.id.to_string()] {
        let (status, _) = h.propfind_depth1(&format!("{MOUNT}/{seg}/")).await;
        assert_eq!(
            status,
            StatusCode::NOT_FOUND,
            "hidden root reachable as {seg}"
        );
    }
    // The visible one still works, so hiding is per-root and not a
    // global off switch.
    let (status, _) = h
        .send("PUT", &format!("{MOUNT}/Shared%20Project/notes.txt"), b"ok")
        .await;
    assert_eq!(status, StatusCode::CREATED);

    // The root itself is untouched — hidden from a compat surface, not
    // from Files.
    assert!(
        h.backend
            .list_roots()
            .await
            .unwrap()
            .iter()
            .any(|r| r.id == secret.id),
        "hiding a root from WebDAV must not affect the RPC surface"
    );

    // Un-hiding restores it.
    h.bridge
        .policy()
        .set_hidden(secret.id, false)
        .expect("unhide");
    let (_, body) = h.propfind_depth1(&format!("{MOUNT}/")).await;
    assert!(body.contains("Client%20Masters"), "{body}");
    assert_eq!(public.name, "Shared Project");
}

/// The bridge is org-confined like every other Files surface: a path
/// cannot climb out of the root it addresses, textually or through a
/// symlink planted inside the live tree.
#[tokio::test(flavor = "multi_thread")]
async fn paths_cannot_escape_the_root_they_address() {
    let h = Harness::new();
    let a = h.root("Root A").await;
    h.root("Root B").await;
    let outside = tempfile::tempdir().expect("outside tempdir");
    std::fs::write(outside.path().join("secret.txt"), b"another org's data").unwrap();
    std::fs::write(h.data_dir.join("loose.txt"), b"outside any root").unwrap();

    // `..` out of a root — the mount's own root is the ceiling, and a
    // path that climbs above it is refused outright rather than
    // resolving against the server filesystem.
    for path in [
        "/org/acme/dav/Root%20A/../loose.txt",
        "/org/acme/dav/Root%20A/../../../../etc/passwd",
    ] {
        let (status, _) = h.send("GET", path, b"").await;
        assert_ne!(status, StatusCode::OK, "escaped via {path}");
    }

    // A symlink inside the root pointing outside it: the textual path
    // never leaves the root, so only resolving it catches this.
    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(outside.path(), std::path::Path::new(&a.path).join("link"))
            .unwrap();
        let (status, _) = h
            .send("GET", "/org/acme/dav/Root%20A/link/secret.txt", b"")
            .await;
        assert_ne!(
            status,
            StatusCode::OK,
            "a symlink out of the live tree must not be followed"
        );
    }

    // A request outside the mount is simply not ours.
    let (status, _) = h.send("PROPFIND", "/org/acme/media/x", b"").await;
    assert_eq!(status, StatusCode::NOT_FOUND);
}

/// Each root is served as its own WebDAV namespace, so a `MOVE` whose
/// `Destination` points into a *different* root has no meaning here and
/// must be refused rather than silently resolving somewhere surprising.
/// Roots never overlap on disk (glossary "File Root"), and moving
/// between them is a Files-level operation, not a file-manager drag.
#[tokio::test(flavor = "multi_thread")]
async fn a_move_between_two_roots_is_refused() {
    let h = Harness::new();
    let a = h.root("Root A").await;
    let b = h.root("Root B").await;
    h.send("PUT", "/org/acme/dav/Root%20A/take.wav", b"take one")
        .await;

    let (status, _) = h
        .send_with(
            "MOVE",
            "/org/acme/dav/Root%20A/take.wav",
            b"",
            &[("Destination", "/org/acme/dav/Root%20B/take.wav")],
        )
        .await;
    // 502 is RFC 4918's answer for a `Destination` on another server or
    // namespace — which is exactly what the other root is here.
    assert_eq!(status, StatusCode::BAD_GATEWAY, "cross-root MOVE");
    assert!(
        std::path::Path::new(&a.path).join("take.wav").exists(),
        "the source survives a refused move"
    );
    assert!(
        !std::path::Path::new(&b.path).join("take.wav").exists(),
        "nothing was written into the other root"
    );
}

/// Roots are created through `FilesService::create_root` — which mints
/// the id, writes the marker and initializes the version store — so the
/// mount point itself is read-only. A client dropping a folder onto the
/// mount must not produce a directory that looks like a root but has no
/// identity or history.
#[tokio::test(flavor = "multi_thread")]
async fn the_mount_point_itself_is_read_only() {
    let h = Harness::new();
    h.root("Existing").await;

    let (status, _) = h
        .send("MKCOL", &format!("{MOUNT}/New%20Project"), b"")
        .await;
    assert_ne!(status, StatusCode::CREATED);
    let (status, _) = h.send("PUT", &format!("{MOUNT}/stray.txt"), b"nope").await;
    assert_ne!(status, StatusCode::CREATED);
    assert!(
        !h.data_dir.join("New Project").exists() && !h.data_dir.join("stray.txt").exists(),
        "the mount point must not be writable"
    );

    // An unknown root is a 404, not a server error.
    let (status, _) = h
        .propfind_depth1(&format!("{MOUNT}/No%20Such%20Root/"))
        .await;
    assert_eq!(status, StatusCode::NOT_FOUND);
}
