//! The share guest lane (issue #272): `/org/{slug}/share/{token}/vox`
//! puts an anonymous visitor in the REAL app's RPC surface, scoped to
//! exactly one Review — same wire types, same clients, so the review
//! player and comment thread work unchanged over a guest connection.
//!
//! The scoping is structural, not advisory: the lane mounts wrapper
//! services that delegate ONLY the review's own calls to the backend
//! and refuse everything else. A guest holds no session and needs
//! none — the token (plus password/expiry, re-checked at upgrade) is
//! the whole grant, and every comment posted through the lane is
//! stamped with the link's attribution server-side (AC 1: "attribution
//! records the link").

use files::{FilesError, FilesService};
use files_proto::{
    BrowseEntry, ChainEntry, CheckpointInfo, DivergenceChoice, DivergenceInfo, FileRootInfo,
    GcReport, HydrationReport, NamedVersion, NewReviewComment, ProjectVersion, RenditionInfo,
    RenditionKind, RestartMode, Review, ReviewComment, RootFlavor, SnapshotInfo, VersionRef,
};
use media_proto::{MediaChunk, MediaError, MediaGrant, MediaInfo, MediaService};
use share_proto::ShareCapabilities;
use uuid::Uuid;

fn denied<T>() -> Result<T, FilesError> {
    Err(FilesError::BadRequest(
        "not available on a guest review link".into(),
    ))
}

/// The Files surface a Review guest sees: the review, its comments,
/// its file's chain and renditions — nothing else in the org.
#[derive(Clone)]
pub struct GuestFilesService {
    files: files::FilesBackend,
    review: Review,
    caps: ShareCapabilities,
    /// Stamped onto every comment (AC 1) — "label (token-prefix)".
    attribution: String,
}

impl GuestFilesService {
    pub fn new(
        files: files::FilesBackend,
        review: Review,
        caps: ShareCapabilities,
        attribution: String,
    ) -> Self {
        Self {
            files,
            review,
            caps,
            attribution,
        }
    }

    fn in_scope(&self, root_id: Uuid, path: &str) -> bool {
        root_id == self.review.root_id && path == self.review.file_path
    }
}

impl FilesService for GuestFilesService {
    // ── the review's own surface ──────────────────────────────────

    async fn find_review(
        &self,
        root_id: Uuid,
        file_path: String,
    ) -> Result<Option<Review>, FilesError> {
        Ok(self
            .in_scope(root_id, &file_path)
            .then(|| self.review.clone()))
    }

    async fn review_for_file(
        &self,
        root_id: Uuid,
        file_path: String,
    ) -> Result<Review, FilesError> {
        // Get, never create: the review already exists (the link was
        // minted on it), and a guest must not mint vault entities.
        if self.in_scope(root_id, &file_path) {
            Ok(self.review.clone())
        } else {
            denied()
        }
    }

    async fn review_comments(&self, review_id: Uuid) -> Result<Vec<ReviewComment>, FilesError> {
        if review_id != self.review.id {
            return denied();
        }
        self.files.review_comments(review_id).await
    }

    async fn add_review_comment(
        &self,
        review_id: Uuid,
        comment: NewReviewComment,
    ) -> Result<ReviewComment, FilesError> {
        if review_id != self.review.id {
            return denied();
        }
        if !self.caps.comment {
            return Err(FilesError::BadRequest(
                "this link is view-only — commenting is not enabled".into(),
            ));
        }
        self.files
            .add_review_comment_via(review_id, comment, self.attribution.clone())
            .await
    }

    async fn chain(&self, root_id: Uuid, path: String) -> Result<Vec<ChainEntry>, FilesError> {
        if !self.in_scope(root_id, &path) {
            return denied();
        }
        self.files.chain(root_id, path).await
    }

    async fn rendition(
        &self,
        root_id: Uuid,
        path: String,
        kind: RenditionKind,
    ) -> Result<RenditionInfo, FilesError> {
        if !self.in_scope(root_id, &path) {
            return denied();
        }
        self.files.rendition(root_id, path, kind).await
    }

    async fn rendition_at(
        &self,
        root_id: Uuid,
        path: String,
        commit_id: String,
        kind: RenditionKind,
    ) -> Result<RenditionInfo, FilesError> {
        if !self.in_scope(root_id, &path) {
            return denied();
        }
        self.files
            .rendition_at(root_id, path, commit_id, kind)
            .await
    }

    /// The switcher's Named Version stars — root-scoped names only.
    async fn list_named_versions(
        &self,
        root_id: Option<Uuid>,
    ) -> Result<Vec<NamedVersion>, FilesError> {
        if root_id != Some(self.review.root_id) {
            return denied();
        }
        self.files.list_named_versions(root_id).await
    }

    // ── everything else: refused ──────────────────────────────────

    async fn create_root(
        &self,
        _path: String,
        _name: String,
        _flavor: RootFlavor,
    ) -> Result<FileRootInfo, FilesError> {
        denied()
    }
    async fn list_roots(&self) -> Result<Vec<FileRootInfo>, FilesError> {
        denied()
    }
    async fn get_root(&self, _id: Uuid) -> Result<FileRootInfo, FilesError> {
        denied()
    }
    async fn browse(
        &self,
        _root_id: Uuid,
        _subpath: String,
    ) -> Result<Vec<BrowseEntry>, FilesError> {
        denied()
    }
    async fn drive_browse(&self, _path: String) -> Result<Vec<BrowseEntry>, FilesError> {
        denied()
    }
    async fn checkpoint_now(
        &self,
        _root_id: Uuid,
        _message: Option<String>,
    ) -> Result<CheckpointInfo, FilesError> {
        denied()
    }
    async fn hint_activity(&self, _root_id: Uuid, _paths: Vec<String>) -> Result<u32, FilesError> {
        denied()
    }
    async fn snapshots(&self, _root_id: Uuid) -> Result<Vec<SnapshotInfo>, FilesError> {
        denied()
    }
    async fn ignore_set(&self, _root_id: Uuid) -> Result<Vec<String>, FilesError> {
        denied()
    }
    async fn set_ignore_set(
        &self,
        _root_id: Uuid,
        _patterns: Vec<String>,
    ) -> Result<Vec<String>, FilesError> {
        denied()
    }
    async fn name_version(
        &self,
        _root_id: Uuid,
        _commit_id: String,
        _name: String,
    ) -> Result<NamedVersion, FilesError> {
        denied()
    }
    async fn resolve_named_version(&self, _id: Uuid) -> Result<VersionRef, FilesError> {
        denied()
    }
    async fn unname_version(&self, _id: Uuid) -> Result<(), FilesError> {
        denied()
    }
    async fn start_project_version(
        &self,
        _root_id: Uuid,
        _label: Option<String>,
    ) -> Result<ProjectVersion, FilesError> {
        denied()
    }
    async fn list_project_versions(
        &self,
        _root_id: Uuid,
    ) -> Result<Vec<ProjectVersion>, FilesError> {
        denied()
    }
    async fn gc_root(
        &self,
        _root_id: Uuid,
        _keep_secs: Option<u64>,
    ) -> Result<GcReport, FilesError> {
        denied()
    }
    async fn dehydrate(&self, _root_id: Uuid, _path: String) -> Result<BrowseEntry, FilesError> {
        denied()
    }
    async fn hydrate(&self, _root_id: Uuid, _path: String) -> Result<BrowseEntry, FilesError> {
        denied()
    }
    async fn hydration_policy(&self, _root_id: Uuid) -> Result<Vec<String>, FilesError> {
        denied()
    }
    async fn set_hydration_policy(
        &self,
        _root_id: Uuid,
        _patterns: Vec<String>,
    ) -> Result<Vec<String>, FilesError> {
        denied()
    }
    async fn apply_hydration_policy(&self, _root_id: Uuid) -> Result<HydrationReport, FilesError> {
        denied()
    }
    async fn restart_project_version(
        &self,
        _root_id: Uuid,
        _mode: RestartMode,
        _label: Option<String>,
    ) -> Result<ProjectVersion, FilesError> {
        denied()
    }
    async fn browse_at(
        &self,
        _root_id: Uuid,
        _commit_id: String,
        _subpath: String,
    ) -> Result<Vec<BrowseEntry>, FilesError> {
        denied()
    }
    async fn copy_forward(
        &self,
        _root_id: Uuid,
        _commit_id: String,
        _paths: Vec<String>,
    ) -> Result<Vec<String>, FilesError> {
        denied()
    }
    async fn divergences(&self, _root_id: Uuid) -> Result<Vec<DivergenceInfo>, FilesError> {
        denied()
    }
    async fn resolve_divergence(
        &self,
        _root_id: Uuid,
        _path: String,
        _choice: DivergenceChoice,
    ) -> Result<CheckpointInfo, FilesError> {
        denied()
    }
    async fn list_reviews(&self, _root_id: Option<Uuid>) -> Result<Vec<Review>, FilesError> {
        denied()
    }
    async fn delete_review_comment(&self, _id: Uuid) -> Result<(), FilesError> {
        denied()
    }
}

/// The media surface a guest sees: exactly one grant prefix — the
/// review root's renditions — so the `<video>` URLs the player builds
/// work, and nothing else on the media route does (AC 2).
#[derive(Clone)]
pub struct GuestMediaService {
    inner: crate::media::MediaServiceImpl,
    allowed_prefix: String,
}

impl GuestMediaService {
    pub fn new(inner: crate::media::MediaServiceImpl, root_id: Uuid) -> Self {
        Self {
            inner,
            allowed_prefix: format!("files/renditions/{root_id}"),
        }
    }
}

impl MediaService for GuestMediaService {
    async fn stat(&self, _content_hash: String) -> Result<MediaInfo, MediaError> {
        Err(MediaError::Internal(
            "not available on a guest review link".into(),
        ))
    }

    async fn read(
        &self,
        _content_hash: String,
        _start: u64,
        _len: u64,
        _tx: vox::Tx<MediaChunk>,
    ) -> Result<(), MediaError> {
        Err(MediaError::Internal(
            "not available on a guest review link".into(),
        ))
    }

    async fn media_grant(&self, prefix: String) -> Result<MediaGrant, MediaError> {
        if prefix != self.allowed_prefix {
            return Err(MediaError::Internal(format!(
                "a guest review link may only mint grants for {}",
                self.allowed_prefix
            )));
        }
        self.inner.media_grant(prefix).await
    }
}
