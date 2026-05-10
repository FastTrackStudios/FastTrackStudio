//! SeaORM-backed `AttachmentService` implementation.
//!
//! Files live in an external provider (Nextcloud is canonical); this service
//! mediates between the metadata table and the WebDAV layer. When no
//! provider is wired in, `nextcloud`-sourced ops surface
//! `provider_not_configured` while pure metadata ops (`list_for_entity`)
//! still work.

use std::sync::Arc;

use sha2::{Digest, Sha256};
use uuid::Uuid;

use crate::attachment::{
    Attachment, AttachmentApi, AttachmentApiCreate, AttachmentApiList, AttachmentRepo,
    DEFAULT_ATTACHMENT_SOURCE, default_remote_path, label_from_path,
};
use crate::provider::NextcloudSync;
use crate::service::{
    AttachmentDownloadResponse, AttachmentService, AttachmentUploadRequest, VaultError,
};

use super::helpers::{convert_model, convert_ref, provider_not_configured};

/// Typed requirements for [`AttachmentServiceImpl`].
///
/// `nextcloud = None` makes provider-backed operations (upload/download/
/// delete on rows where `source = "nextcloud"`) return
/// `provider_not_configured`; the metadata read path (`list_for_entity`)
/// keeps working regardless.
pub struct AttachmentServiceDeps<R> {
    pub repo: R,
    pub nextcloud: Option<Arc<NextcloudSync>>,
}

#[derive(Clone)]
pub struct AttachmentServiceImpl<R> {
    repo: R,
    nextcloud: Option<Arc<NextcloudSync>>,
}

impl<R> AttachmentServiceImpl<R> {
    pub fn new(deps: AttachmentServiceDeps<R>) -> Self {
        Self {
            repo: deps.repo,
            nextcloud: deps.nextcloud,
        }
    }

    fn provider(&self, op: &str) -> Result<&NextcloudSync, VaultError> {
        self.nextcloud
            .as_ref()
            .map(Arc::as_ref)
            .ok_or_else(|| provider_not_configured(op))
    }
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let digest = hasher.finalize();
    digest.iter().map(|b| format!("{b:02x}")).collect()
}

fn is_nextcloud_source(source: &str) -> bool {
    source.eq_ignore_ascii_case("nextcloud")
}

impl<R> AttachmentServiceImpl<R>
where
    R: AttachmentRepo,
{
    async fn fetch_row(&self, id: Uuid) -> Result<Attachment, VaultError> {
        let api = self
            .repo
            .get_attachment(id.to_string())
            .await
            .map_err(|err| {
                if err.to_lowercase().contains("not found") {
                    VaultError::NotFound(err)
                } else {
                    VaultError::IoError(err)
                }
            })?;
        convert_model::<AttachmentApi, Attachment>(api)
    }
}

impl<R> AttachmentService for AttachmentServiceImpl<R>
where
    R: AttachmentRepo,
{
    async fn list_for_entity(
        &self,
        owner_type: String,
        owner_id: Uuid,
    ) -> Result<Vec<Attachment>, VaultError> {
        // The generated repo only takes a serialized filter string, so do the
        // selection in memory. Volumes here are small (attachments per owner
        // entity) — if this ever grows we can push the filter into a
        // crudcrate `filterable` query string.
        let rows = self
            .repo
            .list_attachments(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::IoError)?;
        let owner_id_str = owner_id;
        let owner_type_lc = owner_type.to_ascii_lowercase();
        let mut out = Vec::new();
        for row in rows {
            let attachment = convert_model::<AttachmentApiList, Attachment>(row)?;
            if attachment.owner_id == owner_id_str
                && attachment.owner_type.eq_ignore_ascii_case(&owner_type_lc)
            {
                out.push(attachment);
            }
        }
        out.sort_by_key(|attachment| attachment.created_at);
        Ok(out)
    }

    async fn upload(&self, request: AttachmentUploadRequest) -> Result<Attachment, VaultError> {
        if request.owner_type.trim().is_empty() {
            return Err(VaultError::ParseError(
                "attachment upload missing owner_type".to_string(),
            ));
        }
        if request.bytes.is_empty() {
            return Err(VaultError::ParseError(
                "attachment upload missing bytes".to_string(),
            ));
        }

        let source = if request.source.trim().is_empty() {
            DEFAULT_ATTACHMENT_SOURCE.to_string()
        } else {
            request.source.clone()
        };

        let path = if request.path.trim().is_empty() {
            // Without a path the caller didn't tell us how to name the
            // remote file — synthesize a sensible default that includes the
            // owner ids so multiple attachments don't collide.
            let basename = request
                .label
                .as_deref()
                .filter(|s| !s.trim().is_empty())
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("{}.bin", Uuid::new_v4()));
            default_remote_path(&request.owner_type, request.owner_id, &basename)
        } else {
            request.path.clone()
        };

        let label = request
            .label
            .clone()
            .filter(|s| !s.trim().is_empty())
            .or_else(|| Some(label_from_path(&path)));

        let checksum = sha256_hex(&request.bytes);
        let size_bytes = i64::try_from(request.bytes.len()).ok();

        // Push to provider first when source = nextcloud — if that fails we
        // never write the metadata row, keeping the two stores in sync.
        if is_nextcloud_source(&source) {
            let provider = self.provider("attachment upload to nextcloud")?;
            provider.webdav_put(&path, &request.bytes, None).await?;
        }

        let attachment = Attachment {
            id: Uuid::new_v4(),
            owner_id: request.owner_id,
            owner_type: request.owner_type.clone(),
            source: source.clone(),
            path: path.clone(),
            label,
            mime: request.mime.clone(),
            size_bytes,
            checksum: Some(checksum),
            uploader: request.uploader.clone(),
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
        };

        let create = convert_ref::<Attachment, AttachmentApiCreate>(&attachment)?;
        let api = self
            .repo
            .create_attachment(create)
            .await
            .map_err(VaultError::IoError)?;
        convert_model::<AttachmentApi, Attachment>(api)
    }

    async fn download(&self, id: Uuid) -> Result<AttachmentDownloadResponse, VaultError> {
        let metadata = self.fetch_row(id).await?;
        if !is_nextcloud_source(&metadata.source) {
            return Err(VaultError::IoError(format!(
                "attachment {id} source `{}` is not downloadable from this server",
                metadata.source
            )));
        }
        let provider = self.provider("attachment download from nextcloud")?;
        let bytes = provider.webdav_get(&metadata.path).await?;
        Ok(AttachmentDownloadResponse { metadata, bytes })
    }

    async fn delete(&self, id: Uuid) -> Result<(), VaultError> {
        let metadata = self.fetch_row(id).await?;
        if is_nextcloud_source(&metadata.source) {
            // Best effort: attempt the remote delete first. If the provider
            // is missing we still want to free the row, so degrade to a
            // local-only delete in that case.
            if let Ok(provider) = self.provider("attachment delete from nextcloud") {
                provider.webdav_delete(&metadata.path).await?;
            }
        }
        self.repo
            .delete_attachment(id.to_string())
            .await
            .map_err(VaultError::IoError)?;
        Ok(())
    }
}
