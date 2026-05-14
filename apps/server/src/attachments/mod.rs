//! Phase 7 — attachments (blob storage + signed URL HTTP routes).
//!
//! Layout:
//!
//! - [`object_store`] — `ObjectStore` trait + `LocalFsStore` impl.
//!   New backends (Nextcloud, S3) implement the same trait; the
//!   service is generic over it.
//! - [`signed_url`] — `BlobToken` issuance + verification via the
//!   server's existing `ServerKeypair` (Phase 3). Two purposes:
//!   `Upload` (carries `upload_id`) and `Download` (carries
//!   `content_hash`).
//! - [`service`] — `AttachmentServiceImpl` — implements the
//!   `attachments_proto::AttachmentService` vox trait. Holds an
//!   `UploadSessionMap` keyed by `upload_id` until
//!   `complete_upload` commits the row.
//! - [`routes`] — the two axum handlers: `PUT /blobs/upload` and
//!   `GET /blobs/download/{hash}`. Both verify a `?token=…` in
//!   the query string before doing real work.

pub mod object_store;
pub mod routes;
pub mod service;
pub mod signed_url;

pub use object_store::{LocalFsStore, ObjectStore, default_blob_root};
pub use routes::attachment_router;
pub use service::{AttachmentServiceImpl, UploadSessionMap};
pub use signed_url::{BlobToken, BlobTokenPurpose};
