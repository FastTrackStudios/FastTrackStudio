//! The one seam every RPC method in this crate goes through.
//!
//! All the real work is synchronous — it touches jj-lib and the chunk
//! store, whose futures are not `Send` on every path, so they are driven
//! with `pollster::block_on` rather than awaited inside an
//! `#[architect::rpc]` method's own future (see [`crate::agent`]'s module
//! doc). Running that sync work on `tokio::task::spawn_blocking` rather
//! than inline is the other half: hosting a live tree or replicating a
//! multi-GB root must not stall the runtime's other org RPCs — the same
//! convention `files`' backend adopted after the PR #280 review.

use files_storage_proto::StorageError;

use crate::error::{Error, to_storage_error};

pub async fn blocking<T, F>(f: F) -> Result<T, StorageError>
where
    F: FnOnce() -> Result<T, Error> + Send + 'static,
    T: Send + 'static,
{
    tokio::task::spawn_blocking(f)
        .await
        .map_err(|e| StorageError::Io(format!("blocking task panicked: {e}")))?
        .map_err(to_storage_error)
}
