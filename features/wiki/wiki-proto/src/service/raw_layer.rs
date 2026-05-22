//! Immutable raw source layer.

use crate::error::WikiError;
use crate::ingest::IngestTask;
use crate::raw::{ImportRawSource, RawSourceRef};
use crate::review::ReviewItem;

#[architect::rpc]
pub trait RawLayer {
    fn import_raw_source(
        &self,
        wiki_id: &str,
        source: ImportRawSource,
    ) -> Result<RawSourceRef, WikiError>;
    fn list_raw_sources(&self, wiki_id: &str) -> Result<Vec<RawSourceRef>, WikiError>;
    fn read_raw_source(&self, wiki_id: &str, path: &str) -> Result<Vec<u8>, WikiError>;
    fn delete_raw_source(&self, wiki_id: &str, path: &str) -> Result<Vec<ReviewItem>, WikiError>;
    fn rescan_sources(&self, wiki_id: &str) -> Result<Vec<IngestTask>, WikiError>;
}
