//! Append-only log store. For audit trails, webhook receipts, any
//! "what happened in order" history. Separate sub-trait so
//! key-value backends don't have to grow log semantics they don't
//! support.

use crate::error::StoreError;
use crate::types::LogEntry;

#[architect::rpc]
pub trait LogStore {
    /// Append `data` to `channel`. Returns the assigned `seq`
    /// (monotonic per channel).
    fn append(&self, channel: &str, data: Vec<u8>) -> Result<u64, StoreError>;
    /// Read up to `limit` entries with `seq > from_seq`. Pass
    /// `from_seq = 0` to start from the beginning. Returns the
    /// entries in seq order.
    fn read(&self, channel: &str, from_seq: u64, limit: u32) -> Result<Vec<LogEntry>, StoreError>;
    /// Drop entries with `seq <= up_to_seq`. Used to compact the
    /// log after a downstream consumer has acknowledged.
    fn truncate(&self, channel: &str, up_to_seq: u64) -> Result<(), StoreError>;
}
