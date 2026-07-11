//! Plain key-value store. Values are opaque bytes — encoding is
//! the caller's responsibility (typically JSON).

use crate::error::StoreError;

#[architect::rpc]
pub trait KvStore {
    /// `None` if the key is absent. Backends never invent values.
    fn get(&self, namespace: &str, key: &str) -> Result<Option<Vec<u8>>, StoreError>;
    /// Overwrites any prior value at the same key.
    fn put(&self, namespace: &str, key: &str, value: Vec<u8>) -> Result<(), StoreError>;
    /// Idempotent — deleting a missing key is not an error.
    fn delete(&self, namespace: &str, key: &str) -> Result<(), StoreError>;
    /// Every key in `namespace`. Order is backend-defined; callers
    /// that need a stable order should sort.
    fn list_keys(&self, namespace: &str) -> Result<Vec<String>, StoreError>;
}
