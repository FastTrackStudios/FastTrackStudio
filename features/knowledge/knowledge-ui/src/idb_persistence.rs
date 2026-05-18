//! Local-first persistence: store the Loro snapshot + an
//! append-only update log in IndexedDB so reload restores the
//! doc instantly, without a server round-trip.
//!
//! ## Storage protocol (Loro-native)
//!
//! Mirrors what Loro's persistence doc prescribes
//! (`/tmp/loro-llms.txt` lines 4212-4250):
//!
//! 1. **Snapshot store** — one record per doc. Key = doc UUID
//!    string, value = bytes from
//!    `LoroDoc::export(ExportMode::Snapshot)`.
//! 2. **Updates store** — auto-increment key, value carries
//!    `{ doc: <uuid>, bytes: <update> }`. Indexed by `doc` for
//!    per-doc replay. Appended on every commit via
//!    `LoroDoc::subscribe_local_update`.
//!
//! Cold start replays the snapshot then every update for the
//! given doc. Periodic compaction (every `COMPACT_THRESHOLD`
//! updates) writes a fresh snapshot and clears just the matching
//! updates.
//!
//! ## Threading
//!
//! IndexedDB calls return `!Send` futures because they touch
//! `web-sys` JS objects. That's fine on wasm32 (single-threaded)
//! but means this module is wasm-only.

#![cfg(target_arch = "wasm32")]

use indexed_db_futures::database::Database;
use indexed_db_futures::error::{Error as IdbErrorRaw, OpenDbError};
use indexed_db_futures::prelude::*;
use indexed_db_futures::transaction::TransactionMode;
use indexed_db_futures::typed_array::TypedArray;

const DB_NAME: &str = "task-architect";
const DB_VERSION: u8 = 2;
const STORE_SNAPSHOTS: &str = "snapshots";
const STORE_UPDATES: &str = "updates";
const IDX_UPDATES_DOC: &str = "by_doc";

/// Compaction threshold — when the local in-memory update count
/// for a given doc exceeds this, write a fresh snapshot and
/// clear just that doc's updates.
pub const COMPACT_THRESHOLD: usize = 100;

#[derive(Debug, Clone)]
pub struct IdbError(pub String);

impl std::fmt::Display for IdbError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl From<IdbErrorRaw> for IdbError {
    fn from(e: IdbErrorRaw) -> Self {
        Self(format!("{e:?}"))
    }
}

impl From<OpenDbError> for IdbError {
    fn from(e: OpenDbError) -> Self {
        Self(format!("{e:?}"))
    }
}

/// Cheap handle to the opened DB. Cloneable — the underlying
/// `Database` shares state via the browser's IDB connection.
#[derive(Clone)]
pub struct IdbPersistence {
    db: Database,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
struct UpdateRecord {
    doc: String,
    bytes: Vec<u8>,
}

impl IdbPersistence {
    /// Open (and upgrade if needed) the `task-architect` DB.
    /// Returns `Err` if the browser refuses (private window,
    /// quota, etc.) — callers should fall back to ephemeral.
    pub async fn open() -> Result<Self, IdbError> {
        let db = Database::open(DB_NAME)
            .with_version(DB_VERSION)
            .with_on_upgrade_needed_fut(|event, db| async move {
                let old_version = event.old_version() as u64;
                // v2 changed the schema (per-doc keying). v1 data
                // wasn't shipped to users so we just drop the old
                // stores and recreate.
                if old_version >= 1 && old_version < 2 {
                    let _ = db.delete_object_store(STORE_SNAPSHOTS);
                    let _ = db.delete_object_store(STORE_UPDATES);
                }
                if old_version < 2 {
                    db.create_object_store(STORE_SNAPSHOTS).build()?;
                    let updates = db
                        .create_object_store(STORE_UPDATES)
                        .with_auto_increment(true)
                        .build()?;
                    updates
                        .create_index(IDX_UPDATES_DOC, indexed_db_futures::KeyPath::from("doc"))
                        .build()?;
                }
                Ok(())
            })
            .await?;
        Ok(Self { db })
    }

    /// Load the most recent snapshot for `doc_id`. `None` on
    /// first-ever open of this doc.
    pub async fn load_snapshot(&self, doc_id: &str) -> Result<Option<Vec<u8>>, IdbError> {
        let tx = self.db.transaction(STORE_SNAPSHOTS).build()?;
        let store = tx.object_store(STORE_SNAPSHOTS)?;
        let val: Option<TypedArray<u8>> = store.get(doc_id).primitive()?.await?;
        tx.commit().await?;
        Ok(val.map(|v| v.into()))
    }

    /// Load every update appended after the current snapshot for
    /// `doc_id`, in append order.
    pub async fn load_updates(&self, doc_id: &str) -> Result<Vec<Vec<u8>>, IdbError> {
        let tx = self.db.transaction(STORE_UPDATES).build()?;
        let store = tx.object_store(STORE_UPDATES)?;
        let index = store.index(IDX_UPDATES_DOC)?;
        let Some(mut cursor) = index.open_cursor().with_query(doc_id).serde()?.await? else {
            tx.commit().await?;
            return Ok(Vec::new());
        };
        let mut out: Vec<Vec<u8>> = Vec::new();
        while let Some(rec) = cursor.next_record_ser::<UpdateRecord>().await? {
            out.push(rec.bytes);
        }
        tx.commit().await?;
        Ok(out)
    }

    /// Append one update blob for `doc_id`. Fire-and-forget at
    /// the call site (spawned from `subscribe_local_update`).
    pub async fn append_update(&self, doc_id: &str, bytes: Vec<u8>) -> Result<(), IdbError> {
        let tx = self
            .db
            .transaction(STORE_UPDATES)
            .with_mode(TransactionMode::Readwrite)
            .build()?;
        let store = tx.object_store(STORE_UPDATES)?;
        let rec = UpdateRecord {
            doc: doc_id.to_string(),
            bytes,
        };
        store.add(rec).serde()?.await?;
        tx.commit().await?;
        Ok(())
    }

    /// Replace the snapshot for `doc_id` and atomically clear
    /// just its update records. Used by `compact` after enough
    /// writes accumulate.
    pub async fn write_snapshot_and_clear(
        &self,
        doc_id: &str,
        snapshot: Vec<u8>,
    ) -> Result<(), IdbError> {
        let tx = self
            .db
            .transaction([STORE_SNAPSHOTS, STORE_UPDATES])
            .with_mode(TransactionMode::Readwrite)
            .build()?;
        let snap_store = tx.object_store(STORE_SNAPSHOTS)?;
        let updates_store = tx.object_store(STORE_UPDATES)?;
        snap_store
            .put(TypedArray::new(snapshot))
            .with_key(doc_id)
            .primitive()?
            .await?;
        // Delete every update record matching this doc.
        let index = updates_store.index(IDX_UPDATES_DOC)?;
        if let Some(mut cursor) = index.open_cursor().with_query(doc_id).serde()?.await? {
            while let Some(_rec) = cursor.next_record_ser::<UpdateRecord>().await? {
                cursor.delete()?.await?;
            }
        }
        tx.commit().await?;
        Ok(())
    }
}
