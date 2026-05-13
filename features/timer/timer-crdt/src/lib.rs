//! Loro-backed `TimerRepo`. Source of truth lives in a LoroDoc;
//! persistence is the concern of `timer-db`.

use architect::{Page, RepoError, SortOrder};
use chrono::{DateTime, Utc};
use crdt::EntityCrdt;
use loro::{LoroMap, LoroValue};
use timer_proto::{Timer, TimerCreate, TimerList, TimerRepo, TimerUpdate};
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

/// Zero-sized marker for the `EntityCrdt` impl. Lets us implement
/// the foreign `EntityCrdt` trait without owning the wire struct.
pub struct TimerEntity;

/// Newtype-wrapped repo, implements `TimerRepo`. Construct from a
/// `CrdtDoc` — multiple repos over different entity types can share
/// one doc, mutations commit together, exports cover the whole doc.
#[derive(Clone)]
pub struct TimerRepoLoro {
    inner: LoroRepo<TimerEntity>,
}

impl TimerRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }

    pub fn inner(&self) -> &LoroRepo<TimerEntity> {
        &self.inner
    }

    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

// ── EntityCrdt impl ───────────────────────────────────────────────────
//
// Container layout: a top-level "timer_items" LoroMap keyed by
// uuid string; each entry is a sub-LoroMap with the wire fields.
// For features with hierarchy or ordering, swap this for a LoroTree
// or LoroMovableList — the trait surface stays the same.

impl EntityCrdt for TimerEntity {
    type Wire = Timer;
    type Create = TimerCreate;
    type Update = TimerUpdate;
    type List = TimerList;

    const ROOT: &'static str = "timer_items";

    fn id(wire: &Timer) -> Uuid {
        wire.id
    }

    fn from_create(input: TimerCreate) -> Timer {
        let now = Utc::now();
        Timer {
            id: Uuid::new_v4(),
            name: input.name,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(map: &LoroMap, e: &Timer) -> Result<(), RepoError> {
        map.insert("id", e.id.to_string()).map_err(loro_err)?;
        map.insert("name", e.name.as_str()).map_err(loro_err)?;
        map.insert("created_at", e.created_at.to_rfc3339())
            .map_err(loro_err)?;
        map.insert("updated_at", e.updated_at.to_rfc3339())
            .map_err(loro_err)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Timer, RepoError> {
        Ok(Timer {
            id: parse_uuid(read_str(m, "id")?)?,
            name: read_str(m, "name")?,
            created_at: parse_dt(read_str(m, "created_at")?)?,
            updated_at: parse_dt(read_str(m, "updated_at")?)?,
        })
    }

    fn apply_update(m: &LoroMap, input: TimerUpdate) -> Result<(), RepoError> {
        if let Some(name) = input.name {
            m.insert("name", name.as_str()).map_err(loro_err)?;
        }
        m.insert("updated_at", Utc::now().to_rfc3339())
            .map_err(loro_err)?;
        Ok(())
    }

    fn sort_items(items: &mut [Timer], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "created_at" => items.sort_by(|a, b| a.created_at.cmp(&b.created_at)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<Timer>, total: u32, page: Page) -> TimerList {
        TimerList { items, total, page }
    }
}

// ── TimerRepo forwarders ────────────────────────────────────────────

impl TimerRepo for TimerRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Timer, RepoError> {
        self.inner.get(id).await
    }

    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<TimerList, RepoError> {
        self.inner.list(page, sort, filter).await
    }

    async fn create(&self, input: TimerCreate) -> Result<Timer, RepoError> {
        self.inner.create(input).await
    }

    async fn update(&self, id: Uuid, input: TimerUpdate) -> Result<Timer, RepoError> {
        self.inner.update(id, input).await
    }

    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── codec helpers ─────────────────────────────────────────────────────

fn read_str(m: &LoroMap, k: &str) -> Result<String, RepoError> {
    match m.get(k) {
        Some(loro::ValueOrContainer::Value(LoroValue::String(s))) => Ok((*s).to_string()),
        Some(other) => Err(RepoError::Internal(format!(
            "expected string at key `{k}`, got {other:?}"
        ))),
        None => Err(RepoError::Internal(format!("missing key `{k}`"))),
    }
}

fn parse_uuid(s: String) -> Result<Uuid, RepoError> {
    Uuid::parse_str(&s).map_err(|e| RepoError::Internal(format!("bad uuid: {e}")))
}

fn parse_dt(s: String) -> Result<DateTime<Utc>, RepoError> {
    DateTime::parse_from_rfc3339(&s)
        .map(|dt| dt.with_timezone(&Utc))
        .map_err(|e| RepoError::Internal(format!("bad timestamp: {e}")))
}

fn loro_err<E: std::fmt::Display>(e: E) -> RepoError {
    RepoError::Internal(format!("loro: {e}"))
}
