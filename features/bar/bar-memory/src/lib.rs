//! In-memory `BarRepo` implementation.

use std::sync::Arc;

use architect::{Filter, Page, RepoError, Sort, SortOrder};
use chrono::Utc;
use bar_proto::{
    Bar, BarCreate, BarList, BarRepo, BarUpdate,
};
use tokio::sync::RwLock;
use uuid::Uuid;

#[derive(Clone, Default)]
pub struct BarRepoMemory {
    inner: Arc<RwLock<Vec<Bar>>>,
}

impl BarRepoMemory {
    pub fn new() -> Self {
        Self::default()
    }
}

impl BarRepo for BarRepoMemory {
    async fn get(&self, id: Uuid) -> Result<Bar, RepoError> {
        self.inner
            .read()
            .await
            .iter()
            .find(|e| e.id == id)
            .cloned()
            .ok_or(RepoError::NotFound)
    }

    // r[impl bar.list.empty]
    // r[impl bar.list.pagination.size]
    // r[impl bar.list.pagination.size-clamped]
    // r[impl bar.list.pagination.offset]
    // r[impl bar.list.sort.name-asc]
    // r[impl bar.list.sort.name-desc]
    // r[impl bar.list.sort.unknown]
    async fn list(
        &self,
        page: Page,
        sort: Option<Sort>,
        _filter: Option<Filter>,
    ) -> Result<BarList, RepoError> {
        let mut items: Vec<Bar> = self.inner.read().await.iter().cloned().collect();

        if let Some(s) = sort {
            match s.field.as_str() {
                "name" => {
                    items.sort_by(|a, b| a.name.cmp(&b.name));
                    if matches!(s.order, SortOrder::Desc) {
                        items.reverse();
                    }
                }
                other => {
                    return Err(RepoError::InvalidInput(format!(
                        "unsortable field: {other}"
                    )));
                }
            }
        }

        let total = items.len() as u32;
        let size = page.size.max(1) as usize;
        let start = (page.index as usize).saturating_mul(size);
        let items = items.into_iter().skip(start).take(size).collect();
        Ok(BarList { items, total, page })
    }

    async fn create(&self, input: BarCreate) -> Result<Bar, RepoError> {
        let now = Utc::now();
        let row = Bar {
            id: Uuid::new_v4(),
            name: input.name,
            created_at: now,
            updated_at: now,
        };
        self.inner.write().await.push(row.clone());
        Ok(row)
    }

    async fn update(&self, id: Uuid, input: BarUpdate) -> Result<Bar, RepoError> {
        let mut guard = self.inner.write().await;
        let row = guard
            .iter_mut()
            .find(|e| e.id == id)
            .ok_or(RepoError::NotFound)?;
        if let Some(v) = input.name {
            row.name = v;
        }
        row.updated_at = Utc::now();
        Ok(row.clone())
    }

    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        let mut guard = self.inner.write().await;
        let before = guard.len();
        guard.retain(|e| e.id != id);
        if guard.len() == before {
            return Err(RepoError::NotFound);
        }
        Ok(())
    }
}
