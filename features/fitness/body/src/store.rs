//! File-backed [`BodyService`] impl.

use std::sync::{Arc, Mutex};

use uuid::Uuid;
use vault::Vault;

use crate::model::{BodyEntry, BodyMetric};
use crate::parse::{looks_like_body_metric, parse_page};
use crate::scan::scan_vault;
use crate::service::{BodyError, BodyService};
use crate::write::{default_metric_path, serialize_metric};

#[derive(Clone)]
pub struct Store {
    inner: Arc<Mutex<Vault>>,
}

impl Store {
    pub fn new(vault: Vault) -> Self {
        Self {
            inner: Arc::new(Mutex::new(vault)),
        }
    }

    pub fn from_shared(inner: Arc<Mutex<Vault>>) -> Self {
        Self { inner }
    }

    pub fn shared(&self) -> Arc<Mutex<Vault>> {
        self.inner.clone()
    }
}

fn map_io(e: impl std::fmt::Display) -> BodyError {
    BodyError::Io(e.to_string())
}

fn find_idx(vault: &Vault, id: Uuid) -> Option<usize> {
    vault.pages.iter().position(|p| {
        looks_like_body_metric(p) && parse_page(p).map(|m| m.id == id).unwrap_or(false)
    })
}

impl BodyService for Store {
    fn list(&self) -> Result<Vec<BodyMetric>, BodyError> {
        let guard = self.inner.lock().expect("body store poisoned");
        Ok(scan_vault(&guard))
    }

    fn get(&self, id: &str) -> Result<BodyMetric, BodyError> {
        let uuid = Uuid::parse_str(id).map_err(|e| BodyError::BadRequest(format!("id: {e}")))?;
        let guard = self.inner.lock().expect("body store poisoned");
        for page in guard.pages.iter().filter(|p| looks_like_body_metric(p)) {
            if let Ok(m) = parse_page(page) {
                if m.id == uuid {
                    return Ok(m);
                }
            }
        }
        Err(BodyError::NotFound(id.to_string()))
    }

    fn find_by_kind(&self, kind: &str) -> Result<BodyMetric, BodyError> {
        let needle = kind.trim().to_ascii_lowercase();
        if needle.is_empty() {
            return Err(BodyError::BadRequest("empty kind".into()));
        }
        let guard = self.inner.lock().expect("body store poisoned");
        for page in guard.pages.iter().filter(|p| looks_like_body_metric(p)) {
            if let Ok(m) = parse_page(page) {
                if m.kind.eq_ignore_ascii_case(&needle) {
                    return Ok(m);
                }
            }
        }
        Err(BodyError::NotFound(format!("kind: {kind}")))
    }

    fn create(&self, mut metric: BodyMetric) -> Result<BodyMetric, BodyError> {
        if metric.id.is_nil() {
            metric.id = Uuid::new_v4();
        }
        if metric.path.is_empty() {
            metric.path = default_metric_path(&metric.name, None);
        }
        let now = chrono::Utc::now();
        metric.date_created.get_or_insert(now);
        metric.date_modified = Some(now);
        let body = serialize_metric(&metric).map_err(map_io)?;
        let mut guard = self.inner.lock().expect("body store poisoned");
        if guard.pages.iter().any(|p| p.rel_path == metric.path) {
            return Err(BodyError::AlreadyExists(metric.path));
        }
        vault::create_page(&mut guard, &metric.path, body).map_err(map_io)?;
        Ok(metric)
    }

    fn update(&self, mut metric: BodyMetric) -> Result<BodyMetric, BodyError> {
        let mut guard = self.inner.lock().expect("body store poisoned");
        let idx = find_idx(&guard, metric.id)
            .ok_or_else(|| BodyError::NotFound(metric.id.to_string()))?;
        metric.path = guard.pages[idx].rel_path.clone();
        metric.date_modified = Some(chrono::Utc::now());
        let body = serialize_metric(&metric).map_err(map_io)?;
        guard.pages[idx].raw = body;
        let path = metric.path.clone();
        vault::save_page(&mut guard, &path).map_err(map_io)?;
        Ok(metric)
    }

    fn delete(&self, id: &str) -> Result<(), BodyError> {
        let uuid = Uuid::parse_str(id).map_err(|e| BodyError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("body store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| BodyError::NotFound(id.to_string()))?;
        let path = guard.pages[idx].rel_path.clone();
        vault::delete_page(&mut guard, &path).map_err(map_io)?;
        Ok(())
    }

    fn log_entry(&self, metric_id: &str, mut entry: BodyEntry) -> Result<BodyMetric, BodyError> {
        if entry.id.is_nil() {
            entry.id = Uuid::new_v4();
        }
        let mut metric = self.get(metric_id)?;
        metric.entries.push(entry);
        self.update(metric)
    }
}
