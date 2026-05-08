//! SeaORM-backed [`AudioProductionService`] implementation.
//!
//! Cross-entity service: holds a raw `DatabaseConnection` because mix
//! submission touches both `tracks` and `attachments` in one operation.
//! Status transitions and revision bookkeeping are owned here so the
//! repo CRUD surface stays mechanical.

use sea_orm::{
    ActiveModelTrait, ColumnTrait, DatabaseConnection, EntityTrait, QueryFilter, QueryOrder, Set,
};
use uuid::Uuid;

use crate::attachment;
use crate::property::JsonObject;
use crate::service::{
    AddTrackRequest, AudioProductionService, SubmitMixRequest, TrackPatch, VaultError,
};
use crate::track::{self, TrackApi, TrackStatus};

use super::helpers::convert_model;

/// Typed dependencies for [`AudioProductionServiceImpl`].
pub struct AudioProductionServiceDeps {
    pub db: DatabaseConnection,
}

#[derive(Clone)]
pub struct AudioProductionServiceImpl {
    db: DatabaseConnection,
}

impl AudioProductionServiceImpl {
    pub fn new(deps: AudioProductionServiceDeps) -> Self {
        Self { db: deps.db }
    }

    async fn load_track(&self, id: Uuid) -> Result<track::Model, VaultError> {
        track::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|err| VaultError::IoError(format!("track lookup: {err}")))?
            .ok_or_else(|| VaultError::NotFound(format!("track:{id}")))
    }

    async fn next_sequence(&self, project_id: Uuid) -> Result<u32, VaultError> {
        let existing = track::Entity::find()
            .filter(track::Column::ProjectId.eq(project_id))
            .order_by_desc(track::Column::Sequence)
            .one(&self.db)
            .await
            .map_err(|err| VaultError::IoError(format!("sequence lookup: {err}")))?;
        Ok(existing.map(|t| t.sequence + 1).unwrap_or(1))
    }
}

fn track_to_api(model: track::Model) -> Result<TrackApi, VaultError> {
    convert_model::<track::Model, TrackApi>(model)
}

impl AudioProductionService for AudioProductionServiceImpl {
    async fn list_tracks(&self, project_id: Uuid) -> Result<Vec<TrackApi>, VaultError> {
        let rows = track::Entity::find()
            .filter(track::Column::ProjectId.eq(project_id))
            .order_by_asc(track::Column::Sequence)
            .all(&self.db)
            .await
            .map_err(|err| VaultError::IoError(format!("track list: {err}")))?;
        rows.into_iter().map(track_to_api).collect()
    }

    async fn get_track(&self, id: Uuid) -> Result<Option<TrackApi>, VaultError> {
        let row = track::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|err| VaultError::IoError(format!("track get: {err}")))?;
        match row {
            Some(model) => Ok(Some(track_to_api(model)?)),
            None => Ok(None),
        }
    }

    async fn add_track(&self, request: AddTrackRequest) -> Result<TrackApi, VaultError> {
        if request.title.trim().is_empty() {
            return Err(VaultError::ParseError("track title is empty".to_string()));
        }
        let sequence = match request.sequence {
            Some(s) if s > 0 => s,
            _ => self.next_sequence(request.project_id).await?,
        };
        let now = chrono::Utc::now();
        let active = track::ActiveModel {
            id: Set(Uuid::new_v4()),
            project_id: Set(request.project_id),
            title: Set(request.title),
            sequence: Set(sequence),
            status: Set(TrackStatus::default()),
            bpm: Set(request.bpm),
            key: Set(request.key),
            duration_ms: Set(None),
            time_signature: Set(None),
            daw_session_path: Set(None),
            stems_path: Set(None),
            reference_url: Set(None),
            isrc: Set(None),
            artist: Set(request.artist),
            notes: Set(None),
            revision_number: Set(0),
            created_by: Set(request.created_by),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let saved = active
            .insert(&self.db)
            .await
            .map_err(|err| VaultError::IoError(format!("track insert: {err}")))?;
        track_to_api(saved)
    }

    async fn update_track(&self, id: Uuid, patch: TrackPatch) -> Result<TrackApi, VaultError> {
        let model = self.load_track(id).await?;
        let mut active: track::ActiveModel = model.into();
        if let Some(title) = patch.title {
            active.title = Set(title);
        }
        if let Some(sequence) = patch.sequence {
            active.sequence = Set(sequence);
        }
        if let Some(bpm) = patch.bpm {
            active.bpm = Set(Some(bpm));
        }
        if let Some(key) = patch.key {
            active.key = Set(Some(key));
        }
        if let Some(duration_ms) = patch.duration_ms {
            active.duration_ms = Set(Some(duration_ms));
        }
        if let Some(ts) = patch.time_signature {
            active.time_signature = Set(Some(ts));
        }
        if let Some(p) = patch.daw_session_path {
            active.daw_session_path = Set(Some(p));
        }
        if let Some(p) = patch.stems_path {
            active.stems_path = Set(Some(p));
        }
        if let Some(p) = patch.reference_url {
            active.reference_url = Set(Some(p));
        }
        if let Some(isrc) = patch.isrc {
            active.isrc = Set(Some(isrc));
        }
        if let Some(artist) = patch.artist {
            active.artist = Set(Some(artist));
        }
        if let Some(notes) = patch.notes {
            active.notes = Set(Some(notes));
        }
        active.updated_at = Set(chrono::Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|err| VaultError::IoError(format!("track update: {err}")))?;
        track_to_api(saved)
    }

    async fn transition_status(
        &self,
        id: Uuid,
        new_status: String,
    ) -> Result<TrackApi, VaultError> {
        let status = TrackStatus::parse(&new_status)
            .ok_or_else(|| VaultError::ParseError(format!("unknown status: {new_status}")))?;
        let model = self.load_track(id).await?;
        let mut active: track::ActiveModel = model.into();
        active.status = Set(status);
        active.updated_at = Set(chrono::Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|err| VaultError::IoError(format!("track update: {err}")))?;
        track_to_api(saved)
    }

    async fn submit_mix(&self, request: SubmitMixRequest) -> Result<TrackApi, VaultError> {
        let model = self.load_track(request.track_id).await?;
        if model.status == TrackStatus::Approved {
            return Err(VaultError::ParseError("track already approved".to_string()));
        }
        let new_status = match model.status {
            TrackStatus::Mastering => TrackStatus::Mastering,
            _ => TrackStatus::Mixing,
        };
        let next_revision = model.revision_number + 1;
        let track_id = model.id;
        let mut active: track::ActiveModel = model.into();
        active.status = Set(new_status);
        active.revision_number = Set(next_revision);
        active.updated_at = Set(chrono::Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|err| VaultError::IoError(format!("track update: {err}")))?;

        // Record the mix-bounce attachment (metadata-only — bytes go via
        // AttachmentService::upload separately).
        if let Some(path) = request.mix_path.as_ref().filter(|p| !p.trim().is_empty()) {
            let now = chrono::Utc::now();
            let label = request
                .mix_label
                .clone()
                .or_else(|| request.notes.clone())
                .unwrap_or_else(|| attachment::label_from_path(path));
            let attachment_active = attachment::ActiveModel {
                id: Set(Uuid::new_v4()),
                owner_id: Set(track_id),
                owner_type: Set("track".to_string()),
                source: Set(attachment::DEFAULT_ATTACHMENT_SOURCE.to_string()),
                path: Set(path.clone()),
                label: Set(Some(label)),
                mime: Set(request.mix_mime.clone()),
                size_bytes: Set(None),
                checksum: Set(None),
                uploader: Set(request.uploader.clone()),
                created_at: Set(now),
                updated_at: Set(now),
            };
            attachment_active
                .insert(&self.db)
                .await
                .map_err(|err| VaultError::IoError(format!("attachment insert: {err}")))?;
        }

        track_to_api(saved)
    }

    async fn approve_mix(&self, id: Uuid, actor: String) -> Result<TrackApi, VaultError> {
        let model = self.load_track(id).await?;
        let mut props_value = model.properties.clone().into_inner();
        if !props_value.is_object() {
            props_value = serde_json::Value::Object(serde_json::Map::new());
        }
        if let Some(map) = props_value.as_object_mut() {
            map.insert(
                "approved_by".to_string(),
                serde_json::Value::String(actor.clone()),
            );
            map.insert(
                "approved_at".to_string(),
                serde_json::Value::String(chrono::Utc::now().to_rfc3339()),
            );
        }

        let mut active: track::ActiveModel = model.into();
        active.status = Set(TrackStatus::Approved);
        active.properties = Set(JsonObject::from_value(props_value));
        active.updated_at = Set(chrono::Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|err| VaultError::IoError(format!("track update: {err}")))?;
        track_to_api(saved)
    }
}
