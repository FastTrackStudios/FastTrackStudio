//! Rating service — CRUD operations for preset ratings and reviews.
//!
//! Provides:
//! - `rate_preset` — create or update a user's rating for a preset
//! - `get_ratings` — fetch all ratings for a preset
//! - `get_average_rating` — compute the average score for a preset
//! - Self-rating prevention: users cannot rate their own presets

use chrono::Utc;
use sea_orm::*;
use uuid::Uuid;

use crate::entities::{preset, rating};
use crate::error::{StorageError, StorageResult};

// region: --- DTOs

/// A rating with its associated metadata, returned by query methods.
#[derive(Debug, Clone)]
pub struct RatingInfo {
    pub id: Uuid,
    pub user_id: Uuid,
    pub preset_id: Uuid,
    pub score: i16,
    pub review: Option<String>,
    pub created_at: chrono::DateTime<Utc>,
    pub updated_at: chrono::DateTime<Utc>,
}

impl From<rating::Model> for RatingInfo {
    fn from(m: rating::Model) -> Self {
        Self {
            id: m.id,
            user_id: m.user_id,
            preset_id: m.preset_id,
            score: m.score,
            review: m.review,
            created_at: m.created_at.into(),
            updated_at: m.updated_at.into(),
        }
    }
}

/// Aggregated rating statistics for a preset.
#[derive(Debug, Clone, Copy)]
pub struct RatingStats {
    pub average: f64,
    pub count: u64,
}

// endregion: --- DTOs

// region: --- Service

/// Create or update a rating for a preset.
///
/// Enforces:
/// - Score must be 1–5 (returns `BusinessRule` error otherwise)
/// - Users cannot rate their own presets (checks `preset.author_id`)
/// - One rating per user per preset (upserts on conflict)
pub async fn rate_preset(
    db: &DatabaseConnection,
    user_id: Uuid,
    preset_id: Uuid,
    score: i16,
    review: Option<String>,
) -> StorageResult<RatingInfo> {
    // -- Validate score range
    if !(1..=5).contains(&score) {
        return Err(StorageError::BusinessRule(format!(
            "score must be 1-5, got {score}"
        )));
    }

    // -- Prevent self-rating: look up the preset's author
    let preset_model =
        preset::Entity::find_by_id(preset_id)
            .one(db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "preset",
                id: preset_id,
            })?;

    if preset_model.author_id == Some(user_id) {
        return Err(StorageError::BusinessRule(
            "cannot rate your own preset".into(),
        ));
    }

    let now = Utc::now().fixed_offset();

    // -- Check for existing rating (upsert)
    let existing = rating::Entity::find()
        .filter(rating::Column::UserId.eq(user_id))
        .filter(rating::Column::PresetId.eq(preset_id))
        .one(db)
        .await?;

    let model = if let Some(existing) = existing {
        // Update existing rating
        let mut active: rating::ActiveModel = existing.into();
        active.score = Set(score);
        active.review = Set(review);
        active.updated_at = Set(now);
        active.update(db).await?
    } else {
        // Insert new rating
        let active = rating::ActiveModel {
            id: Set(Uuid::new_v4()),
            user_id: Set(user_id),
            preset_id: Set(preset_id),
            score: Set(score),
            review: Set(review),
            created_at: Set(now),
            updated_at: Set(now),
        };
        active.insert(db).await?
    };

    Ok(model.into())
}

/// Fetch all ratings for a preset, ordered by creation date (newest first).
pub async fn get_ratings(
    db: &DatabaseConnection,
    preset_id: Uuid,
) -> StorageResult<Vec<RatingInfo>> {
    let models = rating::Entity::find()
        .filter(rating::Column::PresetId.eq(preset_id))
        .order_by_desc(rating::Column::CreatedAt)
        .all(db)
        .await?;

    Ok(models.into_iter().map(RatingInfo::from).collect())
}

/// Compute average rating and count for a preset.
///
/// Returns `RatingStats { average: 0.0, count: 0 }` if no ratings exist.
pub async fn get_average_rating(
    db: &DatabaseConnection,
    preset_id: Uuid,
) -> StorageResult<RatingStats> {
    let ratings = rating::Entity::find()
        .filter(rating::Column::PresetId.eq(preset_id))
        .all(db)
        .await?;

    if ratings.is_empty() {
        return Ok(RatingStats {
            average: 0.0,
            count: 0,
        });
    }

    let count = ratings.len() as u64;
    let sum: f64 = ratings.iter().map(|r| r.score as f64).sum();

    Ok(RatingStats {
        average: sum / count as f64,
        count,
    })
}

/// Delete a user's rating for a preset.
///
/// Returns `true` if a rating was deleted, `false` if none existed.
pub async fn delete_rating(
    db: &DatabaseConnection,
    user_id: Uuid,
    preset_id: Uuid,
) -> StorageResult<bool> {
    let result = rating::Entity::delete_many()
        .filter(rating::Column::UserId.eq(user_id))
        .filter(rating::Column::PresetId.eq(preset_id))
        .exec(db)
        .await?;

    Ok(result.rows_affected > 0)
}

// endregion: --- Service

// region: --- Tests

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entities::user;
    use crate::migration::Migrator;
    use sea_orm_migration::MigratorTrait;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    async fn setup_db() -> Result<DatabaseConnection> {
        let db = Database::connect("sqlite::memory:").await?;
        Migrator::up(&db, None).await?;
        Ok(db)
    }

    async fn create_user(db: &DatabaseConnection, username: &str) -> Result<Uuid> {
        let id = Uuid::new_v4();
        let now = Utc::now().fixed_offset();
        user::ActiveModel {
            id: Set(id),
            username: Set(username.to_string()),
            email: Set(format!("{username}@test.com")),
            display_name: Set(None),
            avatar_url: Set(None),
            created_at: Set(now),
            updated_at: Set(now),
        }
        .insert(db)
        .await?;
        Ok(id)
    }

    async fn create_preset(
        db: &DatabaseConnection,
        name: &str,
        author_id: Option<Uuid>,
    ) -> Result<Uuid> {
        let id = Uuid::new_v4();
        let now = Utc::now().fixed_offset();
        preset::ActiveModel {
            id: Set(id),
            name: Set(name.to_string()),
            description: Set(None),
            author_id: Set(author_id),
            category: Set(serde_json::json!({"base_tone": "Clean"})),
            tags: Set(serde_json::json!([])),
            data: Set(serde_json::json!({})),
            is_public: Set(true),
            is_deleted: Set(false),
            is_favorite: Set(false),
            version: Set(1),
            created_at: Set(now),
            updated_at: Set(now),
        }
        .insert(db)
        .await?;
        Ok(id)
    }

    // -- Exec & Check: rate_preset

    #[tokio::test]
    async fn rate_preset_creates_rating() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let author = create_user(&db, "author").await?;
        let reviewer = create_user(&db, "reviewer").await?;
        let preset_id = create_preset(&db, "Clean Tone", Some(author)).await?;

        // -- Exec
        let rating = rate_preset(&db, reviewer, preset_id, 4, Some("Great!".into())).await?;

        // -- Check
        assert_eq!(rating.score, 4);
        assert_eq!(rating.review.as_deref(), Some("Great!"));
        assert_eq!(rating.user_id, reviewer);
        assert_eq!(rating.preset_id, preset_id);
        Ok(())
    }

    #[tokio::test]
    async fn rate_preset_upserts_on_duplicate() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let author = create_user(&db, "author").await?;
        let reviewer = create_user(&db, "reviewer").await?;
        let preset_id = create_preset(&db, "Drive", Some(author)).await?;

        // -- Exec
        rate_preset(&db, reviewer, preset_id, 3, None).await?;
        let updated = rate_preset(&db, reviewer, preset_id, 5, Some("Changed mind".into())).await?;

        // -- Check
        assert_eq!(updated.score, 5);
        assert_eq!(updated.review.as_deref(), Some("Changed mind"));
        let all = get_ratings(&db, preset_id).await?;
        assert_eq!(all.len(), 1, "should upsert, not duplicate");
        Ok(())
    }

    #[tokio::test]
    async fn rate_preset_rejects_invalid_score() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let author = create_user(&db, "author").await?;
        let reviewer = create_user(&db, "reviewer").await?;
        let preset_id = create_preset(&db, "Fuzz", Some(author)).await?;

        // -- Exec & Check
        let err = rate_preset(&db, reviewer, preset_id, 0, None).await;
        assert!(err.is_err());
        assert!(err.unwrap_err().to_string().contains("score must be 1-5"));

        let err = rate_preset(&db, reviewer, preset_id, 6, None).await;
        assert!(err.is_err());
        assert!(err.unwrap_err().to_string().contains("score must be 1-5"));
        Ok(())
    }

    #[tokio::test]
    async fn rate_preset_prevents_self_rating() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let author = create_user(&db, "author").await?;
        let preset_id = create_preset(&db, "My Preset", Some(author)).await?;

        // -- Exec & Check
        let err = rate_preset(&db, author, preset_id, 5, None).await;
        assert!(err.is_err());
        assert!(err
            .unwrap_err()
            .to_string()
            .contains("cannot rate your own preset"));
        Ok(())
    }

    #[tokio::test]
    async fn rate_preset_allows_unowned() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let reviewer = create_user(&db, "reviewer").await?;
        let preset_id = create_preset(&db, "Community Preset", None).await?;

        // -- Exec
        let rating = rate_preset(&db, reviewer, preset_id, 3, None).await?;

        // -- Check
        assert_eq!(rating.score, 3);
        Ok(())
    }

    #[tokio::test]
    async fn rate_preset_not_found() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let reviewer = create_user(&db, "reviewer").await?;
        let missing = Uuid::new_v4();

        // -- Exec & Check
        let err = rate_preset(&db, reviewer, missing, 3, None).await;
        assert!(err.is_err());
        assert!(err.unwrap_err().to_string().contains("not found"));
        Ok(())
    }

    // -- Exec & Check: get_ratings

    #[tokio::test]
    async fn get_ratings_returns_all_for_preset() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let author = create_user(&db, "author").await?;
        let r1 = create_user(&db, "reviewer1").await?;
        let r2 = create_user(&db, "reviewer2").await?;
        let preset_id = create_preset(&db, "Popular", Some(author)).await?;

        rate_preset(&db, r1, preset_id, 4, Some("Nice".into())).await?;
        rate_preset(&db, r2, preset_id, 5, None).await?;

        // -- Exec
        let ratings = get_ratings(&db, preset_id).await?;

        // -- Check
        assert_eq!(ratings.len(), 2);
        Ok(())
    }

    #[tokio::test]
    async fn get_ratings_empty_preset() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let author = create_user(&db, "author").await?;
        let preset_id = create_preset(&db, "Unrated", Some(author)).await?;

        // -- Exec
        let ratings = get_ratings(&db, preset_id).await?;

        // -- Check
        assert!(ratings.is_empty());
        Ok(())
    }

    // -- Exec & Check: get_average_rating

    #[tokio::test]
    async fn get_average_rating_computes_correctly() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let author = create_user(&db, "author").await?;
        let r1 = create_user(&db, "r1").await?;
        let r2 = create_user(&db, "r2").await?;
        let r3 = create_user(&db, "r3").await?;
        let preset_id = create_preset(&db, "Mixed", Some(author)).await?;

        rate_preset(&db, r1, preset_id, 3, None).await?;
        rate_preset(&db, r2, preset_id, 4, None).await?;
        rate_preset(&db, r3, preset_id, 5, None).await?;

        // -- Exec
        let stats = get_average_rating(&db, preset_id).await?;

        // -- Check
        assert!((stats.average - 4.0).abs() < 0.01);
        assert_eq!(stats.count, 3);
        Ok(())
    }

    #[tokio::test]
    async fn get_average_rating_zero_when_none() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let author = create_user(&db, "author").await?;
        let preset_id = create_preset(&db, "New", Some(author)).await?;

        // -- Exec
        let stats = get_average_rating(&db, preset_id).await?;

        // -- Check
        assert!((stats.average - 0.0).abs() < f64::EPSILON);
        assert_eq!(stats.count, 0);
        Ok(())
    }

    // -- Exec & Check: delete_rating

    #[tokio::test]
    async fn delete_rating_removes_existing() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let author = create_user(&db, "author").await?;
        let reviewer = create_user(&db, "reviewer").await?;
        let preset_id = create_preset(&db, "Test", Some(author)).await?;
        rate_preset(&db, reviewer, preset_id, 4, None).await?;

        // -- Exec
        let deleted = delete_rating(&db, reviewer, preset_id).await?;

        // -- Check
        assert!(deleted);
        let ratings = get_ratings(&db, preset_id).await?;
        assert!(ratings.is_empty());
        Ok(())
    }

    #[tokio::test]
    async fn delete_rating_returns_false_when_none() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let reviewer = create_user(&db, "reviewer").await?;
        let author = create_user(&db, "author").await?;
        let preset_id = create_preset(&db, "Test", Some(author)).await?;

        // -- Exec
        let deleted = delete_rating(&db, reviewer, preset_id).await?;

        // -- Check
        assert!(!deleted);
        Ok(())
    }
}

// endregion: --- Tests
