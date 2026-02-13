//! Items repo — concrete data access layer (SeaORM)
//!
//! All database queries for items live here. The service layer
//! calls these methods but never touches SeaORM directly.

use super::entity;
use sea_orm::*;

pub struct ItemRepo {
    db: DatabaseConnection,
}

impl ItemRepo {
    pub fn new(db: DatabaseConnection) -> Self {
        Self { db }
    }

    pub async fn find_by_owner(&self, owner_id: &str) -> Vec<entity::Model> {
        entity::Entity::find()
            .filter(entity::Column::OwnerId.eq(owner_id))
            .order_by_asc(entity::Column::Name)
            .all(&self.db)
            .await
            .unwrap_or_default()
    }

    pub async fn insert(&self, id: String, owner_id: String, name: String) -> Result<(), DbErr> {
        let item = entity::ActiveModel {
            id: Set(id),
            owner_id: Set(owner_id),
            name: Set(name),
            done: Set(false),
        };
        entity::Entity::insert(item).exec(&self.db).await?;
        Ok(())
    }

    pub async fn toggle(&self, id: &str, owner_id: &str) -> bool {
        let Ok(Some(item)) = entity::Entity::find_by_id(id).one(&self.db).await else {
            return false;
        };
        if item.owner_id != owner_id {
            return false;
        }
        let mut active: entity::ActiveModel = item.into();
        let current = active.done.clone().unwrap();
        active.done = Set(!current);
        active.update(&self.db).await.is_ok()
    }

    pub async fn delete(&self, id: &str, owner_id: &str) -> bool {
        let result = entity::Entity::delete_many()
            .filter(entity::Column::Id.eq(id))
            .filter(entity::Column::OwnerId.eq(owner_id))
            .exec(&self.db)
            .await;
        matches!(result, Ok(r) if r.rows_affected > 0)
    }
}
