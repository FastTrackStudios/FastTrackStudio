//! Facade — single import root for the `inventory` feature.
//!
//! Wire types from `inventory-proto` are always re-exported. The
//! `server` feature adds CRDT source-of-truth + SeaORM persistence,
//! and provides [`server::InventoryServiceImpl`] which implements the
//! `InventoryService` trait against [`InventoryItemRepoLoro`] +
//! [`CheckoutEventRepoLoro`].

pub use inventory_proto::*;

#[cfg(feature = "server")]
pub mod server {
    use chrono::{DateTime, Utc};
    use inventory_proto::{
        CheckoutEvent, CheckoutEventCreate, CheckoutEventRepo, CheckoutEventUpdate,
        InventoryItemRepo, InventoryItemUpdate, InventoryService, InventoryServiceError,
    };
    use uuid::Uuid;

    pub use crdt::{CrdtDoc, Persistence};
    pub use inventory_crdt::{
        CheckoutEventEntity, CheckoutEventRepoLoro, InventoryItemEntity, InventoryItemRepoLoro,
    };
    pub use inventory_db::{InventoryMigrator, SeaOrmPersistence};

    /// Default service implementation wired against the Loro repos.
    #[derive(Clone)]
    pub struct InventoryServiceImpl {
        items: InventoryItemRepoLoro,
        events: CheckoutEventRepoLoro,
    }

    impl InventoryServiceImpl {
        pub fn new(items: InventoryItemRepoLoro, events: CheckoutEventRepoLoro) -> Self {
            Self { items, events }
        }
    }

    impl InventoryService for InventoryServiceImpl {
        async fn checkout(
            &self,
            item_id: Uuid,
            person_id: Uuid,
            due_at: Option<DateTime<Utc>>,
            note: Option<String>,
        ) -> Result<CheckoutEvent, InventoryServiceError> {
            // Make sure the item exists.
            let _item = self
                .items
                .get(item_id)
                .await
                .map_err(|_| InventoryServiceError::NotFound)?;

            // Record the event.
            let ev = self
                .events
                .create(CheckoutEventCreate {
                    item_id,
                    person_id: Some(person_id),
                    checked_out_at: Utc::now(),
                    due_at,
                    returned_at: None,
                    note,
                })
                .await
                .map_err(|e| InventoryServiceError::Internal(e.to_string()))?;

            // Flip the item's status + owner.
            self.items
                .update(
                    item_id,
                    InventoryItemUpdate {
                        status: Some("checked-out".into()),
                        owner_id: Some(Some(person_id)),
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| InventoryServiceError::Internal(e.to_string()))?;

            Ok(ev)
        }

        async fn checkin(
            &self,
            item_id: Uuid,
            note: Option<String>,
        ) -> Result<CheckoutEvent, InventoryServiceError> {
            // Find the most recent open event for the item.
            let page = architect::Page {
                index: 0,
                size: 500,
            };
            let list = self
                .events
                .list(page, None, None)
                .await
                .map_err(|e| InventoryServiceError::Internal(e.to_string()))?;

            let mut open: Vec<CheckoutEvent> = list
                .items
                .into_iter()
                .filter(|e| e.item_id == item_id && e.returned_at.is_none())
                .collect();
            open.sort_by(|a, b| b.checked_out_at.cmp(&a.checked_out_at));
            let target = open
                .into_iter()
                .next()
                .ok_or(InventoryServiceError::NotFound)?;

            // Close the event.
            let updated = self
                .events
                .update(
                    target.id,
                    CheckoutEventUpdate {
                        returned_at: Some(Some(Utc::now())),
                        note: note.map(Some),
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| InventoryServiceError::Internal(e.to_string()))?;

            // Flip the item back to active and clear the owner.
            self.items
                .update(
                    item_id,
                    InventoryItemUpdate {
                        status: Some("active".into()),
                        owner_id: Some(None),
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| InventoryServiceError::Internal(e.to_string()))?;

            Ok(updated)
        }
    }
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
