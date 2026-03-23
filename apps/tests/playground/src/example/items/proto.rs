//! Items proto — domain types and service trait

use crate::example::auth::proto::SessionToken;
use facet::Facet;
use vox::Tx;

#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ItemInfo {
    pub id: String,
    pub name: String,
    pub done: bool,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Facet)]
pub enum ItemEvent {
    ListChanged { items: Vec<ItemInfo> },
}

#[vox::service]
pub trait ItemService {
    async fn list_items(&self, token: SessionToken) -> Vec<ItemInfo>;
    async fn create_item(&self, token: SessionToken, name: String) -> String;
    async fn toggle_item(&self, token: SessionToken, id: String);
    async fn delete_item(&self, token: SessionToken, id: String);
    async fn subscribe(&self, token: SessionToken, tx: Tx<ItemEvent>);
}
