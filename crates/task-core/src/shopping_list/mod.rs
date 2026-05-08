//! `ShoppingList` + `ShoppingListItem` — Mealie-style grocery lists.

pub mod item;
pub mod model;

// Re-export everything from the list module for the typical
// `task_core::shopping_list::ShoppingListApi` access pattern.
pub use model::Model as ShoppingList;
pub use model::*;

// Item-side aliases — keep `Item*` aliases so callers don't collide
// with the list's own `ActiveModel`, `Column`, `Entity`, `Model`.
pub use item::{
    ActiveModel as ItemActiveModel, Column as ItemColumn, Entity as ItemEntity, Model as Item,
    Model as ShoppingListItem, ShoppingListItemApi, ShoppingListItemApiCreate,
    ShoppingListItemApiList, ShoppingListItemApiUpdate, ShoppingListItemRepoClient,
    ShoppingListItemRepoDispatcher, ShoppingListItemRepoStorage,
};
