// architect's Entity derive emits cfg-gated blocks; allow
// at crate scope.
#![allow(unexpected_cfgs)]

//! Wire contract for the inbox feature.
//!
//! The inbox is the single trusted capture queue of the FLAP system
//! (capture → process → write). This proto owns the **capture** atom:
//! [`InboxItem`] plus the [`service::Inbox`] CRUD surface the capture
//! UIs (CLI quick-add, web quick-add) and the daily-review page bind
//! to.
//!
//! Items live in the vault as plain markdown (`Records/inbox/<id>.md`);
//! the sibling `inbox` crate owns the parse / write side and the
//! disk-backed backend, exactly like `scheduling` sits on top of
//! `scheduling-proto`.

pub mod error;
pub mod inbox_item;
pub mod schedule;
pub mod service;

pub use error::InboxError;
pub use inbox_item::InboxItem;
pub use schedule::{ReviewResponse, review, schedule};
pub use service::Inbox;

// architect-emitted vox bits: the async client / dispatcher /
// descriptor for the one capability. Mount sites stitch the
// descriptor + `serve` into the org router.
#[cfg(feature = "vox")]
pub use service::inbox::{
    InboxClient, InboxRpc, InboxRpcDispatcher, Service as InboxService, layer as inbox_layer,
    serve as inbox_serve,
};
