//! Facade for the `notifications` feature.
//!
//! - Default surface: wire types (`Notification`, `NotificationChannel`,
//!   `NotificationRule`) re-exported from `notifications-proto`.
//! - `server` feature: adds the [`NotificationRouter`] + delivery
//!   pipeline that subscribes to feature buses (start with
//!   `agent::LiveUpdateBus`) and produces `Notification` rows.

pub use notifications_proto::*;

#[cfg(feature = "server")]
pub mod router;
#[cfg(feature = "server")]
pub use router::{
    DefaultRules, DeliveredNotification, NotificationRouter, ToastBus, ToastSubscription,
};
