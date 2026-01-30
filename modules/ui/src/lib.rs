pub mod bridge;
pub mod components;
pub mod context;
pub mod hooks;
pub mod reactive_state;

// Re-export commonly used components
pub use components::*;
pub use reactive_state::*;

// Re-export bridge utilities
// TODO: Re-enable when rig-control roam service issues are fixed
// pub use bridge::block_type_to_category;

// Re-export context and hooks
// TODO: Re-enable rig-control when roam service issues are fixed
// pub use context::{use_rig_service, use_setlist_service, RigServiceCtx, RigServiceProvider, SetlistServiceCtx, SetlistServiceProvider};
// pub use hooks::{use_rig_actions, use_rig_subscription, use_setlist_actions, use_setlist_subscription, RigActions, SetlistActions};
pub use context::{use_setlist_service, SetlistServiceCtx, SetlistServiceProvider};
pub use hooks::{use_setlist_actions, use_setlist_subscription, SetlistActions};
