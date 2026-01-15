//! Audio processing pipeline
//!
//! This module handles all audio playback and processing logic.

mod click_player;
mod count_player;
mod guide_player;
mod routing;
mod trigger_scheduler;

pub use click_player::{ClickPlayer, ClickPlayerState};
pub use count_player::{CountPlayer, CountPlayerState};
pub use guide_player::{GuidePlayer, GuidePlayerState};
pub use routing::AudioRouter;
pub use trigger_scheduler::{SubdivisionIntervals, TriggerScheduler, TriggerSchedulingParams};
