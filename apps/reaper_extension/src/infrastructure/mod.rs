//! Infrastructure code
//!
//! Infrastructure modules handle REAPER-specific setup and integration.

pub mod action_registry;
pub mod change_detection;
pub mod timer;
pub mod iroh_server;
pub mod menu;
pub mod tracing_config;
pub mod color_utils;
pub mod reactive_logger;
pub mod reactive_polling;
pub mod reactive_app_state;

pub use action_registry::ActionRegistry;
pub use change_detection::ChangeDetection;
pub use timer::{register_timer, TimerCallback};
pub use iroh_server::start_iroh_server;
pub use menu::register_extension_menu;
pub use tracing_config::init_tracing;
pub use color_utils::{pack_rgb_to_u32, unpack_u32_to_rgb, extract_color_from_native, packed_to_hex, get_color_name_and_hex};

