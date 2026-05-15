//! In-process DAW service registration for CLAP plugins.
//!
//! Mounts [`crate::services`] onto an [`architect::LayerRouter`] so
//! `PluginHost` can build a local `Daw` instance without SHM.

use architect::{Layer, LayerRouter};

use crate::Reaper;

/// Create a `LayerRouter` with every REAPER service the DAW exposes.
///
/// Call after REAPER API is initialized (`PluginHost::init` handles
/// this).
pub fn create_daw_handler() -> LayerRouter {
    crate::init_item_broadcaster();
    crate::init_tempo_map_broadcaster();
    crate::services().provide(Reaper)
}
