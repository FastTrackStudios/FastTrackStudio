//! In-process DAW service registration for CLAP plugins.
//!
//! Mounts [`crate::services::all`] onto an [`architect::LayerRouter`]
//! so `PluginHost` can build a local `Daw` instance without SHM.

use architect::LayerRouter;

use crate::{Reaper, services};

/// Create a `LayerRouter` with every REAPER service the DAW exposes.
/// Same surface as the daw-bridge mount, returned as a handler for use
/// with `LocalCaller` (in-process).
///
/// Call after REAPER API is initialized (`PluginHost::init` handles
/// this).
pub fn create_daw_handler() -> LayerRouter {
    crate::init_item_broadcaster();
    crate::init_tempo_map_broadcaster();
    services::all(Reaper).serve()
}
