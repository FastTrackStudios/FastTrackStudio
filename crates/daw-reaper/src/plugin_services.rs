//! In-process DAW service registration for CLAP plugins.
//!
//! Builds the canonical [`daw_proto::service_set::daw_layers`] bundle
//! against the stateless `crate::Reaper` singleton and serves it as an
//! `architect::LayerRouter`. Used by `PluginHost` to create a local `Daw`
//! instance without SHM.

use architect::LayerRouter;
use daw_proto::service_set::daw_layers;

use crate::Reaper;

/// Create a `LayerRouter` with all REAPER DAW service implementations.
///
/// Same surface as the daw-bridge mount, returned as a handler for use
/// with `LocalCaller` (in-process). Call after REAPER API is
/// initialized (`PluginHost::init` handles this).
pub fn create_daw_handler() -> LayerRouter {
    crate::init_item_broadcaster();
    crate::init_tempo_map_broadcaster();
    daw_layers(Reaper).serve()
}
