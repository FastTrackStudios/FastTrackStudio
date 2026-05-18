//! Service surface for `Standalone` — declared as [`architect::Services`].
//!
//! Mirrors `daw-reaper`'s [`Reaper`](daw_reaper::Reaper) services bundle so an
//! in-process `Daw` client can speak to Standalone exactly like it does to
//! REAPER. Only domains with real (non-stub) impls are listed; `midi`, `fx`,
//! `live_midi`, `automation`, `batch` are intentionally omitted until Standalone
//! grows real implementations (calls into those domains via the RPC client
//! will fail with a routing error — same as any missing service).
//!
//! ```ignore
//! use architect::Services;
//! use daw_standalone::Standalone;
//!
//! let router = Standalone::new().into_router();
//! ```

use architect::{Layer, Services, layers};
use daw_proto::{
    action_registry, audio_engine, dawfile_service, event_bus, ext_state, fx_chains, fx_params,
    health, input, item, marker, plugin_loader, project, region, routing, screenset, take,
    tempo_map, toolbar, track, transport, window_geometry,
};

use crate::sync::Standalone;

impl Services for Standalone {
    fn layers() -> impl Layer<Standalone> {
        layers![
            transport::Service,
            project::Service,
            marker::Service,
            region::Service,
            tempo_map::Service,
            audio_engine::Service,
            fx_chains::Service,
            fx_params::Service,
            track::Service,
            routing::Service,
            ext_state::Service,
            health::Service,
            item::Service,
            take::Service,
            action_registry::Service,
            input::Service,
            toolbar::Service,
            screenset::Service,
            dawfile_service::Service,
            window_geometry::Service,
            plugin_loader::Service,
            event_bus::Service,
        ]
    }
}
