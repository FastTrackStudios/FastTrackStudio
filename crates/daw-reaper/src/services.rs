//! REAPER service surface — declared as [`architect::Services`].
//!
//! `impl Services for Reaper` says "Reaper provides this canonical
//! bundle." Same pattern as every per-service trait impl (`impl
//! Transport for Reaper`, `impl Markers for Reaper`, …), now lifted
//! one level to "impl `Services` for Reaper exposes the whole
//! surface."
//!
//! ```ignore
//! use architect::Services;
//! use daw_reaper::Reaper;
//!
//! // Default mount:
//! let router = Reaper.into_router();
//!
//! // With overrides — last add wins on duplicate method_id:
//! let router = Reaper::layers()
//!     .add(fx_chains_mock::mock())              // overrides default fx_chains
//!     .add(dock_host::layer(dock_host))         // bolt-on
//!     .provide(Reaper);
//!
//! // Sub-bundles:
//! let timeline = architect::services![transport, marker, region];
//! let routing  = architect::services![project, routing, track];
//! let router   = timeline.merge(routing).provide(Reaper);
//! ```
//!
//! Adding a service is one line in the [`architect::services!`] list.
//! No bounds, no where clause. Other backends (Pro Tools, Logic, mock)
//! each implement [`architect::Services`] with their own bundle.

use architect::{Layer, ProvideAll, Services, services};
use daw_proto::{
    action_registry, audio_engine, automation, batch, dawfile_service, ext_state, fx, fx_chains,
    fx_params, health, input, item, live_midi, marker, midi, plugin_loader, project, region,
    routing, screenset, take, tempo_map, toolbar, track, transport, window_geometry,
};

use crate::Reaper;

impl Services for Reaper {
    fn layers() -> impl Layer + ProvideAll<Reaper> {
        services![
            transport,
            project,
            marker,
            region,
            tempo_map,
            audio_engine,
            midi,
            fx,
            fx_chains,
            fx_params,
            track,
            routing,
            live_midi,
            ext_state,
            health,
            item,
            take,
            action_registry,
            input,
            toolbar,
            screenset,
            dawfile_service,
            window_geometry,
            plugin_loader,
            automation,
            batch,
        ]
    }
}
