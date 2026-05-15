//! REAPER service registration.
//!
//! The canonical list of services this DAW backend exposes, built as
//! a deferred [`architect::Layer<Reaper>`]. Bind the backend at the
//! very end with [`architect::Layer::provide`]:
//!
//! ```ignore
//! use daw_reaper::{services, Reaper};
//!
//! let router = services().provide(Reaper);
//! ```
//!
//! Service tokens are backend-agnostic; only `.provide(Reaper)` ties
//! them to the REAPER singleton. Bolt-ons with different backends
//! mix in via [`architect::Layer::add`] — any pre-mounted service
//! implements [`architect::Bind`]:
//!
//! ```ignore
//! let router = services()
//!     .add(dock_host::layer(dock_host_backend))   // different backend
//!     .provide(Reaper);
//! ```
//!
//! Other backends (Pro Tools, Logic, headless mocks, …) ship their
//! own equivalent — there's no umbrella trait pretending the union
//! is a spec. Adding a service is a one-line edit here.

use architect::{Layer, services};
use daw_proto::{
    action_registry, audio_engine, automation, batch, dawfile_service, ext_state, fx, fx_chains,
    fx_params, health, input, item, live_midi, marker, midi, plugin_loader, project, region,
    routing, screenset, take, tempo_map, toolbar, track, transport, window_geometry,
};

use crate::Reaper;

/// The full REAPER service surface as a deferred `Layer<Reaper>`.
/// Bind with `.provide(Reaper)` to get a [`architect::LayerRouter`].
pub fn services() -> Layer<Reaper> {
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
