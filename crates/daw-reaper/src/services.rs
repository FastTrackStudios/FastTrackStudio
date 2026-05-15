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
//! Service tokens (`transport::Service`, …) are backend-agnostic
//! values; only `.provide(Reaper)` ties them to the REAPER singleton.
//! Bolt-ons with different backends mix in via [`architect::Layer::add`]
//! (any pre-mounted service implements [`architect::Bind`]):
//!
//! ```ignore
//! let router = services()
//!     .add(dock_host::layer(dock_host_backend))   // pre-bound, different backend
//!     .provide(Reaper);
//! ```
//!
//! Other backends (Pro Tools, Logic, headless mocks, …) ship their
//! own equivalent — there's no umbrella trait pretending the union is
//! a spec. Adding a service is a one-line edit here.

use architect::{Layer, layers};
use daw_proto::{
    action_registry, audio_engine, automation, batch, dawfile_service, ext_state, fx, fx_chains,
    fx_params, health, input, item, live_midi, marker, midi, plugin_loader, project, region,
    routing, screenset, take, tempo_map, toolbar, track, transport, window_geometry,
};

use crate::Reaper;

/// The full REAPER service surface as a deferred `Layer<Reaper>`.
/// Bind with `.provide(Reaper)` to get a [`architect::LayerRouter`].
pub fn services() -> Layer<Reaper> {
    layers![
        transport::Service,
        project::Service,
        marker::Service,
        region::Service,
        tempo_map::Service,
        audio_engine::Service,
        midi::Service,
        fx::Service,
        fx_chains::Service,
        fx_params::Service,
        track::Service,
        routing::Service,
        live_midi::Service,
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
        automation::Service,
        batch::Service,
    ]
}
