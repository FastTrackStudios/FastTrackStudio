//! REAPER service registration — every architect-rpc service this
//! backend implements, bundled as an `architect::Layer<Reaper>`.
//!
//! This is the canonical list of REAPER's surface. Mount it with one
//! call, binding the backend at the very end:
//!
//! ```ignore
//! use daw_reaper::{services, Reaper};
//!
//! let router = services::all().provide(Reaper);
//! ```
//!
//! Service tokens (`transport::Service`, `project::Service`, …) are
//! backend-agnostic values; only `.provide(Reaper)` ties them to the
//! REAPER singleton. Bolt-ons with different backends mix in via
//! `Layer::add` (any pre-mounted service implements `Bind`):
//!
//! ```ignore
//! let router = services::all()
//!     .add(dock_host::layer(dock_host_backend))   // pre-bound, different backend
//!     .provide(Reaper);
//! ```
//!
//! Other backends (Pro Tools, Logic, headless mocks, …) ship their
//! own equivalent — there's no umbrella trait pretending the union is
//! a spec. Adding a service is a one-line edit here. Trait impls
//! missing on `Reaper` fail compilation at the matching `.add` call —
//! one place to look.

use architect::Layer;
use daw_proto::{
    action_registry, audio_engine, automation, batch, dawfile_service, ext_state, fx, fx_chains,
    fx_params, health, input, item, live_midi, marker, midi, plugin_loader, project, region,
    routing, screenset, take, tempo_map, toolbar, track, transport, window_geometry,
};

use crate::Reaper;

/// The full REAPER service surface as a deferred `Layer<Reaper>`.
/// Bind with `.provide(Reaper)` to get a `LayerRouter`.
pub fn all() -> Layer<Reaper> {
    Layer::new()
        .add(transport::Service)
        .add(project::Service)
        .add(marker::Service)
        .add(region::Service)
        .add(tempo_map::Service)
        .add(audio_engine::Service)
        .add(midi::Service)
        .add(fx::Service)
        .add(fx_chains::Service)
        .add(fx_params::Service)
        .add(track::Service)
        .add(routing::Service)
        .add(live_midi::Service)
        .add(ext_state::Service)
        .add(health::Service)
        .add(item::Service)
        .add(take::Service)
        .add(action_registry::Service)
        .add(input::Service)
        .add(toolbar::Service)
        .add(screenset::Service)
        .add(dawfile_service::Service)
        .add(window_geometry::Service)
        .add(plugin_loader::Service)
        .add(automation::Service)
        .add(batch::Service)
}
