//! Named bundles of architect-rpc services.
//!
//! [`daw_layers`] is the canonical "everything a DAW backend exposes"
//! [`architect::LayerSet`]. Build it from any backend that implements the
//! full trait surface; mount it onto a router that implements
//! [`architect::LayerRouter`].
//!
//! ```ignore
//! use architect::LayerSet;
//! use daw_proto::service_set::daw_layers;
//!
//! let handler = daw_layers(reaper)
//!     .merge(dock_host::layer(dock_host))   // bolt-on
//!     .mount(RoutedHandler::new());
//! ```
//!
//! Adding a new service to the DAW surface is a single edit here: add
//! the trait bound + one `.add(...)` call. Every consumer that mounts
//! the bundle picks up the new service automatically.

use architect::LayerSet;

use crate::{
    action_registry, audio_engine, automation, batch, dawfile_service, ext_state, fx, fx_chains,
    fx_params, health, input, item, live_midi, marker, midi, plugin_loader, project, region,
    routing, screenset, take, tempo_map, toolbar, track, transport, window_geometry,
};

/// The complete set of services every DAW backend exposes. Bundle these
/// onto a single backend value with [`daw_layers`].
pub trait DawBackend:
    transport::service::Transport
    + project::Projects
    + marker::Markers
    + region::Regions
    + tempo_map::TempoMap
    + audio_engine::AudioEngine
    + midi::Midi
    + fx::Effects
    + fx_chains::FxChains
    + fx_params::FxParams
    + track::Tracks
    + routing::Routing
    + live_midi::LiveMidi
    + ext_state::ExtState
    + health::Health
    + item::Items
    + take::Takes
    + action_registry::ActionRegistration
    + input::Input
    + toolbar::Toolbar
    + screenset::Screensets
    + dawfile_service::DawFileOps
    + window_geometry::WindowGeometry
    + plugin_loader::PluginLoading
    + automation::Automation
    + batch::BatchExecution
    + architect::HasDispatcher
    + Clone
    + Send
    + Sync
    + 'static
{
}

impl<T> DawBackend for T where
    T: transport::service::Transport
        + project::Projects
        + marker::Markers
        + region::Regions
        + tempo_map::TempoMap
        + audio_engine::AudioEngine
        + midi::Midi
        + fx::Effects
        + fx_chains::FxChains
        + fx_params::FxParams
        + track::Tracks
        + routing::Routing
        + live_midi::LiveMidi
        + ext_state::ExtState
        + health::Health
        + item::Items
        + take::Takes
        + action_registry::ActionRegistration
        + input::Input
        + toolbar::Toolbar
        + screenset::Screensets
        + dawfile_service::DawFileOps
        + window_geometry::WindowGeometry
        + plugin_loader::PluginLoading
        + automation::Automation
        + batch::BatchExecution
        + architect::HasDispatcher
        + Clone
        + Send
        + Sync
        + 'static
{
}

/// Build the canonical [`LayerSet`] for a DAW backend. Every service in
/// the [`DawBackend`] surface contributes one layer.
///
/// Forgetting a trait impl on the backend fails compilation here (and
/// only here) — one place to remember; one place to fix.
pub fn daw_layers<B: DawBackend>(backend: B) -> LayerSet {
    LayerSet::new()
        .add(transport::layer(backend.clone()))
        .add(project::layer(backend.clone()))
        .add(marker::layer(backend.clone()))
        .add(region::layer(backend.clone()))
        .add(tempo_map::layer(backend.clone()))
        .add(audio_engine::layer(backend.clone()))
        .add(midi::layer(backend.clone()))
        .add(fx::layer(backend.clone()))
        .add(fx_chains::layer(backend.clone()))
        .add(fx_params::layer(backend.clone()))
        .add(track::layer(backend.clone()))
        .add(routing::layer(backend.clone()))
        .add(live_midi::layer(backend.clone()))
        .add(ext_state::layer(backend.clone()))
        .add(health::layer(backend.clone()))
        .add(item::layer(backend.clone()))
        .add(take::layer(backend.clone()))
        .add(action_registry::layer(backend.clone()))
        .add(input::layer(backend.clone()))
        .add(toolbar::layer(backend.clone()))
        .add(screenset::layer(backend.clone()))
        .add(dawfile_service::layer(backend.clone()))
        .add(window_geometry::layer(backend.clone()))
        .add(plugin_loader::layer(backend.clone()))
        .add(automation::layer(backend.clone()))
        .add(batch::layer(backend))
}
