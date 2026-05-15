//! REAPER service registration.
//!
//! The canonical list of services this DAW backend exposes, built as
//! a deferred `architect::Layer<B>` where `B` is inferred at
//! `.provide()` time. Bind the backend at the very end:
//!
//! ```ignore
//! use daw_reaper::{services, Reaper};
//!
//! let router = services().provide(Reaper);
//! ```
//!
//! The function is generic over its backend type — the same
//! `services()` call works against any backend that implements every
//! service trait in the where clause below (REAPER, a hypothetical
//! full-surface mock, …). If a backend forgets a trait impl, the
//! error surfaces at the `.provide(...)` call site, naming the
//! missing trait.
//!
//! Service tokens are backend-agnostic; only `.provide(B)` ties them
//! to a concrete backend. Bolt-ons with different backends mix in via
//! [`architect::Layer::add`] — any pre-mounted service implements
//! [`architect::Bind`]:
//!
//! ```ignore
//! let router = services()
//!     .add(dock_host::layer(dock_host_backend))   // different backend
//!     .provide(Reaper);
//! ```
//!
//! Other backends (Pro Tools, Logic, headless mocks, …) ship their
//! own equivalent `services()` with whatever subset of bounds they
//! satisfy. No umbrella trait — the where clause is the spec.

use architect::{HasDispatcher, Layer, services};
use daw_proto::{
    action_registry, audio_engine, automation, batch, dawfile_service, ext_state, fx, fx_chains,
    fx_params, health, input, item, live_midi, marker, midi, plugin_loader, project, region,
    routing, screenset, take, tempo_map, toolbar, track, transport, window_geometry,
};

/// The full REAPER service surface as a deferred `Layer<B>`. Bind
/// with `.provide(backend)` to get a [`architect::LayerRouter`].
///
/// Generic over `B` so the same bundle can be reused across REAPER
/// and any other backend that implements the full set of traits in
/// the where clause. Forgetting a trait impl fails compile at the
/// `.provide(...)` call site with `the trait <X> is not implemented
/// for <backend>` — one place to look.
pub fn services<B>() -> Layer<B>
where
    B: transport::service::Transport
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
        + HasDispatcher
        + Clone
        + Send
        + Sync
        + 'static,
{
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
