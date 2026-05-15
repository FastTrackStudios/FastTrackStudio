//! REAPER service registration.
//!
//! The canonical list of services this DAW backend exposes, built as
//! a deferred [`architect::Layer`] cons-chain. **No where clause** —
//! adding a service is one line in the [`architect::services!`] macro.
//!
//! ```ignore
//! use daw_reaper::{services, Reaper};
//!
//! let router = services().provide(Reaper);
//! // or, inline at any call site:
//! let router = architect::services![transport, project, marker].provide(Reaper);
//! ```
//!
//! The return type is `impl Layer + ProvideAll<Reaper>` — the bundle
//! reads as a list of service names; the return signature declares
//! "this function produces a layer that knows how to provide for
//! `Reaper`." If `Reaper` ever stops implementing a service's trait,
//! the error surfaces at this function (the cons type's
//! `ProvideAll<Reaper>` bound fails), naming the missing trait.
//!
//! Other backends (Pro Tools, Logic, a headless mock, …) ship their
//! own `services()` returning `impl Layer + ProvideAll<TheirBackend>`.
//! No central spec. Adding a service is a one-line edit in the
//! macro list.

use architect::{Layer, ProvideAll, services};
use daw_proto::{
    action_registry, audio_engine, automation, batch, dawfile_service, ext_state, fx, fx_chains,
    fx_params, health, input, item, live_midi, marker, midi, plugin_loader, project, region,
    routing, screenset, take, tempo_map, toolbar, track, transport, window_geometry,
};

use crate::Reaper;

/// The full REAPER service surface as a deferred layer. Bind with
/// `.provide(Reaper)` to get a [`architect::LayerRouter`].
pub fn services() -> impl Layer + ProvideAll<Reaper> {
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
