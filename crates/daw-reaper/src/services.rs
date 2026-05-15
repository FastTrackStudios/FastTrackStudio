//! REAPER service registration — every architect-rpc service this
//! backend implements, bundled as an `architect::LayerSet`.
//!
//! This is the canonical list of REAPER's surface. Mount it with one
//! call:
//!
//! ```ignore
//! use daw_reaper::services;
//!
//! let handler = services::all(daw_reaper::Reaper).serve();
//! ```
//!
//! Other backends (Pro Tools, Logic, a headless mock, …) ship their
//! own equivalent — there's no umbrella trait pretending the union is
//! a spec. The protocol crate (`daw-proto`) only owns the per-service
//! `<svc>::layer(backend)` builders; choosing which services to mount
//! is each backend's job.
//!
//! Adding a service REAPER supports is a one-line edit here. Trait
//! impls missing on `Reaper` fail compilation at the matching `.add`
//! call — one place to look.

use architect::LayerSet;
use daw_proto::{
    action_registry, audio_engine, automation, batch, dawfile_service, ext_state, fx, fx_chains,
    fx_params, health, input, item, live_midi, marker, midi, plugin_loader, project, region,
    routing, screenset, take, tempo_map, toolbar, track, transport, window_geometry,
};

use crate::Reaper;

/// The full REAPER service surface as a [`LayerSet`]. Compose with
/// bolt-ons (`dock_host`, project-importer-shaped extensions, …) via
/// [`LayerSet::merge`] before serving.
pub fn all(reaper: Reaper) -> LayerSet {
    LayerSet::new()
        .add(transport::layer(reaper))
        .add(project::layer(reaper))
        .add(marker::layer(reaper))
        .add(region::layer(reaper))
        .add(tempo_map::layer(reaper))
        .add(audio_engine::layer(reaper))
        .add(midi::layer(reaper))
        .add(fx::layer(reaper))
        .add(fx_chains::layer(reaper))
        .add(fx_params::layer(reaper))
        .add(track::layer(reaper))
        .add(routing::layer(reaper))
        .add(live_midi::layer(reaper))
        .add(ext_state::layer(reaper))
        .add(health::layer(reaper))
        .add(item::layer(reaper))
        .add(take::layer(reaper))
        .add(action_registry::layer(reaper))
        .add(input::layer(reaper))
        .add(toolbar::layer(reaper))
        .add(screenset::layer(reaper))
        .add(dawfile_service::layer(reaper))
        .add(window_geometry::layer(reaper))
        .add(plugin_loader::layer(reaper))
        .add(automation::layer(reaper))
        .add(batch::layer(reaper))
}
