//! In-process DAW service registration for CLAP plugins.
//!
//! Creates a `RoutedHandler` mounting every architect::rpc service on
//! the stateless `crate::Reaper` singleton. Used by `PluginHost` to
//! create a local `Daw` instance without SHM.

use daw_proto::{
    action_registry, audio_engine, automation, batch, dawfile_service, ext_state, fx, health,
    input, item, live_midi, marker, midi, plugin_loader, project, region, routing, screenset, take,
    tempo_map, toolbar, track, transport, window_geometry,
};

use crate::Reaper;
use crate::routed_handler::RoutedHandler;

/// Create a `RoutedHandler` with all REAPER DAW service implementations.
///
/// This is the same set of services that daw-bridge registers, but
/// returned as a handler for use with `LocalCaller` (in-process).
///
/// Call after REAPER API is initialized (`PluginHost::init` handles this).
pub fn create_daw_handler() -> RoutedHandler {
    // Surviving (non-service) broadcasters retained.
    crate::init_item_broadcaster();
    crate::init_tempo_map_broadcaster();

    RoutedHandler::new()
        .with(transport::descriptor(), transport::serve(Reaper))
        .with(project::descriptor(), project::serve(Reaper))
        .with(marker::descriptor(), marker::serve(Reaper))
        .with(region::descriptor(), region::serve(Reaper))
        .with(tempo_map::descriptor(), tempo_map::serve(Reaper))
        .with(audio_engine::descriptor(), audio_engine::serve(Reaper))
        .with(midi::descriptor(), midi::serve(Reaper))
        .with(fx::descriptor(), fx::serve(Reaper))
        .with(track::descriptor(), track::serve(Reaper))
        .with(routing::descriptor(), routing::serve(Reaper))
        .with(live_midi::descriptor(), live_midi::serve(Reaper))
        .with(ext_state::descriptor(), ext_state::serve(Reaper))
        .with(health::descriptor(), health::serve(Reaper))
        .with(item::items_descriptor(), item::serve(Reaper))
        .with(take::descriptor(), take::serve(Reaper))
        .with(
            action_registry::descriptor(),
            action_registry::serve(Reaper),
        )
        .with(input::descriptor(), input::serve(Reaper))
        .with(toolbar::descriptor(), toolbar::serve(Reaper))
        .with(screenset::descriptor(), screenset::serve(Reaper))
        .with(
            dawfile_service::descriptor(),
            dawfile_service::serve(Reaper),
        )
        .with(
            window_geometry::descriptor(),
            window_geometry::serve(Reaper),
        )
        .with(plugin_loader::descriptor(), plugin_loader::serve(Reaper))
        .with(automation::descriptor(), automation::serve(Reaper))
        .with(batch::descriptor(), batch::serve(Reaper))
}
