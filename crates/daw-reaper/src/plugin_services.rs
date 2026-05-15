//! In-process DAW service registration for CLAP plugins.
//!
//! Creates a `RoutedHandler` with all REAPER service implementations.
//! Used by `PluginHost` to create a local `Daw` instance without SHM.

use daw_proto::automation::{AutomationDispatcher, automation_service_service_descriptor};
use daw_proto::{
    ActionRegistrationDispatcher, AudioEngineDispatcher, DawFileOpsDispatcher, EffectsDispatcher,
    HealthDispatcher, InputDispatcher, LiveMidiDispatcher, MidiDispatcher, PluginLoadingDispatcher,
    ProjectsDispatcher, RoutingDispatcher, ScreensetsDispatcher, ToolbarDispatcher,
    WindowGeometryDispatcher,
};
use daw_proto::{ext_state, item, marker, region, take, tempo_map, track, transport};

use daw_proto::{
    action_registry_service_service_descriptor, audio_engine_service_service_descriptor,
    daw_file_service_service_descriptor, fx_service_service_descriptor,
    health_service_service_descriptor, input_service_service_descriptor,
    live_midi_service_service_descriptor, midi_service_service_descriptor,
    plugin_loader_service_service_descriptor, project_service_service_descriptor,
    routing_service_service_descriptor, screenset_service_service_descriptor,
    toolbar_service_service_descriptor, window_geometry_service_service_descriptor,
};

use daw_proto::batch::{BatchExecutionDispatcher, batch_service_service_descriptor};

use crate::routed_handler::RoutedHandler;

/// Create a `RoutedHandler` with all REAPER DAW service implementations.
///
/// This is the same set of services that daw-bridge registers, but
/// returned as a handler for use with `LocalCaller` (in-process).
///
/// Call after REAPER API is initialized (`PluginHost::init` handles this).
pub fn create_daw_handler() -> RoutedHandler {
    // Initialize broadcasters (idempotent if already done by daw-bridge)
    // Transport broadcaster retired alongside the architect::rpc port.
    crate::init_fx_broadcaster();
    // Track broadcaster retired alongside the architect::rpc port —
    // streaming subscriptions live on a sibling trait when revived.
    crate::init_item_broadcaster();
    crate::init_routing_broadcaster();
    crate::init_tempo_map_broadcaster();

    // Create REAPER implementations
    // Transport ported to architect::rpc — mounted via transport::serve(Reaper)
    let project = crate::ReaperProject::new();
    // Regions ported to architect::rpc — mounted via region::serve(Reaper)
    // TempoMap ported to architect::rpc — mounted via tempo_map::serve(Reaper)
    let audio_engine = crate::ReaperAudioEngine::new();
    let midi = crate::ReaperMidi::new();
    let fx = crate::ReaperFx::new();
    // Tracks ported to architect::rpc — mounted via track::serve(Reaper)
    let routing = crate::ReaperRouting::new();
    let live_midi = crate::ReaperLiveMidi::new();
    // ExtState ported — mount via ext_state::serve(Reaper)
    // Items ported to architect::rpc — mount via item::serve(Reaper)
    // Takes ported to architect::rpc — mounted via take::serve(Reaper)
    let health = crate::ReaperHealth::new();
    let action_registry = crate::ReaperActionRegistry::new();
    let input = crate::ReaperInput::new();
    let toolbar = crate::ReaperToolbar::new();
    let screenset = crate::ReaperScreenset::new();
    let dawfile_ops = crate::DawFileOps::new();
    let window_geometry = crate::ReaperWindowGeometry::new();
    let plugin_loader = crate::ReaperPluginLoader::new();
    let automation = crate::ReaperAutomation::new();
    let batch = crate::batch::BatchExecutor::new();

    RoutedHandler::new()
        .with(transport::descriptor(), transport::serve(crate::Reaper))
        .with(
            project_service_service_descriptor(),
            ProjectsDispatcher::new(project),
        )
        // Markers ported to architect::rpc — see `crate::marker`.
        // `marker::serve(Reaper)` builds the dispatcher + bridges sync
        // calls onto REAPER's main thread via `HasDispatcher`.
        .with(marker::descriptor(), marker::serve(crate::Reaper))
        .with(region::descriptor(), region::serve(crate::Reaper))
        .with(tempo_map::descriptor(), tempo_map::serve(crate::Reaper))
        .with(
            audio_engine_service_service_descriptor(),
            AudioEngineDispatcher::new(audio_engine),
        )
        .with(midi_service_service_descriptor(), MidiDispatcher::new(midi))
        // NOTE: MidiAnalysisService is no longer registered here. The impl
        // (chord detection, chart generation) lives in keyflow as the
        // `keyflow-daw-analysis` crate; fts-extensions plugs it in via
        // `RoutedHandler::with(...)` after calling `create_daw_handler()`.
        // This breaks the daw → daw-reaper → keyflow → daw cycle.
        .with(fx_service_service_descriptor(), EffectsDispatcher::new(fx))
        .with(track::descriptor(), track::serve(crate::Reaper))
        .with(
            routing_service_service_descriptor(),
            RoutingDispatcher::new(routing),
        )
        .with(
            live_midi_service_service_descriptor(),
            LiveMidiDispatcher::new(live_midi),
        )
        .with(ext_state::descriptor(), ext_state::serve(crate::Reaper))
        .with(
            health_service_service_descriptor(),
            HealthDispatcher::new(health),
        )
        .with(item::items_descriptor(), item::serve(crate::Reaper))
        .with(take::descriptor(), take::serve(crate::Reaper))
        .with(
            action_registry_service_service_descriptor(),
            ActionRegistrationDispatcher::new(action_registry),
        )
        .with(
            input_service_service_descriptor(),
            InputDispatcher::new(input),
        )
        .with(
            toolbar_service_service_descriptor(),
            ToolbarDispatcher::new(toolbar),
        )
        .with(
            screenset_service_service_descriptor(),
            ScreensetsDispatcher::new(screenset),
        )
        .with(
            daw_file_service_service_descriptor(),
            DawFileOpsDispatcher::new(dawfile_ops),
        )
        .with(
            window_geometry_service_service_descriptor(),
            WindowGeometryDispatcher::new(window_geometry),
        )
        .with(
            plugin_loader_service_service_descriptor(),
            PluginLoadingDispatcher::new(plugin_loader),
        )
        .with(
            automation_service_service_descriptor(),
            AutomationDispatcher::new(automation),
        )
        .with(
            batch_service_service_descriptor(),
            BatchExecutionDispatcher::new(batch),
        )
}
