//! DAW REAPER Implementation
//!
//! This crate provides REAPER-specific implementations of the DAW Protocol.
//! It is designed to be used as an in-process library within the reaper-extension,
//! not as a standalone cell binary.
//!
//! # Main Thread Safety
//!
//! REAPER APIs can only be called from the main thread. This crate uses `TaskSupport`
//! from `reaper-high` to dispatch operations to the main thread.
//!
//! # Usage
//!
//! During extension initialization, call `set_task_support()` with a reference to
//! the global TaskSupport instance:
//!
//! ```rust,ignore
//! // In extension initialization
//! daw_reaper::set_task_support(Global::task_support());
//! ```
//!
//! Then create the dispatchers:
//!
//! ```rust,ignore
//! let transport = daw_reaper::ReaperTransport::new();
//! let project = daw_reaper::ReaperProject::new();
//! let dispatcher = RoutedDispatcher::new(
//!     TransportServiceDispatcher::new(transport),
//!     ProjectServiceDispatcher::new(project),
//! );
//! ```

pub mod batch;
pub mod bootstrap;
pub mod local_caller;
pub mod plugin_services;
pub mod remote;
pub mod routed_handler;
pub mod safe_wrappers;
pub mod sync;
pub mod sync_api;
pub use local_caller::LocalCaller;
pub use remote::ReaperRemote;
pub use sync::ReaperMainThread;
pub use sync_api::DawMainThread;

pub mod action_registry;
pub mod audio_accessor;
pub mod audio_engine;
pub mod automation;
pub mod dawfile_service;
pub mod ext_state;
pub mod fx;
pub mod fx_chains;
pub mod fx_params;
pub mod health;
pub mod input;
pub mod item;
pub mod live_midi;
pub mod main_thread;
pub mod marker;
pub mod midi;
pub mod peak;
pub mod plugin_loader;
pub mod position_conversion;
pub mod project;
pub mod project_context;
pub mod project_import;
pub mod ptr_validation;
pub mod region;
pub mod resource;
pub mod routing;
pub mod screenset;
pub mod take;
pub mod tempo_map;
pub mod toolbar;
pub mod track;
pub mod transport;
pub mod ui;
pub mod window_geometry;

// Re-export the main types
pub use action_registry::{
    ReaperActionRegistry, register_extension_menu, subscribe_action_broadcasts,
};
pub use audio_accessor::ReaperAudioAccessor;
pub use audio_engine::ReaperAudioEngine;
pub use automation::ReaperAutomation;
pub use dawfile_service::DawFileOps;
// `ExtState` impl'd on `Reaper` — see `crate::ext_state`. The old
// async `ReaperExtState` service struct retired with the architect
// rpc port.
pub use fx::ReaperFx;
pub use health::ReaperHealth;
pub use input::ReaperInput;
pub use item::{ReaperItem, ReaperTake};
pub use live_midi::ReaperLiveMidi;
// New shape (post-architect::rpc port): one singleton `Reaper`
// backend that impls per-service traits (`Markers`, eventually
// `Items`, `Tracks`, …) with project context flowing through each
// call. Mounting goes through `marker::serve(Reaper)`. The dispatcher
// is exposed for direct use in tests / non-vox call sites.
pub use marker::{Reaper, ReaperMainThreadDispatcher};
pub use midi::ReaperMidi;
pub use peak::ReaperPeak;
pub use plugin_loader::{ReaperPluginLoader, eager_load_fx_plugins, set_plugin_context};
pub use position_conversion::ReaperPositionConversion;
pub use project::ReaperProject;
pub use project_import::register_project_importer;
// `Regions` impl'd on `Reaper` — see `crate::region`. The old async
// `ReaperRegion` service struct retired with the architect::rpc port.
pub use region::get_regions_on_main_thread;
pub use routing::ReaperRouting;
pub use screenset::ReaperScreenset;
// `TempoMap` impl'd on `Reaper` — see `crate::tempo_map`. The old
// async `ReaperTempoMap` service struct + broadcasters retired with
// the architect::rpc port.
pub use tempo_map::{
    get_tempo_and_time_sig_at_on_main_thread, qn_to_time_on_main_thread, time_to_qn_on_main_thread,
};
pub use toolbar::ReaperToolbar;
// `Tracks` impl'd on `Reaper` — see `crate::track`. The old async
// `ReaperTrack` service struct + broadcasters retired with the port.
pub use track::add_track_on_main_thread;
// `Transport` impl'd on `Reaper` — see `crate::transport`. The old
// async `ReaperTransport` service struct + broadcasters retired with
// the architect::rpc port.
pub use window_geometry::ReaperWindowGeometry;

// Re-export the main thread bridge and transport broadcaster functions
pub use main_thread::set_task_support;

// Re-export FX broadcaster functions
pub use fx::{init_fx_broadcaster, poll_and_broadcast_fx};

// Re-export track broadcaster functions

// Tempo map broadcaster + polling retired with architect::rpc port.
// Stubs kept on `tempo_map` module so existing call sites remain.
pub use tempo_map::{init_tempo_map_broadcaster, poll_and_broadcast_tempo_map};

// Re-export item broadcaster functions
pub use item::{init_item_broadcaster, poll_and_broadcast_items};

// Re-export routing broadcaster functions
pub use routing::{init_routing_broadcaster, poll_and_broadcast_routing};

// Re-export toolbar deferred ops processor
pub use toolbar::process_deferred_ops as process_toolbar_ops;

// Re-export extension bootstrap convenience
pub use bootstrap::{build_extension_daw, build_extension_daw_with};
pub use plugin_services::create_daw_handler;
pub use routed_handler::RoutedHandler;
