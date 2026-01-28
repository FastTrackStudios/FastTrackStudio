//! Rig Management for FastTrackStudio
//!
//! This module provides hierarchical preset management for live VST hosting.
//! It follows the ROAM service pattern for cross-platform RPC support.
//!
//! # Architecture
//!
//! ```text
//! Profile (FTS-Guitar-Worship, FTS-Keys-Jazz)
//! └── Rig (Guitar Rig, Keys Rig)
//!     ├── Sections[] (Amp, Effects, Piano, Organ)
//!     │   ├── Layers[] (1-3 per section)
//!     │   │   ├── LocalBlocks[] (dedicated to this layer)
//!     │   │   ├── SendLevels[] (to global blocks)
//!     │   │   └── Patch → PatchVariations[]
//!     │   └── Routing: Serial | Parallel | Custom
//!     ├── GlobalBlocks[] (shared effects)
//!     └── GlobalRouting
//!
//! Preset (combines Patches from all Sections)
//! └── Snapshot (preset-level variation for song sections)
//!
//! Song (collection of Scenes)
//! └── Scene (selects Preset or Snapshot)
//!
//! Setlist (collection of Songs)
//! ```
//!
//! # Terminology
//!
//! - **Profile**: Purpose-built rig configuration (FTS-Guitar-Worship)
//! - **Rig**: Complete instrument setup (Guitar Rig, Keys Rig)
//! - **Section**: Major part of the rig (Amp, Piano)
//! - **Layer**: Slot within a section for a sound source
//! - **Block**: Single DSP unit (plugin instance)
//! - **Patch**: Sound configuration for a layer (layer-level preset)
//! - **PatchVariation**: Parameter tweak of a patch
//! - **Preset**: Complete rig state combining patches
//! - **Snapshot**: Preset-level variation for song sections
//! - **Scene**: Song section that selects a Preset/Snapshot
//! - **Song**: Collection of Scenes
//! - **Setlist**: Collection of Songs

pub mod core;
#[cfg(feature = "dioxus")]
pub mod dioxus;
pub mod engine;
pub mod local_client;
pub mod mock;
pub mod service;

// Re-export core types
pub use core::{
    // Block types
    Block, GlobalBlock, PluginFormat, PluginId,
    // Layer types
    KeyRange, Layer, SendLevel,
    // Patch types
    Patch, PatchCategory, PatchVariation,
    // Performance types
    MidiTrigger, MidiTriggerType, PerformanceSong, PerformanceSetlist, Scene, SceneTransition,
    SyncUnit, TransitionType,
    // Preset types
    GlobalBlockConfig, PatchAssignment, PatchVariationAssignment, Preset, SectionConfig, Snapshot,
    // Profile and Rig types
    InstrumentType, Profile, Rig,
    // Routing types
    BlockRef, RoutingNode, SectionRouting,
    // Section types
    Section,
};

// Re-export service types
pub use service::{
    GlobalBlockInfo, LocalRigClient, PatchInfo, PresetInfo, ProfileInfo, RigCommand, RigEvent,
    RigInfo, RigService, SceneInfo, SectionInfo, SongInfo,
};

// Re-export mock implementation
pub use mock::MockRig;

// Re-export dioxus signals
#[cfg(feature = "dioxus")]
pub use dioxus::*;
