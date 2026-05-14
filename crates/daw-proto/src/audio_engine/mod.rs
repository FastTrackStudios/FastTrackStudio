//! Audio engine — types + service traits.

mod service;
mod types;

pub use service::{
    AudioEngine, AudioEngineService, AudioEngineServiceClient, AudioEngineServiceDispatcher,
    audio_engine_service_service_descriptor,
};
pub use types::{AudioEngineState, AudioInputChannel, AudioInputInfo, AudioLatency};
