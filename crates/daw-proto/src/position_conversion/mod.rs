//! Position conversion — service trait.

mod service;

pub use service::{
    PositionConversionService, PositionConversionServiceClient,
    PositionConversionServiceDispatcher, position_conversion_service_service_descriptor,
};
