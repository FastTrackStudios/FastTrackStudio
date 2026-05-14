//! Health probe service — service trait.

mod service;

pub use service::{
    HealthService, HealthServiceClient, HealthServiceDispatcher, health_service_service_descriptor,
};
